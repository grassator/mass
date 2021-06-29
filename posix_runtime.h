#ifndef POSIX_RUNTIME_H
#define POSIX_RUNTIME_H

#include "program.h"
#include <sys/mman.h>
#include <dlfcn.h>

static int
posix_section_permissions_to_mprotect_flags(
  Section_Permissions permissions
) {
  int protect_flags = PROT_NONE;
  if (permissions & Section_Permissions_Read) {
    protect_flags |= PROT_READ;
  }
  if (permissions & Section_Permissions_Write) {
    protect_flags |= PROT_WRITE;
  }
  if (permissions & Section_Permissions_Execute) {
    protect_flags |= PROT_EXEC;
  }
  return protect_flags;
}

static u64
posix_buffer_ensure_last_page_is_writable(
  Virtual_Memory_Buffer *buffer
) {
  const s32 page_size = memory_page_size();
  const u64 remainder = buffer->occupied % page_size;
  const u64 protected = buffer->occupied - remainder;
  if (remainder != 0) {
    void *address = buffer->memory + protected;
    if (0 != mprotect(address, page_size, PROT_READ | PROT_WRITE)) {
      panic("UNREACHED");
    }
  }
  return protected;
}

static void
posix_section_protect_from(
  Section *section,
  u64 from
) {
  int protect_flags = posix_section_permissions_to_mprotect_flags(section->permissions);
  u64 size_to_protect = section->buffer.occupied - from;
  if (size_to_protect) {
    void *address = section->buffer.memory + from;
    if (0 != mprotect(address, size_to_protect, protect_flags)) {
      panic("UNREACHED");
    }
  }
}

void *posix_load_library(const char *name) { return dlopen(name, RTLD_LAZY); }
void *posix_load_symbol(void *handle, const char *name) { return dlsym(handle, name); }

static const Native_Library_Load_Callbacks posix_library_load_callbacks = {
  .load_library = posix_load_library,
  .load_symbol = posix_load_symbol,
};

// TODO make this return MASS_RESULT
static void
posix_program_jit(
  Compilation *compilation,
  Jit *jit
) {
  Program *program = jit->program;
  Program_Memory *memory = &jit->program->memory;
  Virtual_Memory_Buffer *code_buffer = &memory->code.buffer;
  Virtual_Memory_Buffer *ro_data_buffer = &memory->ro_data.buffer;

  // Memory protection works on per-page level so with incremental JIT there are two options:
  // 1. Waste memory every time we do JIT due to padding to page size.
  // 2. Switch memory back to writable before new writes.
  // On Windows options 2 is preferable, however some unix-like system disallow multiple
  // transitions between writable and executable so might have to resort to 1 there.
  u64 code_protected_size = posix_buffer_ensure_last_page_is_writable(code_buffer);
  u64 ro_data_protected_size = posix_buffer_ensure_last_page_is_writable(ro_data_buffer);

  Mass_Result result = program_jit_imports(
    compilation->temp_allocator, jit, ro_data_buffer, &posix_library_load_callbacks
  );
  (void)result;

  // Encode newly added functions
  u64 function_count = dyn_array_length(program->functions);
  for (u64 i = jit->previous_counts.functions; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    Function_Layout layout;
    fn_encode(program, code_buffer, builder, &layout);
  }
  jit->previous_counts.functions = function_count;

  // After all the functions are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Setup permissions for read-only data segment
  posix_section_protect_from(&memory->ro_data, ro_data_protected_size);

  // Setup permissions for the code segment
  posix_section_protect_from(&memory->code, code_protected_size);

  program_jit_resolve_relocations(jit);
  program_jit_call_startup_functions(jit);
}

#endif // POSIX_RUNTIME_H
