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

  u64 function_count = dyn_array_length(program->functions);

  // Encode newly added functions
  for (u64 i = jit->previous_counts.functions; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    Function_Layout layout;
    fn_encode(program, code_buffer, builder, &layout);
  }

  // After all the functions are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Setup permissions for read-only data segment
  posix_section_protect_from(&memory->ro_data, ro_data_protected_size);

  // Setup permissions for the code segment
  posix_section_protect_from(&memory->code, code_protected_size);

  // Resolve relocations
  u64 relocation_count = dyn_array_length(program->relocations);
  for (u64 i = jit->previous_counts.relocations; i < relocation_count; ++i) {
    Relocation *relocation = dyn_array_get(program->relocations, i);
    assert(storage_is_label(&relocation->patch_at));
    assert(storage_is_label(&relocation->address_of));
    Label_Index patch_at_index =
      relocation->patch_at.Memory.location.Instruction_Pointer_Relative.label_index;
    Label_Index address_of_index =
      relocation->address_of.Memory.location.Instruction_Pointer_Relative.label_index;
    void *address_of = rip_value_pointer_from_label_index(program, address_of_index);
    void **patch_at = rip_value_pointer_from_label_index(program, patch_at_index);
    *patch_at = address_of;
  }

  // Call new startup functions
  u64 startup_count = dyn_array_length(program->startup_functions);
  for (u64 i = jit->previous_counts.startup; i < startup_count; ++i) {
    Value *value = *dyn_array_get(program->startup_functions, i);
    fn_type_opaque fn = value_as_function(program, value);
    fn();
  }

  jit->previous_counts.relocations = relocation_count;
  jit->previous_counts.functions = function_count;
  jit->previous_counts.startup = startup_count;
}


#endif // POSIX_RUNTIME_H
