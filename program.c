#include "program.h"

#if defined(_WIN32)
#include "win32_runtime.h"
#elif defined(__linux__) || defined(__MACH__)
#include "posix_runtime.h"
#endif

static void
program_init(
  Allocator *allocator,
  Program *program,
  const Calling_Convention *default_calling_convention
) {
  *program = (Program) {
    .labels = dyn_array_make(Array_Label, .capacity = 128, .allocator = allocator),
    .patch_info_array = dyn_array_make(Array_Label_Location_Diff_Patch_Info, .capacity = 128, .allocator = allocator),
    .import_libraries = dyn_array_make(Array_Import_Library, .capacity = 16, .allocator = allocator),
    .startup_functions = dyn_array_make(Array_Value_Ptr, .capacity = 16, .allocator = allocator),
    .relocations = dyn_array_make(Array_Relocation, .capacity = 16, .allocator = allocator),
    .functions = dyn_array_make(Array_Function_Builder, .capacity = 16, .allocator = allocator),
    .default_calling_convention = default_calling_convention,
  };

  #define MAX_CODE_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  #define MAX_RW_DATA_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  #define MAX_RO_DATA_SIZE (640llu * 1024llu * 1024llu) // 640Mb
  // :FunctionTableCallbackMax2Gb
  // (640 + 640 + 640 == 1920) < 2048
  #define MAX_PROGRAM_SIZE (MAX_RW_DATA_SIZE + MAX_CODE_SIZE + MAX_RO_DATA_SIZE)
  virtual_memory_buffer_init(&program->memory.buffer, MAX_PROGRAM_SIZE);

  u64 offset_in_memory = 0;

  program->memory.code = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_CODE_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Execute,
  };
  offset_in_memory += program->memory.code.buffer.capacity;

  program->memory.rw_data = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_RW_DATA_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Read | Section_Permissions_Write,
  };
  offset_in_memory += program->memory.rw_data.buffer.capacity;

  program->memory.ro_data = (Section){
    .buffer = {
      .memory = program->memory.buffer.memory + offset_in_memory,
      .capacity = MAX_RO_DATA_SIZE,
    },
    .base_rva = u64_to_u32(offset_in_memory),
    .permissions = Section_Permissions_Read,
  };
};

static void
program_deinit(
  Program *program
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *library = dyn_array_get(program->import_libraries, i);
    dyn_array_destroy(library->symbols);
  }
  virtual_memory_buffer_deinit(&program->memory.buffer);
  dyn_array_destroy(program->labels);
  dyn_array_destroy(program->patch_info_array);
  dyn_array_destroy(program->import_libraries);
  dyn_array_destroy(program->functions);
  dyn_array_destroy(program->startup_functions);
  dyn_array_destroy(program->relocations);
}

static void
program_set_label_offset(
  Program *program,
  Label_Index label_index,
  u32 offset_in_section
) {
  Label *label = program_get_label(program, label_index);
  label->resolved = true;
  label->offset_in_section = offset_in_section;
}

static void
program_patch_labels(
  Program *program
) {
  for (
    u64 patch_index = 0;
    patch_index < dyn_array_length(program->patch_info_array);
    ++patch_index
  ) {
    Label_Location_Diff_Patch_Info *info = dyn_array_get(program->patch_info_array, patch_index);
    Label *target_label = program_get_label(program, info->target_label_index);
    assert(target_label->resolved);

    s64 from_rva = program_resolve_label_to_rva(program, &info->from);
    s64 target_rva = program_resolve_label_to_rva(program, target_label);

    s64 diff = target_rva - from_rva;
    *info->patch_target = s64_to_s32(diff);
  }
}

static Import_Library *
program_find_import_library(
  const Program *program,
  const Slice library_name
) {
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    if (slice_ascii_case_insensitive_equal(lib->name, library_name)) {
      return lib;
    }
  }
  return 0;
}

static Import_Symbol *
import_library_find_symbol(
  const Import_Library *library,
  const Slice symbol_name
) {
  for (u64 i = 0; i < dyn_array_length(library->symbols); ++i) {
    Import_Symbol *symbol = dyn_array_get(library->symbols, i);
    if (slice_equal(symbol->name, symbol_name)) {
      return symbol;
    }
  }
  return 0;
}

static Import_Symbol *
program_find_import(
  const Program *program,
  const Slice library_name,
  const Slice symbol_name
) {
  Import_Library *lib = program_find_import_library(program, library_name);
  if (!lib) return 0;
  return import_library_find_symbol(lib, symbol_name);
}

static Storage
import_symbol(
  Execution_Context *context,
  const Slice library_name,
  const Slice symbol_name
) {
  Program *program = context->program;
  Import_Library *library = program_find_import_library(program, library_name);
  if (!library) {
    library = dyn_array_push(program->import_libraries, (Import_Library) {
      .name = library_name,
      .symbols = dyn_array_make(Array_Import_Symbol, .allocator = context->allocator),
    });
  }

  Import_Symbol *symbol = import_library_find_symbol(library, symbol_name);

  if (!symbol) {
    Label_Index label = make_label(program, &program->memory.ro_data, symbol_name);
    symbol = dyn_array_push(library->symbols, (Import_Symbol) {
      .name = symbol_name,
      .label32 = label,
    });
  }

  u32 byte_size = 8;
  return data_label32(symbol->label32, byte_size);
}

static Mass_Result
program_jit_imports(
  const Allocator *temp_allocator,
  Jit *jit,
  Virtual_Memory_Buffer *ro_data_buffer,
  const Native_Library_Load_Callbacks *callbacks
) {
  Program *program = jit->program;
  u64 import_count = dyn_array_length(program->import_libraries);
  for (u64 i = jit->previous_counts.imports; i < import_count; ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    void **maybe_handle_pointer = hash_map_get(jit->import_library_handles, lib->name);
    void *handle;
    if (maybe_handle_pointer) {
      handle = *maybe_handle_pointer;
    } else {
      char *library_name = slice_to_c_string(temp_allocator, lib->name);
      handle = callbacks->load_library(library_name);
      assert(handle);
      hash_map_set(jit->import_library_handles, lib->name, handle);
    }

    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, symbol_index);
      Label *label = program_get_label(program, symbol->label32);
      if (!label->resolved) {
        char *symbol_name = slice_to_c_string(temp_allocator, symbol->name);
        fn_type_opaque address = (fn_type_opaque)callbacks->load_symbol(handle, symbol_name);
        assert(address);
        u64 offset = virtual_memory_buffer_append_u64(ro_data_buffer, (u64)address);
        label->offset_in_section = u64_to_u32(offset);
        label->resolved = true;
      }
    }
  }
  jit->previous_counts.imports = import_count;
  return mass_success();
}

static void
program_jit_resolve_relocations(
  Jit *jit
) {
  Program *program = jit->program;
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
  jit->previous_counts.relocations = relocation_count;
}

static void
program_jit_call_startup_functions(
  Jit *jit
) {
  Program *program = jit->program;
  u64 startup_count = dyn_array_length(program->startup_functions);
  for (u64 i = jit->previous_counts.startup; i < startup_count; ++i) {
    Value *value = *dyn_array_get(program->startup_functions, i);
    fn_type_opaque fn = value_as_function(program, value);
    fn();
  }
  jit->previous_counts.startup = startup_count;
}

static PRELUDE_NO_DISCARD Mass_Result
program_jit(
  Compilation *compilation,
  Jit *jit
) {
  Temp_Mark mark = compilation_temp_mark(compilation);
  Mass_Result result;
  #if defined(_WIN32)
  result = win32_program_jit(compilation, jit);
  #elif defined(__linux__) || defined(__MACH__)
  result = posix_program_jit(compilation, jit);
  #else
  panic("JIT compilation is (yet) not implemented for this system");
  #endif
  compilation_temp_reset_to_mark(compilation, mark);
  return result;
}
