#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

typedef enum {
  Mass_Cli_Mode_Compile,
  Mass_Cli_Mode_Run,
} Mass_Cli_Mode;

s32
mass_cli_print_usage() {
  printf("Mass Compiler\n");
  printf("Usage:\n");
  printf("  mass [--run] source_code.mass\n");
  return -1;
}

int main(s32 argc, char **argv) {
  if (argc < 2) {
    return mass_cli_print_usage();
  }

  temp_buffer = bucket_buffer_make(.allocator = allocator_system);
  temp_allocator = bucket_buffer_allocator_make(temp_buffer);

  Mass_Cli_Mode mode = Mass_Cli_Mode_Compile;
  char *raw_file_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--run") == 0) {
      mode = Mass_Cli_Mode_Run;
    } else {
      if (raw_file_path) {
        return mass_cli_print_usage();
      } else {
        raw_file_path = argv[i];
      }
    }
  }

  Slice file_path = slice_from_c_string(raw_file_path);
  Program *program = program_init(&(Program) {0});
  program_import_file(program, slice_literal("lib\\prelude"));
  program_import_file(program, file_path);


  program->entry_point = scope_lookup_force(program->global_scope, slice_literal("main"), 0);

  switch(mode) {
    case Mass_Cli_Mode_Compile: {
      Array_Slice parts = slice_split_by_slice(allocator_default, file_path, slice_literal("\\"));
      Slice base_name = *dyn_array_pop(parts);
      Fixed_Buffer *buffer = fixed_buffer_make();
      fixed_buffer_append_slice(buffer, slice_literal("build\\"));
      fixed_buffer_append_slice(buffer, base_name);
      fixed_buffer_append_slice(buffer, slice_literal(".exe"));
      wchar_t *exe_path_wide = utf8_to_utf16_null_terminated(
        allocator_default,
        fixed_buffer_as_slice(buffer)
      );
      write_executable(exe_path_wide, program);
      break;
    }
    case Mass_Cli_Mode_Run: {
      program_end(program);
      value_as_function(program->entry_point, fn_type_void_to_void)();
      ExitProcess(0);
      break;
    }
  }
  return 0;
}
