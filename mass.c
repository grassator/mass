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
  char *file_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--run") == 0) {
      mode = Mass_Cli_Mode_Run;
    } else {
      if (file_path) {
        return mass_cli_print_usage();
      } else {
        file_path = argv[i];
      }
    }
  }

  Program *program = program_init(&(Program) {0});
  program_import_file(program, slice_from_c_string(file_path));

  program->entry_point = scope_lookup_force(program->global_scope, slice_literal("main"), 0);

  switch(mode) {
    case Mass_Cli_Mode_Compile: {
      // TODO generate correct file name
      write_executable(L"build\\test_cli.exe", program);
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
