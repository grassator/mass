#define WIN32_LEAN_AND_MEAN

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"
#include "program.c"

typedef enum {
  Mass_Cli_Mode_Compile,
  Mass_Cli_Mode_Run,
  Mass_Cli_Mode_Script,
} Mass_Cli_Mode;

s32
mass_cli_print_usage() {
  puts(
    "Mass Compiler v0.0.1\n"
    "Usage:\n"
    "  mass [flags] source_code.mass\n\n"
    "Flags:\n"
    "  --run              Run code in JIT mode\n"
    "  --output           <path>\n"
    "  --binary-format    [pe32:cli, pe32:gui]\n"
    "    Set output binary executable format;"
    #ifdef _WIN32
    " defaults to pe32:cli"
    #endif
  );
  return -1;
}

static s32
mass_cli_print_error(
  Compilation *compilation,
  Mass_Result *result
) {
  assert(result->tag == Mass_Result_Tag_Error);
  mass_print_error(compilation, &result->Error.error);
  return -1;
}

int main(s32 argc, char **argv) {
  if (argc < 2) {
    return mass_cli_print_usage();
  }

  Executable_Type win32_executable_type = Executable_Type_Cli;

  Mass_Cli_Mode mode = Mass_Cli_Mode_Compile;
  char *raw_file_path = 0;
  char *raw_output_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    char *arg = argv[i];
    if (strcmp(arg, "--run") == 0) {
      mode = Mass_Cli_Mode_Run;
    } else if (strcmp(arg, "--script") == 0) {
      mode = Mass_Cli_Mode_Script;
    } else if (strcmp(arg, "--output") == 0) {
      if (++i >= argc) {
        return mass_cli_print_usage();
      }
      raw_output_path = argv[i];
    } else if (strcmp(arg, "--binary-format") == 0) {
      if (++i >= argc) {
        return mass_cli_print_usage();
      }
      const char *format = argv[i];
      if (strcmp(format, "pe32:gui") == 0) {
        win32_executable_type = Executable_Type_Gui;
      } else if (strcmp(format, "pe32:cli") == 0) {
        win32_executable_type = Executable_Type_Cli;
      } else {
        return mass_cli_print_usage();
      }
    } else {
      if (raw_file_path) {
        return mass_cli_print_usage();
      } else {
        raw_file_path = arg;
      }
    }
  }

  if (!raw_file_path) {
    return mass_cli_print_usage();
  }

  // Normalize slashes
  for (char *ch = raw_file_path; *ch; ++ch) {
    if (*ch == '\\') *ch = '/';
  }
  Slice file_path = slice_from_c_string(raw_file_path);

  Os os = Os_Windows;
  switch(mode) {
    case Mass_Cli_Mode_Compile: {
      os = Os_Windows;
      break;
    }
    case Mass_Cli_Mode_Run:
    case Mass_Cli_Mode_Script: {
      os = host_os();
      break;
    }
  }

  Compilation compilation;
  compilation_init(&compilation, os);
  Execution_Context context = execution_context_from_compilation(&compilation);

  program_load_file_module_into_root_scope(&context, slice_literal("std/prelude"));
  if (mass_has_error(&context)) {
    return mass_cli_print_error(&compilation, context.result);
  }

  if (mode == Mass_Cli_Mode_Script) {
    mass_run_script(&context, file_path);
    if (mass_has_error(&context)) {
      return mass_cli_print_error(&compilation, context.result);
    }
    return 0;
  }

  Module *root_module = program_module_from_file(&context, file_path, compilation.root_scope);
  program_import_module(&context, root_module);
  if (mass_has_error(&context)) {
    return mass_cli_print_error(&compilation, context.result);
  }

  const Symbol *main_symbol = mass_ensure_symbol(&compilation, slice_literal("main"));
  Scope_Entry *entry = scope_lookup(compilation.root_scope, main_symbol);
  Value *main = scope_entry_force_value(&compilation, entry);
  if (!main) {
    printf("Could not find entry point function `main`");
    return -1;
  }
  context.program->entry_point = main;
  ensure_function_instance(&compilation, context.program, main, (Value_View){0});
  if (mass_has_error(&context)) {
    return mass_cli_print_error(&compilation, context.result);
  }

  switch(mode) {
    case Mass_Cli_Mode_Compile: {
      Fixed_Buffer *path_buffer =
        fixed_buffer_make(.allocator = allocator_default, .capacity = 16 * 1024 * 1024);
      if (raw_output_path) {
        fixed_buffer_append_slice(path_buffer, slice_from_c_string(raw_output_path));
      } else {
        Array_Slice parts = slice_split_by_slice(allocator_default, file_path, slice_literal("/"));
        Slice base_name = *dyn_array_pop(parts);
        dyn_array_destroy(parts);
        Slice extension = slice_literal(".mass");
        if (slice_ends_with(base_name, extension)) {
          base_name.length -= extension.length;
        }
        fixed_buffer_append_slice(path_buffer, base_name);
        fixed_buffer_append_slice(path_buffer, slice_literal(".exe"));
      }
      write_executable(fixed_buffer_as_slice(path_buffer), &context, win32_executable_type);
      fixed_buffer_destroy(path_buffer);
      break;
    }
    case Mass_Cli_Mode_Script: {
      panic("Script mode expected to be handled at an earlier stage");
    } break;
    case Mass_Cli_Mode_Run: {
      Jit jit;
      jit_init(&jit, context.program);
      program_jit(context.compilation, &jit);
      if (mass_has_error(&context)) {
        return mass_cli_print_error(&compilation, context.result);
      }
      fn_type_opaque main = value_as_function(jit.program, jit.program->entry_point);
      main();
      return 0;
    }
  }
  return 0;
}
