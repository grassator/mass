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
  puts(
    "Mass Compiler v0.0.1\n"
    "Usage:\n"
    "  mass [flags] source_code.mass\n\n"
    "Flags:\n"
    "  --run              Run code in JIT mode\n"
    "  --binary-format    [pe32:cli, pe32:gui]\n"
    "    Set output binary executable format;"
    #ifdef _WIN32
    " defaults to pe32:cli"
    #endif
  );
  return -1;
}

s32
mass_cli_print_error(
  Parse_Error *error
) {
  slice_print(error->message);
  printf("  at ");
  source_range_print_start_position(&error->source_range);
  return -1;
}

int main(s32 argc, char **argv) {
  if (argc < 2) {
    return mass_cli_print_usage();
  }

  Executable_Type win32_executable_type = Executable_Type_Cli;

  Mass_Cli_Mode mode = Mass_Cli_Mode_Compile;
  char *raw_file_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    char *arg = argv[i];
    if (strcmp(arg, "--run") == 0) {
      mode = Mass_Cli_Mode_Run;
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

  // Normalize slashes
  for (char *ch = raw_file_path; *ch; ++ch) {
    if (*ch == '\\') *ch = '/';
  }
  Slice file_path = slice_from_c_string(raw_file_path);

  Compilation_Context context;
  compilation_context_init(allocator_system, &context);

  Scope *module_scope = scope_make(context.allocator, context.scope);
  Module *prelude_module = program_module_from_file(
    &context, slice_literal("lib/prelude"), module_scope
  );

  Mass_Result result = program_import_module(&context, prelude_module);
  if(result.tag != Mass_Result_Tag_Success) {
    return mass_cli_print_error(&result.Error.details);
  }
  Module *root_module = program_module_from_file(
    &context, file_path, module_scope
  );
  result = program_import_module(&context, root_module);
  if(result.tag != Mass_Result_Tag_Success) {
    return mass_cli_print_error(&result.Error.details);
  }

  // FIXME use export scope for this
  Value *main = scope_lookup_force(&context, root_module->own_scope, slice_literal("main"));
  if (!main) {
    printf("Could not find entry point function `main`");
    return -1;
  }
  context.program->entry_point = main;
  ensure_compiled_function_body(&context, main);
  if(context.result->tag != Mass_Result_Tag_Success) {
    return mass_cli_print_error(&context.result->Error.details);
  }

  switch(mode) {
    case Mass_Cli_Mode_Compile: {
      Array_Slice parts = slice_split_by_slice(allocator_default, file_path, slice_literal("/"));
      Slice base_name = *dyn_array_pop(parts);
      Bucket_Buffer *path_builder = bucket_buffer_make();
      bucket_buffer_append_slice(path_builder, slice_literal("build/"));
      Slice extension = slice_literal(".mass");
      if (slice_ends_with(base_name, extension)) {
        base_name.length -= extension.length;
      }
      bucket_buffer_append_slice(path_builder, base_name);
      bucket_buffer_append_slice(path_builder, slice_literal(".exe"));
      bucket_buffer_append_u8(path_builder, 0);
      Fixed_Buffer *path_buffer =
        bucket_buffer_to_fixed_buffer(allocator_default, path_builder); // @Leak
      write_executable((char *)path_buffer->memory, &context, win32_executable_type);
      bucket_buffer_destroy(path_builder);
      break;
    }
    case Mass_Cli_Mode_Run: {
      Jit jit;
      jit_init(&jit, context.program);
      program_jit(&jit);
      fn_type_opaque main = value_as_function(&jit, jit.program->entry_point);
      main();
      return 0;
    }
  }
  return 0;
}
