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
mass_cli_print_errors(
  Array_Parse_Error errors
) {
  for (u64 i = 0; i < dyn_array_length(errors); ++i) {
    Parse_Error *error = dyn_array_get(errors, i);
    slice_print(error->message);
    printf("  at ");
    source_range_print_start_position(&error->source_range);
  }
  return -1;
}

int main(s32 argc, char **argv) {
  if (argc < 2) {
    return mass_cli_print_usage();
  }

  Executable_Type win32_executable_type = Executable_Type_Cli;

  Mass_Cli_Mode mode = Mass_Cli_Mode_Compile;
  const char *raw_file_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    const char *arg = argv[i];
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

  Slice file_path = slice_from_c_string(raw_file_path);

  Bucket_Buffer *compilation_buffer = bucket_buffer_make(.allocator = allocator_system);
  Allocator *compilation_allocator = bucket_buffer_allocator_make(compilation_buffer);

  Compilation_Context context = {
    .allocation_buffer = compilation_buffer,
    .allocator = compilation_allocator,
    .program = program_init(compilation_allocator, &(Program) {0}),
  };

  Parse_Result result = program_import_file(&context, slice_literal("lib/prelude"));
  if(result.tag != Parse_Result_Tag_Success) {
    return mass_cli_print_errors(result.Error.errors);
  }
  result = program_import_file(&context, file_path);
  if(result.tag != Parse_Result_Tag_Success) {
    return mass_cli_print_errors(result.Error.errors);
  }

  context.program->entry_point =
    scope_lookup_force(&context, context.program->global_scope, slice_literal("main"));
  if (!context.program->entry_point) {
    printf("Could not find entry point function `main`");
    return -1;
  }
  if (dyn_array_length(context.program->errors)) {
    return mass_cli_print_errors(context.program->errors);
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
      program_jit(&context);
      fn_type_opaque main = value_as_function(context.program, context.program->entry_point);
      main();
      return 0;
    }
  }
  return 0;
}
