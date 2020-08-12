#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

#define report_error(_error_)\
  do {\
    if (file_handle) CloseHandle(file_handle);\
    printf("Error: %s\n", _error_);\
    return -1;\
  } while (0)

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

int wmain(s32 argc, wchar_t **argv) {
  if (argc < 2) {
    return mass_cli_print_usage();
  }

  Mass_Cli_Mode mode = Mass_Cli_Mode_Compile;
  wchar_t *file_path = 0;
  for (s32 i = 1; i < argc; ++i) {
    if (wcscmp(argv[i], L"--run") == 0) {
      mode = Mass_Cli_Mode_Run;
    } else {
      if (file_path) {
        return mass_cli_print_usage();
      } else {
        file_path = argv[i];
      }
    }
  }

  HANDLE file_handle = CreateFileW(
    file_path,
    GENERIC_READ,
    FILE_SHARE_READ,
    0,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if (!file_handle) report_error("Could not open specified file");

  s32 buffer_size = GetFileSize(file_handle, 0);
  Fixed_Buffer *buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = s32_to_u64(buffer_size),
  );
  s32 bytes_read = 0;
  BOOL is_success = ReadFile(file_handle, buffer->memory, buffer_size, &bytes_read, 0);
  if (!is_success) report_error("Could not read specified file");
  CloseHandle(file_handle);
  file_handle = 0;

  buffer->occupied = s32_to_u64(bytes_read);

  Slice utf8_file_path = utf16_null_terminated_to_utf8(allocator_default, file_path);
  Slice source = fixed_buffer_as_slice(buffer);

  temp_buffer = bucket_buffer_make(.allocator = allocator_system);
  temp_allocator = bucket_buffer_allocator_make(temp_buffer);

  Program *program = program_init(&(Program) {0});

  Tokenizer_Result result = tokenize(utf8_file_path, source);
  if (result.type != Tokenizer_Result_Type_Success) {
    for (u64 i = 0; i < dyn_array_length(result.errors); ++i) {
      Tokenizer_Error *error = dyn_array_get(result.errors, i);
      print_message_with_location(error->message, &error->location);
    }
    return -1;
  }

  token_match_module(result.root, program);

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
