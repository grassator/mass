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

int wmain(s32 argc, wchar_t **argv) {
  if (argc != 2) {
    printf("Mass Compiler\n");
    printf("Usage:\n");
    printf("  mass source_code.mass\n");
    return 0;
  }

  wchar_t *file_path = argv[1];

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

  Program *program = &(Program) {
    .data_buffer = fixed_buffer_make(.allocator = allocator_system, .capacity = 128 * 1024),
    .import_libraries = dyn_array_make(Array_Import_Library, .capacity = 16),
    .functions = dyn_array_make(Array_Function_Builder, .capacity = 16),
    .global_scope = scope_make(0),
  };

  scope_define_value(program->global_scope, slice_literal("s64"), type_s64_value);
  scope_define_value(program->global_scope, slice_literal("s32"), type_s32_value);

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
  write_executable(L"build\\test_cli.exe", program);
  return 0;
}
