#include "prelude.h"
#include "assert.h"
#include "value.h"
#include "function.h"
#include <time.h>

#define PE32_FILE_ALIGNMENT 0x200
#define PE32_SECTION_ALIGNMENT 0x1000
#define PE32_MIN_WINDOWS_VERSION_VISTA 6

enum {
  EXPORT_DIRECTORY_INDEX,
  IMPORT_DIRECTORY_INDEX,
  RESOURCE_DIRECTORY_INDEX,
  EXCEPTION_DIRECTORY_INDEX,
  SECURITY_DIRECTORY_INDEX,
  RELOCATION_DIRECTORY_INDEX,
  DEBUG_DIRECTORY_INDEX,
  ARCHITECTURE_DIRECTORY_INDEX,
  GLOBAL_PTR_DIRECTORY_INDEX,
  TLS_DIRECTORY_INDEX,
  LOAD_CONFIG_DIRECTORY_INDEX,
  BOUND_IMPORT_DIRECTORY_INDEX,
  IAT_DIRECTORY_INDEX,
  DELAY_IMPORT_DIRECTORY_INDEX,
  CLR_DIRECTORY_INDEX,
};

typedef struct {
  Fixed_Buffer *buffer;
  s32 iat_rva;
  s32 iat_size;
  s32 import_directory_rva;
  s32 import_directory_size;
} Encoded_Rdata_Section;

Encoded_Rdata_Section
encode_rdata_section(
  Program * program,
  IMAGE_SECTION_HEADER *header
) {
  #define get_rva() s64_to_s32(s32_to_s64(header->VirtualAddress) + u64_to_s64(buffer->occupied))

  u64 expected_encoded_size = 0;
  program->data_base_rva = header->VirtualAddress;

  {
    // Volatile: This code estimates encoded size based on the code below
    //           and needs to be modified accordingly
    for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
      Import_Library *lib = dyn_array_get(program->import_libraries, i);
      // Library names
      u64 aligned_name_size = u64_align(lib->name.length + 1, 2);
      expected_encoded_size += aligned_name_size;

      expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);

      for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
        Import_Symbol *symbol = dyn_array_get(lib->symbols, i);
        expected_encoded_size += sizeof(s16); // Ordinal Hint

        // Symbol names
        u64 aligned_name_size = u64_align(symbol->name.length + 1, 2);
        expected_encoded_size += aligned_name_size;

        expected_encoded_size += sizeof(u64); // IAT
        expected_encoded_size += sizeof(u64); // Image thunk
      }

      expected_encoded_size += sizeof(u64); // End of IAT list
      expected_encoded_size += sizeof(u64); // End of Image thunk list
    }

    // End of IMAGE_IMPORT_DESCRIPTOR list
    expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);
  }

  u64 global_data_size = u64_align(program->data_buffer->occupied, 16);
  expected_encoded_size += global_data_size;

  Encoded_Rdata_Section result = {
    .buffer = fixed_buffer_make(.allocator = allocator_system, .capacity = expected_encoded_size),
  };

  Fixed_Buffer *buffer = result.buffer;

  void *global_data = fixed_buffer_allocate_bytes(buffer, global_data_size, sizeof(s8));
  memcpy(global_data, program->data_buffer->memory, program->data_buffer->occupied);

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, i);
      symbol->name_rva = get_rva();
      fixed_buffer_append_s16(buffer, 0); // Ordinal Hint, value not required
      u64 name_size = symbol->name.length;
      u64 aligned_name_size = u64_align(name_size + 1, 2);
      memcpy(
        fixed_buffer_allocate_bytes(buffer, aligned_name_size, sizeof(s8)),
        symbol->name.bytes,
        name_size
      );
    }
  }

  result.iat_rva = get_rva();
  // IAT list
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    lib->rva = get_rva();
    for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
      Import_Symbol *fn = dyn_array_get(lib->symbols, i);
      fn->offset_in_data = get_rva() - header->VirtualAddress;
      fixed_buffer_append_u64(buffer, fn->name_rva);
    }

    fixed_buffer_append_u64(buffer, 0);
  }
  result.iat_size = (s32)buffer->occupied;

  // Image thunks
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    lib->image_thunk_rva = get_rva();

    for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
      Import_Symbol *fn = dyn_array_get(lib->symbols, i);
      fixed_buffer_append_u64(buffer, fn->name_rva);
    }
    // End of Image thunks list
    fixed_buffer_append_u64(buffer, 0);
  }

  // Library Names
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    lib->name_rva = get_rva();
    u64 name_size = lib->name.length;
    u64 aligned_name_size = u64_align(name_size + 1, 2);
    memcpy(
      fixed_buffer_allocate_bytes(buffer, aligned_name_size, sizeof(s8)),
      lib->name.bytes,
      name_size
    );
  }

  // Import Directory
  result.import_directory_rva = get_rva();

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);

    IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor =
      fixed_buffer_allocate_unaligned(buffer, IMAGE_IMPORT_DESCRIPTOR);
    *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR) {
      .OriginalFirstThunk = lib->image_thunk_rva,
      .Name = lib->name_rva,
      .FirstThunk = lib->rva,
    };
  }
  result.import_directory_size = get_rva() - result.import_directory_rva;

  // End of IMAGE_IMPORT_DESCRIPTOR list
  *fixed_buffer_allocate_unaligned(buffer, IMAGE_IMPORT_DESCRIPTOR) = (IMAGE_IMPORT_DESCRIPTOR) {0};

  assert(buffer->occupied == expected_encoded_size);

  header->Misc.VirtualSize = u64_to_s32(buffer->occupied);
  header->SizeOfRawData = u64_to_s32(u64_align(buffer->occupied, PE32_FILE_ALIGNMENT));

  return result;
}

typedef struct {
  Fixed_Buffer *buffer;
  s32 entry_point_rva;
} Encoded_Text_Section;

Encoded_Text_Section
encode_text_section(
  Program *program,
  IMAGE_SECTION_HEADER *header
) {
  u64 max_code_size = estimate_max_code_size_in_bytes(program);
  max_code_size = u64_align(max_code_size, PE32_FILE_ALIGNMENT);

  Encoded_Text_Section result = {
    .buffer = fixed_buffer_make(.allocator = allocator_system, .capacity = max_code_size),
  };
  Fixed_Buffer *buffer = result.buffer;

  program->code_base_rva = header->VirtualAddress;

  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    if (builder->value == program->entry_point) {
      result.entry_point_rva = get_rva();
    }
    fn_encode(result.buffer, builder, 0);
  }

  header->Misc.VirtualSize = u64_to_s32(buffer->occupied);
  header->SizeOfRawData = u64_to_s32(u64_align(buffer->occupied, PE32_FILE_ALIGNMENT));

  #undef get_rva
  return result;
}

typedef enum {
  Executable_Type_Gui,
  Executable_Type_Cli,
} Executable_Type;

void
write_executable(
  wchar_t *file_path,
  Program *program,
  Executable_Type executable_type
) {
  assert(program->entry_point);
  // Sections
  IMAGE_SECTION_HEADER sections[] = {
    {
      .Name = ".rdata",
      .Misc = {0},
      .VirtualAddress = 0,
      .SizeOfRawData = 0,
      .PointerToRawData = 0,
      .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
    },
    {
      .Name = ".text",
      .Misc = {0},
      .VirtualAddress = 0,
      .SizeOfRawData = 0,
      .PointerToRawData = 0,
      .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
    },
    {0}
  };

  s32 file_size_of_headers =
    sizeof(IMAGE_DOS_HEADER) +
    sizeof(s32) + // IMAGE_NT_SIGNATURE
    sizeof(IMAGE_FILE_HEADER) +
    sizeof(IMAGE_OPTIONAL_HEADER64) +
    sizeof(sections);

  file_size_of_headers = s32_align(file_size_of_headers, PE32_FILE_ALIGNMENT);
  s32 virtual_size_of_headers = s32_align(file_size_of_headers, PE32_SECTION_ALIGNMENT);

  // Prepare .rdata section
  IMAGE_SECTION_HEADER *rdata_section_header = &sections[0];
  rdata_section_header->PointerToRawData = file_size_of_headers;
  rdata_section_header->VirtualAddress = virtual_size_of_headers;
  Encoded_Rdata_Section encoded_rdata_section = encode_rdata_section(
    program, rdata_section_header
  );
  Fixed_Buffer *rdata_section_buffer = encoded_rdata_section.buffer;

  // Prepare .text section
  IMAGE_SECTION_HEADER *text_section_header = &sections[1];
  text_section_header->PointerToRawData =
    rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;
  text_section_header->VirtualAddress =
    rdata_section_header->VirtualAddress +
    s32_align(rdata_section_header->SizeOfRawData, PE32_SECTION_ALIGNMENT);
  Encoded_Text_Section encoded_text_section = encode_text_section(
    program, text_section_header
  );
  Fixed_Buffer *text_section_buffer = encoded_text_section.buffer;

  // Calculate total size of image in memory once loaded
  s32 virtual_size_of_image =
    text_section_header->VirtualAddress +
    s32_align(text_section_header->SizeOfRawData, PE32_SECTION_ALIGNMENT);

  u64 max_exe_buffer =
    file_size_of_headers +
    rdata_section_header->SizeOfRawData +
    text_section_header->SizeOfRawData;
  Fixed_Buffer *exe_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = max_exe_buffer
  );
  IMAGE_DOS_HEADER *dos_header = fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_DOS_HEADER);

  *dos_header = (IMAGE_DOS_HEADER) {
    .e_magic = IMAGE_DOS_SIGNATURE,
    .e_lfanew = sizeof(IMAGE_DOS_HEADER),
  };
  fixed_buffer_append_s32(exe_buffer, IMAGE_NT_SIGNATURE);

  IMAGE_FILE_HEADER *file_header =
    fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_FILE_HEADER);

  *file_header = (IMAGE_FILE_HEADER) {
    .Machine = IMAGE_FILE_MACHINE_AMD64,
    .NumberOfSections = countof(sections) - 1,
    .TimeDateStamp = (DWORD)time(0),
    .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
    .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
  };

  IMAGE_OPTIONAL_HEADER64 *optional_header =
    fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_OPTIONAL_HEADER64);

  *optional_header = (IMAGE_OPTIONAL_HEADER64) {
    .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
    .SizeOfCode = text_section_header->SizeOfRawData,
    .SizeOfInitializedData = rdata_section_header->SizeOfRawData,
    .AddressOfEntryPoint = encoded_text_section.entry_point_rva,
    .BaseOfCode = text_section_header->VirtualAddress,
    .ImageBase = 0x0000000140000000, // Does not matter as we are using dynamic base
    .SectionAlignment = PE32_SECTION_ALIGNMENT,
    .FileAlignment = PE32_FILE_ALIGNMENT,
    .MajorOperatingSystemVersion = PE32_MIN_WINDOWS_VERSION_VISTA,
    .MinorOperatingSystemVersion = 0,
    .MajorSubsystemVersion = PE32_MIN_WINDOWS_VERSION_VISTA,
    .MinorSubsystemVersion = 0,
    .SizeOfImage = virtual_size_of_image,
    .SizeOfHeaders = file_size_of_headers,
    .Subsystem =
      executable_type == Executable_Type_Cli
        ? IMAGE_SUBSYSTEM_WINDOWS_CUI
        : IMAGE_SUBSYSTEM_WINDOWS_GUI,
    .DllCharacteristics =
      IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA |
      IMAGE_DLLCHARACTERISTICS_NX_COMPAT |
      IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE |
      IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
    .SizeOfStackReserve = 0x100000,
    .SizeOfStackCommit = 0x1000,
    .SizeOfHeapReserve = 0x100000,
    .SizeOfHeapCommit = 0x1000,
    .NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
    .DataDirectory = {0},
  };
  optional_header->DataDirectory[IAT_DIRECTORY_INDEX].VirtualAddress =
    encoded_rdata_section.iat_rva;
  optional_header->DataDirectory[IAT_DIRECTORY_INDEX].Size =
    encoded_rdata_section.iat_size;
  optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].VirtualAddress =
    encoded_rdata_section.import_directory_rva;
  optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].Size =
    encoded_rdata_section.import_directory_size;

  // Write out sections
  for (u32 i = 0; i < countof(sections); ++i) {
    *fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_SECTION_HEADER) = sections[i];
  }

  // .rdata segment
  exe_buffer->occupied = rdata_section_header->PointerToRawData;
  s8 *rdata_memory = fixed_buffer_allocate_bytes(
    exe_buffer, rdata_section_buffer->occupied, sizeof(s8)
  );
  memcpy(rdata_memory, rdata_section_buffer->memory, rdata_section_buffer->occupied);
  exe_buffer->occupied =
    rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;

  // .text segment
  exe_buffer->occupied = text_section_header->PointerToRawData;
  s8 *code_memory = fixed_buffer_allocate_bytes(
    exe_buffer, text_section_buffer->occupied, sizeof(s8)
  );
  memcpy(code_memory, text_section_buffer->memory, text_section_buffer->occupied);
  exe_buffer->occupied =
    text_section_header->PointerToRawData + text_section_header->SizeOfRawData;

  /////////

  HANDLE file = CreateFile(
    file_path,    // name of the write
    GENERIC_WRITE,         // open for writing
    0,                     // do not share
    0,                     // default security
    CREATE_ALWAYS,         // create new file only
    FILE_ATTRIBUTE_NORMAL, // normal file
    0                      // no attr. template
  );

  assert(file != INVALID_HANDLE_VALUE);

  DWORD bytes_written = 0;
  WriteFile(
    file,                 // open file handle
    exe_buffer->memory,    // start of data to write
    (DWORD) exe_buffer->occupied,  // number of bytes to write
    &bytes_written,       // number of bytes that were written
    0
  );

  CloseHandle(file);

  fixed_buffer_destroy(text_section_buffer);
  fixed_buffer_destroy(rdata_section_buffer);
  fixed_buffer_destroy(exe_buffer);
}
