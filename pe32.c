#include "prelude.h"
#include "assert.h"
#include "value.h"
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

void
fn_encode(
  Buffer *buffer,
  Function_Builder *builder
);

typedef struct {
  Buffer buffer;
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
  #define get_rva() (s32)(header->VirtualAddress + buffer->occupied)

  Encoded_Rdata_Section result = {
    // FIXME dynamically resize the buffer
    .buffer = make_buffer(1024 * 1024, PAGE_READWRITE),
  };

  Buffer *buffer = &result.buffer;

  for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
    Import_Library *lib = array_get(program->import_libraries, i);
    for (s32 i = 0; i < array_count(lib->symbols); ++i) {
      Import_Name_To_Rva *symbol = array_get(lib->symbols, i);
      symbol->name_rva = get_rva();
      buffer_append_s16(buffer, 0); // Ordinal Hint, value not required
      size_t name_size = strlen(symbol->name) + 1;
      s32 aligned_name_size = align((s32)name_size, 2);
      memcpy(
        buffer_allocate_size(buffer, aligned_name_size),
        symbol->name,
        name_size
      );
    }
  }

  result.iat_rva = get_rva();
  for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
    Import_Library *lib = array_get(program->import_libraries, i);
    lib->dll.iat_rva = get_rva();
    for (s32 i = 0; i < array_count(lib->symbols); ++i) {
      Import_Name_To_Rva *fn = array_get(lib->symbols, i);
      fn->iat_rva = get_rva();
      buffer_append_u64(buffer, fn->name_rva);
    }
    // End of IAT list
    buffer_append_u64(buffer, 0);
  }
  result.iat_size = (s32)buffer->occupied;

  // Image thunks
  for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
    Import_Library *lib = array_get(program->import_libraries, i);
    lib->image_thunk_rva = get_rva();

    for (s32 i = 0; i < array_count(lib->symbols); ++i) {
      Import_Name_To_Rva *fn = array_get(lib->symbols, i);
      buffer_append_u64(buffer, fn->name_rva);
    }
    // End of IAT list
    buffer_append_u64(buffer, 0);
  }

  buffer_append_s64(buffer, 0);

  // Library Names

  for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
    Import_Library *lib = array_get(program->import_libraries, i);
    lib->dll.name_rva = get_rva();
    size_t name_size = strlen(lib->dll.name) + 1;
    s32 aligned_name_size = align((s32)name_size, 2);
    memcpy(
      buffer_allocate_size(buffer, aligned_name_size),
      lib->dll.name,
      name_size
    );
  }

  // Import Directory
  result.import_directory_rva = get_rva();

  for (s64 i = 0; i < array_count(program->import_libraries); ++i) {
    Import_Library *lib = array_get(program->import_libraries, i);

    IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor =
      buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR);
    *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR) {
      .OriginalFirstThunk = lib->image_thunk_rva,
      .Name = lib->dll.name_rva,
      .FirstThunk = lib->dll.iat_rva,
    };
  }
  result.import_directory_size = get_rva() - result.import_directory_rva;

  // End of IMAGE_IMPORT_DESCRIPTOR list
  *buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR) = (IMAGE_IMPORT_DESCRIPTOR) {0};

  header->Misc.VirtualSize = (s32)buffer->occupied;
  header->SizeOfRawData = (s32)align_u64(buffer->occupied, PE32_FILE_ALIGNMENT);

  return result;
  #undef get_rva
}

void
write_executable(
  Program *program
) {
  u64 max_code_size = estimate_max_code_size_in_bytes(program);
  max_code_size = align_u64(max_code_size, PE32_FILE_ALIGNMENT);

  assert(fits_into_s32(max_code_size));
  s32 max_code_size_s32 = (s32)max_code_size;

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
      .Misc = { .VirtualSize = 0x10 }, // FIXME size of machine code in bytes
      .VirtualAddress = 0,
      .SizeOfRawData = max_code_size_s32,
      .PointerToRawData = 0,
      .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
    },
    {0}
  };

  IMAGE_SECTION_HEADER *rdata_section_header = &sections[0];
  IMAGE_SECTION_HEADER *text_section_header = &sections[1];

  s32 file_size_of_headers =
    sizeof(IMAGE_DOS_HEADER) +
    sizeof(s32) + // IMAGE_NT_SIGNATURE
    sizeof(IMAGE_FILE_HEADER) +
    sizeof(IMAGE_OPTIONAL_HEADER64) +
    sizeof(sections);

  file_size_of_headers = align(file_size_of_headers, PE32_FILE_ALIGNMENT);
  s32 virtual_size_of_headers = align(file_size_of_headers, PE32_SECTION_ALIGNMENT);

  rdata_section_header->VirtualAddress = virtual_size_of_headers;
  Encoded_Rdata_Section encoded_rdata_section = encode_rdata_section(
    program, rdata_section_header
  );
  Buffer rdata_section_buffer = encoded_rdata_section.buffer;

  s32 virtual_size_of_image = 0;
  {
    // Update offsets for sections
    s32 file_section_offset = file_size_of_headers;
    s32 virtual_section_offset = virtual_size_of_headers;

    // -1 makes sure we do not write anything in zero-termination element
    for (u32 i = 0; i < static_array_size(sections) - 1; ++i) {
      sections[i].PointerToRawData = file_section_offset;
      file_section_offset += sections[i].SizeOfRawData;

      sections[i].VirtualAddress = virtual_section_offset;
      virtual_section_offset += align(sections[i].SizeOfRawData, PE32_SECTION_ALIGNMENT);
    }
    virtual_size_of_image = virtual_section_offset;
  }

  // TODO this should be dynamically sized or correctly estimated
  Buffer exe_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  IMAGE_DOS_HEADER *dos_header = buffer_allocate(&exe_buffer, IMAGE_DOS_HEADER);

  *dos_header = (IMAGE_DOS_HEADER) {
    .e_magic = IMAGE_DOS_SIGNATURE,
    .e_lfanew = sizeof(IMAGE_DOS_HEADER),
  };
  buffer_append_s32(&exe_buffer, IMAGE_NT_SIGNATURE);

  IMAGE_FILE_HEADER *file_header = buffer_allocate(&exe_buffer, IMAGE_FILE_HEADER);

  *file_header = (IMAGE_FILE_HEADER) {
    .Machine = IMAGE_FILE_MACHINE_AMD64,
    .NumberOfSections = static_array_size(sections) - 1,
    .TimeDateStamp = (DWORD)time(0),
    .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
    .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
  };

  IMAGE_OPTIONAL_HEADER64 *optional_header = buffer_allocate(&exe_buffer, IMAGE_OPTIONAL_HEADER64);

  *optional_header = (IMAGE_OPTIONAL_HEADER64) {
    .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
    .SizeOfCode = text_section_header->SizeOfRawData,
    .SizeOfInitializedData = 0,
    .AddressOfEntryPoint = 0,
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
    .Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI, // TODO allow user to specify this
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
  optional_header->SizeOfInitializedData = (s32)encoded_rdata_section.buffer.occupied;

  // Write out sections
  for (u32 i = 0; i < static_array_size(sections); ++i) {
    *buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER) = sections[i];
  }

  #define file_offset_to_rva(_section_header_)\
    ((s32)exe_buffer.occupied - \
     (_section_header_)->PointerToRawData + \
     (_section_header_)->VirtualAddress)


  // .rdata segment
  exe_buffer.occupied = rdata_section_header->PointerToRawData;
  s8 *rdata_memory = buffer_allocate_size(&exe_buffer, rdata_section_buffer.occupied);
  memcpy(rdata_memory, rdata_section_buffer.memory, rdata_section_buffer.occupied);

  u64 actual_size_of_rdata = (exe_buffer.occupied - rdata_section_header->PointerToRawData);
  assert(actual_size_of_rdata == 0x6c);

  exe_buffer.occupied =
    rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;

  // .text segment
  exe_buffer.occupied = text_section_header->PointerToRawData;

  program->code_base_file_offset = text_section_header->PointerToRawData;
  program->code_base_rva = text_section_header->VirtualAddress;

  for (
    Function_Builder *builder = array_begin(program->functions);
    builder != array_end(program->functions);
    ++builder
  ) {
    if (builder == program->entry_point) {
      optional_header->AddressOfEntryPoint = file_offset_to_rva(text_section_header);
    }
    fn_encode(&exe_buffer, builder);
  }

  exe_buffer.occupied =
    text_section_header->PointerToRawData + text_section_header->SizeOfRawData;

  /////////

  HANDLE file = CreateFile(
    L"build\\test.exe",    // name of the write
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
    exe_buffer.memory,    // start of data to write
    (DWORD) exe_buffer.occupied,  // number of bytes to write
    &bytes_written,       // number of bytes that were written
    0
  );

  CloseHandle(file);

  free_buffer(&rdata_section_buffer);
  free_buffer(&exe_buffer);
}
