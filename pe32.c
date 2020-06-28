#include "prelude.h"
#include "assert.h"

void write_executable() {
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
    .NumberOfSections = 2,
    .TimeDateStamp = 0x5EF48E56, // FIXME generate ourselves
    .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
    .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
  };

  IMAGE_OPTIONAL_HEADER64 *optional_header = buffer_allocate(&exe_buffer, IMAGE_OPTIONAL_HEADER64);

  *optional_header = (IMAGE_OPTIONAL_HEADER64) {
    .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
    .MajorLinkerVersion = 0x0E, // FIXME remove or replace once initial implementation is done
    .MinorLinkerVersion = 0x1A, // FIXME remove or replace once initial implementation is done
    .SizeOfCode = 0x200, // FIXME calculate based on the amount of machine code
    .SizeOfInitializedData = 0x400, // FIXME calculate based on the amount of global data
    .SizeOfUninitializedData = 0, // FIXME figure out difference between initialed and uninitialized
    .AddressOfEntryPoint = 0x1000, // FIXME resolve to the entry point in the machine code
    .BaseOfCode = 0x1000, // FIXME resolve to the right section containing code
    .ImageBase = 0x0000000140000000, // TODO figure out if we should change this
    .SectionAlignment = 0x1000,
    .FileAlignment = 0x200,
    .MajorOperatingSystemVersion = 6, // FIXME figure out if can be not hard coded
    .MinorOperatingSystemVersion = 0,
    .MajorSubsystemVersion = 6, // FIXME figure out if can be not hard coded
    .MinorSubsystemVersion = 0,
    .SizeOfImage = 0x4000, // FIXME calculate based on the sizes of the sections
    .SizeOfHeaders = 0x400, // FIXME calculate correctly (as described in MSDN)
    .Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI, // TODO allow user to specify this
    .DllCharacteristics =
      IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA |
      IMAGE_DLLCHARACTERISTICS_NX_COMPAT | // TODO figure out what NX is
      IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE |
      IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
    .SizeOfStackReserve = 0x100000,
    .SizeOfStackCommit = 0x1000,
    .SizeOfHeapReserve = 0x100000,
    .SizeOfHeapCommit = 0x1000,
    .NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
    .DataDirectory = {
      {0}, // Export
      {.VirtualAddress = 0x20F8, .Size = 0x28}, // Import FIXME calculate this address and size
      {0}, // Resource
      {0}, // Exception

      {0}, // Security
      {0}, // Relocation
      {.VirtualAddress = 0x2010, .Size = 0x1C}, // Debug FIXME will take a while to implement
      {0}, // Architecture

      {0}, // Global PTR
      {0}, // TLS
      {0}, // Load Config
      {0}, // Bound Import

      {.VirtualAddress = 0x2000, .Size = 0x10}, // IAT (Import Address Table)  FIXME calculate this
      {0}, // Delay Import
      {0}, // CLR
      {0}, // Reserved
    },
  };

  u32 section_offset = optional_header->SizeOfHeaders;

  // .text section
  IMAGE_SECTION_HEADER *text_section_header = buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER);
  *text_section_header = (IMAGE_SECTION_HEADER) {
    .Name = ".text",
    .Misc = 0x10, // FIXME size of machine code in bytes
    .VirtualAddress = optional_header->BaseOfCode,
    .SizeOfRawData = optional_header->SizeOfCode,
    .PointerToRawData = section_offset,
    .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
  };
  section_offset += text_section_header->SizeOfRawData;

  // .rdata section
  IMAGE_SECTION_HEADER *rdata_section_header = buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER);
  *rdata_section_header = (IMAGE_SECTION_HEADER) {
    .Name = ".rdata",
    .Misc = 0x14C, // FIXME size of machine code in bytes
    .VirtualAddress = 0x2000, // FIXME calculate this
    .SizeOfRawData = 0x200, // FIXME calculate this
    .PointerToRawData = section_offset,
    .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
  };
  section_offset += rdata_section_header->SizeOfRawData;

  // NULL header telling that the list is done
  *buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER) = (IMAGE_SECTION_HEADER){0};

  // .text segment
  assert(exe_buffer.occupied < text_section_header->PointerToRawData);
  exe_buffer.occupied = text_section_header->PointerToRawData;

  buffer_append_s8(&exe_buffer, 0x48); // sub rsp 28
  buffer_append_s8(&exe_buffer, 0x83);
  buffer_append_s8(&exe_buffer, 0xEC);
  buffer_append_s8(&exe_buffer, 0x28);
  buffer_append_s8(&exe_buffer, 0xB9);
  buffer_append_s8(&exe_buffer, 0x2A);
  buffer_append_s8(&exe_buffer, 0x00);
  buffer_append_s8(&exe_buffer, 0x00);
  buffer_append_s8(&exe_buffer, 0x00);

  buffer_append_s8(&exe_buffer, 0xFF); // call
  buffer_append_s8(&exe_buffer, 0x15);
  buffer_append_s8(&exe_buffer, 0xF1);
  buffer_append_s8(&exe_buffer, 0x0F);
  buffer_append_s8(&exe_buffer, 0x00);
  buffer_append_s8(&exe_buffer, 0x00);

  buffer_append_s8(&exe_buffer, 0xCC); // int3

  // .rdata segment
  exe_buffer.occupied = rdata_section_header->PointerToRawData;
  s32 iat_rva = 0x2130; // FIXME calculate this
  buffer_append_s32(&exe_buffer, iat_rva);


  exe_buffer.occupied = rdata_section_header->PointerToRawData + 0x10; // FIXME do not hardcode this
  s8 debug_bytes[] = {
    0x00, 0x00, 0x00, 0x00, 0x56, 0x8E, 0xF4, 0x5E, 0x00, 0x00,
    0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0xC4, 0x00, 0x00, 0x00,
    0x2C, 0x20, 0x00, 0x00, 0x2C, 0x06, 0x00, 0x00
  };
  assert(static_array_size(debug_bytes) == 0x1C);
  for (s32 i = 0; i < static_array_size(debug_bytes); ++i) {
    buffer_append_s8(&exe_buffer, debug_bytes[i]);
  }

  // .idata?
  exe_buffer.occupied = rdata_section_header->PointerToRawData + 0xF8;

  IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor =
    buffer_allocate(&exe_buffer, IMAGE_IMPORT_DESCRIPTOR);
  *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR) {
    .OriginalFirstThunk = 0x2120,
    .Name = 0x213E,
    .FirstThunk = 0x2000,
  };

  exe_buffer.occupied = rdata_section_header->PointerToRawData + 0x120;
  IMAGE_THUNK_DATA64 *image_thunk =
    buffer_allocate(&exe_buffer, IMAGE_THUNK_DATA64);
  *image_thunk = (IMAGE_THUNK_DATA64) {0x2130};

  {
    exe_buffer.occupied = rdata_section_header->PointerToRawData + 0x130;
    buffer_append_s16(&exe_buffer, 0x0164); // TODO set to zero
    s8 function_name[] = "ExitProcess";

    s32 aligned_function_name_size = align(static_array_size(function_name), 2);
    memcpy(
      buffer_allocate_size(&exe_buffer, aligned_function_name_size),
      function_name,
      static_array_size(function_name)
    );
  }

  {
    exe_buffer.occupied = rdata_section_header->PointerToRawData + 0x13E;
    s8 library_name[] = "KERNEL32.DLL";

    s32 aligned_name_size = align(static_array_size(library_name), 2);
    memcpy(
      buffer_allocate_size(&exe_buffer, aligned_name_size),
      library_name,
      static_array_size(library_name)
    );
  }
  exe_buffer.occupied =
    rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;

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
}
