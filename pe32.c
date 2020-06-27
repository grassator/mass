#include "prelude.h"
#include "assert.h"

// ALso contains "Rich" header
const s8 DOS_PROGRAM_BYTES[] = {
  0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 0x21, 0xB8, 0x01, 0x4C,
  0xCD, 0x21, 0x54, 0x68, 0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,
  0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F, 0x74, 0x20, 0x62, 0x65,
  0x20, 0x72, 0x75, 0x6E, 0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
  0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 0x24, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x55, 0x58, 0x04, 0xC7, 0x11, 0x39, 0x6A, 0x94,
  0x11, 0x39, 0x6A, 0x94, 0x11, 0x39, 0x6A, 0x94, 0x4A, 0x51, 0x6B, 0x95,
  0x12, 0x39, 0x6A, 0x94, 0x11, 0x39, 0x6B, 0x94, 0x10, 0x39, 0x6A, 0x94,
  0x97, 0x49, 0x6E, 0x95, 0x10, 0x39, 0x6A, 0x94, 0x97, 0x49, 0x68, 0x95,
  0x10, 0x39, 0x6A, 0x94, 0x52, 0x69, 0x63, 0x68, 0x11, 0x39, 0x6A, 0x94,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00
};

void write_executable() {
  Buffer exe_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  IMAGE_DOS_HEADER *dos_header = buffer_allocate(&exe_buffer, IMAGE_DOS_HEADER);

  *dos_header = (IMAGE_DOS_HEADER) {
    .e_magic = IMAGE_DOS_SIGNATURE,
    .e_cblp = 0x90,      // Bytes on last page of file. What does that do??
    .e_cp = 0x03,        // Pages in file. What does that do??
    .e_cparhdr = 0x04,   // Size of header in paragraphs. What does that do??
    .e_minalloc = 0,
    .e_maxalloc = 0xFFFF,
    .e_sp = 0xB8,        // Initial SP value
    .e_lfarlc = 0x40,    // File address of relocation table
    .e_lfanew = 0xC8,
  };

  s8 *dos_program_bytes = buffer_allocate_size(&exe_buffer, sizeof(DOS_PROGRAM_BYTES));
  memcpy(dos_program_bytes, DOS_PROGRAM_BYTES, sizeof(DOS_PROGRAM_BYTES));

  buffer_append_s32(&exe_buffer, IMAGE_NT_SIGNATURE);

  IMAGE_FILE_HEADER *file_header = buffer_allocate(&exe_buffer, IMAGE_FILE_HEADER);

  *file_header = (IMAGE_FILE_HEADER) {
    .Machine = IMAGE_FILE_MACHINE_AMD64,
    .NumberOfSections = 3,
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
    .NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES, // TODO think about shrinking this if possible
    .DataDirectory = {
      {0}, // Export
      {.VirtualAddress = 0x20F8, .Size = 0x28}, // Import FIXME calculate this address and size
      {0}, // Resource
      {.VirtualAddress = 0x3000, .Size = 0x0C}, // Exception FIXME remove exception info??

      {0}, // Security
      {0}, // Relocation
      {.VirtualAddress = 0x2010, .Size = 0x1C}, // Debug FIXME will take a while to implement
      {0}, // Architecture

      {0}, // Global PTR
      {0}, // TLS
      {0}, // Load Config
      {0}, // Bound Import

      {.VirtualAddress = 0x2000, .Size = 0x10}, // IAT (Import Address Table)  FIXME calculate this address and size
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

  // .pdata section
  IMAGE_SECTION_HEADER *pdata_section_header = buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER);
  *pdata_section_header = (IMAGE_SECTION_HEADER) {
    .Name = ".pdata",
    .Misc = 0x0C, // FIXME size of global data in bytes
    .VirtualAddress = 0x3000, // FIXME calculate this
    .SizeOfRawData = 0x200, // FIXME calculate this
    .PointerToRawData = section_offset,
    .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
  };
  section_offset += pdata_section_header->SizeOfRawData;

  // NULL header telling that the list is done
  *buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER) = (IMAGE_SECTION_HEADER){0};

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
