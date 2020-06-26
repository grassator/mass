#include "prelude.h"
#include "assert.h"

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

  /////////

  HANDLE file = CreateFile(
     L"build\\test.exe",       // name of the write
     GENERIC_WRITE,          // open for writing
     0,                      // do not share
     NULL,                   // default security
     CREATE_ALWAYS,          // create new file only
     FILE_ATTRIBUTE_NORMAL,  // normal file
    NULL                    // no attr. template
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
