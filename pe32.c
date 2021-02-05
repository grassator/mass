#include "prelude.h"
#include "assert.h"
#include "value.h"
#include "function.h"
#include "win32_platform.h"
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
  u32 name_rva;
  u32 rva;
  u32 image_thunk_rva;
  Array_u32 symbol_rvas;
} Import_Library_Pe32;
typedef dyn_array_type(Import_Library_Pe32) Array_Import_Library_Pe32;

typedef struct {
  Virtual_Memory_Buffer *buffer;
  s32 iat_rva;
  s32 iat_size;
  s32 import_directory_rva;
  s32 import_directory_size;

  s32 exception_directory_rva;
  s32 exception_directory_size;
} Encoded_Data_Section;

Encoded_Data_Section
encode_data_section(
  Program * program,
  IMAGE_SECTION_HEADER *header,
  Array_Function_Layout layouts
) {
  #define get_rva() s64_to_s32(s32_to_s64(header->VirtualAddress) + u64_to_s64(buffer->occupied))

  Section *section = &program->memory.sections.data;
  section->base_rva = header->VirtualAddress;

  Encoded_Data_Section result = {
    .buffer = &section->buffer,
  };

  Virtual_Memory_Buffer *buffer = result.buffer;

  Bucket_Buffer *temp_buffer = bucket_buffer_make();
  Allocator *temp_allocator = bucket_buffer_allocator_make(temp_buffer);
  Array_Import_Library_Pe32 pe32_libraries =
    dyn_array_make(Array_Import_Library_Pe32, .allocator = temp_allocator);
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    dyn_array_push(pe32_libraries, (Import_Library_Pe32){
      .symbol_rvas = dyn_array_make(
        Array_u32,
        .allocator = temp_allocator,
        .capacity = dyn_array_length(lib->symbols),
      ),
    });
  }

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    Import_Library_Pe32 *pe32_lib = dyn_array_get(pe32_libraries, i);
    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, symbol_index);
      dyn_array_push(pe32_lib->symbol_rvas, get_rva());
      virtual_memory_buffer_append_s16(buffer, 0); // Ordinal Hint, value not required
      u64 name_size = symbol->name.length;
      u64 aligned_name_size = u64_align(name_size + 1, 2);
      memcpy(
        virtual_memory_buffer_allocate_bytes(buffer, aligned_name_size, sizeof(s8)),
        symbol->name.bytes,
        name_size
      );
    }
  }

  result.iat_rva = get_rva();
  // IAT list
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    Import_Library_Pe32 *pe32_lib = dyn_array_get(pe32_libraries, i);
    pe32_lib->rva = get_rva();
    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      Import_Symbol *fn = dyn_array_get(lib->symbols, symbol_index);
      u32 offset = get_rva() - header->VirtualAddress;
      program_set_label_offset(program, fn->label32, offset);
      u32 symbol_rva = *dyn_array_get(pe32_lib->symbol_rvas, symbol_index);
      virtual_memory_buffer_append_u64(buffer, symbol_rva);
    }

    virtual_memory_buffer_append_u64(buffer, 0);
  }
  result.iat_size = (s32)buffer->occupied;

  // Image thunks
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    Import_Library_Pe32 *pe32_lib = dyn_array_get(pe32_libraries, i);
    pe32_lib->image_thunk_rva = get_rva();

    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      u32 symbol_rva = *dyn_array_get(pe32_lib->symbol_rvas, symbol_index);
      virtual_memory_buffer_append_u64(buffer, symbol_rva);
    }
    // End of Image thunks list
    virtual_memory_buffer_append_u64(buffer, 0);
  }

  // Library Names
  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    Import_Library_Pe32 *pe32_lib = dyn_array_get(pe32_libraries, i);
    pe32_lib->name_rva = get_rva();
    u64 name_size = lib->name.length;
    u64 aligned_name_size = u64_align(name_size + 1, 2);
    memcpy(
      virtual_memory_buffer_allocate_bytes(buffer, aligned_name_size, sizeof(s8)),
      lib->name.bytes,
      name_size
    );
  }

  // Import Directory
  result.import_directory_rva = get_rva();

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library_Pe32 *pe32_lib = dyn_array_get(pe32_libraries, i);

    IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor =
      virtual_memory_buffer_allocate_unaligned(buffer, IMAGE_IMPORT_DESCRIPTOR);
    *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR) {
      .OriginalFirstThunk = pe32_lib->image_thunk_rva,
      .Name = pe32_lib->name_rva,
      .FirstThunk = pe32_lib->rva,
    };
  }
  result.import_directory_size = get_rva() - result.import_directory_rva;

  if (result.import_directory_size) {
    // End of IMAGE_IMPORT_DESCRIPTOR list
    *virtual_memory_buffer_allocate_unaligned(buffer, IMAGE_IMPORT_DESCRIPTOR) =
      (IMAGE_IMPORT_DESCRIPTOR) {0};
  }

  // Exception Directory
  {
    u64 size = sizeof(RUNTIME_FUNCTION) * dyn_array_length(program->functions);
    result.exception_directory_rva = get_rva();
    result.exception_directory_size = u64_to_s32(size);
    RUNTIME_FUNCTION *functions =
      virtual_memory_buffer_allocate_bytes(&section->buffer, size, _Alignof(RUNTIME_FUNCTION));
    for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
      Function_Builder *builder = dyn_array_get(program->functions, i);
      RUNTIME_FUNCTION *function = &functions[i];
      Function_Layout *layout = dyn_array_get(layouts, i);
      win32_init_runtime_info_for_function(builder, layout, function, section);
    }
    dyn_array_destroy(layouts);
  }

  header->Misc.VirtualSize = u64_to_s32(buffer->occupied);
  header->SizeOfRawData = u64_to_s32(u64_align(buffer->occupied, PE32_FILE_ALIGNMENT));

  bucket_buffer_destroy(temp_buffer);

  return result;
}

typedef struct {
  Virtual_Memory_Buffer buffer;
  s32 entry_point_rva;
  Array_Function_Layout layouts;
} Encoded_Text_Section;

Encoded_Text_Section
encode_text_section(
  Execution_Context *context,
  IMAGE_SECTION_HEADER *header
) {
  Program *program = context->program;
  u64 max_code_size = estimate_max_code_size_in_bytes(program);
  max_code_size = u64_align(max_code_size, PE32_FILE_ALIGNMENT);

  Encoded_Text_Section result = {0};
  virtual_memory_buffer_init(&result.buffer, max_code_size);
  Virtual_Memory_Buffer *buffer = &result.buffer;

  program->memory.sections.code.base_rva = header->VirtualAddress;

  bool found_entry_point = false;

  result.layouts =
    dyn_array_make(Array_Function_Layout, .capacity = dyn_array_length(program->functions));
  assert(program->entry_point->descriptor->tag == Descriptor_Tag_Function);

  for (u64 i = 0; i < dyn_array_length(program->functions); ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    if (builder->function == &program->entry_point->descriptor->Function) {
      result.entry_point_rva = get_rva();
      found_entry_point = true;
    }
    Function_Layout *layout = dyn_array_push_uninitialized(result.layouts);
    fn_encode(program, buffer, builder, layout);
  }

  if (!found_entry_point) {
    panic("Internal error: Could not find entry point in the list of program functions");
  }

  header->Misc.VirtualSize = u64_to_s32(buffer->occupied);
  header->SizeOfRawData = u64_to_s32(u64_align(buffer->occupied, PE32_FILE_ALIGNMENT));


  #undef get_rva
  return result;
}

static u32
win32_section_permissions_to_pe32_section_characteristics(
  Section_Permissions permissions
) {
  u32 result = 0;
  if (permissions & Section_Permissions_Execute) {
    // TODO What does CNT_CODE actually do?
    result |= IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE;
  } else {
    // TODO What does initialized data actually do?
    result |= IMAGE_SCN_CNT_INITIALIZED_DATA;
  }
  if (permissions & Section_Permissions_Write) {
    result |= IMAGE_SCN_MEM_WRITE;
  } else if (permissions & Section_Permissions_Read) {
    result |= IMAGE_SCN_MEM_READ;
  }
  return result;
}

typedef enum {
  Executable_Type_Gui,
  Executable_Type_Cli,
} Executable_Type;

typedef struct {
  s32 file;
  s32 virtual;
} PE32_Offsets;

static inline PE32_Offsets
pe32_offset_after_size(
  const PE32_Offsets *offsets,
  s32 size
) {
  return (PE32_Offsets) {
    .file = offsets->file + s32_align(size, PE32_FILE_ALIGNMENT),
    .virtual = offsets->virtual + s32_align(size, PE32_SECTION_ALIGNMENT),
  };
}

void
pe32_checksum(
  Fixed_Buffer *buffer,
  IMAGE_OPTIONAL_HEADER64 *header
) {
  // Unaligned checksum would require some modification
  // however since PE32+ executables are always section-aligned
  // we can just assert for sanity.
  assert(buffer->occupied % sizeof(u32) == 0);

  u32 *chunk = (u32 *)buffer->memory;
  u32 *chunk_end = (u32 *)(buffer->memory + buffer->occupied);

  u64 checksum = 0;
  for (; chunk != chunk_end; ++chunk) {
    // Checksum does not include itself so need to skip over 4 bytes
    // where it will be written to in the output file
    if (chunk == &header->CheckSum) continue;
    checksum = (checksum & 0xffffffff) + (*chunk) + (checksum >> 32);
    if (checksum > (1llu << 32)) {
      checksum = (checksum & 0xffffffff) + (checksum >> 32);
    }
  }
  checksum = (checksum & 0xffff) + (checksum >> 16);
  checksum = (checksum) + (checksum >> 16);
  checksum = checksum & 0xffff;

  checksum += buffer->occupied;

  header->CheckSum = u64_to_u32(checksum);
}

void
write_executable(
  const char *file_path,
  Execution_Context *context,
  Executable_Type executable_type
) {
  Program *program = context->program;
  assert(program->entry_point);
  // Sections
  IMAGE_SECTION_HEADER sections[] = {
    {
      .Name = ".text",
      .Misc = {0},
      .VirtualAddress = 0,
      .SizeOfRawData = 0,
      .PointerToRawData = 0,
      .Characteristics = win32_section_permissions_to_pe32_section_characteristics(
        program->memory.sections.code.permissions
      ),
    },
    {
      .Name = ".data",
      .Misc = {0},
      .VirtualAddress = 0,
      .SizeOfRawData = 0,
      .PointerToRawData = 0,
      .Characteristics = win32_section_permissions_to_pe32_section_characteristics(
        program->memory.sections.data.permissions
      ),
    },
    {0}
  };

  s32 file_size_of_headers =
    sizeof(IMAGE_DOS_HEADER) +
    sizeof(s32) + // IMAGE_NT_SIGNATURE
    sizeof(IMAGE_FILE_HEADER) +
    sizeof(IMAGE_OPTIONAL_HEADER64) +
    sizeof(sections);

  PE32_Offsets offsets = {0};

  offsets = pe32_offset_after_size(&offsets, file_size_of_headers);

  // Prepare .text section
  IMAGE_SECTION_HEADER *text_section_header = &sections[0];
  text_section_header->PointerToRawData = offsets.file;
  text_section_header->VirtualAddress = offsets.virtual;
  Encoded_Text_Section encoded_text_section = encode_text_section(context, text_section_header);
  Virtual_Memory_Buffer *text_section_buffer = &encoded_text_section.buffer;
  offsets = pe32_offset_after_size(&offsets, text_section_header->SizeOfRawData);

  // Prepare .data section
  IMAGE_SECTION_HEADER *data_section_header = &sections[1];
  data_section_header->PointerToRawData = offsets.file;
  data_section_header->VirtualAddress = offsets.virtual;
  Encoded_Data_Section encoded_data_section = encode_data_section(
    program, data_section_header, encoded_text_section.layouts
  );
  Virtual_Memory_Buffer *data_section_buffer = encoded_data_section.buffer;
  offsets = pe32_offset_after_size(&offsets, data_section_header->SizeOfRawData);

  // After all the sections are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Calculate total size of image in memory once loaded
  s32 virtual_size_of_image = offsets.virtual;

  u64 max_exe_buffer = offsets.file;
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
    .TimeDateStamp = (u32)time(0),
    .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
    .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
  };

  IMAGE_OPTIONAL_HEADER64 *optional_header =
    fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_OPTIONAL_HEADER64);

  *optional_header = (IMAGE_OPTIONAL_HEADER64) {
    .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
    .SizeOfCode = text_section_header->SizeOfRawData,
    .SizeOfInitializedData = data_section_header->SizeOfRawData,
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

  // :ZeroSizeDirectory
  // If the directory is empty, the RVA also must be 0
  if (encoded_data_section.iat_size) {
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].VirtualAddress =
      encoded_data_section.iat_rva;
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].Size =
      encoded_data_section.iat_size;
  }

  // :ZeroSizeDirectory
  // If the directory is empty, the RVA also must be 0
  if (encoded_data_section.import_directory_size) {
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].VirtualAddress =
      encoded_data_section.import_directory_rva;
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].Size =
      encoded_data_section.import_directory_size;
  }

  optional_header->DataDirectory[EXCEPTION_DIRECTORY_INDEX].VirtualAddress =
    encoded_data_section.exception_directory_rva;
  optional_header->DataDirectory[EXCEPTION_DIRECTORY_INDEX].Size =
    encoded_data_section.exception_directory_size;

  // Write out sections
  for (u32 i = 0; i < countof(sections); ++i) {
    *fixed_buffer_allocate_unaligned(exe_buffer, IMAGE_SECTION_HEADER) = sections[i];
  }

  // .text segment
  exe_buffer->occupied = text_section_header->PointerToRawData;
  s8 *code_memory = fixed_buffer_allocate_bytes(
    exe_buffer, text_section_buffer->occupied, sizeof(s8)
  );
  memcpy(code_memory, text_section_buffer->memory, text_section_buffer->occupied);

  // .data segment
  exe_buffer->occupied = data_section_header->PointerToRawData;
  s8 *data_memory = fixed_buffer_allocate_bytes(
    exe_buffer, data_section_buffer->occupied, sizeof(s8)
  );
  memcpy(data_memory, data_section_buffer->memory, data_section_buffer->occupied);

  // Set the occupied to the expected end of file to ensure
  // correct alignment of the file size
  exe_buffer->occupied = offsets.file;
  assert(exe_buffer->occupied % PE32_FILE_ALIGNMENT == 0);

  pe32_checksum(exe_buffer, optional_header);

  /////////

  FILE *file = fopen(file_path, "wb");
  assert(file);
  fwrite(exe_buffer->memory, 1, exe_buffer->occupied, file);
  fclose(file);

  fixed_buffer_destroy(exe_buffer);
}
