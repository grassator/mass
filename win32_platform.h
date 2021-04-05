#ifndef WIN32_PLATFORM_H
#define WIN32_PLATFORM_H

#include "value.h"

#ifndef _WIN32

#define IMAGE_SCN_CNT_CODE 0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x00000080

#define IMAGE_SCN_MEM_EXECUTE 0x20000000
#define IMAGE_SCN_MEM_READ 0x40000000
#define IMAGE_SCN_MEM_WRITE 0x80000000

#define IMAGE_SUBSYSTEM_WINDOWS_GUI 2
#define IMAGE_SUBSYSTEM_WINDOWS_CUI 3

#define IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA 0x0020
#define IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE 0x0040
#define IMAGE_DLLCHARACTERISTICS_NX_COMPAT 0x0100
#define IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE 0x8000

#define IMAGE_FILE_MACHINE_I386 0x014c
#define IMAGE_FILE_MACHINE_AMD64 0x8664

#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020

#define IMAGE_DOS_SIGNATURE 0x5A4D
#define IMAGE_NT_SIGNATURE 0x4550
#define IMAGE_NT_OPTIONAL_HDR64_MAGIC 0x20b

#pragma pack(push,1)

typedef struct _IMAGE_DATA_DIRECTORY {
    u32   VirtualAddress;
    u32   Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16

typedef struct _IMAGE_FILE_HEADER {
  u16 Machine;
  u16 NumberOfSections;
  u32 TimeDateStamp;
  u32 PointerToSymbolTable;
  u32 NumberOfSymbols;
  u16 SizeOfOptionalHeader;
  u16 Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;

typedef struct _IMAGE_OPTIONAL_HEADER64 {
  u16  Magic;
  u8  MajorLinkerVersion;
  u8  MinorLinkerVersion;
  u32 SizeOfCode;
  u32 SizeOfInitializedData;
  u32 SizeOfUninitializedData;
  u32 AddressOfEntryPoint;
  u32 BaseOfCode;
  u64 ImageBase;
  u32 SectionAlignment;
  u32 FileAlignment;
  u16 MajorOperatingSystemVersion;
  u16 MinorOperatingSystemVersion;
  u16 MajorImageVersion;
  u16 MinorImageVersion;
  u16 MajorSubsystemVersion;
  u16 MinorSubsystemVersion;
  u32 Win32VersionValue;
  u32 SizeOfImage;
  u32 SizeOfHeaders;
  u32 CheckSum;
  u16 Subsystem;
  u16 DllCharacteristics;
  u64 SizeOfStackReserve;
  u64 SizeOfStackCommit;
  u64 SizeOfHeapReserve;
  u64 SizeOfHeapCommit;
  u32 LoaderFlags;
  u32 NumberOfRvaAndSizes;
  IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;

typedef struct _IMAGE_IMPORT_DESCRIPTOR {
  union {
    u32 Characteristics;
    u32 OriginalFirstThunk;
  };
  u32 TimeDateStamp;
  u32 ForwarderChain;
  u32 Name;
  u32 FirstThunk;
} IMAGE_IMPORT_DESCRIPTOR;
typedef IMAGE_IMPORT_DESCRIPTOR *PIMAGE_IMPORT_DESCRIPTOR;

typedef struct _IMAGE_DOS_HEADER {
  u16 e_magic;
  u16 e_cblp;
  u16 e_cp;
  u16 e_crlc;
  u16 e_cparhdr;
  u16 e_minalloc;
  u16 e_maxalloc;
  u16 e_ss;
  u16 e_sp;
  u16 e_csum;
  u16 e_ip;
  u16 e_cs;
  u16 e_lfarlc;
  u16 e_ovno;
  u16 e_res[4];
  u16 e_oemid;
  u16 e_oeminfo;
  u16 e_res2[10];
  s32 e_lfanew;
} IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;

#define IMAGE_SIZEOF_SHORT_NAME 8
// https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-image_section_header
typedef struct _IMAGE_SECTION_HEADER {
  u8 Name[IMAGE_SIZEOF_SHORT_NAME];
  union {
    u32 PhysicalAddress;
    u32 VirtualSize;
  } Misc;
  u32 VirtualAddress;
  u32 SizeOfRawData;
  u32 PointerToRawData;
  u32 PointerToRelocations;
  u32 PointerToLinenumbers;
  u16 NumberOfRelocations;
  u16 NumberOfLinenumbers;
  u32 Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;


#pragma pack(pop)

// https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-160#struct-runtime_function
typedef struct RUNTIME_FUNCTION {
  u32 BeginAddress;
  u32 EndAddress;
  u32 UnwindData;
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif

// https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64?view=vs-2019#unwind-data-definitions-in-c
typedef enum _UNWIND_OP_CODES {
  UWOP_PUSH_NONVOL = 0, /* info == register number */
  UWOP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
  UWOP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
  UWOP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
  UWOP_SAVE_NONVOL,     /* info == register number, offset in next slot */
  UWOP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
  UWOP_SAVE_XMM128 = 8, /* info == XMM reg number, offset in next slot */
  UWOP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
  UWOP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UNWIND_CODE_OPS;

typedef union {
  struct {
    u8 CodeOffset;
    u8 UnwindOp : 4;
    u8 OpInfo   : 4;
  };
  u16 FrameOffset;
  u16 DataForPreviousCode;
} UNWIND_CODE;

typedef struct {
  u8 Version       : 3;
  u8 Flags         : 5;
  u8 SizeOfProlog;
  u8 CountOfCodes;
  u8 FrameRegister : 4;
  u8 FrameOffset   : 4;
  // :UnwindCodeAlignment
  // This struct must be DWORD (32bit) aligned which means
  // there is always an even unwind codes allocated
  UNWIND_CODE UnwindCode[];
} UNWIND_INFO;

UNWIND_INFO *
win32_init_runtime_info_for_function(
  const Function_Builder *builder,
  const Function_Layout *layout,
  RUNTIME_FUNCTION *function,
  Section *section
) {
  u32 unwind_data_rva = u64_to_u32(section->base_rva + section->buffer.occupied);
  UNWIND_INFO *unwind_info = virtual_memory_buffer_allocate_bytes(
    &section->buffer, sizeof(UNWIND_INFO), sizeof(u32)
  );
  *unwind_info = (UNWIND_INFO) {
    .Version = 1,
    .Flags = 0,
    .SizeOfProlog = layout->size_of_prolog,
    .CountOfCodes = 0,
    .FrameRegister = 0,
    .FrameOffset = 0,
  };

  #define WIN32_WRITE_UNWIND_CODE(...)\
    do {\
      *virtual_memory_buffer_allocate(&section->buffer, UNWIND_CODE) = (__VA_ARGS__);\
      unwind_code_index++;\
    } while(0)

  // :Win32UnwindCodes Must match what happens in the function encoding
  u8 unwind_code_index = 0;
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
        WIN32_WRITE_UNWIND_CODE((UNWIND_CODE) {
          .CodeOffset = layout->volatile_register_push_offsets[unwind_code_index],
          .UnwindOp = UWOP_PUSH_NONVOL,
          .OpInfo = s32_to_u8(reg_index),
        });
      }
    }
  }

  if (layout->stack_reserve) {
    assert(layout->stack_reserve >= 8);
    assert(layout->stack_reserve % 8 == 0);
    if (layout->stack_reserve <= 128) {
      WIN32_WRITE_UNWIND_CODE((UNWIND_CODE){
        .CodeOffset = layout->stack_allocation_offset_in_prolog,
        .UnwindOp = UWOP_ALLOC_SMALL,
        .OpInfo = (layout->stack_reserve - 8) / 8,
      });
    } else {
      WIN32_WRITE_UNWIND_CODE((UNWIND_CODE){
        .CodeOffset = layout->stack_allocation_offset_in_prolog,
        .UnwindOp = UWOP_ALLOC_LARGE,
        .OpInfo = 0,
      });
      WIN32_WRITE_UNWIND_CODE((UNWIND_CODE){
        .DataForPreviousCode = u32_to_u16(layout->stack_reserve / 8),
      });
      // TODO support 512k + allocations
    }
  }
  unwind_info->CountOfCodes = unwind_code_index;

  // :UnwindCodeAlignment
  if (unwind_info->CountOfCodes % 2) {
    // Write a dummy code to ensure alignment
    WIN32_WRITE_UNWIND_CODE((UNWIND_CODE){0});
  }

  assert(section->buffer.occupied % sizeof(u32) == 0);

  *function = (RUNTIME_FUNCTION) {
    .BeginAddress = layout->begin_rva,
    .EndAddress = layout->end_rva,
    .UnwindData = unwind_data_rva,
  };

  return unwind_info;
  #undef WIN32_WRITE_UNWIND_CODE
}

static inline u8
win32_prolog_size(
  const Section *section,
  const RUNTIME_FUNCTION *function
) {
  u32 offset_in_section = function->UnwindData - section->base_rva;
  const UNWIND_INFO *info = (const void *)((s8 *)section->buffer.memory + offset_in_section);
  return info->SizeOfProlog;
}

#endif