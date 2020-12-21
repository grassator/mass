#ifndef WIN32_PLATFORM_H
#define WIN32_PLATFORM_H

#include "value.h"
// Partial list of WIN32 API basic type definitions to allow for cross-platform
// compilation while keeping MSDN reference code usable as is.
// https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types

// FIXME windows treats `long` as 32bit while linux treats it as 64bit
// probably best to not use these types at all
typedef int BOOL;
typedef long LONG;
typedef void *PVOID;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef PVOID HANDLE;
typedef int HFILE;
typedef unsigned char BYTE;
typedef unsigned char UBYTE;
typedef unsigned short USHORT;
typedef unsigned long ULONG;
typedef unsigned long long ULONGLONG;
typedef LONG HRESULT;

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

typedef struct _IMAGE_DATA_DIRECTORY {
    DWORD   VirtualAddress;
    DWORD   Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16

typedef struct _IMAGE_FILE_HEADER {
  WORD Machine;
  WORD NumberOfSections;
  DWORD TimeDateStamp;
  DWORD PointerToSymbolTable;
  DWORD NumberOfSymbols;
  WORD SizeOfOptionalHeader;
  WORD Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;

typedef struct _IMAGE_OPTIONAL_HEADER64 {
  WORD  Magic;
  BYTE  MajorLinkerVersion;
  BYTE  MinorLinkerVersion;
  DWORD SizeOfCode;
  DWORD SizeOfInitializedData;
  DWORD SizeOfUninitializedData;
  DWORD AddressOfEntryPoint;
  DWORD BaseOfCode;
  ULONGLONG ImageBase;
  DWORD SectionAlignment;
  DWORD FileAlignment;
  WORD MajorOperatingSystemVersion;
  WORD MinorOperatingSystemVersion;
  WORD MajorImageVersion;
  WORD MinorImageVersion;
  WORD MajorSubsystemVersion;
  WORD MinorSubsystemVersion;
  DWORD Win32VersionValue;
  DWORD SizeOfImage;
  DWORD SizeOfHeaders;
  DWORD CheckSum;
  WORD Subsystem;
  WORD DllCharacteristics;
  ULONGLONG SizeOfStackReserve;
  ULONGLONG SizeOfStackCommit;
  ULONGLONG SizeOfHeapReserve;
  ULONGLONG SizeOfHeapCommit;
  DWORD LoaderFlags;
  DWORD NumberOfRvaAndSizes;
  IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;

typedef struct _IMAGE_IMPORT_DESCRIPTOR {
  union {
    DWORD Characteristics;
    DWORD OriginalFirstThunk;
  };
  DWORD TimeDateStamp;
  DWORD ForwarderChain;
  DWORD Name;
  DWORD FirstThunk;
} IMAGE_IMPORT_DESCRIPTOR;
typedef IMAGE_IMPORT_DESCRIPTOR *PIMAGE_IMPORT_DESCRIPTOR;

typedef struct _IMAGE_DOS_HEADER {
  WORD e_magic;
  WORD e_cblp;
  WORD e_cp;
  WORD e_crlc;
  WORD e_cparhdr;
  WORD e_minalloc;
  WORD e_maxalloc;
  WORD e_ss;
  WORD e_sp;
  WORD e_csum;
  WORD e_ip;
  WORD e_cs;
  WORD e_lfarlc;
  WORD e_ovno;
  WORD e_res[4];
  WORD e_oemid;
  WORD e_oeminfo;
  WORD e_res2[10];
  LONG e_lfanew;
} IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;

#define IMAGE_SIZEOF_SHORT_NAME 8
// https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-image_section_header
typedef struct _IMAGE_SECTION_HEADER {
  BYTE Name[IMAGE_SIZEOF_SHORT_NAME];
  union {
    DWORD PhysicalAddress;
    DWORD VirtualSize;
  } Misc;
  DWORD VirtualAddress;
  DWORD SizeOfRawData;
  DWORD PointerToRawData;
  DWORD PointerToRelocations;
  DWORD PointerToLinenumbers;
  WORD NumberOfRelocations;
  WORD NumberOfLinenumbers;
  DWORD Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;

// https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64?view=msvc-160#struct-runtime_function
typedef struct RUNTIME_FUNCTION {
  ULONG BeginAddress;
  ULONG EndAddress;
  ULONG UnwindData;
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
    UBYTE CodeOffset;
    UBYTE UnwindOp : 4;
    UBYTE OpInfo   : 4;
  };
  USHORT FrameOffset;
  USHORT DataForPreviousCode;
} UNWIND_CODE;

#define UNWIND_INFO_EXCEPTION_DATA_MAX_SIZE sizeof(void *) * 2

#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK 2
#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH 16
#define UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES (sizeof(u32) / sizeof(UNWIND_CODE))
#define UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES  (UNWIND_INFO_EXCEPTION_DATA_MAX_SIZE / sizeof(UNWIND_CODE))

typedef struct {
  UBYTE Version       : 3;
  UBYTE Flags         : 5;
  UBYTE SizeOfProlog;
  UBYTE CountOfCodes;
  UBYTE FrameRegister : 4;
  UBYTE FrameOffset   : 4;
  // FIXME actually turn this into a variadic struct with getter functions
  // :RegisterAllocation need to add more reserved space for UnwindCode
  UNWIND_CODE UnwindCode[
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK +
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH +
    UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES +
    UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES
  ];
} UNWIND_INFO;

void
win32_fn_init_unwind_info(
  const Function_Builder *builder,
  UNWIND_INFO *unwind_info,
  RUNTIME_FUNCTION *function_exception_info,
  u32 unwind_data_rva
) {
  const Function_Layout *layout = &builder->layout;
  assert(unwind_info);
  assert(function_exception_info);
  *unwind_info = (UNWIND_INFO) {
    .Version = 1,
    .Flags = 0,
    .SizeOfProlog = layout->size_of_prolog,
    .CountOfCodes = 0,
    .FrameRegister = 0,
    .FrameOffset = 0,
  };

  // :Win32UnwindCodes Must match what happens in the function encoding
  u8 unwind_code_index = 0;
  for (Register reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->used_register_bitset, reg_index)) {
      if (!register_bitset_get(builder->code_block.register_volatile_bitset, reg_index)) {
        unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE) {
          .CodeOffset = builder->layout.volatile_register_push_offsets[unwind_code_index++],
          .UnwindOp = UWOP_PUSH_NONVOL,
          .OpInfo = s32_to_u8(reg_index),
        };
      }
    }
  }

  if (layout->stack_reserve) {
    assert(layout->stack_reserve >= 8);
    assert(layout->stack_reserve % 8 == 0);
    if (layout->stack_reserve <= 128) {
      unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
        .CodeOffset = layout->stack_allocation_offset_in_prolog,
        .UnwindOp = UWOP_ALLOC_SMALL,
        .OpInfo = (layout->stack_reserve - 8) / 8,
      };
    } else {
      unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
        .CodeOffset = layout->stack_allocation_offset_in_prolog,
        .UnwindOp = UWOP_ALLOC_LARGE,
        .OpInfo = 0,
      };
      unwind_info->UnwindCode[unwind_code_index++] = (UNWIND_CODE){
        .DataForPreviousCode = u32_to_u16(layout->stack_reserve / 8),
      };
      // TODO support 512k + allocations
    }
    unwind_info->CountOfCodes = unwind_code_index;
  }
  // TODO do this on the outside
  *function_exception_info = (RUNTIME_FUNCTION) {
    .BeginAddress = layout->begin_rva,
    .EndAddress = layout->end_rva,
    .UnwindData = unwind_data_rva,
  };
}

#endif