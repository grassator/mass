#ifndef WIN32_PLATFORM_H
#define WIN32_PLATFORM_H

#include "value.h"
// Partial list of WIN32 API basic type definitions to allow for cross-platform
// compilation while keeping MSDN reference code usable as is.
// https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types

typedef int BOOL;
typedef VOID *PVOID;
typedef unsigned long DWORD;
typedef PVOID HANDLE;
typedef int HFILE;
typedef unsigned char UBYTE;
typedef unsigned short USHORT;
typedef unsigned long ULONG;
typedef LONG HRESULT;

#ifndef _WIN32
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