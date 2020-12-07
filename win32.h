#ifndef WIN32_H
#define WIN32_H

#include "value.h"

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

typedef struct Function_Builder Function_Builder;
typedef struct Program Program;

typedef struct {
  const Function_Builder *builder;
  Program *program;
} Win32_Exception_Data;

typedef dyn_array_type(UNWIND_CODE) Array_UNWIND_CODE;

#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK 2
#define UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH 16
#define UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES (sizeof(u32) / sizeof(UNWIND_CODE))
#define UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES  (sizeof(Win32_Exception_Data) / sizeof(UNWIND_CODE))

typedef struct {
  u8 Version       : 3;
  u8 Flags         : 5;
  u8 SizeOfProlog;
  u8 CountOfCodes;
  u8 FrameRegister : 4;
  u8 FrameOffset   : 4;
  // FIXME actually turn this into a variadic struct with getter functions
  // :RegisterAllocation need to add more reserved space for UnwindCode
  UNWIND_CODE UnwindCode[
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_STACK +
    UNWIND_INFO_MAX_COUNT_OF_CODES_FOR_NON_VOLATILE_REGISTER_PUSH +
    UNWIND_INFO_EXCEPTION_HANDLER_SIZE_IN_UNWIND_CODES +
    UNWIND_INFO_EXCEPTION_DATA_SIZE_IN_INWIND_CODES
  ];
} UNWIND_INFO;

static DWORD
win32_section_permissions_to_virtual_protect_flags(
  Section_Permissions permissions
) {
  if (permissions & Section_Permissions_Execute) {
    if (permissions & Section_Permissions_Write) {
      return PAGE_EXECUTE_READWRITE;
    } else if (permissions & Section_Permissions_Read) {
      return PAGE_EXECUTE_READ;
    }
    return PAGE_EXECUTE;
  }
  if (permissions & Section_Permissions_Write) {
    return PAGE_READWRITE;
  } else if (permissions & Section_Permissions_Read) {
    return PAGE_READONLY;
  }
  return PAGE_NOACCESS;
}

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

EXCEPTION_DISPOSITION
win32_program_test_exception_handler(
  EXCEPTION_RECORD *ExceptionRecord,
  u64 EstablisherFrame,
  CONTEXT *ContextRecord,
  DISPATCHER_CONTEXT *DispatcherContext
) {
  RUNTIME_FUNCTION *function = DispatcherContext->FunctionEntry;
  u64 absolute_function_begin_address = DispatcherContext->ImageBase + function->BeginAddress;
  u64 relative_instruction_byte_offset =
    DispatcherContext->ControlPc - absolute_function_begin_address;

  Win32_Exception_Data *exception_data = DispatcherContext->HandlerData;

  if (!exception_data->program->is_stack_unwinding_in_progress) {
    exception_data->program->is_stack_unwinding_in_progress = true;
    printf("Unhandled Exception: ");
    switch(ExceptionRecord->ExceptionCode) {
      case EXCEPTION_ACCESS_VIOLATION: {
        printf("Access Violation");
        break;
      }
      case EXCEPTION_ARRAY_BOUNDS_EXCEEDED: {
        printf("Hardware Array Bounds Check Failed");
        break;
      }
      case EXCEPTION_BREAKPOINT: {
        printf("User Breakpoint");
        break;
      }
      case EXCEPTION_DATATYPE_MISALIGNMENT: {
        printf("Misaligned Read / Write");
        break;
      }
      case EXCEPTION_FLT_DENORMAL_OPERAND: {
        printf("Denormal Float Value Result");
        break;
      }
      case EXCEPTION_FLT_DIVIDE_BY_ZERO: {
        printf("Float Divide By Zero");
        break;
      }
      case EXCEPTION_FLT_INEXACT_RESULT: {
        printf("Float Inexact Decimal Fraction");
        break;
      }
      case EXCEPTION_FLT_INVALID_OPERATION: {
        printf("Float Invalid Operation");
        break;
      }
      case EXCEPTION_FLT_OVERFLOW: {
        printf("Float Overflow");
        break;
      }
      case EXCEPTION_FLT_STACK_CHECK: {
        printf("Stack Overflow After Float Operation");
        break;
      }
      case EXCEPTION_FLT_UNDERFLOW: {
        printf("Float Underflow");
        break;
      }
      case EXCEPTION_ILLEGAL_INSTRUCTION: {
        printf("Illegal Machine Code Instruction");
        break;
      }
      case EXCEPTION_IN_PAGE_ERROR: {
        printf("Read Missing Memory Page");
        break;
      }
      case EXCEPTION_INT_DIVIDE_BY_ZERO: {
        printf("Integer Divide By Zero");
        break;
      }
      case EXCEPTION_INT_OVERFLOW: {
        printf("Integer Overflow");
        break;
      }
      case EXCEPTION_INVALID_DISPOSITION: {
        printf("Invalid Disposition From An Exception Handler");
        break;
      }
      case EXCEPTION_NONCONTINUABLE_EXCEPTION: {
        printf("Continue Execution After Noncontinuable Exception");
        break;
      }
      case EXCEPTION_PRIV_INSTRUCTION: {
        printf("Instruction Not Allowed In Current CPU Mode");
        break;
      }
      case EXCEPTION_SINGLE_STEP: {
        printf("Single Step Instruction");
        break;
      }
      case EXCEPTION_STACK_OVERFLOW: {
        printf("Stack Overflow");
        break;
      }
      default: {
        printf("Unknown");
        break;
      }
    }
    printf(".\n");
  }

  u64 current_offset = 0;
  for (u64 i = 0; i < dyn_array_length(exception_data->builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(exception_data->builder->code_block.instructions, i);
    // DispatcherContext->ControlPc provides IP *after* the instruction that caused the exception
    // so we add instruction byte size before comparing
    current_offset += instruction->encoded_byte_size;
    if (current_offset == relative_instruction_byte_offset) {
      printf("  at ");
      source_range_print_start_position(instruction->source_range);
    }
  }

  return ExceptionContinueSearch;
}

void
win32_program_jit(
  Compilation_Context *context
) {
  Program *program = context->program;
  if (dyn_array_is_initialized(program->import_libraries)) {
    for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
      Import_Library *lib = dyn_array_get(program->import_libraries, i);
      const char *library_name = slice_to_c_string(context->allocator, lib->name);
      HINSTANCE dll_handle = LoadLibraryA(library_name);
      assert(dll_handle);

      for (u64 i = 0; i < dyn_array_length(lib->symbols); ++i) {
        Import_Symbol *symbol = dyn_array_get(lib->symbols, i);

        const char *symbol_name = slice_to_c_string(context->allocator, symbol->name);
        fn_type_opaque fn_address = GetProcAddress(dll_handle, symbol_name);
        assert(fn_address);
        u64 offset = bucket_buffer_append_u64(program->data_section.buffer, (u64)fn_address);
        program_set_label_offset(program, symbol->label32, u64_to_u32(offset));
      }
    }
  }
  u64 code_segment_size = estimate_max_code_size_in_bytes(program) + MAX_ESTIMATED_TRAMPOLINE_SIZE;
  u64 function_count = dyn_array_length(program->functions);
  u64 global_data_size = u64_align(program->data_section.buffer->occupied, 16);
  u64 unwind_info_size = u64_align(sizeof(UNWIND_INFO) * function_count, sizeof(DWORD));
  u64 data_segment_size = global_data_size + unwind_info_size;
  u64 program_size = data_segment_size + code_segment_size;

  // Making a contiguous buffer holding both data and memory to ensure
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = program_size,
  );

  { // Copying and repointing the data segment into contiguous buffer
    void *global_data = fixed_buffer_allocate_bytes(result_buffer, global_data_size, sizeof(s8));
    bucket_buffer_copy_to_memory(program->data_section.buffer, global_data);
    // Setup permissions for the data segment
    DWORD win32_permissions =
      win32_section_permissions_to_virtual_protect_flags(program->data_section.permissions);
    VirtualProtect(global_data, global_data_size, win32_permissions, &(DWORD){0});
  }

  // Since we are writing to the same buffer both data segment and code segment,
  // and there is no weird file vs virtual address stuff going on like in PE32,
  // we can just use natural offsets and ignore the base RVA
  program->data_section.base_rva = 0;
  program->code_section.base_rva = 0;

  UNWIND_INFO *unwind_info_array = fixed_buffer_allocate_bytes(
    result_buffer, sizeof(UNWIND_INFO) * function_count, sizeof(DWORD)
  );

  RUNTIME_FUNCTION *fn_exception_info = allocator_allocate_array(
    allocator_system, RUNTIME_FUNCTION, function_count
  );

  s8 *code_memory = result_buffer->memory + result_buffer->occupied;
  u64 trampoline_target = (u64)win32_program_test_exception_handler;
  u32 trampoline_virtual_address = make_trampoline(program, result_buffer, trampoline_target);

  for (u64 i = 0; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    fn_encode(program, result_buffer, builder);
  }

  for (u64 i = 0; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    UNWIND_INFO *unwind_info = &unwind_info_array[i];
    u32 unwind_data_rva = s64_to_u32((s8 *)unwind_info - result_buffer->memory);
    win32_fn_init_unwind_info(builder, unwind_info, &fn_exception_info[i], unwind_data_rva);
    {
      unwind_info->Flags |= UNW_FLAG_EHANDLER;
      u64 exception_handler_index = u64_align(unwind_info->CountOfCodes, 2);
      u32 *exception_handler_address = (u32 *)&unwind_info->UnwindCode[exception_handler_index];
      *exception_handler_address = trampoline_virtual_address;
      Win32_Exception_Data *exception_data = (void *)(exception_handler_address + 1);
      *exception_data = (Win32_Exception_Data) {
        .builder = builder,
        .program = program,
      };
    }
  }

  // After all the functions are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Setup permissions for the code segment
  DWORD win32_permissions =
    win32_section_permissions_to_virtual_protect_flags(program->code_section.permissions);
  VirtualProtect(code_memory, code_segment_size, win32_permissions, &(DWORD){0});

  if (!RtlAddFunctionTable(
    fn_exception_info, u64_to_u32(function_count), (s64) result_buffer->memory
  )) {
    panic("Could not add function table definition");
  }
  program->jit_buffer = result_buffer;
}


#endif