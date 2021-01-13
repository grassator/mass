#ifndef WIN32_RUNTIME_H
#define WIN32_RUNTIME_H

#include "win32_platform.h"

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

typedef struct {
  const Function_Builder *builder;
  Jit *jit;
} Win32_Exception_Data;

static_assert(
  sizeof(Win32_Exception_Data) <= UNWIND_INFO_EXCEPTION_DATA_MAX_SIZE,
  Win32_Exception_Data__must_fit_into_UNWIND_INFO
);

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

  if (!exception_data->jit->is_stack_unwinding_in_progress) {
    exception_data->jit->is_stack_unwinding_in_progress = true;
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
      source_range_print_start_position(&instruction->source_range);
    }
  }

  return ExceptionContinueSearch;
}

void
win32_program_jit_resolve_dll_imports(
  Jit *jit
) {
  Program *program = jit->program;
  if (!dyn_array_is_initialized(program->import_libraries)) return;

  Bucket_Buffer *temp_buffer = bucket_buffer_make();
  Allocator *temp_allocator = bucket_buffer_allocator_make(temp_buffer);

  for (u64 i = 0; i < dyn_array_length(program->import_libraries); ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    void **maybe_handle_pointer = hash_map_get(jit->import_library_handles, lib->name);
    void *handle;
    if (maybe_handle_pointer) {
      handle = *maybe_handle_pointer;
    } else {
      char *library_name = slice_to_c_string(temp_allocator, lib->name);
      handle = LoadLibraryA(library_name);
      assert(handle);
      hash_map_set(jit->import_library_handles, lib->name, handle);
    }

    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, symbol_index);
      Label *label = program_get_label(program, symbol->label32);
      if (!label->resolved) {
        char *symbol_name = slice_to_c_string(temp_allocator, symbol->name);
        fn_type_opaque address = GetProcAddress(handle, symbol_name);
        assert(address);
        u64 offset = bucket_buffer_append_u64(program->data_section.buffer, (u64)address);
        label->offset_in_section = u64_to_u32(offset);
        label->resolved = true;
      }
    }
  }
  bucket_buffer_destroy(temp_buffer);
}

void
win32_program_jit(
  Jit *jit
) {
  win32_program_jit_resolve_dll_imports(jit);
  Program *program = jit->program;
  u64 code_segment_size = estimate_max_code_size_in_bytes(program) + MAX_ESTIMATED_TRAMPOLINE_SIZE;
  u64 function_count = dyn_array_length(program->functions);
  u64 global_data_size = u64_align(program->data_section.buffer->occupied, 16);
  u64 unwind_info_size = u64_align(sizeof(UNWIND_INFO) * function_count, sizeof(DWORD));
  u64 data_segment_size = global_data_size + unwind_info_size;
  u64 program_size = data_segment_size + code_segment_size;

  if (jit->buffer) {
    panic("TODO support reintrant or better yet incremental jitting");
  }

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
        .jit = jit,
      };
    }
  }

  // After all the functions are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Setup permissions for the code segment
  {
    DWORD win32_permissions =
      win32_section_permissions_to_virtual_protect_flags(program->code_section.permissions);
    VirtualProtect(code_memory, code_segment_size, win32_permissions, &(DWORD){0});
    if (!FlushInstructionCache(GetCurrentProcess(), code_memory, code_segment_size)) {
      panic("Unable to flush instruction cache");
    }
  }

  if (!RtlAddFunctionTable(
    fn_exception_info, u64_to_u32(function_count), (s64) result_buffer->memory
  )) {
    panic("Could not add function table definition");
  }
  jit->buffer = result_buffer;
}


#endif