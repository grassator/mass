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

typedef struct {
  u64 functions;
  u64 imports;
} Win32_Jit_Counters;

typedef struct {
  RUNTIME_FUNCTION *function_table;
  Array_Function_Layout layouts;
  Fixed_Buffer *temp_buffer;
  Allocator temp_allocator;

  Win32_Jit_Counters previous_counts;
} Win32_Jit_Info;

void
win32_program_jit(
  Jit *jit
) {
  Program *program = jit->program;

  Win32_Jit_Info *info;
  if (jit->platform_specific_payload) {
    dyn_array_clear(jit->program->patch_info_array);
    assert(jit->platform_specific_payload);
    info = jit->platform_specific_payload;
    // TODO @Speed use RtlInstallFunctionTableCallback
    RtlDeleteFunctionTable(info->function_table);
    virtual_memory_buffer_deinit(&jit->buffer);
    info->temp_buffer->occupied = 0;
  } else {
    info = allocator_allocate(allocator_default, Win32_Jit_Info);
    Fixed_Buffer *temp_buffer = fixed_buffer_make(
      .allocator = allocator_system,
      .capacity = 1024 * 1024 // 1Mb
    );
    *info = (Win32_Jit_Info) {
      .layouts = dyn_array_make(Array_Function_Layout),
      .temp_buffer = temp_buffer,
      .temp_allocator = *fixed_buffer_allocator_make(temp_buffer),
      .previous_counts = {0},
    };
    jit->platform_specific_payload = info;
  }
  Program_Memory *memory = &jit->program->memory;

  u64 import_count = dyn_array_length(program->import_libraries);
  for (u64 i = info->previous_counts.imports; i < import_count; ++i) {
    Import_Library *lib = dyn_array_get(program->import_libraries, i);
    void **maybe_handle_pointer = hash_map_get(jit->import_library_handles, lib->name);
    void *handle;
    if (maybe_handle_pointer) {
      handle = *maybe_handle_pointer;
    } else {
      char *library_name = slice_to_c_string(&info->temp_allocator, lib->name);
      handle = LoadLibraryA(library_name);
      assert(handle);
      hash_map_set(jit->import_library_handles, lib->name, handle);
    }

    for (u64 symbol_index = 0; symbol_index < dyn_array_length(lib->symbols); ++symbol_index) {
      Import_Symbol *symbol = dyn_array_get(lib->symbols, symbol_index);
      Label *label = program_get_label(program, symbol->label32);
      if (!label->resolved) {
        char *symbol_name = slice_to_c_string(&info->temp_allocator, symbol->name);
        fn_type_opaque address = GetProcAddress(handle, symbol_name);
        assert(address);
        u64 offset = bucket_buffer_append_u64(memory->sections.data.buffer, (u64)address);
        label->offset_in_section = u64_to_u32(offset);
        label->resolved = true;
      }
    }
  }

  // The Layout of the final code is as follows:
  // |--RW-DATA--|--CODE--|--RO-DATA--|
  // This allows code to grow to 1 GB potentially while maintaining
  // access to RW and RO segments with RIP-relative addressing
  static const u64 MAX_RW_DATA_SIZE = 1024 * 1024 * 1024; // 1Gb
  static const u64 MAX_CODE_SIZE = 1024 * 1024 * 1024; // 1Gb
  static const u64 MAX_RO_DATA_SIZE = 1024 * 1024 * 1024; // 1Gb

  u64 program_size = MAX_CODE_SIZE + MAX_RW_DATA_SIZE + MAX_RO_DATA_SIZE;
  u64 function_count = dyn_array_length(program->functions);

  info->function_table = allocator_allocate_array(
    allocator_system, RUNTIME_FUNCTION, function_count
  );

  virtual_memory_buffer_init(&jit->buffer, program_size);

  { // Copying and repointing the data segment into contiguous buffer
    u64 global_data_size = u64_align(memory->sections.data.buffer->occupied, 16);
    if (global_data_size) {
      void *global_data =
        virtual_memory_buffer_allocate_bytes(&jit->buffer, global_data_size, sizeof(s8));
      bucket_buffer_copy_to_memory(memory->sections.data.buffer, global_data);
      // Setup permissions for the data segment
      DWORD win32_permissions =
        win32_section_permissions_to_virtual_protect_flags(memory->sections.data.permissions);
      VirtualProtect(global_data, global_data_size, win32_permissions, &(DWORD){0});
    }
  }

  // Since we are writing to the same buffer both data segment and code segment,
  // and there is no weird file vs virtual address stuff going on like in PE32,
  // we can just use natural offsets and ignore the base RVA
  memory->sections.data.base_rva = 0;
  memory->sections.code.base_rva = 0;

  u32 unwind_data_rva = u64_to_u32(jit->buffer.occupied);
  UNWIND_INFO *unwind_info_array = virtual_memory_buffer_allocate_bytes(
    &jit->buffer, sizeof(UNWIND_INFO) * function_count, sizeof(DWORD)
  );

  u64 code_start_rva = MAX_RW_DATA_SIZE;
  assert(jit->buffer.occupied < MAX_RW_DATA_SIZE);

  jit->buffer.occupied = code_start_rva;
  // FIXME this might break if we change virtual_memory_buffer implementation
  //       so ideally we should use custom code to handle this.
  jit->buffer.committed = code_start_rva;

  s8 *code_memory = jit->buffer.memory + jit->buffer.occupied;
  u64 trampoline_target = (u64)win32_program_test_exception_handler;
  u32 trampoline_virtual_address = make_trampoline(program, &jit->buffer, trampoline_target);

  assert(info->previous_counts.functions == dyn_array_length(info->layouts));
  for (u64 i = info->previous_counts.functions; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    Function_Layout *layout = dyn_array_push(info->layouts, (Function_Layout){0});
    fn_encode(program, &jit->buffer, builder, layout);
  }

  // It is a separate loop from above to make sure unwinding data is not in executable memory
  for (u64 i = 0; i < function_count; ++i) {
    Function_Builder *builder = dyn_array_get(program->functions, i);
    Function_Layout *layout = dyn_array_get(info->layouts, i);
    UNWIND_INFO *unwind_info = &unwind_info_array[i];
    win32_fn_init_unwind_info(
      builder, layout, unwind_info, &info->function_table[i], unwind_data_rva
    );
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
    u64 code_segment_size = jit->buffer.occupied - code_start_rva;
    code_segment_size = u64_align(code_segment_size, memory_page_size());
    DWORD win32_permissions =
      win32_section_permissions_to_virtual_protect_flags(memory->sections.code.permissions);
    VirtualProtect(code_memory, code_segment_size, win32_permissions, &(DWORD){0});
    if (!FlushInstructionCache(GetCurrentProcess(), code_memory, code_segment_size)) {
      panic("Unable to flush instruction cache");
    }
  }

  // TODO consider if we want to not do JIT at all in this case?
  if (function_count) {
    if (!RtlAddFunctionTable(
      info->function_table, u64_to_u32(function_count), (s64) jit->buffer.memory
    )) {
      panic("Could not add function table definition");
    }
  }
  info->previous_counts.functions = function_count;
  info->previous_counts.imports = import_count;;
}


#endif