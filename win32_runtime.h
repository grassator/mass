#ifndef WIN32_RUNTIME_H
#define WIN32_RUNTIME_H

#include "win32_platform.h"
#include "program.h"
#include "source.h"

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
  // :ExceptionDataAlignment
  const Function_Builder *builder;
  Jit *jit;
  Compilation *compilation;
} Win32_Exception_Data;

static void
win32_print_register_state(
  CONTEXT *ContextRecord
) {
  printf("RIP = 0x%016llX\n", ContextRecord->Rip);
  printf("EF = 0x%08X\n", (u32)ContextRecord->EFlags);
  printf("\n");

  printf("RAX = 0x%016llX\n", ContextRecord->Rax);
  printf("RCX = 0x%016llX\n", ContextRecord->Rcx);
  printf("RDX = 0x%016llX\n", ContextRecord->Rdx);
  printf("RBX = 0x%016llX\n", ContextRecord->Rbx);
  printf("RSP = 0x%016llX\n", ContextRecord->Rsp);
  printf("RBP = 0x%016llX\n", ContextRecord->Rbp);
  printf("RSI = 0x%016llX\n", ContextRecord->Rsi);
  printf("RDI = 0x%016llX\n", ContextRecord->Rdi);

  printf("R8  = 0x%016llX\n",  ContextRecord->R8);
  printf("R9  = 0x%016llX\n",  ContextRecord->R9);
  printf("R10 = 0x%016llX\n", ContextRecord->R10);
  printf("R11 = 0x%016llX\n", ContextRecord->R11);
  printf("R12 = 0x%016llX\n", ContextRecord->R12);
  printf("R13 = 0x%016llX\n", ContextRecord->R13);
  printf("R14 = 0x%016llX\n", ContextRecord->R14);
  printf("R15 = 0x%016llX\n", ContextRecord->R15);
  printf("\n");

  struct { const char *name; bool value; } flags[] = {
    { .name = "CF", .value = !!(ContextRecord->EFlags & 0x0001) },
    { .name = "PF", .value = !!(ContextRecord->EFlags & 0x0004) },
    { .name = "AF", .value = !!(ContextRecord->EFlags & 0x0008) },
    { .name = "ZF", .value = !!(ContextRecord->EFlags & 0x0040) },
    { .name = "SF", .value = !!(ContextRecord->EFlags & 0x0080) },
    { .name = "TF", .value = !!(ContextRecord->EFlags & 0x0100) },
    { .name = "IF", .value = !!(ContextRecord->EFlags & 0x0200) },
    { .name = "DF", .value = !!(ContextRecord->EFlags & 0x0400) },
    { .name = "OF", .value = !!(ContextRecord->EFlags & 0x0800) },
  };

  u64 length = 0;
  // Table header
  for (u64 i = 0; i < countof(flags); ++i) {
    if (i != 0) {
      length += strlen(" | ");
      printf(" | ");
    }
    length += strlen(flags[i].name);
    printf("%s", flags[i].name);
  }
  printf("\n");
  for (u64 i = 0; i < length; ++i) {
    // TODO @Speed
    printf("%c", '-');
  }
  printf("\n");
  // Table values
  for (u64 i = 0; i < countof(flags); ++i) {
    if (i != 0) {
      length += strlen("  | ");
      printf("  | ");
    }
    printf("%c", flags[i].value ? 'x' : ' ');
  }
  printf("\n");
}

typedef dyn_array_type(RUNTIME_FUNCTION) Array_RUNTIME_FUNCTION;

typedef struct {
  Array_RUNTIME_FUNCTION function_table;
  u32 trampoline_rva;
} Win32_Jit_Info;

static s64
win32_get_function_index_from_address(
  DWORD64 instruction_address,
  Jit *jit
) {
  Win32_Jit_Info *info = jit->platform_specific_payload;
  Section *code_section = &jit->program->memory.code;
  Virtual_Memory_Buffer *code_buffer = &code_section->buffer;
  if(instruction_address < (DWORD64)code_buffer->memory) return -1;
  if(instruction_address >= (DWORD64)code_buffer->memory + code_buffer->occupied) return -1;

  u32 rva = u64_to_u32(code_section->base_rva + instruction_address - (DWORD64)code_buffer->memory);

  // Do a binary search
  s64 left_bound = 0;
  s64 right_bound = u64_to_s64(dyn_array_length(info->function_table)) - 1;
  while (left_bound <= right_bound) {
    s64 middle = left_bound + (right_bound - left_bound) / 2;
    RUNTIME_FUNCTION *function = dyn_array_get(info->function_table, middle);

    if (rva < function->BeginAddress) {
      right_bound = middle - 1;
    } else if (rva > function->EndAddress) {
      left_bound = middle + 1;
    } else {
      return middle;
    }
  }
  panic("Could not find matching RUNTIME_FUNCTION");

  return -1;
}

static RUNTIME_FUNCTION *
win32_get_runtime_function_callback(
  DWORD64 instruction_address,
  Jit *jit
) {
  s64 runtime_function_index = win32_get_function_index_from_address(instruction_address, jit);
  if (runtime_function_index < 0) return 0;
  Win32_Jit_Info *info = jit->platform_specific_payload;
  return dyn_array_get(info->function_table, runtime_function_index);
}


static const Instruction *
win32_instruction_for_address(
  DWORD64 instruction_address,
  Jit *jit,
  Source_Range *maybe_out_source_range
) {
  s64 runtime_function_index = win32_get_function_index_from_address(instruction_address, jit);
  if (runtime_function_index < 0) return 0;
  Program_Memory *memory = &jit->program->memory;
  Virtual_Memory_Buffer *code_buffer = &memory->code.buffer;

  Win32_Jit_Info *info = jit->platform_specific_payload;
  Function_Builder *builder = dyn_array_get(jit->program->functions, runtime_function_index);
  RUNTIME_FUNCTION *function = dyn_array_get(info->function_table, runtime_function_index);
  const UNWIND_INFO *unwind_info =
    win32_unwind_info_for_function(&memory->ro_data, function);

  u64 absolute_function_begin_address = (u64)code_buffer->memory + function->BeginAddress;
  u64 relative_instruction_byte_offset = instruction_address - absolute_function_begin_address;

  u64 current_offset = unwind_info->SizeOfProlog;
  for (Instruction_Bucket *bucket = builder->code_block.first_bucket; bucket; bucket = bucket->next) {
    for (u64 i = 0; i < bucket->length; ++i) {
      Instruction *instruction = &bucket->items[i];
      if (maybe_out_source_range && instruction->tag == Instruction_Tag_Location) {
        *maybe_out_source_range = instruction->Location.source_range;
      }
      if (instruction->tag != Instruction_Tag_Bytes) continue;
      current_offset += instruction->Bytes.length;
      if (current_offset == relative_instruction_byte_offset) {
        return instruction;
      }
    }
  }
  return 0;
}

static void
win32_print_stack(
  DWORD64 stack_pointer,
  DWORD64 instruction_address,
  Compilation *compilation,
  Jit *jit
) {
  s64 runtime_function_index = win32_get_function_index_from_address(instruction_address, jit);
  if (runtime_function_index < 0) {
    // TODO adjust instruction address to point to the start of the function
    printf("  stopped at external code at 0x%016llX\n", instruction_address);
    return;
  }

  Source_Range source_range;
  const Instruction *instruction =
    win32_instruction_for_address(instruction_address, jit, &source_range);
  if (instruction) {
    printf("  at ");
    source_range_print_start_position(compilation, &source_range);
  }

  Win32_Jit_Info *info = jit->platform_specific_payload;
  Function_Builder *builder = dyn_array_get(jit->program->functions, runtime_function_index);
  RUNTIME_FUNCTION *function = dyn_array_get(info->function_table, runtime_function_index);
  const UNWIND_INFO *unwind_info =
    win32_unwind_info_for_function(&jit->program->memory.ro_data, function);

  // Function prolog pushes the registers which changes RSP and we need to account for that
  u32 register_push_count = win32_unwind_info_pushed_register_count(unwind_info);

  stack_pointer += builder->stack_reserve + register_push_count * 8;

  u64 low_limit;
  u64 high_limit;
  GetCurrentThreadStackLimits(&low_limit, &high_limit);

  if (stack_pointer < low_limit || stack_pointer >= high_limit) {
    printf("Outside of the allocated stack\n");
    return;
  }

  u64 return_address = *(u64 *)stack_pointer;
  // TODO Optimized forwarding functions can use JMP here instead of CALL/RET pair at the end
  //      which means there should not be any adjustment.
  stack_pointer += 0x8; // simulate return address pop with `ret` instruction

  win32_print_stack(stack_pointer, return_address, compilation, jit);
}

static void
mass_print_instruction(
  const Instruction *instruction
) {
  switch (instruction->tag) {
    case Instruction_Tag_Bytes: {
      printf("Bytes(");
      for (u32 i = 0; i < instruction->Bytes.length; ++i) {
        const char *maybe_space = i == 0 ? "" : " ";
        printf("%s%02x", maybe_space, instruction->Bytes.memory[i]);
      }
      printf(")");
    } break;
    case Instruction_Tag_Label: {
      printf("Label(%p)\n", instruction->Label.pointer);
    } break;
    case Instruction_Tag_Label_Patch: {
      printf("Label_Patch(TODO)");
    } break;
    case Instruction_Tag_Stack_Patch: {
      printf("Stack_Patch(TODO)");
    } break;
    case Instruction_Tag_Location: {
      printf("Location(TODO)");
    } break;
  }
}

static const Scope*
win32_debugger_maybe_scope_for_address(
  u64 rip,
  Jit *jit
) {
  const Instruction *instruction = win32_instruction_for_address(rip, jit, 0);
  if (instruction) {
    if (instruction->scope) {
      return instruction->scope;
    } else {
      printf("Found an instruction but it has no scope information. Instruction:\n  ");
      mass_print_instruction(instruction);
      printf("\n");
    }
  } else {
    printf("Could not find the instruction for the given IP\n");
  }
  return 0;
}

static const void *
win32_debugger_register_memory(
  CONTEXT *ContextRecord,
  Register reg
) {
  switch (reg) {
    case Register_A: return &ContextRecord->Rax;
    case Register_C: return &ContextRecord->Rcx;
    case Register_D: return &ContextRecord->Rdx;
    case Register_B: return &ContextRecord->Rbx;
    case Register_SP: return &ContextRecord->Rsp;
    case Register_BP: return &ContextRecord->Rbp;
    case Register_SI: return &ContextRecord->Rsi;
    case Register_DI: return &ContextRecord->Rdi;
    case Register_R8: return &ContextRecord->R8;
    case Register_R9: return &ContextRecord->R9;
    case Register_R10: return &ContextRecord->R10;
    case Register_R11: return &ContextRecord->R11;
    case Register_R12: return &ContextRecord->R12;
    case Register_R13: return &ContextRecord->R13;
    case Register_R14: return &ContextRecord->R14;
    case Register_R15: return &ContextRecord->R15;
    default: assert(!"Unknown general purpose register index"); break;
  }
  return 0;
}

// TODO catch cycles
static void
mass_print_value_with_descriptor_and_memory(
  const Descriptor *descriptor,
  const u8 *memory,
  u64 depth
) {
  if (depth > 5) {
    printf("<...>");
    return;
  }
  u64 byte_size = descriptor_byte_size(descriptor);
  switch (descriptor->tag) {
    case Descriptor_Tag_Void: {
      assert(byte_size == 0);
      printf("<void>");
    } break;
    case Descriptor_Tag_Never: {
      assert(byte_size == 0);
      printf("<never>");
    } break;
    case Descriptor_Tag_Raw: {
      printf("<raw>[");
      for (u64 i = 0; i < byte_size; ++i) {
        const char *space = i == 0 ? "" : " ";
        printf("%s%02x", space, ((u8 *)memory)[i]);
      }
      printf("]");
    } break;
    case Descriptor_Tag_Float: {
      if (byte_size == 8) {
        printf("%f", *(f64 *)memory);
      } else if (byte_size == 4) {
        printf("%f", *(f32 *)memory);
      } else {
        assert(!"Unsupported float byte size");
      }
    } break;
    case Descriptor_Tag_Integer: {
      if (descriptor->Integer.is_signed) {
        switch (byte_size) {
          case 1: printf("%"PRIs8"", *(s8 *)memory); break;
          case 2: printf("%"PRIs16"", *(s16 *)memory); break;
          case 4: printf("%"PRIs32"", *(s32 *)memory); break;
          case 8: printf("%"PRIs64"", *(s64 *)memory); break;
          default: assert(!"Unsupported integer byte size"); break;
        }
      } else {
        switch (byte_size) {
          case 1: printf("%"PRIu8"", *(u8 *)memory); break;
          case 2: printf("%"PRIu16"", *(u16 *)memory); break;
          case 4: printf("%"PRIu32"", *(u32 *)memory); break;
          case 8: printf("%"PRIu64"", *(u64 *)memory); break;
          default: assert(!"Unsupported integer byte size"); break;
        }
      }
    } break;
    case Descriptor_Tag_Function_Instance: {
      printf("TODO support function instances");
    } break;
    case Descriptor_Tag_Fixed_Array: {
      printf("[");
      const Descriptor *item_descriptor = descriptor->Fixed_Array.item;
      u64 item_byte_size = descriptor_byte_size(item_descriptor);
      for (u64 i = 0; i < descriptor->Fixed_Array.length; ++i) {
        if (i != 0) printf(", ");
        const u8 *field_memory = memory + item_byte_size * i;
        mass_print_value_with_descriptor_and_memory(item_descriptor, field_memory, depth + 1);
      }
      printf("]");
    } break;
    case Descriptor_Tag_Struct: {
      printf("[");
      for (u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
        if (i != 0) printf(", ");
        const Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
        printf(".%"PRIslice" = ", SLICE_EXPAND_PRINTF(field->name));
        const u8 *field_memory = memory + field->offset;
        mass_print_value_with_descriptor_and_memory(field->descriptor, field_memory, depth + 1);
      }
      printf("]");
    } break;
    case Descriptor_Tag_Pointer_To: {
      printf("TODO support pointer types");
    } break;
  }
}

static void
mass_debug_print_value(
  CONTEXT *ContextRecord,
  Function_Builder *builder,
  Value *value
) {
  assert(value->tag == Value_Tag_Forced);
  const Storage *storage = &value->Forced.storage;
  const void *memory = 0;
  switch (storage->tag) {
    case Storage_Tag_Immediate: {
      memory = &storage->Immediate.bits;
    } break;
    case Storage_Tag_Eflags: {
      // TODO storage->Eflags.compare_type
      memory = &ContextRecord->EFlags;
    } break;
    case Storage_Tag_Register: {
      memory = win32_debugger_register_memory(ContextRecord, storage->Register.index);
      assert(storage->Register.offset_in_bits % 8 == 0);
      memory = ((u8 *)memory) + (storage->Register.offset_in_bits / 8);
    } break;
    case Storage_Tag_Xmm: {
      switch (storage->Xmm.index) {
        case Register_Xmm0: memory = &ContextRecord->Xmm0; break;
        case Register_Xmm1: memory = &ContextRecord->Xmm1; break;
        case Register_Xmm2: memory = &ContextRecord->Xmm2; break;
        case Register_Xmm3: memory = &ContextRecord->Xmm3; break;
        case Register_Xmm4: memory = &ContextRecord->Xmm4; break;
        case Register_Xmm5: memory = &ContextRecord->Xmm5; break;
        case Register_Xmm6: memory = &ContextRecord->Xmm6; break;
        case Register_Xmm7: memory = &ContextRecord->Xmm7; break;
        case Register_Xmm8: memory = &ContextRecord->Xmm8; break;
        case Register_Xmm9: memory = &ContextRecord->Xmm9; break;
        case Register_Xmm10: memory = &ContextRecord->Xmm10; break;
        case Register_Xmm11: memory = &ContextRecord->Xmm11; break;
        case Register_Xmm12: memory = &ContextRecord->Xmm12; break;
        case Register_Xmm13: memory = &ContextRecord->Xmm13; break;
        case Register_Xmm14: memory = &ContextRecord->Xmm14; break;
        case Register_Xmm15: memory = &ContextRecord->Xmm15; break;
        default: assert(!"Unknown XMM register index"); break;
      }
    } break;
    case Storage_Tag_Static: {
      printf("TODO support static storage");
      return;
    }
    case Storage_Tag_Memory: {
      switch (storage->Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          const Memory_Location_Instruction_Pointer_Relative *rip_relative =
            &storage->Memory.location.Instruction_Pointer_Relative;
          Label *label = rip_relative->label;
          assert(label->resolved);
          assert(label->section);
          printf("TODO support RIP-relative storage");
        } break;
        case Memory_Location_Tag_Indirect: {
          const Memory_Location_Indirect *indirect = &storage->Memory.location.Indirect;
          memory = *(const void **)win32_debugger_register_memory(ContextRecord, indirect->base_register);
          memory = ((u8 *)memory) + indirect->offset;
        } break;
        case Memory_Location_Tag_Stack: {
          const Memory_Location_Stack *stack = &storage->Memory.location.Stack;
          switch (stack->area) {
            case Stack_Area_Local: {
              memory = *(const void **)win32_debugger_register_memory(ContextRecord, Register_SP);
              s32 effective_offset = builder->stack_reserve + stack->offset;
              memory = ((u8 *)memory) + effective_offset;
            } break;
            case Stack_Area_Received_Argument: {
              printf("TODO support stack argument storage");
            } break;
            case Stack_Area_Call_Target_Argument: {
              assert("Got an unexpected call target argument stack storage");
            } break;
          }
        } break;
      }
    } break;
    case Storage_Tag_Disjoint: {
      printf("TODO support disjoint storage");
      return;
    }
  }
  mass_print_value_with_descriptor_and_memory(value->descriptor, memory, 0);
  printf("\n");
}

static void
win32_debugger_loop(
  CONTEXT *ContextRecord,
  Win32_Exception_Data *exception_data
) {
  char line_buffer[256] = {0};
  for (;;) {
    fputs("mdb> ", stdout);
    const char *command_c_string = fgets(line_buffer, countof(line_buffer), stdin);
    if (!command_c_string) break;

    Slice command = slice_from_c_string(command_c_string);
    command = slice_trim_whitespace(command);
    if (
      slice_equal(command, slice_literal("continue")) ||
      slice_equal(command, slice_literal("cont"))
    ) {
      break;
    } else if (
      slice_equal(command, slice_literal("backtrace")) ||
      slice_equal(command, slice_literal("bt"))
    ) {
      win32_print_stack(
        ContextRecord->Rsp,
        ContextRecord->Rip,
        exception_data->compilation,
        exception_data->jit
      );
    } else if (
      slice_equal(command, slice_literal("registers"))
    ) {
      // TODO support this for each stack frame
      win32_print_register_state(ContextRecord);
    } else if (
      slice_equal(command, slice_literal("locals"))
    ) {
      const Scope *maybe_scope = win32_debugger_maybe_scope_for_address(
        ContextRecord->Rip, exception_data->jit
      );
      if (maybe_scope) scope_print_names(maybe_scope, Scope_Print_Flags_Stop_At_First_Declarative);
    } else if (slice_starts_with(command, slice_literal("print "))) {
      Jit *jit = exception_data->jit;
      s64 runtime_function_index = win32_get_function_index_from_address(ContextRecord->Rip, jit);
      if (runtime_function_index < 0) {
        printf("Variable printing is not available in external code.\n");
        return;
      }

      Function_Builder *builder = dyn_array_get(jit->program->functions, runtime_function_index);
      Slice variable_name = slice_sub(command, strlen("print "), command.length);
      variable_name = slice_trim_whitespace(variable_name);
      const Scope *maybe_scope = win32_debugger_maybe_scope_for_address(
        ContextRecord->Rip, exception_data->jit
      );
      if (maybe_scope) {
        const Symbol *variable_symbol = mass_ensure_symbol(exception_data->compilation, variable_name);
        Scope_Entry *scope_entry = scope_lookup(maybe_scope, variable_symbol);
        if (scope_entry) {
          mass_debug_print_value(ContextRecord, builder, scope_entry->value);
        } else {
          printf("Undefined variable '%"PRIslice"'\n", SLICE_EXPAND_PRINTF(variable_name));
        }
      }
    } else if (
      slice_equal(command, slice_literal(""))
    ) {
      // Nothing to do
    } else {
      printf("Unknown command: %s", command_c_string);
    }
  }
}

static EXCEPTION_DISPOSITION
win32_program_test_exception_handler(
  EXCEPTION_RECORD *ExceptionRecord,
  u64 EstablisherFrame,
  CONTEXT *ContextRecord,
  DISPATCHER_CONTEXT *DispatcherContext
) {
  Win32_Exception_Data *exception_data = DispatcherContext->HandlerData;

  if (!exception_data->jit->is_stack_unwinding_in_progress) {
    exception_data->jit->is_stack_unwinding_in_progress = true;
    switch(ExceptionRecord->ExceptionCode) {
      case EXCEPTION_ACCESS_VIOLATION: {
        printf("Unhandled Exception: Access Violation.\n");
        break;
      }
      case EXCEPTION_ARRAY_BOUNDS_EXCEEDED: {
        printf("Unhandled Exception: Hardware Array Bounds Check Failed.\n");
        break;
      }
      case EXCEPTION_BREAKPOINT: {
        printf("Unhandled Exception: User Breakpoint hit\n");
        win32_debugger_loop(ContextRecord, exception_data);
        // Move instruction pointer over the int3 (0xCC) instruction
        ContextRecord->Rip += 1;
        return ExceptionContinueExecution;
      }
      case EXCEPTION_DATATYPE_MISALIGNMENT: {
        printf("Unhandled Exception: Misaligned Read / Write.\n");
        break;
      }
      case EXCEPTION_FLT_DENORMAL_OPERAND: {
        printf("Unhandled Exception: Denormal Float Value Result.\n");
        break;
      }
      case EXCEPTION_FLT_DIVIDE_BY_ZERO: {
        printf("Unhandled Exception: Float Divide By Zero.\n");
        break;
      }
      case EXCEPTION_FLT_INEXACT_RESULT: {
        printf("Unhandled Exception: Float Inexact Decimal Fraction.\n");
        break;
      }
      case EXCEPTION_FLT_INVALID_OPERATION: {
        printf("Unhandled Exception: Float Invalid Operation.\n");
        break;
      }
      case EXCEPTION_FLT_OVERFLOW: {
        printf("Unhandled Exception: Float Overflow.\n");
        break;
      }
      case EXCEPTION_FLT_STACK_CHECK: {
        printf("Unhandled Exception: Stack Overflow After Float Operation.\n");
        break;
      }
      case EXCEPTION_FLT_UNDERFLOW: {
        printf("Unhandled Exception: Float Underflow.\n");
        break;
      }
      case EXCEPTION_ILLEGAL_INSTRUCTION: {
        printf("Unhandled Exception: Illegal Machine Code Instruction.\n");
        break;
      }
      case EXCEPTION_IN_PAGE_ERROR: {
        printf("Unhandled Exception: Read Missing Memory Page.\n");
        break;
      }
      case EXCEPTION_INT_DIVIDE_BY_ZERO: {
        printf("Unhandled Exception: Integer Divide By Zero.\n");
        break;
      }
      case EXCEPTION_INT_OVERFLOW: {
        printf("Unhandled Exception: Integer Overflow.\n");
        break;
      }
      case EXCEPTION_INVALID_DISPOSITION: {
        printf("Unhandled Exception: Invalid Disposition From An Exception Handler.\n");
        break;
      }
      case EXCEPTION_NONCONTINUABLE_EXCEPTION: {
        printf("Unhandled Exception: Continue Execution After Noncontinuable Exception.\n");
        break;
      }
      case EXCEPTION_PRIV_INSTRUCTION: {
        printf("Unhandled Exception: Instruction Not Allowed In Current CPU Mode.\n");
        break;
      }
      case EXCEPTION_SINGLE_STEP: {
        printf("Unhandled Exception: Single Step Instruction.\n");
        break;
      }
      case EXCEPTION_STACK_OVERFLOW: {
        // If stack overflow happens during compile time JIT execution
        // and if there is a `setjmp` before such call it should be possible
        // to use the info in the saved `jmp_buf` to restore the stack to a
        // somewhat reasonable state and `_resetstkoflw` to restore the guard
        // page and then continue the execution to properly display the error
        // to the user

        // Right now we can't do much in this case and just let the system / debugger handle it
        return ExceptionContinueSearch;
      }
      default: {
        printf("Unknown.\n");
        break;
      }
    }
    win32_print_stack(
      ContextRecord->Rsp,
      ContextRecord->Rip,
      exception_data->compilation,
      exception_data->jit
    );
  }

  return ExceptionContinueSearch;
}

static u64
win32_buffer_ensure_last_page_is_writable(
  Virtual_Memory_Buffer *buffer
) {
  const s32 page_size = memory_page_size();
  const u64 remainder = buffer->occupied % page_size;
  const u64 protected = buffer->occupied - remainder;
  if (remainder != 0) {
    VirtualProtect(buffer->memory + protected, page_size, PAGE_READWRITE, &(DWORD){0});
  }
  return protected;
}

static void
win32_section_protect_from(
  Section *section,
  u64 from
) {
  DWORD flags = win32_section_permissions_to_virtual_protect_flags(section->permissions);
  u64 size_to_protect = section->buffer.occupied - from;
  if (size_to_protect) {
    VirtualProtect(section->buffer.memory + from, size_to_protect, flags, &(DWORD){0});
  }
}

static void *win32_load_library(const char *name) { return LoadLibraryA(name); }
static void *win32_load_symbol(void *handle, const char *name) { return (void *)GetProcAddress(handle, name); }

static const Native_Library_Load_Callbacks win32_library_load_callbacks = {
  .load_library = win32_load_library,
  .load_symbol = win32_load_symbol,
};

static void
win32_program_jit(
  Compilation *compilation,
  Jit *jit
) {
  Program *program = jit->program;
  Program_Memory *memory = &jit->program->memory;
  Virtual_Memory_Buffer *code_buffer = &memory->code.buffer;
  Virtual_Memory_Buffer *ro_data_buffer = &memory->ro_data.buffer;

  Win32_Jit_Info *info;
  if (jit->platform_specific_payload) {
    dyn_array_clear(jit->program->patch_info_array);
    info = jit->platform_specific_payload;
  } else {
    info = allocator_allocate(allocator_default, Win32_Jit_Info);
    *info = (Win32_Jit_Info) {
      .trampoline_rva = memory->code.base_rva + make_trampoline(
        code_buffer, (s64)win32_program_test_exception_handler
      ),
      .function_table = dyn_array_make(
        Array_RUNTIME_FUNCTION, .allocator = allocator_system, .capacity = 128,
      ),
    };
    jit->platform_specific_payload = info;
    // https://docs.microsoft.com/en-us/windows/win32/api/winnt/nf-winnt-rtlinstallfunctiontablecallback
    {
      // IMPORTANT this needs to use full program memory address space
      //           otherwise RVA calculation will be broken
      DWORD64 base_address = (s64)memory->buffer.memory;
      DWORD64 table_id = base_address | 0x3;
      DWORD length = u64_to_u32(memory->buffer.capacity);
      // :FunctionTableCallbackMax2Gb
      // The definition of RtlInstallFunctionTableCallback says that the length
      // parameter is DWORD, which is an alias for `unsigned long`. This should allow
      // for up to 4GB sizes, however the function actually fails if you provide
      // length more than 2Gb. Sigh.
      if (!RtlInstallFunctionTableCallback(
        table_id, base_address, length,
        (PGET_RUNTIME_FUNCTION_CALLBACK)win32_get_runtime_function_callback,
        jit, 0
      )) {
        panic("RtlInstallFunctionTableCallback failed");
      }
    }
  }

  program_jit_imports(compilation, jit, ro_data_buffer, &win32_library_load_callbacks);
  if (mass_has_error(compilation)) return;

  u64 function_count = dyn_array_length(program->functions);
  if (jit->previous_counts.functions != function_count) {
    assert(dyn_array_length(info->function_table) == jit->previous_counts.functions);
    dyn_array_reserve_uninitialized(info->function_table, function_count);
    // Memory protection works on per-page level so with incremental JIT there are two options:
    // 1. Waste memory every time we do JIT due to padding to page size.
    // 2. Switch memory back to writable before new writes.
    // On Windows options 2 is preferable, however some unix-like system disallow multiple
    // transitions between writable and executable so might have to resort to 1 there.
    u64 code_protected_size = win32_buffer_ensure_last_page_is_writable(code_buffer);

    // Encode newly added functions
    for (u64 i = jit->previous_counts.functions; i < function_count; ++i) {
      Function_Builder *builder = dyn_array_get(program->functions, i);
      Function_Layout layout;

      fn_encode(program, code_buffer, builder, &layout);

      RUNTIME_FUNCTION *function = dyn_array_get(info->function_table, i);
      UNWIND_INFO *unwind_info = win32_init_runtime_info_for_function(
        builder, &layout, function, &memory->ro_data
      );
      // Handler (if present) must immediately follow the unwind info struct
      {
        unwind_info->Flags |= UNW_FLAG_EHANDLER;
        u64 offset = virtual_memory_buffer_append_u32(
          &memory->ro_data.buffer, info->trampoline_rva
        );
        assert(offset % sizeof(DWORD) == 0);
        // :ExceptionDataAlignment
        // This data might end up misaligned on 64 bit OSes because UNWIND_INFO
        // as well as trampoline_rva are both 32-bit aligned while exception
        // data contains addresses which are 64 bits. Whether it is misaligned or
        // not would depend on the count of unwind codes so the best we can do is
        // to just mark the struct as packed and let the compiler deal with
        // potentially misaligned reads and writes.
        Win32_Exception_Data *exception_data = virtual_memory_buffer_allocate_bytes(
          &memory->ro_data.buffer, sizeof(Win32_Exception_Data), sizeof(DWORD)
        );
        *exception_data = (Win32_Exception_Data) {
          .builder = builder,
          .jit = jit,
          .compilation = compilation,
        };
      }
    }
    jit->previous_counts.functions = function_count;

    // After all the functions are encoded we should know all the offsets
    // and can patch all the label locations
    program_patch_labels(program);

    // Setup permissions for the code segment
    {
      win32_section_protect_from(&memory->code, code_protected_size);
      u64 size_to_flush = memory->code.buffer.occupied - code_protected_size;
      if (size_to_flush) {
        if (!FlushInstructionCache(
          GetCurrentProcess(),
          code_buffer->memory + code_protected_size,
          size_to_flush
        )) {
          panic("Unable to flush instruction cache");
        }
      }
    }
  }

  {
    // Keep all the pages of read-only segment read-only protected
    // The last page is kept writable as unprotecting it for every JIT attempt can be
    // prohibitable expensive - easily end up with a 100% increase in compilation time.
    u64 page_size = memory_page_size();
    u64 current_full_pages = memory->ro_data.buffer.occupied / page_size;
    if (jit->previous_counts.protected_ro_data_page_count < current_full_pages) {
      DWORD flags = win32_section_permissions_to_virtual_protect_flags(memory->ro_data.permissions);
      u64 pages_to_protect = current_full_pages - jit->previous_counts.protected_ro_data_page_count;
      u64 size_to_protect = pages_to_protect * page_size;
      u64 from = jit->previous_counts.protected_ro_data_page_count * page_size;
      VirtualProtect(memory->ro_data.buffer.memory + from, size_to_protect, flags, &(DWORD){0});
      jit->previous_counts.protected_ro_data_page_count = current_full_pages;
    }
  }


  program_jit_resolve_relocations(jit);
}


#endif