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
  _Alignas(DWORD) const Function_Builder *builder;
  _Alignas(DWORD) Jit *jit;
} Win32_Exception_Data;

void
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

s64
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

RUNTIME_FUNCTION *
win32_get_runtime_function_callback(
  DWORD64 instruction_address,
  Jit *jit
) {
  s64 runtime_function_index = win32_get_function_index_from_address(instruction_address, jit);
  if (runtime_function_index < 0) return 0;
  Win32_Jit_Info *info = jit->platform_specific_payload;
  return dyn_array_get(info->function_table, runtime_function_index);
}

const Instruction *
win32_instruction_for_address(
  DWORD64 instruction_address,
  Jit *jit
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
  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    current_offset += instruction->encoded_byte_size;
    if (current_offset == relative_instruction_byte_offset) {
      return instruction;
    }
  }
  return 0;
}

void
win32_print_stack(
  DWORD64 stack_pointer,
  DWORD64 instruction_address,
  Jit *jit
) {
  s64 runtime_function_index = win32_get_function_index_from_address(instruction_address, jit);
  if (runtime_function_index < 0) {
    // TODO adjust instruction address to point to the start of the function
    printf("  stopped at external code at %016llX\n", instruction_address);
    return;
  }
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
  for (u64 i = 0; i < dyn_array_length(builder->code_block.instructions); ++i) {
    Instruction *instruction = dyn_array_get(builder->code_block.instructions, i);
    current_offset += instruction->encoded_byte_size;
    if (current_offset == relative_instruction_byte_offset) {
      printf("  at ");
      source_range_print_start_position(&instruction->source_range);
    }
  }

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

  win32_print_stack(stack_pointer, return_address, jit);
}

EXCEPTION_DISPOSITION
win32_program_test_exception_handler(
  EXCEPTION_RECORD *ExceptionRecord,
  u64 EstablisherFrame,
  CONTEXT *ContextRecord,
  DISPATCHER_CONTEXT *DispatcherContext
) {
  Win32_Exception_Data *exception_data = DispatcherContext->HandlerData;

  if (!exception_data->jit->is_stack_unwinding_in_progress) {
    printf("Unhandled Exception: ");

    switch(ExceptionRecord->ExceptionCode) {
      case EXCEPTION_ACCESS_VIOLATION: {
        printf("Access Violation.\n");
        break;
      }
      case EXCEPTION_ARRAY_BOUNDS_EXCEEDED: {
        printf("Hardware Array Bounds Check Failed.\n");
        break;
      }
      case EXCEPTION_BREAKPOINT: {
        printf("User Breakpoint hit\n");
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
            win32_print_stack(ContextRecord->Rsp, ContextRecord->Rip, exception_data->jit);
          } else if (
            slice_equal(command, slice_literal("registers"))
          ) {
            // TODO support this for each stack frame
            win32_print_register_state(ContextRecord);
          } else if (
            slice_equal(command, slice_literal("locals"))
          ) {
            const Instruction *instruction =
              win32_instruction_for_address(ContextRecord->Rip, exception_data->jit);
            if (instruction && instruction->scope) {
              scope_print_names(instruction->scope);
            } else {
              printf("No debug information for current IP is available\n");
            }
          } else if (slice_starts_with(command, slice_literal("print "))) {
            Slice variable_name = slice_sub(command, strlen("print "), command.length);
            variable_name = slice_trim_whitespace(variable_name);
            const Instruction *instruction =
              win32_instruction_for_address(ContextRecord->Rip, exception_data->jit);
            if (instruction && instruction->scope) {
              Scope_Entry *scope_entry = scope_lookup(instruction->scope, variable_name);
              if (scope_entry->tag != Scope_Entry_Tag_Value) {
                panic("TODO support other scope entry types");
              }
              Value *value = scope_entry->Value.value;
              if (value) {
                if (value->descriptor->name.length) {
                  // TODO print actual value
                  printf("%"PRIslice" {0}", SLICE_EXPAND_PRINTF(value->descriptor->name));
                  puts("");
                }
                // TODO
                //print_operand(&value->storage);
                //puts("");
                if (value->descriptor == &descriptor_s64) {
                  // TODO
                  //switch(value->storage.tag) {
                    //default:
                    //case Storage_Tag_None:
                    //case Storage_Tag_Any:
                    //case Storage_Tag_Static:
                    //case Storage_Tag_Eflags:
                    //case Storage_Tag_Xmm:
                    //case Storage_Tag_Memory: {
                      //panic("TODO implement printing this storage type");
                      //break;
                    //}
                    //case Storage_Tag_Register: {
                      //assert(value->storage.byte_size == 8);
                      //DWORD64 register_value = 0;
                      //switch(value->storage.Register.index) {
                        //case Register_A: register_value = ContextRecord->Rax; break;
                        //case Register_C: register_value = ContextRecord->Rcx; break;
                        //case Register_D: register_value = ContextRecord->Rdx; break;
                        //case Register_B: register_value = ContextRecord->Rbx; break;
                        //case Register_SP: register_value = ContextRecord->Rsp; break;
                        //case Register_BP: register_value = ContextRecord->Rbp; break;
                        //case Register_SI: register_value = ContextRecord->Rsi; break;
                        //case Register_DI: register_value = ContextRecord->Rdi; break;
                        //case Register_R8: register_value = ContextRecord->R8; break;
                        //case Register_R9: register_value = ContextRecord->R9; break;
                        //case Register_R10: register_value = ContextRecord->R10; break;
                        //case Register_R11: register_value = ContextRecord->R11; break;
                        //case Register_R12: register_value = ContextRecord->R12; break;
                        //case Register_R13: register_value = ContextRecord->R13; break;
                        //case Register_R14: register_value = ContextRecord->R14; break;
                        //case Register_R15: register_value = ContextRecord->R15; break;
                        //case Register_Xmm0:
                        //case Register_Xmm1:
                        //case Register_Xmm2:
                        //case Register_Xmm3:
                        //case Register_Xmm4:
                        //case Register_Xmm5:
                        //case Register_Xmm6:
                        //case Register_Xmm7:
                        //case Register_Xmm8:
                        //case Register_Xmm9:
                        //case Register_Xmm10:
                        //case Register_Xmm11:
                        //case Register_Xmm12:
                        //case Register_Xmm13:
                        //case Register_Xmm14:
                        //case Register_Xmm15:
                        //default: {
                          //printf("TODO support XMM registers\n");
                          //break;
                        //}
                      //}
                      //printf("0x%016llX\n", register_value);
                    //}
                  //}
                } else {
                  printf("TODO support generic printing of values\n");
                }
              } else {
                printf("No debug information for current IP is available\n");
              }
            } else {
              printf("Undefined variable '%"PRIslice"'\n", SLICE_EXPAND_PRINTF(variable_name));
            }
          } else {
            printf("Unknown command: %s", command_c_string);
          }
        }
        // Move instruction pointer over the int3 (0xCC) instruction
        ContextRecord->Rip += 1;
        return ExceptionContinueExecution;
      }
      case EXCEPTION_DATATYPE_MISALIGNMENT: {
        printf("Misaligned Read / Write.\n");
        break;
      }
      case EXCEPTION_FLT_DENORMAL_OPERAND: {
        printf("Denormal Float Value Result.\n");
        break;
      }
      case EXCEPTION_FLT_DIVIDE_BY_ZERO: {
        printf("Float Divide By Zero.\n");
        break;
      }
      case EXCEPTION_FLT_INEXACT_RESULT: {
        printf("Float Inexact Decimal Fraction.\n");
        break;
      }
      case EXCEPTION_FLT_INVALID_OPERATION: {
        printf("Float Invalid Operation.\n");
        break;
      }
      case EXCEPTION_FLT_OVERFLOW: {
        printf("Float Overflow.\n");
        break;
      }
      case EXCEPTION_FLT_STACK_CHECK: {
        printf("Stack Overflow After Float Operation.\n");
        break;
      }
      case EXCEPTION_FLT_UNDERFLOW: {
        printf("Float Underflow.\n");
        break;
      }
      case EXCEPTION_ILLEGAL_INSTRUCTION: {
        printf("Illegal Machine Code Instruction.\n");
        break;
      }
      case EXCEPTION_IN_PAGE_ERROR: {
        printf("Read Missing Memory Page.\n");
        break;
      }
      case EXCEPTION_INT_DIVIDE_BY_ZERO: {
        printf("Integer Divide By Zero.\n");
        break;
      }
      case EXCEPTION_INT_OVERFLOW: {
        printf("Integer Overflow.\n");
        break;
      }
      case EXCEPTION_INVALID_DISPOSITION: {
        printf("Invalid Disposition From An Exception Handler.\n");
        break;
      }
      case EXCEPTION_NONCONTINUABLE_EXCEPTION: {
        printf("Continue Execution After Noncontinuable Exception.\n");
        break;
      }
      case EXCEPTION_PRIV_INSTRUCTION: {
        printf("Instruction Not Allowed In Current CPU Mode.\n");
        break;
      }
      case EXCEPTION_SINGLE_STEP: {
        printf("Single Step Instruction.\n");
        break;
      }
      case EXCEPTION_STACK_OVERFLOW: {
        printf("Stack Overflow.\n");
        break;
      }
      default: {
        printf("Unknown.\n");
        break;
      }
    }
    win32_print_stack(ContextRecord->Rsp, ContextRecord->Rip, exception_data->jit);
    win32_print_register_state(ContextRecord);
    exception_data->jit->is_stack_unwinding_in_progress = true;
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

void *win32_load_library(const char *name) { return LoadLibraryA(name); }
void *win32_load_symbol(void *handle, const char *name) { return (void *)GetProcAddress(handle, name); }

static const Native_Library_Load_Callbacks win32_library_load_callbacks = {
  .load_library = win32_load_library,
  .load_symbol = win32_load_symbol,
};

// TODO make this return MASS_RESULT
static void
win32_program_jit(
  Compilation *compilation,
  Jit *jit
) {
  Program *program = jit->program;
  Program_Memory *memory = &jit->program->memory;
  Virtual_Memory_Buffer *code_buffer = &memory->code.buffer;
  Virtual_Memory_Buffer *ro_data_buffer = &memory->ro_data.buffer;

  // Memory protection works on per-page level so with incremental JIT there are two options:
  // 1. Waste memory every time we do JIT due to padding to page size.
  // 2. Switch memory back to writable before new writes.
  // On Windows options 2 is preferable, however some unix-like system disallow multiple
  // transitions between writable and executable so might have to resort to 1 there.
  u64 code_protected_size = win32_buffer_ensure_last_page_is_writable(code_buffer);
  u64 ro_data_protected_size = win32_buffer_ensure_last_page_is_writable(ro_data_buffer);

  Win32_Jit_Info *info;
  if (jit->platform_specific_payload) {
    dyn_array_clear(jit->program->patch_info_array);
    info = jit->platform_specific_payload;
  } else {
    info = allocator_allocate(allocator_default, Win32_Jit_Info);
    *info = (Win32_Jit_Info) {
      .trampoline_rva = memory->code.base_rva + make_trampoline(
        program, code_buffer, (u64)win32_program_test_exception_handler
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
        table_id, base_address, length, win32_get_runtime_function_callback, jit, 0
      )) {
        panic("RtlInstallFunctionTableCallback failed");
      }
    }
  }

  Mass_Result result = program_jit_imports(
    compilation->temp_allocator, jit, ro_data_buffer, &win32_library_load_callbacks
  );
  (void)result;

  u64 function_count = dyn_array_length(program->functions);
  assert(dyn_array_length(info->function_table) == jit->previous_counts.functions);
  dyn_array_reserve_uninitialized(info->function_table, function_count);

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
      };
    }
  }


  // After all the functions are encoded we should know all the offsets
  // and can patch all the label locations
  program_patch_labels(program);

  // Setup permissions for read-only data segment
  win32_section_protect_from(&memory->ro_data, ro_data_protected_size);

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

  // Resolve relocations
  u64 relocation_count = dyn_array_length(program->relocations);
  for (u64 i = jit->previous_counts.relocations; i < relocation_count; ++i) {
    Relocation *relocation = dyn_array_get(program->relocations, i);
    assert(storage_is_label(&relocation->patch_at));
    assert(storage_is_label(&relocation->address_of));
    Label_Index patch_at_index =
      relocation->patch_at.Memory.location.Instruction_Pointer_Relative.label_index;
    Label_Index address_of_index =
      relocation->address_of.Memory.location.Instruction_Pointer_Relative.label_index;
    void *address_of = rip_value_pointer_from_label_index(program, address_of_index);
    void **patch_at = rip_value_pointer_from_label_index(program, patch_at_index);
    *patch_at = address_of;
  }

  // Call new startup functions
  u64 startup_count = dyn_array_length(program->startup_functions);
  for (u64 i = jit->previous_counts.startup; i < startup_count; ++i) {
    Value *value = *dyn_array_get(program->startup_functions, i);
    fn_type_opaque fn = value_as_function(program, value);
    fn();
  }

  jit->previous_counts.relocations = relocation_count;
  jit->previous_counts.functions = function_count;
  jit->previous_counts.startup = startup_count;
}


#endif