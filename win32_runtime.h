#ifndef WIN32_RUNTIME_H
#define WIN32_RUNTIME_H

#include "win32_platform.h"
#include "program.h"
#include "source.h"


typedef enum {
  Debugger_Register_Type_General,
  Debugger_Register_Type_Vector,
  Debugger_Register_Type_Stack_Pointer,
  Debugger_Register_Type_Instruction_Pointer,
  Debugger_Register_Type_Opaque,
} Debugger_Register_Type;

typedef struct {
  Slice name;
  u64 offset;
  u32 bit_size;
  Debugger_Register_Type type;
} Debugger_Register_Info;

static const Debugger_Register_Info DEBUGGER_X86_64_REGISTER_INFO_ARRAY[] = {
  {.name = slice_literal_fields("rip"), .bit_size = 64, .offset = 0, .type = Debugger_Register_Type_Instruction_Pointer},
  {.name = slice_literal_fields("eflags"), .bit_size = 32, .offset = 8, .type = Debugger_Register_Type_Opaque},

  {.name = slice_literal_fields("rax"), .bit_size = 64, .offset = 16, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("rcx"), .bit_size = 64, .offset = 24, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("rdx"), .bit_size = 64, .offset = 32, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("rbx"), .bit_size = 64, .offset = 40, .type = Debugger_Register_Type_General},

  {.name = slice_literal_fields("rsp"), .bit_size = 64, .offset = 48, .type = Debugger_Register_Type_Stack_Pointer},
  {.name = slice_literal_fields("rbp"), .bit_size = 64, .offset = 56, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("rsi"), .bit_size = 64, .offset = 64, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("rdi"), .bit_size = 64, .offset = 72, .type = Debugger_Register_Type_General},

  {.name = slice_literal_fields("r8"),  .bit_size = 64, .offset = 80, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r9"), .bit_size = 64, .offset = 88, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r10"), .bit_size = 64, .offset = 96, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r11"), .bit_size = 64, .offset = 104, .type = Debugger_Register_Type_General},

  {.name = slice_literal_fields("r12"), .bit_size = 64, .offset = 112, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r13"), .bit_size = 64, .offset = 120, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r14"), .bit_size = 64, .offset = 128, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("r15"), .bit_size = 64, .offset = 136, .type = Debugger_Register_Type_General},

  {.name = slice_literal_fields("xmm0"), .bit_size = 128, .offset = 144, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm1"), .bit_size = 128, .offset = 160, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm2"), .bit_size = 128, .offset = 176, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm3"), .bit_size = 128, .offset = 192, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm4"), .bit_size = 128, .offset = 208, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm5"), .bit_size = 128, .offset = 224, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm6"), .bit_size = 128, .offset = 240, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm7"), .bit_size = 128, .offset = 256, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm8"), .bit_size = 128, .offset = 272, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm9"), .bit_size = 128, .offset = 288, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm10"), .bit_size = 128, .offset = 304, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm11"), .bit_size = 128, .offset = 320, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm12"), .bit_size = 128, .offset = 336, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm13"), .bit_size = 128, .offset = 352, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm14"), .bit_size = 128, .offset = 368, .type = Debugger_Register_Type_General},
  {.name = slice_literal_fields("xmm15"), .bit_size = 128, .offset = 384, .type = Debugger_Register_Type_General},
};

typedef struct {
  Allocator *temp_allocator;
  Compilation *compilation;
  Jit *jit;

  const Debugger_Register_Info *register_info;
  u64 register_count;
  u8* register_memory;
} Debugger_Context;

static u64
debugger_first_register_value_with_type(
  const Debugger_Context *debugger_context,
  Debugger_Register_Type type
) {
  for (u64 i = 0; i < debugger_context->register_count; ++i) {
    const Debugger_Register_Info *register_info = &debugger_context->register_info[i];
    if (register_info->type == type) {
      assert(register_info->bit_size == 64); // We support only 64 architectures for now
      return *(const u64*)(debugger_context->register_memory + register_info->offset);
    }
  }
  assert(!"Could not find a register with specified type");
  return 0;
}

static void*
debugger_register_memory_with_name(
  Debugger_Context *debugger_context,
  Slice name
) {
  for (u64 i = 0; i < debugger_context->register_count; ++i) {
    const Debugger_Register_Info *register_info = &debugger_context->register_info[i];
    if (slice_equal(register_info->name, name)) {
      return debugger_context->register_memory + register_info->offset;
    }
  }
  assert(!"Could not find a register with specified name");
  return 0;
}

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

typedef dyn_array_type(RUNTIME_FUNCTION) Array_RUNTIME_FUNCTION;

typedef struct {
  Array_RUNTIME_FUNCTION function_table;
  u32 trampoline_rva;
} Win32_Jit_Info;

static s64
win32_get_function_index_from_address(
  u64 instruction_address,
  Jit *jit
) {
  Win32_Jit_Info *info = jit->platform_specific_payload;
  Section *code_section = &jit->program->memory.code;
  Virtual_Memory_Buffer *code_buffer = &code_section->buffer;
  if(instruction_address < (u64)code_buffer->memory) return -1;
  if(instruction_address >= (u64)code_buffer->memory + code_buffer->occupied) return -1;

  u32 rva = u64_to_u32(code_section->base_rva + instruction_address - (u64)code_buffer->memory);

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
      if (current_offset == relative_instruction_byte_offset) {
        return instruction;
      }
      current_offset += instruction->Bytes.length;
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
debugger_x86_64_register_memory(
  Debugger_Context *debugger_context,
  Register reg
) {
  switch (reg) {
    case Register_A: return debugger_register_memory_with_name(debugger_context, slice_literal("rax"));
    case Register_C: return debugger_register_memory_with_name(debugger_context, slice_literal("rcx"));
    case Register_D: return debugger_register_memory_with_name(debugger_context, slice_literal("rdx"));
    case Register_B: return debugger_register_memory_with_name(debugger_context, slice_literal("rbx"));
    case Register_SP: return debugger_register_memory_with_name(debugger_context, slice_literal("rsp"));
    case Register_BP: return debugger_register_memory_with_name(debugger_context, slice_literal("rbp"));
    case Register_SI: return debugger_register_memory_with_name(debugger_context, slice_literal("rsi"));
    case Register_DI: return debugger_register_memory_with_name(debugger_context, slice_literal("rdi"));
    case Register_R8: return debugger_register_memory_with_name(debugger_context, slice_literal("r8"));
    case Register_R9: return debugger_register_memory_with_name(debugger_context, slice_literal("r9"));
    case Register_R10: return debugger_register_memory_with_name(debugger_context, slice_literal("r10"));
    case Register_R11: return debugger_register_memory_with_name(debugger_context, slice_literal("r11"));
    case Register_R12: return debugger_register_memory_with_name(debugger_context, slice_literal("r12"));
    case Register_R13: return debugger_register_memory_with_name(debugger_context, slice_literal("r13"));
    case Register_R14: return debugger_register_memory_with_name(debugger_context, slice_literal("r14"));
    case Register_R15: return debugger_register_memory_with_name(debugger_context, slice_literal("r15"));
    default: assert(!"Unknown general purpose register index"); break;
  }
  return 0;
}

static const void *
mass_debugger_value_memory(
  Debugger_Context *debugger_context,
  Function_Builder *builder,
  const Value *value
) {
  assert(value->tag == Value_Tag_Forced);
  const Storage *storage = &value->Forced.storage;
  switch (storage->tag) {
    case Storage_Tag_Immediate: {
      return &storage->Immediate.bits;
    }
    case Storage_Tag_Eflags: {
      // TODO storage->Eflags.compare_type
      assert(!"TODO");
      return 0;
    }
    case Storage_Tag_Register: {
      const void *memory = debugger_x86_64_register_memory(debugger_context, storage->Register.index);
      assert(storage->Register.offset_in_bits % 8 == 0);
      return ((u8 *)memory) + (storage->Register.offset_in_bits / 8);
    }
    case Storage_Tag_Xmm: {
      switch (storage->Xmm.index) {
        case Register_Xmm0: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm0"));
        case Register_Xmm1: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm1"));
        case Register_Xmm2: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm2"));
        case Register_Xmm3: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm3"));
        case Register_Xmm4: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm4"));
        case Register_Xmm5: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm5"));
        case Register_Xmm6: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm6"));
        case Register_Xmm7: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm7"));
        case Register_Xmm8: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm8"));
        case Register_Xmm9: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm9"));
        case Register_Xmm10: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm10"));
        case Register_Xmm11: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm11"));
        case Register_Xmm12: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm12"));
        case Register_Xmm13: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm13"));
        case Register_Xmm14: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm14"));
        case Register_Xmm15: return debugger_register_memory_with_name(debugger_context, slice_literal("xmm15"));
        default: assert(!"Unknown XMM register index"); break;
      }
    } break;
    case Storage_Tag_Static: {
      return storage->Static.pointer;
    }
    case Storage_Tag_Memory: {
      switch (storage->Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          const Memory_Location_Instruction_Pointer_Relative *rip_relative =
            &storage->Memory.location.Instruction_Pointer_Relative;
          Label *label = rip_relative->label;
          assert(label->resolved);
          assert(label->section);
          return ((u8*)label->section->buffer.memory) + label->offset_in_section;
        }
        case Memory_Location_Tag_Indirect: {
          const Memory_Location_Indirect *indirect = &storage->Memory.location.Indirect;
          const void *memory = *(const void **) debugger_x86_64_register_memory(debugger_context, indirect->base_register);
          return ((u8 *)memory) + indirect->offset;
        }
        case Memory_Location_Tag_Stack: {
          const Memory_Location_Stack *stack = &storage->Memory.location.Stack;
          switch (stack->area) {
            case Stack_Area_Local: {
              const void *memory = *(const void **) debugger_x86_64_register_memory(debugger_context, Register_SP);
              s32 effective_offset = builder->stack_reserve + stack->offset;
              return ((u8 *)memory) + effective_offset;
            }
            case Stack_Area_Received_Argument: {
              // :StackLayout
              const void *memory = *(const void **) debugger_x86_64_register_memory(debugger_context, Register_SP);
              s32 push_size = calling_convention_x86_64_push_size(builder);
              s32 return_address_size = X86_64_REGISTER_SIZE;
              s32 effective_offset = builder->stack_reserve + push_size + return_address_size + stack->offset;
              return ((u8 *)memory) + effective_offset;
            }
            case Stack_Area_Call_Target_Argument: {
              assert("Got an unexpected call target argument stack storage");
            } break;
          }
        } break;
      }
    } break;
    case Storage_Tag_Disjoint: {
      printf("TODO support disjoint storage");
    }
  }
  return 0;
}

// TODO catch cycles
static void
mass_print_value_with_descriptor_and_memory_internal(
  const Descriptor *descriptor,
  const u8 *memory,
  u64 depth
) {
  if (memory == 0) {
    printf("<NULL>");
    return;
  }
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
      Bucket_Buffer *buffer = bucket_buffer_make();
      mass_append_function_signature_string(buffer, descriptor->Function_Instance.info);
      Fixed_Buffer *fixed_buffer = bucket_buffer_to_fixed_buffer(allocator_default, buffer);
      bucket_buffer_destroy(buffer);
      Slice slice = fixed_buffer_slice(fixed_buffer, (Range_u64){0, fixed_buffer->occupied});
      printf("%"PRIslice"", SLICE_EXPAND_PRINTF(slice));
      fixed_buffer_destroy(fixed_buffer);
    } break;
    case Descriptor_Tag_Fixed_Array: {
      printf("[");
      const Descriptor *item_descriptor = descriptor->Fixed_Array.item;
      u64 item_byte_size = descriptor_byte_size(item_descriptor);
      for (u64 i = 0; i < descriptor->Fixed_Array.length; ++i) {
        if (i != 0) printf(", ");
        const u8 *field_memory = memory + item_byte_size * i;
        mass_print_value_with_descriptor_and_memory_internal(
          item_descriptor, field_memory, depth + 1
        );
      }
      printf("]");
    } break;
    case Descriptor_Tag_Struct: {
      // FIXME Right now there is no real concept of a tagged union, so at least for the ones
      //       coming from the compiler we just do a bit of duck typing check.
      const Struct_Field *tag_field;
      bool has_tag = struct_find_field_by_name(descriptor, slice_literal("tag"), &tag_field, &(u64){ 0 });
      Scope_Map *tag_scope_map = 0;
      Slice chosen_tag = {0};
      if (
        has_tag &&
        tag_field->descriptor->bit_size.as_u64 == 32 &&
        tag_field->descriptor->tag == Descriptor_Tag_Raw &&
        tag_field->descriptor->brand &&
        tag_field->descriptor->own_module &&
        tag_field->descriptor->own_module->own_scope->tag == Scope_Tag_Declarative
      ) {
        tag_scope_map = tag_field->descriptor->own_module->own_scope->Declarative.map;
        const u8 *field_memory = memory + tag_field->offset;
        for (u64 i = 0; i < tag_scope_map->capacity; ++i) {
          Scope_Map__Entry *entry = &tag_scope_map->entries[i];
          if (entry->occupied) {
            const void *value_memory = storage_static_memory(&value_as_forced(entry->value->value)->storage);
            if (memcmp(field_memory, value_memory, 4) == 0) {
              chosen_tag = entry->value->name;
              printf(".%"PRIslice"", SLICE_EXPAND_PRINTF(chosen_tag));
              break;
            }
          }
        }
        if (chosen_tag.length == 0) {
          printf("<INVALID TAG %d>", *(u32*)field_memory);
        }
      }
      printf("[");
      for (u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
        const Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
        // If we are in the tagged union only print selected field
        if (tag_scope_map && !slice_equal(field->name, chosen_tag)) {
          // FIXME @Speed this is slow but getting a symbol here for a hash lookup is awkward atm
          for (u64 i = 0; i < tag_scope_map->capacity; ++i) {
            Scope_Map__Entry *entry = &tag_scope_map->entries[i];
            if (entry->occupied && slice_equal(entry->value->name, field->name)) goto skip_field;
          }
        }
        printf(".%"PRIslice" = ", SLICE_EXPAND_PRINTF(field->name));
        const u8 *field_memory = memory + field->offset;
        mass_print_value_with_descriptor_and_memory_internal(
          field->descriptor, field_memory, depth + 1
        );
        if (i + 1 != dyn_array_length(descriptor->Struct.fields)) printf(", ");
        skip_field:;
      }
      printf("]");
    } break;
    case Descriptor_Tag_Pointer_To: {
      const void *pointer = *(const void **)memory;
      printf("&<%p>: ", pointer);
      mass_print_value_with_descriptor_and_memory_internal(
        descriptor->Pointer_To.descriptor, pointer, depth + 1
      );
    } break;
    default: {
      assert(!"Unknown descriptor tag");
    } break;
  }
}

static void
mass_print_value_with_descriptor_and_memory(
  const Descriptor *descriptor,
  const void *memory
) {
  mass_print_value_with_descriptor_and_memory_internal(descriptor, memory, 0);
}

static void
debugger_loop(
  Debugger_Context *debugger_context
) {
  char line_buffer[256] = {0};
  const char *auto_command = 0;
  for (;;) {
    fputs("mdb> ", stdout);
    const char *command_c_string = auto_command;
    if (auto_command) {
      auto_command = 0;
    } else {
      command_c_string = fgets(line_buffer, countof(line_buffer), stdin);
    }
    if (!command_c_string) break;

    u64 stack_pointer = debugger_first_register_value_with_type(
      debugger_context, Debugger_Register_Type_Stack_Pointer);
    u64 instruction_pointer = debugger_first_register_value_with_type(
      debugger_context, Debugger_Register_Type_Instruction_Pointer);
    Compilation *compilation = debugger_context->compilation;
    Jit *jit = debugger_context->jit;

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
      win32_print_stack(stack_pointer, instruction_pointer, compilation, jit);
    } else if (
      slice_equal(command, slice_literal("locals"))
    ) {
      const Scope *maybe_scope = win32_debugger_maybe_scope_for_address(instruction_pointer, jit);
      if (maybe_scope) scope_print_names(maybe_scope, Scope_Print_Flags_Stop_At_First_Declarative);
    } else if (slice_starts_with(command, slice_literal("print "))) {
      s64 runtime_function_index = win32_get_function_index_from_address(instruction_pointer, jit);
      if (runtime_function_index < 0) {
        printf("Variable printing is not available in external code.\n");
        return;
      }

      Function_Builder *builder = dyn_array_get(jit->program->functions, runtime_function_index);
      Slice variable_name = slice_sub(command, strlen("print "), command.length);
      variable_name = slice_trim_whitespace(variable_name);
      const Scope *maybe_scope = win32_debugger_maybe_scope_for_address(instruction_pointer, jit);
      if (maybe_scope) {
        const Symbol *variable_symbol = mass_ensure_symbol(compilation, variable_name);
        Scope_Entry *scope_entry = scope_lookup(maybe_scope, variable_symbol);
        if (scope_entry) {
          const void *memory = mass_debugger_value_memory(debugger_context, builder, scope_entry->value);
          mass_print_value_with_descriptor_and_memory(scope_entry->value->descriptor, memory);
          printf("\n");
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
        Virtual_Memory_Buffer temp_buffer;
        virtual_memory_buffer_init(&temp_buffer, 4096 * 64);
        Allocator *temp_allocator = virtual_memory_buffer_allocator_make(&temp_buffer);

        // we could probably iterate over the array, but ContextRecord is definitely big enough as we copy from it
        u8 *register_memory = (u8 *)allocator_allocate(temp_allocator, CONTEXT);

        Debugger_Context debugger_context = {
          .temp_allocator = temp_allocator,
          .register_info = DEBUGGER_X86_64_REGISTER_INFO_ARRAY,
          .register_count = countof(DEBUGGER_X86_64_REGISTER_INFO_ARRAY),
          .register_memory = register_memory,
          .compilation = exception_data->compilation,
          .jit = exception_data->jit,
        };
        #define win32_copy_reg(_NAME_)\
          do {\
            char field_name[] = #_NAME_;         \
            for (int i = 0; i < countof(field_name); ++i) field_name[i] = (char)tolower(field_name[i]);                  \
            void *register_memory = debugger_register_memory_with_name(&debugger_context, slice_from_c_string(field_name));\
            memcpy(register_memory, &ContextRecord->_NAME_, sizeof(ContextRecord->_NAME_));\
      } while(0)
        win32_copy_reg(Rip); win32_copy_reg(EFlags);

        win32_copy_reg(Rax); win32_copy_reg(Rcx); win32_copy_reg(Rdx); win32_copy_reg(Rbx);
        win32_copy_reg(Rsp); win32_copy_reg(Rbp); win32_copy_reg(Rsi); win32_copy_reg(Rdi);

        win32_copy_reg(R8); win32_copy_reg(R9); win32_copy_reg(R10); win32_copy_reg(R11);
        win32_copy_reg(R12); win32_copy_reg(R13); win32_copy_reg(R14); win32_copy_reg(R15);

        win32_copy_reg(Xmm0);  win32_copy_reg(Xmm1);  win32_copy_reg(Xmm2);  win32_copy_reg(Xmm3);
        win32_copy_reg(Xmm4);  win32_copy_reg(Xmm5);  win32_copy_reg(Xmm6);  win32_copy_reg(Xmm7);
        win32_copy_reg(Xmm8);  win32_copy_reg(Xmm9);  win32_copy_reg(Xmm10); win32_copy_reg(Xmm11);
        win32_copy_reg(Xmm12); win32_copy_reg(Xmm13); win32_copy_reg(Xmm14); win32_copy_reg(Xmm15);
        #undef win32_copy_reg

        printf("Unhandled Exception: User Breakpoint hit\n");
        debugger_loop(&debugger_context);
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