#ifndef CALLING_CONVENTION_H
#define CALLING_CONVENTION_H

#include "types.h"
#include "value.h"

static Function_Call_Setup
calling_convention_x86_64_windows_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static const Calling_Convention calling_convention_x86_64_windows = {
  .call_setup_proc = calling_convention_x86_64_windows_call_setup_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_C) | (1llu << Register_D) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Return
    (1llu << Register_A) |
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

static Function_Call_Setup
calling_convention_x86_64_system_v_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
);

static const Calling_Convention calling_convention_x86_64_system_v = {
  .call_setup_proc = calling_convention_x86_64_system_v_call_setup_proc,
  .register_volatile_bitset = (
    // Arguments
    (1llu << Register_DI) | (1llu << Register_SI) | (1llu << Register_D) |
    (1llu << Register_C) | (1llu << Register_R8) | (1llu << Register_R9) |
    // Varargs / Return
    (1llu << Register_A) | (1llu << Register_D) | // 'D' is used both for args and return
    // Other
    (1llu << Register_R10) | (1llu << Register_R11)
  ),
};

#endif // CALLING_CONVENTION_H

static void
calling_convention_x86_64_common_end_proc(
  Program *program,
  Function_Builder *builder
);

#ifdef CALLING_CONVENTION_IMPLEMENTATION

static s32
calling_convention_x86_64_adjust_stack_offset(
  Stack_Area area,
  s32 stack_offset,
  s32 stack_reserve,
  s32 argument_stack_base
) {
  switch(area) {
    case Stack_Area_Local: {
      assert(stack_offset < 0);
      return stack_reserve + stack_offset;
    }
    case Stack_Area_Received_Argument: {
      assert(stack_offset >= 0);
      return argument_stack_base + stack_offset;
    }
    case Stack_Area_Call_Target_Argument: {
      assert(stack_offset >= 0);
      return stack_offset;
    }
  }
  panic("Unexpected stack area tag");
  return 0;
}

static void
calling_convention_x86_64_common_end_proc(
  Program *program,
  Function_Builder *builder
) {
  const s32 register_size = 8;

  s32 push_size = 0;
  // :RegisterPushPop
  // pushes change the stack pointer so we need to account for that
  for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
    if (register_bitset_get(builder->register_used_bitset, reg_index)) {
      if (!register_bitset_get(builder->register_volatile_bitset, reg_index)) {
        push_size += register_size;
      }
    }
  }

  builder->stack_reserve += builder->max_call_parameters_stack_size;

  // Here is how stack looks at this point
  //   > return address
  //   > some number of pushes
  //   > locals
  // first we make all of them 8-byte aligned - return address and pushes are
  // naturally register-sized and locals are aligned here:
  builder->stack_reserve = s32_align(builder->stack_reserve, register_size);
  // their sum must then be 16-byte aligned as per ABI
  s32 return_address_size = register_size;
  s32 argument_stack_base = builder->stack_reserve + push_size + return_address_size;
  if (argument_stack_base % 16) {
    argument_stack_base += register_size;
    builder->stack_reserve += register_size;
  }

  // Adjust stack locations
  Instruction *previous = 0;
  for (Instruction_Bucket *bucket = builder->code_block.first_bucket; bucket; bucket = bucket->next) {
    for (u64 i = 0; i < bucket->length; ++i) {
      Instruction *instruction = &bucket->items[i];
      switch(instruction->tag) {
        case Instruction_Tag_Label:
        case Instruction_Tag_Label_Patch: {
          // Nothing to do
        } break;
        case Instruction_Tag_Bytes: {
          // Handled below
        } break;
        // :StackPatch
        case Instruction_Tag_Stack_Patch: {
          assert(previous->tag == Instruction_Tag_Bytes);
          Instruction_Stack_Patch *patch = &instruction->Stack_Patch;
          u8 *mod_r_m = previous->Bytes.memory + patch->mod_r_m_offset_in_previous_instruction;
          u8 mod_r_m_byte_size = 1;
          u8 sib_byte_size = 1;
          s32 *displacement = (s32 *)(mod_r_m + mod_r_m_byte_size + sib_byte_size);
          *displacement = calling_convention_x86_64_adjust_stack_offset(
            patch->stack_area, *displacement, builder->stack_reserve, argument_stack_base
          );
          // :OversizedStackOffsets
          // Patch the instruction to have a smaller size displacement if it fits
          if (s32_fits_into_s8(*displacement)) {
            // overwrite MOD part with MOD_8
            *mod_r_m &= 0b00111111;
            *mod_r_m |= MOD_Displacement_s8 << 6;
            // Move the remaining bytes 3 positions to the left
            u8 *remainder_bytes = (u8 *)displacement + sizeof(s32);
            u8 *instruction_end = previous->Bytes.memory + previous->Bytes.length;
            s64 remainder_length = instruction_end - remainder_bytes;
            memmove((s8 *)displacement + 1, remainder_bytes, remainder_length);
            previous->Bytes.length -= sizeof(s32) - sizeof(s8);
          }
        } break;
      }
      previous = instruction;
    }
  }
}

typedef enum {
  SYSTEM_V_NO_CLASS,
  SYSTEM_V_INTEGER,
  SYSTEM_V_SSE,
  SYSTEM_V_SSEUP,
  SYSTEM_V_X87,
  SYSTEM_V_X87UP,
  SYSTEM_V_COMPLEX_X87,
  SYSTEM_V_MEMORY,
} SYSTEM_V_ARGUMENT_CLASS;
typedef dyn_array_type(SYSTEM_V_ARGUMENT_CLASS) Array_SYSTEM_V_ARGUMENT_CLASS;

typedef struct {
  SYSTEM_V_ARGUMENT_CLASS class;
  const Descriptor *descriptor;
  u64 eightbyte_count;
} System_V_Classification;

typedef struct {
  const Register *items;
  u32 count;
  u32 index;
} System_V_Registers;

typedef struct {
  System_V_Registers general;
  System_V_Registers vector;
} System_V_Register_State;

static void
x86_64_system_v_adjust_classification_if_no_register_available(
  System_V_Register_State *registers,
  System_V_Classification *classification
) {
  if (classification->class == SYSTEM_V_INTEGER) {
    if (registers->general.index + classification->eightbyte_count > registers->general.count) {
      classification->class = SYSTEM_V_MEMORY;
    }
  }
  if (classification->class == SYSTEM_V_SSE) {
    if (registers->vector.index + classification->eightbyte_count > registers->vector.count) {
      classification->class = SYSTEM_V_MEMORY;
    }
  }
}

static Memory_Layout_Item
x86_64_system_v_memory_layout_item_for_classification(
  System_V_Register_State *registers,
  const System_V_Classification *classification,
  Slice name,
  u64 *stack_offset
) {
  u64 byte_size = descriptor_byte_size(classification->descriptor);
  Storage storage = storage_none;
  switch(classification->class) {
    case SYSTEM_V_NO_CLASS: {
      goto absolute;
    } break;
    case SYSTEM_V_INTEGER: {
      System_V_Registers *gpr = &registers->general;
      assert (gpr->index + classification->eightbyte_count <= gpr->count);
      if (classification->eightbyte_count == 1) {
        Register reg = gpr->items[gpr->index++];
        storage = storage_register(reg, byte_size);
      } else if (classification->eightbyte_count == 2) {
        storage = (Storage) {
          .tag = Storage_Tag_Unpacked,
          .byte_size = byte_size,
          .Unpacked = {
            .registers = {
              gpr->items[gpr->index++],
              gpr->items[gpr->index++]
            },
          },
        };
      } else {
        panic("Unexpected eightbyte_count for an INTEGER class argument");
      }
      goto absolute;
    } break;
    case SYSTEM_V_SSE: {
      assert (registers->vector.index + classification->eightbyte_count <= registers->vector.count);
      if (classification->eightbyte_count == 1) {
        Register reg = registers->vector.items[registers->vector.index++];
        storage = storage_register(reg, byte_size);
      } else {
        panic("TODO support packed vector values");
      }
      goto absolute;
    } break;
    case SYSTEM_V_MEMORY: {
      u64 alignment = descriptor_byte_alignment(classification->descriptor);
      *stack_offset = u64_align(*stack_offset, u64_max(8, alignment));
      Memory_Layout_Item result = {
        .tag = Memory_Layout_Item_Tag_Base_Relative,
        .flags = Memory_Layout_Item_Flags_None,
        .declaration = {
          .name = name,
          .descriptor = classification->descriptor,
        },
        .Base_Relative = {.offset = *stack_offset},
      };
      *stack_offset += byte_size;
      return result;
    } break;
    case SYSTEM_V_SSEUP:
    case SYSTEM_V_X87:
    case SYSTEM_V_X87UP:
    case SYSTEM_V_COMPLEX_X87: {
      panic("TODO");
    } break;
    default: {
      panic("Unpexected SYSTEM_V class");
    } break;
  }

  absolute:
  return (Memory_Layout_Item){
    .tag = Memory_Layout_Item_Tag_Absolute,
    .declaration = {
      .name = name,
      .descriptor = classification->descriptor,
    },
    .Absolute = {.storage = storage},
  };
}

// TODO verify this implementation against GCC
// https://github.com/gcc-mirror/gcc/blob/master/gcc/config/i386/i386.c#L2080

typedef enum {
  System_V_Aggregate_Iterator_Tag_Struct,
  System_V_Aggregate_Iterator_Tag_Array,
} System_V_Aggregate_Iterator_Tag;

typedef struct {
  System_V_Aggregate_Iterator_Tag tag;
  const Descriptor *aggregate;
  const Descriptor *item;
  u64 offset;
  u64 next_index;
} System_V_Aggregate_Iterator;

static inline u64
system_v_item_iterator_count(
  const System_V_Aggregate_Iterator *it
) {
  switch(it->tag) {
    case System_V_Aggregate_Iterator_Tag_Struct: {
      assert(it->aggregate->tag == Descriptor_Tag_Struct);
      return dyn_array_length(it->aggregate->Struct.memory_layout.items);
    }
    case System_V_Aggregate_Iterator_Tag_Array: {
      assert(it->aggregate->tag == Descriptor_Tag_Fixed_Size_Array);
      return it->aggregate->Fixed_Size_Array.length;
    }
  }
  panic("UNEXPECTED System_V_Aggregate_Iterator_Tag");
  return 0;
}

static inline bool
system_v_item_iterator_next(
  System_V_Aggregate_Iterator *it
) {
  u64 item_count = system_v_item_iterator_count(it);
  if (it->next_index >= item_count) return false;
  switch(it->tag) {
    case System_V_Aggregate_Iterator_Tag_Struct: {
      assert(it->aggregate->tag == Descriptor_Tag_Struct);
      const Memory_Layout_Item *item =
        dyn_array_get(it->aggregate->Struct.memory_layout.items, it->next_index);
      it->item = item->declaration.descriptor;
      assert(item->tag == Memory_Layout_Item_Tag_Base_Relative);
      it->offset = item->Base_Relative.offset;
    } break;
    case System_V_Aggregate_Iterator_Tag_Array: {
      assert(it->aggregate->tag == Descriptor_Tag_Fixed_Size_Array);
      it->item = it->aggregate->Fixed_Size_Array.item;
      it->offset = descriptor_byte_size(it->item) * it->next_index;
    } break;
    default: {
      panic("UNEXPECTED System_V_Aggregate_Iterator_Tag");
      return false;
    }
  }
  it->next_index += 1;
  return true;
}

static System_V_Classification
x86_64_system_v_classify(
  const Allocator *allocator,
  const Descriptor *descriptor
);

static System_V_Classification
x86_64_system_v_classify_aggregate(
  const Allocator *allocator,
  System_V_Aggregate_Iterator *it
) {
  u64 byte_size = descriptor_byte_size(it->aggregate);
  u64 eightbyte = 8;

  bool has_unaligned = false; // FIXME calculate this
  // 1. If the size of an object is larger than eight eightbytes,
  // or it contains unaligned fields, it has class MEMORY
  if (byte_size > 8 * eightbyte || has_unaligned) {
    return (System_V_Classification){ .class = SYSTEM_V_MEMORY, .descriptor = it->aggregate };
  }
  // 2. If a C++ object is non-trivial for the purpose of calls, as specified in the
  // C++ ABI 13, it is passed by invisible reference (the object is replaced in the
  // parameter list by a pointer that has class INTEGER)
  bool is_c_plus_plus_non_trivial = false; // TODO allow to specify / detect this
  if (is_c_plus_plus_non_trivial) {
    const Descriptor *descriptor = descriptor_reference_to(allocator, it->aggregate);
    return (System_V_Classification){ .class = SYSTEM_V_INTEGER, .descriptor = descriptor };
  }

  // 3. If the size of the aggregate exceeds a single eightbyte, each is classified
  // separately. Each eightbyte gets initialized to class NO_CLASS.
  SYSTEM_V_ARGUMENT_CLASS eightbyte_class = SYSTEM_V_NO_CLASS;

  // TODO use a temp allocator
  Array_SYSTEM_V_ARGUMENT_CLASS eightbyte_classes = dyn_array_make(
    Array_SYSTEM_V_ARGUMENT_CLASS,
    .allocator = allocator,
    .capacity = system_v_item_iterator_count(it),
  );

  // 4. Each field of an object is classified recursively so that always two fields are
  // considered. The resulting class is calculated according to the classes of the
  // fields in the eightbyte:
  u64 last_offset = 0;
  u64 last_eightbyte_offset = 0;
  u64 struct_eightbyte_count = 0;
  while(system_v_item_iterator_next(it)) {
    u64 field_eightbyte_count = (it->offset - last_eightbyte_offset) / eightbyte;
    if (field_eightbyte_count >= 1) {
      dyn_array_push(eightbyte_classes, eightbyte_class);
      eightbyte_class = SYSTEM_V_NO_CLASS;
      last_eightbyte_offset = it->offset;
      struct_eightbyte_count += field_eightbyte_count;
    }

    // FIXME this algorithm is designed to operate on C-style aggregates
    //       which unlike the Memory_Layout can not specify arbitrary offsets
    //       and overlaps. So we might have to convert to that representation
    //       or do some really fancy tracking of overlaps (unions).
    if (it->offset < last_offset) {
      panic("FIXME support unions");
    } else {
      last_offset = it->offset + descriptor_byte_size(it->item);
    }

    System_V_Classification field_classification =
      x86_64_system_v_classify(allocator, it->item);
    SYSTEM_V_ARGUMENT_CLASS field_class = field_classification.class;
    // 4(a) If both classes are equal, this is the resulting class.
    if (eightbyte_class == field_class) {
      eightbyte_class = field_class;
    } else
    // 4(b) If one of the classes is NO_CLASS, the resulting class is the other class.
    if (field_class == SYSTEM_V_NO_CLASS) {
      eightbyte_class = eightbyte_class;
    } else if (eightbyte_class == SYSTEM_V_NO_CLASS) {
      eightbyte_class = field_class;
    } else
    // 4(c) If one of the classes is MEMORY, the result is the MEMORY class.
    if (field_class == SYSTEM_V_MEMORY) {
      eightbyte_class = SYSTEM_V_MEMORY;
    } else
    // 4(d) If one of the classes is INTEGER, the result is the INTEGER class.
    if (eightbyte_class == SYSTEM_V_INTEGER || field_class == SYSTEM_V_INTEGER) {
      eightbyte_class = SYSTEM_V_INTEGER;
    } else
    // 4(e) If one of the classes is X87, X87UP, COMPLEX_X87 class, MEMORY is used as class.
    if (
      eightbyte_class == SYSTEM_V_X87 ||
      eightbyte_class == SYSTEM_V_X87UP ||
      eightbyte_class == SYSTEM_V_COMPLEX_X87 ||
      field_class == SYSTEM_V_X87 ||
      field_class == SYSTEM_V_X87UP ||
      field_class == SYSTEM_V_COMPLEX_X87
    ) {
      eightbyte_class = SYSTEM_V_MEMORY;
    }
    // 4(f) Otherwise class SSE is used.
    else {
      eightbyte_class = SYSTEM_V_SSE;
    }
  }
  if (eightbyte_class != SYSTEM_V_NO_CLASS) {
    dyn_array_push(eightbyte_classes, eightbyte_class);
    eightbyte_class = SYSTEM_V_NO_CLASS;
    struct_eightbyte_count += 1;
  }

  SYSTEM_V_ARGUMENT_CLASS struct_class = SYSTEM_V_NO_CLASS;

  // 5. Then a post merger cleanup is done:
  DYN_ARRAY_FOREACH(SYSTEM_V_ARGUMENT_CLASS, class, eightbyte_classes) {
    // 5(a) If one of the classes is MEMORY, the whole argument is passed in memory.
    if (*class == SYSTEM_V_MEMORY) {
      struct_class = SYSTEM_V_MEMORY;
      break;
    }
    // 5(b) If X87UP is not preceded by X87, the whole argument is passed in memory.
    if (0) {
      // TODO
    }
    // 5(c) If the size of the aggregate exceeds two eightbytes and the first eightbyte
    // isn't SSE or any other eightbyte isn’t SSEUP, the whole argument is passed in memory.
    bool is_first = class == dyn_array_get(eightbyte_classes, 0);
    if (byte_size > 2 * eightbyte) {
      if (is_first) {
        if (*class != SYSTEM_V_SSE) {
          struct_class = SYSTEM_V_MEMORY;
          break;
        }
      } else {
        if (*class != SYSTEM_V_SSEUP) {
          struct_class = SYSTEM_V_MEMORY;
          break;
        }
      }
    }
    // 5(d) If SSEUP is not preceded by SSE or SSEUP, it is converted to SSE.
    if (*class == SYSTEM_V_SSEUP) {
      SYSTEM_V_ARGUMENT_CLASS previous = is_first ? SYSTEM_V_NO_CLASS : *(class - 1);
      if (previous != SYSTEM_V_SSE && previous != SYSTEM_V_SSEUP) {
        *class = SYSTEM_V_SSE;
      }
    }
  }
  if (struct_class == SYSTEM_V_NO_CLASS) {
    struct_class = *dyn_array_get(eightbyte_classes, 0);
  }

  dyn_array_destroy(eightbyte_classes);

  System_V_Classification classification = {
    .descriptor = it->aggregate,
    .eightbyte_count = struct_eightbyte_count,
    .class = struct_class,
  };
  return classification;
}

static System_V_Classification
x86_64_system_v_classify(
  const Allocator *allocator,
  const Descriptor *descriptor
) {
  u64 byte_size = descriptor_byte_size(descriptor);
  if (byte_size == 0) {
    return (System_V_Classification){ .class = SYSTEM_V_NO_CLASS, .descriptor = descriptor };
  }
  u64 eightbyte = 8;
  switch(descriptor->tag) {
    case Descriptor_Tag_Function_Instance:
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Reference_To:
    case Descriptor_Tag_Opaque: {
      if (byte_size <= eightbyte) {
        SYSTEM_V_ARGUMENT_CLASS class =
          descriptor_is_float(descriptor) ? SYSTEM_V_SSE : SYSTEM_V_INTEGER;
        return (System_V_Classification){
          .class = class,
          .descriptor = descriptor,
          .eightbyte_count = 1,
        };
      } else {
        return (System_V_Classification){ .class = SYSTEM_V_MEMORY, .descriptor = descriptor };
      }
      break;
    }
    case Descriptor_Tag_Struct: {
      System_V_Aggregate_Iterator it = {
        .tag = System_V_Aggregate_Iterator_Tag_Struct,
        .aggregate = descriptor,
      };
      return x86_64_system_v_classify_aggregate(allocator, &it);
    }
    case Descriptor_Tag_Fixed_Size_Array: {
      System_V_Aggregate_Iterator it = {
        .tag = System_V_Aggregate_Iterator_Tag_Array,
        .aggregate = descriptor,
      };
      return x86_64_system_v_classify_aggregate(allocator, &it);
    }
  }
  panic("Unexpected descriptor tag");
  return (System_V_Classification){0};
}

static Function_Call_Setup
calling_convention_x86_64_system_v_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  Function_Call_Setup result = {
    .calling_convention = &calling_convention_x86_64_system_v,
  };
  if (function->returns.declaration.descriptor == &descriptor_void) {
    result.callee_return_value = &void_value;
    result.caller_return_value = &void_value;
  } else {
    static const Register general_registers[] = { Register_A, Register_D };
    static const Register vector_registers[] = { Register_Xmm0, Register_Xmm1 };

    System_V_Register_State registers = {
      .general = {
        .items = general_registers,
        .count = countof(general_registers),
        .index = 0,
      },
      .vector = {
        .items = vector_registers,
        .count = countof(vector_registers),
        .index = 0,
      },
    };

    System_V_Classification classification =
      x86_64_system_v_classify(allocator_default, function->returns.declaration.descriptor);
    x86_64_system_v_adjust_classification_if_no_register_available(&registers, &classification);
    if (classification.class == SYSTEM_V_MEMORY) {
      result.flags |= Function_Call_Setup_Flags_Indirect_Return;
      result.caller_return_value = value_init(
        allocator_allocate(allocator, Value),
        function->returns.declaration.descriptor,
        storage_indirect(descriptor_byte_size(function->returns.declaration.descriptor), Register_A),
        function->returns.declaration.source_range
      );
      result.callee_return_value = value_init(
        allocator_allocate(allocator, Value),
        function->returns.declaration.descriptor,
        storage_indirect(descriptor_byte_size(function->returns.declaration.descriptor), Register_DI),
        function->returns.declaration.source_range
      );
    } else {
      u64 stack_offset = 0;
      Memory_Layout_Item item = x86_64_system_v_memory_layout_item_for_classification(
        &registers, &classification, function->returns.declaration.name, &stack_offset
      );
      assert(item.tag == Memory_Layout_Item_Tag_Absolute);

      Value *common_return_value = value_init(
        allocator_allocate(allocator, Value),
        item.declaration.descriptor,
        item.Absolute.storage,
        function->returns.declaration.source_range
      );
      result.callee_return_value = common_return_value;
      result.caller_return_value = common_return_value;
    }
  }

  static const Register general_registers[] = {
    Register_DI, Register_SI, Register_D, Register_C, Register_R8, Register_R9
  };
  static const Register vector_registers[] = {
    Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3,
    Register_Xmm4, Register_Xmm5, Register_Xmm6, Register_Xmm7,
  };

  System_V_Register_State registers = {
    .general = {
      .items = general_registers,
      .count = countof(general_registers),
      .index = (result.flags & Function_Call_Setup_Flags_Indirect_Return) ? 1 : 0,
    },
    .vector = {
      .items = vector_registers,
      .count = countof(vector_registers),
      .index = 0,
    },
  };

  result.arguments_layout = (Memory_Layout){
    .items = dyn_array_make(
      Array_Memory_Layout_Item,
      .allocator = allocator,
      .capacity = dyn_array_length(function->parameters) + 1,
    ),
  };

  u64 stack_offset = 0;
  DYN_ARRAY_FOREACH(Function_Parameter, param, function->parameters) {
    if (param->tag == Function_Parameter_Tag_Exact_Static) continue;
    System_V_Classification classification =
      x86_64_system_v_classify(allocator_default, param->declaration.descriptor);
    x86_64_system_v_adjust_classification_if_no_register_available(&registers, &classification);

    Memory_Layout_Item struct_item = x86_64_system_v_memory_layout_item_for_classification(
      &registers, &classification, param->declaration.name, &stack_offset
    );

    dyn_array_push(result.arguments_layout.items, struct_item);
  }
  result.parameters_stack_size = u64_to_u32(u64_align(stack_offset, 8));

  if (result.flags & Function_Call_Setup_Flags_Indirect_Return) {
    const Descriptor *reference = descriptor_reference_to(allocator, function->returns.declaration.descriptor);
    dyn_array_push(result.arguments_layout.items, (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Absolute,
      .flags = Memory_Layout_Item_Flags_Uninitialized,
      .declaration = {
        .name = {0}, // Defining return value name happens separately
        .descriptor = reference,
        .source_range = function->returns.declaration.source_range,
      },
      .Absolute = { .storage = storage_register_for_descriptor(Register_DI, reference), },
    });
  }

  return result;
}

static Function_Call_Setup
calling_convention_x86_64_windows_call_setup_proc(
  const Allocator *allocator,
  const Function_Info *function
) {
  Function_Call_Setup result = {
    .calling_convention = &calling_convention_x86_64_windows,
  };
  if (function->returns.declaration.descriptor == &descriptor_void) {
    result.callee_return_value = &void_value;
    result.caller_return_value = &void_value;
  } else {
    if (descriptor_is_float(function->returns.declaration.descriptor)) {
      Value *common_return_value = value_init(
        allocator_allocate(allocator, Value),
        function->returns.declaration.descriptor,
        storage_register_for_descriptor(Register_Xmm0, function->returns.declaration.descriptor),
        function->returns.declaration.source_range
      );
      result.callee_return_value = common_return_value;
      result.caller_return_value = common_return_value;
    } else {
      if (descriptor_byte_size(function->returns.declaration.descriptor) > 8) {
        result.flags |= Function_Call_Setup_Flags_Indirect_Return;
        const Descriptor *reference =
          descriptor_reference_to(allocator, function->returns.declaration.descriptor);

        result.caller_return_value = value_init(
          allocator_allocate(allocator, Value),
          reference,
          storage_register_for_descriptor(Register_A, reference),
          function->returns.declaration.source_range
        );
        result.callee_return_value = value_init(
          allocator_allocate(allocator, Value),
          reference,
          storage_register_for_descriptor(Register_C, reference),
          function->returns.declaration.source_range
        );
      } else {
        Value *common_return_value = value_init(
          allocator_allocate(allocator, Value),
          function->returns.declaration.descriptor,
          storage_register_for_descriptor(Register_A, function->returns.declaration.descriptor),
          function->returns.declaration.source_range
        );
        result.callee_return_value = common_return_value;
        result.caller_return_value = common_return_value;
      }
    }
  }

  static const Register general_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
  static const Register float_registers[] = {Register_Xmm0, Register_Xmm1, Register_Xmm2, Register_Xmm3};
  assert(countof(general_registers) == countof(float_registers));

  result.arguments_layout = (Memory_Layout){
    .items = dyn_array_make(
      Array_Memory_Layout_Item,
      .allocator = allocator,
      .capacity = dyn_array_length(function->parameters) + 1,
    ),
  };

  u64 index = (result.flags & Function_Call_Setup_Flags_Indirect_Return) ? 1 : 0;

  DYN_ARRAY_FOREACH(Function_Parameter, param, function->parameters) {
    if (param->tag == Function_Parameter_Tag_Exact_Static) continue;
    Memory_Layout_Item item = {
      .flags = Memory_Layout_Item_Flags_None,
      .declaration = param->declaration,
    };

    u64 byte_size = descriptor_byte_size(item.declaration.descriptor);
    bool is_large_argument = byte_size > 8;
    Storage arg_storage;
    if (is_large_argument) {
      item.declaration.descriptor = descriptor_reference_to(allocator, item.declaration.descriptor);
    }
    if (index < countof(general_registers)) {
      Register reg = descriptor_is_float(item.declaration.descriptor)
        ? float_registers[index]
        : general_registers[index];
      arg_storage = storage_register_for_descriptor(reg, item.declaration.descriptor);
      item.tag = Memory_Layout_Item_Tag_Absolute;
      item.Absolute.storage = arg_storage;
    } else {
      item.tag = Memory_Layout_Item_Tag_Base_Relative;
      item.Base_Relative.offset = index * 8;
    }
    dyn_array_push(result.arguments_layout.items, item);
    index += 1;
  }

  if (result.flags & Function_Call_Setup_Flags_Indirect_Return) {
    const Descriptor *return_descriptor =
      descriptor_reference_to(allocator, function->returns.declaration.descriptor);
    dyn_array_push(result.arguments_layout.items, (Memory_Layout_Item) {
      .tag = Memory_Layout_Item_Tag_Absolute,
      .flags = Memory_Layout_Item_Flags_Uninitialized,
      .declaration = {
        .name = {0}, // Defining return value name happens separately
        .descriptor = return_descriptor,
        .source_range = function->returns.declaration.source_range,
      },
      .Absolute = { .storage = storage_register_for_descriptor(Register_C, return_descriptor), },
    });
  }

  // In this calling convention a home area for at least 4 arguments is always reserved
  result.parameters_stack_size = u64_to_u32(u64_max(4, dyn_array_length(function->parameters)) * 8);

  return result;
}

#endif

