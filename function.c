#include "prelude.h"
#include "value.h"

Value *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  fn->stack_reserve += byte_size;
  Operand operand = stack(-fn->stack_reserve, byte_size);
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = descriptor,
    .operand = operand,
  };
  return result;
}

void
move_value(
  Function_Builder *fn,
  Value *a,
  Value *b
) {
  // TODO figure out more type checking
  u32 a_size = descriptor_byte_size(a->descriptor);
  u32 b_size = descriptor_byte_size(b->descriptor);

  if (a_size != b_size) {
    if (!(
      b->operand.type == Operand_Type_Immediate_32 &&
      a_size == 8
    )) {
      assert(!"Mismatched operand size when moving");
    }
  }

  if ((
    b->operand.type == Operand_Type_Immediate_64 &&
    a->operand.type != Operand_Type_Register
  ) || (
    a->operand.type == Operand_Type_Memory_Indirect &&
    b->operand.type == Operand_Type_Memory_Indirect
  )) {
    Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
    // TODO Can be a problem if RAX is already used as temp
    encode(fn, (Instruction) {mov, {reg_a->operand, b->operand, 0}});
    encode(fn, (Instruction) {mov, {a->operand, reg_a->operand}});
  } else {
    encode(fn, (Instruction) {mov, {a->operand, b->operand, 0}});
  }
}

Patch_32
make_jz(
  Function_Builder *fn
) {
  encode(fn, (Instruction) {jz, {imm32(0xcc), 0, 0}});
  u64 ip = fn->buffer->occupied;
  s32 *location = (s32 *)(fn->buffer->memory + fn->buffer->occupied - sizeof(s32));
  return (const Patch_32) { .location = location, .ip = ip };
}

Patch_32
make_jmp(
  Function_Builder *fn
) {
  encode(fn, (Instruction) {jmp, {imm32(0xcc), 0, 0}});
  u64 ip = fn->buffer->occupied;
  s32 *location = (s32 *)(fn->buffer->memory + fn->buffer->occupied - sizeof(s32));
  return (const Patch_32) { .location = location, .ip = ip };
}

void
patch_jump_to_here(
  Function_Builder *fn,
  Patch_32 patch
) {
  *patch.location = (s32) (fn->buffer->occupied - patch.ip);
}

void
patch_jump_to_ip(
  Patch_32 patch,
  u64 ip
) {
  *patch.location = (s32) (ip - patch.ip);
}

Jump_Patch_List *
make_jump_patch(
  Function_Builder *builder,
  Jump_Patch_List *next
) {
  Jump_Patch_List *return_patch = temp_allocate(Jump_Patch_List);
  *return_patch = (const Jump_Patch_List) {
    .patch = make_jmp(builder),
    .next = next,
  };
  return return_patch;
}

void
resolve_jump_patch_list(
  Function_Builder *builder,
  Jump_Patch_List *list
) {
  Jump_Patch_List *jump_patch = list;
  while (jump_patch) {
    patch_jump_to_here(builder, jump_patch->patch);
    jump_patch = jump_patch->next;
  }
}

Function_Builder
fn_begin(Value **result, Buffer *buffer) {
  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (const Descriptor) {
    .type = Descriptor_Type_Function,
    .function = {
      .argument_list = temp_allocate_array(Value, 16),
      .argument_count = 0,
      .returns = 0,
    },
  };
  Function_Builder builder = {
    .stack_reserve = 0,
    .return_patch_list = 0,
    .buffer = buffer,
    .descriptor = descriptor,
    .result = result,
    .code = buffer->memory + buffer->occupied,
    .stack_displacements = temp_allocate_array(Stack_Patch, MAX_DISPLACEMENT_COUNT),
  };
  Value *fn_value = temp_allocate(Value);
  *fn_value = (const Value) {
    .descriptor = descriptor,
    .operand = imm64((s64) builder.code)
  };
  *result = fn_value;


  // @Volatile @ReserveStack
  encode(&builder, (Instruction) {sub, {rsp, imm32(0xcccccccc), 0}});
  return builder;
}

Descriptor *
fn_update_result(
  Function_Builder *builder
) {
  builder->descriptor->function.argument_count = builder->next_argument_index;
  return (*builder->result)->descriptor;
}

void
fn_ensure_frozen(
  Descriptor_Function *function
) {
  if(function->frozen) return;

  if (!function->returns) {
    function->returns = &void_value;
  }
  function->frozen = true;
}

void
fn_freeze(
  Function_Builder *builder
) {
  fn_ensure_frozen(&builder->descriptor->function);
}

bool
fn_is_frozen(
  Function_Builder *builder
) {
  return builder->descriptor->function.frozen;
}

void
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  s32 stack_size = align(builder->stack_reserve, 16) + alignment;

  { // Override stack reservation
    u64 save_occupied = builder->buffer->occupied;
    builder->buffer->occupied = (builder->code - builder->buffer->memory);

    // @Volatile @ReserveStack
    encode(builder, (Instruction) {sub, {rsp, imm32(stack_size), 0}});
    builder->buffer->occupied = save_occupied;
  }

  for (u64 i = 0; i < builder->stack_displacement_count; ++i) {
    Stack_Patch *patch = &builder->stack_displacements[i];
    s32 displacement = *patch->location;
    // @Volatile @StackPatch
    // Negative diplacement is used to encode local variables
    if (displacement < 0) {
      *patch->location = stack_size + displacement;
    } else
    // Positive values larger than max_call_parameters_stack_size
    if (displacement >= (s32)builder->max_call_parameters_stack_size) {
      // Return address will be pushed on the stack by the caller and we need to account for that
      s32 return_address_size = 8;
      *patch->location = stack_size + displacement + return_address_size;
    }
  }

  resolve_jump_patch_list(builder, builder->return_patch_list);

  encode(builder, (Instruction) {add, {rsp, imm32(stack_size), 0}});
  encode(builder, (Instruction) {ret, {0}});
  fn_freeze(builder);
}

Value *
fn_arg(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  assert(!fn_is_frozen(builder));
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size <= 8);
  Descriptor_Function *function = &builder->descriptor->function;
  s32 argument_index = builder->next_argument_index;
  builder->next_argument_index++;
  switch (argument_index) {
    case 0: {
      function->argument_list[0] = *value_register_for_descriptor(Register_C, descriptor);
      break;
    }
    case 1: {
      function->argument_list[1] = *value_register_for_descriptor(Register_D, descriptor);
      break;
    }
    case 2: {
      function->argument_list[2] = *value_register_for_descriptor(Register_R8, descriptor);
      break;
    }
    case 3: {
      function->argument_list[3] = *value_register_for_descriptor(Register_R9, descriptor);
      break;
    }
    default: {
      // @Volatile @StackPatch
      s32 offset = argument_index * 8;
      Operand operand = stack(offset, byte_size);

      function->argument_list[argument_index] = (const Value) {
        .descriptor = descriptor,
        .operand = operand,
      };
      break;
    }
  }
  fn_update_result(builder);
  return &function->argument_list[argument_index];
}

void
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  Descriptor_Function *function = &builder->descriptor->function;
  if (function->returns) {
    assert(same_type(function->returns->descriptor, to_return->descriptor));
  } else {
    assert(!fn_is_frozen(builder));
    if (to_return->descriptor->type != Descriptor_Type_Void) {
      function->returns = value_register_for_descriptor(Register_A, to_return->descriptor);
    } else {
      function->returns = &void_value;
    }
  }

  if (to_return->descriptor->type != Descriptor_Type_Void) {
    move_value(builder, function->returns, to_return);
  }
  builder->return_patch_list = make_jump_patch(builder, builder->return_patch_list);
  fn_update_result(builder);
}

Patch_32 make_if(
  Function_Builder *builder,
  Value *value
) {
  encode(builder, (Instruction) {cmp, {value->operand, imm32(0), 0}});

  return make_jz(builder);
}

typedef struct {
  bool done;
  u64 start_ip;
  Jump_Patch_List *jump_patch_list;
} Loop_Builder;

void
make_loop_end(
  Function_Builder *builder,
  Loop_Builder *loop
) {
  patch_jump_to_ip(make_jmp(builder), loop->start_ip);
  resolve_jump_patch_list(builder, loop->jump_patch_list);
  loop->done = true;
}

void
assert_not_register_ax(
  Value *overload
) {
  assert(overload);
  if (overload->operand.type == Operand_Type_Register) {
    assert(overload->operand.reg != Register_A);
  }
}

typedef enum {
  Arithmetic_Operation_Plus,
  Arithmetic_Operation_Minus,
} Arithmetic_Operation;

Value *
plus_or_minus(
  Arithmetic_Operation operation,
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  if (!(
    a->descriptor->type == Descriptor_Type_Pointer &&
    b->descriptor->type == Descriptor_Type_Integer &&
    b->descriptor->integer.byte_size == 8
  )) {
    assert(same_value_type(a, b));
    assert(a->descriptor->type == Descriptor_Type_Integer);
  }

  assert_not_register_ax(a);
  assert_not_register_ax(b);

  Value *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  switch(operation) {
    case Arithmetic_Operation_Plus: {
      encode(builder, (Instruction) {add, {reg_a->operand, temp_b->operand, 0}});
      break;
    }
    case Arithmetic_Operation_Minus: {
      encode(builder, (Instruction) {sub, {reg_a->operand, temp_b->operand, 0}});
      break;
    }
    default: {
      assert(!"Unknown arithmetic operation");
    }
  }

  Value *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  return temp;
}

Value *
plus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  return plus_or_minus(Arithmetic_Operation_Plus, builder, a, b);
}

Value *
minus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  return plus_or_minus(Arithmetic_Operation_Minus, builder, a, b);
}

Value *
multiply(
  Function_Builder *builder,
  Value *x,
  Value *y
) {
  assert(same_value_type(x, y));
  assert(x->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(x);
  assert_not_register_ax(y);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value *y_temp = reserve_stack(builder, y->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, y->descriptor);
  move_value(builder, reg_a, y);
  move_value(builder, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, x->descriptor);
  move_value(builder, reg_a, x);

  // TODO check operand sizes
  encode(builder, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  Value *temp = reserve_stack(builder, x->descriptor);
  move_value(builder, temp, reg_a);

  return temp;
}

Value *
divide(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  assert(same_value_type(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);

  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(builder, rdx_temp, reg_rdx);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, b->descriptor);
  move_value(builder, divisor, b);

  switch (descriptor_byte_size(a->descriptor)) {
    case 8: {
      encode(builder, (Instruction) {cqo, {0}});
      break;
    }
    case 4: {
      encode(builder, (Instruction) {cdq, {0}});
      break;
    }
    case 2: {
      encode(builder, (Instruction) {cwd, {0}});
      break;
    }
    default: {
      assert(!"Unsupported byte size when dividing");
    }
  }
  encode(builder, (Instruction) {idiv, {divisor->operand, 0, 0}});

  Value *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  // Restore RDX
  move_value(builder, reg_rdx, rdx_temp);

  return temp;
}

#define Plus(_a_, _b_) plus(&builder_, _a_, _b_)
#define Minus(_a_, _b_) minus(&builder_, _a_, _b_)
#define Multiply(_a_, _b_) multiply(&builder_, _a_, _b_)
#define Divide(_a_, _b_) divide(&builder_, _a_, _b_)

#define SizeOfDescriptor(_descriptor_) value_from_s32(descriptor_byte_size(_descriptor_))
#define SizeOf(_value_) value_byte_size(_value_)

#define ReflectDescriptor (_descriptor_) fn_reflect(&builder_, _descriptor_)

#define IfBuilder(_builder_, _value_) \
  for (\
    Patch_32 patch__ = make_if(_builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
    patch_jump_to_here(_builder_, patch__)\
  )
#define If(_value_) IfBuilder(&builder_, _value_)

#define Loop \
  for ( \
    Loop_Builder loop_builder_ = { .start_ip = builder_.buffer->occupied, .jump_patch_list = 0 }; \
    !loop_builder_.done; \
    make_loop_end(&builder_, &loop_builder_) \
  )

#define Continue patch_jump_to_ip(make_jmp(&builder_), loop_builder_.start_ip)
#define Break \
  loop_builder_.jump_patch_list = make_jump_patch(&builder_, loop_builder_.jump_patch_list)

typedef enum {
  Compare_Equal,
  Compare_Less,
  Compare_Greater,
} Compare;

Value *
compare(
  Function_Builder *builder,
  Compare operation,
  Value *a,
  Value *b
) {
  Value *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  // TODO check that types are comparable
  encode(builder, (Instruction) {cmp, {reg_a->operand, temp_b->operand, 0}});

  // TODO use xor
  reg_a = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(
    builder,
    reg_a,
    value_from_s64(0)
  );

  // TODO use correct operand size of a byte for these instructions
  switch(operation) {
    case Compare_Equal: {
      encode(builder, (Instruction) {setz, {rax, 0, 0}});
      break;
    }
    case Compare_Less: {
      encode(builder, (Instruction) {setl, {rax, 0, 0}});
      break;
    }
    case Compare_Greater: {
      encode(builder, (Instruction) {setg, {rax, 0, 0}});
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }

  Value *result = reserve_stack(builder, &descriptor_s64);
  move_value(builder, result, reg_a);
  return result;
}

#define Eq(_a_, _b_) compare(&builder_, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(&builder_, Compare_Less, (_a_), (_b_))
#define Greater(_a_, _b_) compare(&builder_, Compare_Greater, (_a_), (_b_))

Value *
value_pointer_to(
  Function_Builder *builder,
  Value *value
) {
  // TODO support register
  // TODO support immediates
  assert(
    value->operand.type == Operand_Type_Memory_Indirect ||
    value->operand.type == Operand_Type_RIP_Relative
  );
  Descriptor *result_descriptor = descriptor_pointer_to(value->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, result_descriptor);
  encode(builder, (Instruction) {lea, {reg_a->operand, value->operand, 0}});

  Value *result = reserve_stack(builder, result_descriptor);
  move_value(builder, result, reg_a);

  return result;
}


Value *
call_function_overload(
  Function_Builder *builder,
  Value *to_call,
  Value **argument_list,
  s64 argument_count
) {
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(descriptor->argument_count == argument_count);

  fn_ensure_frozen(descriptor);

  for (s64 i = 0; i < argument_count; ++i) {
    assert(same_value_type(&descriptor->argument_list[i], argument_list[i]));
    move_value(builder, &descriptor->argument_list[i], argument_list[i]);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u32 parameters_stack_size = (u32)max(4, argument_count) * 8;

  // FIXME support this for fns that accept arguments
  u32 return_size = descriptor_byte_size(descriptor->returns->descriptor);
  if (return_size > 8) {
    parameters_stack_size += return_size;
    Descriptor *return_pointer_descriptor = descriptor_pointer_to(descriptor->returns->descriptor);
    Value *reg_c =
      value_register_for_descriptor(Register_C, return_pointer_descriptor);
    encode(builder, (Instruction) {lea, {reg_c->operand, descriptor->returns->operand, 0}});
  }

  builder->max_call_parameters_stack_size = max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  );

  s64 start_address = (s64) builder->buffer->memory;
  s64 end_address = start_address + builder->buffer->capacity;
  if (
    to_call->operand.type == Operand_Type_Immediate_64 &&
    to_call->operand.imm64 >= start_address && to_call->operand.imm64 <= end_address
  ) {
    encode(builder, (Instruction) {call, {imm32(0xCCCCCCCC), 0, 0}});
    u8 *current_address = builder->buffer->memory + builder->buffer->occupied;
    s32 *offset_for_immediate = (s32 *)(current_address - sizeof(s32));

    s64 relative_offset = to_call->operand.imm64 - (s64)current_address;
    assert(relative_offset > INT_MIN);
    assert(relative_offset < INT_MAX);
    s32 rel32 = (s32)relative_offset;
    *offset_for_immediate = rel32;
  } else {
    Value *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
    move_value(builder, reg_a, to_call);
    encode(builder, (Instruction) {call, {reg_a->operand, 0, 0}});
  }

  if (return_size <= 8) {
    Value *result = reserve_stack(builder, descriptor->returns->descriptor);
    move_value(builder, result, descriptor->returns);
    return result;
  }

  return descriptor->returns;
}

Value *
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  Value **argument_list,
  s64 argument_count
) {
  assert(to_call);
  while (to_call) {
    Descriptor_Function *descriptor = &to_call->descriptor->function;
    bool match = true;
    for (s64 arg_index = 0; arg_index < argument_count; ++arg_index) {
      // FIXME @Overloads
      Value *arg = argument_list[arg_index];
      if(!same_value_type(&descriptor->argument_list[arg_index], arg)) {
        match = false;
        break;
      }
    }
    if (match) {
      return call_function_overload(builder, to_call, argument_list, argument_count);
    }
    to_call = descriptor->next_overload;
  }
  assert(!"No matching overload found");
  return 0;
}

#define Function(_id_) \
  Value *_id_ = 0; \
  for (Function_Builder builder_ = fn_begin(&_id_, &function_buffer); !fn_is_frozen(&builder_); fn_end(&builder_))

#define Return(_value_) \
  fn_return(&builder_, _value_)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(&builder_, (_descriptor_))

#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = reserve_stack(&builder_, (_descriptor_)); \
  move_value(&builder_, _id_, (_value_))

#define Stack_s32(_id_, _value_) Stack((_id_), &descriptor_s32, _value_)
#define Stack_s64(_id_, _value_) Stack((_id_), &descriptor_s64, _value_)


#define Call(_target_, ...)\
  call_function_value(\
    &builder_,\
    (_target_),\
    (Value **)((Value *[]){0, ##__VA_ARGS__}) + 1, \
    static_array_size(((Value *[]){0, ##__VA_ARGS__})) - 1 \
  )


