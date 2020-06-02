#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"

Value_Overload *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  fn->stack_reserve += byte_size;
  Operand operand = stack(-fn->stack_reserve, byte_size);
  Value_Overload *result = temp_allocate(Value_Overload);
  *result = (const Value_Overload) {
    .descriptor = descriptor,
    .operand = operand,
  };
  return result;
}

void
move_value(
  Function_Builder *fn,
  Value_Overload *a,
  Value_Overload *b
) {
  // TODO figure out more type checking
  u32 a_size = descriptor_byte_size(a->descriptor);
  u32 b_size = descriptor_byte_size(b->descriptor);

  // TODO deal with imm64
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
    Value_Overload *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
    // TODO Can be a problem if RAX is already used as temp
    encode(fn, (Instruction) {mov, {reg_a->operand, b->operand, 0}});
    encode(fn, (Instruction) {mov, {a->operand, reg_a->operand}});
  } else {
    encode(fn, (Instruction) {mov, {a->operand, b->operand, 0}});
  }
}

void
assert_not_register_ax(
  Value_Overload *overload
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
  Value_Overload *a,
  Value_Overload *b
) {
  assert(same_overload_type(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(a);
  assert_not_register_ax(b);

  Value_Overload *temp_b = reserve_stack(builder, b->descriptor);
  move_value(builder, temp_b, b);

  Value_Overload *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
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

  Value_Overload *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  return single_overload_value(temp);
}

Value *
plus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // FIXME @Overloads
  Value_Overload *a_overload = maybe_get_if_single_overload(a);
  assert(a_overload);
  Value_Overload *b_overload = maybe_get_if_single_overload(b);
  assert(b_overload);
  return plus_or_minus(Arithmetic_Operation_Plus, builder, a_overload, b_overload);
}

Value *
minus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // FIXME @Overloads
  Value_Overload *a_overload = maybe_get_if_single_overload(a);
  assert(a_overload);
  Value_Overload *b_overload = maybe_get_if_single_overload(b);
  assert(b_overload);
  return plus_or_minus(Arithmetic_Operation_Minus, builder, a_overload, b_overload);
}

Value *
multiply(
  Function_Builder *builder,
  Value *x_value,
  Value *y_value
) {
  // FIXME @Overloads
  Value_Overload *x = maybe_get_if_single_overload(x_value);
  assert(x);
  Value_Overload *y = maybe_get_if_single_overload(y_value);
  assert(y);

  assert(same_overload_type(x, y));
  assert(x->descriptor->type == Descriptor_Type_Integer);

  assert_not_register_ax(x);
  assert_not_register_ax(y);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value_Overload *y_temp = reserve_stack(builder, y->descriptor);

  Value_Overload *reg_a = value_register_for_descriptor(Register_A, y->descriptor);
  move_value(builder, reg_a, y);
  move_value(builder, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, x->descriptor);
  move_value(builder, reg_a, x);

  // TODO check operand sizes
  encode(builder, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  Value_Overload *temp = reserve_stack(builder, x->descriptor);
  move_value(builder, temp, reg_a);

  return single_overload_value(temp);
}

Value *
divide(
  Function_Builder *builder,
  Value *a_value,
  Value *b_value
) {
  // FIXME @Overloads
  Value_Overload *a = maybe_get_if_single_overload(a_value);
  assert(a);
  Value_Overload *b = maybe_get_if_single_overload(b_value);
  assert(b);

  assert(same_overload_type(a, b));
  assert(a->descriptor->type == Descriptor_Type_Integer);

  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // Save RDX as it will be used for the remainder
  Value_Overload *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value_Overload *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(builder, rdx_temp, reg_rdx);

  Value_Overload *reg_a = value_register_for_descriptor(Register_A, a->descriptor);
  move_value(builder, reg_a, a);

  // TODO deal with signed / unsigned
  Value_Overload *divisor = reserve_stack(builder, b->descriptor);
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

  Value_Overload *temp = reserve_stack(builder, a->descriptor);
  move_value(builder, temp, reg_a);

  // Restore RDX
  move_value(builder, reg_rdx, rdx_temp);

  return single_overload_value(temp);
}

Patch_32
make_jz(
  Function_Builder *fn
) {
  encode(fn, (Instruction) {jz, {imm32(0xcc), 0, 0}});
  u64 ip = fn->buffer.occupied;
  s32 *location = (s32 *)(fn->buffer.memory + fn->buffer.occupied - sizeof(s32));
  return (const Patch_32) { .location = location, .ip = ip };
}

Patch_32
make_jmp(
  Function_Builder *fn
) {
  encode(fn, (Instruction) {jmp, {imm32(0xcc), 0, 0}});
  u64 ip = fn->buffer.occupied;
  s32 *location = (s32 *)(fn->buffer.memory + fn->buffer.occupied - sizeof(s32));
  return (const Patch_32) { .location = location, .ip = ip };
}

void
patch_jump_to_here(
  Function_Builder *fn,
  Patch_32 patch
) {
  *patch.location = (s32) (fn->buffer.occupied - patch.ip);
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
fn_begin(Value **result) {

  Descriptor *descriptor = temp_allocate(Descriptor);
  *descriptor = (const Descriptor) {
    .type = Descriptor_Type_Function,
    .function = {
      .argument_list = temp_allocate_size(sizeof(Value) * 16),
      .argument_count = 0,
      .returns = 0,
    },
  };
  Function_Builder builder = {
    .stack_reserve = 0,
    .return_patch_list = 0,
    .buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE),
    .descriptor = descriptor,
    .result = result,
  };
  Value_Overload *fn_value = temp_allocate(Value_Overload);
  *fn_value = (const Value_Overload) {
    .descriptor = descriptor,
    .operand = imm64((s64) builder.buffer.memory)
  };
  Value *result_value = single_overload_value(fn_value);

  *result = result_value;


  // @Volatile @ReserveStack
  encode(&builder, (Instruction) {sub, {rsp, imm32(0xcccccccc), 0}});
  return builder;
}

Value_Overload *
fn_get_value_overload(
  Function_Builder *builder
) {
  Value *fn = (*builder->result);
  assert(fn->overload_count == 1);
  Value_Overload *overload = maybe_get_if_single_overload(fn);
  assert(overload);
  return overload;
}

Descriptor *
fn_update_result(
  Function_Builder *builder
) {
  builder->descriptor->function.argument_count = builder->next_argument_index;
  Value_Overload *overload = fn_get_value_overload(builder);
  return overload->descriptor;
}

void
fn_ensure_frozen(
  Descriptor_Function *function
) {
  if(function->frozen) return;

  if (!function->returns) {
    function->returns = &void_value_overload;
  }
  function->frozen = true;
}

void
fn_freeze(
  Function_Builder *builder
) {
  Value_Overload *overload = fn_get_value_overload(builder);
  fn_ensure_frozen(&overload->descriptor->function);
}

bool
fn_is_frozen(
  Function_Builder *builder
) {
  Value_Overload *overload = fn_get_value_overload(builder);
  return overload->descriptor->function.frozen;
}

void
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  builder->stack_reserve += builder->max_call_parameters_stack_size;
  s32 stack_size = align(builder->stack_reserve, 16) + alignment;

  { // Override stack reservation
    u64 save_occupied = builder->buffer.occupied;
    builder->buffer.occupied = 0;

    // @Volatile @ReserveStack
    encode(builder, (Instruction) {sub, {rsp, imm32(stack_size), 0}});
    builder->buffer.occupied = save_occupied;
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

      function->argument_list[argument_index] = (const Value_Overload) {
        .descriptor = descriptor,
        .operand = operand,
      };
      break;
    }
  }
  fn_update_result(builder);
  return single_overload_value(&function->argument_list[argument_index]);
}

void
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  // We can no longer modify the return value after fn has been called
  // or after builder has been committed through fn_end() call
  // FIXME
  //assert(!builder->frozen);

  // FIXME @Overloads
  Value_Overload *overload = maybe_get_if_single_overload(to_return);
  assert(overload);
  Descriptor_Function *function = &builder->descriptor->function;
  if (function->returns) {
    assert(same_type(function->returns->descriptor, overload->descriptor));
  } else {
    assert(!fn_is_frozen(builder));
    if (overload->descriptor->type != Descriptor_Type_Void) {
      function->returns = value_register_for_descriptor(Register_A, overload->descriptor);
    } else {
      function->returns = &void_value_overload;
    }
  }

  if (overload->descriptor->type != Descriptor_Type_Void) {
    move_value(builder, function->returns, overload);
  }
  builder->return_patch_list = make_jump_patch(builder, builder->return_patch_list);
  fn_update_result(builder);
}

// TODO create variadic macro for call_function_value
Value *
call_function_overload(
  Function_Builder *builder,
  Value_Overload *to_call,
  Value_Overload **argument_list,
  s64 argument_count
) {
  assert(to_call->descriptor->type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor->function;
  assert(descriptor->argument_count == argument_count);

  fn_ensure_frozen(descriptor);

  for (s64 i = 0; i < argument_count; ++i) {
    assert(same_overload_type(&descriptor->argument_list[i], argument_list[i]));
    move_value(builder, &descriptor->argument_list[i], argument_list[i]);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u32 parameters_stack_size = (u32)max(4, argument_count) * 8;
  builder->max_call_parameters_stack_size = max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  );

  Value_Overload *reg_a = value_register_for_descriptor(Register_A, to_call->descriptor);
  move_value(builder, reg_a, to_call);
  encode(builder, (Instruction) {call, {reg_a->operand, 0, 0}});

  Value_Overload *result = reserve_stack(builder, descriptor->returns->descriptor);
  move_value(builder, result, descriptor->returns);

  return single_overload_value(result);
}

Value *
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  Value *argument_list,
  s64 argument_count
) {
  Value_Overload **arg_overload_list =
    temp_allocate_size(sizeof(Value_Overload *) * argument_count);
  for (s64 overload_index = 0; overload_index < to_call->overload_count; ++overload_index) {
    Value_Overload *overload = &to_call->overload_list[overload_index];
    Descriptor_Function *descriptor = &overload->descriptor->function;
    bool match = true;
    for (s64 arg_index = 0; arg_index < argument_count; ++arg_index) {
      // FIXME @Overloads
      Value_Overload *arg_overload = maybe_get_if_single_overload(&argument_list[arg_index]);
      assert(arg_overload);
      arg_overload_list[arg_index] = arg_overload;
      if(!same_overload_type(&descriptor->argument_list[arg_index], arg_overload)) {
        match = false;
        break;
      }
    }
    if (match) {
      return call_function_overload(builder, overload, arg_overload_list, argument_count);
    }
  }
  assert(!"No matching overload found");
  return 0;
}

#define Function(_id_) \
  Value *_id_ = 0; \
  for (Function_Builder builder_ = fn_begin(&_id_); !fn_is_frozen(&builder_); fn_end(&builder_))

#define Return(_value_) \
  fn_return(&builder_, _value_)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(&builder_, (_descriptor_))

#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

//#define CONCAT_HELPER(A,B) A##B
//#define CONCAT(A,B) CONCAT_HELPER(A, B)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = single_overload_value(reserve_stack(&builder_, (_descriptor_))); \
  move_value(&builder_, maybe_get_if_single_overload(_id_), (_value_))

#define Stack_s32(_id_, _value_) Stack((_id_), &descriptor_s32, _value_)
#define Stack_s64(_id_, _value_) Stack((_id_), &descriptor_s64, _value_)

#define Plus(_a_, _b_) plus(&builder_, _a_, _b_)
#define Minus(_a_, _b_) minus(&builder_, _a_, _b_)
#define Multiply(_a_, _b_) multiply(&builder_, _a_, _b_)
#define Divide(_a_, _b_) divide(&builder_, _a_, _b_)

Patch_32 make_if(
  Function_Builder *builder,
  Value *value
) {
  Value_Overload *overload = maybe_get_if_single_overload(value);
  assert(overload);
  encode(builder, (Instruction) {cmp, {overload->operand, imm32(0), 0}});

  return make_jz(builder);
}

#define If(_value_) \
  for (\
    Patch_32 patch__ = make_if(&builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
    patch_jump_to_here(&builder_, patch__)\
  )

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

#define Loop \
  for ( \
    Loop_Builder loop_builder_ = { .start_ip = builder_.buffer.occupied, .jump_patch_list = 0 }; \
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
  assert(same_value_type(a, b));
  Value_Overload *a_overload = maybe_get_if_single_overload(a);
  assert(a_overload);
  Value_Overload *b_overload = maybe_get_if_single_overload(b);
  assert(b_overload);

  Value_Overload *temp_b = reserve_stack(builder, b_overload->descriptor);
  move_value(builder, temp_b, b_overload);

  Value_Overload *reg_a = value_register_for_descriptor(Register_A, a_overload->descriptor);
  move_value(builder, reg_a, a_overload);

  // TODO check that types are comparable
  encode(builder, (Instruction) {cmp, {reg_a->operand, temp_b->operand, 0}});

  // TODO use xor
  reg_a = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(
    builder,
    reg_a,
    maybe_get_if_single_overload(value_from_s64(0))
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

  Value_Overload *result = reserve_stack(builder, &descriptor_s64);
  move_value(builder, result, reg_a);
  return single_overload_value(result);
}

#define Eq(_a_, _b_) compare(&builder_, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(&builder_, Compare_Less, (_a_), (_b_))
#define Greater(_a_, _b_) compare(&builder_, Compare_Greater, (_a_), (_b_))

typedef struct Struct_Builder_Field {
  Descriptor_Struct_Field struct_field;
  struct Struct_Builder_Field *next;
} Struct_Builder_Field;

typedef struct {
  u32 offset;
  u32 field_count;
  Struct_Builder_Field *field_list;
} Struct_Builder;

Struct_Builder
struct_begin() {
  return (const Struct_Builder) {0};
}

Descriptor_Struct_Field *
struct_add_field(
  Struct_Builder *builder,
  Descriptor *descriptor
) {
  Struct_Builder_Field *builder_field = temp_allocate(Struct_Builder_Field);

  u32 size = descriptor_byte_size(descriptor);
  builder->offset = align(builder->offset, size);

  builder_field->struct_field.descriptor = descriptor;
  builder_field->struct_field.offset = builder->offset;

  builder_field->next = builder->field_list;
  builder->field_list = builder_field;

  // TODO alignment
  builder->offset += size;
  builder->field_count++;

  return &builder_field->struct_field;
}

Descriptor *
struct_end(
  Struct_Builder *builder
) {
  assert(builder->field_count);

  Descriptor *result = temp_allocate(Descriptor);
  Descriptor_Struct_Field *field_list = temp_allocate_size(
    sizeof(Descriptor_Struct_Field) * builder->field_count
  );

  Struct_Builder_Field *field = builder->field_list;
  u64 index = builder->field_count - 1;
  while (field) {
    field_list[index--] = field->struct_field;
    field = field->next;
  }
  result->type = Descriptor_Type_Struct;
  result->struct_ = (const Descriptor_Struct) {
    .field_list = field_list,
    .field_count = builder->field_count,
  };

  return result;
}

Value *
make_identity(
  Descriptor *type
) {
  Function(id) {
    Arg(x, type);
    Return(x);
  }
  return id;
}

Value *
make_add_two(
  Descriptor *type
) {
  Function(addtwo) {
    Arg(x, type);
    Return(Plus(x, value_from_s64(2)));
  }
  return addtwo;
}

spec("mass") {
  before() {
    temp_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  }

  before_each() {
    buffer_reset(&temp_buffer);
  }

  it("should support ad-hoc polymorphism / overloading") {
    Function(sizeof_s32) {
      Arg_s32(x);
      (void)x;
      Return(value_from_s64(4));
    }
    Function(sizeof_s64) {
      Arg_s64(x);
      (void)x;
      Return(value_from_s64(8));
    }

    Value_Overload *a = maybe_get_if_single_overload(sizeof_s32);
    Value_Overload *b = maybe_get_if_single_overload(sizeof_s64);

    Value_Overload overload_list[] = {*a, *b};

    Value overload = {
      .overload_list = overload_list,
      .overload_count = 2,
    };

    Function(checker_value) {
      Value *x = call_function_value(&builder_, &overload, value_from_s64(0), 1);
      Value *y = call_function_value(&builder_, &overload, value_from_s32(0), 1);
      Return(Plus(x, y));
    }

    fn_type_void_to_s64 checker = value_as_function(checker_value, fn_type_void_to_s64);
    check(checker() == 12);
  }

  it("should support parametric polymorphism") {
    Value *id_s64 = make_identity(&descriptor_s64);
    Value *id_s32 = make_identity(&descriptor_s32);
    Value *addtwo_s64 = make_add_two(&descriptor_s64);
    Function(check) {
      call_function_value(&builder_, id_s64, value_from_s64(0), 1);
      call_function_value(&builder_, id_s32, value_from_s32(0), 1);
      call_function_value(&builder_, addtwo_s64, value_from_s64(0), 1);
    }
  }

  it("should say that the types are the same for integers of the same size") {
    check(same_type(&descriptor_s32, &descriptor_s32));
  }

  it("should say that the types are not the same for integers of different sizes") {
    check(!same_type(&descriptor_s64, &descriptor_s32));
  }

  it("should say that pointer and a s64 are different types") {
    check(!same_type(&descriptor_s64, descriptor_pointer_to(&descriptor_s64)));
  }

  it("should say that (s64 *) is not the same as (s32 *)") {
    check(!same_type(descriptor_pointer_to(&descriptor_s32), descriptor_pointer_to(&descriptor_s64)));
  }

  it("should say that (s64[2]) is not the same as (s32[2])") {
    check(!same_type(
      descriptor_array_of(&descriptor_s32, 2),
      descriptor_array_of(&descriptor_s64, 2)
    ));
  }

  it("should say that (s64[10]) is not the same as (s64[2])") {
    check(!same_type(
      descriptor_array_of(&descriptor_s64, 10),
      descriptor_array_of(&descriptor_s64, 2)
    ));
  }

  it("should support polymorphic values") {
    Value_Overload *a = maybe_get_if_single_overload(value_from_s32(0));
    Value_Overload *b = maybe_get_if_single_overload(value_from_s64(0));

    Value_Overload overload_list[] = {*a, *b};

    Value overload = {
      .overload_list = overload_list,
      .overload_count = 2,
    };

    Value_Overload_Pair *pair = get_matching_values(&overload, value_from_s64(0));
    check(same_overload_type(pair->a, b));
  }

  it("should say that structs are different if their descriptors are different pointers") {
    Struct_Builder struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s32);
    Descriptor *a = struct_end(&struct_builder);

    struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s32);
    Descriptor *b = struct_end(&struct_builder);

    check(same_type(a, a));
    check(!same_type(a, b));
  }

  it("should say functions with the same signature have the same type") {
    Function(a) {
      Arg(arg0, &descriptor_s32);
      (void)arg0;
    }
    Function(b) {
      Arg(arg0, &descriptor_s32);
      (void)arg0;
    }
    check(same_value_type(a, b));
  }

  it("should say functions with the different signatures have the different type") {
    Function(a) {
      Arg(arg0, &descriptor_s32);
      (void)arg0;
    }
    Function(b) {
      Arg(arg0, &descriptor_s32);
      Arg(arg1, &descriptor_s32);
      (void)arg0;
      (void)arg1;
    }
    Function(c) {
      Arg(arg0, &descriptor_s64);
      (void)arg0;
    }
    Function(d) {
      Arg(arg0, &descriptor_s32);
      (void)arg0;
      Return(value_from_s32(0));
    }
    check(!same_value_type(a, b));
    check(!same_value_type(a, c));
    check(!same_value_type(a, d));
  }

  it("should create function that will return 42") {
    Function(the_answer) {
      Return(value_from_s32(42));
    }
    s32 result = value_as_function(the_answer, fn_type_void_to_s32)();
    check(result == 42);
  }

  it("should create function that returns s64 value that was passed") {
    Function(id) {
      Arg_s64(x);
      Return(x);
    }
    s64 result = value_as_function(id, fn_type_s64_to_s64)(42);
    check(result == 42);
  }

  it("should create function increments s32 value passed to it") {
    Function(increment) {
      // TODO add a check that all argument are defined before stack variables
      Arg_s32(x);

      Stack_s32(one, maybe_get_if_single_overload(value_from_s32(1)));
      Stack_s32(two, maybe_get_if_single_overload(value_from_s32(2)));

      Return(Plus(x, Minus(two, one)));
    }
    s32 result = value_as_function(increment, fn_type_s32_to_s32)(42);
    check(result == 43);
  }

  it("should create a function to call a no argument fn") {
    Function(the_answer) {
      Return(value_from_s32(42));
    }
    Function(caller) {
      Arg(fn, maybe_get_if_single_overload(the_answer)->descriptor);
      Return(call_function_value(&builder_, fn, 0, 0));
    }
    s32 result = value_as_function(caller, fn_type__void_to_s32__to_s32)(
      value_as_function(the_answer, fn_type_void_to_s32)
    );
    check(result == 42);
  }

  it("should create a partially applied function") {
    Function(id) {
      Arg_s64(x);
      Return(x);
    }
    Function(partial) {
      Return(call_function_value(&builder_, id, value_from_s64(42), 1));
    }
    fn_type_void_to_s64 the_answer = value_as_function(partial, fn_type_void_to_s64);
    s64 result = the_answer();
    check(result == 42);
  }

  it("should have a function that returns 0 if arg is zero, 1 otherwise") {
    Function(is_non_zero_value) {
      Arg_s32(x);
      If(Eq(x, value_from_s32(0))) {
        Return(value_from_s32(0));
      }
      Return(value_from_s32(1));
    }
    fn_type_s32_to_s32 is_non_zero = value_as_function(is_non_zero_value, fn_type_s32_to_s32);
    s32 result = is_non_zero(0);
    check(result == 0);
    result = is_non_zero(42);
    check(result == 1);
  }

  it("should return 3rd argument") {
    Function(third) {
      Arg_s64(arg0);
      Arg_s64(arg1);
      Arg_s64(arg2);

      (void)arg0; // unused
      (void)arg1; // unused

      Return(arg2);
    }
    s64 result = value_as_function(third, fn_type_s64_s64_s64_to_s64)(1, 2, 3);
    check(result == 3);
  }

  it("should return 6th argument") {
    Function(args) {
      Arg_s64(arg0);
      Arg_s64(arg1);
      Arg_s64(arg2);
      Arg_s64(arg3);
      Arg_s32(arg4);
      Arg_s64(arg5);

      (void)arg0; // unused
      (void)arg1; // unused
      (void)arg2; // unused
      (void)arg3; // unused
      (void)arg4; // unused

      Return(arg5);
    }
    s64 result = value_as_function(args, fn_type_s64_s64_s64_s64_s64_s64_to_s64)(1, 2, 3, 4, 5, 6);
    check(result == 6);
  }

  it("should be able to call a function with more than 4 arguments") {
    Function(args) {
      Arg_s64(arg0);
      Arg_s64(arg1);
      Arg_s64(arg2);
      Arg_s64(arg3);
      Arg_s32(arg4);
      Arg_s64(arg5);

      (void)arg0; // unused
      (void)arg1; // unused
      (void)arg2; // unused
      (void)arg3; // unused
      (void)arg4; // unused

      Return(arg5);
    }
    Function(caller) {
      Value arguments[6] = {
        *value_from_s64(10),
        *value_from_s64(20),
        *value_from_s64(30),
        *value_from_s64(40),
        *value_from_s32(50),
        *value_from_s64(60),
      };
      Return(call_function_value(&builder_, args, arguments, static_array_size(arguments)));
    }
    s64 result = value_as_function(caller, fn_type_void_to_s64)();
    check(result == 60);
  }

  it("should make function that multiplies by 2") {
    Function(twice) {
      Arg_s64(x);
      Return(Multiply(x, value_from_s64(2)));
    }

    s64 result = value_as_function(twice, fn_type_s64_to_s64)(42);
    check(result == 84);
  }

  it("should make function that divides two number") {
    Function(half) {
      Arg_s32(x);
      Arg_s32(y);
      Return(Divide(x, y));
    }

    s32 result = value_as_function(half, fn_type_s32_s32_to_s32)(-42, 2);
    check(result == -21);
  }

  it("should parse c function forward declarations") {
    c_function_value("void fn_void()", 0);
    c_function_value("void fn_int(int)", 0);
    Value *explicit_void_arg = c_function_value("void fn_void(void)", 0);
    Value_Overload *overload = maybe_get_if_single_overload(explicit_void_arg);
    check(overload);
    check(overload->descriptor->function.argument_count == 0);
  }

  it("should say 'Hello, world!'") {
    const char *message = "Hello, world!";

    Value_Overload message_overload = {
      .descriptor = descriptor_pointer_to(&descriptor_s8),
      .operand = imm64((s64) message),
    };

    Value *message_value = single_overload_value(&message_overload);

    Value *puts_value = c_function_value("int puts(const char*)", (fn_type_opaque) puts);

    Function(hello) {
      call_function_value(&builder_, puts_value, message_value, 1);
    }

    value_as_function(hello, fn_type_void_to_void)();
  }

  it("should calculate fibonacci numbers") {
    Function(fib) {
      Arg_s64(n);
      If(Eq(n, value_from_s64(0))) {
        Return(value_from_s64(0));
      }
      If(Eq(n, value_from_s64(1))) {
        Return(value_from_s64(1));
      }

      Value *minusOne = Minus(n, value_from_s64(1));
      Value *minusTwo = Minus(n, value_from_s64(2));

      Value *fMinusOne = call_function_value(&builder_, fib, minusOne, 1);
      Value *fMinusTwo = call_function_value(&builder_, fib, minusTwo, 1);

      Value *result = Plus(fMinusOne, fMinusTwo);

      Return(result);
    }

    fn_type_s64_to_s64 f = value_as_function(fib, fn_type_s64_to_s64);
    check(f(0) == 0);
    check(f(1) == 1);
    check(f(2) == 1);
    check(f(3) == 2);
    check(f(6) == 8);
  }

  it("should support structs") {
    // struct Size { s8 width; s32 height; };

    Struct_Builder struct_builder = struct_begin();

    Descriptor_Struct_Field *width_field = struct_add_field(&struct_builder, &descriptor_s32);
    Descriptor_Struct_Field *height_field = struct_add_field(&struct_builder, &descriptor_s32);
    struct_add_field(&struct_builder, &descriptor_s32);

    Descriptor *size_struct_descriptor = struct_end(&struct_builder);

    Descriptor *size_struct_pointer_descriptor = descriptor_pointer_to(size_struct_descriptor);

    Function(area) {
      Arg(size_struct, size_struct_pointer_descriptor);
      Value_Overload *size_struct_overload = maybe_get_if_single_overload(size_struct);
      assert(size_struct_overload);
      // TODO deal with temporaries here instead of hardcoding RCX
      encode(&builder_, (Instruction) {mov, {rcx, size_struct_overload->operand, 0}});

      Value_Overload width_value = {
        .descriptor = width_field->descriptor,
        .operand = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = descriptor_byte_size(width_field->descriptor),
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rcx.reg,
            .displacement = width_field->offset,
          }
        }
      };

      Value_Overload height_value = {
        .descriptor = height_field->descriptor,
        .operand = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = descriptor_byte_size(height_field->descriptor),
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rcx.reg,
            .displacement = height_field->offset,
          }
        }
      };

      Return(Multiply(
        single_overload_value(&width_value),
        single_overload_value(&height_value)
      ));
    }

    struct { s32 width; s32 height; s32 dummy; } size = { 10, 42 };
    s32 result = value_as_function(area, fn_type_voidp_to_s32)(&size);
    check(result == 420);
    check(sizeof(size) == descriptor_byte_size(size_struct_descriptor));
  }

  it("should add 1 to all numbers in an array") {
    s32 array[] = {1, 2, 3};

    Descriptor array_descriptor = {
      .type = Descriptor_Type_Fixed_Size_Array,
      .array = {
        .item = &descriptor_s32,
        .length = 3,
      },
    };

    Descriptor array_pointer_descriptor = {
      .type = Descriptor_Type_Pointer,
      .pointer_to = &array_descriptor,
    };

    Function(increment) {
      Arg(arr, &array_pointer_descriptor);

      Stack_s32(index, maybe_get_if_single_overload(value_from_s32(0)));
      Value_Overload *index_overload = maybe_get_if_single_overload(index);

      Stack(temp, &array_pointer_descriptor, maybe_get_if_single_overload(arr));

      u32 item_byte_size = descriptor_byte_size(array_pointer_descriptor.pointer_to->array.item);
      Loop {
        // TODO check that the descriptor in indeed an array
        s32 length = (s32)array_pointer_descriptor.pointer_to->array.length;
        If(Greater(index, value_from_s32(length))) {
          Break;
        }

        Value_Overload *temp_overload = maybe_get_if_single_overload(temp);
        Value_Overload *reg_a = value_register_for_descriptor(Register_A,
          temp_overload->descriptor);
        move_value(&builder_, reg_a, temp_overload);

        Operand pointer = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = item_byte_size,
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rax.reg,
            .displacement = 0,
          }
        };
        encode(&builder_, (Instruction) {inc, {pointer, 0, 0}});
        encode(&builder_, (Instruction) {add, {temp_overload->operand, imm32(item_byte_size), 0}});

        encode(&builder_, (Instruction) {inc, {index_overload->operand, 0, 0}});
      }

    }
    value_as_function(increment, fn_type_s32p_to_void)(array);

    check(array[0] == 2);
    check(array[1] == 3);
    check(array[2] == 4);
  }
}
