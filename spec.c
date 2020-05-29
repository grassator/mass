#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"


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
    .descriptor = *descriptor,
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
  u32 a_size = descriptor_byte_size(&a->descriptor);
  u32 b_size = descriptor_byte_size(&b->descriptor);

  // TODO deal with imm64
  if (a_size != b_size) {
    if (!(
      b->operand.type == Operand_Type_Immediate_32 &&
      a_size == 8
    )) {
      assert(!"Mismatched operand size when moving");
    }
  }

  if (
    b->operand.type == Operand_Type_Immediate_64 &&
    a->operand.type != Operand_Type_Register
  ) {
    // TODO Can be a problem if RAX is already used as temp
    encode(fn, (Instruction) {mov, {rax, b->operand, 0}});
    encode(fn, (Instruction) {mov, {a->operand, rax}});
  } else {
    encode(fn, (Instruction) {mov, {a->operand, b->operand, 0}});
  }
}

void
assert_not_register_ax(
  Value *value
) {
  assert(value);
  if (value->operand.type == Operand_Type_Register) {
    assert(value->operand.reg != Register_A);
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
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  Value *reg_a = value_register_for_descriptor(Register_A, &a->descriptor);
  move_value(builder, reg_a, a);

  switch(operation) {
    case Arithmetic_Operation_Plus: {
      encode(builder, (Instruction) {add, {reg_a->operand, b->operand, 0}});
      break;
    }
    case Arithmetic_Operation_Minus: {
      encode(builder, (Instruction) {sub, {reg_a->operand, b->operand, 0}});
      break;
    }
    default: {
      assert(!"Unknown arithmetic operation");
    }
  }

  Value *temp = reserve_stack(builder, &a->descriptor);
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
  // TODO type check values
  assert_not_register_ax(x);
  assert_not_register_ax(y);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value *y_temp = reserve_stack(builder, &y->descriptor);

  Value *reg_a = value_register_for_descriptor(Register_A, &y->descriptor);
  move_value(builder, reg_a, y);
  move_value(builder, y_temp, reg_a);

  reg_a = value_register_for_descriptor(Register_A, &x->descriptor);
  move_value(builder, reg_a, x);

  // TODO check operand sizes
  encode(builder, (Instruction) {imul, {reg_a->operand, y_temp->operand}});

  Value *temp = reserve_stack(builder, &x->descriptor);
  move_value(builder, temp, reg_a);

  return temp;
}

Value *
divide(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // Save RDX as it will be used for the remainder
  Value *rdx_temp = reserve_stack(builder, &descriptor_s64);

  Value *reg_rdx = value_register_for_descriptor(Register_A, &descriptor_s64);
  move_value(builder, rdx_temp, reg_rdx);

  Value *reg_a = value_register_for_descriptor(Register_A, &a->descriptor);
  move_value(builder, reg_a, a);

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, &b->descriptor);
  move_value(builder, divisor, b);

  switch (descriptor_byte_size(&a->descriptor)) {
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

  Value *temp = reserve_stack(builder, &a->descriptor);
  move_value(builder, temp, reg_a);

  // Restore RDX
  move_value(builder, reg_rdx, rdx_temp);

  return temp;
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
fn_begin() {
  Function_Builder fn = {
    .stack_reserve = 0,
    .return_patch_list = 0,
    .buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE),
    .descriptor = (const Descriptor_Function) {0}
  };

  // @Volatile @ArgumentCount
  fn.descriptor.argument_list = malloc(sizeof(Value) * 16);
  fn.descriptor.returns = malloc(sizeof(Value));

  // @Volatile @ReserveStack
  encode(&fn, (Instruction) {sub, {rsp, imm32(0xcccccccc), 0}});
  return fn;
}

Value *
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

  builder->descriptor.argument_count = builder->next_argument_index;
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = {
      .type = Descriptor_Type_Function,
      .function = builder->descriptor,
    },
    .operand = imm64((s64) builder->buffer.memory)
  };
  return result;
}

Value *
fn_arg(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size <= 8);
  s32 argument_index = fn->next_argument_index;
  fn->next_argument_index++;
  switch (argument_index) {
    case 0: {
      fn->descriptor.argument_list[0] = *value_register_for_descriptor(Register_C, descriptor);
      break;
    }
    case 1: {
      fn->descriptor.argument_list[1] = *value_register_for_descriptor(Register_D, descriptor);
      break;
    }
    case 2: {
      fn->descriptor.argument_list[2] = *value_register_for_descriptor(Register_R8, descriptor);
      break;
    }
    case 3: {
      fn->descriptor.argument_list[3] = *value_register_for_descriptor(Register_R9, descriptor);
      break;
    }
    default: {
      // @Volatile @StackPatch
      s32 offset = argument_index * 8;
      Operand operand = stack(offset, byte_size);

      fn->descriptor.argument_list[argument_index] = (const Value) {
        .descriptor = *descriptor,
        .operand = operand,
      };
      break;
    }
  }
  return &fn->descriptor.argument_list[argument_index];
}

Value *
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  // FIXME check that all return paths return the same type
  if (to_return->descriptor.type != Descriptor_Type_Void) {
    Value *reg_a = value_register_for_descriptor(Register_A, &to_return->descriptor);
    move_value(builder, reg_a, to_return);
    *builder->descriptor.returns = *reg_a;
  } else {
    *builder->descriptor.returns = void_value;
  }
  builder->return_patch_list = make_jump_patch(builder, builder->return_patch_list);

  return to_return;
}

// TODO create variadic macro for call_function_value
Value *
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  Value *argument_list,
  s64 argument_count
) {
  assert(to_call->descriptor.type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor.function;
  assert(descriptor->argument_count == argument_count);

  for (s64 i = 0; i < argument_count; ++i) {
    // FIXME add proper type checks for arguments
    assert(descriptor->argument_list[i].descriptor.type == argument_list[i].descriptor.type);
    move_value(builder, &descriptor->argument_list[i], &argument_list[i]);
  }

  // If we call a function, then we need to reserve space for the home
  // area of at least 4 arguments?
  u32 parameters_stack_size = (u32)max(4, argument_count) * 8;
  builder->max_call_parameters_stack_size = max(
    builder->max_call_parameters_stack_size,
    parameters_stack_size
  );

  Value *reg_a = value_register_for_descriptor(Register_A, &to_call->descriptor);
  move_value(builder, reg_a, to_call);

  encode(builder, (Instruction) {call, {reg_a->operand, 0, 0}});

  return descriptor->returns;
}

#define Function(_id_) \
  Value *_id_ = 0; \
  for (Function_Builder builder_ = fn_begin(); !(_id_) ; _id_ = fn_end(&builder_))

#define Return(_value_) \
  fn_return(&builder_, _value_)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(&builder_, (_descriptor_))

#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = reserve_stack(&builder_, (_descriptor_)); \
  move_value(&builder_, (_id_), (_value_))

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
  encode(builder, (Instruction) {cmp, {value->operand, imm32(0), 0}});

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
  // TODO typechecking
  encode(builder, (Instruction) {cmp, {a->operand, b->operand, 0}});
  // TODO use xor
  Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s64);
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

spec("mass") {
  before() {
    temp_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  }

  before_each() {
    buffer_reset(&temp_buffer);
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

      Stack_s32(one, value_from_s32(1));
      Stack_s32(two, value_from_s32(2));

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
      Arg(fn, &the_answer->descriptor);
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
    Value explicit_void_arg = c_function_value("void fn_void(void)", 0);
    check(explicit_void_arg.descriptor.function.argument_count == 0);
  }

  it("should say 'Hello, world!'") {
    const char *message = "Hello, world!";
    Descriptor message_descriptor = {
      .type = Descriptor_Type_Fixed_Size_Array,
      .array = {
        .item = &descriptor_s8,
        .length = strlen(message + 1/* null terminator */),
      },
    };

    Value message_value = {
      .descriptor = {
        .type = Descriptor_Type_Pointer,
        .pointer_to = &message_descriptor,
      },
      .operand = imm64((s64) message),
    };

    Value puts_value = c_function_value("int puts(const char*)", (fn_type_opaque) puts);

    Function(hello) {
      call_function_value(&builder_, &puts_value, &message_value, 1);
    }

    value_as_function(hello, fn_type_void_to_void)();
  }


  it("should support structs") {
    // struct Size { s8 width; s32 height; };

    Struct_Builder struct_builder = struct_begin();

    Descriptor_Struct_Field *width_field = struct_add_field(&struct_builder, &descriptor_s32);
    Descriptor_Struct_Field *height_field = struct_add_field(&struct_builder, &descriptor_s64);
    struct_add_field(&struct_builder, &descriptor_s32);

    Descriptor *size_struct_descriptor = struct_end(&struct_builder);

    Descriptor *size_struct_pointer_descriptor = descriptor_pointer_to(size_struct_descriptor);

    Function(area) {
      Arg(size_struct, size_struct_pointer_descriptor);
      // TODO deal with temporaries here instead of hardcoding RCX
      encode(&builder_, (Instruction) {mov, {rcx, size_struct->operand, 0}});

      Value width_value = {
        .descriptor = *width_field->descriptor,
        .operand = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = descriptor_byte_size(width_field->descriptor),
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rcx.reg,
            .displacement = width_field->offset,
          }
        }
      };

      Value height_value = {
        .descriptor = *height_field->descriptor,
        .operand = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = descriptor_byte_size(height_field->descriptor),
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rcx.reg,
            .displacement = height_field->offset,
          }
        }
      };

      Return(Multiply(&width_value, &height_value));
    }

    struct { s32 width; s64 height; s32 dummy; } size = { 10, 42 };
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
      Stack_s32(index, value_from_s32(0));
      Stack(temp, &array_pointer_descriptor, arr);

      u32 item_byte_size = descriptor_byte_size(array_pointer_descriptor.pointer_to->array.item);
      Loop {
        // TODO check that the descriptor in indeed an array
        s32 length = (s32)array_pointer_descriptor.pointer_to->array.length;
        If(Greater(index, value_from_s32(length))) {
          Break;
        }

        Value *reg_a = value_register_for_descriptor(Register_A, &temp->descriptor);
        move_value(&builder_, reg_a, temp);

        Operand pointer = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = item_byte_size,
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rax.reg,
            .displacement = 0,
          }
        };
        encode(&builder_, (Instruction) {inc, {pointer, 0, 0}});
        encode(&builder_, (Instruction) {add, {temp->operand, imm32(item_byte_size), 0}});

        encode(&builder_, (Instruction) {inc, {index->operand, 0, 0}});
      }

    }
    value_as_function(increment, fn_type_s32p_to_void)(array);

    check(array[0] == 2);
    check(array[1] == 3);
    check(array[2] == 4);
  }
}
