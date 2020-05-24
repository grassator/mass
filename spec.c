#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>
#include <math.h>

#include "value.c"
#include "prelude.c"
#include "instruction.c"
#include "encoding.c"

typedef struct {
  s32 stack_reserve;
  u8 next_argument_index;
  Buffer buffer;

  Descriptor_Function descriptor;
} Function_Builder;

Value
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  Operand operand = stack(fn->stack_reserve, byte_size);
  fn->stack_reserve += byte_size;
  return (const Value) {
    .descriptor = *descriptor,
    .operand = operand,
  };
}

void
assign(
  Function_Builder *fn,
  Operand a,
  Operand b
) {
  encode(&fn->buffer, (Instruction) {mov, {a, b, 0}});
}

void
assert_not_register_ax(
  Value *value
) {
  assert(value);
  if (value->operand.type == Operand_Type_Register) {
    assert(value->operand.reg.index != rax.reg.index);
  }
}

Value
plus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});

  // TODO deal with imm64
  encode(&builder->buffer, (Instruction) {add, {rax, b->operand, 0}});

  // TODO correctly size the temporary value
  Value temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp.operand, rax, 0}});

  return temp;
}

Value
minus(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});

  // TODO deal with imm64
  encode(&builder->buffer, (Instruction) {sub, {rax, b->operand, 0}});

  // TODO correctly size the temporary value
  Value temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp.operand, rax, 0}});

  return temp;
}

Value
multiply(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // TODO deal with signed / unsigned
  // TODO support double the size of the result?
  // TODO make the move only for imm value
  Value b_temp = reserve_stack(builder, &b->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {rax, b->operand, 0}});
  encode(&builder->buffer, (Instruction) {mov, {b_temp.operand, rax, 0}});

  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});
  encode(&builder->buffer, (Instruction) {imul, {rax, b_temp.operand}});

  // TODO correctly size the temporary value
  Value temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp.operand, rax, 0}});

  return temp;
}

Value
divide(
  Function_Builder *builder,
  Value *a,
  Value *b
) {
  // TODO type check values
  assert_not_register_ax(a);
  assert_not_register_ax(b);

  // Save RDX as it will be used for the remainder
  Value rdx_temp = reserve_stack(builder, &descriptor_s64);
  encode(&builder->buffer, (Instruction) {mov, {rdx_temp.operand, rdx, 0}});

  // TODO make a move version that checks size of operands
  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});

  // TODO deal with signed / unsigned
  Value divisor = reserve_stack(builder, &b->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {divisor.operand, b->operand, 0}});

  switch (descriptor_byte_size(&a->descriptor)) {
    case 8: {
      encode(&builder->buffer, (Instruction) {cqo, {0}});
      break;
    }
    case 4: {
      encode(&builder->buffer, (Instruction) {cdq, {0}});
      break;
    }
    case 2: {
      encode(&builder->buffer, (Instruction) {cwd, {0}});
      break;
    }
    default: {
      assert(!"Unsupported byte size when dividing");
    }
  }
  encode(&builder->buffer, (Instruction) {idiv, {divisor.operand, 0, 0}});

  // TODO correctly size the temporary value
  Value temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp.operand, rax, 0}});

  // Restore RDX
  encode(&builder->buffer, (Instruction) {mov, {rdx, rdx_temp.operand, 0}});

  return temp;
}


Function_Builder
fn_begin() {
  Function_Builder fn = {
    .stack_reserve = 0x0,
    .buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE),
    .descriptor = (const Descriptor_Function) {0}
  };

  // @Volatile @ArgumentCount
  fn.descriptor.argument_list = malloc(sizeof(Value) * 4);
  fn.descriptor.returns = malloc(sizeof(Value));

  // @Volatile @ReserveStack
  encode(&fn.buffer, (Instruction) {sub, {rsp, imm32(0xcccccccc), 0}});
  return fn;
}

s32
align(
  s32 number,
  s32 alignment
) {
  return (s32)(ceil((double)number / alignment) * alignment);
}

Value
fn_end(
  Function_Builder *builder
) {
  u8 alignment = 0x8;
  s32 stack_size = align(builder->stack_reserve, 16) + alignment;

  { // Override stack reservation
    u64 save_occupied = builder->buffer.occupied;
    builder->buffer.occupied = 0;

    // @Volatile @ReserveStack
    encode(&builder->buffer, (Instruction) {sub, {rsp, imm32(stack_size), 0}});
    builder->buffer.occupied = save_occupied;
  }

  encode(&builder->buffer, (Instruction) {add, {rsp, imm32(stack_size), 0}});
  encode(&builder->buffer, (Instruction) {ret, {0}});

  builder->descriptor.argument_count = builder->next_argument_index;
  return (const Value) {
    .descriptor = {
      .type = Descriptor_Type_Function,
      .function = builder->descriptor,
    },
    .operand = imm64((s64) builder->buffer.memory)
  };
}

Value
fn_arg(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  assert(byte_size <= 8);
  switch (fn->next_argument_index++) {
    case 0: {
      Value arg = {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = rcx.reg,
          .byte_size = byte_size,
        },
      };

      fn->descriptor.argument_list[0] = arg;
      return arg;
    }
    case 1: {
      Value arg = {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = rdx.reg,
          .byte_size = byte_size,
        },
      };
      fn->descriptor.argument_list[1] = arg;
      return arg;
    }
    case 2: {
      Value arg = {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = r8.reg,
          .byte_size = byte_size,
        },
      };
      fn->descriptor.argument_list[2] = arg;
      return arg;
    }
    case 3: {
      Value arg = {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = r9.reg,
          .byte_size = byte_size,
        },
      };
      fn->descriptor.argument_list[3] = arg;
      return arg;
    }
  }
  // @Volatile @ArgumentCount
  assert(!"More than 4 arguments are not supported at the moment.");
  return (const Value){0};
}

void
fn_return(
  Function_Builder *fn,
  Value to_return
) {
  // FIXME check that all return paths return the same type
  *fn->descriptor.returns = to_return;

  u32 byte_size = descriptor_byte_size(&to_return.descriptor);
  assert(byte_size <= 8);

  Operand return_operand = {
    .type = Operand_Type_Register,
    .reg = rax.reg,
    .byte_size = byte_size,
  };

  if (to_return.descriptor.type != Descriptor_Type_Void) {
    encode(&fn->buffer, (Instruction) {mov, {return_operand, to_return.operand, 0}});
  }
}

Value
make_constant_s32(
  s32 integer
) {
  Function_Builder builder = fn_begin();
  {
    fn_return(&builder, value_from_s32(integer));
  }
  return fn_end(&builder);
}

Value
make_identity_s64() {
  Function_Builder builder = fn_begin();
  {
    Value arg0 = fn_arg(&builder, &descriptor_s64);
    fn_return(&builder, arg0);
  }
  return fn_end(&builder);
}

Value
make_increment_s64() {
  Function_Builder builder = fn_begin();
  {
    Value x = reserve_stack(&builder, &descriptor_s64);
    assign(&builder, x.operand, imm32(1));
    Value y = reserve_stack(&builder, &descriptor_s64);
    assign(&builder, y.operand, imm32(2));
    Value arg0 = fn_arg(&builder, &descriptor_s64);
    Value temp = minus(&builder, &arg0, &x);
    Value temp2 = plus(&builder, &temp, &y);
    fn_return(&builder, temp2);
  }
  return fn_end(&builder);
}

Value
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
    encode(
      &builder->buffer,
      (Instruction) {mov, {descriptor->argument_list[i].operand, argument_list[i].operand, 0}}
     );
  }

  encode(&builder->buffer, (Instruction) {mov, {rax, to_call->operand, 0}});
  encode(&builder->buffer, (Instruction) {call, {rax, 0, 0}});

  return *descriptor->returns;
}

Value
make_call_no_arg_return_s32() {
  Function_Builder builder = fn_begin();
  {
    Value *returns = malloc(sizeof(Value));
    *returns = (const Value){0};
    returns->descriptor = descriptor_s32;
    returns->operand = rax;

    Descriptor arg0_descriptor = {
      .type = Descriptor_Type_Function,
      .function = {
        .argument_list = 0,
        .argument_count = 0,
        .returns = returns,
      }
    };
    Value arg0 = fn_arg(&builder, &arg0_descriptor);

    Value result = call_function_value(&builder, &arg0, 0, 0);
    fn_return(&builder, result);
  }
  return fn_end(&builder);
}

Value
make_partial_application_s64(
  Value *original_fn,
  s64 arg
) {
  Function_Builder builder = fn_begin();
  {
    Value applied_arg0 = value_from_s64(arg);
    Value result = call_function_value(&builder, original_fn, &applied_arg0, 1);
    fn_return(&builder, result);
  }
  return fn_end(&builder);
}

typedef struct {
  s32 *location;
  u64 ip;
} Patch_32;

Patch_32
make_jz(
  Function_Builder *fn
) {
  encode(&fn->buffer, (Instruction) {jz, {imm32(0xcc), 0, 0}});
  u64 ip = fn->buffer.occupied;
  s32 *location = (s32 *)(fn->buffer.memory + fn->buffer.occupied - sizeof(s32));
  return (const Patch_32) { .location = location, .ip = ip };
}

Patch_32
make_jmp(
  Function_Builder *fn
) {
  encode(&fn->buffer, (Instruction) {jmp, {imm32(0xcc), 0, 0}});
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

Patch_32 make_if(
  Function_Builder *builder,
  Value value
) {
  encode(&builder->buffer, (Instruction) {cmp, {value.operand, imm32(0), 0}});

  return make_jz(builder);
}

#define If(_value_) \
  for (Patch_32 patch__ = make_if(&builder, _value_), *dummy__ = 0; !(dummy__++) ; patch_jump_to_here(&builder, patch__))

typedef enum {
  Compare_Equal,
  Compare_Less,
} Compare;

Value
compare(
  Function_Builder *builder,
  Compare operation,
  Value a,
  Value b
) {
  // TODO typechecking
  encode(&builder->buffer, (Instruction) {cmp, {a.operand, b.operand, 0}});
  // TODO use xor
  encode(&builder->buffer, (Instruction) {mov, {rax, imm32(0), 0}});

  switch(operation) {
    case Compare_Equal: {
      encode(&builder->buffer, (Instruction) {setz, {rax, 0, 0}});
      break;
    }
    case Compare_Less: {
      encode(&builder->buffer, (Instruction) {setl, {rax, 0, 0}});
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }


  Value result = reserve_stack(builder, &descriptor_s64);
  encode(&builder->buffer, (Instruction) {mov, {result.operand, rax, 0}});
  return result;
}

#define Eq(_a_, _b_) compare(&builder, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(&builder, Compare_Less, (_a_), (_b_))

Value
make_is_non_zero() {
  Function_Builder builder = fn_begin();
  {
    Patch_32 return_patch = {0};
    Value arg0 = fn_arg(&builder, &descriptor_s32);

    If(Eq(arg0, value_from_s32(0))) {
      fn_return(&builder, value_from_s32(0));
      return_patch = make_jmp(&builder);
    }

    fn_return(&builder, value_from_s32(1));
    patch_jump_to_here(&builder, return_patch);
  }
  return fn_end(&builder);
}

spec("mass") {
  before() {
    temp_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  }

  before_each() {
    buffer_reset(&temp_buffer);
  }

  it("should create function that will return 42") {
    Value the_answer = make_constant_s32(42);
    s32 result = value_as_function(&the_answer, fn_type_void_to_s32)();
    check(result == 42);
  }

  it("should create function that returns s64 value that was passed") {
    Value id_value = make_identity_s64();
    fn_type_s64_to_s64 id_s64 = (fn_type_s64_to_s64)id_value.operand.imm64;
    s64 result = id_s64(42);
    check(result == 42);
  }

  it("should create function increments s64 value passed to it") {
    Value inc_s64 = make_increment_s64();
    s64 result = value_as_function(&inc_s64, fn_type_s64_to_s64)(42);
    check(result == 43);
  }

  it("should create a function to call a no argument fn") {
    Value the_answer = make_constant_s32(42);
    Value caller= make_call_no_arg_return_s32();
    s32 result = value_as_function(&caller, fn_type__void_to_s32__to_s32)(
      value_as_function(&the_answer, fn_type_void_to_s32)
    );
    check(result == 42);
  }

  it("should create a partially applied function") {
    Value id_value = make_identity_s64();
    Value partial_fn_value = make_partial_application_s64(&id_value, 42);
    fn_type_void_to_s64 the_answer = value_as_function(&partial_fn_value, fn_type_void_to_s64);
    s64 result = the_answer();
    check(result == 42);
  }

  it("should have a function that returns 0 if arg is zero, 1 otherwise") {
    Value is_non_zero_value = make_is_non_zero();
    fn_type_s32_to_s32 is_non_zero = value_as_function(&is_non_zero_value, fn_type_s32_to_s32);
    s32 result = is_non_zero(0);
    check(result == 0);
    result = is_non_zero(42);
    check(result == 1);
  }

  it("should return 3rd argument") {
    Function_Builder builder = fn_begin();
    {
      (void)fn_arg(&builder, &descriptor_s64);
      (void)fn_arg(&builder, &descriptor_s64);
      Value arg2 = fn_arg(&builder, &descriptor_s64);
      fn_return(&builder, arg2);
    }
    Value value = fn_end(&builder);
    fn_type_s64_s64_s64_to_s64 third = value_as_function(&value, fn_type_s64_s64_s64_to_s64);

    s64 result = third(1, 2, 3);
    check(result == 3);
  }

  it("should make function that multiplies by 2") {
    Function_Builder builder = fn_begin();
    {
      Value arg0 = fn_arg(&builder, &descriptor_s64);
      Value two = value_from_s32(2);
      Value to_return = multiply(&builder, &arg0, &two);
      fn_return(&builder, to_return);
    }
    Value value = fn_end(&builder);
    fn_type_s64_to_s64 twice = value_as_function(&value, fn_type_s64_to_s64);

    s64 result = twice(42);
    check(result == 84);
  }

  it("should make function that divides two number") {
    Function_Builder builder = fn_begin();
    {
      Value arg0 = fn_arg(&builder, &descriptor_s32);
      Value arg1 = fn_arg(&builder, &descriptor_s32);
      Value to_return = divide(&builder, &arg0, &arg1);
      fn_return(&builder, to_return);
    }
    Value value = fn_end(&builder);
    fn_type_s32_s32_to_s32 divide_fn = value_as_function(&value, fn_type_s32_s32_to_s32);

    s32 result = divide_fn(-42, 2);
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

    Function_Builder builder = fn_begin();
    {
      call_function_value(&builder, &puts_value, &message_value, 1);
      fn_return(&builder, void_value);
    }
    Value value = fn_end(&builder);

    value_as_function(&value, fn_type_void_to_void)();
  }


  it("should support structs") {
    // struct Size { s32 width; s32 height; };
    Descriptor_Struct_Field fields[2] = {
      {
        .descriptor = &descriptor_s32,
        .offset = 0,
      },
      {
        .descriptor = &descriptor_s32,
        .offset = sizeof(s32),
      },
    };
    Descriptor_Struct_Field *width_field = &fields[0];
    Descriptor_Struct_Field *height_field = &fields[1];

    Descriptor size_struct_descriptor = {
      .type = Descriptor_Type_Struct,
      .struct_ = {
        .field_list = fields,
        .field_count = 2,
      },
    };

    Descriptor size_struct_pointer_descriptor = {
      .type = Descriptor_Type_Pointer,
      .pointer_to = &size_struct_descriptor,
    };

    Function_Builder builder = fn_begin();
    {
      Value arg0 = fn_arg(&builder, &size_struct_pointer_descriptor);

      // TODO figure out how to deal with temporary values
      encode(&builder.buffer, (Instruction) {mov, {rcx, arg0.operand, 0}});

      Value width_value = {
        .descriptor = *width_field->descriptor,
        .operand = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = descriptor_byte_size(width_field->descriptor),
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rcx.reg.index,
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
            .reg = rcx.reg.index,
            .displacement = height_field->offset,
          }
        }
      };

      fn_return(&builder, multiply(&builder, &width_value, &height_value));
    }
    Value value = fn_end(&builder);

    fn_type_voidp_to_s32 area = value_as_function(&value, fn_type_voidp_to_s32);

    struct { s32 width; s32 height; } size = { 10, 42 };
    s32 result = area(&size);
    check(result == 420);
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

    Function_Builder builder = fn_begin();
    {
      Value arg0 = fn_arg(&builder, &array_pointer_descriptor);
      Value index = reserve_stack(&builder, &descriptor_s32);
      encode(&builder.buffer, (Instruction) {mov, {index.operand, imm32(0), 0}});

      Value temp = reserve_stack(&builder, &arg0.descriptor);
      encode(&builder.buffer, (Instruction) {mov, {temp.operand, arg0.operand, 0}});

      u32 item_byte_size = descriptor_byte_size(array_pointer_descriptor.pointer_to->array.item);
      u64 loop_start_ip = builder.buffer.occupied;
      {
        encode(&builder.buffer, (Instruction) {mov, {rax, temp.operand, 0}});

        Operand pointer = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = item_byte_size,
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rax.reg.index,
            .displacement = 0,
          }
        };
        encode(&builder.buffer, (Instruction) {inc, {pointer, 0, 0}});

        // Loop body goes here
        encode(&builder.buffer, (Instruction) {add, {temp.operand, imm32(item_byte_size), 0}});
      }

      // TODO this is a do {} while loop and we should use while
      encode(&builder.buffer, (Instruction) {inc, {index.operand, 0, 0}});
      // TODO check that the descriptor in indeed an array
      s32 length = (s32)array_pointer_descriptor.pointer_to->array.length;
      If(Less(index, value_from_s32(length))) {
        patch_jump_to_ip(make_jmp(&builder), loop_start_ip);
      }
      // for (int i = 0; i < 3; ++i)

      //call_function_value(&builder, &puts_value, &message_value, 1);
      fn_return(&builder, void_value);
    }
    Value value = fn_end(&builder);
    value_as_function(&value, fn_type_s32p_to_void)(array);

    check(array[0] == 2);
    check(array[1] == 3);
    check(array[2] == 4);
  }
}
