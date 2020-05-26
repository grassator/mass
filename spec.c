#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"

typedef struct {
  s32 *location;
  u64 ip;
} Patch_32;

typedef struct Jump_Patch_List {
  Patch_32 patch;
  struct Jump_Patch_List *next;
} Jump_Patch_List;

typedef struct {
  s32 stack_reserve;
  u8 next_argument_index;
  Buffer buffer;

  Jump_Patch_List *return_patch_list;

  Descriptor_Function descriptor;
} Function_Builder;

Value *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
) {
  u32 byte_size = descriptor_byte_size(descriptor);
  Operand operand = stack(fn->stack_reserve, byte_size);
  fn->stack_reserve += byte_size;
  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = *descriptor,
    .operand = operand,
  };
  return result;
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

Value *
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
  Value *temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp->operand, rax, 0}});

  return temp;
}

Value *
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
  Value *temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp->operand, rax, 0}});

  return temp;
}

Value *
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
  Value *b_temp = reserve_stack(builder, &b->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {rax, b->operand, 0}});
  encode(&builder->buffer, (Instruction) {mov, {b_temp->operand, rax, 0}});

  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});
  encode(&builder->buffer, (Instruction) {imul, {rax, b_temp->operand}});

  // TODO correctly size the temporary value
  Value *temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp->operand, rax, 0}});

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
  encode(&builder->buffer, (Instruction) {mov, {rdx_temp->operand, rdx, 0}});

  // TODO make a move version that checks size of operands
  encode(&builder->buffer, (Instruction) {mov, {rax, a->operand, 0}});

  // TODO deal with signed / unsigned
  Value *divisor = reserve_stack(builder, &b->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {divisor->operand, b->operand, 0}});

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
  encode(&builder->buffer, (Instruction) {idiv, {divisor->operand, 0, 0}});

  // TODO correctly size the temporary value
  Value *temp = reserve_stack(builder, &a->descriptor);
  encode(&builder->buffer, (Instruction) {mov, {temp->operand, rax, 0}});

  // Restore RDX
  encode(&builder->buffer, (Instruction) {mov, {rdx, rdx_temp->operand, 0}});

  return temp;
}

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
  fn.descriptor.argument_list = malloc(sizeof(Value) * 4);
  fn.descriptor.returns = malloc(sizeof(Value));

  // @Volatile @ReserveStack
  encode(&fn.buffer, (Instruction) {sub, {rsp, imm32(0xcccccccc), 0}});
  return fn;
}

Value *
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

  resolve_jump_patch_list(builder, builder->return_patch_list);

  encode(&builder->buffer, (Instruction) {add, {rsp, imm32(stack_size), 0}});
  encode(&builder->buffer, (Instruction) {ret, {0}});

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
  switch (fn->next_argument_index++) {
    case 0: {
      fn->descriptor.argument_list[0] = (const Value) {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = rcx.reg,
          .byte_size = byte_size,
        },
      };
      return &fn->descriptor.argument_list[0];
    }
    case 1: {
      fn->descriptor.argument_list[1] = (const Value) {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = rdx.reg,
          .byte_size = byte_size,
        },
      };
      return &fn->descriptor.argument_list[1];
    }
    case 2: {
      fn->descriptor.argument_list[2] = (const Value) {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = r8.reg,
          .byte_size = byte_size,
        },
      };
      return &fn->descriptor.argument_list[2];
    }
    case 3: {
      fn->descriptor.argument_list[3] = (const Value) {
        .descriptor = *descriptor,
        .operand = {
          .type = Operand_Type_Register,
          .reg = r9.reg,
          .byte_size = byte_size,
        },
      };
      return &fn->descriptor.argument_list[3];
    }
  }
  // @Volatile @ArgumentCount
  assert(!"More than 4 arguments are not supported at the moment.");
  return 0;
}

Value *
fn_return(
  Function_Builder *builder,
  Value *to_return
) {
  // FIXME check that all return paths return the same type
  *builder->descriptor.returns = *to_return;

  u32 byte_size = descriptor_byte_size(&to_return->descriptor);
  assert(byte_size <= 8);

  Operand return_operand = {
    .type = Operand_Type_Register,
    .reg = rax.reg,
    .byte_size = byte_size,
  };

  if (to_return->descriptor.type != Descriptor_Type_Void) {
    encode(&builder->buffer, (Instruction) {mov, {return_operand, to_return->operand, 0}});
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
    encode(
      &builder->buffer,
      (Instruction) {mov, {descriptor->argument_list[i].operand, argument_list[i].operand, 0}}
     );
  }

  encode(&builder->buffer, (Instruction) {mov, {rax, to_call->operand, 0}});
  encode(&builder->buffer, (Instruction) {call, {rax, 0, 0}});

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
  assign(&builder_, (_id_)->operand, (_value_)->operand)

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
  encode(&builder->buffer, (Instruction) {cmp, {value->operand, imm32(0), 0}});

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
  encode(&builder->buffer, (Instruction) {cmp, {a->operand, b->operand, 0}});
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
    case Compare_Greater: {
      encode(&builder->buffer, (Instruction) {setg, {rax, 0, 0}});
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }


  Value *result = reserve_stack(builder, &descriptor_s64);
  encode(&builder->buffer, (Instruction) {mov, {result->operand, rax, 0}});
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

  it("should create function increments s64 value passed to it") {
    Function(increment) {
      // TODO add a check that all argument are defined before stack variables
      Arg_s64(x);

      Stack_s64(one, value_from_s32(1));
      Stack_s64(two, value_from_s32(2));

      Return(Plus(x, Minus(two, one)));
    }
    s64 result = value_as_function(increment, fn_type_s64_to_s64)(42);
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

  it("should make function that multiplies by 2") {
    Function(twice) {
      Arg_s64(x);
      Return(Multiply(x, value_from_s32(2)));
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
      encode(&builder_.buffer, (Instruction) {mov, {rcx, size_struct->operand, 0}});

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
      Stack(temp, &arr->descriptor, arr);

      u32 item_byte_size = descriptor_byte_size(array_pointer_descriptor.pointer_to->array.item);
      Loop {
        // TODO check that the descriptor in indeed an array
        s32 length = (s32)array_pointer_descriptor.pointer_to->array.length;
        If(Greater(index, value_from_s32(length))) {
          Break;
        }

        encode(&builder_.buffer, (Instruction) {mov, {rax, temp->operand, 0}});

        Operand pointer = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = item_byte_size,
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rax.reg.index,
            .displacement = 0,
          }
        };
        encode(&builder_.buffer, (Instruction) {inc, {pointer, 0, 0}});
        encode(&builder_.buffer, (Instruction) {add, {temp->operand, imm32(item_byte_size), 0}});

        encode(&builder_.buffer, (Instruction) {inc, {index->operand, 0, 0}});
      }

    }
    value_as_function(increment, fn_type_s32p_to_void)(array);

    check(array[0] == 2);
    check(array[1] == 3);
    check(array[2] == 4);
  }
}
