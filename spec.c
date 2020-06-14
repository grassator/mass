#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"

Value *
fn_reflect(
  Function_Builder *builder,
  Descriptor *descriptor
) {
  Value *result = reserve_stack(builder, &descriptor_struct_reflection);
  // FIXME support all types
  assert(descriptor->type == Descriptor_Type_Struct);
  // FIXME support generic allocation of structs on the stack
  move_value(builder, result, value_from_s32(descriptor->struct_.field_count));
  return result;
}

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
  Descriptor *descriptor,
  const char *name
) {
  Struct_Builder_Field *builder_field = temp_allocate(Struct_Builder_Field);

  u32 size = descriptor_byte_size(descriptor);
  builder->offset = align(builder->offset, size);

  builder_field->struct_field.name = name;
  builder_field->struct_field.descriptor = descriptor;
  builder_field->struct_field.offset = builder->offset;

  builder_field->next = builder->field_list;
  builder->field_list = builder_field;

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
  Descriptor_Struct_Field *field_list = temp_allocate_array(
    Descriptor_Struct_Field, builder->field_count
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
ensure_memory(
  Value *value
) {
  Operand operand = value->operand;
  if (operand.type == Operand_Type_Memory_Indirect) return value;
  Value *result = temp_allocate(Value);
  if (value->descriptor->type != Descriptor_Type_Pointer) assert(!"Not implemented");
  if (value->operand.type != Operand_Type_Register) assert(!"Not implemented");
  *result = (const Value) {
    .descriptor = value->descriptor->pointer_to,
    .operand = {
      .type = Operand_Type_Memory_Indirect,
      .indirect = {
        .reg = value->operand.reg,
        .displacement = 0,
      },
    },
  };
  return result;
}

Value *
struct_get_field(
  Value *raw_value,
  const char *name
) {
  Value *struct_value = ensure_memory(raw_value);
  Descriptor *descriptor = struct_value->descriptor;
  assert(descriptor->type == Descriptor_Type_Struct);
  for (s32 i = 0; i < descriptor->struct_.field_count; ++i) {
    Descriptor_Struct_Field *field = &descriptor->struct_.field_list[i];
    if (strcmp(field->name, name) == 0) {
      Value *result = temp_allocate(Value);
      Operand operand = struct_value->operand;
      // FIXME support more operands
      assert(operand.type == Operand_Type_Memory_Indirect);
      operand.byte_size = descriptor_byte_size(field->descriptor);
      operand.indirect.displacement += field->offset;
      *result = (const Value) {
        .descriptor = field->descriptor,
        .operand = operand,
      };
      return result;
    }
  }

  assert(!"Could not find a field with specified name");
  return 0;
}


Buffer function_buffer;

Value *
maybe_cast_to_tag(
  Function_Builder *builder,
  const char *name,
  Value *value
) {
  assert(value->descriptor->type == Descriptor_Type_Pointer);
  Descriptor *descriptor = value->descriptor->pointer_to;

  // FIXME
  assert(value->operand.type == Operand_Type_Register);
  Value *tag_value = temp_allocate(Value);
  *tag_value = (const Value) {
    .descriptor = &descriptor_s64,
    .operand = {
      .type = Operand_Type_Memory_Indirect,
      .byte_size = descriptor_byte_size(&descriptor_s64),
      .indirect = {
        .reg = value->operand.reg,
        .displacement = 0,
      },
    },
  };

  s64 count = descriptor->tagged_union.struct_count;
  for (s32 i = 0; i < count; ++i) {
    Descriptor_Struct *struct_ = &descriptor->tagged_union.struct_list[i];
    if (strcmp(struct_->name, name) == 0) {

      Descriptor *constructor_descriptor = temp_allocate(Descriptor);
      *constructor_descriptor = (const Descriptor) {
        .type = Descriptor_Type_Struct,
        .struct_ = *struct_,
      };
      Descriptor *pointer_descriptor = descriptor_pointer_to(constructor_descriptor);
      Value *result_value = temp_allocate(Value);
      *result_value = (const Value) {
        .descriptor = pointer_descriptor,
        .operand = rbx,
      };

      move_value(builder, result_value, value_from_s64(0));

      Value *comparison = compare(builder, Compare_Equal, tag_value, value_from_s64(i));
      IfBuilder(builder, comparison) {
        move_value(builder, result_value, value);
        Value *sum = plus(builder, result_value, value_from_s64(sizeof(s64)));
        move_value(builder, result_value, sum);
      }
      return result_value;
    }
  }
  assert(!"Could not find specified name in the tagged union");
  return 0;
}

typedef struct {
  int64_t x;
  int64_t y;
} Point;

Point test() {
  return (Point){42, 84};
}

fn_type_s32_to_s8
create_is_character_in_set_checker_fn(
  const char *characters
) {
  assert(characters);

  Function(checker) {
    Arg_s32(character);

    for (const char *ch = characters; *ch; ++ch) {
      If(Eq(character, value_from_s32(*ch))) {
        Return(value_from_s8(1));
      }
    }

    Return(value_from_s8(0));
  }

  return value_as_function(checker, fn_type_s32_to_s8);
}


spec("mass") {

  before() {
    temp_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
  }

  before_each() {
    function_buffer = make_buffer(128 * 1024, PAGE_EXECUTE_READWRITE);
    buffer_reset(&temp_buffer);
  }

  after_each() {
    free_buffer(&function_buffer);
  }

  it("should have a way to create a function to checks if a character is one of the provided set") {
    fn_type_s32_to_s8 is_whitespace = create_is_character_in_set_checker_fn(" \n\r\t");
    check(is_whitespace(' '));
    check(is_whitespace('\r'));
    check(is_whitespace('\n'));
    check(is_whitespace('\t'));
    check(!is_whitespace('a'));
    check(!is_whitespace('2'));
    check(!is_whitespace('-'));
  }

  it("should support returning structs larger than 64 bits on the stack") {
    Struct_Builder struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s64, "x");
    struct_add_field(&struct_builder, &descriptor_s64, "y");
    Descriptor *point_struct_descriptor = struct_end(&struct_builder);

    Value *return_overload = temp_allocate(Value);
    *return_overload = (Value) {
      .descriptor = point_struct_descriptor,
      .operand = stack(0, descriptor_byte_size(point_struct_descriptor)),
    };

    Descriptor *c_test_fn_descriptor = temp_allocate(Descriptor);
    *c_test_fn_descriptor = (Descriptor){
      .type = Descriptor_Type_Function,
      .function = {
        .argument_list = 0,
        .argument_count = 0,
        .returns = return_overload,
        .frozen = false,
      },
    };
    Value *c_test_fn_value = temp_allocate(Value);
    *c_test_fn_value = (Value) {
      .descriptor = c_test_fn_descriptor,
      .operand = imm64((s64)test),
    };

    Function(checker_value) {
      Value *test_result = Call(c_test_fn_value);
      Value *x = struct_get_field(test_result, "x");
      Return(x);
    }

    fn_type_void_to_s64 checker = value_as_function(checker_value, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should support RIP-relative addressing") {
    buffer_append_s32(&function_buffer, 42);
    Value rip = {
      .descriptor = &descriptor_s32,
      .operand = {
        .type = Operand_Type_RIP_Relative,
        .byte_size = descriptor_byte_size(&descriptor_s32),
        .imm64 = (s64) function_buffer.memory,
      },
    };

    Function(checker_value) {
      Return(&rip);
    }
    fn_type_void_to_s32 checker = value_as_function(checker_value, fn_type_void_to_s32);
    check(checker() == 42);
  }

  it("should support sizeof operator on values") {
    Value *sizeof_s32 = SizeOf(value_from_s32(0));
    check(sizeof_s32);
    check(sizeof_s32->operand.type == Operand_Type_Immediate_32);
    check(sizeof_s32->operand.imm32 == 4);
  }

  it("should support sizeof operator on descriptors") {
    Value *sizeof_s32 = SizeOfDescriptor(&descriptor_s32);
    check(sizeof_s32);
    check(sizeof_s32->operand.type == Operand_Type_Immediate_32);
    check(sizeof_s32->operand.imm32 == 4);
  }

  it("should support reflection on structs") {
    Struct_Builder struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s32, "x");
    struct_add_field(&struct_builder, &descriptor_s32, "y");
    Descriptor *point_struct_descriptor = struct_end(&struct_builder);

    Function(field_count) {
      Value *overload = fn_reflect(&builder_, point_struct_descriptor);
      Stack(struct_, &descriptor_struct_reflection, overload);
      Return(struct_get_field(struct_, "field_count"));
    }
    s32 count = value_as_function(field_count, fn_type_void_to_s32)();
    check(count == 2);
  }

  it("should support tagged unions") {
    Descriptor_Struct_Field some_fields[] = {
      {
        .name = "value",
        .descriptor = &descriptor_s64,
        .offset = 0,
      },
    };

    Descriptor_Struct constructors[] = {
      {
        .name = "None",
        .field_list = 0,
        .field_count = 0,
      },
      {
        .name = "Some",
        .field_list = some_fields,
        .field_count = static_array_size(some_fields),
      },
    };

    Descriptor option_s64_descriptor = {
      .type = Descriptor_Type_Tagged_Union,
      .tagged_union = {
        .struct_list = constructors,
        .struct_count = static_array_size(constructors),
      },
    };


    Function(with_default_value) {
      Arg(option_value, descriptor_pointer_to(&option_s64_descriptor));
      Arg_s64(default_value);
      Value *some = maybe_cast_to_tag(&builder_, "Some", option_value);
      If(some) {
        Value *value = struct_get_field(some, "value");
        Return(value);
      }
      Return(default_value);
    }

    fn_type_voidp_s64_to_s64 with_default =
      value_as_function(with_default_value, fn_type_voidp_s64_to_s64);
    struct { s64 tag; s64 maybe_value; } test_none = {0};
    struct { s64 tag; s64 maybe_value; } test_some = {1, 21};
    check(with_default(&test_none, 42) == 42);
    check(with_default(&test_some, 42) == 21);
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

  it("should say that structs are different if their descriptors are different pointers") {
    Struct_Builder struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s32, "x");
    Descriptor *a = struct_end(&struct_builder);

    struct_builder = struct_begin();
    struct_add_field(&struct_builder, &descriptor_s32, "x");
    Descriptor *b = struct_end(&struct_builder);

    check(same_type(a, a));
    check(!same_type(a, b));
  }

  it("should support structs") {
    // struct Size { s8 width; s32 height; };

    Struct_Builder struct_builder = struct_begin();

    struct_add_field(&struct_builder, &descriptor_s32, "width");
    struct_add_field(&struct_builder, &descriptor_s32, "height");
    struct_add_field(&struct_builder, &descriptor_s32, "dummy");

    Descriptor *size_struct_descriptor = struct_end(&struct_builder);

    Descriptor *size_struct_pointer_descriptor = descriptor_pointer_to(size_struct_descriptor);

    Function(area) {
      Arg(size_struct, size_struct_pointer_descriptor);
      Return(Multiply(
        struct_get_field(size_struct, "width"),
        struct_get_field(size_struct, "height")
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

      Stack_s32(index, value_from_s32(0));

      Stack(temp, &array_pointer_descriptor, arr);

      u32 item_byte_size = descriptor_byte_size(array_pointer_descriptor.pointer_to->array.item);
      Loop {
        // TODO check that the descriptor in indeed an array
        s32 length = (s32)array_pointer_descriptor.pointer_to->array.length;
        If(Greater(index, value_from_s32(length))) {
          Break;
        }

        Value *reg_a = value_register_for_descriptor(Register_A, temp->descriptor);
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
