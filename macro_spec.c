#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"
#include "macro.h"

Value *
make_identity(
  Program *program_,
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
  Program *program_,
  Descriptor *type
) {
  Function(addtwo) {
    Arg(x, type);
    Return(Plus(x, value_from_s64(2)));
  }
  return addtwo;
}

Value *
maybe_cast_to_tag(
  Function_Builder *builder,
  Slice name,
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
    if (slice_equal(name, struct_->name)) {

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
  Program *program_,
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
  program_end(program_);

  return value_as_function(checker, fn_type_s32_to_s8);
}


spec("spec") {

  static Program test_program = {0};
  static Program *program_ = &test_program;

  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_allocator_make(temp_buffer);
    program_init(program_);
  }

  after_each() {
    program_deinit(program_);
    bucket_buffer_destroy(temp_buffer);
  }

  it("should have a way to create a function to checks if a character is one of the provided set") {
    fn_type_s32_to_s8 is_whitespace = create_is_character_in_set_checker_fn(program_, " \n\r\t");
    check(is_whitespace(' '));
    check(is_whitespace('\r'));
    check(is_whitespace('\n'));
    check(is_whitespace('\t'));
    check(!is_whitespace('a'));
    check(!is_whitespace('2'));
    check(!is_whitespace('-'));
  }

  it("should support returning structs larger than 64 bits on the stack") {
    Descriptor *point_struct_descriptor = descriptor_struct_make();
    descriptor_struct_add_field(point_struct_descriptor, &descriptor_s64, slice_literal("x"));
    descriptor_struct_add_field(point_struct_descriptor, &descriptor_s64, slice_literal("y"));

    Value *return_overload = temp_allocate(Value);
    *return_overload = (Value) {
      .descriptor = point_struct_descriptor,
      .operand = stack(0, descriptor_byte_size(point_struct_descriptor)),
    };

    Descriptor *c_test_fn_descriptor = temp_allocate(Descriptor);
    *c_test_fn_descriptor = (Descriptor){
      .type = Descriptor_Type_Function,
      .function = {
        .arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator),
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
      Value *x = struct_get_field(test_result, slice_literal("x"));
      Return(x);
    }
    program_end(program_);

    fn_type_void_to_s64 checker = value_as_function(checker_value, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should support RIP-relative addressing") {
    Value *global_a = value_global(program_, &descriptor_s32);
    {
      check(global_a->operand.type == Operand_Type_RIP_Relative);

      s32 *address = rip_value_pointer(program_, global_a);
      *address = 32;
    }
    Value *global_b = value_global(program_, &descriptor_s32);
    {
      check(global_b->operand.type == Operand_Type_RIP_Relative);
      s32 *address = rip_value_pointer(program_, global_b);
      *address = 10;
    }

    Function(checker_value) {
      Return(Plus(global_a, global_b));
    }
    program_end(program_);
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

  it("should support tagged unions") {
    Array_Descriptor_Struct_Field some_fields = dyn_array_make(Array_Descriptor_Struct_Field);
    dyn_array_push(some_fields, (Descriptor_Struct_Field) {
      .name = slice_literal("value"),
      .descriptor = &descriptor_s64,
      .offset = 0,
    });

    Descriptor_Struct constructors[] = {
      {
        .name = slice_literal("None"),
        .fields = dyn_array_make(Array_Descriptor_Struct_Field),
      },
      {
        .name = slice_literal("Some"),
        .fields = some_fields,
      },
    };

    Descriptor option_s64_descriptor = {
      .type = Descriptor_Type_Tagged_Union,
      .tagged_union = {
        .struct_list = constructors,
        .struct_count = countof(constructors),
      },
    };


    Function(with_default_value) {
      Arg(option_value, descriptor_pointer_to(&option_s64_descriptor));
      Arg_s64(default_value);
      Value *some = maybe_cast_to_tag(builder_, slice_literal("Some"), option_value);
      If(some) {
        Value *value = struct_get_field(some, slice_literal("value"));
        Return(value);
      }
      Return(default_value);
    }
    program_end(program_);

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

  it("should say that (s64 *) is not the same as (void *)") {
    check(same_type(descriptor_pointer_to(&descriptor_s64), descriptor_pointer_to(&descriptor_void)));
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
    Descriptor *a = descriptor_struct_make();
    descriptor_struct_add_field(a, &descriptor_s32, slice_literal("x"));

    Descriptor *b = descriptor_struct_make();
    descriptor_struct_add_field(b, &descriptor_s32, slice_literal("x"));

    check(same_type(a, a));
    check(!same_type(a, b));
  }

  it("should support structs") {
    // struct Size { s8 width; s32 height; };

    Descriptor *size_struct_descriptor = descriptor_struct_make();
    descriptor_struct_add_field(size_struct_descriptor, &descriptor_s32, slice_literal("width"));
    descriptor_struct_add_field(size_struct_descriptor, &descriptor_s32, slice_literal("height"));
    descriptor_struct_add_field(size_struct_descriptor, &descriptor_s32, slice_literal("dummy"));

    Descriptor *size_struct_pointer_descriptor = descriptor_pointer_to(size_struct_descriptor);

    Function(area) {
      Arg(size_struct, size_struct_pointer_descriptor);
      Return(Multiply(
        struct_get_field(size_struct, slice_literal("width")),
        struct_get_field(size_struct, slice_literal("height"))
      ));
    }
    program_end(program_);

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
        move_value(builder_, reg_a, temp);

        Operand pointer = {
          .type = Operand_Type_Memory_Indirect,
          .byte_size = item_byte_size,
          .indirect = (const Operand_Memory_Indirect) {
            .reg = rax.reg,
            .displacement = 0,
          }
        };
        push_instruction(builder_, (Instruction) {inc, {pointer, 0, 0}});
        push_instruction(builder_, (Instruction) {add, {temp->operand, imm32(item_byte_size), 0}});

        push_instruction(builder_, (Instruction) {inc, {index->operand, 0, 0}});
      }

    }
    program_end(program_);
    value_as_function(increment, fn_type_s32p_to_void)(array);

    check(array[0] == 2);
    check(array[1] == 3);
    check(array[2] == 4);
  }

  it("should write out an executable that exits with status code 42") {
    Value *ExitProcess_value = c_function_import(program_, "kernel32.dll", "s64 ExitProcess(s32)");

    Function(my_exit) {
      Call(ExitProcess_value, value_from_s32(42));
    }

    Function(main) {
      program_->entry_point = builder_->value;
      Call(my_exit);
    }
    write_executable(L"build\\test.exe", program_, Executable_Type_Cli);
  }

  it("should write out an executable that prints Hello, world!") {
    Value *GetStdHandle_value = c_function_import(program_, "kernel32.dll", "s64 GetStdHandle(s32)");
    Value *STD_OUTPUT_HANDLE_value = value_from_s32(-11);
    Value *ExitProcess_value = c_function_import(program_, "kernel32.dll", "s64 ExitProcess(s32)");
    Value *WriteFile_value = c_function_import(
      program_,
      "kernel32.dll",
      "s32 WriteFile(s64, void *, s32, void *, s64)"
    );

    Function(main) {
      program_->entry_point = builder_->value;
      Value *handle = Call(GetStdHandle_value, STD_OUTPUT_HANDLE_value);
      Stack_s32(bytes_written, value_from_s32(0));
      Value *bytes_written_ptr = value_pointer_to(builder_, bytes_written);
      Value *message_bytes = value_global_c_string_from_slice(
        program_, slice_literal("Hello, world!")
      );
      Value *message_ptr = value_pointer_to(builder_, message_bytes);
      Call(
        WriteFile_value,
        handle, // hFile
        message_ptr, // lpBuffer
        value_from_s32(message_bytes->descriptor->array.length), // nNumberOfBytesToWrite
        bytes_written_ptr, // lpNumberOfBytesWritten
        value_from_s64(0)  // lpOverlapped
      );
      Call(ExitProcess_value, value_from_s32(0));
    }
    write_executable(L"build\\hello_world.exe", program_, Executable_Type_Cli);
  }

  it("should support an empty Function") {
    Function(checker_value) {}
    program_end(program_);
  }

  it("should support short-curcuiting &&") {
    Function(checker_value) {
      Arg_s32(number);
      Arg_s8(condition);

      Value *less = Less(number, value_from_s32(42));
      Return(And(less, condition));
    }
    program_end(program_);

    fn_type_s32_s8_to_s8 checker = value_as_function(checker_value, fn_type_s32_s8_to_s8);
    check(!checker(52, true));
    check(!checker(32, false));
    check(checker(32, true));
  }

  it("should support short-curcuiting ||") {
    Function(checker_value) {
      Arg_s32(number);
      Arg_s8(condition);

      Value *less = Less(number, value_from_s32(42));
      Return(Or(less, condition));
    }
    program_end(program_);

    fn_type_s32_s8_to_s8 checker = value_as_function(checker_value, fn_type_s32_s8_to_s8);
    check(checker(52, true));
    check(checker(false, 32));
    check(checker(32, true));
    check(!checker(52, false));
  }

  it("should support multi-way case block") {
    Function(checker_value) {
      Arg_s32(number);

      Match {
        Case(Eq(number, value_from_s32(42))) {
          Return(number);
        }
        Case(Less(number, value_from_s32(50))) {
          Return(value_from_s32(0));
        }
        CaseAny {
          Return(value_from_s32(100));
        }
      }
    }
    program_end(program_);

    fn_type_s32_to_s32 checker = value_as_function(checker_value, fn_type_s32_to_s32);
    check(checker(42) == 42);
    check(checker(32) == 0);
    check(checker(52) == 100);
  }

  it("should support ad-hoc polymorphism / overloading") {
    Function(sizeof_s32) {
      Arg_s32(x);
      (void)x;
      Return(value_from_s64(4));
    }
    Function(sizeof_s64_value) {
      Arg_s64(x);
      (void)x;
      Return(value_from_s64(8));
    }

    sizeof_s32->descriptor->function.next_overload = sizeof_s64_value;

    Function(checker_value) {
      Value *x = Call(sizeof_s32, value_from_s64(0));
      Value *y = Call(sizeof_s32, value_from_s32(0));
      Return(Plus(x, y));
    }

    program_end(program_);

    fn_type_void_to_s64 sizeof_s64 = value_as_function(sizeof_s64_value, fn_type_void_to_s64);
    check(sizeof_s64() == 8);

    fn_type_void_to_s64 checker = value_as_function(checker_value, fn_type_void_to_s64);
    check(checker() == 12);
  }

  it("should support parametric polymorphism") {
    Value *id_s64 = make_identity(program_, &descriptor_s64);
    Value *id_s32 = make_identity(program_, &descriptor_s32);
    Value *addtwo_s64 = make_add_two(program_, &descriptor_s64);
    Function(check) {
      Call(id_s64, value_from_s64(0));
      Call(id_s32, value_from_s32(0));
      Call(addtwo_s64, value_from_s64(0));
    }
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
    program_end(program_);
    s32 result = value_as_function(the_answer, fn_type_void_to_s32)();
    check(result == 42);
  }

  it("should create function that returns s64 value that was passed") {
    Function(id) {
      Arg_s64(x);
      Return(x);
    }
    program_end(program_);
    s64 result = value_as_function(id, fn_type_s64_to_s64)(42);
    check(result == 42);
  }

  it("should create function increments s32 value passed to it") {
    Function(increment) {
      Arg_s32(x);

      Stack_s32(one, value_from_s32(1));
      Stack_s32(two, value_from_s32(2));

      Return(Plus(x, Minus(two, one)));
    }
    program_end(program_);
    s32 result = value_as_function(increment, fn_type_s32_to_s32)(42);
    check(result == 43);
  }

  it("should correctly handle constant conditions") {
    Function(checker_value) {
      If(Eq(value_from_s32(1), value_from_s32(0))) {
        Return(value_from_s32(0));
      }
      If(Eq(value_from_s32(1), value_from_s32(1))) {
        Return(value_from_s32(1));
      }
      Return(value_from_s32(-1));

      for(u64 i = 0; i < dyn_array_length(builder_->instructions); ++i) {
        Instruction *instruction = dyn_array_get(builder_->instructions, i);
        if (instruction->mnemonic.name) {
          check(strcmp(instruction->mnemonic.name, cmp.name) != 0);
        }
      }
    }

    program_end(program_);
    fn_type_void_to_s32 checker = value_as_function(checker_value, fn_type_void_to_s32);
    s32 result = checker();
    check(result == 1);
  }

  it("should have a function that returns 0 if arg is zero, 1 otherwise") {
    Function(is_non_zero_value) {
      Arg_s32(x);
      If(Eq(x, value_from_s32(0))) {
        Return(value_from_s32(0));
      }
      Return(value_from_s32(1));
    }
    program_end(program_);
    fn_type_s32_to_s32 is_non_zero = value_as_function(is_non_zero_value, fn_type_s32_to_s32);
    s32 result = is_non_zero(0);
    check(result == 0);
    result = is_non_zero(42);
    check(result == 1);
  }

  it("should make function that multiplies by 2") {
    Function(twice) {
      Arg_s64(x);
      Return(Multiply(x, value_from_s64(2)));
    }
    program_end(program_);

    s64 result = value_as_function(twice, fn_type_s64_to_s64)(42);
    check(result == 84);
  }

  it("should make function that divides two number") {
    Function(half) {
      Arg_s32(x);
      Arg_s32(y);
      Return(Divide(x, y));
    }
    program_end(program_);

    s32 result = value_as_function(half, fn_type_s32_s32_to_s32)(-42, 2);
    check(result == -21);
  }

  it("should create a function to call a no argument fn") {
    Function(the_answer) {
      Return(value_from_s32(42));
    }
    Function(caller) {
      Arg(fn, the_answer->descriptor);
      Return(call_function_value(builder_, fn, 0, 0));
    }
    program_end(program_);
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
      Return(Call(
        id,
        value_from_s64(42)
      ));
    }
    program_end(program_);
    fn_type_void_to_s64 the_answer = value_as_function(partial, fn_type_void_to_s64);
    s64 result = the_answer();
    check(result == 42);
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
    program_end(program_);
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
    program_end(program_);
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
      Return(Call(
        args,
        value_from_s64(10),
        value_from_s64(20),
        value_from_s64(30),
        value_from_s64(40),
        value_from_s32(50),
        value_from_s64(60)
      ));
    }
    program_end(program_);
    s64 result = value_as_function(caller, fn_type_void_to_s64)();
    check(result == 60);
  }

  it("should parse c function forward declarations") {
    c_function_descriptor("void fn_void()");
    c_function_descriptor("void fn_int(int)");
    Descriptor *descriptor = c_function_descriptor("void fn_void(void)");
    check(dyn_array_length(descriptor->function.arguments) == 0);
  }

  it("should be able to call puts() to say 'Hello, world!'") {
    const char *message = "Hello, world!";

    Value message_value = {
      .descriptor = descriptor_pointer_to(&descriptor_s8),
      .operand = imm64((s64) message),
    };

    Value *puts_value = c_function_value("int puts(const char*)", (fn_type_opaque) puts);

    Function(hello) {
      Call(puts_value, &message_value);
    }
    program_end(program_);

    value_as_function(hello, fn_type_void_to_void)();
  }

  it("should be able to call imported function") {
    Value *GetStdHandle_value = c_function_import(
      program_,
      "kernel32.dll",
      "s64 GetStdHandle(s32)"
    );

    Function(checker_value) {
      Return(Call(GetStdHandle_value, value_from_s32(STD_INPUT_HANDLE)));
    }
    program_end(program_);
    HANDLE actual = (HANDLE) value_as_function(checker_value, fn_type_void_to_s64)();
    check(actual == GetStdHandle(STD_INPUT_HANDLE));
  }

  it("should be able to call puts() to say 'Hi!'") {
    Value *puts_value = c_function_value("int puts(const char*)", (fn_type_opaque) puts);

    Descriptor message_descriptor = {
      .type = Descriptor_Type_Fixed_Size_Array,
      .array = {
        .item = &descriptor_s8,
        .length = 4,
      },
    };

    u8 hi[] = {'H', 'i', '!', 0};
    s32 hi_s32 = *((s32 *)hi);
    Function(hello) {
      Value *message_value = reserve_stack(builder_, &message_descriptor);
      move_value(builder_, message_value, value_from_s32(hi_s32));
      Value *message_pointer_value = value_pointer_to(builder_, message_value);
      Call(puts_value, message_pointer_value);
    }
    program_end(program_);

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

      Value *fMinusOne = Call(fib, minusOne);
      Value *fMinusTwo = Call(fib, minusTwo);

      Value *result = Plus(fMinusOne, fMinusTwo);

      Return(result);
    }
    program_end(program_);

    fn_type_s64_to_s64 f = value_as_function(fib, fn_type_s64_to_s64);
    check(f(0) == 0);
    check(f(1) == 1);
    check(f(2) == 1);
    check(f(3) == 2);
    check(f(6) == 8);
  }

  it("should be able to encode instructions with implicit A register argument") {
    Function(check) {
      Value *result = value_register_for_descriptor(Register_A, &descriptor_s32);
      move_value(builder_, result, value_from_s32(40));
      push_instruction(builder_, (Instruction) {add, {result->operand, imm32(2), 0}});
      Return(result);
    }
    program_end(program_);
    fn_type_void_to_s32 f = value_as_function(check, fn_type_void_to_s32);
    check(f() == 42);
  }

  it("should be able to return an f32 value") {
    Function(check) {
      Return(value_from_f32(program_, 42.0f));
    }
    program_end(program_);
    fn_type_void_to_f32 f = value_as_function(check, fn_type_void_to_f32);
    f32 result = f();
    check(result == 42.0f);
  }

  it("should be able to have a f32 identity function") {
    Function(check) {
      Arg(arg, &descriptor_f32);
      Return(arg);
    }
    program_end(program_);
    fn_type_f32_to_f32 f = value_as_function(check, fn_type_f32_to_f32);
    f32 result = f(42.0f);
    check(result == 42.0f);
  }
}
