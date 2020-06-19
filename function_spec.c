#ifndef MACRO_DEBUG

#include "bdd-for-c.h"

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"

#endif

Buffer function_buffer;

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
spec("function") {

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

  it("should support short-curciting &&") {
    Function(checker_value) {
      Arg_s32(number);
      Arg_s8(condition);

      Value *less = Less(number, value_from_s32(42));
      Return(And(less, condition));
    }

    fn_type_s32_s8_to_s8 checker = value_as_function(checker_value, fn_type_s32_s8_to_s8);
    check(!checker(52, true));
    check(!checker(32, false));
    check(checker(32, true));
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

    DWORD previous;
    VirtualProtect(function_buffer.memory, function_buffer.capacity, PAGE_EXECUTE, &previous);

    fn_type_void_to_s64 sizeof_s64 = value_as_function(sizeof_s64_value, fn_type_void_to_s64);
    check(sizeof_s64() == 8);

    fn_type_void_to_s64 checker = value_as_function(checker_value, fn_type_void_to_s64);
    check(checker() == 12);
  }

  it("should support parametric polymorphism") {
    Value *id_s64 = make_identity(&descriptor_s64);
    Value *id_s32 = make_identity(&descriptor_s32);
    Value *addtwo_s64 = make_add_two(&descriptor_s64);
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

  it("should create a function to call a no argument fn") {
    Function(the_answer) {
      Return(value_from_s32(42));
    }
    Function(caller) {
      Arg(fn, the_answer->descriptor);
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
      Return(Call(
        id,
        value_from_s64(42)
      ));
    }
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
      Return(Call(
        args,
        value_from_s64(10),
        value_from_s64(20),
        value_from_s64(30),
        value_from_s64(40),
        value_from_s32(50),
        value_from_s64(60),
      ));
    }
    s64 result = value_as_function(caller, fn_type_void_to_s64)();
    check(result == 60);
  }

  it("should parse c function forward declarations") {
    c_function_value("void fn_void()", 0);
    c_function_value("void fn_int(int)", 0);
    Value *explicit_void_arg = c_function_value("void fn_void(void)", 0);
    check(explicit_void_arg->descriptor->function.argument_count == 0);
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

    value_as_function(hello, fn_type_void_to_void)();
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
      Value *message_value = reserve_stack(&builder_, &message_descriptor);
      move_value(&builder_, message_value, value_from_s32(hi_s32));
      Value *message_pointer_value = value_pointer_to(&builder_, message_value);
      Call(puts_value, message_pointer_value);
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

      Value *fMinusOne = Call(fib, minusOne);
      Value *fMinusTwo = Call(fib, minusTwo);

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
}
