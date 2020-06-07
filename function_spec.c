#include "bdd-for-c.h"

#include "prelude.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"

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

    Value_Overload *overload_list[] = {a, b};

    Value overload = {
      .overload_list = overload_list,
      .overload_count = 2,
    };

    Function(checker_value) {
      Value *x = call_function_value(&builder_, &overload, value_from_s64(0), 1);
      Value *y = call_function_value(&builder_, &overload, value_from_s32(0), 1);
      Return(Plus(x, y));
    }

    DWORD previous;
    VirtualProtect(function_buffer.memory, function_buffer.capacity, PAGE_EXECUTE, &previous);

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

  it("should parse c function forward declarations") {
    c_function_value("void fn_void()", 0);
    c_function_value("void fn_int(int)", 0);
    Value *explicit_void_arg = c_function_value("void fn_void(void)", 0);
    Value_Overload *overload = maybe_get_if_single_overload(explicit_void_arg);
    check(overload);
    check(overload->descriptor->function.argument_count == 0);
  }

  it("should be able to call puts() to say 'Hello, world!'") {
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
}
