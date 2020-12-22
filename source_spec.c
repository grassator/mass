#include "bdd-for-c.h"

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

typedef void (*fn_type_void_to_void)(void);
typedef f32 (*fn_type_void_to_f32)(void);
typedef f32 (*fn_type_f32_to_f32)(f32);
typedef f64 (*fn_type_f64_to_f64)(f64);
typedef s32 (*fn_type_void_to_s32)(void);
typedef s64 (*fn_type_void_to_s64)(void);
typedef const char *(*fn_type_void_to_const_charp)(void);
typedef s16 (*fn_type_s16_to_s16)(s16);
typedef void (*fn_type_voidp_to_void)(void*);
typedef s32 (*fn_type_voidp_to_s32)(void*);
typedef s64 (*fn_type_voidp_s64_to_s64)(void*, s64);
typedef s8  (*fn_type_s32_s8_to_s8)(s32, s8);
typedef void (*fn_type_s32p_to_void)(s32*);
typedef s8 (*fn_type_s32_to_s8)(s32);
typedef s8 (*fn_type_void_to_s8)();
typedef s32 (*fn_type_s32_to_s32)(s32);
typedef s64 (*fn_type_s32_to_s64)(s32);
typedef s32 (*fn_type_s32_s32_to_s32)(s32, s32);
typedef s64 (*fn_type_s64_to_s64)(s64);
typedef s64 (*fn_type_s64_s64_to_s64)(s64, s64);
typedef s64 (*fn_type_s64_s64_s64_to_s64)(s64, s64, s64);
typedef s64 (*fn_type_s64_s64_s64_s64_s64_to_s64)(s64, s64, s64, s64, s64);
typedef s64 (*fn_type_s64_s64_s64_s64_s64_s64_to_s64)(s64, s64, s64, s64, s64, s64);
typedef s32 (*fn_type__void_to_s32__to_s32)(fn_type_void_to_s32);

typedef u8 (*fn_type_void_to_u8)(void);

typedef struct {
  s64 x;
  s64 y;
} Test_128bit;

typedef Test_128bit (*fn_type_s64_to_test_128bit_struct)(s64);

bool
spec_check_and_print_program(
  Program *program
) {
  bool has_errors = false;
  for (u64 i = 0; i < dyn_array_length(program->errors); ++i) {
    has_errors = true;
    Parse_Error *error = dyn_array_get(program->errors, i);

    slice_print(error->message);
    printf("\n  at ");
    source_range_print_start_position(&error->source_range);
  }
  return has_errors;
}

#define test_program_inline_source_base(_source_, _fn_value_id_)\
  Slice source = slice_literal(_source_);\
  Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});\
  check(result.tag == Tokenizer_Result_Tag_Success);\
  token_parse(&test_context, token_view_from_token_array(result.Success.tokens));\
  Value *_fn_value_id_ = scope_lookup_force(\
    &test_context, test_context.program->global_scope, slice_literal(#_fn_value_id_)\
  );\
  (void)_fn_value_id_

#define test_program_inline_source(_source_, _fn_value_id_)\
  test_program_inline_source_base(_source_, _fn_value_id_);\
  check(!spec_check_and_print_program(test_context.program));\
  check(_fn_value_id_);\
  program_jit(&test_context);\
  check(!spec_check_and_print_program(test_context.program))


spec("source") {
  static Slice test_file_name = slice_literal_fields("_test_.mass");
  static Compilation_Context test_context = {0};

  before_each() {
    compilation_context_init(allocator_system, &test_context);
  }

  after_each() {
    compilation_context_deinit(&test_context);
  }

  // Scope
  it("should be able to set and lookup values") {
    Value *test = value_from_s64(test_context.allocator, 42);
    Scope *root_scope = scope_make(test_context.allocator, 0);
    scope_define_value(root_scope, slice_literal("test"), test);
    Scope_Entry *entry = scope_lookup(root_scope, slice_literal("test"));
    check(entry->type == Scope_Entry_Type_Value);
    check(entry->value == test);
  }

  it("should be able to lookup things from parent scopes") {
    Value *global = value_from_s64(test_context.allocator, 42);
    Scope *root_scope = scope_make(test_context.allocator, 0);
    scope_define_value(root_scope, slice_literal("global"), global);

    Value *level_1_test = value_from_s64(test_context.allocator, 1);
    Scope *scope_level_1 = scope_make(test_context.allocator, root_scope);
    scope_define_value(scope_level_1, slice_literal("test"), level_1_test);

    Value *level_2_test = value_from_s64(test_context.allocator, 1);
    Scope *scope_level_2 = scope_make(test_context.allocator, scope_level_1);
    scope_define_value(scope_level_2, slice_literal("test"), level_2_test);

    Scope_Entry *entry = scope_lookup(root_scope, slice_literal("global"));
    check(entry->type == Scope_Entry_Type_Value);
    check(entry->value == global);
  }

  // Tokenizer
  it("should be able to tokenize an empty string") {
    Slice source = slice_literal("");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 0);
  }

  it("should be able to tokenize a comment") {
    Slice source = slice_literal("// foo\n");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 0);
  }

  it("should be able to turn newlines into tokens") {
    Slice source = slice_literal("\n");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);
    const Token *new_line = *dyn_array_get(result.Success.tokens, 0);
    check(new_line->tag == Token_Tag_Newline);
    check(slice_equal(new_line->source, slice_literal("\n")));
  }

  it("should be able to parse hex integers") {
    Slice source = slice_literal("0xCAFE");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);
    const Token *token = *dyn_array_get(result.Success.tokens, 0);
    check(token->tag == Token_Tag_Hex_Integer);
    check(slice_equal(token->source, slice_literal("0xCAFE")));
  }

  it("should be able to parse binary integers") {
    Slice source = slice_literal("0b100");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);
    const Token *token = *dyn_array_get(result.Success.tokens, 0);
    check(token->tag == Token_Tag_Binary_Integer);
    check(slice_equal(token->source, slice_literal("0b100")));
  }

  it("should be able to tokenize a sum of integers") {
    Slice source = slice_literal("12 + foo123");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 3);

    const Token *a_num = *dyn_array_get(result.Success.tokens, 0);
    check(a_num->tag == Token_Tag_Integer);
    check(slice_equal(a_num->source, slice_literal("12")));

    const Token *plus = *dyn_array_get(result.Success.tokens, 1);
    check(plus->tag == Token_Tag_Operator);
    check(slice_equal(plus->source, slice_literal("+")));

    const Token *id = *dyn_array_get(result.Success.tokens, 2);
    check(id->tag == Token_Tag_Id);
    check(slice_equal(id->source, slice_literal("foo123")));
  }

  it("should be able to tokenize groups") {
    Slice source = slice_literal("(x)");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);

    const Token *paren = *dyn_array_get(result.Success.tokens, 0);
    check(paren->tag == Token_Tag_Group);
    check(paren->Group.type == Token_Group_Type_Paren);
    check(dyn_array_length(paren->Group.children) == 1);
    check(slice_equal(paren->source, slice_literal("(x)")));

    const Token *id = *dyn_array_get(paren->Group.children, 0);
    check(id->tag == Token_Tag_Id);
  }

  it("should be able to tokenize strings") {
    Slice source = slice_literal("\"foo 123\"");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);
    const Token *string = *dyn_array_get(result.Success.tokens, 0);
    check(slice_equal(string->source, slice_literal("\"foo 123\"")));
  }

  it("should be able to tokenize nested groups with different braces") {
    Slice source = slice_literal("{[]}");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
    check(dyn_array_length(result.Success.tokens) == 1);

    const Token *curly = *dyn_array_get(result.Success.tokens, 0);
    check(curly->tag == Token_Tag_Group);
    check(curly->Group.type == Token_Group_Type_Curly);
    check(dyn_array_length(curly->Group.children) == 1);
    check(slice_equal(curly->source, slice_literal("{[]}")));

    const Token *square = *dyn_array_get(curly->Group.children, 0);
    check(square->tag == Token_Tag_Group);
    check(square->Group.type == Token_Group_Type_Square);
    check(dyn_array_length(square->Group.children) == 0);
    check(slice_equal(square->source, slice_literal("[]")));
  }

  it("should be able to tokenize complex input") {
    Slice source = slice_literal(
      "foo :: (x: s8) -> {\n"
      "  return x + 3;\n"
      "}"
    );
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
  }

  it("should report a failure when encountering a brace that is not closed") {
    Slice source = slice_literal("(foo");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Error);
    check(dyn_array_length(result.Error.errors) == 1);
    Parse_Error *error = dyn_array_get(result.Error.errors, 0);
    check(slice_equal(error->source_range.file->path, test_file_name));
    check(error->source_range.offsets.from == 4);
    check(error->source_range.offsets.to == 4);
    check(slice_equal(error->message, slice_literal("Unexpected end of file. Expected a closing brace.")));
  }

  it("should report a failure when encountering a mismatched brace that") {
    Slice source = slice_literal("(foo}");
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Error);
    check(dyn_array_length(result.Error.errors) == 1);
    Parse_Error *error = dyn_array_get(result.Error.errors, 0);
    check(slice_equal(error->source_range.file->path, test_file_name));
    check(error->source_range.offsets.from == 4);
    check(error->source_range.offsets.to == 4);
    check(slice_equal(error->message, slice_literal("Mismatched closing brace")));
  }

  it("should be able to include raw machine code bytes") {
    // :WindowsOnly
    test_program_inline_source(
      // FIXME return type here should be s64, but then it will expect a value to return
      //       which does not exist as we are dealing with machine code bytes directly
      "foo :: () -> () { inline_machine_code_bytes(0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00) }",
      foo
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, foo);
    check(checker() == 42);
  }

  it("should be able to reference a declared label in raw machine code bytes") {
    // :WindowsOnly
    test_program_inline_source(
      // FIXME return type here should be s64, but then it will expect a value to return
      //       which does not exist as we are dealing with machine code bytes directly
      "foo :: () -> (s64) {"
        "goto start;"
        "label from_machine_code;"
        "return 42;"
        "label start;"
        // "goto from_machine_code;"
        "inline_machine_code_bytes(0xE9, from_machine_code);"
        "10"
      "}",
      foo
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, foo);
    check(checker() == 42);
  }

  it("should be able to tokenize complex input") {
    Slice source = slice_literal(
      "foo :: (x: s8) -> {\n"
      "  return x + 3;\n"
      "}"
    );
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);
  }

  #ifdef _WIN32
  it("should be unwind stack on hardware exception on Windows") {
    Mass_Result result =
      program_import_file(&test_context, slice_literal("fixtures\\error_runtime_divide_by_zero"));
    check(result.tag == Mass_Result_Tag_Success);
    Value *main =
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("main"));
    check(main);

    program_jit(&test_context);
    check(!dyn_array_length(test_context.program->errors))

    fn_type_s32_to_s32 checker = (fn_type_s32_to_s32)value_as_function(test_context.program, main);

    volatile bool caught_exception = false;
    __try {
      checker(0);
    }
    __except(EXCEPTION_EXECUTE_HANDLER) {
      caught_exception = true;
    }
    check(caught_exception);
  }
  #endif

  it("should be able to parse and run a void -> s64 function") {
    test_program_inline_source("foo :: () -> (s64) { 42 }", foo);
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, foo);
    check(checker() == 42);
  }

  it("should be able to turn binary literals into integers") {
    test_program_inline_source("foo :: () -> (s64) { 0b00101010 }", foo);
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, foo);
    check(checker() == 42);
  }

  it("should correctly save volatile registers when calling other functions") {
    test_program_inline_source(
      "inner :: (x : s64) -> () { x = 21 };"
      "outer :: (x : s64) -> (s64) { inner(1); x }",
      outer
    );
    fn_type_s64_to_s64 checker = (fn_type_s64_to_s64)value_as_function(test_context.program, outer);
    s64 actual = checker(42);
    check(actual == 42);
  }

  it("should be able to parse and run a s64 -> s64 function") {
    test_program_inline_source("foo :: (x : s64) -> (s64) { x }", foo);
    fn_type_s64_to_s64 checker = (fn_type_s64_to_s64)value_as_function(test_context.program, foo);
    check(checker(42) == 42);
  }

  it("should be able to define, assign and lookup an s64 variable on the stack") {
    test_program_inline_source(
      "foo :: () -> (s64) { y : s8; y = 10; x := 21; x = 32; x + y }",
      foo
    );
    fn_type_void_to_s8 checker = (fn_type_void_to_s8)value_as_function(test_context.program, foo);
    check(checker() == 42);
  }

  it("should be able to parse and run a triple plus function") {
    test_program_inline_source(
      "plus :: (x : s64, y : s64, z : s64) -> (s64) { x + y + z }",
      plus
    );
    fn_type_s64_s64_s64_to_s64 checker =
      (fn_type_s64_s64_s64_to_s64)value_as_function(test_context.program, plus);
    check(checker(30, 10, 2) == 42);
  }

  it("should be able to parse and run a subtraction of a negative number") {
    test_program_inline_source(
      "plus_one :: (x : s64) -> (s64) { x - -1 }",
      plus_one
    );
    fn_type_s64_to_s64 checker =
      (fn_type_s64_to_s64)value_as_function(test_context.program, plus_one);
    check(checker(41) == 42);
  }

  it("should be able to parse and run a sum passed to another function as an argument") {
    test_program_inline_source(
      "id :: (ignored : s64, x : s64) -> (s64) { x }\n"
      "plus :: () -> (s64) { x : s64 = 40; y : s64 = 2; id(0, x + y) }",
      plus
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, plus);
    check(checker() == 42);
  }

  it("should be able to parse and run macro id function") {
    test_program_inline_source(
      "id :: macro (x : s64) -> (s64) { x }\n"
      "test :: () -> (s64) { id(42) }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  it("should be able to parse and run macro id fn with an explicit return and an immediate arg") {
    test_program_inline_source(
      "id :: macro (x : s64) -> (s64) { if (x > 0) { return 20 }; x }\n"
      "test :: () -> (s64) { id(42) + 1 }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 21);
  }

  it("should be able to parse and run macro with a literal s64 type") {
    test_program_inline_source(
      "broken_plus :: macro (x : s64, cast(s64, 0)) -> (s64) { x + 1 }\n"
      "broken_plus :: macro (x : s64, y : s64) -> (s64) { x + y }\n"
      "test :: () -> (s64) { broken_plus(cast(s64, 41), cast(s64, 0)) }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  xit("should allow changes to the passed arguments to macro function") {
    test_program_inline_source(
      "process :: macro (y : any) -> () { y = 42; }\n"
      "test :: () -> (s64) { x := 20; process(x); x }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  xit("should allow changes to the passed arguments to inline function") {
    test_program_inline_source(
      "process :: inline (y : s64) -> () { y = 20; }\n"
      "test :: () -> (s64) { x := 42; process(x); x }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  it("should be able to parse negation inside a constant expression") {
    test_program_inline_source(
      "THE_ANSWER :: -(-42);"
      "test :: () -> (s64) { THE_ANSWER }",
      test
    );
    fn_type_void_to_s64 checker = (fn_type_void_to_s64)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  it("should be able to parse and run multiple function definitions") {
    test_program_inline_source(
      "proxy :: () -> (s32) { plus(1, 2); plus(30 + 10, 2) }\n"
      "plus :: (x : s32, y : s32) -> (s32) { x + y }",
      proxy
    );
    s32 answer = ((fn_type_void_to_s32)value_as_function(test_context.program, proxy))();
    check(answer == 42);
  }

  it("should be able to define a local function") {
    test_program_inline_source(
      "checker :: () -> (s64) { local :: () -> (s64) { 42 }; local() }",
      checker
    );
    s64 answer = ((fn_type_void_to_s64)value_as_function(test_context.program, checker))();
    check(answer == 42);
  }

  it("should be able to parse and run functions with overloads") {
    Slice source = slice_literal(
      "size_of :: (x : s32) -> (s64) { 4 }\n"
      "size_of :: (x : s64) -> (s64) { 8 }\n"
      "checker_s64 :: (x : s64) -> (s64) { size_of(x) }\n"
      "checker_s32 :: (x : s32) -> (s64) { size_of(x) }\n"
    );
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);

    token_parse(&test_context, token_view_from_token_array(result.Success.tokens));

    Value *checker_s64 =
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("checker_s64"));
    Value *checker_32 =
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("checker_s32"));

    program_jit(&test_context);

    {
      s64 size = ((fn_type_s64_to_s64)value_as_function(test_context.program, checker_s64))(0);
      check(size == 8);
    }

    {
      s64 size = ((fn_type_s32_to_s64)value_as_function(test_context.program, checker_32))(0);
      check(size == 4);
    }
  }

  it("should be able to have an explicit return") {
    test_program_inline_source(
      "checker :: (x : s32) -> (s32) { return x }",
      checker
    );
    s32 actual = ((fn_type_s32_to_s32)value_as_function(test_context.program, checker))(42);
    check(actual == 42);
  }

  it("should be able to parse and run if statement") {
    test_program_inline_source(
      "is_positive :: (x : s32) -> (s8) {"
        "if (x < 0) { return 0 };"
        "1"
      "}",
      is_positive
    );
    fn_type_s32_to_s8 is_positive_fn =
      (fn_type_s32_to_s8)value_as_function(test_context.program, is_positive);
    check(is_positive_fn(42) == 1);
    check(is_positive_fn(-2) == 0);
  }

  it("should be able to parse typed definition and assignment in the same statement") {
    test_program_inline_source(
      "test_fn :: () -> (s32) {"
        "result : s32 = 42;"
        "result"
      "}",
      test_fn
    );
    fn_type_void_to_s32 checker =
      (fn_type_void_to_s32)value_as_function(test_context.program, test_fn);
    check(checker() == 42);
  }

  it("should be able to parse and run a program with labels and goto") {
    test_program_inline_source(
      "sum_up_to :: (x : s32) -> (s32) {"
        "sum : s32;"
        "sum = 0;"
        "label loop;"
        "if (x < 0) { return sum };"
        "sum = sum + x;"
        "x = x - 1;"
        "goto loop;"
      "}",
      sum_up_to
    );
    fn_type_s32_to_s32 sum_up_to_fn =
      (fn_type_s32_to_s32)value_as_function(test_context.program, sum_up_to);
    check(sum_up_to_fn(0) == 0);
    check(sum_up_to_fn(1) == 1);
    check(sum_up_to_fn(2) == 3);
    check(sum_up_to_fn(3) == 6);
  }

  it("should be able to goto a label defined after the goto") {
    test_program_inline_source(
      "test :: () -> (s32) {"
        "x : s32 = 42;"
        "goto skip;"
        "x = 0;"
        "label skip;"
        "x"
      "}",
      test
    );
    fn_type_void_to_s32 checker =
      (fn_type_void_to_s32)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  it("should be able to define and use a syntax macro without a capture") {
    test_program_inline_source(
      "syntax (\"the\" \"answer\") 42;"
      "checker :: () -> (s32) { the answer }",
      checker
    );
    fn_type_void_to_s32 checker_fn =
      (fn_type_void_to_s32)value_as_function(test_context.program, checker);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a syntax macro matching start and end of statement") {
    test_program_inline_source(
      "syntax (^ \"foo\" $) 42;"
      "checker :: () -> (s64) { foo := 20; foo }",
      checker
    );
    fn_type_void_to_s64 checker_fn =
      (fn_type_void_to_s64)value_as_function(test_context.program, checker);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a syntax macro matching a curly brace block") {
    test_program_inline_source(
      "syntax (^ \"block\" {}@body $) body;"
      "checker :: () -> (s64) { block { 42 } }",
      checker
    );
    fn_type_void_to_s64 checker_fn =
      (fn_type_void_to_s64)value_as_function(test_context.program, checker);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a syntax macro matching a sequence at the end") {
    test_program_inline_source(
      "syntax (^ \"comment\" ..@ignore $);"
      "checker :: () -> (s64) { x := 42; comment x = x + 1; x }",
      checker
    );
    fn_type_void_to_s64 checker_fn =
      (fn_type_void_to_s64)value_as_function(test_context.program, checker);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a syntax macro matching a sequence in the middle") {
    test_program_inline_source(
      "syntax (\"comment_start\" ..@ignore \"comment_end\");"
      "checker :: () -> (s64) { x := 42; x = x comment_start + 1 comment_end; x }",
      checker
    );
    fn_type_void_to_s64 checker_fn =
      (fn_type_void_to_s64)value_as_function(test_context.program, checker);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a syntax macro with a capture") {
    test_program_inline_source(
      "syntax (\"negative\" .@x) (-x);"
      "checker :: () -> (s32) { negative 42 }",
      checker
    );
    fn_type_void_to_s32 checker_fn =
      (fn_type_void_to_s32)value_as_function(test_context.program, checker);
    check(checker_fn() == -42);
  }

  it("should be able to define and use a macro for while loop") {
    program_import_file(&test_context, slice_literal("lib\\prelude"));
    test_program_inline_source(
      "sum_up_to :: (x : s32) -> (s32) {"
        "sum : s32;"
        "sum = 0;"
        "while (x >= 0) {"
          "sum = sum + x;"
          "x = x + (-1);"
        "};"
        "return sum"
      "}",
      sum_up_to
    );
    fn_type_s32_to_s32 sum_up_to_fn =
      (fn_type_s32_to_s32)value_as_function(test_context.program, sum_up_to);
    check(sum_up_to_fn(0) == 0);
    check(sum_up_to_fn(1) == 1);
    check(sum_up_to_fn(2) == 3);
    check(sum_up_to_fn(3) == 6);
  }

  it("should be able to parse fixed-bit sized type definitions") {
    test_program_inline_source(
      "int8 :: bit_type(8);"
      "test :: () -> () {"
        "x : int8;"
      "}",
      test
    );

    fn_type_void_to_void checker =
      (fn_type_void_to_void)value_as_function(test_context.program, test);
    checker();
  }

  it("should be able to parse struct definitions") {
    test_program_inline_source(
      "Point :: c_struct({ x : s32; y : s32; });"
      "test :: () -> (s32) {"
        "p : Point; p.x = 20; p.y = 22;"
        "p.x + p.y"
      "}",
      test
    );

    fn_type_void_to_s32 checker =
      (fn_type_void_to_s32)value_as_function(test_context.program, test);
    check(checker() == 42);
  }

  it("should be able to call a function at compile time") {
    test_program_inline_source_base(
      "STATUS_CODE :: the_answer();"
      "the_answer :: () -> (s8) { 42 }",
      STATUS_CODE
    );

    Value *status = scope_lookup_force(
      &test_context, test_context.program->global_scope, slice_literal("STATUS_CODE")
    );
    check(status);
    check(descriptor_is_integer(status->descriptor));
    check(status->operand.tag == Operand_Tag_Immediate);
    check(status->operand.byte_size == 1);
    check(operand_immediate_memory_as_s8(&status->operand) == 42);
  }

  it("should be able to execute arbitrary expression at compile time") {
    test_program_inline_source_base(
      "STATUS_CODE :: @( the_answer() + 2 );"
      "the_answer :: () -> (s8) { 40 }",
      STATUS_CODE
    );

    Value *status = scope_lookup_force(
      &test_context, test_context.program->global_scope, slice_literal("STATUS_CODE")
    );
    check(status);
    check(descriptor_is_integer(status->descriptor));
    check(status->operand.tag == Operand_Tag_Immediate);
    check(status->operand.byte_size == 1);
    check(operand_immediate_memory_as_s8(&status->operand) == 42);
  }

  it("should be able to return structs while accepting other arguments") {
    test_program_inline_source(
      "Test_128bit :: c_struct({ x : s64; y : s64 });"
      "return_struct :: (x : s64) -> (Test_128bit) {"
        "result : Test_128bit;"
        "result.x = x;"
        "result.y = x / 2;"
        "result"
      "}",
      return_struct
    );

    fn_type_s64_to_test_128bit_struct checker =
      (fn_type_s64_to_test_128bit_struct) value_as_function(test_context.program, return_struct);
    Test_128bit test_128bit = checker(42);
    check(test_128bit.x == 42);
    check(test_128bit.y == 21);
  }

  it("should be able to parse and run functions with local overloads") {
    test_program_inline_source(
      "size_of :: (x : s32) -> (s64) { 4 }\n"
      "checker :: (x : s32) -> (s64) { size_of :: (x : s64) -> (s64) { 8 }; size_of(x) }",
      checker
    );
    s64 size = ((fn_type_s32_to_s64)value_as_function(test_context.program, checker))(0);
    check(size == 4);
  }

  it("should parse and return C-compatible strings") {
    test_program_inline_source(
      "checker :: () -> ([s8]) { \"test\" }",
      checker
    );
    const char *string =
      ((fn_type_void_to_const_charp)value_as_function(test_context.program, checker))();
    check(strcmp(string, "test") == 0);
  }

  it("should be able to return unsigned integer literals") {
    test_program_inline_source(
      "return_200 :: () -> (u8) { 200 }",
      return_200
    );
    fn_type_void_to_u8 checker =
      (fn_type_void_to_u8)value_as_function(test_context.program, return_200);
    check(checker() == 200);
  }

  it("should use correct EFLAGS values when dealing with unsigned integers") {
    test_program_inline_source(
      "test :: () -> (u8) { x : u8 = 200; x < 0 }",
      test
    );
    fn_type_void_to_u8 checker = (fn_type_void_to_u8)value_as_function(test_context.program, test);
    check(checker() == false);
  }

  it("should parse and correctly deal with 16 bit values") {
    test_program_inline_source(
      "add_one :: (x : s16) -> (s16) { x + 1 }",
      add_one
    );
    fn_type_s16_to_s16 checker = (fn_type_s16_to_s16)value_as_function(test_context.program, add_one);
    check(checker(8) == 9);
  }

  it("should parse and write out an executable that exits with status code 42") {
    Slice source = slice_literal(
      "main :: () -> () { ExitProcess(42) }\n"
      "ExitProcess :: (status : s32) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")"
    );
    Tokenizer_Result result = tokenize(test_context.allocator, &(Source_File){test_file_name, source});
    check(result.tag == Tokenizer_Result_Tag_Success);

    token_parse(&test_context, token_view_from_token_array(result.Success.tokens));

    test_context.program->entry_point =
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("main"));
    check(test_context.program->entry_point->descriptor->tag != Descriptor_Tag_Any);
    check(!spec_check_and_print_program(test_context.program));

    write_executable("build\\test_parsed.exe", &test_context, Executable_Type_Cli);
  }

  it("should parse and write an executable that prints Hello, world!") {
    program_import_file(&test_context, slice_literal("fixtures\\hello_world"));
    test_context.program->entry_point =
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("main"));
    check(test_context.program->entry_point);
    check(test_context.program->entry_point->descriptor->tag != Descriptor_Tag_Any);
    check(!spec_check_and_print_program(test_context.program));

    write_executable("build\\hello_world.exe", &test_context, Executable_Type_Cli);
  }

  it("should be able to run fibonnacii") {
    test_program_inline_source(
      "fibonnacci :: (n : s64) -> (s64) {"
        "if (n < 2) { return n };"
        "fibonnacci(n + (-1)) + fibonnacci(n + (-2))"
      "}",
      fibonnacci
    );

    fn_type_s64_to_s64 checker =
      (fn_type_s64_to_s64)value_as_function(test_context.program, fibonnacci);
    check(checker(0) == 0);
    check(checker(1) == 1);
    check(checker(10) == 55);
  }

  it("should be able to run fizz buzz") {
    Mass_Result result = program_import_file(&test_context, slice_literal("lib\\prelude"));
    check(result.tag == Mass_Result_Tag_Success);
    result = program_import_file(&test_context, slice_literal("fixtures\\fizz_buzz"));
    check(result.tag == Mass_Result_Tag_Success);

    Value *fizz_buzz = scope_lookup_force(
      &test_context, test_context.program->global_scope, slice_literal("fizz_buzz")
    );
    check(fizz_buzz);

    program_jit(&test_context);
    check(!spec_check_and_print_program(test_context.program));

    fn_type_void_to_void checker =
      (fn_type_void_to_void)value_as_function(test_context.program, fizz_buzz);
    checker();
  }

  describe("User Error") {
    it("should report an error for an `if` statement without a body or a condition") {
      test_program_inline_source_base("main :: () -> () { if; }", main);
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("`if` keyword must be followed by an expression"), error->message));
    }
    it("should report an error for an `if` statement with an incorrect condition") {
      test_program_inline_source_base("main :: () -> () { if (1 < 0) { 0 } 42; }", main);
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Could not parse the expression"), error->message));
    }
    it("should be reported when encountering invalid pointer type") {
      test_program_inline_source_base("main :: (arg : [s32 s32]) -> () {}", main);
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Pointer type must have a single type inside"), error->message));
    }
    it("should be report wrong argument type to external()") {
      test_program_inline_source_base(
        "exit :: (status: s32) -> () external(\"kernel32.dll\", 42)", exit
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Second argument to external() must be a literal string"), error->message));
    }
    it("should be wrong type of label identifier") {
      test_program_inline_source_base(
        "main :: (status: s32) -> () { x : s32; goto x; }", main
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("x is not a label"), error->message));
    }
    it("should be reported when non-type id is being used as type") {
      test_program_inline_source_base(
        "foo :: () -> () {};"
        "main :: (arg : foo) -> () {}", main
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("foo is not a type"), error->message));
    }
    it("should be reported when non-type token is being used as type") {
      test_program_inline_source_base(
        "main :: (arg : 42) -> () {}", main
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("42 is not a type"), error->message));
    }
    it("should be reported when encountering unknown type") {
      Mass_Result result =
        program_import_file(&test_context, slice_literal("fixtures\\error_unknown_type"));
      check(result.tag == Mass_Result_Tag_Success);
      scope_lookup_force(&test_context, test_context.program->global_scope, slice_literal("main"));
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Could not find type s33"), error->message));
    }
    it("should be reported when encountering unknown top level statement") {
      test_program_inline_source_base(
        "foo bar", main
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Could not parse a top level statement"), error->message));
    }
    it("should report an error for macro external functions") {
      test_program_inline_source_base(
        "ExitProcess :: macro (x : s64) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")\n"
        "test :: () -> (s64) { ExitProcess(42) }",
        test
      );
      check(dyn_array_length(test_context.program->errors));
    }
    it("should report an overload overlap") {
      test_program_inline_source_base(
        "overload :: (x : s64, y : s32) -> () { }\n"
        "overload :: (x : s32, y : s64) -> () { }\n"
        "test :: () -> () { overload(1, 2) }",
        test
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(slice_literal("Could not decide which overload to pick"), error->message));
    }

    it("should report an end of statement marker not at the start of a syntax definition") {
      test_program_inline_source_base(
        "syntax (\"foo\" ^ .@_ );"
        "dummy :: () -> () {}",
        dummy
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(
        slice_literal("^ operator (statement start match) can only appear at the start of the pattern."),
        error->message
      ));
    }

    it("should report an end of statement marker not at the end of a syntax definition") {
      test_program_inline_source_base(
        "syntax (\"foo\" $ .@_ );"
        "dummy :: () -> () {}",
        dummy
      );
      check(dyn_array_length(test_context.program->errors));
      Parse_Error *error = dyn_array_get(test_context.program->errors, 0);
      check(slice_equal(
        slice_literal("$ operator (statement end match) can only appear at the end of the pattern."),
        error->message
      ));
    }
  }
}
