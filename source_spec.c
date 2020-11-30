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
  Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});\
  check(result.type == Tokenizer_Result_Type_Success);\
  token_match_module(result.root, program_);\
  Value *_fn_value_id_ = scope_lookup_force(program_, program_->global_scope, slice_literal(#_fn_value_id_), 0);\
  (void)_fn_value_id_

#define test_program_inline_source(_source_, _fn_value_id_)\
  test_program_inline_source_base(_source_, _fn_value_id_);\
  check(_fn_value_id_);\
  program_jit(program_);\
  check(!spec_check_and_print_program(program_))


spec("source") {
  static Program test_program = {0};
  static Program *program_ = &test_program;
  static Slice test_file_name = slice_literal_fields("_test_.mass");

  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_allocator_make(temp_buffer);
    program_init(program_);
  }

  after_each() {
    program_deinit(program_);
    bucket_buffer_destroy(temp_buffer);
  }

  // Scope
  it("should be able to set and lookup values") {
    Value *test = value_from_s64(42);
    Scope *root_scope = scope_make(0);
    scope_define_value(root_scope, slice_literal("test"), test);
    Array_Scope_Entry *entries = scope_lookup(root_scope, slice_literal("test"));
    check(dyn_array_length(*entries) == 1);
    Scope_Entry *entry = dyn_array_pop(*entries);
    check(entry->type == Scope_Entry_Type_Value);
    check(entry->value == test);
  }

  it("should be able to lookup things from parent scopes") {
    Value *global = value_from_s64(42);
    Scope *root_scope = scope_make(0);
    scope_define_value(root_scope, slice_literal("global"), global);

    Value *level_1_test = value_from_s64(1);
    Scope *scope_level_1 = scope_make(root_scope);
    scope_define_value(scope_level_1, slice_literal("test"), level_1_test);

    Value *level_2_test = value_from_s64(1);
    Scope *scope_level_2 = scope_make(scope_level_1);
    scope_define_value(scope_level_2, slice_literal("test"), level_2_test);

    Array_Scope_Entry *entries = scope_lookup(root_scope, slice_literal("global"));
    check(dyn_array_length(*entries) == 1);
    Scope_Entry *entry = dyn_array_pop(*entries);
    check(entry->type == Scope_Entry_Type_Value);
    check(entry->value == global);
  }

  // Tokenizer
  it("should be able to tokenize an empty string") {
    Slice source = slice_literal("");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 0);
  }

  it("should be able to tokenize a comment") {
    Slice source = slice_literal("// foo\n");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 0);
  }

  it("should be able to turn newlines into tokens") {
    Slice source = slice_literal("\n");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 1);
    Token *newline = *dyn_array_get(root->children, 0);
    check(newline->type == Token_Type_Newline);
    check(slice_equal(newline->source, slice_literal("\n")));
  }

  it("should be able to turn hex digits") {
    Slice source = slice_literal("0xCAFE");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 1);
    Token *token = *dyn_array_get(root->children, 0);
    check(token->type == Token_Type_Hex_Integer);
    check(slice_equal(token->source, slice_literal("0xCAFE")));
  }

  it("should be able to tokenize a sum of integers") {
    Slice source = slice_literal("12 + foo123");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 3);
    check(slice_equal(root->source, source));

    Token *a_num = *dyn_array_get(root->children, 0);
    check(a_num->type == Token_Type_Integer);
    check(slice_equal(a_num->source, slice_literal("12")));

    Token *plus = *dyn_array_get(root->children, 1);
    check(plus->type == Token_Type_Operator);
    check(slice_equal(plus->source, slice_literal("+")));

    Token *id = *dyn_array_get(root->children, 2);
    check(id->type == Token_Type_Id);
    check(slice_equal(id->source, slice_literal("foo123")));
  }

  it("should be able to tokenize groups") {
    Slice source = slice_literal("(x)");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);

    Token *paren = *dyn_array_get(root->children, 0);
    check(paren->type == Token_Type_Paren);
    check(dyn_array_length(paren->children) == 1);
    check(slice_equal(paren->source, slice_literal("(x)")));

    Token *id = *dyn_array_get(paren->children, 0);
    check(id->type == Token_Type_Id);
  }

  it("should be able to tokenize strings") {
    Slice source = slice_literal("\"foo 123\"");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);
    Token *string = *dyn_array_get(root->children, 0);
    check(slice_equal(string->source, slice_literal("\"foo 123\"")));
  }

  it("should be able to tokenize nested groups with different braces") {
    Slice source = slice_literal("{[]}");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);

    Token *curly = *dyn_array_get(root->children, 0);
    check(curly->type == Token_Type_Curly);
    check(dyn_array_length(curly->children) == 1);
    check(slice_equal(curly->source, slice_literal("{[]}")));

    Token *square = *dyn_array_get(curly->children, 0);
    check(square->type == Token_Type_Square);
    check(dyn_array_length(square->children) == 0);
    check(slice_equal(square->source, slice_literal("[]")));
  }

  it("should be able to tokenize complex input") {
    Slice source = slice_literal(
      "foo :: (x: s8) -> {\n"
      "  return x + 3;\n"
      "}"
    );
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
  }

  it("should report a failure when encountering a brace that is not closed") {
    Slice source = slice_literal("(foo");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Error);
    check(dyn_array_length(result.errors) == 1);
    Parse_Error *error = dyn_array_get(result.errors, 0);
    check(slice_equal(error->source_range.file->path, test_file_name));
    check(error->source_range.offsets.from == 4);
    check(error->source_range.offsets.to == 4);
    check(slice_equal(error->message, slice_literal("Unexpected end of file. Expected a closing brace.")));
  }

  it("should report a failure when encountering a mismatched brace that") {
    Slice source = slice_literal("(foo}");
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Error);
    check(dyn_array_length(result.errors) == 1);
    Parse_Error *error = dyn_array_get(result.errors, 0);
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
    fn_type_void_to_s64 checker = value_as_function(foo, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should be able to tokenize complex input") {
    Slice source = slice_literal(
      "foo :: (x: s8) -> {\n"
      "  return x + 3;\n"
      "}"
    );
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
  }

  it("should be unwind stack on hardware exception") {
    Parse_Result result =
      program_import_file(program_, slice_literal("fixtures\\error_runtime_divide_by_zero"));
    check(result.type == Parse_Result_Type_Success);
    Value *main =
      scope_lookup_force(program_, program_->global_scope, slice_literal("main"), 0);
    check(main);

    program_jit(program_);
    check(!dyn_array_length(program_->errors))

    fn_type_s32_to_s32 checker = value_as_function(main, fn_type_s32_to_s32);

    volatile bool caught_exception = false;
    __try {
      checker(0);
    }
    __except(EXCEPTION_EXECUTE_HANDLER) {
      caught_exception = true;
    }
    check(caught_exception);
  }

  it("should be able to parse and run a void -> s64 function") {
    test_program_inline_source("foo :: () -> (s64) { 42 }", foo);
    fn_type_void_to_s64 checker = value_as_function(foo, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should correctly save volatile registers when calling other functions") {
    test_program_inline_source(
      "inner :: (x : s64) -> () { x = 21 };"
      "outer :: (x : s64) -> (s64) { inner(1); x }",
      outer
    );
    fn_type_s64_to_s64 checker = value_as_function(outer, fn_type_s64_to_s64);
    s64 actual = checker(42);
    check(actual == 42);
  }

  it("should be able to parse and run a s64 -> s64 function") {
    test_program_inline_source("foo :: (x : s64) -> (s64) { x }", foo);
    fn_type_s64_to_s64 checker = value_as_function(foo, fn_type_s64_to_s64);
    check(checker(42) == 42);
  }

  it("should be able to define, assign and lookup an s64 variable on the stack") {
    test_program_inline_source(
      "foo :: () -> (s64) { y : s8; y = 10; x := 21; x = 32; x + y }",
      foo
    );
    fn_type_void_to_s8 checker = value_as_function(foo, fn_type_void_to_s8);
    check(checker() == 42);
  }

  it("should be able to parse and run a triple plus function") {
    test_program_inline_source(
      "plus :: (x : s64, y : s64, z : s64) -> (s64) { x + y + z }",
      plus
    );
    fn_type_s64_s64_s64_to_s64 checker =
      value_as_function(plus, fn_type_s64_s64_s64_to_s64);
    check(checker(30, 10, 2) == 42);
  }

  it("should be able to parse and run a sum passed to another function as an argument") {
    test_program_inline_source(
      "id :: (ignored : s64, x : s64) -> (s64) { x }\n"
      "plus :: () -> (s64) { x : s64 = 40; y : s64 = 2; id(0, x + y) }",
      plus
    );
    fn_type_void_to_s64 checker = value_as_function(plus, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should report an error for inline external functions") {
    test_program_inline_source_base(
      "ExitProcess :: inline (x : s64) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")\n"
      "test :: () -> (s64) { ExitProcess(42) }",
      test
    );
    check(dyn_array_length(program_->errors));
  }

  it("should be able to parse and run inline id function") {
    test_program_inline_source(
      "id :: inline (x : s64) -> (s64) { x }\n"
      "test :: () -> (s64) { id(42) }",
      test
    );
    fn_type_void_to_s64 checker = value_as_function(test, fn_type_void_to_s64);
    check(checker() == 42);
  }

  it("should be able to parse and run inline id with an explicit return function") {
    test_program_inline_source(
      "id :: inline (x : s64) -> (s64) { if (x > 0) { return 20 }; x }\n"
      "test :: () -> (s64) { id(42) + 1 }",
      test
    );
    fn_type_void_to_s64 checker = value_as_function(test, fn_type_void_to_s64);
    check(checker() == 21);
  }

  it("should be able to parse and run multiple function definitions") {
    test_program_inline_source(
      "proxy :: () -> (s32) { plus(1, 2); plus(30 + 10, 2) }\n"
      "plus :: (x : s32, y : s32) -> (s32) { x + y }",
      proxy
    );
    s32 answer = value_as_function(proxy, fn_type_void_to_s32)();
    check(answer == 42);
  }

  it("should be able to define a local function") {
    test_program_inline_source(
      "checker :: () -> (s64) { local :: () -> (s64) { 42 }; local() }",
      checker
    );
    s64 answer = value_as_function(checker, fn_type_void_to_s64)();
    check(answer == 42);
  }

  it("should be able to parse and run functions with overloads") {
    Slice source = slice_literal(
      "size_of :: (x : s32) -> (s64) { 4 }\n"
      "size_of :: (x : s64) -> (s64) { 8 }\n"
      "checker_s64 :: (x : s64) -> (s64) { size_of(x) }\n"
      "checker_s32 :: (x : s32) -> (s64) { size_of(x) }\n"
    );
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);

    token_match_module(result.root, program_);

    Value *checker_s64 =
      scope_lookup_force(program_, program_->global_scope, slice_literal("checker_s64"), 0);
    Value *checker_32 =
      scope_lookup_force(program_, program_->global_scope, slice_literal("checker_s32"), 0);

    program_jit(program_);

    {
      s64 size = value_as_function(checker_s64, fn_type_s64_to_s64)(0);
      check(size == 8);
    }

    {
      s64 size = value_as_function(checker_32, fn_type_s32_to_s64)(0);
      check(size == 4);
    }
  }

  it("should be able to have an explicit return") {
    test_program_inline_source(
      "checker :: (x : s32) -> (s32) { return x }",
      checker
    );
    s32 actual = value_as_function(checker, fn_type_s32_to_s32)(42);
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
    fn_type_s32_to_s8 is_positive_fn = value_as_function(is_positive, fn_type_s32_to_s8);
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
    fn_type_void_to_s32 checker = value_as_function(test_fn, fn_type_void_to_s32);
    check(checker() == 42);
  }

  it("should be able to parse and run a program with labels and goto") {
    test_program_inline_source(
      "sum_up_to :: (x : s32) -> (s32) {"
        "sum : s32;"
        "sum = 0;"
        "loop : label;"
        "if (x < 0) { return sum };"
        "sum = sum + x;"
        "x = x + (-1);"
        "goto loop;"
      "}",
      sum_up_to
    );
    fn_type_s32_to_s32 sum_up_to_fn = value_as_function(sum_up_to, fn_type_s32_to_s32);
    check(sum_up_to_fn(0) == 0);
    check(sum_up_to_fn(1) == 1);
    check(sum_up_to_fn(2) == 3);
    check(sum_up_to_fn(3) == 6);
  }

  it("should be able to define and use a macro without a capture") {
    test_program_inline_source(
      "macro (the answer) (42)"
      "checker :: () -> (s32) { the answer }",
      checker
    );
    fn_type_void_to_s32 checker_fn = value_as_function(checker, fn_type_void_to_s32);
    check(checker_fn() == 42);
  }

  it("should be able to define and use a macro with a capture") {
    test_program_inline_source(
      "macro (negative _x) (- x)"
      "checker :: () -> (s32) { negative 42 }",
      checker
    );
    fn_type_void_to_s32 checker_fn = value_as_function(checker, fn_type_void_to_s32);
    check(checker_fn() == -42);
  }

  it("should be able to define and use a macro for while loop") {
    program_import_file(program_, slice_literal("lib\\prelude"));
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
    fn_type_s32_to_s32 sum_up_to_fn = value_as_function(sum_up_to, fn_type_s32_to_s32);
    check(sum_up_to_fn(0) == 0);
    check(sum_up_to_fn(1) == 1);
    check(sum_up_to_fn(2) == 3);
    check(sum_up_to_fn(3) == 6);
  }

  it("should be able to run fizz buzz") {
    Parse_Result result = program_import_file(program_, slice_literal("lib\\prelude"));
    check(result.type == Parse_Result_Type_Success);
    result = program_import_file(program_, slice_literal("fixtures\\fizz_buzz"));
    check(result.type == Parse_Result_Type_Success);

    Value *fizz_buzz =
      scope_lookup_force(program_, program_->global_scope, slice_literal("fizz_buzz"), 0);
    check(fizz_buzz);

    program_jit(program_);
    check(!dyn_array_length(program_->errors))

    fn_type_void_to_void checker = value_as_function(fizz_buzz, fn_type_void_to_void);
    checker();
  }

  it("should be able to parse fixed-bit sized type definitions") {
    test_program_inline_source(
      "int8 :: type(8);"
      "test :: () -> () {"
        "x : int8;"
      "}",
      test
    );

    fn_type_void_to_void checker = value_as_function(test, fn_type_void_to_void);
    checker();
  }

  it("should be able to parse struct definitions") {
    test_program_inline_source(
      "Point :: type(c_struct) { x : s32; y : s32; };"
      "test :: () -> (s32) {"
        "p : Point; p.x = 20; p.y = 22;"
        "p.x + p.y"
      "}",
      test
    );

    fn_type_void_to_s32 checker = value_as_function(test, fn_type_void_to_s32);
    check(checker() == 42);
  }

  it("should be able to call a function at compile time") {
    test_program_inline_source_base(
      "STATUS_CODE :: the_answer();"
      "the_answer :: () -> (s8) { 42 }",
      STATUS_CODE
    );

    Value *status =
      scope_lookup_force(program_, program_->global_scope, slice_literal("STATUS_CODE"), 0);
    check(status);
    check(status->descriptor->type == Descriptor_Type_Integer);
    check(status->operand.type == Operand_Type_Immediate_8);
    check(status->operand.s8 == 42);
  }

  it("should be able to execute arbitrary expression at compile time") {
    test_program_inline_source_base(
      "STATUS_CODE :: @( the_answer() + 2 );"
      "the_answer :: () -> (s8) { 40 }",
      STATUS_CODE
    );

    Value *status =
      scope_lookup_force(program_, program_->global_scope, slice_literal("STATUS_CODE"), 0);
    check(status);
    check(status->descriptor->type == Descriptor_Type_Integer);
    check(status->operand.type == Operand_Type_Immediate_8);
    check(status->operand.s8 == 42);
  }

  it("should be able to return structs while accepting other arguments") {
    test_program_inline_source(
      "Test_128bit :: type(c_struct) { x : s64; y : s64 };"
      "return_struct :: (x : s64) -> (Test_128bit) {"
        "result : Test_128bit;"
        "result.x = x;"
        "result.y = x / 2;"
        "result"
      "}",
      return_struct
    );

    fn_type_s64_to_test_128bit_struct checker =
      value_as_function(return_struct, fn_type_s64_to_test_128bit_struct);
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
    s64 size = value_as_function(checker, fn_type_s32_to_s64)(0);
    check(size == 4);
  }

  it("should parse and return c compatible strings") {
    test_program_inline_source(
      "checker :: () -> ([s8]) { \"test\" }",
      checker
    );
    const char *string = value_as_function(checker, fn_type_void_to_const_charp)();
    check(strcmp(string, "test") == 0);
  }

  it("should be able to return unsigned integer literals") {
    test_program_inline_source(
      "return_200 :: () -> (u8) { 200 }",
      return_200
    );
    fn_type_void_to_u8 checker = value_as_function(return_200, fn_type_void_to_u8);
    check(checker() == 200);
  }

  it("should use correct EFLAGS values when dealing with unsigned integers") {
    test_program_inline_source(
      "test :: () -> (u8) { x : u8 = 200; x < 0 }",
      test
    );
    fn_type_void_to_u8 checker = value_as_function(test, fn_type_void_to_u8);
    check(checker() == false);
  }

  it("should parse and correctly deal with 16 bit values") {
    test_program_inline_source(
      "add_one :: (x : s16) -> (s16) { x + 1 }",
      add_one
    );
    fn_type_s16_to_s16 checker = value_as_function(add_one, fn_type_s16_to_s16);
    check(checker(8) == 9);
  }

  it("should parse and write out an executable that exits with status code 42") {
    Slice source = slice_literal(
      "main :: () -> () { ExitProcess(42) }\n"
      "ExitProcess :: (status : s32) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")"
    );
    Tokenizer_Result result = tokenize(&(Source_File){test_file_name, source});
    check(result.type == Tokenizer_Result_Type_Success);

    token_match_module(result.root, program_);

    program_->entry_point = scope_lookup_force(program_, program_->global_scope, slice_literal("main"), 0);
    check(program_->entry_point->descriptor->type != Descriptor_Type_Any);
    check(!spec_check_and_print_program(program_));

    write_executable(L"build\\test_parsed.exe", program_, Executable_Type_Cli);
  }

  it("should parse and write an executable that prints Hello, world!") {
    program_import_file(program_, slice_literal("fixtures\\hello_world"));
    program_->entry_point = scope_lookup_force(program_, program_->global_scope, slice_literal("main"), 0);
    check(program_->entry_point->descriptor->type != Descriptor_Type_Any);
    check(!spec_check_and_print_program(program_));

    write_executable(L"build\\hello_world.exe", program_, Executable_Type_Cli);
  }

  describe("User Error") {
    it("should be reported when encountering invalid pointer type") {
      test_program_inline_source_base("main :: (arg : [s32 s32]) -> () {}", main);
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("Pointer type must have a single type inside"), error->message));
    }
    it("should be report wrong argument type to external()") {
      test_program_inline_source_base(
        "exit :: (status: s32) -> () external(\"kernel32.dll\", 42)", exit
      );
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("Second argument to external() must be a literal string"), error->message));
    }
    it("should be wrong type of label identifier") {
      test_program_inline_source_base(
        "main :: (status: s32) -> () { x : s32; goto x; }", main
      );
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("x is not a label"), error->message));
    }
    it("should be reported when non-type id is being used as type") {
      test_program_inline_source_base(
        "foo :: () -> () {};"
        "main :: (arg : foo) -> () {}", main
      );
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("foo is not a type"), error->message));
    }
    it("should be reported when non-type token is being used as type") {
      test_program_inline_source_base(
        "main :: (arg : 42) -> () {}", main
      );
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("42 is not a type"), error->message));
    }
    it("should be reported when encountering unknown type") {
      Parse_Result result =
        program_import_file(program_, slice_literal("fixtures\\error_unknown_type"));
      check(result.type == Parse_Result_Type_Success);
      scope_lookup_force(program_, program_->global_scope, slice_literal("main"), 0);
      check(dyn_array_length(program_->errors));
      Parse_Error *error = dyn_array_get(program_->errors, 0);
      check(slice_equal(slice_literal("Could not find type s33"), error->message));
    }
  }
}
