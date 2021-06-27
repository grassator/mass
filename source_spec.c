#include "bdd-for-c.h"

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

typedef struct {
  s64 x;
  s64 y;
} Test_128bit;

bool
spec_check_mass_result(
  const Mass_Result *result
) {
  if (result->tag == Mass_Result_Tag_Success) return true;
  const Mass_Error *error = &result->Error.error;
  Fixed_Buffer *error_buffer = mass_error_to_string(error);
  slice_print(fixed_buffer_as_slice(error_buffer));
  fixed_buffer_destroy(error_buffer);
  printf("\n  at ");
  source_range_print_start_position(&result->Error.error.source_range);
  return false;
}

typedef s64 (*Spec_Callback)();
static s64 spec_callback() { return 42; }

#define spec_check_slice(_ACTUAL_, _EXPECTED_)\
  do {\
    Slice actual = (_ACTUAL_);\
    Slice expected = (_EXPECTED_);\
    if (!slice_equal(actual, expected)) {\
      check(false, "Expected %"PRIslice", got %"PRIslice,\
        SLICE_EXPAND_PRINTF(expected), SLICE_EXPAND_PRINTF(actual));\
    }\
  } while(0)

static Compilation test_compilation = {0};
static Execution_Context test_context = {0};
static Slice test_file_name = slice_literal_fields("_test_.mass");
static Module test_module = {0};
static Jit test_jit;

static inline void
test_init_module(
  Slice source
) {
  program_module_init(&test_module, test_file_name, source, test_context.scope);
  test_context.module = &test_module;
}

typedef enum {
  Test_Program_Source_Tag_Inline,
  Test_Program_Source_Tag_File,
} Test_Program_Source_Tag;

typedef struct {
  Test_Program_Source_Tag tag;
  union {
    Slice text;
    Slice path;
  };
} Test_Program_Source;

static inline Value *
test_program_source_base(
  const char *id,
  Execution_Context *context,
  Test_Program_Source source
) {
  Module *prelude_module = program_module_from_file(
    &test_context, slice_literal("std/prelude"), test_context.scope
  );
  test_context.module = prelude_module;
  Mass_Result result = program_import_module(&test_context, prelude_module);
  MASS_ON_ERROR(result) return 0;
  switch(source.tag) {
    case Test_Program_Source_Tag_Inline: {
      test_init_module(source.text);
      program_parse(context);
    } break;
    case Test_Program_Source_Tag_File: {
      Module *module = program_module_from_file(&test_context, source.path, test_context.scope);
      program_import_module(&test_context, module);
    } break;
  }
  MASS_ON_ERROR(*context->result) return 0;
  // FIXME lookup main in exported scope
  Value *value = scope_lookup_force(
    &test_context,test_context.module->own_scope, slice_from_c_string(id), &COMPILER_SOURCE_RANGE
  );
  if (value) {
    if (value->descriptor == &descriptor_overload_set) {
      const Overload_Set *set = storage_static_as_c_type(&value->storage, Overload_Set);
      assert(dyn_array_length(set->items) == 1);
      value = *dyn_array_get(set->items, 0);
    }
    if (value->descriptor == &descriptor_function_literal) {
      ensure_function_instance(context, value);
    }
  }
  return value;
}

fn_type_opaque
test_program_source_function(
  const char *function_id,
  Execution_Context *context,
  Test_Program_Source source
) {
  Value *value = test_program_source_base(function_id, context, source);
  MASS_ON_ERROR(*context->result) return 0;
  if (!value) return 0;
  jit_init(&test_jit, context->program);
  program_jit(&test_jit);
  MASS_ON_ERROR(*context->result) return 0;
  fn_type_opaque fn = value_as_function(test_jit.program, value);
  MASS_ON_ERROR(*context->result) return 0;
  return fn;
}

static Value *
test_program_inline_source_base(
  const char *id,
  Execution_Context *context,
  const char *source
) {
  return test_program_source_base(id, context, (Test_Program_Source) {
    .tag = Test_Program_Source_Tag_Inline,
    .text = slice_from_c_string(source),
  });
}

fn_type_opaque
test_program_inline_source_function(
  const char *function_id,
  Execution_Context *context,
  const char *source
) {
  return test_program_source_function(function_id, context, (Test_Program_Source) {
    .tag = Test_Program_Source_Tag_Inline,
    .text = slice_from_c_string(source),
  });
}

static Value *
test_program_external_source_base(
  const char *id,
  Execution_Context *context,
  const char *path
) {
  return test_program_source_base(id, context, (Test_Program_Source) {
    .tag = Test_Program_Source_Tag_File,
    .path = slice_from_c_string(path),
  });
}

fn_type_opaque
test_program_external_source_function(
  const char *function_id,
  Execution_Context *context,
  const char *path
) {
  return test_program_source_function(function_id, context, (Test_Program_Source) {
    .tag = Test_Program_Source_Tag_File,
    .path = slice_from_c_string(path),
  });
}


spec("source") {
  before_each() {
    compilation_init(&test_compilation, host_calling_convention());
    test_context = execution_context_from_compilation(&test_compilation);
  }

  after_each() {
    compilation_deinit(&test_compilation);
  }

  describe("Scope") {
    it("should be able to set and lookup values") {
      Value *test = value_from_s64(&test_context, 42, (Source_Range){0});
      Scope *root_scope = scope_make(test_context.allocator, 0);
      scope_define_value(root_scope, 0, (Source_Range){0}, slice_literal("test"), test);
      Scope_Entry *entry = scope_lookup(root_scope, slice_literal("test"));
      check(entry->tag == Scope_Entry_Tag_Value);
      check(entry->Value.value == test);
    }

    it("should be able to lookup things from parent scopes") {
      Value *global = value_from_s64(&test_context, 42, (Source_Range){0});
      Scope *root_scope = scope_make(test_context.allocator, 0);
      scope_define_value(root_scope, 0, (Source_Range){0}, slice_literal("global"), global);

      Value *level_1_test = value_from_s64(&test_context, 1, (Source_Range){0});
      Scope *scope_level_1 = scope_make(test_context.allocator, root_scope);
      scope_define_value(scope_level_1, 0, (Source_Range){0}, slice_literal("test"), level_1_test);

      Value *level_2_test = value_from_s64(&test_context, 1, (Source_Range){0});
      Scope *scope_level_2 = scope_make(test_context.allocator, scope_level_1);
      scope_define_value(scope_level_2, 0, (Source_Range){0}, slice_literal("test"),  level_2_test);

      Scope_Entry *entry =
        scope_lookup(scope_level_2, slice_literal("global"));
      check(entry->tag == Scope_Entry_Tag_Value);
      check(entry->Value.value == global);
    }
  }

  describe("Tokenizer") {
    it("should be able to tokenize an empty string") {
      Slice source = slice_literal("");

      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 0);
    }

    it("should be able to tokenize a comment") {
      Slice source = slice_literal("// foo\n");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 0);
    }

    it("should be able to turn newlines into fake semicolon tokens on top level") {
      Slice source = slice_literal("foo\n");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 2);
      Value *new_line = value_view_get(tokens, 1);
      check(value_is_symbol(new_line));
      spec_check_slice(value_as_symbol(new_line)->name, slice_literal(";"));
    }

    it("should be able to parse hex integers") {
      Slice source = slice_literal("0xCAFE");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(&token->source_range), slice_literal("0xCAFE"));
      check(token->descriptor == &descriptor_number_literal);
      check(token->storage.tag == Storage_Tag_Static);
      Number_Literal *literal = storage_static_as_c_type(&token->storage, Number_Literal);
      check(!literal->negative);
      check(literal->bits == 0xCAFE);
    }

    it("should be able to parse binary integers") {
      Slice source = slice_literal("0b100");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(&token->source_range), slice_literal("0b100"));
      check(token->descriptor == &descriptor_number_literal);
      check(token->storage.tag == Storage_Tag_Static);
      Number_Literal *literal = storage_static_as_c_type(&token->storage, Number_Literal);
      check(literal->bits == 0b100);
      check(!literal->negative);
    }

    it("should be able to tokenize a sum of integers") {
      Slice source = slice_literal("12 + foo123");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 3);

      Value *a_num = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(&a_num->source_range), slice_literal("12"));

      Value *plus = value_view_get(tokens, 1);
      check(value_is_symbol(plus));
      spec_check_slice(value_as_symbol(plus)->name, slice_literal("+"));

      Value *id = value_view_get(tokens, 2);
      check(value_is_symbol(id));
      spec_check_slice(source_from_source_range(&id->source_range), slice_literal("foo123"));
    }

    it("should be able to tokenize groups") {
      Slice source = slice_literal("(x)");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *paren = value_view_get(tokens, 0);
      check(value_is_group(paren));
      check(value_as_group(paren)->tag == Group_Tag_Paren);
      check(value_as_group(paren)->children.length == 1);
      spec_check_slice(source_from_source_range(&paren->source_range), slice_literal("(x)"));

      Value *id = value_view_get(value_as_group(paren)->children, 0);
      check(value_is_symbol(id));
    }

    it("should be able to tokenize strings") {
      Slice source = slice_literal("\"foo 123\"");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *string = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(&string->source_range), slice_literal("\"foo 123\""));
    }

    it("should be able to tokenize nested groups with different braces") {
      Slice source = slice_literal("{[]}");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *curly = value_view_get(tokens, 0);
      check(value_is_group(curly));
      check(value_as_group(curly)->tag == Group_Tag_Curly);
      check(value_as_group(curly)->children.length == 1);
      spec_check_slice(source_from_source_range(&curly->source_range), slice_literal("{[]}"));

      Value *square = value_view_get(value_as_group(curly)->children, 0);
      check(value_is_group(square));
      check(value_as_group(square)->tag == Group_Tag_Square);
      check(value_as_group(square)->children.length == 0);
      spec_check_slice(source_from_source_range(&square->source_range), slice_literal("[]"));
    }

    it("should be able to tokenize complex input") {
      Slice source = slice_literal(
        "foo :: fn(x: s8) -> {\n"
        "  return x + 3;\n"
        "}"
      );
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
    }

    it("should report a failure when encountering a brace that is not closed") {
      Slice source = slice_literal("(foo");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Unexpected_Token);
      spec_check_slice(error->source_range.file->path, test_file_name);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }

    it("should report a failure when encountering a mismatched brace that") {
      Slice source = slice_literal("(foo}");
      Value_View tokens;
      Mass_Result result =
        tokenize(test_context.compilation, &(Source_File){test_file_name, source}, &tokens);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Unexpected_Token);
      spec_check_slice(error->source_range.file->path, test_file_name);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }
  }

  #ifdef _WIN32
  describe("Raw Machine Code") {
    // This test relies on Windows calling convention
    it("should be able to include raw machine code bytes") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (result : s64) {"
          "inline_machine_code_bytes(0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00);"
          "result"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to reference a declared label in raw machine code bytes") {
      // TODO only run on X64 hosts
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) {"
          "label placeholder from_machine_code;"
          // "goto from_machine_code;"
          "inline_machine_code_bytes(0xE9, from_machine_code);"
          "return 10;"
          "label from_machine_code;"
          "42"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }
  #endif

  #if defined(_WIN32)
  describe("Win32: Structured Exceptions") {
    it("should be unwind stack on hardware exception on Windows") {
      fn_type_opaque checker = test_program_external_source_function(
        "main", &test_context, "fixtures\\error_runtime_divide_by_zero"
      );
      check(spec_check_mass_result(test_context.result));
      volatile bool caught_exception = false;
      __try {
        checker();
      }
      __except(EXCEPTION_EXECUTE_HANDLER) {
        caught_exception = true;
      }
      check(caught_exception);
    }
  }
  #endif

  describe("if / else") {
    it("should be able to parse and run if expression") {
      s64(*checker)(s32) = (s64(*)(s32))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : s32) -> (s64) {"
          "if x < 0 then 0 else 1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 1);
      check(checker(-2) == 0);
    }
    it("should work with a value instead of comparison as a condition") {
      s64(*is_zero)(s32) = (s64(*)(s32))test_program_inline_source_function(
        "is_zero", &test_context,
        "is_zero :: fn(x : s32) -> (s64) {"
          "if x then false else true"
        "}"
      );
      check(is_zero);
      check(is_zero(42) == false);
      check(is_zero(-2) == false);
      check(is_zero(0) == true);
    }
    it("should report an error on missing `then` inside of an if expression") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if true else 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
    }
    it("should report an error on double `then` inside of an if expression") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if true then 0 then 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
    }
    it("should be able to parse and run if statement") {
      s8(*checker)(s32) = (s8(*)(s32))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : s32) -> (s8) {\n"
          "if (x < 10) { return 0 }\n"
          "1\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 1);
      check(checker(-2) == 0);
    }
    it("should be able to parse and run if / else statement ") {
      s8(*checker)(s32) = (s8(*)(s32))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : s32) -> (s8) {\n"
          // FIXME figure out how to deal with a newline before `else`
          "if (x < 10) { return 0 } else { return 1 }\n"
          "0"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 1);
      check(checker(-2) == 0);
    }

    it("should report an error for an `if` statement without a body or a condition") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if; }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Undefined_Variable);
      spec_check_slice(error->Undefined_Variable.name, slice_literal(";"));
    }

    it("should report an error for an `if` statement with an incorrect condition") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if (1 < 0) { 0 } 42; }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
    }
  }

  describe("Functions") {
    it("should be able to parse and run a void -> void function") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> () { }"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }

    it("should be able to parse and run a void -> s64 function") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) { 42 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    #if defined(_WIN32) // TODO support on Linux
    it("should correctly deal with sign extending negative immediate integers") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) { -2147483647  }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == -INT64_C(2147483647));
    }

    it("should correctly deal with larger than INT32_MAX immediate integers") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (u64) { 2147483649  }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 2147483649llu);
    }

    it("should correctly deal with larger than INT64_MAX immediate integers") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (u64) { 18446744073709551615 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 18446744073709551615llu);
    }
    #endif

    it("should be able to parse and run a function with 5 arguments") {
      s8(*checker)(s8, s8, s8, s8, s8) = (s8(*)(s8, s8, s8, s8, s8))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8) -> (s8) { x5 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(1, 2, 3, 4, 5) == 5);
    }

    it("should correctly work with nested 5 argument calls") {
      s8(*checker)(s8, s8, s8, s8, s8) = (s8(*)(s8, s8, s8, s8, s8))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8) -> (s8) { bar(x1, x2, x3, x4, x5); x5 }\n"
        "bar :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8) -> () { x5 = 42; }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(1, 2, 3, 4, 5) == 5);
    }

    it("should correctly save volatile registers when calling other functions") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "outer", &test_context,
        "inner :: fn(x : s64) -> () { x = 21 };"
        "outer :: fn(x : s64) -> (s64) { inner(1); x }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker(42);
      check(actual == 42);
    }

    it("should be able to parse and run a s64 -> s64 function") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn(x : s64) -> (s64) { x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 42);
    }

    it("should be able to parse and run multiple function definitions") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "proxy", &test_context,
        "proxy :: fn() -> (s32) { plus(1, 2); plus(40, 2) }\n"
        "plus :: fn(x : s32, y : s32) -> (s32) { x + y }"
      );
      check(spec_check_mass_result(test_context.result));
      s32 answer = checker();
      check(answer == 42);
    }

    it("should be able to define a local function") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { local :: fn() -> (s64) { 42 }; local() }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker();
      check(answer == 42);
    }

    it("should be able to parse and run functions with overloads") {
      s64(*checker)(s32) = (s64(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "my_size_of :: fn(x : s32) -> (s64) { 4 }\n"
        "my_size_of :: fn(x : s64) -> (s64) { 8 }\n"
        "checker :: fn(x : s32) -> (s64) { my_size_of(x) }\n"
      );
      check(spec_check_mass_result(test_context.result));
      s64 size = checker(0);
      check(size == 4);
    }

    // TODO support either
    //  * lookup in parents and merging in of that overload set
    //  * some way to manually merge it in via smth like
    //    `my_size_of :: @scope.parent.my_size_of | fn(x : s64) -> (s64) { 8 }`
    xit("should be able to parse and run functions with local overloads") {
      s64(*checker)(s32) = (s64(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "my_size_of :: fn(x : s32) -> (s64) { 4 }\n"
        "checker :: fn(x : s32) -> (s64) {\n"
          "my_size_of :: fn(x : s64) -> (s64) { 8 }\n"
          "my_size_of(x)\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      s64 size = checker(0);
      check(size == 4);
    }

    it("should report an overload overlap") {
      test_program_inline_source_base(
        "test", &test_context,
        "overload :: fn(x : s64, y : s32) -> () { }\n"
        "overload :: fn(x : s32, y : s64) -> () { }\n"
        "test :: fn() -> () { overload(1, 2) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Undecidable_Overload);
    }

    it("should support default arguments") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test_default :: fn(x : s64, y : s64 = 20) -> (s64) { x + y }\n"
        "test :: fn() -> (s64) { (test_default(20) + \n test_default(0, 2)) }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 42);
    }

    it("should support default arguments that refer to previous arguments") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test_default :: fn(x : s64, y : s64 = x) -> (s64) { x + y }\n"
        "test :: fn() -> (s64) { (test_default(20) + \n test_default(0, 2)) }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 42);
    }

    it("should support default arguments with inference") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test_default :: fn(x : s64, y := 20) -> (s64) { x + y }\n"
        "test :: fn() -> (s64) { (test_default(20) + \n test_default(0, 2)) }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 42);
    }

    it("should disallow default arguments coming after non-default ones") {
      test_program_inline_source_function(
        "test", &test_context,
        "test :: fn(x : s64, y : s64 = x, z : s32) -> () {}\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Non_Trailing_Default_Argument);
    }

    it("should support capturing static arguments") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: { ANSWER :: 42; fn() -> (s64) { ANSWER } }\n"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 42);
    }

    it("should be able to have an explicit return") {
      s32(*checker)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : s32) -> (s32) { if x > 0 { return x }; 0 }"
      );
      check(spec_check_mass_result(test_context.result));
      s32 actual = checker(42);
      check(actual == 42);
    }

    it("should be able to refer to the current scope") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s32) {"
          "result : s32 = 42;"
          "this_scope :: @scope;"
          "this_scope.result"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to run fibonnacii") {
      s64(*fibonnacci)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "fibonnacci", &test_context,
        "fibonnacci :: fn(n : s64) -> (s64) {\n"
          "if (n < 2) { return n }\n"
          // FIXME Inline sum does not work on Linux because of some problem
          //       with register release
          // "fibonnacci(n - 1) + fibonnacci(n - 2)"
          "n1 := fibonnacci(n - 1)\n"
          "n2 := fibonnacci(n - 2)\n"
          "n1 + n2"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      check(fibonnacci(0) == 0);
      check(fibonnacci(1) == 1);
      check(fibonnacci(10) == 55);
    }

    it("should allow non-parenthesized return type") {
      s32(*checker)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x: s32) -> s32 { x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 42);
    }

    it("should report an error when encountering multiple return types") {
      test_program_inline_source_base(
        "exit", &test_context,
        "exit :: fn(status: s32) -> (s32, s32) {}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Unimplemented);
    }

    it("should report an error when non-type id is being used as a type") {
      test_program_inline_source_base(
        "main", &test_context,
        "foo :: fn() -> () {};"
        "main :: fn(arg : foo) -> () {}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_type);
      check(error->Type_Mismatch.actual == &descriptor_function_literal);
    }

    it("should report an error when encountering an unknown type") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn(status : s33) -> () {}"
      );
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Undefined_Variable);
      spec_check_slice(error->Undefined_Variable.name, slice_literal("s33"));
    }

    it("should be able to get the type_of an expression without evaluating it") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "counter := 0\n"
        "checker :: fn() -> (s64) {"
          "type_of({ counter = 1; var := 0; var })\n"
          "counter"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 0);
    }

    it("should be able to get the fields of the descriptor provided from type_of") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo := 0\n"
        "id :: fn(x : type_of(foo)) -> (type_of(foo)) { x }\n"
        "checker :: fn() -> (s64) {"
          "x : s64 = 42\n"
          "id(x)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when non-type token is being used as a type") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () {"
          "type_of();"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_No_Matching_Overload);
    }

    it("should be able to get the size_of an expression") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "size_of({ var := 0; var })"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 8);
    }

    it("should be able to get the size_of an expression without evaluating it") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "counter := 0\n"
        "checker :: fn() -> (s64) {"
          "size_of({ counter = 1; var := 0; var })\n"
          "counter"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 0);
    }

    it("should support defining a compile-time function") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "compile_time_fn :: @fn() -> (s64) { 42 }\n"
        "checker :: fn() -> (s64) { compile_time_fn() }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to accept a function as an argument and call it") {
      s64 (*checker)(Spec_Callback foo) =
        (s64 (*)(Spec_Callback))test_program_inline_source_function(
          "foo", &test_context,
          "foo :: fn(callback : fn() -> (s64)) -> (s64) { callback() }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker(spec_callback) == 42);
    }

    it("should be able to have user-defined intrinsics") {
      s64 (*checker)(Spec_Callback foo) =
        (s64 (*)(Spec_Callback))test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: @intrinsic { \\2 }\n"
          "checker :: fn() -> (s64) { my_intrinsic(1, 2) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker(spec_callback) == 2);
    }

    it("should choose non-intrinsic overload if one is available") {
      s64 (*checker)(Spec_Callback foo) =
        (s64 (*)(Spec_Callback))test_program_inline_source_function(
          "checker", &test_context,
          "foo :: fn (x : s64, y : s64) -> s64 { 42 }\n"
          "foo :: @intrinsic { \\2 }\n"
          "checker :: fn() -> (s64) { foo(1, 2) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker(spec_callback) == 42);
    }
  }

  describe("Assignment") {
    it("should report type mismatch when assigning") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> () { x : s32 = 0; y : s64 = 1; x = y; }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_s32);
      check(error->Type_Mismatch.actual == &descriptor_s64);
    }
    it("should report an error when LHS of the := is not a symbol") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { 2 := 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Invalid_Identifier);
    }
    it("should report an error for multi-definition assignment") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { foo, bar := 42, 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Unimplemented);
    }
    it("should be able to define, assign and lookup an s64 variable on the stack") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) { y : s8; y = 10; x := 21; x = 32; x + y }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should be able to assign to a void value") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) { () = 10; 42 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should be able to parse typed definition and assignment in the same statement") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s32) {"
          "result : s32 = 42;"
          "result"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("Math") {
    #define MATH_CHECKER_FN(LEFT_TYPE, RIGHT_TYPE, OPERATOR)\
      LEFT_TYPE(*checker)(LEFT_TYPE, RIGHT_TYPE) = \
        (LEFT_TYPE(*)(LEFT_TYPE, RIGHT_TYPE))test_program_inline_source_function(\
          "test", &test_context,\
          "test :: fn(x : " #LEFT_TYPE ", y : " #RIGHT_TYPE ") -> ("#LEFT_TYPE") { x " #OPERATOR " y }"\
        )
    describe("division") {
      #if 0
      it("debug math witout a macro") {
        MATH_CHECKER_FN(u8, u8, /);
        check(spec_check_mass_result(test_context.result));
        check(checker(10u, 3u) == 10u / 3u);
        check(checker(UINT8_MAX, 3u) == UINT8_MAX / 3u);
      }
      #endif

      #define MATCH_CHECK_UNSIGNED_DIVIDE_AND_REMAINDER(BITS)\
        it("should correctly handle u"#BITS" divide") {\
          MATH_CHECKER_FN(u##BITS, u##BITS, /);\
          check(spec_check_mass_result(test_context.result));\
          check(checker(10u, 3u) == 10u / 3u);\
          check(checker(UINT##BITS##_MAX, 3u) == UINT##BITS##_MAX / 3u);\
        }\
        it("should correctly handle u"#BITS" remainder") {\
          MATH_CHECKER_FN(u##BITS, u##BITS, %);\
          check(spec_check_mass_result(test_context.result));\
          check(checker(10u, 3u) == 10u % 3u);\
          check(checker(13u, 10u) == 13u % 10u);\
          check(checker(UINT##BITS##_MAX, 3u) == UINT##BITS##_MAX % 3u);\
          check(checker(3u, UINT##BITS##_MAX) == 3u % UINT##BITS##_MAX);\
        }

      MATCH_CHECK_UNSIGNED_DIVIDE_AND_REMAINDER(8)
      MATCH_CHECK_UNSIGNED_DIVIDE_AND_REMAINDER(16)
      MATCH_CHECK_UNSIGNED_DIVIDE_AND_REMAINDER(32)
      MATCH_CHECK_UNSIGNED_DIVIDE_AND_REMAINDER(64)

      #define MATCH_CHECK_SIGNED_DIVIDE_AND_REMAINDER(BITS)\
        it("should correctly handle s"#BITS" divide") {\
          MATH_CHECKER_FN(s##BITS, s##BITS, /);\
          check(spec_check_mass_result(test_context.result));\
          check(checker(10, 3) == 10 / 3);\
          check(checker(10, -3) == 10 / -3);\
          check(checker(INT##BITS##_MAX, 3) == INT##BITS##_MAX / 3);\
          check(checker(INT##BITS##_MIN, -3) == INT##BITS##_MIN / -3);\
        }\
        it("should correctly handle s"#BITS" remainder") {\
          MATH_CHECKER_FN(s##BITS, s##BITS, %);\
          check(spec_check_mass_result(test_context.result));\
          check(checker(10, 3) == 10 % 3);\
          check(checker(10, -3) == 10 % -3);\
          check(checker(13, 10) == 13 % 10);\
          check(checker(-13, 10) == -13 % 10);\
          check(checker(INT##BITS##_MAX, 3) == INT##BITS##_MAX % 3);\
          check(checker(INT##BITS##_MIN, -3) == INT##BITS##_MIN % -3);\
        }

      MATCH_CHECK_SIGNED_DIVIDE_AND_REMAINDER(8)
      MATCH_CHECK_SIGNED_DIVIDE_AND_REMAINDER(16)
      MATCH_CHECK_SIGNED_DIVIDE_AND_REMAINDER(32)
      MATCH_CHECK_SIGNED_DIVIDE_AND_REMAINDER(64)
    }

    describe("multiplication") {
      it("should correctly handle s8 multiplication") {
        MATH_CHECKER_FN(s8, s8, *);
        check(spec_check_mass_result(test_context.result));
        check(checker(-30, 3) == -30 * 3);
      }
      it("should correctly handle u8 multiplication") {
        MATH_CHECKER_FN(u8, u8, *);
        check(spec_check_mass_result(test_context.result));
        check(checker(128u, 3u) == 128u);
      }
    }

    #if defined(_WIN32) // TODO support on Linux
    it("should have an add function") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { x := 40; add(x, 2) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should have a subtract function") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { subtract(cast(s64, 44), 2) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should have a multiply function") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { multiply(cast(s64, 21), 2) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should have a divide function") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { divide(cast(s64, 84), 2) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should have a remainder function") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { remainder(cast(s64, 142), 100) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    #endif
  }

  describe("Operators") {
    it("should be able to use prefix backslash operator for reflection") {
      Descriptor_Tag(*checker)() = (Descriptor_Tag(*)())test_program_inline_source_function(
        "checker", &test_context,
        "mass :: import(\"mass\")\n"
        "checker :: {\n"
          "mass :: import(\"mass\")\n"
          "fn() -> (mass.Descriptor_Tag._Type) { (\\42).descriptor.tag }\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == Descriptor_Tag_Struct);
    }

    it("should be able to use postfix backslash operator for reification") {
        s64(*checker)() = (s64(*)())test_program_inline_source_function(
            "checker", &test_context,
            "checker :: fn() -> (s64) { \\42\\ }"
        );
        check(spec_check_mass_result(test_context.result));
        check(checker() == 42);
    }

    it("should be able to parse and run a triple plus function") {
      s64(*checker)(s64, s64, s64) = (s64(*)(s64, s64, s64))test_program_inline_source_function(
        "plus", &test_context,
        "plus :: fn(x : s64, y : s64, z : s64) -> (s64) { x + y + z }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(30, 10, 2) == 42);
    }

    #if defined(_WIN32) // TODO support on Linux
    it("should be able to parse and run a subtraction of a negative literal") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "plus_one", &test_context,
        "plus_one :: fn(x : s64) -> (s64) { x - -1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(41) == 42);
    }
    #endif

    it("should be able to parse and run a sum passed to another function as an argument") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "plus", &test_context,
        "id :: fn(ignored : s64, x : s64) -> (s64) { x }\n"
        "plus :: fn() -> (s64) { x : s64 = 40; y : s64 = 2; id(0, x + y) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be possible to define infix operators") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "operator 18 (++ x) { x = x + 1; x };"
        "test :: fn() -> (s64) { y := 41; ++y }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be possible to define postfix operators") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "operator 18 (x ++) { result := x; x = x + 1; result };"
        "test :: fn() -> (s64) { y := 42; y++ }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be possible to define an overloaded prefix and postfix operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "operator 18 (++ x) { x = x + 1; x };"
        "operator 19 (x ++) { result := x; x = x + 1; result };"
        "test :: fn() -> (s64) { y := 41; ++y++ }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be possible to define an overloaded prefix and infix operator") {
      s64(*checker)(s64, s64) = (s64(*)(s64, s64))test_program_inline_source_function(
        "test", &test_context,
        "operator 18 (++ x) { x = x + 1; x };"
        "operator 14 (x ++ y) { x + y };"
        "test :: fn(x: s64, y: s64) -> (s64) { x ++ ++y }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(40, 1) == 42);
    }

    it("should be possible to define infix operators") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "operator 15 (x ** y) { x * y };"
        "test :: fn() -> (s64) { 21 ** 2 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when defining an overloaded infix operator") {
      test_program_inline_source_base(
        "dummy", &test_context,
        "operator 15 (x ** y) { x * y };"
        "operator 15 (x ** y) { x * y };"
        "dummy :: fn() -> (s64) { 21 ** 2 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Operator_Infix_Suffix_Conflict);
      spec_check_slice(error->Operator_Infix_Suffix_Conflict.symbol, slice_literal("**"));
    }

    it("should report an error when defining an overloaded infix and postfix operator") {
      test_program_inline_source_base(
        "dummy", &test_context,
        "operator 15 (x ** y) { x * y };"
        "operator 15 (x **) { x * x };"
        "dummy :: fn() -> (s64) { 21 ** 2 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Operator_Infix_Suffix_Conflict);
      spec_check_slice(error->Operator_Infix_Suffix_Conflict.symbol, slice_literal("**"));
    }

    #if defined(_WIN32) // TODO support on Linux
    xit("should have a built-in compile-time shift operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 2 << 1 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 4);
    }

    xit("should have a built-in compile-time bitwise and operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 0b110 & 0b011 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 0b10);
    }

    xit("should have correctly handle the difference between addressof and bitwise and operators") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (&s64) { x := 0 & 1; &x }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual);
    }

    xit("should have a built-in compile-time bitwise or operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 0b110 | 0b011 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 0b111);
    }
    #endif
  }

  describe("Compile Time Execution") {
    it("should correctly detect direct circular dependency in static declarations") {
      test_program_inline_source_base(
        "LOOP", &test_context,
        "LOOP :: LOOP"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Circular_Dependency);
    }
    it("should correctly detect indirect circular dependency in static declarations") {
      test_program_inline_source_base(
        "FOO", &test_context,
        "FOO :: BAR\n"
        "BAR :: FOO"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Circular_Dependency);
    }
    it("should be able to call a function at compile time") {
      Value *status = test_program_inline_source_base(
        "STATUS_CODE", &test_context,
        "STATUS_CODE :: the_answer();"
        "the_answer :: fn() -> (s8) { 42 }"
      );

      check(status);
      check(descriptor_is_integer(status->descriptor));
      check(status->storage.tag == Storage_Tag_Static);
      check(status->storage.byte_size == 1);
      check(*storage_static_as_c_type(&status->storage, s8) == 42);
    }

    it("should be able to to do nested compile time calls") {
      Value *result = test_program_inline_source_base(
        "RESULT", &test_context,
        "RESULT :: get_a();"
        "B :: get_b();"
        "get_a :: fn() -> (s8) { B };"
        "get_b :: fn() -> (s8) { 42 }"
      );

      check(result);
      check(descriptor_is_integer(result->descriptor));
      check(result->storage.tag == Storage_Tag_Static);
      check(result->storage.byte_size == 1);
      check(*storage_static_as_c_type(&result->storage, s8) == 42);
    }

    it("should support compile time blocks") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { @{ 20 + 22 } }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when trying to access the context at runtime") {
      test_program_inline_source_base(
        "checker", &test_context,
        "checker :: fn() -> () { @context; }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Epoch_Mismatch);
    }

    it("should be able to use if / else statement at the top level") {
      test_program_inline_source_base(
        "DUMMY", &test_context,
        "do_stuff :: fn() -> () {}\n"
        "if 1 { do_stuff() }\n"
        "DUMMY :: 42"
      );
      check(test_context.result->tag == Mass_Result_Tag_Success);
    }

    it("should be able to use if / else to choose an implementation at compile time") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "CONDITION :: 1\n"
        "TEST :: if CONDITION then 42 else 1000\n"
      );

      check(value);
      check(value->descriptor == &descriptor_number_literal);
      check(value->storage.tag == Storage_Tag_Static);
      Number_Literal *literal = storage_static_as_c_type(&value->storage, Number_Literal);
      check(literal->bits == 42);
      check(literal->negative == false);
    }

    it("should be able combine if / else, @scope and using for conditional definitions") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "CONDITION :: 1\n"
        "using if CONDITION then { TEST :: 42; @scope } else { TEST :: 1000; @scope }\n"
      );

      check(value);
      check(value->descriptor == &descriptor_number_literal);
      check(value->storage.tag == Storage_Tag_Static);
      Number_Literal *literal = storage_static_as_c_type(&value->storage, Number_Literal);
      check(literal->bits == 42);
      check(literal->negative == false);
    }

    it("should not be able to use runtime values in a static context (when not causing force)") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> (s64) { foo := 42; bar := foo; @( foo ) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Epoch_Mismatch);
    }

    it("should not be able to use runtime values in a static context inside a macro") {
      test_program_inline_source_base(
        "test", &test_context,
        "comptime :: macro(x : s64) -> (s64) { @( x ) }\n"
        "test :: fn() -> (s64) { foo := 42; comptime(foo) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Epoch_Mismatch);
    }

    it("should support compile-time arithmetic operations") {
      s64 (*checker)() = (s64 (*)())test_program_inline_source_function(
        "checker", &test_context,
        "RESULT :: 40 + 1 + 1\n"
        "checker :: fn() -> (s64) { RESULT }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("Macro") {
    it("should be able to parse and run macro id function") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "id :: macro(x : s64) -> (s64) { x }\n"
        "test :: fn() -> (s64) { id(42) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to parse and run macro id fn with an explicit return and an immediate arg") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "id :: macro(x : s64) -> (s64) { if (x > 0) { return 20 }; x }\n"
        "test :: fn() -> (s64) { id(42) + 1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 21);
    }

    it("should allow changes to the passed arguments to macro function") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "process :: macro(y : s64) -> () { y = 42; }\n"
        "test :: fn() -> (s64) { x := 20; process(x); x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to define and use a syntax macro without a capture") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "syntax (\"the\" \"answer\") 42;"
        "checker :: fn() -> (s32) { the answer }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to define and use a syntax macro matching a curly brace block") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "syntax statement (\"block\" {}@body) body();"
        "checker :: fn() -> (s64) { result := 20; block { result = 42 }; result }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to define and use a syntax macro matching a sequence at the end") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "syntax statement (\"comment\" ..@ignore);"
        "checker :: fn() -> (s64) { x := 42; comment x = x + 1; x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to define and use a syntax macro with a capture") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        // TODO figure out what should be the rule for subtracting unsigned integers
        "syntax (\"negative\" .@x) (cast(s32, 0) - x());"
        "checker :: fn() -> (s32) { negative 42 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == -42);
    }

    it("should be able to use a while loop") {
      s32(*sum_up_to)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "sum_up_to", &test_context,
        "sum_up_to :: fn(x : s32) -> (s32) {"
          "sum : s32;"
          "sum = 0;"
          "while (x >= 0) {"
            "sum = sum + x;"
            "x = x - 1;"
          "};"
          "sum"
        "}"
      );
      check(sum_up_to);
      check(sum_up_to(0) == 0);
      check(sum_up_to(1) == 1);
      check(sum_up_to(2) == 3);
      check(sum_up_to(3) == 6);
    }

    it("should be able to use `break` statement inside of the while loop") {
      s32(*checker)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : s32) -> (s32) {"
          "sum : s32;"
          "sum = 0;"
          "while (x >= 0) {"
            "break;"
            "sum = sum + x;"
            "x = x - 1;"
          "};"
          "sum"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(0) == 0);
      check(checker(1) == 0);
    }

    it("should be able to use `continue` statement inside of the while loop") {
      s32(*checker)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : s32) -> (s32) {"
          "sum : s32;"
          "sum = 0;"
          "while (x >= 0) {"
            "sum = sum + x;"
            "x = x - 1;"
            "continue;"
          "};"
          "sum"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(0) == 0);
      check(checker(1) == 1);
      check(checker(2) == 3);
    }

    it("should be able to run a long compile-time loop") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : s64) -> (s64) {\n"
          "i := 0\n"
          "while i < 1000000 { i = i + 1 }\n"
          "i\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 1000000);
    }

    it("should report an error for macro external functions") {
      test_program_inline_source_base(
        "test", &test_context,
        "ExitProcess :: macro(x : s64) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")\n"
        "test :: fn() -> (s64) { ExitProcess(42) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }

    it("should be able to parse and run macro id function at compile time") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "FOO :: 42\n"
        "id :: macro(x : s64) -> (s64) { x }\n"
        "BAR :: id(FOO)\n"
        "test :: fn() -> (s64) { BAR }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("labels / goto") {
    it("should be able to parse and run a program with labels and goto") {
      s32(*sum_up_to)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "sum_up_to", &test_context,
        "sum_up_to :: fn(x : s32) -> (s32) {"
          "sum : s32;"
          "sum = 0;"
          "label loop;"
          "if (x < 0) { return sum };"
          "sum = sum + x;"
          "x = x - 1;"
          "goto loop;"
          // FIXME This return is never reached and ideally should not be required
          //       but currently there is no way to track dead branches
          "sum"
        "}"
      );
      check(sum_up_to);
      check(sum_up_to(0) == 0);
      check(sum_up_to(1) == 1);
      check(sum_up_to(2) == 3);
      check(sum_up_to(3) == 6);
    }

    it("should be able to goto a label defined after the goto") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s32) {"
          "x : s32 = 42;"
          "label placeholder skip;"
          "goto skip;"
          "x = 0;"
          "label skip;"
          "x"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when encountering wrong type of label identifier") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn(status: s32) -> () { x : s32; goto x; }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }
  }

  describe("Strings") {
    it("should parse and return C-compatible strings") {
      const char *(*checker)(void) = (const char *(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (&u8) { c_string(\"test\") }"
      );
      check(spec_check_mass_result(test_context.result));
      const char *string = checker();
      check(strcmp(string, "test") == 0);
    }
    it("should accept string arguments") {
      const char *(*checker)(Slice) = (const char *(*)(Slice))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(string : String) -> (&u8) {\n"
          "string.bytes\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      Slice input = slice_literal("foo");
      const char *result = checker(input);
      check(result == input.bytes);
    }
    it("should be able to return a string") {
      Slice(*checker)(void) = (Slice(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (String) {\n"
          "\"foo\""
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      Slice result = checker();
      check(slice_equal(result, slice_literal("foo")));
    }
  }

  describe("Fixed Size Arrays") {
    it("should be able to define a variable with a fixed size array type") {
      s8(*checker)(void) = (s8(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s8) {"
          "foo : s8 * 64;"
          "foo.0 = 42;"
          "foo.0"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      u8 actual = checker();
      check(actual == 42);
    }

    it("should report an error when fixed size array size does not resolve to an integer") {
      test_program_inline_source_base(
        "test", &test_context,
        "BAR :: \"foo\"; "
        "test :: fn() -> () {"
          "foo : s8 * BAR"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.actual == &descriptor_slice);
    }

    #if defined(_WIN32) // TODO support on Linux
    it("should report an error when fixed size array size is negative") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> () {"
          "foo : s8 * -42"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
    }
    #endif
  }

  describe("User-defined Types") {
    it("should be able to parse fixed-bit sized type definitions") {
      fn_type_opaque checker = test_program_inline_source_function(
        "test", &test_context,
        "int8 :: bit_type(8);"
        "test :: fn() -> () {"
          "x : int8;"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }

    it("should be able to parse struct definitions") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct({ x : s32; y : s32; });"
        "test :: fn() -> (s32) {"
          "p : Point; p.x = 20; p.y = 22;"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when field name is not an identifier") {
      test_program_inline_source_base(
        "main", &test_context,
        "Point :: c_struct({ x : s32; y : s32; });"
        "main :: fn() -> () { p : Point; p.(x) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Invalid_Identifier);
    }

    it("should report an error when a struct does not have the requested field") {
      test_program_inline_source_base(
        "main", &test_context,
        "Point :: c_struct({ x : s32; y : s32; });"
        "main :: fn() -> () { p : Point; p.foo }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Unknown_Field);
      spec_check_slice(error->Unknown_Field.name, slice_literal("foo"));
    }

    it("should auto-dereference pointers to struct on field access") {
      s64(*checker)(Test_128bit*) = (s64(*)(Test_128bit*))test_program_inline_source_function(
        "checker", &test_context,
        "Test_128bit :: c_struct({ x : s64; y : s64 })\n"
        "checker :: fn(input : &Test_128bit) -> (s64) {\n"
          "input.y\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = {.x = 21, .y = 42};
      check(checker(&test_128bit));
    }

    it("should be able to return structs while accepting other arguments") {
      Test_128bit(*checker)(s64) = (Test_128bit(*)(s64))test_program_inline_source_function(
        "return_struct", &test_context,
        "Test_128bit :: c_struct({ x : s64; y : s64 });"
        "return_struct :: fn(x : s64) -> (Test_128bit) {"
          "result : Test_128bit;"
          "result.x = x;"
          "result.y = x / 2;"
          "result"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = checker(42);
      check(test_128bit.x == 42);
      check(test_128bit.y == 21);
    }

    it("should correctly handle struct argument fields as arguments to another call") {
      s64(*checker)(Test_128bit) = (s64(*)(Test_128bit))test_program_inline_source_function(
        "checker", &test_context,
        "Test_128bit :: c_struct({ x : s64; y : s64 });"
        "test_sum :: fn(x : s64, y : s64) -> (s64) { x + y }\n"
        "checker :: fn(x : Test_128bit) -> (s64) {"
          "test_sum(x.x, x.y)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = { .x = 20, .y = 22 };
      check(checker(test_128bit) == 42);
    }

    // Both System_V and win64 will pass 7th argument on the stack
    xit("should be able to use a larger-than-register struct passed as the 7th arguments") {
      s8(*checker)(s8, s8, s8, s8, s8, s8, Test_128bit) =
        (s8(*)(s8, s8, s8, s8, s8, s8, Test_128bit))test_program_inline_source_function(
          "foo", &test_context,
          "Test_128bit :: c_struct({ x : s64; y : s64 })\n"
          "foo :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8, x6 : s8, x7 : Test_128bit ) -> (s64) { x7.x }"
        );
      check(spec_check_mass_result(test_context.result));
      Test_128bit test_128bit = { .x = 42, .y = 20 };
      check(checker(1, 2, 3, 4, 5, 6, test_128bit) == 42);
    }
  }

  describe("Unsigned Integers") {
    it("should be able to return unsigned integer literals") {
      u8(*checker)(void) = (u8(*)(void))test_program_inline_source_function(
        "return_200", &test_context,
        "return_200 :: fn() -> (u8) { 200 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 200);
    }

    it("should use correct EFLAGS values when dealing with unsigned integers") {
      u8(*checker)(void) = (u8(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s8) { x : u8 = 200; x < 0 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == false);
    }
  }

  describe("Signed Integers") {
    it("should parse and correctly deal with 16 bit values") {
      s16(*checker)(s16) = (s16(*)(s16))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : s16) -> (s16) { x + 1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(8) == 9);
    }
  }


  describe("Modules") {
    it("should support importing modules") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "sample_module :: import(\"fixtures/sample_module\")\n"
        "checker :: fn() -> (s32) { sample_module.the_answer }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    // TODO somehow test that modules are only imported once
    it("should support importing the same module multiple times") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "A :: import(\"fixtures/foo/../sample_module\")\n"
        "B :: import(\"fixtures\\\\sample_module\")\n"
        "checker :: fn() -> (s64) { A.the_answer + B.the_answer }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 84);
    }
    it("should support importing compiler functionality through `mass` module") {
      Value *value = test_program_inline_source_base(
        "Infix", &test_context,
        "Infix :: { mass :: import(\"mass\") ; mass.Operator_Fixity.Infix }"
      );
      check(value);
      check(value->descriptor == &descriptor_operator_fixity);
      check(value->storage.tag == Storage_Tag_Static);
      s32 item_value = *storage_static_as_c_type(&value->storage, s32);
      check(item_value == Operator_Fixity_Infix);
    }
    it("should support failing the compilation by calling fail() at compile time") {
     test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() { fail(@context, \"fail()\", \"Oops\") }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_User_Defined);
      spec_check_slice(error->User_Defined.name, slice_literal("fail()"));
      spec_check_slice(error->detailed_message, slice_literal("Oops"));
    }
    it("should support static_assert") {
      test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() { static_assert(false, \"Oops\") }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_User_Defined);
      spec_check_slice(error->User_Defined.name, slice_literal("Static Assert Failed"));
      spec_check_slice(error->detailed_message, slice_literal("Oops"));
    }
  }

  describe("PE32 Executables") {
    #if defined(_WIN32) // TODO support on Linux
    it("should parse and write out an executable that exits with status code 42") {
      Program *test_program = test_context.program;
      test_program->default_calling_convention = &calling_convention_x86_64_windows;
      test_program->entry_point = test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { ExitProcess(42) }\n"
        "ExitProcess :: fn(status : s32) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")"
      );
      check(spec_check_mass_result(test_context.result));
      write_executable("build/test_parsed.exe", &test_context, Executable_Type_Cli);
    }

    it("should parse and write an executable that prints Hello, world!") {
      Program *test_program = test_context.program;
      test_program->default_calling_convention = &calling_convention_x86_64_windows;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "fixtures/hello_world"
      );
      check(spec_check_mass_result(test_context.result));
      write_executable("build/hello_world.exe", &test_context, Executable_Type_Cli);
    }

    xit("should parse and write an executable with a lot of constant folding") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "..\\compile-time-benchmark\\folding"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_context, test_program->entry_point);
      check(spec_check_mass_result(test_context.result));
      write_executable("build\\folding.exe", &test_context, Executable_Type_Cli);
    }

    xit("should parse and write an executable with a lot of print statements") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "..\\compile-time-benchmark\\print"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_context, test_program->entry_point);
      check(spec_check_mass_result(test_context.result));
      write_executable("build\\print.exe", &test_context, Executable_Type_Cli);
    }
    #endif
  }

  describe("Relocations") {
    #if defined(_WIN32) // TODO support on Linux
    it("should work in JIT code") {
      fn_type_opaque checker = test_program_external_source_function(
        "test", &test_context, "fixtures/relocations"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }

    it("should work in an executable") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "fixtures/relocations"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_context, test_program->entry_point);
      check(spec_check_mass_result(test_context.result));
      write_executable("build/relocations.exe", &test_context, Executable_Type_Cli);
    }
    #endif
  }

  describe("Complex Examples") {
    #if defined(_WIN32) // TODO support on Linux
    it("should be able to run fizz buzz") {
      fn_type_opaque fizz_buzz =
        test_program_external_source_function("fizz_buzz", &test_context, "fixtures/fizz_buzz");
      check(spec_check_mass_result(test_context.result));
      check(fizz_buzz);
      fizz_buzz();
    }
    #endif
  }
}
