#include "bdd-for-c.h"

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"
#include "program.c"

typedef struct {
  s64 x;
  s64 y;
} Test_128bit;

static bool
spec_check_mass_result_internal(
  Compilation *compilation,
  const Mass_Result *result
) {
  if (result->tag == Mass_Result_Tag_Success) return true;
  const Mass_Error *error = &result->Error.error;
  Fixed_Buffer *error_buffer = mass_error_to_string(compilation, error);
  slice_print(fixed_buffer_as_slice(error_buffer));
  fixed_buffer_destroy(error_buffer);
  printf("\n  at ");
  source_range_print_start_position(compilation, &result->Error.error.source_range);
  return false;
}

#define spec_check_mass_result(_RESULT_)\
  spec_check_mass_result_internal(test_context.compilation, (_RESULT_))

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

static const Slice test_file_name = slice_literal_fields("_test_.mass");

typedef enum {
  Test_Program_Source_Tag_Inline,
  Test_Program_Source_Tag_File,
} Test_Program_Source_Tag;

typedef struct {
  Test_Program_Source_Tag tag;
  union {
    const char *text;
    Slice path;
  };
} Test_Program_Source;

static Source_Range
test_inline_source_range(
  Compilation *compilation,
  const char *source
) {
  Slice text = slice_from_c_string(source);
  Source_File *file = allocator_allocate(compilation->allocator, Source_File);
  *file = (Source_File) {
    .path = test_file_name,
    .text = text,
  };
  return (Source_Range) {
    .file = file,
    .offsets = {.from = 0, .to = u64_to_u32(text.length), },
  };
}

static inline Value *
test_program_source_base(
  const char *id,
  Execution_Context *context,
  Test_Program_Source source
) {
  Mass_Result result =
    program_load_file_module_into_root_scope(context, slice_literal("std/prelude"));
  MASS_ON_ERROR(result) return 0;

  Module *test_module = 0;
  switch(source.tag) {
    case Test_Program_Source_Tag_Inline: {
      test_module = allocator_allocate(context->allocator, Module);
      Source_Range source_range = test_inline_source_range(context->compilation, source.text);
      *test_module = (Module) {
        .source_range = source_range,
        .own_scope = context->scope,
      };
    } break;
    case Test_Program_Source_Tag_File: {
      test_module = program_module_from_file(context, source.path, context->scope);
    } break;
  }
  program_import_module(context, test_module);
  MASS_ON_ERROR(*context->result) return 0;
  Slice id_slice = slice_from_c_string(id);
  Source_Range symbol_source_range;
  INIT_LITERAL_SOURCE_RANGE(&symbol_source_range, "__test_symbol__");
  const Symbol *symbol = mass_ensure_symbol(context->compilation, id_slice);
  Value *value = scope_lookup_force(
    context, test_module->own_scope, symbol, &symbol_source_range
  );
  if (value) {
    if (value->descriptor == &descriptor_overload_set) {
      const Overload_Set *set = storage_static_as_c_type(&value->storage, Overload_Set);
      assert(dyn_array_length(set->items) == 1);
      value = *dyn_array_get(set->items, 0);
    }
    if (value->descriptor == &descriptor_function_literal) {
      ensure_function_instance(context->compilation, context->program, value, (Value_View){0});
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
  static Jit test_jit;
  jit_init(&test_jit, context->program);
  Mass_Result jit_result = program_jit(context->compilation, &test_jit);
  MASS_ON_ERROR(jit_result) {
    context_error(context, jit_result.Error.error);
    return 0;
  }
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
    .text = source,
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
    .text = source,
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

static fn_type_opaque
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
  static Compilation test_compilation = {0};
  static Execution_Context test_context = {0};

  before_each() {
    compilation_init(&test_compilation, host_calling_convention());
    test_context = execution_context_from_compilation(&test_compilation);
  }

  after_each() {
    compilation_deinit(&test_compilation);
  }

  describe("Scope") {
    it("should be able to set and lookup values") {
      Value *test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_s64, imm64(42), (Source_Range){0}
      );
      Scope *root_scope = scope_make(test_context.allocator, 0);
      const Symbol *symbol = mass_ensure_symbol(&test_compilation, slice_literal("test"));
      scope_define_value(root_scope, 0, (Source_Range){0}, symbol, test);
      Scope_Entry *entry = scope_lookup(root_scope, symbol);
      check(entry->value == test);
    }

    it("should be able to lookup things from parent scopes") {
      Value *global = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_s64, imm64(42), (Source_Range){0}
      );
      Scope *root_scope = scope_make(test_context.allocator, 0);
      const Symbol *global_symbol = mass_ensure_symbol(&test_compilation, slice_literal("global"));
      scope_define_value(root_scope, 0, (Source_Range){0}, global_symbol, global);

      const Symbol *test_symbol = mass_ensure_symbol(&test_compilation, slice_literal("test"));
      Value *level_1_test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_s64, imm64(1), (Source_Range){0}
      );
      Scope *scope_level_1 = scope_make(test_context.allocator, root_scope);
      scope_define_value(scope_level_1, 0, (Source_Range){0}, test_symbol, level_1_test);

      Value *level_2_test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_s64, imm64(2), (Source_Range){0}
      );
      Scope *scope_level_2 = scope_make(test_context.allocator, scope_level_1);
      scope_define_value(scope_level_2, 0, (Source_Range){0}, test_symbol,  level_2_test);

      Scope_Entry *entry = scope_lookup(scope_level_2, global_symbol);
      check(entry->value == global);
    }
  }

  describe("Tokenizer") {
    it("should be able to tokenize an empty string") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "");

      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 0);
    }

    it("should be able to tokenize a comment") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "// foo\n");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 0);
    }

    it("should be able to turn newlines into fake semicolon tokens on top level") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "foo\n");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 2);
      Value *new_line = value_view_get(tokens, 1);
      check(value_is_symbol(new_line));
      spec_check_slice(value_as_symbol(new_line)->name, slice_literal(";"));
    }

    it("should be able to parse hex integers") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0xCAFE");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &token->source_range), slice_literal("0xCAFE"));
      check(token->descriptor == &descriptor_i64);
      check(token->storage.tag == Storage_Tag_Static);
      i64 *literal = storage_static_as_c_type(&token->storage, i64);
      check(literal->bits == 0xCAFE);
    }

    it("should be able to parse binary integers") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0b100");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &token->source_range), slice_literal("0b100"));
      check(token->descriptor == &descriptor_i64);
      check(token->storage.tag == Storage_Tag_Static);
      i64 *literal = storage_static_as_c_type(&token->storage, i64);
      check(literal->bits == 0b100);
    }

    it("should be able to tokenize a sum of integers") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "12 + foo123");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 3);

      Value *a_num = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &a_num->source_range), slice_literal("12"));

      Value *plus = value_view_get(tokens, 1);
      check(value_is_symbol(plus));
      spec_check_slice(value_as_symbol(plus)->name, slice_literal("+"));

      Value *id = value_view_get(tokens, 2);
      check(value_is_symbol(id));
      spec_check_slice(source_from_source_range(test_context.compilation, &id->source_range), slice_literal("foo123"));
    }

    it("should be able to tokenize groups") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(x)");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *paren = value_view_get(tokens, 0);
      check(value_is_group_paren(paren));
      check(value_as_group_paren(paren)->children.length == 1);
      spec_check_slice(source_from_source_range(test_context.compilation, &paren->source_range), slice_literal("(x)"));

      Value *id = value_view_get(value_as_group_paren(paren)->children, 0);
      check(value_is_symbol(id));
    }

    it("should be able to tokenize strings") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "\"foo 123\"");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *string = value_view_get(tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &string->source_range), slice_literal("\"foo 123\""));
    }

    it("should be able to tokenize nested groups with different braces") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "{[]}");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *curly = value_view_get(tokens, 0);
      check(value_is_group_curly(curly));
      check(value_as_group_curly(curly)->children.length == 1);
      spec_check_slice(source_from_source_range(test_context.compilation, &curly->source_range), slice_literal("{[]}"));

      Value *square = value_view_get(value_as_group_curly(curly)->children, 0);
      check(value_is_group_square(square));
      check(value_as_group_square(square)->children.length == 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &square->source_range), slice_literal("[]"));
    }

    it("should be able to tokenize complex input") {
      Source_Range source_range = test_inline_source_range(
        test_context.compilation,
        "foo :: fn(x: s8) -> {\n"
        "  return x + 3;\n"
        "}"
      );
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Success);
    }

    it("should report a failure when encountering a brace that is not closed") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(foo");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Unexpected_Token);
      check(error->source_range.file == source_range.file);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }

    it("should report a failure when encountering a mismatched brace") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(foo}");
      Value_View tokens;
      Mass_Result result = tokenize(test_context.compilation, source_range, &tokens);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Unexpected_Token);
      check(error->source_range.file == source_range.file);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }
  }

  #ifdef _WIN32
  describe("Raw Machine Code") {
    // This test relies on Windows calling convention
    it("should be able to include raw machine code bytes") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn(x : s64) -> (s64) {"
          "inline_machine_code_bytes(0x48, 0xc7, 0xc1, 0x2a, 0x00, 0x00, 0x00)\n"
          "x"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(21) == 42);
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
      bool(*is_zero)(s32) = (bool(*)(s32))test_program_inline_source_function(
        "is_zero", &test_context,
        "is_zero :: fn(x : s32) -> (bool) {"
          "if x != 0 then false else true"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(is_zero(42) == false);
      check(is_zero(-2) == false);
      check(is_zero(0) == true);
    }
    it("should correctly handle always-false condition") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn() -> (s64) {"
          "if false then 21 else 42"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should correctly handle always-true condition") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn() -> (s64) {"
          "if true then 42 else 21"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should report an error on missing `then` inside of an if expression") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if true else 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }
    it("should report an error on double `then` inside of an if expression") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { if true then 0 then 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }
    it("should be able to parse and run if / else statement ") {
      s8(*checker)(s32) = (s8(*)(s32))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : s32) -> (s8) {\n"
          "if x < 10 then { return 0 } else { return 1 }\n"
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
        "foo :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8) -> (s8) { bar(x1, x2, x3, x4, x5) + x5 }\n"
        "bar :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8) -> (s8) { x4 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(1, 2, 3, 4, 5) == 9);
    }

    it("should correctly save volatile registers when calling other functions") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "outer", &test_context,
        "inner :: fn(x : s64) -> () { };"
        "outer :: fn(x : s64) -> (s64) { inner(1); x }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker(42);
      check(actual == 42);
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

    it("should be able to put a non-generic function literal into a typed local variable") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "local : (fn() -> (s64)) = fn() -> (s64) { 42 };"
          "local()"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker();
      check(answer == 42);
    }

    it("should be able to put a generic function literal into a typed local variable") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "local : (fn(x : s64) -> (s64)) = fn(x) -> (x) { x };"
          "local(42)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker();
      check(answer == 42);
    }

    it("should be able to put select from an overload set into a typed local variable") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "foo :: fn(x : s64) -> (s64) { x }\n"
          "foo :: fn(x : s32) -> (s32) { x }\n"
          "local : (fn(x : s64) -> (s64)) = foo\n"
          "local(42)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker();
      check(answer == 42);
    }

    it("should be able to put a function literal into an inferred local variable") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "local := fn() -> (s64) { 42 };"
          "local()"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker();
      check(answer == 42);
    }

    it("should report an error when trying to use a macro as fn pointer") {
      test_program_inline_source_base(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "local := macro() { 42 };"
          "local()"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_No_Runtime_Use);
    }

    it("should report an error when trying to use an inferred local from a generic function") {
      test_program_inline_source_base(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "local := macro() { 42 };"
          "local()"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_No_Runtime_Use);
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

    it("should prefer exact static value overload") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn(type :: s32) -> (s64) { 42 }\n"
        "foo :: fn(type : Type) -> (s64) { 21 }\n"
        "checker :: fn() -> (s64) { foo(s32) }\n"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 42);
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

    it("should support overload resolution based on fields in the tuple") {
      s64(*checker)() = (s64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn(x : [s64]) -> (s64) { 21 }\n"
        "foo :: fn(x : [String]) -> (s64) { 42 }\n"
        "checker :: fn() -> (s64) {\n"
          "foo([\"bar\"])\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when trying to overload a non-function") {
      test_program_inline_source_base(
        "overload", &test_context,
        "overload :: 42\n"
        "overload :: 21\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Non_Function_Overload);
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
        "test :: fn(x : s64, y : s64 = 20, z : s32) -> () {}\n"
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
        "checker :: fn(x : s32) -> (s32) { if x > 0 then { return x } else {}; 0 }"
      );
      check(spec_check_mass_result(test_context.result));
      s32 actual = checker(42);
      check(actual == 42);
    }

    it("should be able to run fibonnacii") {
      s64(*fibonnacci)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "fibonnacci", &test_context,
        "fibonnacci :: fn(n : s64) -> (s64) {\n"
          "if n < 2 then n else fibonnacci(n - 1) + fibonnacci(n - 2)"
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

    it("should report an error when non-type id is being used as a type") {
      test_program_inline_source_base(
        "main", &test_context,
        "foo :: fn() -> () {};"
        "main :: fn(arg : foo) -> () {}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_descriptor_pointer);
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
        "compile_time_fn :: fn() => (s64) { 42 }\n"
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
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "intrinsic_id :: fn(x : i64) => (i64) intrinsic { arguments.values.0 }\n"
          "checker :: fn() -> (s64) { intrinsic_id(42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to have access to arguments view in user-defined intrinsics") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn(a : i64, b : i64) => (i64)"
            "intrinsic { arguments.values.1 }\n"
          "checker :: fn() -> (s64) { my_intrinsic(21, 42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to allocate and return a void value through an intrinsic") {
      void (*checker)() = test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn() => () intrinsic {\n"
            "value : &MASS.Value = allocate(MASS.allocator, MASS.Value)\n"
            "value.source_range = arguments.source_range\n"
            "value.descriptor = type_of(())\n"
            "value.storage.tag = MASS.Storage_Tag.None\n"
            "value"
          "}\n"
          "checker :: fn() -> () { my_intrinsic() }\n"
        );
      check(spec_check_mass_result(test_context.result));
      checker();
    }

    it("should be able to allocate and return a static value through an intrinsic") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn() => () intrinsic {\n"
            "value : &MASS.Value = allocate(MASS.allocator, MASS.Value)\n"
            "value.source_range = arguments.source_range\n"
            "value.descriptor = u64\n"
            "value.storage.tag = MASS.Storage_Tag.Static\n"
            "value.storage.bit_size = value.descriptor.bit_size\n"
            "value.storage.Static.memory.tag = MASS.Static_Memory_Tag.U64\n"
            "value.storage.Static.memory.U64.value = 42\n"
            "value\n"
          "}\n"
          "checker :: fn() -> (u64) { my_intrinsic() }\n"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to allocate and return a lazy void value through an intrinsic") {
      void (*checker)() = test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn() => () intrinsic {\n"
            "lazy_value_proc :: fn("
              "compilation : &MASS.Compilation,"
              "builder : &MASS.Function_Builder,"
              "expected_result : &MASS.Expected_Result,"
              "source_range : &MASS.Source_Range,"
              "payload : &type_of(())"
            ") -> (&MASS.Value) {\n"
              "value : &MASS.Value = allocate(MASS.allocator, MASS.Value)\n"
              "value.source_range = source_range.*\n"
              "value.descriptor = type_of(())\n"
              "value.storage.tag = MASS.Storage_Tag.None\n"
              "value.storage.bit_size = value.descriptor.bit_size\n"
              "value"
            "}\n"
            "value : &MASS.Value = allocate(MASS.allocator, MASS.Value)\n"

            "lazy_value : &MASS.Lazy_Value = allocate(MASS.allocator, MASS.Lazy_Value)\n"
            "lazy_value.context = context.*\n"
            "lazy_value.descriptor = type_of(())\n"
            "lazy_value.proc = lazy_value_proc\n"
            "lazy_value.payload = 0\n"

            "value.source_range = arguments.source_range\n"
            "value.descriptor = MASS.Lazy_Value\n"
            "value.storage.tag = MASS.Storage_Tag.Static\n"
            "value.storage.bit_size = value.descriptor.bit_size\n"
            "value.storage.Static.memory.tag = MASS.Static_Memory_Tag.Heap\n"
            "value.storage.Static.memory.Heap.pointer = lazy_value\n"
            "value"
          "}\n"
          "checker :: fn() -> () { my_intrinsic() }\n"
        );
      check(spec_check_mass_result(test_context.result));
      checker();
    }
    it("should report an error when a non-compile-time fn has an intrinsic body") {
      test_program_inline_source_base(
        "checker", &test_context,
        "my_intrinsic :: fn() -> () intrinsic { 0 }\n"
        "checker :: fn() -> () { my_intrinsic() }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
    }

    it("should support referencing the type of one argument in the return type") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "identity :: fn(x : s64) -> (x) { x }\n"
          "checker :: fn() -> (s64) { identity(42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support templated parameters") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "identity :: fn(x) -> (x) { x }\n"
          "checker :: fn() -> (s64) { x := 42; identity(x) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support calls to a templated function with same typed parameters") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "identity :: fn(x) -> (x) { x }\n"
          "checker :: fn() -> (s64) { identity(cast(s64, 39)) + identity(cast(s64, 3)) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support calls to a templated function with differently typed parameters") {
      u64 (*checker)() =
        (u64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "identity :: fn(x) -> (x) { x }\n"
          "checker :: fn() -> (u64) {\n"
            "string := identity(\"foo\"); string.length + identity(cast(u64, 39))\n"
          "}"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should correctly discard templated functions with less parameters than arguments") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "weird_plus :: fn(x) -> (x) { x }\n"
          "weird_plus :: fn(x : s64, y : s64) -> (s64) { x + y }\n"
          "checker :: fn() -> (s64) { weird_plus(39, 3); }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should correctly discard templated functions with more parameters than arguments") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "weird_plus :: fn(x, y, z) -> (x) { x }\n"
          "weird_plus :: fn(x : s64, y : s64) -> (s64) { x + y }\n"
          "checker :: fn() -> (s64) { weird_plus(39, 3); }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support referring to the previous argument's type") {
      u64 (*checker)() =
        (u64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_add :: fn(x : y, y : u64) -> (x) { x + y }\n"
          "checker :: fn() -> (u64) {\n"
            "my_add(31, 11)\n"
          "}"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when argument types refer to each other") {
      test_program_inline_source_function(
        "checker", &test_context,
        "my_add :: fn(x : y, y : x) -> (x) { x + y }\n"
        "checker :: fn() -> (u64) {\n"
          "my_add(31, 11)\n"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }
  }

  describe("Assignment") {
    it("should not allow assignment to the arguments") {
      test_program_inline_source_function(
        "test", &test_context,
        "test :: fn(x : s64) -> () { x = 21 };"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Assignment_To_Constant);
    }

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

    it("should allow casting one pointer type to another") {
      s8(*checker)(void) = (s8(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s8) {\n"
          "x := 42\n"
          "cast(&s8, &x).*\n"
        "};"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
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
    it("should correctly sign extend numbers when assigning") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (s64) { a : u8 = 200; a }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 200);
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
    it("should support assigning a literal zero to a pointer") {
      test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> () {"
          "pointer : &s32 = 0;"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
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

    it("should support a u8 version of the remainder") {
      u8(*checker)() = (u8(*)())test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (u8) { remainder(cast(u8, 142), 100) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("Operators") {
    it("should be able to parse and run a triple plus function") {
      s64(*checker)(s64, s64, s64) = (s64(*)(s64, s64, s64))test_program_inline_source_function(
        "plus", &test_context,
        "plus :: fn(x : s64, y : s64, z : s64) -> (s64) { x + y + z }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(30, 10, 2) == 42);
    }

    it("should be able to parse and run a subtraction of a negative literal") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "plus_one", &test_context,
        "plus_one :: fn(x : s64) -> (s64) { x - -1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(41) == 42);
    }

    it("should be able to parse and run a negation of an s64 number") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "plus_one", &test_context,
        "plus_one :: fn(x : s64) -> (s64) { -x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(-42) == 42);
    }

    it("should be able to parse and run a sum passed to another function as an argument") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "plus", &test_context,
        "id :: fn(ignored : s64, x : s64) -> (s64) { x }\n"
        "plus :: fn() -> (s64) { x : s64 = 40; y : s64 = 2; id(0, x + y) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when defining an overloaded infix operator") {
      test_program_inline_source_base(
        "dummy", &test_context,
        "operator 15 (x ** y) multiply;"
        "operator 15 (x ** y) multiply;"
        "dummy :: fn() -> (s64) { 21 ** 2 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Operator_Fixity_Conflict);
      check(error->Operator_Fixity_Conflict.fixity == Operator_Fixity_Infix);
      spec_check_slice(error->Operator_Fixity_Conflict.symbol, slice_literal("**"));
    }

    it("should report an error when defining an overloaded infix and postfix operator") {
      test_program_inline_source_base(
        "dummy", &test_context,
        "operator 15 (x ** y) multiply;"
        "operator 15 (x **) multiply;"
        "dummy :: fn() -> (s64) { 21 ** 2 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Operator_Fixity_Conflict);
      check(error->Operator_Fixity_Conflict.fixity == Operator_Fixity_Postfix);
      spec_check_slice(error->Operator_Fixity_Conflict.symbol, slice_literal("**"));
    }

    it("should have a built-in compile-time shift operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 2 << 1 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 4);
    }

    it("should have a built-in compile-time bitwise and operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 0b110 & 0b011 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 0b10);
    }

    it("should have correctly handle the difference between addressof and bitwise and operators") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (&s64) { x := 0 & 1; &x }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual);
    }

    it("should have a built-in compile-time bitwise or operator") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) { 0b110 | 0b011 }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 actual = checker();
      check(actual == 0b111);
    }

    it("should support defining a custom 'empty space' operator handler") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "apply :: macro(symbol :: 'foo, literal : i64) -> (i64) { literal }\n"
        "checker :: fn() -> (s64) { foo 42 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("Quote / Unquote") {
    it("should be possible to use for non-lexical symbol lookup") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "my_symbol :: 'foo\n"
        "foo :: 42\n"
        "checker :: fn() -> (s64) { my_symbol' }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should be possible to use for splicing in a block") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "my_block :: '{ foo }\n"
        "foo :: 42\n"
        "checker :: fn() -> (s64) { my_block' }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
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
      check(status->storage.bit_size.as_u64 == 8);
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
      check(result->storage.bit_size.as_u64 == 8);
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

    it("should be able to use if / else statement at the top level") {
      test_program_inline_source_base(
        "DUMMY", &test_context,
        "do_stuff :: fn() -> () {}\n"
        "if true then { do_stuff() } else {}\n"
        "DUMMY :: 42"
      );
      check(test_context.result->tag == Mass_Result_Tag_Success);
    }

    it("should be able to use if / else to choose an implementation at compile time") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "TEST :: if true then 42 else 1000\n"
      );

      check(value);
      check(value->descriptor == &descriptor_i64);
      check(value->storage.tag == Storage_Tag_Static);
      i64 *literal = storage_static_as_c_type(&value->storage, i64);
      check(literal->bits == 42);
    }

    it("should be able combine if / else, inline modules and `using` for conditional definitions") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "using if true then module { TEST :: 42 } else module { TEST :: 1000 }\n"
      );

      check(value);
      check(value->descriptor == &descriptor_i64);
      check(value->storage.tag == Storage_Tag_Static);
      i64 *literal = storage_static_as_c_type(&value->storage, i64);
      check(literal->bits == 42);
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
        "checker", &test_context,
        "id :: macro(x : s64) -> (s64) { if x > 0 then { return 20 } else {}; x }\n"
        "checker :: fn() -> (s64) { id(42) + 1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 21);
    }

    it("should type check macro return value if it is defined") {
      test_program_inline_source_base(
        "checker", &test_context,
        "oops :: macro() -> (s64) { \"oops\" }\n"
        "checker :: fn() -> (s64) { oops() }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_s64);
      check(error->Type_Mismatch.actual == &descriptor_slice);
    }

    it("should support macro parameters without type assertions") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "identity :: macro(x) { x }\n"
          "checker :: fn() -> (s64) { identity(42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to should be able to handle fragment statement") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "test_fragment :: ~> { return 42 }\n"
        "checker :: fn() -> (s64) {\n"
          "test_fragment\n"
          "21"
        "}"
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

    it("should be able to use a while loop") {
      s32(*sum_up_to)(s32) = (s32(*)(s32))test_program_inline_source_function(
        "sum_up_to", &test_context,
        "sum_up_to :: fn(to : s32) -> (s32) {"
          "x := to;"
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
        "checker :: fn(to : s32) -> (s32) {"
          "x := to;"
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
        "checker :: fn(to : s32) -> (s32) {"
          "x := to;"
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
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Parse);
      Slice actual_error_slice = source_from_source_range(test_context.compilation, &error->source_range);
      Slice expected_error_slice = slice_literal("external(\"kernel32.dll\", \"ExitProcess\")");
      check(slice_equal(slice_trim_whitespace(actual_error_slice), expected_error_slice));
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
        "sum_up_to :: fn(to : s32) -> (s32) {"
          "x := to;"
          "sum : s32;"
          "sum = 0;"
          "label loop;"
          "if x < 0 then { return sum } else {};"
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
      check(error->tag == Mass_Error_Tag_No_Matching_Overload);
    }

    it("should report an error when fixed size array size is negative") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> () {"
          "foo : s8 * -42"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }
  }

  describe("User-defined Types") {
    it("should be able to parse struct definitions") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p : Point; p.x = 20; p.y = 22;"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should mark a field of a constant struct as a constant as well") {
      test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn(p : Point) -> () { p.x = 21 };"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Assignment_To_Constant);
    }

    it("should be able to parse tuples and access their elements") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) {"
          "p := [20, 22]\n"
          "p.0 + p.1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to parse nested tuples and access their elements") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) {"
          "p := [[12, 8], 22]\n"
          "p.0.0 + p.0.1 + p.1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to parse struct definitions") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p : Point = [20, 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to dereference struct pointers") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p : Point = [20, 22]\n"
          "pointer := &p\n"
          "dereferenced := pointer.*\n"
          "dereferenced.x + dereferenced.y\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to explicitly cast a tuple to a struct type that matches it") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "cast(Point, [42, 0]).x"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should allow casting unrelated types with the same bit size") {
      s8(*checker)(void) = (s8(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Wrapped :: c_struct [x : s32]\n"
        "test :: fn() -> (s32) {\n"
          "x : Wrapped = [42]\n"
          "cast(s32, x)\n"
        "};"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support C-style literal syntax") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p := Point [20, 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support using tuple as a type") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "tuple_id :: fn(x : [s64]) -> (s64) {"
          "x.0"
        "}\n"
        "test :: fn() -> (s64) {"
          "tuple := [42]\n"
          "tuple_id(tuple)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support named fields in a tuple as a type") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) {"
          "tuple : [foo : s64] = [42]\n"
          "tuple.foo"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when tuple is assigned to something that is not a struct") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> (s32) {"
          "p : s32 = [20]\n"
          "p"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
    }

    it("should report an error when tuple does not have enough fields for a struct") {
      test_program_inline_source_base(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p : Point = [20]\n"
          "p.x + p.y"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
    }

    it("should report an error when tuple has too many fields for a struct") {
      test_program_inline_source_base(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p : Point = [20, 1, 1]\n"
          "p.x + p.y"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
    }

    it("should report an error when field name is not an identifier") {
      test_program_inline_source_base(
        "main", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "main :: fn() -> () { p : Point; p.(x) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Invalid_Identifier);
    }

    it("should report an error when a struct does not have the requested field") {
      test_program_inline_source_base(
        "main", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "main :: fn() -> () { p : Point; p.foo }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Unknown_Field);
      spec_check_slice(error->Unknown_Field.name, slice_literal("foo"));
    }

    it("should support receiving register-sized structs in the function") {
      struct Point { s32 x; s32 y;};
      s32(*checker)(struct Point) = (s32(*)(struct Point))test_program_inline_source_function(
        "checker", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "checker :: fn(p: Point) -> (s32) {"
          "p.x - p.y"
        "}"
      );
      struct Point p = {44, 2};
      check(spec_check_mass_result(test_context.result));
      check(checker(p) == 42);
    }

    it("should support passing register-sized structs into the function") {
      s32(*checker)() = (s32(*)())test_program_inline_source_function(
        "checker", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "nested :: fn(p: Point) -> (s32) {"
          "p.x - p.y"
        "}\n"
        "checker :: fn() -> (s32) {"
          "p : Point\n"
          "p.x = 44\n"
          "p.y = 2\n"
          "nested(p)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support passing register-sized structs in larger structs into the function") {
      s32(*checker)() = (s32(*)())test_program_inline_source_function(
        "checker", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "Line :: c_struct [from : Point, to : Point]\n"
        "nested :: fn(line: Line) -> (s32) {"
          "line.to.y - line.from.y"
        "}\n"
        "checker :: fn() -> (s32) {"
          "from : Point; from.x = 31; from.y = 2\n"
          "to : Point; to.x = 60; to.y = 44\n"
          "line : Line; line.from = from; line.to = to\n"
          "nested(line)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support passing register-sized arrays in larger structs into the function") {
      s32(*checker)() = (s32(*)())test_program_inline_source_function(
        "checker", &test_context,
        "Line :: c_struct [from : s32 * 2, to : s32 * 2]\n"
        "nested :: fn(line: Line) -> (s32) {"
          "line.to.1 - line.from.1"
        "}\n"
        "checker :: fn() -> (s32) {"
          "from : s32 * 2; from.0 = 31; from.1 = 2\n"
          "to : s32 * 2; to.0 = 60; to.1 = 44\n"
          "line : Line; line.from = from; line.to = to\n"
          "nested(line)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should auto-dereference pointers to struct on field access") {
      s64(*checker)(Test_128bit*) = (s64(*)(Test_128bit*))test_program_inline_source_function(
        "checker", &test_context,
        "Test_128bit :: c_struct [ x : s64, y : s64 ]\n"
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
        "Test_128bit :: c_struct [ x : s64, y : s64 ]\n"
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
        "Test_128bit :: c_struct [ x : s64, y : s64 ]\n"
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
    it("should be able to use a larger-than-register struct passed as the 7th arguments") {
      s8(*checker)(s8, s8, s8, s8, s8, s8, Test_128bit) =
        (s8(*)(s8, s8, s8, s8, s8, s8, Test_128bit))test_program_inline_source_function(
          "foo", &test_context,
          "Test_128bit :: c_struct [ x : s64, y : s64 ]\n"
          "foo :: fn(x1: s8, x2 : s8, x3 : s8, x4 : s8, x5 : s8, x6 : s8, x7 : Test_128bit ) -> (s64) { x7.x }"
        );
      check(spec_check_mass_result(test_context.result));
      Test_128bit test_128bit = { .x = 42, .y = 20 };
      check(checker(1, 2, 3, 4, 5, 6, test_128bit) == 42);
    }

    it("should be able to access fields of a returned struct") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn() -> (String) { \"foo\" }\n"
        "checker :: fn() -> (u64) { foo().length }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 3);
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
      bool(*checker)(void) = (bool(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (bool) { x : u8 = 200; x < 0 }"
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

  describe("Asserts") {
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

    it("should support inline modules") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> s64 {"
          "Foo :: module { answer :: 42 }\n"
          "Foo.answer"
        "}\n"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support global static variables inside inline modules") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> s64 {"
          "Foo :: module { answer := 42 }\n"
          "Foo.answer"
        "}\n"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
  }

  describe("Script mode") {
    it("should support script-mode execution") {
      Mass_Result result =
        program_load_file_module_into_root_scope(&test_context, slice_literal("std/prelude"));
      check(spec_check_mass_result(&result));
      mass_run_script(&test_context, slice_literal("fixtures/script_mode"));
      check(spec_check_mass_result(test_context.result));
    }
  }

  describe("PE32 Executables") {
    it("should parse and write out an executable that exits with status code 42") {
      Program *test_program = test_context.program;
      test_program->default_calling_convention = &calling_convention_x86_64_windows;
      test_program->entry_point = test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { ExitProcess(42) }\n"
        "ExitProcess :: fn(status : s32) -> (s64) external(\"kernel32.dll\", \"ExitProcess\")"
      );
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/test_parsed.exe"), &test_context, Executable_Type_Cli);
    }

    it("should parse and write an executable that prints Hello, world!") {
      Program *test_program = test_context.program;
      test_program->os = Os_Windows;
      test_program->default_calling_convention = &calling_convention_x86_64_windows;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "fixtures/hello_world"
      );
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/hello_world.exe"), &test_context, Executable_Type_Cli);
    }

    xit("should parse and write an executable with a lot of constant folding") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "../compile-time-benchmark/folding"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_compilation, test_program, test_program->entry_point, (Value_View){0});
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/folding.exe"), &test_context, Executable_Type_Cli);
    }

    xit("should parse and write an executable with a 1 million function calls") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "../compile-time-benchmark/print"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_compilation, test_program, test_program->entry_point, (Value_View){0});
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/print.exe"), &test_context, Executable_Type_Cli);
    }
  }

  describe("Relocations") {
    it("should work in JIT code") {
      Slice(*checker)(void) = (Slice(*)(void))test_program_external_source_function(
        "test", &test_context, "fixtures/relocations"
      );
      check(spec_check_mass_result(test_context.result));
      Slice result = checker();
      spec_check_slice(result, slice_literal("a"));
    }

    it("should work in an executable") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "fixtures/relocations"
      );
      check(test_program->entry_point);
      ensure_function_instance(&test_compilation, test_program, test_program->entry_point, (Value_View){0});
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/relocations.exe"), &test_context, Executable_Type_Cli);
    }
  }
  #if defined(__linux__)
  describe("Syscall") {
    it("should be able to print out a string") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "STDOUT_FILENO :: 1\n"
        "SYS_WRITE :: 1\n"
        "write :: fn(descriptor : s32, buffer : &u8, size : u64) "
          "-> (s32) syscall(SYS_WRITE)\n"
        "checker :: fn() -> () {\n"
          "hello :: \"Hello, world!\\n\"\n"
          "write(STDOUT_FILENO, hello.bytes, hello.length) \n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }
  }
  #endif

  describe("External (DLL Imports)") {
    #if defined(__linux__)
    it("should be able to print out a string") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "write :: fn(descriptor : s32, buffer : &u8, size : u64) "
          "-> (s64) external(\"libc.so.6\", \"write\")\n"
        "STDOUT_FILENO :: 1\n"
        "checker :: fn() -> () {\n"
          "hello :: \"Hello, world!\\n\"\n"
          "write(STDOUT_FILENO, hello.bytes, hello.length) \n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }
    #endif

    it("should report a user error when failing to JIT load a library") {
      test_program_inline_source_function(
        "checker", &test_context,
        "broken_import :: fn() -> () external(\"very broken dll name\", \"and a broken symbol\") \n"
        "checker :: fn() { broken_import() }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Dynamic_Library_Load);
    }
  }

  describe("Complex Examples") {
    it("should be able to run fizz buzz") {
      fn_type_opaque fizz_buzz =
        test_program_external_source_function("fizz_buzz", &test_context, "fixtures/fizz_buzz");
      check(spec_check_mass_result(test_context.result));
      check(fizz_buzz);
      fizz_buzz();
    }
  }
}
