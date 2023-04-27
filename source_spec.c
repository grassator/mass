#include "bdd-for-c.h"

#include "pe32.c"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"
#include "program.c"

typedef struct {
  u64 x;
  u64 y;
} Test_128bit;
static_assert(sizeof(Test_128bit) * 8 == 128, "Expected a 128bit struct");

typedef struct {
  u64 x;
  u64 y;
  u64 z;
} Test_192bit;
static_assert(sizeof(Test_192bit) * 8 == 192, "Expected a 192bit struct");

typedef struct {
  u8 x;
  u8 y;
  u8 z;
} Test_24bit;
static_assert(sizeof(Test_24bit) * 8 == 24, "Expected a 24bit struct");

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

typedef u64 (*Spec_Callback)();
static u64 spec_callback() { return 42; }

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
  Mass_Context *context,
  Test_Program_Source source
) {
  program_load_file_module_into_root_scope(context, slice_literal("std/prelude"));
  if (mass_has_error(context)) return 0;

  Module *test_module = 0;
  switch(source.tag) {
    case Test_Program_Source_Tag_Inline: {
      test_module = allocator_allocate(context->allocator, Module);
      Source_Range source_range = test_inline_source_range(context->compilation, source.text);
      *test_module = (Module) {
        .source_range = source_range,
        .own_scope = context->compilation->root_scope,
      };
    } break;
    case Test_Program_Source_Tag_File: {
      test_module = program_module_from_file(context, source.path, context->compilation->root_scope);
    } break;
  }
  program_import_module(context, test_module);
  if (mass_has_error(context)) return 0;
  Slice id_slice = slice_from_c_string(id);
  Source_Range symbol_source_range;
  INIT_LITERAL_SOURCE_RANGE(&symbol_source_range, "__test_symbol__");
  const Symbol *symbol = mass_ensure_symbol(context->compilation, id_slice);
  Scope_Entry *entry = scope_lookup(context->compilation->root_scope, symbol);
  Value *value = scope_entry_force_value(context, entry);
  if (value && value->descriptor == &descriptor_function_literal) {
    const Function_Literal *literal = value_as_function_literal(value);
    Function_Info info;
    mass_function_info_init_for_header_and_maybe_body(
      context, literal->own_scope, &literal->header, literal->body, &info
    );
    if (mass_has_error(context)) return 0;
    return ensure_function_instance(context, value, info.parameters);
  }
  return value;
}

fn_type_opaque
test_program_source_function(
  const char *function_id,
  Mass_Context *context,
  Test_Program_Source source
) {
  Value *value = test_program_source_base(function_id, context, source);
  if (mass_has_error(context)) return 0;
  if (!value) return 0;
  static Jit test_jit;
  jit_init(&test_jit, context->program);
  program_jit(context, &test_jit);
  if (mass_has_error(context)) return 0;
  fn_type_opaque fn = value_as_function(test_jit.program, value);
  if (mass_has_error(context)) return 0;
  return fn;
}

static Value *
test_program_inline_source_base(
  const char *id,
  Mass_Context *context,
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
  Mass_Context *context,
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
  Mass_Context *context,
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
  Mass_Context *context,
  const char *path
) {
  return test_program_source_function(function_id, context, (Test_Program_Source) {
    .tag = Test_Program_Source_Tag_File,
    .path = slice_from_c_string(path),
  });
}

spec("source") {
  static Compilation test_compilation = {0};
  static Mass_Context test_context = {0};

  before_each() {
    compilation_init(&test_compilation, host_os());
    test_context = mass_context_from_compilation(&test_compilation);
  }

  after_each() {
    compilation_deinit(&test_compilation);
  }

  describe("Scope") {
    it("should be able to set and lookup values") {
      Value *test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_i64, imm64(42), (Source_Range){0}
      );
      Scope *root_scope = scope_make(test_context.allocator, 0);
      const Symbol *symbol = mass_ensure_symbol(&test_compilation, slice_literal("test"));
      scope_define_value(root_scope, (Epoch){0}, (Source_Range){0}, symbol, test);
      Scope_Entry *entry = scope_lookup(root_scope, symbol);
      check(entry->value == test);
    }

    it("should be able to lookup things from parent scopes") {
      Value *global = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_i64, imm64(42), (Source_Range){0}
      );
      Scope *root_scope = scope_make(test_context.allocator, 0);
      const Symbol *global_symbol = mass_ensure_symbol(&test_compilation, slice_literal("global"));
      scope_define_value(root_scope, (Epoch){0}, (Source_Range){0}, global_symbol, global);

      const Symbol *test_symbol = mass_ensure_symbol(&test_compilation, slice_literal("test"));
      Value *level_1_test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_i64, imm64(1), (Source_Range){0}
      );
      Scope *scope_level_1 = scope_make(test_context.allocator, root_scope);
      scope_define_value(scope_level_1, (Epoch){0}, (Source_Range){0}, test_symbol, level_1_test);

      Value *level_2_test = value_init(
        allocator_allocate(test_context.allocator, Value),
        &descriptor_i64, imm64(2), (Source_Range){0}
      );
      Scope *scope_level_2 = scope_make(test_context.allocator, scope_level_1);
      scope_define_value(scope_level_2, (Epoch){0}, (Source_Range){0}, test_symbol,  level_2_test);

      Scope_Entry *entry = scope_lookup(scope_level_2, global_symbol);
      check(entry->value == global);
    }
  }

  describe("Tokenizer") {
    it("should be able to tokenize an empty string") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "");

      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Success);
      check(block.first_statement == 0);
    }

    it("should be able to tokenize a comment") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "// foo\n");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Success);
      check(block.first_statement == 0);
    }

    it("should count single-line comment as a statement separator") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "  a//\nb");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Success);
      check(block.first_statement->next == block.last_statement, "Expected 2 statements");
    }

    it("should be able to tokenize ids containing letters and _") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "foo_123");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Success);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(&tokens, 0);
      check(value_is_symbol(token));
    }

    it("should be able to turn newlines into fake semicolon tokens on top level") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "foo\n");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Success);
      check(block.first_statement == block.last_statement);
    }

    it("should be able to parse a lone 0") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(&tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &token->source_range), slice_literal("0"));
      check(value_is_i64(token));
      const i64 *literal = value_as_i64(token);
      check(literal->bits == 0);
    }

    it("should be able to parse a 0 followed by an identifier") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0foo");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 2);
      {
        Value *zero = value_view_get(&tokens, 0);
        spec_check_slice(source_from_source_range(test_context.compilation, &zero->source_range), slice_literal("0"));
        check(value_is_i64(zero));
        const i64 *literal = value_as_i64(zero);
        check(literal->bits == 0);
      }
      {
        Value *foo = value_view_get(&tokens, 1);
        spec_check_slice(source_from_source_range(test_context.compilation, &foo->source_range), slice_literal("foo"));
        check(value_is_symbol(foo));
      }
    }

    it("should be able to parse hex integers") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0xCAFE");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(&tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &token->source_range), slice_literal("0xCAFE"));
      check(value_is_i64(token));
      const i64 *literal = value_as_i64(token);
      check(literal->bits == 0xCAFE);
    }

    it("should be able to parse a hex integer followed by an identifier") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0xfffoo");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 2);
      {
        Value *number = value_view_get(&tokens, 0);
        spec_check_slice(source_from_source_range(test_context.compilation, &number->source_range), slice_literal("0xfff"));
        check(value_is_i64(number));
        const i64 *literal = value_as_i64(number);
        check(literal->bits == 0xfff);
      }
      {
        Value *symbol = value_view_get(&tokens, 1);
        spec_check_slice(source_from_source_range(test_context.compilation, &symbol->source_range), slice_literal("oo"));
        check(value_is_symbol(symbol));
      }
    }

    it("should be able to parse binary integers") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "0b100");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *token = value_view_get(&tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &token->source_range), slice_literal("0b100"));
      check(value_is_i64(token));
      const i64 *literal = value_as_i64(token);
      check(literal->bits == 0b100);
    }

    it("should be able to tokenize a sum of integer and a symbol") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "12 + foo123");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 3);

      Value *a_num = value_view_get(&tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &a_num->source_range), slice_literal("12"));

      Value *plus = value_view_get(&tokens, 1);
      check(value_is_symbol(plus));
      spec_check_slice(value_as_symbol(plus)->name, slice_literal("+"));

      Value *id = value_view_get(&tokens, 2);
      check(value_is_symbol(id));
      spec_check_slice(source_from_source_range(test_context.compilation, &id->source_range), slice_literal("foo123"));
    }

    it("should be able to tokenize strings") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "\"foo 123\"");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);
      Value *string = value_view_get(&tokens, 0);
      spec_check_slice(source_from_source_range(test_context.compilation, &string->source_range), slice_literal("\"foo 123\""));
    }

    it("should be able to tokenize groups") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(x)");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement == block.last_statement);
      Value_View tokens = block.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *paren = value_view_get(&tokens, 0);
      check(value_is_group_paren(paren));
      check(value_as_group_paren(paren)->children.length == 1);
      spec_check_slice(source_from_source_range(test_context.compilation, &paren->source_range), slice_literal("(x)"));

      Value *id = value_view_get(&value_as_group_paren(paren)->children, 0);
      check(value_is_symbol(id));
    }

    it("should be able to tokenize nested groups with different braces") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "{[]}");
      Ast_Block root;
      Mass_Result result = tokenize(&test_context, source_range, &root);
      check(root.first_statement == root.last_statement);
      Value_View tokens = root.first_statement->children;
      check(result.tag == Mass_Result_Tag_Success);
      check(tokens.length == 1);

      Value *block_value = value_view_get(&tokens, 0);
      check(value_is_ast_block(block_value));
      const Ast_Block *block = value_as_ast_block(block_value);
      check(block->first_statement == block->last_statement);
      spec_check_slice(source_from_source_range(test_context.compilation, &block_value->source_range), slice_literal("{[]}"));

      Value *square = value_view_get(&block->first_statement->children, 0);
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
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(block.first_statement);
      check(block.first_statement == block.last_statement);
      check(result.tag == Mass_Result_Tag_Success);
    }

    it("should report a failure when encountering a brace that is not closed") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(foo");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Tokenizer);
      check(error->source_range.file == source_range.file);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }

    it("should report a failure when encountering a mismatched brace") {
      Source_Range source_range = test_inline_source_range(test_context.compilation, "(foo}");
      Ast_Block block;
      Mass_Result result = tokenize(&test_context, source_range, &block);
      check(result.tag == Mass_Result_Tag_Error);
      Mass_Error *error = &result.Error.error;
      check(error->tag == Mass_Error_Tag_Tokenizer);
      check(error->source_range.file == source_range.file);
      check(error->source_range.offsets.from == 4);
      check(error->source_range.offsets.to == 4);
    }
  }

  describe("if / else") {
    it("should be able to parse and run if expression") {
      u64(*checker)(u64) = (u64(*)(u64))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : i64) -> (i64) {"
          "if x == 0 then 0 else 1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 1);
      check(checker(0) == 0);
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
          "if x < 10 then { return 0 } else { return 1 }"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 1);
      check(checker(-2) == 0);
    }

    it("should be able to parse and run an `if` statement without an `else` block") {
      s8(*checker)(s32) = (s8(*)(s32))test_program_inline_source_function(
        "is_positive", &test_context,
        "is_positive :: fn(x : s32) -> (s8) {\n"
          "if x < 10 then { return 0 }\n"
          "1"
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

    it("should support specifying a function signature separate from the body") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "My_Proc :: fn() -> (s64)\n"
        "foo :: My_Proc { 42 }"
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

    it("should be able to make argument writable by reassigning it to a local variable") {
      s64(*checker)(s64) = (s64(*)(s64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x: s64) -> (s64) { x := x; x = x * 2; x }"
      );
      check(spec_check_mass_result(test_context.result));
      s64 answer = checker(INT32_MAX);
      check(answer == (s64)INT32_MAX * 2);
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

    it("should be able to select from an overload set into a typed local variable") {
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

    it("should be able to select from an overload set into a function argument") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) {"
          "foo :: fn(x : s64) -> (s64) { x }\n"
          "foo :: fn(x : s32) -> (s32) { x }\n"
          "callback :: fn(f : fn(x : s64) -> (s64)) -> (s64) { 42 }\n"
          "callback(foo)"
        "}"
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
        "Bar :: c_struct[x:i64]\n"
        "foo :: fn(x : [s64]) -> (s64) { 21 }\n"
        "foo :: fn(x : [Bar]) -> (s64) { 42 }\n"
        "checker :: fn() -> (s64) {\n"
          "bar := Bar [1]\n"
          "foo([bar])\n"
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
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test_default :: fn(x : i64, y : i64 = 20) -> (i64) { x + y }\n"
        "test :: fn() -> (i64) { (test_default(20) + \n test_default(0, 2)) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support default arguments with inference") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test_default :: fn(x : i64, y := 20) -> (i64) { x + y }\n"
        "test :: fn() -> (i64) { (test_default(20) + \n test_default(0, 2)) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should disallow default arguments coming after non-default ones") {
      test_program_inline_source_function(
        "test", &test_context,
        "test :: fn(x : i64, y : i64 = 20, z : i64) -> () {}\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Non_Trailing_Default_Argument);
    }

    it("should support capturing static arguments") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: { ANSWER :: 42; fn() -> (i64) { ANSWER } }\n"
      );
      check(spec_check_mass_result(test_context.result));
      u64 actual = checker();
      check(actual == 42);
    }

    it("should be able to have an explicit return") {
      u64(*checker)(u64) = (u64(*)(u64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : i64) -> (i64) { if x == 0 then { return 42 }; 0 }"
      );
      check(spec_check_mass_result(test_context.result));
      u64 actual = checker(0);
      check(actual == 42);
    }

    it("should be able to have an explicit return at the end of function") {
      u64(*checker)(u64) = (u64(*)(u64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : i64) -> (i64) { return x }"
      );
      check(spec_check_mass_result(test_context.result));
      u64 actual = checker(42);
      check(actual == 42);
    }

    it("should report an error when unreacheable statements are found after a return") {
      test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : i64) -> (i64) { return x; x + 2 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Unreachable_Statement);
    }

    it("should be able to have an explicit return without any expression following") {
      void(*checker)(u64 *) = (void(*)(u64 *))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : &i64) -> () { if x.* == 0 then { return }; x.* = 42 }"
      );
      check(spec_check_mass_result(test_context.result));
      u64 number = 21;
      checker(&number);
      check(number == 42);
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

    it("should support inferred return types for a non-recursive fn with no explicit returns") {
      u64(*checker)(u64) = (u64(*)(u64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x: i64) -> _ { x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 42);
    }

    it("should support inferred return types for a non-recursive fn with an explicit return at the end") {
      u64(*checker)(u64) = (u64(*)(u64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x: i64) -> _ { return x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(42) == 42);
    }

    // :RecursiveInferredType
    it("should report an error when trying to infer a type of a recursive fn") {
      test_program_inline_source_base(
        "checker", &test_context,
        "checker :: fn(x: i64) -> _ { checker(1) }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
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

    it("should be able to eval something as a type") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "my_struct_type :: parse_type([x : i64])\n"
        "checker :: fn() -> (i64) {"
          "foo := my_struct_type [42]\n"
          "foo.x"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
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
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo :: 0\n"
        "id :: fn(x : type_of(foo)) -> (type_of(foo)) { x }\n"
        "checker :: fn() -> (i64) {"
          "id(42)"
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

    it("should support defining a void to void function") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "compile_time_fn :: fn() => () { }\n"
        "checker :: fn() -> () { compile_time_fn() }"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }

    it("should prefer a compile-time overload over a runtime if args are compile-time known") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn(x : i64) => (i64) { 42 }\n"
        "foo :: fn(x : i64) -> (i64) { 21 }\n"
        "checker :: fn() -> (i64) { foo(1) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should not select a compile-time overload over a runtime one if args are runtime") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn(x : i64) => (i64) { 21 }\n"
        "foo :: fn(x : i64) -> (i64) { 42 }\n"
        "checker :: fn() -> (i64) { x := 1; foo(x) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should allow using a compile-time overload inside a c compile-time fn") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn() => (i64) { bar(42) }\n"
        "bar :: fn(x : i64) => (i64) { x }\n"
        "checker :: fn() -> (i64) { foo() }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support multiple calls to generic compile-time fns with different typed args") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "my_generic :: fn(x) => (x) { x }\n"
        "checker :: fn() -> (i64) {\n"
          "my_generic([1, 2])\n"
          "my_generic(42)\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to accept a function as an argument and call it") {
      u64(*checker)(Spec_Callback foo) =
        (u64(*)(Spec_Callback))test_program_inline_source_function(
          "foo", &test_context,
          "foo :: fn(callback : fn() -> (i64)) -> (i64) { callback() }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker(spec_callback) == 42);
    }

    // :TemporaryCallTarget
    it("should be able to call a function returned from another fn") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
          "foo", &test_context,
          "callback :: fn() -> (i64) { 42 }\n"
          "make_callback :: fn() -> (fn() -> (i64)) { callback }\n"
          "foo :: fn() -> (i64) { make_callback()() }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
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
          "checker :: fn() -> (s64) {\n"
            "data := identity([.foo = 3]); data.foo + identity(39)\n"
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

    it("should support static type resolution for generic types in the body") {
      u64 (*checker)() =
        (u64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_bit_size :: fn(x) -> (u64) { type :: type_of(x); type_descriptor(type).bit_size.as_u64 }\n"
          "checker :: fn() -> (u64) {\n"
            "x : u32 = 0\n"
            "my_bit_size(x)\n"
          "}"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 32);
    }

    it("should support fns requiring staticly known args") {
      u64 (*checker)() = (u64 (*)())test_program_inline_source_function(
        "checker", &test_context,
        "static_i64_identity :: fn(@x) -> (x) { internal :: x; internal }\n"
        "checker :: fn() -> (i64) {\n"
          "static_i64_identity(42)\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error if a static generic arg is not of the specified type") {
      test_program_inline_source_base(
        "checker", &test_context,
        "static_i64_identity :: fn(@x : i64) -> (i64) { internal :: x; internal }\n"
        "checker :: fn() -> (i64) {\n"
          "static_i64_identity(())\n"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(same_type(error->Type_Mismatch.expected, &descriptor_i64));
    }

    it("should support fns requiring staticly known templated parameters") {
      u64 (*checker)() = (u64 (*)())test_program_inline_source_function(
        "checker", &test_context,
        "static_i64_identity :: fn(@x) -> (x) { internal :: x; internal }\n"
        "checker :: fn() -> (u64) {\n"
          "static_i64_identity(42)\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should not match a static templated parameter to a non-static value") {
      test_program_inline_source_function(
        "checker", &test_context,
        "static_i64_identity :: fn(@x) -> (x) { internal :: x; internal }\n"
        "checker :: fn() -> (u64) {\n"
          "x := 42\n"
          "static_i64_identity(x)\n"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
    }

    it("should support type constraints on template parameters") {
      u64 (*checker)() =
        (u64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "type_constraint_any :: fn(t : Type) -> (bool) { true }\n"
          "type_constraint_none :: fn(t : Type) -> (bool) { false }\n"
          "foo :: fn(x ~ type_constraint_any) -> (i64) { 42 }\n"
          "foo :: fn(x ~ type_constraint_none) -> (i64) { 21 }\n"
          "checker :: fn() -> (i64) {\n"
            "foo(1)\n"
          "}"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should prefer a match with a type constraint over the one without") {
      u64 (*checker)() =
        (u64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "type_constraint_any :: fn(t : Type) -> (bool) { true }\n"
          "foo :: fn(x ~ type_constraint_any) -> (i64) { 42 }\n"
          "foo :: fn(x) -> (i64) { 21 }\n"
          "checker :: fn() -> (i64) {\n"
            "foo(1)\n"
          "}"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when the constraint function does not match the signature") {
      test_program_inline_source_base(
        "checker", &test_context,
        "constraint_with_a_wrong_signature :: fn() -> () { }\n"
        "foo :: fn(x ~ constraint_with_a_wrong_signature) -> (i64) { 42 }\n"
        "foo :: fn(x) -> (i64) { 21 }\n"
        "checker :: fn() -> (i64) {\n"
          "foo(1)\n"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
    }
  }

  describe("Intrinsics") {
    it("should be able to have user-defined intrinsics") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "intrinsic_id :: fn(x : i64) -> (i64) intrinsic { arguments.0 }\n"
          "checker :: fn() -> (s64) { intrinsic_id(42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should validate the return type of the intrinsic when specified") {
      test_program_inline_source_function(
        "checker", &test_context,
        "intrinsic_id :: fn(x : i64, y : String) -> (i64) intrinsic { arguments.1 }\n"
        "checker :: fn() -> () { intrinsic_id(42, \"foo\") }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_i64);
      check(error->Type_Mismatch.actual == &descriptor_slice);
    }

    it("should be able to have access to the arguments view in user-defined intrinsics") {
      s64 (*checker)() =
        (s64 (*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn(a : i64, b : i64) -> (i64) intrinsic { arguments.1 }\n"
          "checker :: fn() -> (s64) { my_intrinsic(21, 42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when trying to directly recurse into the same intrinsic") {
      test_program_inline_source_function(
        "checker", &test_context,
        "my_intrinsic :: fn() -> () intrinsic {\n"
          "my_intrinsic()\n"
        "}\n"
        "checker :: fn() -> () { my_intrinsic() }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Recursive_Intrinsic_Use);
    }

    it("should report an error when trying to indirectly recurse into the same intrinsic") {
      test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn() -> () { my_intrinsic() }\n"
        "my_intrinsic :: fn() -> () intrinsic { foo() }\n"
        "checker :: fn() -> () { my_intrinsic() }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Recursive_Intrinsic_Use);
    }

    it("should support static generic parameters in intrinsics") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
          "checker", &test_context,
          "my_intrinsic :: fn(@a) -> (i64) intrinsic { arguments.0 }\n"
          "checker :: fn() -> (i64) { my_intrinsic(42) }"
        );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
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
    it("should report an error when LHS of the := is not a symbol") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { 2 := 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_symbol);
      check(error->Type_Mismatch.actual == &descriptor_i64);
    }
    it("should report an error for multi-definition assignment") {
      test_program_inline_source_base(
        "main", &test_context,
        "main :: fn() -> () { foo, bar := 42, 42 }"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
      check(error->Type_Mismatch.expected == &descriptor_symbol);
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
    it("should be able to assign an integer to an i8 value that would fit it") {
      u8(*checker)(void) = (u8(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (i8) { x : i8 = 42; x }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should be able to define, assign and lookup an i64 variable on the stack") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (i64) { y := 10; x := 21; x = 32; x + y }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should be able to assign to a void value") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "foo", &test_context,
        "foo :: fn() -> (i64) { () = 10; 42 }"
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
    describe("addition") {
      it("should correctly handle i64") {
        MATH_CHECKER_FN(i64, i64, +);
        check(spec_check_mass_result(test_context.result));
        check(checker((i64){10}, (i64){3}).bits == 10 + 3);
      }
      it("should correctly handle s64") {
        MATH_CHECKER_FN(s64, s64, +);
        check(spec_check_mass_result(test_context.result));
        check(checker(10, 3) == 10 + 3);
      }
    }

    describe("division") {
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
    it("should be able to use an operator in parens as an immediate function target") {
      u64(*checker)(u64, u64) = (u64(*)(u64, u64))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(x : i64, y : i64) -> (i64) { (+)(x, y) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker(30, 12) == 42);
    }
  }

  describe("Accessors") {
    it("should support parsing named accessors") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> () { .foo }"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
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
        "the_answer :: fn() => (i64) { 42 }"
      );

      check(status);
      check(value_as_i64(status)->bits == 42);
    }

    it("should be able to to do nested compile time calls") {
      Value *result = test_program_inline_source_base(
        "RESULT", &test_context,
        "RESULT :: get_a();"
        "B :: get_b();"
        "get_a :: fn() => (i64) { B };"
        "get_b :: fn() => (i64) { 42 }"
      );

      check(spec_check_mass_result(test_context.result));
      check(value_as_i64(result)->bits == 42);
    }

    it("should support static exact params in compile-time fns") {
      Value *result = test_program_inline_source_base(
        "RESULT", &test_context,
        "RESULT :: foo(42)\n"
        "foo :: fn(x :: 42) => (x) { x }"
      );

      check(spec_check_mass_result(test_context.result));
      check(value_as_i64(result)->bits == 42);
    }

    it("should support static generic params in compile-time fns") {
      Value *result = test_program_inline_source_base(
        "RESULT", &test_context,
        "RESULT :: foo(\"bar\").length + foo(39)\n"
        "foo :: fn(@x) => (x) { x }"
      );

      check(spec_check_mass_result(test_context.result));
      check(value_as_i64(result)->bits == 42);
    }

    it("should consider a value compile-time known if all the statements are") {
      Value *result = test_program_inline_source_base(
        "RESULT", &test_context,
        "RESULT :: {1;2;3;42}\n"
      );

      check(spec_check_mass_result(test_context.result));
      check(value_as_i64(result)->bits == 42);
    }

    it("should support compile time blocks") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (s64) { @{ 20 + 22 } }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to use if / else to choose an implementation at compile time") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "TEST :: if true then 42 else 1000\n"
      );

      check(value_is_i64(value));
      const i64 *literal = value_as_i64(value);
      check(literal->bits == 42);
    }

    it("should be able combine if / else, inline modules and `using` for conditional definitions") {
      Value *value = test_program_inline_source_base(
        "TEST", &test_context,
        "using if true then module { TEST :: 42 } else module { TEST :: 1000 }\n"
      );

      check(value_is_i64(value));
      const i64 *literal = value_as_i64(value);
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

    it("should support compile-time arithmetic operations") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "RESULT :: 40 + 1 + 1\n"
        "checker :: fn() -> (i64) { RESULT }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support compile-time comparsion operations") {
      bool (*checker)() = (bool (*)())test_program_inline_source_function(
        "checker", &test_context,
        "RESULT :: 42 == 42\n"
        "checker :: fn() -> (bool) { RESULT }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker());
    }

    it("should support defining new types at compile time") {
      u64 (*checker)() = (u64 (*)())test_program_inline_source_function(
        "checker", &test_context,
        "i64x2 :: type_id_test(i64 * 2)\n"
        "type_id_test :: fn(type : Type) => _ intrinsic { arguments.0 }\n"
        "checker :: fn() -> (i64) { x : i64x2 = [1, 2]; x.1 }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 2);
    }
  }

  describe("C Enums") {
    it("should get a value from a type-based module defined in an intrinsic") {
      u64 (*checker)() = (u64 (*)())test_program_inline_source_function(
        "checker", &test_context,
        "SomeEnum :: c_enum(i64, [ .Foo = 42 ])\n"
        "accepts_some_enum :: fn(x : SomeEnum) -> (i64) { cast(i64, x) }\n"
        "checker :: fn() -> (i64) { accepts_some_enum(SomeEnum.Foo) }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should support equality") {
      bool (*checker)() = (bool (*)())test_program_inline_source_function(
        "checker", &test_context,
        "SomeEnum :: c_enum(i64, [ .Foo = 42 ])\n"
        "checker :: fn() -> (bool) { x := SomeEnum.Foo; x == .Foo }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker());
    }
  }

  describe("Strings") {
    it("should accept string arguments") {
      const char *(*checker)(Slice) = (const char *(*)(Slice))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn(string : String) -> (&i8) {\n"
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
    it("should be able to accept a string-like argument at compile time") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn(input : String) => (i64) {\n"
          "input.length"
        "}\n"
        "checker :: fn() -> (i64) {\n"
          "foo([\"foo\".bytes, 3])"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 3);
    }
  }

  describe("Fixed Size Arrays") {
    it("should be able to define a variable with a fixed size array type") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (i64) {"
          "foo : i64 * 64;"
          "foo.1 = 42;"
          "foo.1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to use multidimensional arrays") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (i64) {"
          "foo : i64 * 4 * 4;"
          "foo.1.1 = 42;"
          "foo.1.1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should be able to use a dynamic i64 variable to index an array") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "helper :: fn(x : i64) -> (i64) {\n"
          "foo : i64 * 64;\n"
          "foo.(x) = 42\n"
          "foo.(x)\n"
        "}\n"
        "test :: fn() -> (i64) {\n"
          "helper(2)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should support initializing a fixed-size array from a tuple") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (i64) {"
          "foo : i64 * 3 = [11, 42, 600]\n"
          "foo.1"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
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

    it("should be able to parse unnamed tuple structs and access their elements") {
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

    it("should allow assigning a tuple with in-order named fields to a struct") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p := Point [.x = 20, .y = 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should allow assigning a tuple with out-of-order named fields to a struct") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p := Point [.y = 22, .x = 20]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should allow assigning a tuple with shorthand named fields") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {\n"
          "x := s32(20)\n"
          "p := Point [.x, .y = 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should allow a tuple with a mix of named and unnamed fields to a struct") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p := Point [.x = 20, 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when assigned tuple has duplicate fields") {
      test_program_inline_source_base(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p := Point [.x = 20, .x = 22]\n"
          "p.x + p.y"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Redefinition);
    }

    it("should be able to handle named fields in inferred tuple structs") {
      s64(*checker)(void) = (s64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) {"
          "p := [.x = 20, .y = 22]\n"
          "p.x + p.y"
        "}"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }

    it("should report an error when inferred struct tuple type has duplicate fields") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> (s32) {"
          "p := [.x = 20, .x = 22]\n"
          "p.x"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Redefinition);
    }

    it("should allow assigning a tuple with anonymous fields to a struct") {
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

    it("should support defining a compile-time struct") {
      s32(*checker)(void) = (s32(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "Point :: c_struct [x : s32, y : s32]\n"
        "test :: fn() -> (s32) {"
          "p :: Point [31, 11]\n"
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
          "p := Point [20, 22]\n"
          "pointer := &p\n"
          "dereferenced := pointer.*\n"
          "dereferenced.x + dereferenced.y\n"
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

    it("should support using tuple as a type") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "test", &test_context,
        "tuple_id :: fn(x : [i64]) -> (i64) { x.0 }\n"
        "test :: fn() -> (i64) {"
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

    it("should report an error when encountering duplicate fields in a tuple as a type") {
      test_program_inline_source_function(
        "test", &test_context,
        "test :: fn() -> (s64) {"
          "tuple : [foo : s64, foo : s64] = [42, 42]\n"
          "tuple.foo"
        "}"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_Redefinition);
    }

    it("should report an error when tuple is assigned to something that is not a struct") {
      test_program_inline_source_base(
        "test", &test_context,
        "test :: fn() -> (s32) {"
          "p := s32(0)\n"
          "p = [20]\n"
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
          "p := Point[20]\n"
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
          "p := Point[20, 1, 1]\n"
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
      check(error->tag == Mass_Error_Tag_Type_Mismatch);
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
      u64(*checker)(Test_128bit*) = (u64(*)(Test_128bit*))test_program_inline_source_function(
        "checker", &test_context,
        "Test_128bit :: c_struct [ x : i64, y : i64 ]\n"
        "checker :: fn(input : &Test_128bit) -> (i64) {\n"
          "input.y\n"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = {.x = 21, .y = 42};
      check(checker(&test_128bit));
    }

    it("should support passing arguments that are not power-of-2size and fit into a register") {
      u8(*checker)(Test_24bit) = (u8(*)(Test_24bit))test_program_inline_source_function(
        "checker", &test_context,
        "Test_24bit :: c_struct [ x : i8, y : i8, z : i8 ]\n"
        "checker :: fn(input : Test_24bit) -> (i8) { input.y }"
      );
      check(spec_check_mass_result(test_context.result));

      Test_24bit test_24bit = { .x = 3, .y = 42, .z = 78 };
      check(checker(test_24bit) == 42);
    }

    it("should support returning values that are not power-of-2 size and fit into a register") {
      Test_24bit(*checker)() = (Test_24bit(*)())test_program_inline_source_function(
        "checker", &test_context,
        "Test_24bit :: c_struct [ x : i8, y : i8, z : i8 ]\n"
        "checker :: fn() -> (Test_24bit) { [ cast(i8, 3), cast(i8, 42), cast(i8, 78) ] }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker().y == 42);
    }

    it("should be able to return 128bit structs while accepting other arguments") {
      Test_128bit(*checker)(u64, u64) = (Test_128bit(*)(u64, u64))test_program_inline_source_function(
        "return_struct", &test_context,
        "Test_128bit :: c_struct [ x : i64, y : i64 ]\n"
        "return_struct :: fn(x : i64, y : i64) -> (Test_128bit) {"
          "result : Test_128bit;"
          "result.x = x;"
          "result.y = y;"
          "result"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = checker(42, 21);
      check(test_128bit.x == 42);
      check(test_128bit.y == 21);
    }

    it("should correctly handle 128bit struct argument fields as arguments to another call") {
      u64(*checker)(Test_128bit) = (u64(*)(Test_128bit))test_program_inline_source_function(
        "checker", &test_context,
        "Test_128bit :: c_struct [ x : u64, y : u64 ]\n"
        "test_sum :: fn(x : u64, y : u64) -> (u64) { x + y }\n"
        "checker :: fn(x : Test_128bit) -> (u64) {"
          "test_sum(x.x, x.y)"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_128bit test_128bit = { .x = 20, .y = 22 };
      check(checker(test_128bit) == 42);
    }

    // Both System_V and win64 will pass 7th argument on the stack
    it("should be able to use a 128bit struct passed as the 7th arguments") {
      u64(*checker)(u8, u8, u8, u8, u8, u8, Test_128bit) =
        (u64(*)(u8, u8, u8, u8, u8, u8, Test_128bit))test_program_inline_source_function(
          "foo", &test_context,
          "Test_128bit :: c_struct [ x : i64, y : i64 ]\n"
          "foo :: fn(x1: i8, x2 : i8, x3 : i8, x4 : i8, x5 : i8, x6 : i8, x7 : Test_128bit ) -> (i64)"
          "{ x7.y + x7.x }"
        );
      check(spec_check_mass_result(test_context.result));
      Test_128bit test_128bit = { .x = 32, .y = 10 };
      check(checker(1, 2, 3, 4, 5, 6, test_128bit) == 42);
    }

    it("should correctly handle 192bit struct arguments") {
      u64(*checker)(Test_192bit) = (u64(*)(Test_192bit))test_program_inline_source_function(
        "checker", &test_context,
        "Test_192bit :: c_struct [ x : u64, y : u64, z : u64 ]\n"
        "checker :: fn(x : Test_192bit) -> (u64) {"
          "x.x + x.y + x.z"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_192bit input = { .x = 11, .y = 21, .z = 10 };
      check(checker(input) == 42);
    }

    it("should correctly handle 192bit struct return type") {
      Test_192bit(*checker)() = (Test_192bit(*)())test_program_inline_source_function(
        "checker", &test_context,
        "Test_192bit :: c_struct [ x : u64, y : u64, z : u64 ]\n"
        "checker :: fn() -> (Test_192bit) {"
          "[.x = 11, .y = 21, .z = 10]"
        "}"
      );
      check(spec_check_mass_result(test_context.result));

      Test_192bit output = checker();
      check(output.x == 11);
      check(output.y == 21);
      check(output.z == 10);
    }

    it("should correctly handle Label storage with an offset ") {
      Expected_Result(*checker)() = (Expected_Result(*)())test_program_inline_source_function(
        "checker", &test_context,
        "proxy :: fn() => (MASS.Expected_Result) {\n"
          "i64_descriptor :: type_descriptor(i64) \n"
          "temp_storage := MASS.storage_register(MASS.Register.A, i64_descriptor.bit_size)\n"
          "expected := MASS.expected_result_exact(i64_descriptor, temp_storage)\n"
          "expected"
        "}\n"
        "checker :: fn() -> (MASS.Expected_Result) { proxy() }"
      );
      check(spec_check_mass_result(test_context.result));

      Storage temp_storage = storage_register(Register_A, (Bits){64});
      Expected_Result from_c = mass_expected_result_exact(&descriptor_i64, temp_storage);
      Expected_Result from_mass = checker();
      check(from_c.tag == Expected_Result_Tag_Exact);
      check(from_c.tag == from_mass.tag);
      check(storage_equal(&from_c.Exact.storage, &temp_storage));
      check(same_type(from_c.Exact.descriptor,from_mass.Exact.descriptor));
    }

    it("should be able to access fields of a returned struct") {
      u64(*checker)() = (u64(*)())test_program_inline_source_function(
        "checker", &test_context,
        "foo :: fn() -> (String) { \"foo\" }\n"
        "checker :: fn() -> (i64) { foo().length }"
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
        "checker :: fn() -> () { static_assert(false, \"Oops\") }\n"
      );
      check(test_context.result->tag == Mass_Result_Tag_Error);
      Mass_Error *error = &test_context.result->Error.error;
      check(error->tag == Mass_Error_Tag_User_Defined);
      spec_check_slice(error->User_Defined.name, slice_literal("Static Assert Failed"));
      spec_check_slice(error->detailed_message, slice_literal("Oops"));
    }
    xit("should support runtime asserts") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> () { assert(false, \"Oops\") }\n"
      );
      check(spec_check_mass_result(test_context.result));
      checker();
    }
  }

  describe("Modules") {
    it("should support importing modules") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "sample_module :: import(\"fixtures/sample_module\")\n"
        "checker :: fn() -> (i64) { sample_module.the_answer }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == 42);
    }
    it("should only import module once") {
      bool(*checker)(void) = (bool(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "A :: import(\"fixtures/foo/../sample_module\")\n"
        "B :: import(\"fixtures\\\\sample_module\")\n"
        "checker :: fn() -> (bool) { &A == &B }"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker());
    }

    it("should support inline modules") {
      u64(*checker)(void) = (u64(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (i64) {"
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
        "checker :: fn() -> (s64) {"
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
      program_load_file_module_into_root_scope(&test_context, slice_literal("std/prelude"));
      check(spec_check_mass_result(test_context.result));
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
        "main", &test_context, "../compile-time-execution-benchmark/folding"
      );
      check(test_program->entry_point);
      Array_Resolved_Function_Parameter params = dyn_array_static_empty(Array_Resolved_Function_Parameter);
      ensure_function_instance(&test_context, test_program->entry_point, params);
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/folding.exe"), &test_context, Executable_Type_Cli);
    }

    xit("should parse and write an executable with a 1 million function calls") {
      Program *test_program = test_context.program;
      test_program->entry_point = test_program_external_source_base(
        "main", &test_context, "../compile-time-execution-benchmark/print"
      );
      check(test_program->entry_point);
      Array_Resolved_Function_Parameter params = dyn_array_static_empty(Array_Resolved_Function_Parameter);
      ensure_function_instance(&test_context, test_program->entry_point, params);
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
      check(spec_check_mass_result(test_context.result));
      check(test_program->entry_point);
      Array_Resolved_Function_Parameter params = dyn_array_static_empty(Array_Resolved_Function_Parameter);
      ensure_function_instance(&test_context, test_program->entry_point, params);
      check(spec_check_mass_result(test_context.result));
      write_executable(slice_literal("build/relocations.exe"), &test_context, Executable_Type_Cli);
    }
  }

  #if defined(_WIN32) || defined(__linux__)
  describe("Standard Library") {
    it("should support virtual memory allocation") {
      void *(*checker)(void) = (void *(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> _ {"
          "vm :: import(\"std/virtual_memory\")\n"
          "vm.allocate(4096, .READ_WRITE)"
        "}\n"
      );
      check(spec_check_mass_result(test_context.result));
      void *memory = checker();
      check(memory);
      *(u8 *)memory = 42;
      check(((u8 *)memory)[0] == 42);
    }
  }
  #endif

  #if defined(_WIN32)
  describe("Windows") {
    it("should print a stack trace on hardware exception") {
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
    it("should be able to determine the OS type") {
      Os(*checker)(void) = (Os(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (Os) { get_target_os()}\n"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == Os_Windows);
    }
  }
  #endif

  #if defined(__linux__)
  describe("Linux") {
    it("should be able to determine the OS type") {
      Os(*checker)(void) = (Os(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "checker :: fn() -> (Os) { get_target_os()}\n"
      );
      check(spec_check_mass_result(test_context.result));
      check(checker() == Os_Linux);
    }
  }
  #endif

  describe("External (DLL Imports)") {
    #if defined(__linux__)
    it("should be able to print out a string") {
      void(*checker)(void) = (void(*)(void))test_program_inline_source_function(
        "checker", &test_context,
        "write :: fn(descriptor : s32, buffer : &i8, size : i64) "
          "-> (i64) external(\"libc.so.6\", \"write\")\n"
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
        "checker :: fn() -> () { broken_import() }\n"
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
