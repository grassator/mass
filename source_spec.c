#include "bdd-for-c.h"
#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"


spec("source") {
  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_create_allocator(temp_buffer);
  }

  after_each() {
    bucket_buffer_destroy(temp_buffer);
  }

  it("should be able to tokenize an empty string") {
    Slice source = slice_from_string_literal("");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->parent == 0);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 0);
  }

  it("should be able to tokenize a comment") {
    Slice source = slice_from_string_literal("// foo\n");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
    check(root->parent == 0);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 0);
  }

  it("should be able to tokenize a sum of integers") {
    Slice source = slice_from_string_literal("12 + foo123");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 3);
    check(slice_equal(root->source, source));

    Token *a_num = *dyn_array_get(root->children, 0);
    check(a_num->type == Token_Type_Integer);
    check(slice_equal(a_num->source, slice_from_string_literal("12")));

    Token *plus = *dyn_array_get(root->children, 1);
    check(plus->type == Token_Type_Operator);
    check(slice_equal(plus->source, slice_from_string_literal("+")));

    Token *id = *dyn_array_get(root->children, 2);
    check(id->type == Token_Type_Id);
    check(slice_equal(id->source, slice_from_string_literal("foo123")));
  }

  it("should be able to tokenize groups") {
    Slice source = slice_from_string_literal("(x)");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);

    Token *paren = *dyn_array_get(root->children, 0);
    check(paren->type == Token_Type_Paren);
    check(dyn_array_length(paren->children) == 1);
    check(slice_equal(paren->source, slice_from_string_literal("(x)")));

    Token *id = *dyn_array_get(paren->children, 0);
    check(id->type == Token_Type_Id);
  }

  it("should be able to tokenize strings") {
    Slice source = slice_from_string_literal("\"foo 123\"");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);
    Token *string = *dyn_array_get(root->children, 0);
    check(slice_equal(string->source, slice_from_string_literal("\"foo 123\"")));
  }

  it("should be able to tokenize nested groups with different braces") {
    Slice source = slice_from_string_literal("{[]}");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(dyn_array_length(root->children) == 1);

    Token *curly = *dyn_array_get(root->children, 0);
    check(curly->type == Token_Type_Curly);
    check(dyn_array_length(curly->children) == 1);
    check(slice_equal(curly->source, slice_from_string_literal("{[]}")));

    Token *square = *dyn_array_get(curly->children, 0);
    check(square->type == Token_Type_Square);
    check(dyn_array_length(square->children) == 0);
    check(slice_equal(square->source, slice_from_string_literal("[]")));
  }

  it("should be able to tokenize complex input") {
    Slice source = slice_from_string_literal(
      "foo :: (x: s8) -> {\n"
      "  return x + 3;\n"
      "}"
    );
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Success);
    Token *root = result.root;
    check(root);
  }

  it("should report a failure when encountering a brace that is not closed") {
    Slice source = slice_from_string_literal("(foo");
    Tokenizer_Result result = tokenize("_test_.mass", source);
    check(result.type == Tokenizer_Result_Type_Error);
    check(dyn_array_length(result.errors) == 1);
    Tokenizer_Error *error = dyn_array_get(result.errors, 0);
    check(strcmp(error->location.filename, "_test_.mass") == 0);
    check(error->location.line, 1);
    check(error->location.column, 4);
    check(strcmp(error->message, "Unexpected end of file. Expected a closing brace.") == 0);

    //print_message_with_location(error->message, &error->location);
  }
}
