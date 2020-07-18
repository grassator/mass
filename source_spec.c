#include "bdd-for-c.h"
#include "source.c"


spec("source") {
  before_each() {
    temp_buffer = fixed_buffer_make(&allocator_system, 10 * 1024 * 1024);
    temp_allocator = fixed_buffer_create_allocator(temp_buffer);
  }

  after_each() {
    fixed_buffer_destroy(temp_buffer);
  }

  it("should be able to tokenize an empty string") {
    Slice source = slice_from_string_literal("");
    Token *root = tokenize(source);
    check(root);
    check(root->parent == 0);
    check(root->type == Token_Type_Module);
    check(dyn_array_length(root->children) == 0);
  }

  it("should be able to tokenize a sum of integers") {
    Slice source = slice_from_string_literal("12 + foo123");
    Token *root = tokenize(source);
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
    Token *root = tokenize(source);
    check(dyn_array_length(root->children) == 1);

    Token *paren = *dyn_array_get(root->children, 0);
    check(paren->type == Token_Type_Paren);
    check(dyn_array_length(paren->children) == 1);
    check(slice_equal(paren->source, slice_from_string_literal("(x)")));

    Token *id = *dyn_array_get(paren->children, 0);
    check(id->type == Token_Type_Id);
  }

  it("should be able to tokenize complex expressions") {
    Slice source = slice_from_string_literal("(42 + (foo + 123 + 1423))");
    Token *root = tokenize(source);
    check(root);
  }
}
