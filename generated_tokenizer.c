/* Generated by re2c */
#line 1 "tokenizer.re.c"
// If you want to make changes to the tokenizer, edit tokenizer.re.c
// and then regenerate this file on a Linux machine / WSL:
//
// re2c tokenizer.re.c -o generated_tokenizer.c --no-version --no-generation-date
//
#include "types.h"

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  Compilation *compilation,
  Source_Range source_range,
  Value_View *out_tokens
) {
  Slice input = source_from_source_range(compilation, &source_range);

  const Allocator *allocator = compilation->allocator;

  Array_Value_Ptr stack = dyn_array_make(Array_Value_Ptr, .capacity = 100);
  Array_Tokenizer_Parent parent_stack =
    dyn_array_make(Array_Tokenizer_Parent, .capacity = 16);

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

  Fixed_Buffer *string_buffer = fixed_buffer_make(.capacity = 4096);

  u64 offset = 0;
  u64 marker = 0;
  u64 token_start_offset = 0;

  #define TOKENIZER_CURRENT_SLICE()\
    slice_sub(input, token_start_offset, offset)

  #define TOKENIZER_CURRENT_RANGE()\
    (Source_Range){\
      .file = source_range.file,\
      .offsets = {\
        .from = u64_to_u32(token_start_offset),\
        .to = u64_to_u32(offset),\
      }\
    }

  #define TOKENIZER_PUSH_LITERAL(_BASE_, _SLICE_)\
    dyn_array_push(stack, \
      value_i64(allocator, (_SLICE_), (_BASE_), TOKENIZER_CURRENT_RANGE())\
    )

  #define TOKENIZER_HANDLE_ERROR(_EXPECTED_SLICE_)\
    do {\
      result = (Mass_Result) {\
        .tag = Mass_Result_Tag_Error,\
        .Error.error = {\
          .tag = Mass_Error_Tag_Unexpected_Token,\
          .Unexpected_Token = { .expected = (_EXPECTED_SLICE_), },\
          .source_range = {\
            .file = source_range.file,\
            .offsets = {.from = u64_to_u32(offset) - 1, .to = u64_to_u32(offset) - 1},\
          }\
        }\
      };\
      goto done;\
    } while (0)

  #define TOKENIZER_PUSH_SYMBOL(_TYPE_)\
    dyn_array_push(stack, \
      token_make_symbol_value(\
        compilation, TOKENIZER_CURRENT_SLICE(), TOKENIZER_CURRENT_RANGE()\
      )\
    )

  #define TOKENIZER_GROUP_START(_VARIANT_)\
    tokenizer_group_start(\
      allocator, &stack, &parent_stack, &descriptor_group_##_VARIANT_, TOKENIZER_CURRENT_RANGE()\
    )

  #define TOKENIZER_GROUP_END(_VARIANT_)\
    if (!tokenizer_group_end_##_VARIANT_(compilation, &stack, &parent_stack, offset))\
      TOKENIZER_HANDLE_ERROR((Slice){0})

  for (;;) {
    token_start_offset = offset;
    
#line 85 "generated_tokenizer.c"
{
  char yych;
  unsigned int yyaccept = 0;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\t':
  case '\v':
  case ' ':  goto yy4;
  case '\n':  goto yy7;
  case '\r':  goto yy9;
  case '!':
  case '$':
  case '%':
  case '&':
  case '*':
  case '+':
  case ',':
  case '-':
  case '.':
  case ':':
  case ';':
  case '<':
  case '=':
  case '>':
  case '?':
  case '@':
  case '\\':
  case '^':
  case '|':
  case '~':  goto yy10;
  case '"':  goto yy13;
  case '\'':  goto yy14;
  case '(':  goto yy15;
  case ')':  goto yy17;
  case '/':  goto yy19;
  case '0':  goto yy20;
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':  goto yy22;
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
  case 'G':
  case 'H':
  case 'I':
  case 'J':
  case 'K':
  case 'L':
  case 'M':
  case 'N':
  case 'O':
  case 'P':
  case 'Q':
  case 'R':
  case 'S':
  case 'T':
  case 'U':
  case 'V':
  case 'W':
  case 'X':
  case 'Y':
  case 'Z':
  case '_':
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
  case 'g':
  case 'h':
  case 'i':
  case 'j':
  case 'k':
  case 'l':
  case 'm':
  case 'n':
  case 'o':
  case 'p':
  case 'q':
  case 'r':
  case 's':
  case 't':
  case 'u':
  case 'v':
  case 'w':
  case 'x':
  case 'y':
  case 'z':  goto yy24;
  case '[':  goto yy27;
  case ']':  goto yy29;
  case '{':  goto yy31;
  case '}':  goto yy33;
  default:
    if (offset >= input.length) goto yy54;
    goto yy2;
  }
yy2:
  ++offset;
yy3:
#line 151 "tokenizer.re.c"
  { TOKENIZER_HANDLE_ERROR((Slice){0}); }
#line 197 "generated_tokenizer.c"
yy4:
  yyaccept = 0;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\t':
  case '\v':
  case ' ':  goto yy4;
  case '/':  goto yy35;
  default:  goto yy6;
  }
yy6:
#line 145 "tokenizer.re.c"
  { continue; }
#line 213 "generated_tokenizer.c"
yy7:
  ++offset;
yy8:
#line 127 "tokenizer.re.c"
  {
        token_start_offset = offset; // :FakeSemicolon
        tokenizer_maybe_push_fake_semicolon(
          compilation, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }
#line 225 "generated_tokenizer.c"
yy9:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\n':  goto yy7;
  default:  goto yy8;
  }
yy10:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
yy11:
  switch (yych) {
  case '!':
  case '$':
  case '%':
  case '&':
  case '*':
  case '+':
  case ',':
  case '-':
  case '.':
  case '/':
  case ':':
  case ';':
  case '<':
  case '=':
  case '>':
  case '?':
  case '@':
  case '\\':
  case '^':
  case '|':
  case '~':  goto yy10;
  default:  goto yy12;
  }
yy12:
#line 124 "tokenizer.re.c"
  { TOKENIZER_PUSH_SYMBOL(); continue; }
#line 264 "generated_tokenizer.c"
yy13:
  yyaccept = 1;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  if (yych <= 0x00) {
    if (offset >= input.length) goto yy3;
    goto yy37;
  }
  goto yy38;
yy14:
  ++offset;
  goto yy12;
yy15:
  ++offset;
#line 116 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(paren); continue; }
#line 282 "generated_tokenizer.c"
yy17:
  ++offset;
#line 119 "tokenizer.re.c"
  { TOKENIZER_GROUP_END(paren); continue; }
#line 287 "generated_tokenizer.c"
yy19:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case 0x00:  goto yy12;
  case '/':  goto yy42;
  default:  goto yy11;
  }
yy20:
  yyaccept = 2;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case 'B':
  case 'b':  goto yy44;
  case 'X':
  case 'x':  goto yy45;
  default:  goto yy21;
  }
yy21:
#line 98 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_10, digits);
        continue;
      }
#line 315 "generated_tokenizer.c"
yy22:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':  goto yy22;
  default:  goto yy21;
  }
yy24:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
  case 'G':
  case 'H':
  case 'I':
  case 'J':
  case 'K':
  case 'L':
  case 'M':
  case 'N':
  case 'O':
  case 'P':
  case 'Q':
  case 'R':
  case 'S':
  case 'T':
  case 'U':
  case 'V':
  case 'W':
  case 'X':
  case 'Y':
  case 'Z':
  case '_':
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
  case 'g':
  case 'h':
  case 'i':
  case 'j':
  case 'k':
  case 'l':
  case 'm':
  case 'n':
  case 'o':
  case 'p':
  case 'q':
  case 'r':
  case 's':
  case 't':
  case 'u':
  case 'v':
  case 'w':
  case 'x':
  case 'y':
  case 'z':  goto yy24;
  default:  goto yy26;
  }
yy26:
#line 148 "tokenizer.re.c"
  { TOKENIZER_PUSH_SYMBOL(); continue; }
#line 404 "generated_tokenizer.c"
yy27:
  ++offset;
#line 117 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(square); continue; }
#line 409 "generated_tokenizer.c"
yy29:
  ++offset;
#line 120 "tokenizer.re.c"
  { TOKENIZER_GROUP_END(square); continue; }
#line 414 "generated_tokenizer.c"
yy31:
  ++offset;
#line 118 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(curly); continue; }
#line 419 "generated_tokenizer.c"
yy33:
  ++offset;
#line 121 "tokenizer.re.c"
  { TOKENIZER_GROUP_END(curly); continue; }
#line 424 "generated_tokenizer.c"
yy35:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '/':  goto yy46;
  default:  goto yy36;
  }
yy36:
  offset = marker;
  switch (yyaccept) {
  case 0:
    goto yy6;
  case 1:
    goto yy3;
  default:
    goto yy21;
  }
yy37:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
yy38:
  switch (yych) {
  case '"':  goto yy39;
  case '\\':  goto yy41;
  default:
    if (offset >= input.length) goto yy36;
    goto yy37;
  }
yy39:
  ++offset;
#line 135 "tokenizer.re.c"
  {
        Slice raw_bytes = slice_sub(input, token_start_offset + 1, offset - 1);
        tokenizer_push_string_literal(
          compilation, &string_buffer, &stack, raw_bytes, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }
#line 463 "generated_tokenizer.c"
yy41:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  if (yych <= 0x00) {
    if (offset >= input.length) goto yy36;
    goto yy37;
  }
  goto yy37;
yy42:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\n':
  case '\r':  goto yy12;
  case '!':
  case '$':
  case '%':
  case '&':
  case '*':
  case '+':
  case ',':
  case '-':
  case '.':
  case '/':
  case ':':
  case ';':
  case '<':
  case '=':
  case '>':
  case '?':
  case '@':
  case '\\':
  case '^':
  case '|':
  case '~':  goto yy42;
  default:
    if (offset >= input.length) goto yy12;
    goto yy46;
  }
yy44:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':  goto yy48;
  default:  goto yy36;
  }
yy45:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':  goto yy51;
  default:  goto yy36;
  }
yy46:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\n':
  case '\r':  goto yy6;
  default:
    if (offset >= input.length) goto yy6;
    goto yy46;
  }
yy48:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':  goto yy48;
  default:  goto yy50;
  }
yy50:
#line 104 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_2, digits);
        continue;
      }
#line 564 "generated_tokenizer.c"
yy51:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':  goto yy51;
  default:  goto yy53;
  }
yy53:
#line 110 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_16, digits);
        continue;
      }
#line 600 "generated_tokenizer.c"
yy54:
#line 94 "tokenizer.re.c"
  { break; }
#line 604 "generated_tokenizer.c"
}
#line 152 "tokenizer.re.c"

  }

  if (dyn_array_length(parent_stack)) {
    offset++;
    TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a closing brace.");
  }

  #undef TOKENIZER_CURRENT_SLICE
  #undef TOKENIZER_CURRENT_RANGE
  #undef TOKENIZER_HANDLE_ERROR
  #undef TOKENIZER_PUSH_SYMBOL
  #undef TOKENIZER_PUSH_LITERAL

  done:
  if (result.tag == Mass_Result_Tag_Success) {
    Source_Range children_range = {
      .file = source_range.file,
      .offsets = {.from = 0, .to = u64_to_u32(input.length)},
    };
    *out_tokens = temp_token_array_into_value_view(
      allocator, dyn_array_raw(stack), u64_to_u32(dyn_array_length(stack)), children_range
    );
  }
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(stack);
  dyn_array_destroy(parent_stack);
  return result;
}

