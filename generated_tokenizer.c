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
  Source_File *file,
  Value_View *out_tokens
) {
  Slice input = file->text;
  if (input.length > UINT32_MAX) {
    return mass_error((Mass_Error) {
      .tag = Mass_Error_Tag_File_Too_Large,
      .File_Too_Large = { .path = file->path },
      .source_range = { .file = file, .offsets = {.from = UINT32_MAX, .to = UINT32_MAX} },
    });
  }

  const Allocator *allocator = compilation->allocator;
  assert(!dyn_array_is_initialized(file->line_ranges));
  file->line_ranges = dyn_array_make(Array_Range_u64, .capacity = file->text.length / 40);

  Range_u64 current_line = {0};

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
      .file = file,\
      .offsets = {\
        .from = u64_to_u32(token_start_offset),\
        .to = u64_to_u32(offset),\
      }\
    }

  #define TOKENIZER_PUSH_LITERAL(_BASE_, _SLICE_)\
    dyn_array_push(stack, \
      value_number_literal(allocator, (_SLICE_), (_BASE_), TOKENIZER_CURRENT_RANGE())\
    )

  #define TOKENIZER_HANDLE_ERROR(_EXPECTED_SLICE_)\
    do {\
      result = (Mass_Result) {\
        .tag = Mass_Result_Tag_Error,\
        .Error.error = {\
          .tag = Mass_Error_Tag_Unexpected_Token,\
          .Unexpected_Token = { .expected = (_EXPECTED_SLICE_), },\
          .source_range = {\
            .file = file,\
            .offsets = {.from = u64_to_u32(offset) - 1, .to = u64_to_u32(offset) - 1},\
          }\
        }\
      };\
      goto done;\
    } while (0)

  #define TOKENIZER_PUSH_SYMBOL(_TYPE_)\
    dyn_array_push(stack, \
      token_make_symbol(allocator, TOKENIZER_CURRENT_SLICE(), (_TYPE_), TOKENIZER_CURRENT_RANGE())\
    )

  #define TOKENIZER_GROUP_START(_TAG_)\
    tokenizer_group_start(allocator, &stack, &parent_stack, (_TAG_), TOKENIZER_CURRENT_RANGE())

  #define TOKENIZER_GROUP_END(_PAREN_)\
    if (!tokenizer_group_end(allocator, &stack, &parent_stack, (_PAREN_), offset))\
      TOKENIZER_HANDLE_ERROR((Slice){0})

  for (;;) {
    token_start_offset = offset;
    
#line 92 "generated_tokenizer.c"
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
  case '(':  goto yy14;
  case ')':  goto yy16;
  case '/':  goto yy18;
  case '0':  goto yy19;
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':  goto yy21;
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
  case 'z':  goto yy23;
  case '[':  goto yy26;
  case ']':  goto yy28;
  case '{':  goto yy30;
  case '}':  goto yy32;
  default:
    if (offset >= input.length) goto yy53;
    goto yy2;
  }
yy2:
  ++offset;
yy3:
#line 161 "tokenizer.re.c"
  { TOKENIZER_HANDLE_ERROR((Slice){0}); }
#line 203 "generated_tokenizer.c"
yy4:
  yyaccept = 0;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\t':
  case '\v':
  case ' ':  goto yy4;
  case '/':  goto yy34;
  default:  goto yy6;
  }
yy6:
#line 155 "tokenizer.re.c"
  { continue; }
#line 219 "generated_tokenizer.c"
yy7:
  ++offset;
yy8:
#line 134 "tokenizer.re.c"
  {
        current_line.to = offset;
        dyn_array_push(file->line_ranges, current_line);
        current_line.from = current_line.to;
        token_start_offset = offset; // :FakeSemicolon
        tokenizer_maybe_push_fake_semicolon(
          allocator, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }
#line 234 "generated_tokenizer.c"
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
#line 131 "tokenizer.re.c"
  { TOKENIZER_PUSH_SYMBOL(Symbol_Type_Operator_Like); continue; }
#line 273 "generated_tokenizer.c"
yy13:
  yyaccept = 1;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  if (yych <= 0x00) {
    if (offset >= input.length) goto yy3;
    goto yy36;
  }
  goto yy37;
yy14:
  ++offset;
#line 123 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(Group_Tag_Paren); continue; }
#line 288 "generated_tokenizer.c"
yy16:
  ++offset;
#line 126 "tokenizer.re.c"
  { TOKENIZER_GROUP_END(')'); continue; }
#line 293 "generated_tokenizer.c"
yy18:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case 0x00:  goto yy12;
  case '/':  goto yy41;
  default:  goto yy11;
  }
yy19:
  yyaccept = 2;
  ++offset;
  marker = offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case 'B':
  case 'b':  goto yy43;
  case 'X':
  case 'x':  goto yy44;
  default:  goto yy20;
  }
yy20:
#line 105 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_10, digits);
        continue;
      }
#line 321 "generated_tokenizer.c"
yy21:
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
  case '9':  goto yy21;
  default:  goto yy20;
  }
yy23:
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
  case 'z':  goto yy23;
  default:  goto yy25;
  }
yy25:
#line 158 "tokenizer.re.c"
  { TOKENIZER_PUSH_SYMBOL(Symbol_Type_Id_Like); continue; }
#line 410 "generated_tokenizer.c"
yy26:
  ++offset;
#line 124 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(Group_Tag_Square); continue; }
#line 415 "generated_tokenizer.c"
yy28:
  ++offset;
#line 127 "tokenizer.re.c"
  { TOKENIZER_GROUP_END(']'); continue; }
#line 420 "generated_tokenizer.c"
yy30:
  ++offset;
#line 125 "tokenizer.re.c"
  { TOKENIZER_GROUP_START(Group_Tag_Curly); continue; }
#line 425 "generated_tokenizer.c"
yy32:
  ++offset;
#line 128 "tokenizer.re.c"
  { TOKENIZER_GROUP_END('}'); continue; }
#line 430 "generated_tokenizer.c"
yy34:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '/':  goto yy45;
  default:  goto yy35;
  }
yy35:
  offset = marker;
  switch (yyaccept) {
  case 0:
    goto yy6;
  case 1:
    goto yy3;
  default:
    goto yy20;
  }
yy36:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
yy37:
  switch (yych) {
  case '"':  goto yy38;
  case '\\':  goto yy40;
  default:
    if (offset >= input.length) goto yy35;
    goto yy36;
  }
yy38:
  ++offset;
#line 145 "tokenizer.re.c"
  {
        Slice raw_bytes = slice_sub(input, token_start_offset + 1, offset - 1);
        tokenizer_push_string_literal(
          compilation, &string_buffer, &stack, raw_bytes, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }
#line 469 "generated_tokenizer.c"
yy40:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  if (yych <= 0x00) {
    if (offset >= input.length) goto yy35;
    goto yy36;
  }
  goto yy36;
yy41:
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
  case '~':  goto yy41;
  default:
    if (offset >= input.length) goto yy12;
    goto yy45;
  }
yy43:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':  goto yy47;
  default:  goto yy35;
  }
yy44:
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
  case 'f':  goto yy50;
  default:  goto yy35;
  }
yy45:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '\n':
  case '\r':  goto yy6;
  default:
    if (offset >= input.length) goto yy6;
    goto yy45;
  }
yy47:
  ++offset;
  yych = offset < input.length ? input.bytes[offset] : 0;
  switch (yych) {
  case '0':
  case '1':  goto yy47;
  default:  goto yy49;
  }
yy49:
#line 111 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_2, digits);
        continue;
      }
#line 570 "generated_tokenizer.c"
yy50:
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
  case 'f':  goto yy50;
  default:  goto yy52;
  }
yy52:
#line 117 "tokenizer.re.c"
  {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_16, digits);
        continue;
      }
#line 606 "generated_tokenizer.c"
yy53:
#line 101 "tokenizer.re.c"
  { break; }
#line 610 "generated_tokenizer.c"
}
#line 162 "tokenizer.re.c"

  }

  current_line.to = file->text.length;
  dyn_array_push(file->line_ranges, current_line);

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
      .file = file,
      .offsets = {.from = 0, .to = u64_to_u32(input.length)},
    };
    *out_tokens = temp_token_array_into_value_view(
      allocator, dyn_array_raw(stack), dyn_array_length(stack), children_range
    );
  }
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(stack);
  dyn_array_destroy(parent_stack);
  return result;
}

