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

  #define TOKENIZER_GROUP_START(_TAG_)\
    tokenizer_group_start(allocator, &stack, &parent_stack, (_TAG_), TOKENIZER_CURRENT_RANGE())

  #define TOKENIZER_GROUP_END(_PAREN_)\
    if (!tokenizer_group_end(allocator, &stack, &parent_stack, (_PAREN_), offset))\
      TOKENIZER_HANDLE_ERROR((Slice){0})

  for (;;) {
    token_start_offset = offset;
    /*!re2c
      re2c:indent:string = "  ";
      re2c:yyfill:enable = 0;
      re2c:eof = 0;
      re2c:flags:input = custom;
      re2c:api:style = free-form;
      re2c:define:YYCTYPE    = char;
      re2c:define:YYLESSTHAN = "offset >= input.length";
      re2c:define:YYPEEK     = "offset < input.length ? input.bytes[offset] : 0";
      re2c:define:YYSKIP     = "++offset;";
      re2c:define:YYBACKUP   = "marker = offset;";
      re2c:define:YYRESTORE  = "offset = marker;";

      $ { break; }

      // integer literals
      decimal = '0' | [1-9][0-9]*;
      decimal {
        Slice digits = slice_sub(input, token_start_offset, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_10, digits);
        continue;
      }
      binary = '0b' [01]+;
      binary {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_2, digits);
        continue;
      }
      hex = '0x' [0-9a-fA-F]+;
      hex {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_16, digits);
        continue;
      }

      "(" { TOKENIZER_GROUP_START(&descriptor_group_paren); continue; }
      "[" { TOKENIZER_GROUP_START(&descriptor_group_square); continue; }
      "{" { TOKENIZER_GROUP_START(&descriptor_group_curly); continue; }
      ")" { TOKENIZER_GROUP_END(')'); continue; }
      "]" { TOKENIZER_GROUP_END(']'); continue; }
      "}" { TOKENIZER_GROUP_END('}'); continue; }

      operator = [+*%/=!@^&$\\:;,?|.~<>-]+|['];
      operator { TOKENIZER_PUSH_SYMBOL(); continue; }

      newline = "\r\n" | "\r" | "\n";
      newline {
        token_start_offset = offset; // :FakeSemicolon
        tokenizer_maybe_push_fake_semicolon(
          compilation, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }

      ["] ([^"\\] | [\\][^])* ["] {
        Slice raw_bytes = slice_sub(input, token_start_offset + 1, offset - 1);
        tokenizer_push_string_literal(
          compilation, &string_buffer, &stack, raw_bytes, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }

      single_line_comment = "//" [^\r\n]*;
      whitespace = ([ \t\v] | single_line_comment)+;
      whitespace { continue; }

      identifier = [a-zA-Z_][a-zA-Z_0-9]*; // named definition
      identifier { TOKENIZER_PUSH_SYMBOL(); continue; }

      // default rule (error)
      * { TOKENIZER_HANDLE_ERROR((Slice){0}); }
    */
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

