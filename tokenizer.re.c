// If you want to make changes to the tokenizer, edit tokenizer.re.c
// and then regenerate this file on a Linux machine / WSL:
//
// re2c tokenizer.re.c -o generated_tokenizer.c --no-version --no-generation-date
//
#include "types.h"

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  Mass_Context *context,
  Source_Range source_range,
  Array_Value_View *out_statements
) {
  Compilation *compilation = context->compilation;
  Slice input = source_range.file->text;

  const Allocator *allocator = compilation->allocator;

  Array_Value_Ptr stack = dyn_array_make(Array_Value_Ptr, .capacity = 100);
  Array_Tokenizer_Parent parent_stack =
    dyn_array_make(Array_Tokenizer_Parent, .capacity = 16);

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

  Fixed_Buffer *string_buffer = fixed_buffer_make(.capacity = 4096);

  u64 offset = source_range.offsets.from;
  u64 token_start_offset = offset;
  u64 marker = offset;
  u64 end_offset = source_range.offsets.to;

  #define TOKENIZER_CURRENT_RANGE()\
    (Source_Range){\
      .file = source_range.file,\
      .offsets = {\
        .from = u64_to_u32(token_start_offset),\
        .to = u64_to_u32(offset),\
      }\
    }

  // Create top-level block
  tokenizer_group_start_curly(allocator, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE());

  #define TOKENIZER_CURRENT_SLICE()\
    slice_sub(input, token_start_offset, offset)

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
      goto defer;\
    } while (0)

  #define TOKENIZER_PUSH_SYMBOL(_TYPE_)\
    dyn_array_push(stack, \
      token_make_symbol_value(\
        context, TOKENIZER_CURRENT_SLICE(), TOKENIZER_CURRENT_RANGE()\
      )\
    )

  #define TOKENIZER_GROUP_START(_VARIANT_)\
    tokenizer_group_start(\
      allocator, &stack, &parent_stack, &descriptor_group_##_VARIANT_, TOKENIZER_CURRENT_RANGE()\
    )

  #define TOKENIZER_GROUP_END(_VARIANT_)\
    if (!tokenizer_group_end_##_VARIANT_(context, &stack, &parent_stack, offset))\
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
      re2c:define:YYLESSTHAN = "offset >= end_offset";
      re2c:define:YYPEEK     = "offset < end_offset ? input.bytes[offset] : 0";
      re2c:define:YYSKIP     = "++offset;";
      re2c:define:YYBACKUP   = "marker = offset;";
      re2c:define:YYRESTORE  = "offset = marker;";

      $ { break; }

      single_line_comment = "//" [^\r\n]*;
      single_line_comment { continue; }

      // @Volatile :TokenizerNumbers
      // integer literals
      decimal = '0' | [1-9][0-9_]*;
      decimal {
        Slice digits = slice_sub(input, token_start_offset, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_10, digits);
        continue;
      }
      binary = '0b' [01_]+;
      binary {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_2, digits);
        continue;
      }
      hex = '0x' [0-9a-fA-F_]+;
      hex {
        Slice digits = slice_sub(input, token_start_offset + 2, offset);
        TOKENIZER_PUSH_LITERAL(Number_Base_16, digits);
        continue;
      }

      "(" { TOKENIZER_GROUP_START(paren); continue; }
      "[" { TOKENIZER_GROUP_START(square); continue; }
      "{" {
        tokenizer_group_start_curly(allocator, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE());
        continue;
      }
      ")" { TOKENIZER_GROUP_END(paren); continue; }
      "]" { TOKENIZER_GROUP_END(square); continue; }
      "}" { TOKENIZER_GROUP_END(curly); continue; }

      operator = [+*%/=!@^&$\\:,?|.~<>-]+|['];
      operator { TOKENIZER_PUSH_SYMBOL(); continue; }

      end_of_statement = "\r\n" | "\r" | "\n" | ";";
      end_of_statement {
        tokenizer_maybe_push_statement(context, &stack, &parent_stack, offset - 1);
        continue;
      }

      ["] ([^"\\] | [\\][^])* ["] {
        Slice raw_bytes = slice_sub(input, token_start_offset + 1, offset - 1);
        tokenizer_push_string_literal(
          context, &string_buffer, &stack, raw_bytes, TOKENIZER_CURRENT_RANGE()
        );
        continue;
      }

      whitespace = [ \t\v]+;
      whitespace { continue; }

      identifier = [a-zA-Z_][a-zA-Z_0-9]*; // named definition
      identifier { TOKENIZER_PUSH_SYMBOL(); continue; }

      // default rule (error)
      * { TOKENIZER_HANDLE_ERROR((Slice){0}); }
    */
  }

  if (dyn_array_length(parent_stack) != 1) {
    offset++;
    TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a closing brace.");
  }

  TOKENIZER_GROUP_END(curly);

  #undef TOKENIZER_CURRENT_SLICE
  #undef TOKENIZER_CURRENT_RANGE
  #undef TOKENIZER_HANDLE_ERROR
  #undef TOKENIZER_PUSH_SYMBOL
  #undef TOKENIZER_PUSH_LITERAL

  defer:
  if (result.tag == Mass_Result_Tag_Success) {
    assert(dyn_array_length(stack) == 1);
    Value *root_value = *dyn_array_pop(stack);
    const Ast_Block *root = value_as_ast_block(root_value);
    *out_statements = root->statements;
  }
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(stack);
  dyn_array_destroy(parent_stack);
  return result;
}

