#include "types.h"

static Mass_Result
tokenizer_handle_error(
  Mass_Context *context,
  Slice expected,
  Source_Range source_range
) {
  return (Mass_Result) {
    .tag = Mass_Result_Tag_Error,
    .Error.error = {
      .tag = Mass_Error_Tag_Unexpected_Token,
      .Unexpected_Token = { .expected = expected, },
      .source_range = source_range,
    }
  };
}

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  Mass_Context *context,
  Source_Range source_range,
  Array_Value_View *out_statements
) {
  Slice input = source_range.file->text;

  Array_Value_Ptr stack = dyn_array_make(Array_Value_Ptr, .capacity = 100);
  Array_Tokenizer_Parent parent_stack =
    dyn_array_make(Array_Tokenizer_Parent, .capacity = 16);

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

  Fixed_Buffer *string_buffer = fixed_buffer_make(.capacity = 4096);

  #define TOKENIZER_CURRENT_RANGE()\
    (Source_Range){\
      .file = source_range.file,\
      .offsets = {\
        .from = u64_to_u32(token_start_offset),\
        .to = u64_to_u32(offset),\
      }\
    }

  #define TOKENIZER_HANDLE_ERROR(_EXPECTED_SLICE_)\
    do {\
      Source_Range error_range = {\
        .file = source_range.file, \
        .offsets = {.from = u64_to_u32(offset) - 1, .to = u64_to_u32(offset) - 1},\
      };\
      result = tokenizer_handle_error(context, slice_literal(_EXPECTED_SLICE_), error_range);\
      goto defer;\
    } while (0)

  #define TOKENIZER_PUSH_SYMBOL(_TYPE_)\
    dyn_array_push(stack, \
      token_make_symbol_value(\
        context, slice_sub(input, token_start_offset, offset), TOKENIZER_CURRENT_RANGE()\
      )\
    )

  #define TOKENIZER_GROUP_START(_VARIANT_)\
    tokenizer_group_start_##_VARIANT_(context, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE())

  #define TOKENIZER_GROUP_END(_VARIANT_)\
    do {\
      if (!tokenizer_group_end_##_VARIANT_(context, &stack, &parent_stack, offset + 1)) {\
        ++offset;\
        TOKENIZER_HANDLE_ERROR("Expected a " #_VARIANT_);\
      }\
    } while (0)

  enum Category {
    Digit = 1 << 0,
    Id_Start = 1 << 1,
    Space = 1 << 2,
    Special = 1 << 3,
    Symbol = 1 << 4,
    Newline = 1 << 5,
    Slash = 1 << 6,
    Error = 1 << 7,

    _Category_Last = Error,
  };
  static_assert(_Category_Last < (1 << sizeof(u8) * 8), "Category should fit into a u8");

  static u8 CHAR_CATEGORY_MAP[256] = {0};
  static enum Category CATEGORY_CONTINUATION_MASK[_Category_Last + 1] = {
    [Error] = Error,
    [Digit] = Digit | Id_Start,
    [Id_Start] = Digit | Id_Start,
    [Space] = Space,
    [Special] = Space,
    [Symbol] = Symbol,
    [Newline] = Newline,
    [Slash] = Slash,
  };

  static u8 DIGIT_DECODER[128] = {0};

  static bool tokenizer_initialized = false;
  if (!tokenizer_initialized) {
    for (s32 i = 0; i < countof(DIGIT_DECODER); ++i) {
      DIGIT_DECODER[i] = 0xFF;
    }

    for (s32 i = 0; i < 0x80; ++i) {
      CHAR_CATEGORY_MAP[i] = Symbol;
    }
    for (s32 i = 0x80; i <= 0xFF; ++i) {
      CHAR_CATEGORY_MAP[i] = Error;
    }
    for (char ch = 0; ch < ' '; ++ch) {
      CHAR_CATEGORY_MAP[ch] = Error;
    }
    CHAR_CATEGORY_MAP['/'] = Slash;

    CHAR_CATEGORY_MAP[' '] = Space;
    CHAR_CATEGORY_MAP['\t'] = Space;

    CHAR_CATEGORY_MAP['\r'] = Newline;
    CHAR_CATEGORY_MAP['\n'] = Newline;

    CHAR_CATEGORY_MAP['"'] = Special;
    CHAR_CATEGORY_MAP[';'] = Special;
    CHAR_CATEGORY_MAP['('] = Special;
    CHAR_CATEGORY_MAP[')'] = Special;
    CHAR_CATEGORY_MAP['{'] = Special;
    CHAR_CATEGORY_MAP['}'] = Special;
    CHAR_CATEGORY_MAP['['] = Special;
    CHAR_CATEGORY_MAP[']'] = Special;

    for (char ch = '0'; ch <= '9'; ++ch) {
      CHAR_CATEGORY_MAP[ch] = Digit;
      DIGIT_DECODER[ch] = ch - '0';
    }

    CHAR_CATEGORY_MAP['_'] = Id_Start;
    for (char ch = 'a'; ch <= 'z'; ++ch) {
      CHAR_CATEGORY_MAP[ch] = Id_Start;
      if (ch <= 'f') DIGIT_DECODER[ch] = ch - 'a' + 10;
    }
    for (char ch = 'A'; ch <= 'Z'; ++ch) {
      CHAR_CATEGORY_MAP[ch] = Id_Start;
      if (ch <= 'F') DIGIT_DECODER[ch] = ch - 'A' + 10;
    }
  }

  u64 offset = source_range.offsets.from;
  u64 end_offset = source_range.offsets.to;
  u64 token_start_offset = offset;

  // Create top-level block
  tokenizer_group_start_curly(context, &stack, &parent_stack, TOKENIZER_CURRENT_RANGE());

  bool should_finalize = true;
  enum Category starting_category = Space;
  char current = 0;
  enum Category category = Space;
  enum Category continuation_mask = 0;

  for (; offset < end_offset; ++offset) {
    current = input.bytes[offset];
    category = CHAR_CATEGORY_MAP[current];

    if (category == Slash && starting_category == Slash) {
      for (; offset < end_offset; ++offset) {
        char current = input.bytes[offset];
        if (current == '\n') {
          // TODO support \r\n here
          break;
        }
      }
      tokenizer_maybe_push_statement(context, &stack, &parent_stack, offset);
      category = starting_category = Space;
      continuation_mask = CATEGORY_CONTINUATION_MASK[starting_category];
      continue;
    }

    if ((category & continuation_mask)) continue;

    finalize:
    switch(starting_category) {
      case Newline: {
        tokenizer_maybe_push_statement(context, &stack, &parent_stack, offset);
      } break;
      case Digit: {
        Slice source = slice_sub(input, token_start_offset, offset);
        u32 base = 10;
        u64 digit_index = 0;
        if (source.length > 1 && source.bytes[0] == '0') {
          char second = source.bytes[1];
          if (CHAR_CATEGORY_MAP[second] == Id_Start) {
            switch(second) {
              case 'b': {
                base = 2;
                digit_index = 2; // skip over `0b`
              } break;
              case 'o': {
                base = 8;
                digit_index = 2; // skip over `0o`
              } break;
              case 'x': {
                base = 16;
                digit_index = 2; // skip over `0f`
              } break;
              default: { // happens when we have smth like `0foo`
                digit_index = 1; // skip over `0`
              } break;
            }
          } else { // e.g. 0777
            TOKENIZER_HANDLE_ERROR(
              "Numbers are not allowed to have `0` at the start.\n"
              "If you meant to specify an octal number, prefix it with `0o`"
            );
          }
        }
        u64 literal = 0;
        for (; digit_index < source.length; ++digit_index) {
          char ch = source.bytes[digit_index];
          if (ch == '_') continue;
          u8 digit = DIGIT_DECODER[ch];
          if (digit >= base) break;
          literal *= base;
          literal += digit;
        }
        offset = token_start_offset + digit_index;
        Value *value = value_make(
          context, &descriptor_i64, storage_immediate(&literal), TOKENIZER_CURRENT_RANGE()
        );
        dyn_array_push(stack, value);

        //  when we have smth like `0foo` or `0xCAFEwww`
        if (digit_index < source.length) {
          // Reset tokenizer to the state after the matched number
          offset -= 1;
          should_finalize = true;
          category = Space;
        }
      } break;
      case Id_Start:
      case Symbol: {
        TOKENIZER_PUSH_SYMBOL();
      } break;
      case Space: {
        // Nothing to do
      } break;
      case Slash: {
        TOKENIZER_PUSH_SYMBOL();
      } break;
      case Error: {
        TOKENIZER_HANDLE_ERROR("Unexpected character");
      } break;
      case Special: {
        panic("UNREACHEABLE");
      } break;
    }
    token_start_offset = offset;

    if (category == Special) {
      category = Space;
      switch(current) {
        case '"': {
          ++offset;
          bool found_end = false;
          for (; offset < end_offset; ++offset) {
            char current = input.bytes[offset];
            if (current == '"') {
              if (input.bytes[offset - 1] != '\\') {
                found_end = true;
                Slice raw_bytes = slice_sub(input, token_start_offset + 1, offset);
                Source_Range string_range = TOKENIZER_CURRENT_RANGE();
                string_range.offsets.to += 1;
                tokenizer_push_string_literal(context, &string_buffer, &stack, raw_bytes, string_range);
                break;
              }
            }
          }
          if (!found_end) TOKENIZER_HANDLE_ERROR("Unterminated string");
        } break;
        case ';': { tokenizer_maybe_push_statement(context, &stack, &parent_stack, offset + 1); } break;
        case '(': { TOKENIZER_GROUP_START(paren); } break;
        case '[': { TOKENIZER_GROUP_START(square); } break;
        case '{': { TOKENIZER_GROUP_START(curly); } break;
        case ')': { TOKENIZER_GROUP_END(paren); } break;
        case ']': { TOKENIZER_GROUP_END(square); } break;
        case '}': { TOKENIZER_GROUP_END(curly); } break;
        default: {
          panic("UNREACHEABLE");
        } break;
      }
    }

    starting_category = category;
    continuation_mask = CATEGORY_CONTINUATION_MASK[starting_category];
  }

  if (should_finalize) {
    should_finalize = false;
    goto finalize;
  }

  if (dyn_array_length(parent_stack) != 1) {
    TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a closing brace.");
  }

  TOKENIZER_GROUP_END(curly);

  #undef TOKENIZER_GROUP_START
  #undef TOKENIZER_GROUP_END
  #undef TOKENIZER_CURRENT_RANGE
  #undef TOKENIZER_HANDLE_ERROR
  #undef TOKENIZER_PUSH_SYMBOL

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

