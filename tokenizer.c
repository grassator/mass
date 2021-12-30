#include "types.h"

typedef struct {
  Value *value;
  u64 index;
} Tokenizer_Parent;
typedef dyn_array_type(Tokenizer_Parent) Array_Tokenizer_Parent;

typedef struct {
  Array_Value_Ptr token_stack;
  Array_Tokenizer_Parent parent_stack;
} Tokenizer_State;

static inline Value *
tokenizer_make_symbol(
  Mass_Context *context,
  Slice name,
  Source_Range source_range
) {
  const Symbol *symbol = mass_ensure_symbol(context->compilation, name);

  return value_make(context, &descriptor_symbol, storage_static(symbol), source_range);
}

static inline Value_View
tokenizer_value_view_for_children(
  const Allocator *allocator,
  Value **children,
  u32 child_count,
  Source_Range children_range
) {
  Value_View result = { .values = 0, .length = child_count, .source_range = children_range };
  if (child_count) {
    Value **tokens = allocator_allocate_array(allocator, Value *, child_count);
    memcpy(tokens, children, child_count * sizeof(tokens[0]));
    result.values = tokens;
  }
  return result;
}

static inline Value_View
tokenizer_make_group_children_view(
  Mass_Context *context,
  Tokenizer_State *state,
  Tokenizer_Parent *parent,
  Value *parent_value,
  u64 offset
) {
  Value **children_values = dyn_array_raw(state->token_stack) + parent->index + 1;
  u64 child_count = dyn_array_length(state->token_stack) - parent->index - 1;
  state->token_stack.data->length = parent->index + 1; // pop the children
  assert(offset);

  Source_Range children_range = {
    .file = parent_value->source_range.file,
    .offsets = {.from = u64_to_u32(offset - 1), .to = u64_to_u32(offset - 1)},
  };
  if (child_count) {
    Value *first_child = children_values[0];
    children_range.offsets.from = first_child->source_range.offsets.from;
  }

  parent_value->source_range.offsets.to = u64_to_u32(offset);

  return tokenizer_value_view_for_children(
    context->allocator, children_values, u64_to_u32(child_count), children_range
  );
}

static inline bool
tokenizer_maybe_push_statement(
  Mass_Context *context,
  Tokenizer_State *state,
  u64 offset
) {
  assert(dyn_array_length(state->parent_stack));
  Tokenizer_Parent *parent = dyn_array_last(state->parent_stack);
  if(parent->value->descriptor != &descriptor_ast_block) return false;
  bool has_children = parent->index + 1 != dyn_array_length(state->token_stack);
  // Do not treat leading newlines as semicolons
  if (!has_children) return true;

  Ast_Block *group = (Ast_Block *)value_as_ast_block(parent->value);

  assert(offset);
  Value_View statement = tokenizer_make_group_children_view(
    context, state, parent, parent->value, offset
  );
  dyn_array_push(group->statements, statement);
  return true;
}

static inline void
tokenizer_group_push(
  Tokenizer_State *state,
  Value *value
) {
  dyn_array_push(state->parent_stack, (Tokenizer_Parent){
    .value = value,
    .index = dyn_array_length(state->token_stack)
  });
  dyn_array_push(state->token_stack, value);
}

static inline void
tokenizer_group_start_curly(
  Mass_Context *context,
  Tokenizer_State *state,
  Source_Range source_range
) {
  Ast_Block *group = mass_allocate(context, Ast_Block);
  // TODO use temp allocator first?
  *group = (Ast_Block){.statements = dyn_array_make(Array_Value_View, .allocator = context->allocator)};
  Value *value = value_make(context, &descriptor_ast_block, storage_immediate(group), source_range);
  tokenizer_group_push(state, value);
}

static inline void
tokenizer_group_start_paren(
  Mass_Context *context,
  Tokenizer_State *state,
  Source_Range source_range
) {
  Group_Paren *group = mass_allocate(context, Group_Paren);
  Value *value = value_make(context, &descriptor_group_paren, storage_static(group), source_range);
  tokenizer_group_push(state, value);
}

static inline void
tokenizer_group_start_square(
  Mass_Context *context,
  Tokenizer_State *state,
  Source_Range source_range
) {
  Group_Square *group = mass_allocate(context, Group_Square);
  Value *value = value_make(context, &descriptor_group_square, storage_static(group), source_range);
  tokenizer_group_push(state, value);
}

static inline bool
tokenizer_group_end_paren(
  Mass_Context *context,
  Tokenizer_State *state,
  u64 offset
) {
  if (!dyn_array_length(state->parent_stack)) return false;
  Tokenizer_Parent *parent = dyn_array_pop(state->parent_stack);
  Value *parent_value = *dyn_array_get(state->token_stack, parent->index);
  if (parent_value->descriptor != &descriptor_group_paren) return false;

  Value_View children = tokenizer_make_group_children_view(
    context, state, parent, parent_value, offset
  );

  Group_Paren *group = mass_allocate(context, Group_Paren);
  *group = (Group_Paren){.children = children};
  assert(parent_value->tag == Value_Tag_Forced);
  parent_value->Forced.storage = storage_static(group);

  return true;
}

static inline bool
tokenizer_group_end_square(
  Mass_Context *context,
  Tokenizer_State *state,
  u64 offset
) {
  if (!dyn_array_length(state->parent_stack)) return false;
  Tokenizer_Parent *parent = dyn_array_pop(state->parent_stack);
  Value *parent_value = *dyn_array_get(state->token_stack, parent->index);
  if (parent_value->descriptor != &descriptor_group_square) return false;

  Value_View children = tokenizer_make_group_children_view(
    context, state, parent, parent_value, offset
  );
  Group_Square *group = mass_allocate(context, Group_Square);
  *group = (Group_Square){.children = children};
  assert(parent_value->tag == Value_Tag_Forced);
  parent_value->Forced.storage = storage_static(group);

  return true;
}

static inline bool
tokenizer_group_end_curly(
  Mass_Context *context,
  Tokenizer_State *state,
  u64 offset
) {
  if (!tokenizer_maybe_push_statement(context, state, offset)) return false;
  dyn_array_pop(state->parent_stack);
  return true;
}

static inline void
tokenizer_push_string_literal(
  Mass_Context *context,
  Fixed_Buffer **string_buffer,
  Array_Value_Ptr *stack,
  Slice raw_bytes,
  Source_Range source_range
) {
  (*string_buffer)->occupied = 0;
  Slice remainder = raw_bytes;
  for(s64 escape_index; ; ) {
    escape_index = slice_index_of_char(remainder, '\\');
    if (escape_index == -1) {
      break;
    }
    Slice to_copy = slice_sub(remainder, 0, escape_index);
    remainder = slice_sub(remainder, escape_index + 2, remainder.length);
    fixed_buffer_resizing_ensure_capacity(string_buffer, to_copy.length);
    fixed_buffer_append_slice(*string_buffer, to_copy);
    char ch = raw_bytes.bytes[escape_index + 1];
    s8 escaped_character;
    switch (ch) {
      case 'n': escaped_character = '\n'; break;
      case 'r': escaped_character = '\r'; break;
      case 't': escaped_character = '\t'; break;
      case 'v': escaped_character = '\v'; break;
      case '0': escaped_character = '\0'; break;
      default: escaped_character = ch; break;
    }
    fixed_buffer_resizing_append_s8(string_buffer, escaped_character);
  }
  {
    fixed_buffer_resizing_ensure_capacity(string_buffer, remainder.length);
    fixed_buffer_append_slice(*string_buffer, remainder);
  }

  u64 length = (*string_buffer)->occupied;
  char *bytes = allocator_allocate_bytes(context->allocator, length, 1);
  memcpy(bytes, (*string_buffer)->memory, length);

  allocator_allocate_bulk(context->allocator, combined, {
    Descriptor bits_descriptor;
    Mass_Byte_Slice byte_slice;
    Value string_value;
  });

  hash_map_set(context->compilation->static_pointer_length_map, bytes, length);

  Mass_Byte_Slice *byte_slice = &combined->byte_slice;
  *byte_slice = (Mass_Byte_Slice){bytes, length};
  Value *byte_slice_value = value_init(
    &combined->string_value,
    &descriptor_byte_slice, storage_static(byte_slice), source_range
  );
  dyn_array_push(*stack, byte_slice_value);
}

static Mass_Result
tokenizer_handle_error(
  Mass_Context *context,
  Slice detailed_message,
  Source_Range source_range
) {
  return (Mass_Result) {
    .tag = Mass_Result_Tag_Error,
    .Error.error = {
      .tag = Mass_Error_Tag_Tokenizer,
      .source_range = source_range,
      .detailed_message = detailed_message,
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

  Tokenizer_State state = {
    // FIXME use temp allocator
    .token_stack = dyn_array_make(Array_Value_Ptr, .capacity = 100),
    .parent_stack = dyn_array_make(Array_Tokenizer_Parent, .capacity = 32),
  };

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

  #define TOKENIZER_HANDLE_ERROR(_MESSAGE_)\
    do {\
      Source_Range error_range = {\
        .file = source_range.file, \
        .offsets = {.from = u64_to_u32(offset) - 1, .to = u64_to_u32(offset) - 1},\
      };\
      result = tokenizer_handle_error(context, slice_literal(_MESSAGE_), error_range);\
      goto defer;\
    } while (0)

  #define TOKENIZER_GROUP_START(_VARIANT_)\
    tokenizer_group_start_##_VARIANT_(context, &state, TOKENIZER_CURRENT_RANGE())

  #define TOKENIZER_GROUP_END(_VARIANT_)\
    do {\
      if (!tokenizer_group_end_##_VARIANT_(context, &state, offset + 1)) {\
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
    Other = 1 << 7,

    _Category_Last = Other,
  };
  static_assert(_Category_Last < (1 << sizeof(u8) * 8), "Category should fit into a u8");

  static u8 CHAR_CATEGORY_MAP[256] = {0};
  static enum Category CATEGORY_CONTINUATION_MASK[_Category_Last + 1] = {
    [Other] = Other,
    [Digit] = Digit | Id_Start,
    [Id_Start] = Digit | Id_Start,
    [Space] = Space,
    [Special] = Space,
    [Symbol] = Symbol,
    [Newline] = Newline,
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
      CHAR_CATEGORY_MAP[i] = Other;
    }
    for (char ch = 0; ch < ' '; ++ch) {
      CHAR_CATEGORY_MAP[ch] = Other;
    }

    CHAR_CATEGORY_MAP[' '] = Space;
    CHAR_CATEGORY_MAP['\t'] = Space;

    CHAR_CATEGORY_MAP['\r'] = Newline;
    CHAR_CATEGORY_MAP['\n'] = Newline;

    CHAR_CATEGORY_MAP['/'] = Special;
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
  tokenizer_group_start_curly(context, &state, TOKENIZER_CURRENT_RANGE());

  bool should_finalize = true;
  enum Category starting_category = Space;
  char current = 0;
  enum Category category = Space;
  enum Category continuation_mask = 0;

  for (; offset < end_offset; ++offset) {
    current = input.bytes[offset];
    category = CHAR_CATEGORY_MAP[current];

    if ((category & continuation_mask)) continue;

    finalize:
    switch(starting_category) {
      case Newline: {
        tokenizer_maybe_push_statement(context, &state, offset);
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
        dyn_array_push(state.token_stack, value);

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
        Value *symbol = tokenizer_make_symbol(
          context, slice_sub(input, token_start_offset, offset), TOKENIZER_CURRENT_RANGE()
        );
        dyn_array_push(state.token_stack, symbol);
      } break;
      case Space: {
        // Nothing to do
      } break;
      case Other: {
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
                tokenizer_push_string_literal(
                  context, &string_buffer, &state.token_stack, raw_bytes, string_range
                );
                break;
              }
            }
          }
          if (!found_end) TOKENIZER_HANDLE_ERROR("Unterminated string");
        } break;
        case '/': {
          if (offset + 1 < end_offset && input.bytes[offset + 1] == '/') {
            starting_category = Space;
            continuation_mask = ~Newline;
            continue;
          } else {
            category = Symbol;
          }
        } break;
        case ';': { tokenizer_maybe_push_statement(context, &state, offset + 1); } break;
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

  if (dyn_array_length(state.parent_stack) != 1) {
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
    assert(dyn_array_length(state.token_stack) == 1);
    Value *root_value = *dyn_array_pop(state.token_stack);
    const Ast_Block *root = value_as_ast_block(root_value);
    *out_statements = root->statements;
  }
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(state.token_stack);
  dyn_array_destroy(state.parent_stack);
  return result;
}

