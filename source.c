#include "prelude.h"
#include "source.h"
#include "function.h"

static inline const Token *
token_view_get(
  Token_View view,
  u64 index
) {
  assert(index < view.length);
  return view.tokens[index];
}

static inline const Token *
token_view_peek(
  Token_View view,
  u64 index
) {
  return index < view.length ? view.tokens[index] : 0;
}

static inline const Token *
token_view_last(
  Token_View view
) {
  assert(view.length);
  return view.tokens[view.length - 1];
}

static inline Token_View
token_view_rest(
  Token_View view,
  u64 index
) {
  assert(index <= view.length);
  return (Token_View) {
    .tokens = view.tokens + index,
    .length = view.length - index,
  };
}

static inline Token_View
token_view_from_token_array(
  Array_Const_Token_Ptr token_array
) {
  return (Token_View) {
    .tokens = dyn_array_raw(token_array),
    .length = dyn_array_length(token_array),
  };
}

static inline Array_Const_Token_Ptr
token_array_from_view(
  const Allocator *allocator,
  Token_View view
) {
  // TODO optimize
  Array_Const_Token_Ptr result = dyn_array_make(Array_Const_Token_Ptr, .capacity = view.length);
  for (u64 i = 0; i < view.length; ++i) {
    dyn_array_push(result, token_view_get(view, i));
  }
  return result;
}

Source_Range
source_range_from_token_view(
  Token_View view
) {
  assert(view.length);
  const Token *first = token_view_get(view, 0);
  const Token *last = token_view_last(view);
  assert(first->source_range.file == last->source_range.file);
  return (Source_Range) {
    .file = first->source_range.file,
    .offsets = {
      .from = first->source_range.offsets.from,
      .to = last->source_range.offsets.to,
    },
  };
}

Scope *
scope_make(
  Allocator *allocator,
  Scope *parent
) {
  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .parent = parent,
    .map = hash_map_make(Scope_Map),
  };
  return scope;
}

void
scope_print_names(
  Scope *scope
) {
  while (scope) {
    for (u64 i = 0; i < scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &scope->map->entries[i];
      if (entry->occupied) {
        slice_print(entry->key);
        printf(" ; ");
      }
    }
    scope = scope->parent;
  }
  printf("\n");
}

static inline bool
scope_entry_matches_flag_mask(
  Scope_Entry *entry,
  Scope_Entry_Flags flag_mask
) {
  return (entry->flags & flag_mask) == flag_mask;
}

Scope_Entry *
scope_lookup(
  Scope *scope,
  Slice name,
  Scope_Entry_Flags flag_mask
) {
  for (; scope; scope = scope->parent) {
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    Scope_Entry *entry = *entry_pointer;
    if (entry && scope_entry_matches_flag_mask(entry, flag_mask)) {
      return entry;
    }
  }
  return 0;
}

Value *
token_parse_constant_expression(
  Compilation_Context *context,
  Token_View view
);

PRELUDE_NO_DISCARD Mass_Result
token_force_value(
  Compilation_Context *context,
  const Token *token,
  Value *result_value
);

typedef struct {
  Scope *scope;
  Scope_Entry_Flags flag_mask;
  Slice name;
  Scope_Entry *entry;
  bool done;
} Scope_Overload_Iterator;

Scope_Entry *
scope_lookup_overload(
  Scope_Overload_Iterator *it
) {
  if (it->done) return 0;
  for (; it->scope && !it->entry; it->scope = it->scope->parent) {
    Scope_Entry **entry_pointer = hash_map_get(it->scope->map, it->name);
    if (!entry_pointer) continue;
    if (scope_entry_matches_flag_mask(*entry_pointer, it->flag_mask)) {
      it->entry = *entry_pointer;
    }
  }
  Scope_Entry *result = it->entry;
  if (it->entry) {
    it->entry = it->entry->next_overload;
    while (it->entry && !scope_entry_matches_flag_mask(it->entry, it->flag_mask)) {
      it->entry = it->entry->next_overload;
    }
  }
  it->done = !it->entry;
  return result;
}

Value *
scope_entry_force(
  Compilation_Context *context,
  Scope_Entry *entry
) {
  switch(entry->type) {
    case Scope_Entry_Type_Operator: {
      // TODO Should scope entries have a Source_Range?
      context_error_snprintf(
        context, (Source_Range){0},
        "Operators are not allowed in this context"
      );
      return 0;
    }
    case Scope_Entry_Type_Lazy_Expression: {
      Scope_Lazy_Expression *expr = &entry->lazy_expression;
      Value *result = 0;
      Compilation_Context lazy_context = *context;
      lazy_context.scope = expr->scope;
      lazy_context.builder = expr->maybe_builder;
      if (expr->maybe_builder) {
        result = value_any(context->allocator);
        token_parse_expression(&lazy_context, expr->tokens, result);
      } else {
        result = token_parse_constant_expression(&lazy_context, expr->tokens);
      }
      *entry = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .flags = entry->flags,
        .value = result,
        .next_overload = entry->next_overload,
      };
      return result;
    }
    case Scope_Entry_Type_Value: {
      return entry->value;
    }
  }
  panic("Internal Error: Unexpected scope entry type");
  return 0;
}

Value *
scope_lookup_force(
  Compilation_Context *context,
  Scope *scope,
  Slice name,
  Scope_Entry_Flags flag_mask
) {
  flag_mask |= context->scope_entry_lookup_flags;
  Scope_Entry *entry = 0;
  for (; scope; scope = scope->parent) {
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    if (*entry_pointer && scope_entry_matches_flag_mask(*entry_pointer, flag_mask)) {
      entry = *entry_pointer;
      break;
    }
  }
  if (!entry) {
    return 0;
  }

  // Force lazy entries
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    if (it->type == Scope_Entry_Type_Lazy_Expression) {
      scope_entry_force(context, it);
    }
  }

  Value *result = 0;
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    assert(it->type == Scope_Entry_Type_Value);

    // To support recursive functions without a hack like `self` we
    // force the lazy value in two steps. First creates a valid Value
    // the second one, here, actually processes function body
    if (it->value && it->value->descriptor->tag == Descriptor_Tag_Function) {
      Descriptor_Function *function = &it->value->descriptor->Function;
      if (function->flags & Descriptor_Function_Flags_Pending_Body_Compilation) {
        function->flags &= ~Descriptor_Function_Flags_Pending_Body_Compilation;
        const Token *body = function->body;

        Value *return_result_value =
          function->returns->descriptor->tag == Descriptor_Tag_Void
          ? value_any(context->allocator)
          : function->returns;
        {
          Compilation_Context body_context = *context;
          body_context.scope_entry_lookup_flags = Scope_Entry_Flags_None;
          body_context.scope = function->scope;
          body_context.builder = function->builder;
          token_parse_block(&body_context, body, return_result_value);
        }
        fn_end(function->builder);
      }
    }

    if (!result) {
      result = it->value;
    } else {
      if (it->value->descriptor->tag != Descriptor_Tag_Function) {
        panic("Only functions support overloading");
      }
      Value *overload = it->value;
      overload->next_overload = result;
      result = overload;
    }
  }

  // For functions we need to gather up overloads from all parent scopes
  if (result && result->descriptor->tag == Descriptor_Tag_Function) {
    Value *last = result;
    Scope *parent = scope;
    for (;;) {
      parent = parent->parent;
      if (!parent) break;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(context, parent, name, flag_mask);
      if (!overload) panic("Just checked that hash map has the name so lookup must succeed");
      if (overload->descriptor->tag != Descriptor_Tag_Function) {
        panic("There should only be function overloads");
      }
      while (last->next_overload) {
        last = last->next_overload;
      }
      last->next_overload = overload;
    };
  }
  return result;
}

static inline void
scope_define(
  Scope *scope,
  Slice name,
  Scope_Entry entry
) {
  Scope_Entry *allocated = allocator_allocate(scope->map->allocator, Scope_Entry);
  *allocated = entry;
  if (hash_map_has(scope->map, name)) {
    // We just checked that the map has the entry so it safe to deref right away
    Scope_Entry *it = *hash_map_get(scope->map, name);
    // TODO Consider using a hash map that allows multiple values instead
    while (it->next_overload) {
      it = it->next_overload;
    }
    it->next_overload = allocated;
  } else {
    hash_map_set(scope->map, name, allocated);
  }
}

bool
code_point_is_operator(
  s32 code_point
) {
  switch(code_point) {
    case '+':
    case '-':
    case '=':
    case '!':
    case '@':
    case '%':
    case '^':
    case '&':
    case '$':
    case '*':
    case '/':
    case ':':
    case ';':
    case ',':
    case '?':
    case '|':
    case '.':
    case '~':
    case '>':
    case '<':
      return true;
    default:
      return false;
  }
}

const Token_Pattern token_pattern_comma_operator = {
  .tag = Token_Tag_Operator,
  .source = slice_literal_fields(","),
};

const Token_Pattern token_pattern_semicolon = {
  .tag = Token_Tag_Operator,
  .source = slice_literal_fields(";"),
};

bool
token_match_internal(
  const Token *token,
  const Token_Pattern *pattern
) {
  if (pattern->group_tag) {
    if (token->tag != Token_Tag_Group) return false;
    return token->Group.tag == pattern->group_tag;
  }
  if (pattern->tag && pattern->tag != token->tag) return false;
  if (pattern->source.length) {
    return slice_equal(token->source, pattern->source);
  }
  return true;
}

bool
token_match(
  const Token *token,
  const Token_Pattern *pattern
) {
  bool result = token_match_internal(token, pattern);
  if (!result && pattern->or) {
    return token_match(token, pattern->or);
  }
  return result;
}

u64
tokenizer_handle_decimal_integer_end(
  Slice digits
) {
  bool ok = true;
  u64 result = slice_parse_u64(digits, &ok);
  if (!ok) {
    panic("Internal Error: Mismatch between tokenizer and decimal integer parser");
  }
  return result;
}

u64
tokenizer_handle_hex_integer_end(
  Slice digits
) {
  digits = slice_sub(digits, 2, digits.length); // Skip over 0x
  bool ok = true;
  u64 result = slice_parse_hex(digits, &ok);
  if (!ok) {
    panic("Internal Error: Mismatch between tokenizer and hex integer parser");
  }
  return result;
}

u64
tokenizer_handle_binary_integer_end(
  Slice digits
) {
  digits = slice_sub(digits, 2, digits.length); // Skip over 0b
  bool ok = true;
  u64 result = slice_parse_binary(digits, &ok);
  if (!ok) {
    panic("Internal Error: Mismatch between tokenizer and binary integer parser");
  }
  return result;
}

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  const Allocator *allocator,
  Source_File *file,
  Array_Const_Token_Ptr *out_tokens
) {

  Array_Const_Token_Ptr parent_stack = dyn_array_make(Array_Const_Token_Ptr);
  Token *root = &(Token){0};
  root->Group.children = dyn_array_make(Array_Const_Token_Ptr);

  assert(!dyn_array_is_initialized(file->line_ranges));
  file->line_ranges = dyn_array_make(Array_Range_u64);

  enum Tokenizer_State {
    Tokenizer_State_Default,
    Tokenizer_State_Decimal_Integer,
    Tokenizer_State_Binary_Integer,
    Tokenizer_State_Hex_Integer,
    Tokenizer_State_Operator,
    Tokenizer_State_Id,
    Tokenizer_State_String,
    Tokenizer_State_String_Escape,
    Tokenizer_State_Single_Line_Comment,
  };

  Range_u64 current_line = {0};
  enum Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;
  Fixed_Buffer *string_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = 4096,
  );

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

#define current_token_source()\
   slice_sub(file->text, current_token->source_range.offsets.from, i)

#define start_token(_type_)\
  do {\
    current_token = allocator_allocate(allocator, Token);\
    *current_token = (Token) {\
      .tag = (_type_),\
      .source_range = {\
        .file = file,\
        .offsets = {.from = i, .to = i},\
      }\
    };\
  } while(0)

#define do_push\
  do {\
    current_token->source = \
      slice_sub_range(file->text, current_token->source_range.offsets);\
    dyn_array_push(parent->Group.children, current_token);\
    current_token = 0;\
    state = Tokenizer_State_Default;\
  } while(0)

#define reject_and_push\
  do {\
    current_token->source_range.offsets.to = i;\
    do_push;\
  } while(0)

#define accept_and_push\
  do {\
    current_token->source_range.offsets.to = i + 1;\
    do_push;\
  } while(0)

#define TOKENIZER_HANDLE_ERROR(_MESSAGE_)\
  do {\
    result = (const Mass_Result) {\
      .tag = Mass_Result_Tag_Error,\
      .Error.details = {\
        .message = slice_literal(_MESSAGE_),\
        .source_range = {\
          .file = file,\
          .offsets = {.from = i, .to = i},\
        }\
      }\
    };\
    goto err;\
  } while (0)
#define push_line()\
  do {\
    current_line.to = i + 1;\
    dyn_array_push(file->line_ranges, current_line);\
    current_line.from = current_line.to;\
    if (parent->tag == Token_Tag_None || parent->Group.tag == Token_Group_Tag_Curly) {\
      /* Do not treating leading newlines as semicolons */ \
      if (dyn_array_length(parent->Group.children)) {\
        start_token(Token_Tag_Operator);\
        current_token->source_range.offsets = (Range_u64){ i + 1, i + 1 };\
        current_token->source = slice_literal(";");\
        dyn_array_push(parent->Group.children, current_token);\
      }\
    }\
    current_token = 0;\
    state = Tokenizer_State_Default;\
  } while(0)

  u64 i = 0;
  for (; i < file->text.length; ++i) {
    u8 ch = file->text.bytes[i];
    u8 peek = i + 1 < file->text.length ? file->text.bytes[i + 1] : 0;

    retry: switch(state) {
      case Tokenizer_State_Default: {
        if (ch == '\n') {
          push_line();
        } else if (ch == '\r') {
          if (peek == '\n') i++;
          push_line();
        } else if (isspace(ch)) {
          continue;
        } else if (ch == '0' && peek == 'x') {
          start_token(Token_Tag_Value);
          i++;
          state = Tokenizer_State_Hex_Integer;
        } else if (ch == '0' && peek == 'b') {
          start_token(Token_Tag_Value);
          i++;
          state = Tokenizer_State_Binary_Integer;
        } else if (isdigit(ch)) {
          start_token(Token_Tag_Value);
          state = Tokenizer_State_Decimal_Integer;
        } else if (isalpha(ch) || ch == '_') {
          start_token(Token_Tag_Id);
          state = Tokenizer_State_Id;
        } else if(ch == '/' && peek == '/') {
          state = Tokenizer_State_Single_Line_Comment;
        } else if (code_point_is_operator(ch)) {
          start_token(Token_Tag_Operator);
          state = Tokenizer_State_Operator;
        } else if (ch == '"') {
          string_buffer->occupied = 0;
          start_token(Token_Tag_String);
          state = Tokenizer_State_String;
        } else if (ch == '(' || ch == '{' || ch == '[') {
          start_token(Token_Tag_Group);
          current_token->Group.tag =
            ch == '(' ? Token_Group_Tag_Paren :
            ch == '{' ? Token_Group_Tag_Curly :
            Token_Group_Tag_Square;
          current_token->Group.children = dyn_array_make(Array_Const_Token_Ptr, 4);
          dyn_array_push(parent->Group.children, current_token);
          dyn_array_push(parent_stack, parent);
          parent = current_token;
        } else if (ch == ')' || ch == '}' || ch == ']') {
          if (parent->tag != Token_Tag_Group) {
            panic("Tokenizer: unexpected closing char for group");
          }
          s8 expected_paren = 0;
          switch (parent->Group.tag) {
            case Token_Group_Tag_Paren: {
              expected_paren = ')';
              break;
            }
            case Token_Group_Tag_Curly: {
              // Newlines at the end of the block do not count as semicolons otherwise this:
              // { 42
              // }
              // is being interpreted as:
              // { 42 ; }
              while (dyn_array_length(parent->Group.children)) {
                const Token *last_token = *dyn_array_last(parent->Group.children);
                bool is_last_token_a_fake_semicolon = (
                  token_match(last_token, &token_pattern_semicolon) &&
                  range_length(last_token->source_range.offsets) == 0
                );
                if (!is_last_token_a_fake_semicolon) break;
                dyn_array_pop(parent->Group.children);
              }

              expected_paren = '}';
              break;
            }
            case Token_Group_Tag_Square: {
              expected_paren = ']';
              break;
            }
          }
          if (ch != expected_paren) {
            TOKENIZER_HANDLE_ERROR("Mismatched closing brace");
          }
          parent->source_range.offsets.to = i + 1;
          parent->source = slice_sub_range(file->text, parent->source_range.offsets);
          if (!dyn_array_length(parent_stack)) {
            TOKENIZER_HANDLE_ERROR("Encountered a closing brace without a matching open one");
          }
          parent = (Token *)*dyn_array_pop(parent_stack);
          current_token = 0;
        } else {
          TOKENIZER_HANDLE_ERROR("Unpexpected input");
        }
        break;
      }
      case Tokenizer_State_Decimal_Integer: {
        if (!isdigit(ch)) {
          u64 bits = tokenizer_handle_decimal_integer_end(current_token_source());
          current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Hex_Integer: {
        if (!code_point_is_hex_digit(ch)) {
          u64 bits = tokenizer_handle_hex_integer_end(current_token_source());
          current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Binary_Integer: {
        if (ch != '0' && ch != '1') {
          u64 bits = tokenizer_handle_binary_integer_end(current_token_source());
          current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Id: {
        if (!(isalpha(ch) || isdigit(ch) || ch == '_')) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Operator: {
        if (!code_point_is_operator(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_String: {
        if (ch == '\\') {
          state = Tokenizer_State_String_Escape;
        } else if (ch == '"') {
          char *string = allocator_allocate_bytes(allocator, string_buffer->occupied, 1);
          memcpy(string, string_buffer->memory, string_buffer->occupied);
          current_token->String.slice = (Slice){string, string_buffer->occupied};
          accept_and_push;
        } else {
          fixed_buffer_resizing_append_u8(&string_buffer, ch);
        }
        break;
      }
      case Tokenizer_State_String_Escape: {
        s8 escaped_character;
        switch (ch) {
          case 'n': escaped_character = '\n'; break;
          case 'r': escaped_character = '\r'; break;
          case 't': escaped_character = '\t'; break;
          case 'v': escaped_character = '\v'; break;
          case '0': escaped_character = '\0'; break;
          default: escaped_character = ch; break;
        }
        fixed_buffer_resizing_append_s8(&string_buffer, escaped_character);
        state = Tokenizer_State_String;
        break;
      }
      case Tokenizer_State_Single_Line_Comment: {
        if (ch == '\n') {
          state = Tokenizer_State_Default;
        }
        break;
      }
    }
  }

  // Handle end of file
  switch(state) {
    case Tokenizer_State_Id:
    case Tokenizer_State_Default:
    case Tokenizer_State_Operator:
    case Tokenizer_State_Single_Line_Comment: {
      // Nothing to do
      break;
    }

    case Tokenizer_State_Decimal_Integer: {
      u64 bits = tokenizer_handle_decimal_integer_end(current_token_source());
      current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
      break;
    }
    case Tokenizer_State_Hex_Integer: {
      u64 bits = tokenizer_handle_hex_integer_end(current_token_source());
      current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
      break;
    }
    case Tokenizer_State_Binary_Integer: {
      u64 bits = tokenizer_handle_binary_integer_end(current_token_source());
      current_token->Value.value = value_from_unsigned_immediate(allocator, bits);
      break;
    }
    case Tokenizer_State_String:
    case Tokenizer_State_String_Escape: {
      TOKENIZER_HANDLE_ERROR("String without closing quote");
      break;
    }
  }

  current_line.to = file->text.length;
  dyn_array_push(file->line_ranges, current_line);

  if (parent != root) {
    TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a closing brace.");
  }
  // current_token can be null in case of an empty input
  if (current_token) {
    // Strings need to be terminated with a '"'
    if (state == Tokenizer_State_String) {
      TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a \".");
    } else {
      accept_and_push;
    }
  }

  err:
#undef tokenizer_error
#undef start_token
#undef push_and_retry
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(parent_stack);
  if (result.tag == Mass_Result_Tag_Success) {
    *out_tokens = root->Group.children;
  } else {
    // TODO @Leak cleanup token memory
  }
  return result;
}

const Token *
token_peek(
  Token_View view,
  u64 index
) {
  if (index < view.length) {
    return view.tokens[index];
  }
  return 0;
}

const Token *
token_peek_match(
  Token_View view,
  u64 index,
  const Token_Pattern *pattern
) {
  const Token *token = token_peek(view, index);
  if (!token) return 0;
  if (!token_match(token, pattern)) return 0;
  return token;
}

static inline const Token_View *
token_view_array_push(
  Array_Token_View *array,
  Token_View to_push
) {
  const Token_View *view = dyn_array_push_uninitialized(*array);
  // Need to cast here because we need to initialize somehow
  // a const pointer and that is not allowed
  *(Token_View *)view = to_push;
  return view;
}

typedef struct {
  Token_View view;
  u64 index;
  bool done;
} Token_View_Split_Iterator;

Token_View
token_split_next(
  Token_View_Split_Iterator *it,
  const Token_Pattern *separator
) {
  if (it->done) return (Token_View){0};
  u64 start_index = it->index;
  for (
    ;
    it->index < it->view.length;
    it->index++
  ) {
    const Token *token = token_view_get(it->view, it->index);
    if (token_match(token, separator)) {
      Token_View result = {
        .tokens = it->view.tokens + start_index,
        .length = it->index - start_index,
      };
      // Skip over the separator
      it->index++;
      return result;
    }
  }
  it->done = true;
  return token_view_rest(it->view, start_index);
}

Descriptor *
value_ensure_type(
  Compilation_Context *context,
  Value *value,
  Source_Range source_range,
  Slice type_name
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  if (!value) return 0;
  if (value->descriptor != &descriptor_type) {
    context_error_snprintf(
      context, source_range, "%"PRIslice" is not a type",
      SLICE_EXPAND_PRINTF(type_name)
    );
    return 0;
  }
  Descriptor *descriptor = operand_immediate_memory_as_descriptor(&value->operand);
  return descriptor;
}

Descriptor *
scope_lookup_type(
  Compilation_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice type_name
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  Scope_Entry *scope_entry = scope_lookup(scope, type_name, Scope_Entry_Flags_Static);
  if (!scope_entry) {
    Scope_Entry *runtime_entry = scope_lookup(scope, type_name, Scope_Entry_Flags_None);
    if (runtime_entry) {
      context_error_snprintf(
        context, source_range, "Could not find type %"PRIslice"."
        "There is a runtime value with the same name, but only static values can be used as types.",
        SLICE_EXPAND_PRINTF(type_name)
      );
    } else {
      context_error_snprintf(
        context, source_range, "Could not find type %"PRIslice,
        SLICE_EXPAND_PRINTF(type_name)
      );
    }
    return 0;
  }
  Value *value = scope_entry_force(context, scope_entry);
  return value_ensure_type(context, value, source_range, type_name);
}

#define Token_Maybe_Match(_id_, ...)\
  const Token *(_id_) = token_peek_match(view, peek_index, &(Token_Pattern) { __VA_ARGS__ });\
  if (_id_) (++peek_index)

#define Token_Match(_id_, ...)\
  Token_Maybe_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .tag = Token_Tag_Operator, .source = slice_literal(_op_))

typedef struct {
  Slice name;
  Value *value;
} Token_Match_Arg;

Descriptor *
token_force_type(
  Compilation_Context *context,
  Scope *scope,
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  if (!token) return 0;

  Descriptor *descriptor = 0;
  switch (token->tag) {
    case Token_Tag_None: {
      panic("Internal Error: Encountered token with an uninitialized tag");
      break;
    }
    case Token_Tag_Id: {
      descriptor = scope_lookup_type(context, scope, token->source_range, token->source);
      if (!descriptor) {
        MASS_ON_ERROR(*context->result) return 0;
        context_error_snprintf(
          context, token->source_range, "Could not find type %"PRIslice,
          SLICE_EXPAND_PRINTF(token->source)
        );
      }
      break;
    }
    case Token_Tag_Group: {
      if (token->Group.tag != Token_Group_Tag_Square) {
        panic("TODO");
      }
      if (dyn_array_length(token->Group.children) != 1) {
        context_error_snprintf(
          context, token->source_range, "Pointer type must have a single type inside"
        );
        return 0;
      }
      const Token *child = *dyn_array_get(token->Group.children, 0);
      if (child->tag != Token_Tag_Id) {
        panic("TODO: should be recursive");
      }
      descriptor = allocator_allocate(context->allocator, Descriptor);
      *descriptor = (Descriptor) {
        .tag = Descriptor_Tag_Pointer,
        .Pointer.to = scope_lookup_type(context, scope, child->source_range, child->source),
      };
      break;
    }
    case Token_Tag_Value: {
      return value_ensure_type(context, token->Value.value, token->source_range, token->source);
    }
    case Token_Tag_Operator:
    case Token_Tag_String:
    default: {
      panic("TODO");
      break;
    }
  }
  return descriptor;
}

u64
token_match_pattern(
  Token_View view,
  Macro *macro,
  Array_Token_View *out_match
) {
  u64 pattern_length = dyn_array_length(macro->pattern);
  if (!pattern_length) panic("Zero-length pattern does not make sense");

  dyn_array_clear(*out_match);
  u64 pattern_index = 0;
  u64 view_index = 0;

  for (; pattern_index < pattern_length && view_index < view.length; pattern_index++) {
    Macro_Pattern *pattern = dyn_array_get(macro->pattern, pattern_index);
    switch(pattern->tag) {
      case Macro_Pattern_Tag_Single_Token: {
        const Token *token = token_peek_match(view, view_index, &pattern->Single_Token.token_pattern);
        if (!token) {
          return 0;
        }
        dyn_array_push(*out_match, (Token_View){
          .tokens = view.tokens + view_index,
          .length = 1
        });
        view_index++;
        break;
      }
      case Macro_Pattern_Tag_Any_Token_Sequence: {
        u64 any_token_start_view_index = view_index;
        Macro_Pattern *peek = pattern_index + 1 < pattern_length
          ? dyn_array_get(macro->pattern, pattern_index + 1)
          : 0;
        assert(!peek || peek->tag == Macro_Pattern_Tag_Single_Token);
        for (; view_index < view.length; ++view_index) {
          const Token *token = token_view_get(view, view_index);
          if (peek && token_match(token, &peek->Single_Token.token_pattern)) {
            break;
          }
        }
        dyn_array_push(*out_match, (Token_View){
          .tokens = view.tokens + any_token_start_view_index,
          .length = view_index - any_token_start_view_index,
        });
        break;
      }
    }
  }

  // Did not match full pattern
  if (
    pattern_index != pattern_length &&
    !(
      pattern_index == pattern_length - 1 &&
      dyn_array_last(macro->pattern)->tag == Macro_Pattern_Tag_Any_Token_Sequence
    )
  ) {
    return 0;
  }

  // There are tokens remaining in the statement after the match
  if (macro->statement_end && view_index != view.length) {
    return 0;
  }
  return view_index;
}

Array_Const_Token_Ptr
token_apply_macro_replacements(
  Compilation_Context *context,
  Macro_Replacement_Map *map,
  Token_View source
) {
  if (context->result->tag != Mass_Result_Tag_Success) return dyn_array_make(Array_Const_Token_Ptr);

  Array_Const_Token_Ptr result = dyn_array_make(Array_Const_Token_Ptr, .capacity = source.length);
  for (u64 i = 0; i < source.length; ++i) {
    const Token *token = source.tokens[i];
    switch (token->tag) {
      case Token_Tag_None: {
        panic("Internal Error: Encountered token with an uninitialized tag");
        break;
      }
      case Token_Tag_Id: {
        Slice name = token->source;
        Token_View *view = hash_map_get(map, name);
        if (view) {
          for (u64 view_index = 0; view_index < view->length; ++view_index) {
            Token *copy = allocator_allocate(context->allocator, Token);
            *copy = *token_view_get(*view, view_index);
            dyn_array_push(result, copy);
          }
        } else {
          Token *copy = allocator_allocate(context->allocator, Token);
          *copy = *token;
          dyn_array_push(result, copy);
        }
        continue;
      }
      case Token_Tag_Operator:
      case Token_Tag_String: {
        Token *copy = allocator_allocate(context->allocator, Token);
        *copy = *token;
        dyn_array_push(result, copy);
        break;
      }
      case Token_Tag_Group: {
        Token *copy = allocator_allocate(context->allocator, Token);
        *copy = *token;
        copy->Group.tag = token->Group.tag;
        copy->Group.children = token_apply_macro_replacements(
          context, map, token_view_from_token_array(token->Group.children)
        );
        dyn_array_push(result, copy);
        break;
      }
      case Token_Tag_Value: {
        if (token->Value.value->operand.tag != Operand_Tag_Immediate) {
          panic("Only immediate operand values are safe to clone inside a macro");
        }
        if (token->Value.value->descriptor->tag == Descriptor_Tag_Any) {
          panic("Unpexected Any value in the cloned macro tree");
        }
        Token *copy = allocator_allocate(context->allocator, Token);
        *copy = *token;
        dyn_array_push(result, copy);
        break;
      }
    }
  }
  return result;
}

Array_Const_Token_Ptr
token_parse_macro_match(
  Compilation_Context *context,
  Array_Token_View match,
  Macro *macro
) {
  // FIXME switch to an out parameter
  if (context->result->tag != Mass_Result_Tag_Success) return dyn_array_make(Array_Const_Token_Ptr);

  Macro_Replacement_Map *map = hash_map_make(Macro_Replacement_Map);
  if (dyn_array_length(macro->pattern) != dyn_array_length(match)) {
    panic("Internal Error: Should not have chosen the macro if pattern length do not match");
  }
  for (u64 i = 0; i < dyn_array_length(macro->pattern); ++i) {
    Macro_Pattern *item = dyn_array_get(macro->pattern, i);
    switch(item->tag) {
      case Macro_Pattern_Tag_Single_Token: {
        Token_View single_token_view = *dyn_array_get(match, i);
        if (single_token_view.length != 1) {
          panic("Internal Error: Single Token matches should have a single token");
        }
        if (item->Single_Token.capture_name.length) {
          hash_map_set(map, item->Single_Token.capture_name, single_token_view);
        }
        break;
      }
      case Macro_Pattern_Tag_Any_Token_Sequence: {
        if (item->Any_Token_Sequence.capture_name.length) {
          hash_map_set(map, item->Any_Token_Sequence.capture_name, *dyn_array_get(match, i));
        }
        break;
      }
    }
  }
  Array_Const_Token_Ptr replacement = token_apply_macro_replacements(context, map, macro->replacement);
  hash_map_destroy(map);
  return replacement;
}

void
token_parse_macros(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Scope *scope
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Array_Token_View match = dyn_array_make(Array_Token_View);
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);

      start: for (;;) {
        for (u64 i = 0; i < dyn_array_length(*tokens); ++i) {
          if (macro->statement_start && i != 0) break;
          Token_View sub_view = token_view_rest(token_view_from_token_array(*tokens), i);
          u64 match_length = token_match_pattern(sub_view, macro, &match);
          if (match_length) {
            Array_Const_Token_Ptr replacement = token_parse_macro_match(context, match, macro);
            dyn_array_splice(*tokens, i, match_length, replacement);
            dyn_array_destroy(replacement);
            goto start;
          }
        }
        break;
      }
    }
  }
  dyn_array_destroy(match);
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_View view
);

Descriptor *
token_match_type(
  Compilation_Context *context,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Descriptor *descriptor = token_match_fixed_array_type(context, view);
  if (descriptor) return descriptor;
  if (!view.length) panic("Caller must not call token_match_type with empty token list");
  const Token *token = token_view_get(view, 0);
  if (view.length > 1) {
    context_error_snprintf(
      context, token->source_range, "Can not resolve type"
    );
    return 0;
  }
  return token_force_type(context, context->scope, token);
}

bool
token_maybe_split_on_operator(
  Token_View view,
  Slice operator,
  Token_View *lhs,
  Token_View *rhs,
  const Token **operator_token
) {
  u64 lhs_end = 0;
  u64 rhs_start = 0;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    if (token->tag == Token_Tag_Operator && slice_equal(token->source, operator)) {
      *operator_token = token;
      lhs_end = i;
      rhs_start = i + 1;
      break;
    }
  }

  if (lhs_end == 0) {
    *lhs = *rhs = (Token_View){0};
    return false;
  }

  *lhs = (Token_View) { .tokens = view.tokens, .length = lhs_end };
  *rhs = token_view_rest(view, rhs_start);

  return true;
}

Token_Match_Arg
token_match_argument(
  Compilation_Context *context,
  Token_View view,
  Descriptor_Function *function
) {
  Token_Match_Arg arg = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return arg;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;
  if (token_maybe_split_on_operator(view, slice_literal(":"), &lhs, &rhs, &operator)) {
    if (lhs.length == 0) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects an identifier on the left hand side"
      );
      goto err;
    }
    if (lhs.length > 1 || !token_match(lhs.tokens[0], &(Token_Pattern){ .tag = Token_Tag_Id })) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects only a single identifier on the left hand side"
      );
      goto err;
    }
    Descriptor *type_descriptor = token_match_type(context, rhs);
    if (!type_descriptor) {
      goto err;
    }

    arg.name = lhs.tokens[0]->source;

    if (type_descriptor == &descriptor_any) {
      // TODO figure out what should happen here or not use any type for macros
      arg.value = value_any(context->allocator);
    } else {
      arg.value = function_next_argument_value(
        context->allocator, function, type_descriptor
      );
    }
  } else {
    arg.value = token_parse_constant_expression(context, view);
  }

  err:
  return arg;
}

Token_Match_Arg
token_match_return_type(
  Compilation_Context *context,
  Token_View view,
  Descriptor_Function *function
) {
  Token_Match_Arg arg = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return arg;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;
  Descriptor *type_descriptor;
  if (token_maybe_split_on_operator(view, slice_literal(":"), &lhs, &rhs, &operator)) {
    if (lhs.length == 0) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects an identifier on the left hand side"
      );
      goto err;
    }
    if (lhs.length > 1 || !token_match(lhs.tokens[0], &(Token_Pattern){ .tag = Token_Tag_Id })) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects only a single identifier on the left hand side"
      );
      goto err;
    }
    type_descriptor = token_match_type(context, rhs);
    arg.name = lhs.tokens[0]->source;
  } else {
    type_descriptor = token_match_type(context, view);
  }

  if (!type_descriptor) {
    goto err;
  }

  function_return_descriptor(context, function, type_descriptor);
  arg.value = function->returns;

  err:
  return arg;
}

PRELUDE_NO_DISCARD Mass_Result
token_force_value(
  Compilation_Context *context,
  const Token *token,
  Value *result_value
) {
  MASS_TRY(*context->result);

  Scope *scope = context->scope;
  switch(token->tag) {
    case Token_Tag_None: {
      panic("Internal Error: Encountered token with an uninitialized tag");
      break;
    }
    case Token_Tag_String: {
      Slice string = token->String.slice;
      Value *string_bytes = value_global_c_string_from_slice(context, string);
      load_address(context, context->builder, &token->source_range, result_value, string_bytes);
      return *context->result;
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name, Scope_Entry_Flags_None);
      if (!value) {
        MASS_TRY(*context->result);
        context_error_snprintf(
          context, token->source_range,
          "Undefined variable %"PRIslice,
          SLICE_EXPAND_PRINTF(name)
        );
        return *context->result;
      } else {
        move_value(context->allocator, context->builder, &token->source_range, result_value, value);
      }
      return *context->result;
    }
    case Token_Tag_Value: {
      if (token->Value.value) {
        move_value(
          context->allocator, context->builder, &token->source_range, result_value, token->Value.value
        );
      } else {
        // TODO consider what should happen here
      }
      return *context->result;
    }
    case Token_Tag_Group: {
      switch(token->Group.tag) {
        case Token_Group_Tag_Paren: {
          Token_View expression_tokens = token_view_from_token_array(token->Group.children);
          token_parse_expression(context, expression_tokens, result_value);
          return *context->result;
        }
        case Token_Group_Tag_Curly: {
          token_parse_block(context, token, result_value);
          return *context->result;
        }
        case Token_Group_Tag_Square: {
          panic("TODO");
          return *context->result;
        }
      }
      break;
    }

    case Token_Tag_Operator: {
      panic("TODO");
      return *context->result;
    }
  }
  panic("Not reached");
  return *context->result;
}


// FIXME pass in the function definition
Array_Value_Ptr
token_match_call_arguments(
  Compilation_Context *context,
  const Token *token
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (context->result->tag != Mass_Result_Tag_Success) return result;

  if (dyn_array_length(token->Group.children) != 0) {
    Token_View children = token_view_from_token_array(token->Group.children);
    Token_View_Split_Iterator it = { .view = children };

    while (!it.done) {
      if (context->result->tag != Mass_Result_Tag_Success) return result;
      Token_View view = token_split_next(&it, &token_pattern_comma_operator);
      // TODO :TargetValue
      // There is an interesting conundrum here that we need to know the types of the
      // arguments for overload resolution, but then we need the exact function definition
      // to know the result_value definition to do the evaluation. Proper solution would
      // be to introduce :TypeOnlyEvalulation, but for now we will just create a special
      // target value that can be anything that will behave like type inference and is
      // needed regardless for something like x := (...)
      Value *result_value = value_any(context->allocator);
      token_parse_expression(context, view, result_value);
      dyn_array_push(result, result_value);
    }
  }
  return result;
}

static inline Token *
token_value_make(
  Compilation_Context *context,
  Value *result,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token *result_token = allocator_allocate(context->allocator, Token);
  *result_token = (Token){
    .tag = Token_Tag_Value,
    .source_range = source_range,
    .source = slice_sub_range(source_range.file->text, source_range.offsets),
    .Value = { result },
  };
  return result_token;
}

void
scope_add_macro(
  Scope *scope,
  Macro *macro
) {
  if (!dyn_array_is_initialized(scope->macros)) {
    scope->macros = dyn_array_make(Array_Macro_Ptr);
  }
  dyn_array_push(scope->macros, macro);
}

const Token *
token_handle_user_defined_operator(
  Compilation_Context *context,
  Token_View args,
  User_Defined_Operator *operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // FIXME This is almost identical with the macro function call

  // We make a nested scope based on the original scope
  // instead of current scope for hygiene reasons.
  Scope *body_scope = scope_make(context->allocator, operator->scope);
  assert(operator->argument_count == args.length);

  for (u8 i = 0; i < operator->argument_count; ++i) {
    Slice arg_name = operator->argument_names[i];
    Value *arg_value = value_any(context->allocator);
    token_force_value(context, token_view_get(args, i), arg_value);
    scope_define(body_scope, arg_name, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = arg_value,
    });
  }

  Value *result_value = value_any(context->allocator);

  // Define a new return target label and value so that explicit return statements
  // jump to correct location and put value in the right place
  Label_Index fake_return_label_index =
    make_label(context->program, &context->program->data_section);
  {
    scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = &(Value) {
        .descriptor = &descriptor_void,
        .operand = code_label32(fake_return_label_index),
      },
    });
    scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = result_value,
    });
  }

  const Token *body = operator->body;
  {
    Compilation_Context body_context = *context;
    body_context.scope = body_scope;
    token_parse_block(&body_context, body, result_value);
  }

  Source_Range call_range = source_range_from_token_view(args);

  push_instruction(
    &context->builder->code_block.instructions,
    call_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = fake_return_label_index
    }
  );

  return token_value_make(context, result_value, call_range);
}

static inline Slice
operator_fixity_to_lowercase_slice(
  Operator_Fixity fixity
) {
  switch(fixity) {
    case Operator_Fixity_Infix: return slice_literal("an infix");
    case Operator_Fixity_Prefix: return slice_literal("a prefix");
    case Operator_Fixity_Postfix: return slice_literal("a postfix");
  }
  panic("Unexpected fixity");
  return slice_literal("");
}

bool
token_parse_operator_definition(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  User_Defined_Operator *operator = 0;

  u64 peek_index = 0;
  Token_Match(keyword_token, .tag = Token_Tag_Id, .source = slice_literal("operator"));

  Token_Maybe_Match(precedence_token, 0);

  if (!precedence_token) {
    context_error_snprintf(
      context, keyword_token->source_range,
      "'operator' keyword must be followed by a precedence number"
    );
    goto err;
  }

  Value *precedence_value = value_any(context->allocator);
  token_force_value(context, precedence_token, precedence_value);

  if (!precedence_value || !descriptor_is_unsigned_integer(precedence_value->descriptor)) {
    context_error_snprintf(
      context, precedence_token->source_range,
      "Operator precedence must be an unsigned number"
    );
    goto err;
  }

  assert(precedence_value->operand.tag == Operand_Tag_Immediate);

  u64 precendence = operand_immediate_value_up_to_u64(&precedence_value->operand);
  (void)precendence;

  Token_Maybe_Match(pattern_token, .group_tag = Token_Group_Tag_Paren);

  if (!pattern_token) {
    context_error_snprintf(
      context, precedence_token->source_range,
      "Operator definition have a pattern in () following the precedence"
    );
    goto err;
  }

  Token_Maybe_Match(body_token, .group_tag = Token_Group_Tag_Curly);

  if (!body_token) {
    context_error_snprintf(
      context, pattern_token->source_range,
      "Operator definition have a macro body in {} following the pattern"
    );
    goto err;
  }

  Token_View definition = token_view_from_token_array(pattern_token->Group.children);

  operator = allocator_allocate(context->allocator, User_Defined_Operator);
  *operator = (User_Defined_Operator) {
    .body = body_token,
    .scope = context->scope,
  };

  const Token *operator_token;
  const Token *arguments[2] = {0};

  // prefix and postfix
  if (definition.length == 2) {
    operator->fixity = token_view_get(definition, 0)->tag == Token_Tag_Operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Postfix;
    operator->argument_count = 1;
    if (operator->fixity == Operator_Fixity_Prefix) {
      operator_token = token_view_get(definition, 0);
      arguments[0] = token_view_get(definition, 1);
    } else {
      operator_token = token_view_get(definition, 1);
      arguments[0] = token_view_get(definition, 0);
    }
  } else if (definition.length == 3) { // infix
    operator->argument_count = 2;
    operator->fixity = Operator_Fixity_Infix;
    operator_token = token_view_get(definition, 1);
    arguments[0] = token_view_get(definition, 0);
    arguments[1] = token_view_get(definition, 2);
  } else {
    operator_token = 0;
    context_error_snprintf(
      context, pattern_token->source_range,
      "Expected the pattern to have two (for prefix / postfix) or three tokens"
    );
    goto err;
  }

  for (u8 i = 0; i < operator->argument_count; ++i) {
    if (arguments[i]->tag != Token_Tag_Id) {
      context_error_snprintf(
        context, arguments[i]->source_range,
        "Operator argument must be an identifier"
      );
      goto err;
    }
    operator->argument_names[i] = arguments[i]->source;
  }

  if (operator_token->tag != Token_Tag_Operator) {
    context_error_snprintf(
      context, operator_token->source_range,
      "Expected an operator token here"
    );
    goto err;
  }

  Scope_Entry *existing_scope_entry =
    scope_lookup(context->scope, operator_token->source, Scope_Entry_Flags_Static);
  while (existing_scope_entry) {
    if (existing_scope_entry->type != Scope_Entry_Type_Operator) {
      panic("Internal Error: Found an operator-like scope entry that is not an operator");
    }
    Scope_Entry_Operator *operator_entry = &existing_scope_entry->Operator;
    if ((
      operator->fixity == operator_entry->fixity
    ) || (
      operator->fixity == Operator_Fixity_Infix &&
      operator_entry->fixity == Operator_Fixity_Postfix
    ) || (
      operator->fixity == Operator_Fixity_Postfix &&
      operator_entry->fixity == Operator_Fixity_Infix
    )) {
      Slice existing = operator_fixity_to_lowercase_slice(operator_entry->fixity);
      context_error_snprintf(
        context, keyword_token->source_range,
        "There is already %"PRIslice" operator %"PRIslice
        ". You can only have one definition for prefix and one for infix or suffix.",
        SLICE_EXPAND_PRINTF(existing), SLICE_EXPAND_PRINTF(operator_token->source)
      );
      goto err;
    }
    existing_scope_entry = existing_scope_entry->next_overload;
  }

  scope_define(context->scope, operator_token->source, (Scope_Entry) {
    .type = Scope_Entry_Type_Operator,
    .flags = Scope_Entry_Flags_Static,
    .Operator = {
      .precedence = precendence,
      .argument_count = operator->argument_count,
      .fixity = operator->fixity,
      .handler = token_handle_user_defined_operator,
      .handler_payload = operator,
    }
  });

  return true;

  err:
  if (operator) allocator_deallocate(context->allocator, operator, sizeof(*operator));
  return true;
}

bool
token_parse_syntax_definition(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id, .source = slice_literal("syntax"));

  Token_Maybe_Match(pattern_token, .group_tag = Token_Group_Tag_Paren);

  if (!pattern_token) {
    panic("TODO user error");
  }

  Token_View replacement = token_view_rest(view, peek_index);
  Token_View definition = token_view_from_token_array(pattern_token->Group.children);

  Array_Macro_Pattern pattern = dyn_array_make(Array_Macro_Pattern);
  bool statement_start = false;
  bool statement_end = false;

  for (u64 i = 0; i < definition.length; ++i) {
    const Token *token = token_view_get(definition, i);

    switch(token->tag) {
      case Token_Tag_None: {
        panic("Unexpected None Token");
        break;
      }
      case Token_Tag_String: {
        dyn_array_push(pattern, (Macro_Pattern) {
          .tag = Macro_Pattern_Tag_Single_Token,
          .Single_Token = {
            .token_pattern = {
              .source = token->String.slice,
            }
          },
        });
        break;
      }
      case Token_Tag_Group: {
        if (dyn_array_length(token->Group.children)) {
          context_error_snprintf(
            context, token->source_range,
            "Nested group matches are not supported in syntax declarations (yet)"
          );
          goto err;
        }
        dyn_array_push(pattern, (Macro_Pattern) {
          .tag = Macro_Pattern_Tag_Single_Token,
          .Single_Token = {
            .token_pattern = {
              .group_tag = token->Group.tag,
            }
          },
        });
        break;
      }
      case Token_Tag_Operator: {
        if (
          slice_equal(token->source, slice_literal("..@")) ||
          slice_equal(token->source, slice_literal(".@")) ||
          slice_equal(token->source, slice_literal("@"))
        ) {
          const Token *pattern_name = token_view_peek(definition, ++i);
          if (!pattern_name || pattern_name->tag != Token_Tag_Id) {
            context_error_snprintf(
              context, token->source_range,
              "@ operator in a syntax definition requires an id after it"
            );
            goto err;
          }
          Macro_Pattern *last_pattern = 0;
          if (slice_equal(token->source, slice_literal("@"))) {
            last_pattern = dyn_array_last(pattern);
          } else if (slice_equal(token->source, slice_literal(".@"))) {
            last_pattern = dyn_array_push(pattern, (Macro_Pattern) {
              .tag = Macro_Pattern_Tag_Single_Token,
            });
          } else if (slice_equal(token->source, slice_literal("..@"))) {
            last_pattern = dyn_array_push(pattern, (Macro_Pattern) {
              .tag = Macro_Pattern_Tag_Any_Token_Sequence,
            });
          } else {
            panic("Internal Error: Unexpected @-like operator");
          }
          if (!last_pattern) {
            context_error_snprintf(
              context, token->source_range,
              "@ requires a valid pattern before it"
            );
            goto err;
          }
          switch(last_pattern->tag) {
            case Macro_Pattern_Tag_Single_Token: {
              last_pattern->Single_Token.capture_name = pattern_name->source;
              break;
            }
            case Macro_Pattern_Tag_Any_Token_Sequence: {
              last_pattern->Any_Token_Sequence.capture_name = pattern_name->source;
              break;
            }
          }
        } else if (slice_equal(token->source, slice_literal("^"))) {
          if (i != 0) {
            context_error_snprintf(
              context, token->source_range,
              "^ operator (statement start match) can only appear at the start of the pattern."
            );
            goto err;
          }
          statement_start = true;
        } else if (slice_equal(token->source, slice_literal("$"))) {
          if (i != definition.length - 1) {
            context_error_snprintf(
              context, token->source_range,
              "$ operator (statement end match) can only appear at the end of the pattern."
            );
            goto err;
          }
          statement_end = true;
        } else {
          context_error_snprintf(
            context, token->source_range,
            "Unsupported operator %"PRIslice" in a syntax definition",
            SLICE_EXPAND_PRINTF(token->source)
          );
          goto err;
        }
        break;
      }
      case Token_Tag_Id:
      case Token_Tag_Value: {
        context_error_snprintf(
          context, token->source_range,
          "Unsupported token tag in a syntax definition"
        );
        goto err;
        break;
      }
    }
  }

  Macro *macro = allocator_allocate(context->allocator, Macro);
  *macro = (Macro){
    .pattern = pattern,
    .replacement = replacement,
    .statement_start = statement_start,
    .statement_end = statement_end,
  };

  scope_add_macro(scope, macro);


  return true;

  err:
  dyn_array_destroy(pattern);
  return true;
}

bool
token_match_struct_field(
  Compilation_Context *context,
  Descriptor *struct_descriptor,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_rest(view, peek_index);
  Descriptor *descriptor = token_match_type(context, rest);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, name->source);
  return true;
}

Token *
token_process_bit_type_definition(
  Compilation_Context *context,
  Token_View view,
  const Token *args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View args_view = { .tokens = &args, .length = 1 };
  Value *bit_size_value = token_parse_constant_expression(context, args_view);
  if (!bit_size_value) {
    // TODO print error
    goto err;
  }

  if (!descriptor_is_integer(bit_size_value->descriptor)) {
    // TODO err
    goto err;
  }
  if (bit_size_value->operand.tag != Operand_Tag_Immediate) {
    // TODO err
    goto err;
  }
  u64 bit_size = s64_to_u64(operand_immediate_value_up_to_s64(&bit_size_value->operand));
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Opaque,
    .Opaque = { .bit_size = bit_size },
  };

  Value *result = allocator_allocate(context->allocator, Value);
  *result = type_value_for_descriptor(descriptor);
  return token_value_make(context, result, args->source_range);

  err:
  return 0;
}

Token *
token_process_c_struct_definition(
  Compilation_Context *context,
  Token_View view,
  const Token *args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!token_match(args, &(Token_Pattern) { .group_tag = Token_Group_Tag_Paren })) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct must be followed by ()"
    );
    goto err;
  }
  if (dyn_array_length(args->Group.children) != 1) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct expects 1 argument, got %"PRIu64,
      dyn_array_length(args->Group.children)
    );
    goto err;
  }
  const Token *layout_block = *dyn_array_get(args->Group.children, 0);
  if (!token_match(layout_block, &(Token_Pattern) { .group_tag = Token_Group_Tag_Curly })) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct expects a {} block as the argument"
    );
    goto err;
  }

  Value *result = allocator_allocate(context->allocator, Value);
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);

  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .Struct = {
      .fields = dyn_array_make(Array_Descriptor_Struct_Field),
    },
  };

  if (dyn_array_length(layout_block->Group.children) != 0) {
    Token_View layout_block_children =
      token_view_from_token_array(layout_block->Group.children);

    Token_View_Split_Iterator it = { .view = layout_block_children };
    while (!it.done) {
      Token_View field_view = token_split_next(&it, &token_pattern_semicolon);
      token_match_struct_field(context, descriptor, field_view);
    }
  }

  *result = type_value_for_descriptor(descriptor);
  return token_value_make(context, result, args->source_range);

  err:
  return 0;
}

Token *
token_import_match_arguments(
  Source_Range source_range,
  Token_View view,
  Compilation_Context *context
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  const Token *library_name_token = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_String,
  });
  if (!library_name_token) {
    context_error_snprintf(
      context, source_range,
      "First argument to external() must be a literal string"
    );
    return 0;
  }
  const Token *comma = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_Operator,
    .source = slice_literal(","),
  });
  if (!comma) {
    context_error_snprintf(
      context, source_range,
      "external(\"library_name\", \"symbol_name\") requires two arguments"
    );
    return 0;
  }
  const Token *symbol_name_token = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_String,
  });
  if (!symbol_name_token) {
    context_error_snprintf(
      context, source_range,
      "Second argument to external() must be a literal string"
    );
    return 0;
  }
  Slice library_name = library_name_token->String.slice;
  Slice symbol_name = symbol_name_token->String.slice;

  Value *result = allocator_allocate(context->allocator, Value);
  *result = (const Value) {
    .descriptor = 0,
    .operand = import_symbol(context, library_name, symbol_name),
  };
  return token_value_make(context, result, source_range_from_token_view(view));
}

typedef const Token *(*token_rewrite_expression_callback)(
  Compilation_Context *context,
  Token_View,
  Value *result_value,
  u64 *replacement_count
);

void
token_rewrite_expression(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Value *result_value,
  token_rewrite_expression_callback callback
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  // FIXME speed
  Array_Const_Token_Ptr replacement = dyn_array_make(Array_Const_Token_Ptr);
  start: for (;;) {
    for (u64 i = 0; i < dyn_array_length(*tokens); ++i) {
      Token_View rewrite_view = token_view_rest(token_view_from_token_array(*tokens), i);
      u64 replacement_count = 0;
      const Token *token = callback(context, rewrite_view, result_value, &replacement_count);
      if (replacement_count) {
        if (token) dyn_array_push(replacement, token);
        dyn_array_splice(*tokens, i, replacement_count, replacement);
        if (token) dyn_array_pop(replacement);
        goto start;
      }
    }
    break;
  }
  dyn_array_destroy(replacement);
}

Value *
token_process_function_literal(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  const Token *args,
  const Token *return_types,
  const Token *body
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Scope *function_scope = scope_make(context->allocator, context->program->global_scope);

  Function_Builder *builder = 0;
  Descriptor *descriptor = 0;

  if(!body) return 0;

  // TODO think about a better way to distinguish imports
  bool is_external = body->tag == Token_Tag_Value;

  if (is_external) {
    if(!body->Value.value) return 0;
    descriptor = allocator_allocate(context->allocator, Descriptor);
    *descriptor = (Descriptor) {
      .tag = Descriptor_Tag_Function,
      .Function = {
        .arguments = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator),
        .argument_names = dyn_array_make(Array_Slice, .allocator = context->allocator),
        .returns = 0,
      },
    };
    descriptor->Function.flags |= Descriptor_Function_Flags_External;
  } else {
    builder = fn_begin(context);
    descriptor = builder->value->descriptor;
    Value *return_label_value = allocator_allocate(context->allocator, Value);
    *return_label_value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(builder->code_block.end_label),
    };
    scope_define(function_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = return_label_value,
    });
  }

  if (dyn_array_length(return_types->Group.children) == 0) {
    descriptor->Function.returns = &void_value;
  } else {
    Token_View children = token_view_from_token_array(return_types->Group.children);
    Token_View_Split_Iterator it = { .view = children };

    for (u64 i = 0; !it.done; ++i) {
      if (i > 0) {
        context_error_snprintf(
          context, return_types->source_range,
          "Multiple return types are not supported at the moment"
        );
        return 0;
      }
      Token_View arg_view = token_split_next(&it, &token_pattern_comma_operator);

      Compilation_Context arg_context = *context;
      arg_context.scope_entry_lookup_flags = Scope_Entry_Flags_None;
      arg_context.scope = function_scope;
      arg_context.builder = builder;
      Token_Match_Arg arg = token_match_return_type(&arg_context, arg_view, &descriptor->Function);

      // Return values can be named
      if (arg.name.length) {
        scope_define(function_scope, arg.name, (Scope_Entry) {
          .type = Scope_Entry_Type_Value,
          .flags = Scope_Entry_Flags_None,
          .value = descriptor->Function.returns,
        });
      }

      // :ReturnTypeLargerThanRegister
      // Make sure we don't stomp the address of a larger-than-register
      // return value during the execution of the function
      if (
        !is_external &&
        arg.value->operand.tag == Operand_Tag_Memory &&
        arg.value->operand.Memory.location.tag == Memory_Location_Tag_Indirect
      ) {
        register_bitset_set(
          &builder->code_block.register_occupied_bitset,
          arg.value->operand.Memory.location.Indirect.base_register
        );
      }
    }
  }

  scope_define(function_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .flags = Scope_Entry_Flags_None,
    .value = descriptor->Function.returns,
  });

  if (dyn_array_length(args->Group.children) != 0) {
    Token_View children = token_view_from_token_array(args->Group.children);
    Token_View_Split_Iterator it = { .view = children };

    while (!it.done) {
      Token_View arg_view = token_split_next(&it, &token_pattern_comma_operator);
      Compilation_Context arg_context = *context;
      arg_context.scope_entry_lookup_flags = Scope_Entry_Flags_None;
      arg_context.scope = function_scope;
      arg_context.builder = builder;
      Token_Match_Arg arg = token_match_argument(&arg_context, arg_view, &descriptor->Function);
      MASS_ON_ERROR(*context->result) return 0;

      // Literal values do not have a name ATM
      if (arg.name.length) {
        scope_define(function_scope, arg.name, (Scope_Entry) {
          .type = Scope_Entry_Type_Value,
          .flags = Scope_Entry_Flags_None,
          .value = arg.value,
        });
      }
      dyn_array_push(descriptor->Function.argument_names, arg.name);
      dyn_array_push(descriptor->Function.arguments, arg.value);

      if (!is_external && arg.value->operand.tag == Operand_Tag_Register) {
        register_bitset_set(
          &builder->code_block.register_occupied_bitset,
          arg.value->operand.Register.index
        );
      }
    }
    assert(
      dyn_array_length(descriptor->Function.argument_names) ==
      dyn_array_length(descriptor->Function.arguments)
    );
  }

  Value *result = 0;
  if (is_external) {
    body->Value.value->descriptor = descriptor;
    result = body->Value.value;
  } else {
    descriptor->Function.scope = function_scope;
    descriptor->Function.body = body;
    descriptor->Function.builder = builder;
    descriptor->Function.flags |= Descriptor_Function_Flags_Pending_Body_Compilation;
    result = builder->value;
  }
  return result;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

Token *
compile_time_eval(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  const Token *first_token = token_view_get(view, 0);
  const Source_Range *source_range = &first_token->source_range;

  Program eval_program = {
    .import_libraries = dyn_array_copy(Array_Import_Library, context->program->import_libraries),
    // FIXME this is probably broken now because code ones should point to a different section
    .labels = dyn_array_copy(Array_Label, context->program->labels),
    .patch_info_array =
      dyn_array_copy(Array_Label_Location_Diff_Patch_Info, context->program->patch_info_array),
    .functions = dyn_array_copy(Array_Function_Builder, context->program->functions),
    .global_scope = scope_make(context->allocator, context->program->global_scope),
    .data_section = context->program->data_section,
    .code_section = {
      .buffer = bucket_buffer_make(.allocator = allocator_system),
      .permissions = Section_Permissions_Execute,
    }
  };

  Compilation_Context eval_context = *context;
  eval_context.scope_entry_lookup_flags = Scope_Entry_Flags_Static;
  eval_context.scope = scope_make(context->allocator, context->scope);
  eval_context.program = &eval_program;
  eval_context.builder = fn_begin(&eval_context);
  function_return_descriptor(
    context, &eval_context.builder->value->descriptor->Function, &descriptor_void
  );

  // FIXME We have to call token_parse_expression here before we figure out
  //       what is the return value because we need to figure out the return type.
  //       Ideally there would be a type-only eval available instead
  Value *expression_result_value = value_any(context->allocator);
  token_parse_expression(&eval_context, view, expression_result_value);
  MASS_ON_ERROR(*eval_context.result) {
    context->result = eval_context.result;
    return 0;
  }

  u32 result_byte_size = expression_result_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(context->allocator, result_byte_size, alignment);

  // Load the address of the result
  Register out_register = register_acquire_temp(eval_context.builder);
  Value out_value_register = {
    .descriptor = &descriptor_s64,
    .operand = {
      .tag = Operand_Tag_Register,
      .byte_size = 8,
      .Register.index = out_register
    }
  };
  Value result_address = {
    .descriptor = &descriptor_s64,
    .operand = imm64(context->allocator, (u64)result),
  };

  move_value(context->allocator, eval_context.builder, source_range, &out_value_register, &result_address);

  // Use memory-indirect addressing to copy
  Value *out_value = allocator_allocate(context->allocator, Value);
  *out_value = (Value) {
    .descriptor = expression_result_value->descriptor,
    .operand = (Operand){
      .tag = Operand_Tag_Memory,
      .byte_size = expression_result_value->operand.byte_size,
      .Memory.location = {
        .tag = Memory_Location_Tag_Indirect,
        .Indirect = {
          .base_register = out_register
        },
      },
    },
  };

  move_value(context->allocator, eval_context.builder, source_range, out_value, expression_result_value);
  fn_end(eval_context.builder);

  program_jit(&eval_context);

  fn_type_opaque jitted_code =
    (fn_type_opaque)value_as_function(&eval_program, eval_context.builder->value);

  jitted_code();
  Value *token_value = allocator_allocate(context->allocator, Value);
  *token_value = (Value) {
    .descriptor = out_value->descriptor,
  };
  switch(out_value->descriptor->tag) {
    case Descriptor_Tag_Void: {
      token_value->operand = (Operand){0};
      break;
    }
    case Descriptor_Tag_Any: {
      panic("Internal Error: We should never get Any type from comp time eval");
      break;
    }
    case Descriptor_Tag_Fixed_Size_Array:
    case Descriptor_Tag_Pointer:
    case Descriptor_Tag_Struct: {
      panic("TODO move to data section or maybe we should allocate from there right away above?");
      break;
    };
    case Descriptor_Tag_Opaque: {
      token_value->operand = (Operand){
        .tag = Operand_Tag_Immediate,
        .byte_size = result_byte_size,
        .Immediate = {
          .memory = result,
        },
      };
      break;
    }
    case Descriptor_Tag_Function: {
      panic("TODO figure out how that works");
      break;
    }
  }
  return token_value_make(context, token_value, source_range_from_token_view(view));
}

typedef struct {
  Slice source;
  Source_Range source_range;
  Scope_Entry_Operator scope_entry;
} Operator_Stack_Entry;
typedef dyn_array_type(Operator_Stack_Entry) Array_Operator_Stack_Entry;

typedef void (*Operator_Dispatch_Proc)(
  Compilation_Context *,
  Token_View,
  Array_Const_Token_Ptr *token_stack,
  Operator_Stack_Entry *operator
);

const Token *
token_handle_operand_variant_of(
  Compilation_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (dyn_array_length(args) != 1) {
    context_error_snprintf(
      context, *source_range,
      "operand_variant_of expects a single argument"
    );
    return 0;
  }

  Value *value = *dyn_array_get(args, 0);

  Value *result = allocator_allocate(context->allocator, Value);

  switch(value->operand.tag) {
    case Operand_Tag_None:
    case Operand_Tag_Any:
    case Operand_Tag_Immediate:
    case Operand_Tag_Eflags:
    case Operand_Tag_Xmm:
    case Operand_Tag_Memory: {
      panic("TODO implement operand reflection for more types");
      break;
    }
    case Operand_Tag_Register: {
      // TODO figure out a better encoding for small immediates
      u8 *register_index = allocator_allocate(context->allocator, u8);
      *register_index = value->operand.Register.index;
      Descriptor *result_descriptor = 0;
      switch(value->operand.byte_size) {
        case 1: result_descriptor = &descriptor_register_8; break;
        case 2: result_descriptor = &descriptor_register_16; break;
        case 4: result_descriptor = &descriptor_register_32; break;
        case 8: result_descriptor = &descriptor_register_64; break;
        default: {
          panic("Internal Error: Unsupported register size");
          break;
        }
      }
      *result = (Value) {
        .descriptor = &descriptor_register_8,
        .operand = {
          .tag = Operand_Tag_Immediate,
          .byte_size = 1,
          .Immediate.memory = register_index,
        }
      };
    }
  }

  const Token *result_token = token_value_make(context, result, *source_range);

  return result_token;
}

const Token *
token_handle_cast(
  Compilation_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  Descriptor *cast_to_descriptor =
    value_ensure_type(context, type, *source_range, slice_literal("TODO cast source"));

  assert(descriptor_is_integer(cast_to_descriptor));
  assert(value->descriptor->tag == cast_to_descriptor->tag);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *result = value;
  if (cast_to_byte_size != original_byte_size) {
    result = allocator_allocate(context->allocator, Value);

    if (value->operand.tag == Operand_Tag_Immediate) {
      if (descriptor_is_signed_integer(cast_to_descriptor)) {
        s64 integer = operand_immediate_value_up_to_s64(&value->operand);
        switch(cast_to_byte_size) {
          case 1: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm8(context->allocator, (s8)integer),
            };
            break;
          }
          case 2: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm16(context->allocator, (s16)integer),
            };
            break;
          }
          case 4: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm32(context->allocator, (s32)integer),
            };
            break;
          }
          case 8: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm64(context->allocator, (s64)integer),
            };
            break;
          }
          default: {
            panic("Unsupported integer size when casting");
            break;
          }
        }
      } else {
        u64 integer = operand_immediate_value_up_to_u64(&value->operand);
        switch(cast_to_byte_size) {
          case 1: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm8(context->allocator, (u8)integer),
            };
            break;
          }
          case 2: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm16(context->allocator, (u16)integer),
            };
            break;
          }
          case 4: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm32(context->allocator, (u32)integer),
            };
            break;
          }
          case 8: {
            *result = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm64(context->allocator, (u64)integer),
            };
            break;
          }
          default: {
            panic("Unsupported integer size when casting");
            break;
          }
        }
      }
    } else if (cast_to_byte_size < original_byte_size) {
      *result = (Value) {
        .descriptor = cast_to_descriptor,
        .operand = value->operand,
      };
      result->operand.byte_size = cast_to_byte_size;
    } else if (cast_to_byte_size > original_byte_size) {
      panic("Not implemented cast to a larger type");
      //result = reserve_stack(context->allocator, builder, cast_to_descriptor);
      //move_value(context->allocator, builder, source_range, result, value);
    }
  }
  const Token *result_token = token_value_make(context, result, *source_range);

  return result_token;
}

const Token *
token_handle_negation(
  Compilation_Context *context,
  Token_View args,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  assert(args.length == 1);
  const Token *token = token_view_get(args, 0);

  Value *value = value_any(context->allocator);
  token_force_value(context, token, value);
  if (descriptor_is_integer(value->descriptor) && value->operand.tag == Operand_Tag_Immediate) {
    switch(value->operand.byte_size) {
      case 1: {
        value->operand = imm8(context->allocator, -operand_immediate_memory_as_s8(&value->operand));
        break;
      }
      case 2: {
        value->operand = imm16(context->allocator, -operand_immediate_memory_as_s16(&value->operand));
        break;
      }
      case 4: {
        value->operand = imm32(context->allocator, -operand_immediate_memory_as_s32(&value->operand));
        break;
      }
      case 8: {
        value->operand = imm64(context->allocator, -operand_immediate_memory_as_s64(&value->operand));
        break;
      }
      default: {
        panic("Internal error, unexpected integer immediate size");
        break;
      }
    }
  } else {
    panic("TODO");
  }
  Token *new_token = token_value_make(context, value, token->source_range);
  return new_token;
}

void
token_dispatch_constant_operator(
  Compilation_Context *context,
  Token_View view,
  Array_Const_Token_Ptr *token_stack,
  Operator_Stack_Entry *operator_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;

  if (
    slice_equal(operator, slice_literal("-")) &&
    operator_entry->scope_entry.fixity == Operator_Fixity_Prefix
  ) {
    const Token *token = *dyn_array_pop(*token_stack);
    Token_View args_view = {
      .tokens = &token,
      .length = 1,
    };
    const Token *new_token = token_handle_negation(context, args_view, 0);
    dyn_array_push(*token_stack, new_token);
  } else if (slice_equal(operator, slice_literal("()"))) {
    const Token *args = *dyn_array_pop(*token_stack);
    const Token *function = *dyn_array_pop(*token_stack);

    const Token *result;
    // TODO turn `external` into a compile-time function call
    if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("external"))
    ) {
      Token_View args_children = token_view_from_token_array(args->Group.children);
      result = token_import_match_arguments(args->source_range, args_children, context);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("bit_type"))
    ) {
      result = token_process_bit_type_definition(context, view, args);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("c_struct"))
    ) {
      result = token_process_c_struct_definition(context, view, args);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("cast"))
    ) {
      // TODO turn `cast` into a compile-time function call / macro
      Array_Value_Ptr arg_values = token_match_call_arguments(context, args);
      result = token_handle_cast(context, &args->source_range, arg_values);
      dyn_array_destroy(arg_values);
    }  else {
      Token_View call_view = {
        .tokens = (const Token *[]){function, args},
        .length = 2,
      };
      result = compile_time_eval(context, call_view, context->scope);
    }
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("->"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    const Token *return_types = *dyn_array_pop(*token_stack);
    const Token *arguments = *dyn_array_pop(*token_stack);
    Value *function_value = token_process_function_literal(
      context, view, context->scope, arguments, return_types, body
    );
    Token *result = token_value_make(context, function_value, arguments->source_range);
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("macro"))) {
    const Token *function = *dyn_array_last(*token_stack);
    Value *function_value = value_any(context->allocator);
    token_force_value(context, function, function_value);
    if (function_value) {
      if (function_value->descriptor->tag == Descriptor_Tag_Function) {
        Descriptor_Function *descriptor = &function_value->descriptor->Function;
        if (descriptor->flags & Descriptor_Function_Flags_External) {
          context_error_snprintf(
            context, function->source_range,
            "External functions can not be macro"
          );
        } else {
          descriptor->flags |= Descriptor_Function_Flags_Macro;
        }
      } else {
        context_error_snprintf(
          context, function->source_range,
          "Trying to mark a non-function as macro"
        );
      }
    }
  } else {
    panic("TODO: Unknown operator");
  }
}

bool
token_handle_operator(
  Compilation_Context *context,
  Token_View view,
  Operator_Dispatch_Proc dispatch_proc,
  Array_Const_Token_Ptr *token_stack,
  Array_Operator_Stack_Entry *operator_stack,
  Slice new_operator,
  Source_Range source_range,
  Operator_Fixity fixity_mask
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Scope_Entry *scope_entry = scope_lookup(context->scope, new_operator, Scope_Entry_Flags_Static);

  if (!scope_entry) {
    context_error_snprintf(
      context, source_range,
      "Unknown operator %"PRIslice,
      SLICE_EXPAND_PRINTF(new_operator)
    );
    return false;
  }

  Scope_Entry_Operator *operator_entry = 0;
  while (scope_entry) {
    if (scope_entry->type != Scope_Entry_Type_Operator) {
      context_error_snprintf(
        context, source_range,
        "%"PRIslice" is not an operator",
        SLICE_EXPAND_PRINTF(new_operator)
      );
      return false;
    }
    operator_entry = &scope_entry->Operator;
    if (operator_entry->fixity & fixity_mask) break;
    scope_entry = scope_entry->next_overload;
  }

  while (dyn_array_length(*operator_stack)) {
    Operator_Stack_Entry *last_operator = dyn_array_last(*operator_stack);

    if (last_operator->scope_entry.precedence <= operator_entry->precedence) {
      break;
    }

    dyn_array_pop(*operator_stack);

    // apply the operator on the stack
    dispatch_proc(context, view, token_stack, last_operator);
  }
  dyn_array_push(*operator_stack, (Operator_Stack_Entry) {
    .source = new_operator,
    .source_range = source_range,
    .scope_entry = *operator_entry,
  });
  return true;
}

Value *
token_parse_constant_expression(
  Compilation_Context *context,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!view.length) {
    return &void_value;
  }
  Array_Const_Token_Ptr token_stack = dyn_array_make(Array_Const_Token_Ptr);
  Array_Operator_Stack_Entry operator_stack = dyn_array_make(Array_Operator_Stack_Entry);

  Value *result = 0;
  bool is_previous_an_operator = true;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    Operator_Fixity fixity_mask = is_previous_an_operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Infix | Operator_Fixity_Postfix;

    switch(token->tag) {
      case Token_Tag_None: {
        panic("Internal Error: Encountered token with an uninitialized tag");
        break;
      }
      case Token_Tag_String:
      case Token_Tag_Value: {
        dyn_array_push(token_stack, token);
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Group: {
        dyn_array_push(token_stack, token);
        switch (token->Group.tag) {
          case Token_Group_Tag_Paren: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_constant_operator,
                &token_stack, &operator_stack, slice_literal("()"), token->source_range,
                Operator_Fixity_Postfix
              )) goto err;
            }
            break;
          }
          case Token_Group_Tag_Curly: {
            // Nothing special to do for now?
            break;
          }
          case Token_Group_Tag_Square: {
            panic("TODO support parsing [] in constant expressions");
            break;
          }
        }
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Id: {
        Scope_Entry *scope_entry =
          scope_lookup(context->scope, token->source, Scope_Entry_Flags_Static);
        if (scope_entry && scope_entry->type == Scope_Entry_Type_Operator) {
          if (!token_handle_operator(
            context, view, token_dispatch_constant_operator,
            &token_stack, &operator_stack, token->source, token->source_range,
            // FIXME figure out how to deal with fixity for non-symbol operators
            Operator_Fixity_Prefix
          )) goto err;
          is_previous_an_operator = true;
        } else {
          is_previous_an_operator = false;
          dyn_array_push(token_stack, token);
        }

        break;
      }
      case Token_Tag_Operator: {
        Slice operator = token->source;
        if (!token_handle_operator(
          context, view, token_dispatch_constant_operator,
          &token_stack, &operator_stack, operator, token->source_range,
          fixity_mask
        )) goto err;
        is_previous_an_operator = true;
        break;
      }
    }
  }

  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_constant_operator(context, view, &token_stack, entry);
  }
  if (dyn_array_length(token_stack) == 1) {
    const Token *token = *dyn_array_last(token_stack);
    result = value_any(context->allocator);
    token_force_value(context, token, result);
  } else {
    // FIXME user error
  }

  err:

  dyn_array_destroy(token_stack);
  dyn_array_destroy(operator_stack);

  return result;
}

bool
token_parse_constant_definitions(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;

  if (!token_maybe_split_on_operator(view, slice_literal("::"), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) return false;
  const Token *name = token_view_get(lhs, 0);
  if (name->tag != Token_Tag_Id) return false;
  scope_define(context->scope, name->source, (Scope_Entry) {
    .type = Scope_Entry_Type_Lazy_Expression,
    .flags = Scope_Entry_Flags_Static,
    .lazy_expression = {
      .tokens = rhs,
      .scope = context->scope,
    },
  });
  return true;
}

bool
token_maybe_macro_call_with_lazy_arguments(
  Compilation_Context *context,
  Value *target,
  Token_View args_view,
  const Source_Range *call_source_range,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 arg_count = 0;
  for (
    Token_View_Split_Iterator it = {.view = args_view};
    !it.done;
    token_split_next(&it, &token_pattern_comma_operator)
  ) {
    arg_count++;
  }

  Value *maybe_macro_overload = 0;

  for (Value *to_call = target; to_call; to_call = to_call->next_overload) {
    assert(to_call->descriptor->tag == Descriptor_Tag_Function);
    Descriptor_Function *descriptor = &to_call->descriptor->Function;
    if (!(descriptor->flags & Descriptor_Function_Flags_Macro)) continue;
    if (arg_count != dyn_array_length(descriptor->argument_names)) continue;
    // TODO check for overlapping matches
    maybe_macro_overload = to_call;
  }

  if (!maybe_macro_overload) {
    return false;
  }

  Descriptor_Function *function = &maybe_macro_overload->descriptor->Function;
  assert(function->scope->parent);
  // We make a nested scope based on function's original parent scope
  // instead of current scope for hygiene reasons. I.e. function body
  // should not have access to locals inside the call scope.
  Scope *body_scope = scope_make(context->allocator, function->scope->parent);

  Token_View_Split_Iterator it = {.view = args_view};
  for (u64 i = 0; i < dyn_array_length(function->argument_names); ++i) {
    Slice arg_name = *dyn_array_get(function->argument_names, i);
    assert(!it.done);
    Token_View arg_expr = token_split_next(&it, &token_pattern_comma_operator);
    scope_define(body_scope, arg_name, (Scope_Entry) {
      .type = Scope_Entry_Type_Lazy_Expression,
      .flags = Scope_Entry_Flags_Static,
      .lazy_expression = {
        .tokens = arg_expr,
        .scope = context->scope,
        .maybe_builder = context->builder,
      },
    });
  }

  // Define a new return target label and value so that explicit return statements
  // jump to correct location and put value in the right place
  Label_Index fake_return_label_index =
    make_label(context->program, &context->program->data_section);
  {
    scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = &(Value) {
        .descriptor = &descriptor_void,
        .operand = code_label32(fake_return_label_index),
      },
    });
    assert(result_value);
    scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = result_value,
    });
  }

  const Token *body = function->body;
  {
    Compilation_Context body_context = *context;
    body_context.scope = body_scope;
    token_parse_block(&body_context, body, result_value);
  }

  push_instruction(
    &context->builder->code_block.instructions,
    *call_source_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = fake_return_label_index
    }
  );
  return true;
}

const Token *
token_handle_function_call(
  Compilation_Context *context,
  const Token *target_token,
  const Token *args_token,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *target = value_any(context->allocator);
  MASS_ON_ERROR(token_force_value(context, target_token, target)) return 0;
  assert(token_match(args_token, &(Token_Pattern){.group_tag = Token_Group_Tag_Paren}));

  // TODO consider how this should be exposed in the syntax
  //Token_View args_view = token_view_from_token_array(args_token->Group.children);
  //const Source_Range *call_source_range = &target_token->source_range;
  //if (token_maybe_macro_call_with_lazy_arguments(
    //context, target, args_view, call_source_range, builder, result_value
  //)) {
    //return token_value_make(context, result_value, *call_source_range);
  //}

  Array_Value_Ptr args;

  // FIXME this is a bit of hack to make sure we don't use argument registers for
  //       computing arguments as it might end up in the wrong one and overwrite.
  //       The right fix for that is to add type-only evaluation to figure out the
  //       correct target register for each argument and use it during mathcing.
  //       We will need type-only eval anyway for things like `typeof(some expression)`.
  {
    Register arg_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
    bool acquired_registers[countof(arg_registers)] = {0};
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      Register reg_index = arg_registers[i];
      if (!register_bitset_get(context->builder->code_block.register_occupied_bitset, reg_index)) {
        register_acquire(context->builder, reg_index);
        acquired_registers[i] = true;
      }
    }

    args = token_match_call_arguments(context, args_token);

    // Release any registers that we fake acquired to make sure that the actual call
    // does not unnecessarily store them to stack
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      if (acquired_registers[i]) {
        Register reg_index = arg_registers[i];
        register_release(context->builder, reg_index);
      }
    }
  }

  if (target->descriptor->tag != Descriptor_Tag_Function) {
    context_error_snprintf(
      context, target_token->source_range,
      "%"PRIslice" is not a function",
      SLICE_EXPAND_PRINTF(target_token->source)
    );
    return false;
  }
  Token *result = 0;
  const Source_Range *source_range = &target_token->source_range;

  struct Overload_Match { Value *value; s64 score; } match = { .score = -1 };
  for (Value *to_call = target; to_call; to_call = to_call->next_overload) {
    assert(to_call->descriptor->tag == Descriptor_Tag_Function);
    Descriptor_Function *descriptor = &to_call->descriptor->Function;
    if (dyn_array_length(args) != dyn_array_length(descriptor->arguments)) continue;
    s64 score = calculate_arguments_match_score(descriptor, args);
    if (score == match.score) {
      // TODO improve error message
      // TODO provide names of matched overloads
      context_error_snprintf(
        context, target_token->source_range,
        "Could not decide which overload to pick"
      );
      return 0;
    } else if (score > match.score) {
      match.value = to_call;
      match.score = score;
    } else {
      // Skip a worse match
    }
  }

  Value *overload = match.value;
  if (overload) {
    Value *return_value;
    Descriptor_Function *function = &overload->descriptor->Function;

    if (function->flags & Descriptor_Function_Flags_Macro) {
      assert(function->scope->parent);
      // We make a nested scope based on function's original parent scope
      // instead of current scope for hygiene reasons. I.e. function body
      // should not have access to locals inside the call scope.
      Scope *body_scope = scope_make(context->allocator, function->scope->parent);

      for (u64 i = 0; i < dyn_array_length(function->arguments); ++i) {
        Slice arg_name = *dyn_array_get(function->argument_names, i);
        Value *arg_value = *dyn_array_get(args, i);
        scope_define(body_scope, arg_name, (Scope_Entry) {
          .type = Scope_Entry_Type_Value,
          // FIXME Think if this is correct
          .flags = context->scope_entry_lookup_flags,
          .value = arg_value,
        });
      }
      return_value = result_value;

      // Define a new return target label and value so that explicit return statements
      // jump to correct location and put value in the right place
      Label_Index fake_return_label_index =
        make_label(context->program, &context->program->data_section);
      {
        scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
          .type = Scope_Entry_Type_Value,
          .flags = Scope_Entry_Flags_None,
          .value = &(Value) {
            .descriptor = &descriptor_void,
            .operand = code_label32(fake_return_label_index),
          },
        });
        assert(return_value);
        scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
          .type = Scope_Entry_Type_Value,
          .flags = Scope_Entry_Flags_None,
          .value = return_value,
        });
      }

      const Token *body = function->body;
      {
        Compilation_Context body_context = *context;
        body_context.scope = body_scope;
        token_parse_block(&body_context, body, return_value);
      }

      push_instruction(
        &context->builder->code_block.instructions,
        target_token->source_range,
        (Instruction) {
          .type = Instruction_Type_Label,
          .label = fake_return_label_index
        }
      );
    } else {
      return_value = call_function_overload(context, source_range, overload, args);
    }

    result = token_value_make(context, return_value, target_token->source_range);
  } else {
    // TODO add better error message
    context_error_snprintf(
      context, target_token->source_range,
      "Could not find matching overload"
    );
  }
  dyn_array_destroy(args);
  return result;
}

void
token_dispatch_operator(
  Compilation_Context *context,
  Token_View view,
  Array_Const_Token_Ptr *token_stack,
  Operator_Stack_Entry *operator_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;
  const Token *result_token;

  if (operator_entry->scope_entry.handler) {
    u64 argument_count = operator_entry->scope_entry.argument_count;
    if (dyn_array_length(*token_stack) < argument_count) {
      // FIXME provide source range
      context_error_snprintf(
        context, operator_entry->source_range,
        "Operator %"PRIslice" required %"PRIu64", got %"PRIu64,
        SLICE_EXPAND_PRINTF(operator), argument_count, dyn_array_length(*token_stack)
      );
      return;
    }
    // TODO maybe reverse the arguments in place on the stack @Speed
    Array_Const_Token_Ptr args = dyn_array_make(
      Array_Const_Token_Ptr, .capacity = argument_count
    );
    for (u64 i = 0; i < argument_count; ++i) {
      dyn_array_push(args, *dyn_array_pop(*token_stack));
    }

    Token_View args_view = token_view_from_token_array(args);
    result_token = operator_entry->scope_entry.handler(
      context, args_view, operator_entry->scope_entry.handler_payload
    );
    dyn_array_destroy(args);
  } else if (slice_equal(operator, slice_literal("[]"))) {
    const Token *brackets = *dyn_array_pop(*token_stack);
    const Token *target_token = *dyn_array_pop(*token_stack);

    Value *array = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, target_token, array)) return;
    Value *index_value = value_any(context->allocator);
    Token_View index_tokens = token_view_from_token_array(brackets->Group.children);
    token_parse_expression(context, index_tokens, index_value);
    assert(array->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
    assert(array->operand.tag == Operand_Tag_Memory);
    assert(array->operand.Memory.location.tag == Memory_Location_Tag_Indirect);
    assert(!array->operand.Memory.location.Indirect.maybe_index_register.has_value);

    Descriptor *item_descriptor = array->descriptor->Fixed_Size_Array.item;
    u32 item_byte_size = descriptor_byte_size(item_descriptor);

    Value *result = allocator_allocate(context->allocator, Value);
    *result = (Value) {
      .descriptor = item_descriptor,
      .operand = array->operand
    };
    result->operand.byte_size = item_byte_size;
    if (index_value->operand.tag == Operand_Tag_Immediate) {
      s32 index = s64_to_s32(operand_immediate_value_up_to_s64(&index_value->operand));
      result->operand.Memory.location.Indirect.offset = index * item_byte_size;
    } else {
      // @InstructionQuality
      // This code is very general in terms of the operands where the base
      // or the index are stored, but it is

      Register reg = register_acquire_temp(context->builder);
      Value *new_base_register =
        value_register_for_descriptor(context->allocator, reg, &descriptor_s64);

      // Move the index into the register
      move_value(
        context->allocator,
        context->builder,
        &target_token->source_range,
        new_base_register,
        index_value
      );

      // Multiply index by the item byte size
      multiply(
        context->allocator,
        context->builder,
        &target_token->source_range,
        new_base_register,
        new_base_register,
        value_from_s64(context->allocator, item_byte_size)
      );

      {
        // @InstructionQuality
        // TODO If the source does not have index, on X64 it should be possible to avoid
        //      using an extra register and put the index into SIB on x64

        // Load previous address into a temp register
        Register temp_register = register_acquire_temp(context->builder);
        Value temp_value = {
          .descriptor = &descriptor_s64,
          .operand = operand_register_for_descriptor(temp_register, &descriptor_s64)
        };

        load_address(context, context->builder, &target_token->source_range, &temp_value, array);

        plus(
          context->allocator,
          context->builder,
          &target_token->source_range,
          new_base_register,
          new_base_register,
          &temp_value
        );
        register_release(context->builder, temp_register);
      }

      result->operand = (Operand) {
        .tag = Operand_Tag_Memory,
        .byte_size = item_byte_size,
        .Memory.location = {
          .tag = Memory_Location_Tag_Indirect,
          .Indirect = {
            .base_register = new_base_register->operand.Register.index,
          }
        }
      };
    }
    result_token = token_value_make(context, result, brackets->source_range);
  } else if (slice_equal(operator, slice_literal("()"))) {
    const Token *args_token = *dyn_array_pop(*token_stack);
    const Token *target = *dyn_array_pop(*token_stack);
    // TODO turn `cast` into a compile-time function call / macro
    if (
      target->tag == Token_Tag_Id &&
      slice_equal(target->source, slice_literal("cast"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      result_token = token_handle_cast(context, &args_token->source_range, args);
      dyn_array_destroy(args);
    } else if (
      target->tag == Token_Tag_Id &&
      slice_equal(target->source, slice_literal("operand_variant_of"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      result_token = token_handle_operand_variant_of(context, &args_token->source_range, args);
      dyn_array_destroy(args);
    } else {
      result_token = token_handle_function_call(
        context, target, args_token, value_any(context->allocator)
      );
    }
  } else if (slice_equal(operator, slice_literal("&"))) {
    const Token *pointee_token = *dyn_array_pop(*token_stack);

    Value *pointee = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, pointee_token, pointee)) return;

    Descriptor *pointer_descriptor = descriptor_pointer_to(context->allocator, pointee->descriptor);
    // FIXME figure out how to avoid stack allocation
    Value *result_value = reserve_stack(context->allocator, context->builder, pointer_descriptor);
    load_address(
      context, context->builder, &pointee_token->source_range, result_value, pointee
    );

    result_token = token_value_make(context, result_value, pointee_token->source_range);
  } else if (slice_equal(operator, slice_literal("@"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    if (body->tag == Token_Tag_Group && body->Group.tag == Token_Group_Tag_Paren) {
      Token_View eval_view = token_view_from_token_array(body->Group.children);
      result_token = compile_time_eval(context, eval_view, context->scope);
    } else {
      context_error_snprintf(
        context, body->source_range,
        "@ operator must be followed by a parenthesized expression"
      );
      result_token = token_value_make(context, 0, body->source_range);
    }
  } else if (slice_equal(operator, slice_literal("."))) {
    const Token *rhs = *dyn_array_pop(*token_stack);
    const Token *lhs = *dyn_array_pop(*token_stack);

    Value *result_value = 0;
    if (rhs->tag == Token_Tag_Id) {
      Value *struct_value = value_any(context->allocator);
      MASS_ON_ERROR(token_force_value(context, lhs, struct_value)) return;
      result_value = struct_get_field(context->allocator, struct_value, rhs->source);
    } else {
      context_error_snprintf(
        context, rhs->source_range,
        "Right hand side of the . operator must be an identifier"
      );
      return;
    }
    result_token = token_value_make(context, result_value, lhs->source_range);
  } else if (
    slice_equal(operator, slice_literal("+")) ||
    slice_equal(operator, slice_literal("-")) ||
    slice_equal(operator, slice_literal("*")) ||
    slice_equal(operator, slice_literal("/")) ||
    slice_equal(operator, slice_literal("%"))
  ) {
    const Token *rhs = *dyn_array_pop(*token_stack);
    const Token *lhs = *dyn_array_pop(*token_stack);

    Value *lhs_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, lhs, lhs_value)) return;
    Value *rhs_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, rhs, rhs_value)) return;

    Descriptor *larger_descriptor =
      descriptor_byte_size(lhs_value->descriptor) > descriptor_byte_size(rhs_value->descriptor)
      ? lhs_value->descriptor
      : rhs_value->descriptor;

    Function_Builder *builder = context->builder;

    // FIXME figure out how to avoid this
    Value *result_value = reserve_stack(context->allocator, builder, larger_descriptor);
    if (slice_equal(operator, slice_literal("+"))) {
      plus(context->allocator, builder, &lhs->source_range, result_value, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("-"))) {
      minus(context->allocator, builder, &lhs->source_range, result_value, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("*"))) {
      multiply(context->allocator, builder, &lhs->source_range, result_value, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("/"))) {
      divide(context->allocator, builder, &lhs->source_range, result_value, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("%"))) {
      value_remainder(context->allocator, builder, &lhs->source_range, result_value, lhs_value, rhs_value);
    } else {
      panic("Internal error: Unexpected operator");
    }
    result_token = token_value_make(context, result_value, lhs->source_range);
  } else if (
    slice_equal(operator, slice_literal(">")) ||
    slice_equal(operator, slice_literal("<")) ||
    slice_equal(operator, slice_literal(">=")) ||
    slice_equal(operator, slice_literal("<=")) ||
    slice_equal(operator, slice_literal("==")) ||
    slice_equal(operator, slice_literal("!="))
  ) {
    const Token *rhs = *dyn_array_pop(*token_stack);
    const Token *lhs = *dyn_array_pop(*token_stack);

    Value *lhs_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, lhs, lhs_value)) return;
    Value *rhs_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, rhs, rhs_value)) return;

    if (!descriptor_is_integer(lhs_value->descriptor)) {
      context_error_snprintf(
        context, lhs->source_range,
        "Left hand side of the  %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }
    if (!descriptor_is_integer(rhs_value->descriptor)) {
      context_error_snprintf(
        context, rhs->source_range,
        "Right hand side of the  %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }

    if (!(same_value_type_or_can_implicitly_move_cast(lhs_value, rhs_value))) {
      context_error_snprintf(
        context, lhs->source_range,
        "Incompatible integer types in comparison"
      );
      return;
    }

    Compare_Type compare_type = 0;

    if (slice_equal(operator, slice_literal(">"))) compare_type = Compare_Type_Signed_Greater;
    else if (slice_equal(operator, slice_literal("<"))) compare_type = Compare_Type_Signed_Less;
    else if (slice_equal(operator, slice_literal(">="))) compare_type = Compare_Type_Signed_Greater_Equal;
    else if (slice_equal(operator, slice_literal("<="))) compare_type = Compare_Type_Signed_Less_Equal;
    else if (slice_equal(operator, slice_literal("=="))) compare_type = Compare_Type_Equal;
    else if (slice_equal(operator, slice_literal("!="))) compare_type = Compare_Type_Not_Equal;

    if (descriptor_is_unsigned_integer(lhs_value->descriptor)) {
      switch(compare_type) {
        case Compare_Type_Equal:
        case Compare_Type_Not_Equal: {
          break;
        }

        case Compare_Type_Unsigned_Below:
        case Compare_Type_Unsigned_Below_Equal:
        case Compare_Type_Unsigned_Above:
        case Compare_Type_Unsigned_Above_Equal: {
          panic("Internal error. Expected to parse operators as signed compares");
          break;
        }

        case Compare_Type_Signed_Less: {
          compare_type = Compare_Type_Unsigned_Below;
          break;
        }
        case Compare_Type_Signed_Less_Equal: {
          compare_type = Compare_Type_Unsigned_Below_Equal;
          break;
        }
        case Compare_Type_Signed_Greater: {
          compare_type = Compare_Type_Unsigned_Above;
          break;
        }
        case Compare_Type_Signed_Greater_Equal: {
          compare_type = Compare_Type_Unsigned_Above_Equal;
          break;
        }
        default: {
          assert(!"Unsupported comparison");
        }
      }
    }

    Value *raw = value_any(context->allocator);
    compare(
      context->allocator, compare_type, context->builder,
      &lhs->source_range, raw, lhs_value, rhs_value
    );

    // FIXME figure out how to avoid this
    Value *result_value = reserve_stack(context->allocator, context->builder, raw->descriptor);
    move_value(context->allocator, context->builder, &lhs->source_range, result_value, raw);
    result_token = token_value_make(context, result_value, lhs->source_range);
  } else {
    panic("TODO: Unknown operator");
    result_token = 0;
  }

  dyn_array_push(*token_stack, result_token);
}

bool
token_parse_expression(
  Compilation_Context *context,
  Token_View view,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!view.length) {
    // FIXME provide a source range
    move_value(context->allocator, context->builder, &(Source_Range){0}, result_value, &void_value);
    return true;
  }

  Array_Const_Token_Ptr token_stack = dyn_array_make(Array_Const_Token_Ptr);
  Array_Operator_Stack_Entry operator_stack = dyn_array_make(Array_Operator_Stack_Entry);

  bool is_previous_an_operator = true;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    Operator_Fixity fixity_mask = is_previous_an_operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Infix | Operator_Fixity_Postfix;

    switch(token->tag) {
      case Token_Tag_None: {
        panic("Internal Error: Encountered token with an uninitialized tag");
        break;
      }
      case Token_Tag_String:
      case Token_Tag_Id:
      case Token_Tag_Value: {
        dyn_array_push(token_stack, token);
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Group: {
        dyn_array_push(token_stack, token);
        switch (token->Group.tag) {
          case Token_Group_Tag_Paren: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_operator,
                &token_stack, &operator_stack, slice_literal("()"), token->source_range,
                Operator_Fixity_Postfix
              )) goto err;
            }
            break;
          }
          case Token_Group_Tag_Curly: {
            // Nothing special to do for now?
            break;
          }
          case Token_Group_Tag_Square: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_operator,
                &token_stack, &operator_stack, slice_literal("[]"), token->source_range,
                Operator_Fixity_Postfix
              )) goto err;
            }
            break;
          }
        }
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Operator: {
        Slice operator = token->source;
        if (!token_handle_operator(
          context, view, token_dispatch_operator,
          &token_stack, &operator_stack, operator, token->source_range,
          fixity_mask
        )) goto err;
        is_previous_an_operator = true;
        break;
      }
    }
  }

  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_operator(context, view, &token_stack, entry);
  }
  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(token_stack) == 1) {
      const Token *token = *dyn_array_last(token_stack);
      assert(token);
      MASS_ON_ERROR(token_force_value(context, token, result_value)) goto err;
    } else {
      context_error_snprintf(
        context, source_range_from_token_view(view),
        "Could not parse the expression"
      );
    }
  }

  err:

  dyn_array_destroy(token_stack);
  dyn_array_destroy(operator_stack);

  return true;
}

bool
token_parse_statement(
  Compilation_Context *context,
  Token_View view,
  const Source_Range *source_range,
  Value *result_value
);

bool
token_parse_block(
  Compilation_Context *context,
  const Token *block,
  Value *block_result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  assert(block->tag == Token_Tag_Group);
  assert(block->Group.tag == Token_Group_Tag_Curly);
  Array_Const_Token_Ptr children = block->Group.children;
  if (!dyn_array_length(children)) return false;
  Token_View children_view = token_view_from_token_array(children);
  Token_View_Split_Iterator it = { .view = children_view };

  Compilation_Context body_context = *context;
  body_context.scope = scope_make(context->allocator, context->scope);

  while (!it.done) {
    Token_View view = token_split_next(&it, &token_pattern_semicolon);
    if (!view.length) continue;
    bool is_last_statement = it.done;
    Value *result_value = is_last_statement
      ? block_result_value
      : value_any(body_context.allocator);

    token_parse_statement(&body_context, view, &block->source_range, result_value);
  }
  return true;
}

bool
token_parse_statement_label(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("label"));
  Token_View rest = token_view_rest(view, peek_index);

  if (
    rest.length != 1 ||
    !token_match(token_view_get(rest, 0), &(Token_Pattern){ .tag = Token_Tag_Id })
  ) {
    context_error_snprintf(
      context, keyword->source_range,
      "`label` must be followed by an identifier"
    );
    goto err;
  }

  const Token *id = token_view_get(rest, 0);

  // :ForwardLabelRef
  // First try to lookup a label that might have been declared by `goto`
  // FIXME make sure we don't double declare label
  Scope *function_scope = context->builder->value->descriptor->Function.scope;
  Scope_Entry *scope_entry = scope_lookup(function_scope, id->source, 0);
  Value *value;
  if (scope_entry) {
    value = scope_entry_force(context, scope_entry);
  } else {
    Label_Index label = make_label(context->program, &context->program->code_section);
    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(label),
    };
    // FIXME this should define a label in the function scope, but because
    // the macros are not hygienic we can not do that yet
    //scope_define_value(function_scope, id->source, value);
    scope_define(context->scope, id->source, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = value,
    });
  }

  if (
    value->descriptor != &descriptor_void ||
    value->operand.tag != Operand_Tag_Memory ||
    value->operand.Memory.location.tag != Memory_Location_Tag_Instruction_Pointer_Relative
  ) {
    context_error_snprintf(
      context, keyword->source_range,
      "Trying to redefine variable %"PRIslice" as a label",
      SLICE_EXPAND_PRINTF(id->source)
    );
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = value->operand.Memory.location.Instruction_Pointer_Relative.label_index
    }
  );

  err:
  return true;
}

bool
token_parse_statement_if(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("if"));

  Token_View rest = token_view_rest(view, peek_index);

  if (!rest.length) {
    context_error_snprintf(
      context, keyword->source_range,
      "`if` keyword must be followed by an expression"
    );
    goto err;
  }

  const Token *body = token_view_last(rest);
  Token_View condition_view = {
    .tokens = rest.tokens,
    .length = rest.length - 1,
  };

  Value *condition_value = value_any(context->allocator);
  token_parse_expression(context, condition_view, condition_value);
  if (condition_value->descriptor->tag == Descriptor_Tag_Any) {
    goto err;
  }

  Label_Index else_label = make_if(
    context, &context->builder->code_block.instructions, &keyword->source_range, condition_value
  );
  token_parse_block(context, body, value_any(context->allocator));
  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = else_label}
  );

  err:
  return true;
}

bool
token_parse_goto(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("goto"));
  Token_View rest = token_view_rest(view, peek_index);
  if (rest.length == 0) {
    context_error_snprintf(
      context, keyword->source_range,
      "`goto` keyword must be followed by an identifier"
    );
    goto err;
  }

  if (rest.length > 1) {
    context_error_snprintf(
      context, token_view_get(rest, 1)->source_range,
      "Unexpected token"
    );
    goto err;
  }
  const Token *id = token_view_get(rest, 0);
  if (!token_match(id, &(Token_Pattern){.tag = Token_Tag_Id})) {
    context_error_snprintf(
      context, id->source_range,
      "`goto` keyword must be followed by an identifier"
    );
    goto err;
  }

  Scope_Entry *scope_entry = scope_lookup(context->scope, id->source, 0);
  Value *value;
  if (scope_entry) {
    value = scope_entry_force(context, scope_entry);
  } else {
    // :ForwardLabelRef
    // If we didn't find an identifier with this name, declare one and hope
    // that some label will resolve it
    // FIXME somehow report unresolved labels
    Label_Index label = make_label(context->program, &context->program->code_section);
    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(label),
    };
    // Label declarations are always done in the function scope as they
    // might need to jump out of a nested block.
    scope_define(context->builder->value->descriptor->Function.scope, id->source, (Scope_Entry) {
      .type = Scope_Entry_Type_Value,
      .flags = Scope_Entry_Flags_None,
      .value = value,
    });
  }

  if (
    value->descriptor != &descriptor_void ||
    value->operand.tag != Operand_Tag_Memory ||
    value->operand.Memory.location.tag != Memory_Location_Tag_Instruction_Pointer_Relative
  ) {
    context_error_snprintf(
      context, keyword->source_range,
      "%"PRIslice" is not a label",
      SLICE_EXPAND_PRINTF(id->source)
    );
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.assembly = {jmp, {value->operand, 0, 0}}}
  );

  err:
  return true;
}

bool
token_parse_explicit_return(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("return"));
  Token_View rest = token_view_rest(view, peek_index);
  bool has_return_expression = rest.length > 0;

  Scope_Entry *scope_value_entry =
    scope_lookup(context->scope, MASS_RETURN_VALUE_NAME, Scope_Entry_Flags_None);
  assert(scope_value_entry);
  Value *fn_return = scope_entry_force(context, scope_value_entry);
  assert(fn_return);

  bool is_any_return = fn_return->descriptor->tag == Descriptor_Tag_Any;
  token_parse_expression(context, rest, fn_return);

  // FIXME with inline functions and explicit returns we can end up with multiple immediate
  //       values that are trying to be moved in the same return value
  if (is_any_return) {
    Value *stack_return = reserve_stack(context->allocator, context->builder, fn_return->descriptor);
    move_value(context->allocator, context->builder, &keyword->source_range, stack_return, fn_return);
    *fn_return = *stack_return;
  }

  bool is_void = fn_return->descriptor->tag == Descriptor_Tag_Void;
  if (!is_void && !has_return_expression) {
    context_error_snprintf(
      context, keyword->source_range,
      "Explicit return from a non-void function requires a value"
    );
  }

  Scope_Entry *scope_label_entry =
    scope_lookup(context->scope, MASS_RETURN_LABEL_NAME, Scope_Entry_Flags_None);
  assert(scope_label_entry);
  Value *return_label = scope_entry_force(context, scope_label_entry);
  assert(return_label);
  assert(return_label->descriptor == &descriptor_void);
  assert(operand_is_label(&return_label->operand));

  push_instruction(
    &context->builder->code_block.instructions,
    keyword->source_range,
    (Instruction) {.assembly = {jmp, {return_label->operand, 0, 0}}}
  );

  return true;
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(type, .tag = Token_Tag_Id);
  Token_Match(square_brace, .group_tag = Token_Group_Tag_Square);
  Descriptor *descriptor =
    scope_lookup_type(context, context->scope, type->source_range, type->source);

  Token_View size_view = token_view_from_token_array(square_brace->Group.children);
  Value *size_value = token_parse_constant_expression(context, size_view);
  if (!size_value) return 0;
  if (!descriptor_is_integer(size_value->descriptor)) {
    context_error_snprintf(
      context, square_brace->source_range,
      "Fixed size array size is not an integer"
    );
    return 0;
  }
  if (size_value->operand.tag != Operand_Tag_Immediate) {
    context_error_snprintf(
      context, square_brace->source_range,
      "Fixed size array size must be known at compile time"
    );
    return 0;
  }
  u32 length = s64_to_u32(operand_immediate_value_up_to_s64(&size_value->operand));

  // TODO extract into a helper
  Descriptor *array_descriptor = allocator_allocate(context->allocator, Descriptor);
  *array_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Fixed_Size_Array,
    .Fixed_Size_Array = {
      .item = descriptor,
      .length = length,
    },
  };
  return array_descriptor;
}

bool
token_parse_inline_machine_code_bytes(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(id_token, .tag = Token_Tag_Id, .source = slice_literal("inline_machine_code_bytes"));
  // TODO improve error reporting and / or transition to compile time functions when available
  Token_Match(args_token, .group_tag = Token_Group_Tag_Paren);

  Array_Value_Ptr args = token_match_call_arguments(context, args_token);

  Instruction_Bytes bytes = {
    .label_offset_in_instruction = INSTRUCTION_BYTES_NO_LABEL,
  };

  for (u64 i = 0; i < dyn_array_length(args); ++i) {
    if (bytes.length >= 15) {
      context_error_snprintf(
        context, args_token->source_range,
        "Expected a maximum of 15 bytes"
      );
    }
    Value *value = *dyn_array_get(args, i);
    if (!value) continue;
    if (operand_is_label(&value->operand)) {
      if (bytes.label_offset_in_instruction != INSTRUCTION_BYTES_NO_LABEL) {
        context_error_snprintf(
          context, args_token->source_range,
          "inline_machine_code_bytes only supports one label"
        );
        goto err;
      }
      bytes.label_index = value->operand.Memory.location.Instruction_Pointer_Relative.label_index;
      bytes.label_offset_in_instruction = u64_to_u8(i);
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
    } else {
      if (!descriptor_is_integer(value->descriptor)) {
        context_error_snprintf(
          context, args_token->source_range,
          "inline_machine_code_bytes expects arguments to be integers"
        );
        goto err;
      }
      if (value->operand.tag != Operand_Tag_Immediate) {
        context_error_snprintf(
          context, args_token->source_range,
          "inline_machine_code_bytes expects arguments to be compile-time known"
        );
        goto err;
      }
      u64 byte = operand_immediate_value_up_to_u64(&value->operand);
      if (!u64_fits_into_u8(byte)) {
        context_error_snprintf(
          context, args_token->source_range,
          "Expected integer between 0 and 255, got %"PRIu64,
          byte
        );
        goto err;
      }
      bytes.memory[bytes.length++] = s64_to_u8(byte);
    }
  }

  push_instruction(
    &context->builder->code_block.instructions, id_token->source_range,
    (Instruction) {
      .type = Instruction_Type_Bytes,
      .Bytes = bytes,
     }
  );

  err:
  return true;
}

Value *
token_parse_definition(
  Compilation_Context *context,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_rest(view, peek_index);
  Descriptor *descriptor = token_match_type(context, rest);
  if (!descriptor) {
    context_error_snprintf(
      context, define->source_range,
      "Could not find type"
    );
    goto err;
  }
  Value *value = reserve_stack(context->allocator, context->builder, descriptor);
  scope_define(context->scope, name->source, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .flags = Scope_Entry_Flags_None,
    .value = value,
  });
  return value;

  err:
  return 0;
}

bool
token_parse_definitions(
  Compilation_Context *context,
  Token_View state,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  return !!token_parse_definition(context, state);
}

bool
token_parse_definition_and_assignment_statements(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;

  if (!token_maybe_split_on_operator(view, slice_literal(":="), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) return false;
  const Token *name = token_view_get(view, 0);

  if (name->tag != Token_Tag_Id) return false;

  Value *value = value_any(context->allocator);
  token_parse_expression(context, rhs, value);

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (descriptor_is_integer(value->descriptor) && value->operand.tag == Operand_Tag_Immediate) {
    value = value_from_s64(context->allocator, operand_immediate_value_up_to_s64(&value->operand));
  } else if (
    value->descriptor->tag == Descriptor_Tag_Opaque &&
    value->operand.tag == Operand_Tag_Immediate
  ) {
    panic("TODO decide how to handle opaque types");
  }
  Value *on_stack = reserve_stack(context->allocator, context->builder, value->descriptor);
  move_value(context->allocator, context->builder, &name->source_range, on_stack, value);

  scope_define(context->scope, name->source, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .flags = Scope_Entry_Flags_None,
    .value = on_stack,
  });
  return true;
}

bool
token_parse_assignment(
  Compilation_Context *context,
  Token_View view,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;
  if (!token_maybe_split_on_operator(view, slice_literal("="), &lhs, &rhs, &operator)) {
    return false;
  }

  Value *target = token_parse_definition(context, lhs);
  if (!target) {
    target = value_any(context->allocator);
    token_parse_expression(context, lhs, target);
  }
  token_parse_expression(context, rhs, target);
  return true;
}

bool
token_parse_statement(
  Compilation_Context *context,
  Token_View view,
  const Source_Range *source_range,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Array_Const_Token_Ptr statement_tokens = token_array_from_view(allocator_system, view);
  // TODO consider how this should work
  token_parse_macros(context, &statement_tokens, context->scope);
  view = token_view_from_token_array(statement_tokens);
  for (
    Scope *statement_matcher_scope = context->scope;
    statement_matcher_scope;
    statement_matcher_scope = statement_matcher_scope->parent
  ) {
    if (!dyn_array_is_initialized(statement_matcher_scope->statement_matchers)) {
      continue;
    }
    for (u64 i = 0 ; i < dyn_array_length(statement_matcher_scope->statement_matchers); ++i) {
      Token_Statement_Matcher matcher =
        *dyn_array_get(statement_matcher_scope->statement_matchers, i);
      if (matcher.proc(context, view, matcher.payload)) {
        return true;
      }
    }
  }

  bool result = token_parse_expression(context, view, result_value);
  dyn_array_destroy(statement_tokens);
  return result;
}

PRELUDE_NO_DISCARD Mass_Result
token_parse(
  Compilation_Context *context,
  Token_View view
) {
  if (!view.length) return *context->result;

  Token_View_Split_Iterator it = { .view = view };
  while (!it.done) {
    MASS_TRY(*context->result);
    Token_View statement = token_split_next(&it, &token_pattern_semicolon);
    if (!statement.length) continue;
    if (token_parse_syntax_definition(context, statement, context->program->global_scope)) {
      continue;
    }
    if (token_parse_operator_definition(context, statement, context->program->global_scope)) {
      continue;
    }
    if (token_parse_constant_definitions(context, statement, 0)) {
      continue;
    }

    // Report unmatched statements
    Source_Range source_range = source_range_from_token_view(statement);
    context_error_snprintf(
      context, source_range,
      "Could not parse a top level statement"
    );
    break;
  }

  return *context->result;
}

Mass_Result
program_parse(
  Compilation_Context *context,
  Source_File *file
) {
  Array_Const_Token_Ptr tokens;
  MASS_TRY(tokenize(context->allocator, file, &tokens));
  MASS_TRY(token_parse(context, token_view_from_token_array(tokens)));
  return *context->result;
}

Fixed_Buffer *
program_absolute_path(
  Slice raw_path
) {
  Slice result_path = raw_path;

  #ifdef _WIN32
  bool is_relative_path = raw_path.length < 2 || raw_path.bytes[1] != ':';

  Fixed_Buffer *sys_buffer = 0;
  if (is_relative_path) {
    sys_buffer = fixed_buffer_make(
      .allocator = allocator_system,
      .capacity = 10 * 1024
    );
    s32 current_dir_size = GetCurrentDirectory(0, 0) * sizeof(wchar_t);
    sys_buffer->occupied =
      GetCurrentDirectory(current_dir_size, (wchar_t *)sys_buffer->memory) * sizeof(wchar_t);
    fixed_buffer_append_s16(sys_buffer, L'\\');
    Allocator *convert_allocator = fixed_buffer_allocator_init(sys_buffer, &(Allocator){0});
    utf8_to_utf16_null_terminated(convert_allocator, raw_path);
    wchar_t *wide_string = (wchar_t *)sys_buffer->memory;
    result_path = utf16_null_terminated_to_utf8(convert_allocator, wide_string);
  }
  #else
  bool is_relative_path = !slice_starts_with(raw_path, slice_literal("/"));
  Fixed_Buffer *sys_buffer = 0;
  if (is_relative_path) {
    char cwd[1024];
    if (getcwd(cwd, sizeof(cwd)) != 0) {
      sys_buffer = fixed_buffer_make(
        .allocator = allocator_system,
        .capacity = 10 * 1024
      );
      fixed_buffer_append_slice(sys_buffer, slice_from_c_string(cwd));
      fixed_buffer_append_u8(sys_buffer, '/');
      fixed_buffer_append_slice(sys_buffer, raw_path);
      result_path = fixed_buffer_as_slice(sys_buffer);
    }
  }
  #endif
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = result_path.length + 1024
  );

  fixed_buffer_append_slice(result_buffer, result_path);

  if (sys_buffer) fixed_buffer_destroy(sys_buffer);
  return result_buffer;
}

Mass_Result
program_import_file(
  Compilation_Context *context,
  Slice file_path
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = program_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Source_File *file = allocator_allocate(context->allocator, Source_File);
  *file = (Source_File) {
    .path = file_path,
    .text = {0},
  };
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) {
    return (Mass_Result) {
      .tag = Mass_Result_Tag_Error,
      .Error.details = {
        .message = slice_literal("Unable to open the file"),
        .source_range = {
          .file = file,
        },
      },
    };
  }
  file->text = fixed_buffer_as_slice(buffer);
  return program_parse(context, file);
}

