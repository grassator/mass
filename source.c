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

static Token_View
token_view_slice(
  const Token_View *view,
  u64 start_index,
  u64 end_index
) {
  assert(end_index <= view->length);
  assert(start_index <= end_index);

  Source_Range source_range = view->source_range;
  source_range.offsets.to = end_index == view->length
    ? view->source_range.offsets.to
    : view->tokens[end_index]->source_range.offsets.from;
  source_range.offsets.from = start_index == end_index
    ? source_range.offsets.to
    : view->tokens[start_index]->source_range.offsets.from;

  return (Token_View) {
    .tokens = view->tokens + start_index,
    .length = end_index - start_index,
    .source_range = source_range,
  };
}

static inline Token_View
token_view_rest(
  const Token_View *view,
  u64 index
) {
  return token_view_slice(view, index, view->length);
}

static inline Token_View
token_view_from_token_array(
  Array_Const_Token_Ptr token_array,
  const Source_Range *source_range
) {
  return (Token_View) {
    .tokens = dyn_array_raw(token_array),
    .length = dyn_array_length(token_array),
    .source_range = *source_range
  };
}

static Token_View
token_view_from_group_token(
  const Token *token
) {
  assert(token->tag == Token_Tag_Group);
  Source_Range children_source_range;
  if (dyn_array_length(token->Group.children)) {
    const Token *first = *dyn_array_get(token->Group.children, 0);
    const Token *last = *dyn_array_last(token->Group.children);
    children_source_range = (Source_Range) {
      .file = token->source_range.file,
      .offsets = {
        .from = first->source_range.offsets.from,
        .to = last->source_range.offsets.to,
      },
    };
  } else {
    // TODO maybe we should this during tokenization for groups
    //      and correctly remove braces
    children_source_range = token->source_range;
  }

  return token_view_from_token_array(token->Group.children, &token->source_range);
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

Scope *
scope_make(
  const Allocator *allocator,
  Scope *parent
) {
  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .allocator = allocator,
    .parent = parent,
    .map = 0,
  };
  return scope;
}

void
scope_print_names(
  Scope *scope
) {
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    for (u64 i = 0; i < scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &scope->map->entries[i];
      if (entry->occupied) {
        slice_print(entry->key);
        printf(" ; ");
      }
    }
  }
  printf("\n");
}

Scope_Entry *
scope_lookup(
  Scope *scope,
  Slice name
) {
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    Scope_Entry *entry = *entry_pointer;
    if (entry) {
      return entry;
    }
  }
  return 0;
}

PRELUDE_NO_DISCARD Mass_Result
assign(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *target,
  Value *source
) {
  MASS_TRY(*context->result);

  if (target->operand.tag == Operand_Tag_Eflags) {
    panic("Internal Error: Trying to move into Eflags");
  }
  if (target->descriptor->tag == Descriptor_Tag_Void) {
    return *context->result;
  }
  if (target->descriptor->tag == Descriptor_Tag_Any) {
    assert(target->operand.tag == Operand_Tag_Any);
    target->descriptor = source->descriptor;
    target->operand = source->operand;
    target->next_overload = source->next_overload;
    return *context->result;
  }

  assert(source->descriptor->tag != Descriptor_Tag_Function);
  if (same_value_type_or_can_implicitly_move_cast(target, source)) {
    move_value(context->allocator, context->builder, source_range, &target->operand, &source->operand);
    return *context->result;
  }
  // TODO elaborate the error
  context_error_snprintf(
    context, *source_range, "Incompatible type"
  );
  return *context->result;
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

Value *
scope_entry_force(
  Compilation_Context *context,
  Scope_Entry *entry
) {
  switch(entry->tag) {
    case Scope_Entry_Tag_Operator: {
      panic("Internal Error: Operators are not allowed in this context");
      return 0;
    }
    case Scope_Entry_Tag_Lazy_Expression: {
      Scope_Entry_Lazy_Expression *expr = &entry->Lazy_Expression;
      Compilation_Context lazy_context = *context;
      lazy_context.scope = expr->scope;
      Value *result = token_parse_constant_expression(&lazy_context, expr->tokens);
      *entry = (Scope_Entry) {
        .tag = Scope_Entry_Tag_Value,
        .Value.value = result,
        .next_overload = entry->next_overload,
      };
      return result;
    }
    case Scope_Entry_Tag_Value: {
      return entry->Value.value;
    }
  }
  panic("Internal Error: Unexpected scope entry type");
  return 0;
}

Value *
scope_lookup_force(
  Compilation_Context *context,
  Scope *scope,
  Slice name
) {
  Scope_Entry *entry = 0;
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    if (*entry_pointer) {
      entry = *entry_pointer;
      break;
    }
  }
  if (!entry) {
    return 0;
  }

  // Force lazy entries
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    if (it->tag == Scope_Entry_Tag_Lazy_Expression) {
      scope_entry_force(context, it);
    }
  }

  Value *result = 0;
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    assert(it->tag == Scope_Entry_Tag_Value);

    if (!result) {
      result = it->Value.value;
    } else {
      if (it->Value.value->descriptor->tag != Descriptor_Tag_Function) {
        panic("Only functions support overloading");
      }
      Value *overload = it->Value.value;
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
      if (!parent->map) continue;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(context, parent, name);
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
  if (!scope->map) {
    scope->map = Scope_Map__make(scope->allocator);
  }
  Scope_Entry *allocated = allocator_allocate(scope->allocator, Scope_Entry);
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

typedef struct {
  Token *token;
  Array_Const_Token_Ptr children;
} Tokenizer_Parent;
typedef dyn_array_type(Tokenizer_Parent) Array_Tokenizer_Parent;

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  const Allocator *allocator,
  Source_File *file,
  Array_Const_Token_Ptr *out_tokens
) {

  Array_Tokenizer_Parent parent_stack = dyn_array_make(Array_Tokenizer_Parent);
  // FIXME get rid of this
  Token *root = &(Token){0};

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
  Tokenizer_Parent parent = {root, dyn_array_make(Array_Const_Token_Ptr)};
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
    current_token->source = source_from_source_range(&current_token->source_range);\
    dyn_array_push(parent.children, current_token);\
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
    if (parent.token->tag == Token_Tag_None || parent.token->Group.tag == Token_Group_Tag_Curly) {\
      /* Do not treating leading newlines as semicolons */ \
      if (dyn_array_length(parent.children)) {\
        start_token(Token_Tag_Operator);\
        current_token->source_range.offsets = (Range_u64){ i + 1, i + 1 };\
        current_token->source = slice_literal(";");\
        dyn_array_push(parent.children, current_token);\
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
          dyn_array_push(parent.children, current_token);
          dyn_array_push(parent_stack, parent);
          parent = (Tokenizer_Parent){current_token, dyn_array_make(Array_Const_Token_Ptr, 4)};
        } else if (ch == ')' || ch == '}' || ch == ']') {
          if (parent.token->tag != Token_Tag_Group) {
            panic("Tokenizer: unexpected closing char for group");
          }
          s8 expected_paren = 0;
          switch (parent.token->Group.tag) {
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
              while (dyn_array_length(parent.children)) {
                const Token *last_token = *dyn_array_last(parent.children);
                bool is_last_token_a_fake_semicolon = (
                  token_match(last_token, &token_pattern_semicolon) &&
                  range_length(last_token->source_range.offsets) == 0
                );
                if (!is_last_token_a_fake_semicolon) break;
                dyn_array_pop(parent.children);
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
          parent.token->Group.children = parent.children;
          parent.token->source_range.offsets.to = i + 1;
          parent.token->source = source_from_source_range(&parent.token->source_range);
          if (!dyn_array_length(parent_stack)) {
            TOKENIZER_HANDLE_ERROR("Encountered a closing brace without a matching open one");
          }
          parent = *dyn_array_pop(parent_stack);
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

  if (parent.token != root) {
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
    *out_tokens = parent.children;
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
      Token_View result = token_view_slice(&it->view, start_index, it->index);
      // Skip over the separator
      it->index++;
      return result;
    }
  }
  it->done = true;
  return token_view_rest(&it->view, start_index);
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
  Scope_Entry *scope_entry = scope_lookup(scope, type_name);
  if (!scope_entry) {
    context_error_snprintf(
      context, source_range, "Could not find type %"PRIslice,
      SLICE_EXPAND_PRINTF(type_name)
    );
    return 0;
  }
  Value *value = scope_entry_force(context, scope_entry);
  return value_ensure_type(context, value, source_range, type_name);
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

static inline Token_View
token_view_match_till_end_of_statement(
  Token_View view,
  u64 *peek_index
) {
  u64 start_index = *peek_index;
  for (; *peek_index < view.length; *peek_index += 1) {
    const Token *token = token_view_get(view, *peek_index);
    if (token->tag == Token_Tag_Operator && slice_equal(token->source, slice_literal(";"))) {
      *peek_index += 1;
      return token_view_slice(&view, start_index, *peek_index - 1);
    }
  }
  return token_view_slice(&view, start_index, *peek_index);
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

typedef enum {
  Macro_Match_Mode_Expression,
  Macro_Match_Mode_Statement
} Macro_Match_Mode;

u64
token_match_pattern(
  Token_View view,
  Macro *macro,
  Array_Token_View *out_match,
  Macro_Match_Mode mode
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
        dyn_array_push(*out_match, token_view_slice(&view, view_index, view_index + 1));
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
          if (
            !peek &&
            mode == Macro_Match_Mode_Statement &&
            token_match(token, &token_pattern_semicolon)) {
            break;
          }
          if (peek && token_match(token, &peek->Single_Token.token_pattern)) {
            break;
          }
        }
        dyn_array_push(*out_match, token_view_slice(&view, any_token_start_view_index, view_index));
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

  return view_index;
}

void
token_apply_macro_syntax(
  Compilation_Context *context,
  Array_Token_View match,
  Macro *macro,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  assert(macro->scope);

  // All captured token sequences need to have access to the same base scope
  // to support implementing disjointed syntax, such as for (;;) loop
  // or switch / pattern matching.
  // Ideally there should be a way to control this explicitly somehow.
  Scope *captured_scope = scope_make(context->allocator, context->scope);

  Scope *expansion_scope;
  if (macro->transparent) {
    expansion_scope = captured_scope;
  } else {
    expansion_scope = scope_make(context->allocator, macro->scope);
  }

  for (u64 i = 0; i < dyn_array_length(macro->pattern); ++i) {
    Macro_Pattern *item = dyn_array_get(macro->pattern, i);
    Slice capture_name = {0};

    switch(item->tag) {
      case Macro_Pattern_Tag_Single_Token: {
        capture_name = item->Single_Token.capture_name;
        break;
      }
      case Macro_Pattern_Tag_Any_Token_Sequence: {
        capture_name = item->Any_Token_Sequence.capture_name;
        break;
      }
    }

    if (!capture_name.length) continue;

    Token_View capture_view = *dyn_array_get(match, i);

    Token *fake_body = allocator_allocate(context->allocator, Token);
    *fake_body = (Token) {
      .tag = Token_Tag_Group,
      .source_range = capture_view.source_range,
      .source = source_from_source_range(&capture_view.source_range),
      .Group = {
        .tag = Token_Group_Tag_Curly,
        .children = token_array_from_view(context->allocator, capture_view),
      }
    };

    Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
    *descriptor = (Descriptor) {
      .tag = Descriptor_Tag_Function,
      .Function = {
        .arguments = (Array_Function_Argument){&dyn_array_zero_items},
        .scope = captured_scope,
        .body = fake_body,
        .flags
          = Descriptor_Function_Flags_Macro
          | Descriptor_Function_Flags_No_Own_Scope
          | Descriptor_Function_Flags_No_Own_Return,
        .returns = {
          .name = {0},
          .descriptor = macro->replacement.length ? &descriptor_any : &descriptor_void,
        }
      },
    };

    Value *result = allocator_allocate(context->allocator, Value);
    *result = (Value) {
      .descriptor = descriptor,
      .operand = {.tag = Operand_Tag_None },
      .compiler_source_location = COMPILER_SOURCE_LOCATION_FIELDS,
    };

    scope_define(expansion_scope, capture_name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = result,
    });
  }

  Compilation_Context body_context = *context;
  body_context.scope = expansion_scope;

  token_parse_expression(&body_context, macro->replacement, result_value, Expression_Parse_Mode_Default);
}

u64
token_parse_macro_statement(
  Compilation_Context *context,
  Token_View token_view,
  Value *result_value,
  void *payload
) {
  assert(payload);
  if (!token_view.length) return 0;
  Macro *macro = payload;
  // TODO @Speed would be nice to not need this copy
  Array_Token_View match = dyn_array_make(Array_Token_View);
  u64 match_length = token_match_pattern(token_view, macro, &match, Macro_Match_Mode_Statement);
  if (!match_length) return 0;
  Token_View rest = token_view_rest(&token_view, match_length);
  if (rest.length) {
    if (token_match(token_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length += 1;
    } else {
      return 0;
    }
  }
  token_apply_macro_syntax(context, match, macro, result_value);
  dyn_array_destroy(match);
  return match_length;
}


const Token *
token_parse_macros(
  Compilation_Context *context,
  Token_View token_view,
  u64 *match_length
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Array_Token_View match = dyn_array_make(Array_Token_View);
  Token *replacement = 0;
  Scope *scope = context->scope;
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);

      *match_length = token_match_pattern(token_view, macro, &match, Macro_Match_Mode_Expression);
      if (!*match_length) continue;

      Value *macro_result = value_any(context->allocator);
      token_apply_macro_syntax(context, match, macro, macro_result);
      Token_View matched_view = token_view_rest(&token_view, *match_length);
      replacement = allocator_allocate(context->allocator, Token);
      *replacement = (Token){
        .tag = Token_Tag_Value,
        .source_range = matched_view.source_range,
        .source = source_from_source_range(&matched_view.source_range),
        .Value = { macro_result },
      };
      goto defer;
    }
  }
  defer:
  dyn_array_destroy(match);
  return replacement;
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
  MASS_ON_ERROR(*context->result) return 0;
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
  bool found = false;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    if (token->tag == Token_Tag_Operator && slice_equal(token->source, operator)) {
      *operator_token = token;
      lhs_end = i;
      rhs_start = i + 1;
      found = true;
      break;
    }
  }
  if (!found) return false;

  *lhs = token_view_slice(&view, 0, lhs_end);
  *rhs = token_view_rest(&view, rhs_start);

  return true;
}

Function_Argument
token_match_argument(
  Compilation_Context *context,
  Token_View view,
  Descriptor_Function *function
) {
  Function_Argument arg = {0};
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

    arg = (Function_Argument) {
      .tag = Function_Argument_Tag_Any_Of_Type,
      .Any_Of_Type = {
        .descriptor = token_match_type(context, rhs),
        .name = lhs.tokens[0]->source,
      },
    };
  } else {
    Value *value = value_any(context->allocator);
    compile_time_eval(context, view, value);
    arg = (Function_Argument) {
      .tag = Function_Argument_Tag_Exact,
      .Exact = {
        .descriptor = value->descriptor,
        .operand = value->operand,
      },
    };
  }

  err:
  return arg;
}

Function_Return
token_match_return_type(
  Compilation_Context *context,
  Token_View view
) {
  Function_Return returns = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return returns;

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
    returns.descriptor = token_match_type(context, rhs);
    returns.name = lhs.tokens[0]->source;
  } else {
    returns.descriptor = token_match_type(context, view);
  }

  err:
  return returns;
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
      Value *value = value_global_c_string_from_slice(context, string);
      return assign(context, &token->source_range, result_value, value);
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name);
      MASS_TRY(*context->result);
      if (!value) {
        context_error_snprintf(
          context, token->source_range,
          "Undefined variable %"PRIslice,
          SLICE_EXPAND_PRINTF(name)
        );

        return *context->result;
      } else {
        return assign(context, &token->source_range, result_value, value);
      }
    }
    case Token_Tag_Value: {
      if (token->Value.value) {
        return assign(context, &token->source_range, result_value, token->Value.value);
      } else {
        // TODO consider what should happen here
      }
      return *context->result;
    }
    case Token_Tag_Group: {
      switch(token->Group.tag) {
        case Token_Group_Tag_Paren: {
          Token_View expression_tokens = token_view_from_group_token(token);
          token_parse_expression(context, expression_tokens, result_value, Expression_Parse_Mode_Default);
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
    Token_View children = token_view_from_group_token(token);
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
      token_parse_expression(context, view, result_value, Expression_Parse_Mode_Default);
      dyn_array_push(result, result_value);
    }
  }
  return result;
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

void
token_handle_user_defined_operator(
  Compilation_Context *context,
  Token_View args,
  Value *result_value,
  User_Defined_Operator *operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  // FIXME This is almost identical with the macro function call

  // We make a nested scope based on the original scope
  // instead of current scope for hygiene reasons.
  Scope *body_scope = scope_make(context->allocator, operator->scope);
  assert(operator->argument_count == args.length);

  for (u8 i = 0; i < operator->argument_count; ++i) {
    Slice arg_name = operator->argument_names[i];
    Value *arg_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, token_view_get(args, i), arg_value)) return;
    scope_define(body_scope, arg_name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = arg_value,
    });
  }

  // Define a new return target label and value so that explicit return statements
  // jump to correct location and put value in the right place
  Program *program = context->program;
  Label_Index fake_return_label_index = make_label(program, &program->data_section, MASS_RETURN_LABEL_NAME);
  {
    Value *return_label_value = allocator_allocate(context->allocator, Value);
    *return_label_value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(fake_return_label_index),
    };
    scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = return_label_value,
    });
    scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = result_value,
    });
  }

  const Token *body = operator->body;
  {
    Compilation_Context body_context = *context;
    body_context.scope = body_scope;
    token_parse_block(&body_context, body, result_value);
  }

  push_instruction(
    &context->builder->code_block.instructions,
    args.source_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = fake_return_label_index
    }
  );
}

void
token_handle_user_defined_operator_proc(
  Compilation_Context *context,
  Token_View args,
  Value *result_value,
  void *payload
) {
  token_handle_user_defined_operator(context, args, result_value, payload);
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
  Token_View view
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
  MASS_ON_ERROR(token_force_value(context, precedence_token, precedence_value)) goto err;

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

  Token_View definition = token_view_from_group_token(pattern_token);

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
    scope_lookup(context->scope, operator_token->source);
  while (existing_scope_entry) {
    if (existing_scope_entry->tag != Scope_Entry_Tag_Operator) {
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
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = precendence,
      .argument_count = operator->argument_count,
      .fixity = operator->fixity,
      .handler = token_handle_user_defined_operator_proc,
      .handler_payload = operator,
    }
  });

  return true;

  err:
  if (operator) allocator_deallocate(context->allocator, operator, sizeof(*operator));
  return true;
}

bool
token_parse_import_statement(
  Compilation_Context *context,
  Token_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // import "foo" as foo
  u64 peek_index = 0;
  Token_Match(import_keywors, .tag = Token_Tag_Id, .source = slice_literal("import"));
  Token_Maybe_Match(file_path, .tag = Token_Tag_String);
  Token_Maybe_Match(as_keyword, .tag = Token_Tag_Id, .source = slice_literal("as"));
  Token_Maybe_Match(name, .tag = Token_Tag_Id);

  if (!file_path) {
    context_error_snprintf(
      context, name->source_range,
      "import keyword must be followed by a path to the imported file"
    );
    goto err;
  }
  if (!as_keyword) {
    context_error_snprintf(
      context, name->source_range,
      "import path must be followed by `as` keyword"
    );
    goto err;
  }
  if (!name) {
    context_error_snprintf(
      context, name->source_range,
      "`as` keyword must be followed by a name you can use to access imported scope"
    );
    goto err;
  }

  // TODO Probably want to cache the root scope somewhere
  Scope *root_scope = context->scope;
  while (root_scope->parent) root_scope = root_scope->parent;

  Scope *module_scope = scope_make(context->allocator, root_scope);
  Module *module = program_module_from_file(
    context, file_path->String.slice, module_scope
  );
  program_import_module(context, module);

  Value *module_value = value_make(
    context->allocator,
    &descriptor_scope,
    (Operand){
      .tag = Operand_Tag_Immediate,
      .byte_size = sizeof(Scope *),
      .Immediate.memory = module_scope,
    }
  );

  // TODO consider making imports lazy
  scope_define(context->scope, name->source, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = module_value,
  });

  err:

  return true;
}

u64
token_parse_syntax_definition(
  Compilation_Context *context,
  Token_View view,
  Value *result_value,
  void *payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id, .source = slice_literal("syntax"));
  Token_Maybe_Match(statement, .tag = Token_Tag_Id, .source = slice_literal("statement"));
  Token_Maybe_Match(transparent, .tag = Token_Tag_Id, .source = slice_literal("transparent"));

  Token_Maybe_Match(pattern_token, .group_tag = Token_Group_Tag_Paren);

  if (!pattern_token) {
    context_error_snprintf(
      context, name->source_range,
      "Syntax definition requires a parenthesized pattern definitions"
    );
    goto err;
  }

  Token_View replacement = token_view_match_till_end_of_statement(view, &peek_index);
  Token_View definition = token_view_from_group_token(pattern_token);

  Array_Macro_Pattern pattern = dyn_array_make(Array_Macro_Pattern);

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
    .scope = context->scope,
    .transparent = !!transparent
  };
  if (statement) {
    if (!dyn_array_is_initialized(context->scope->statement_matchers)) {
      context->scope->statement_matchers =
        dyn_array_make(Array_Token_Statement_Matcher, .allocator = context->allocator);
    }
    dyn_array_push(context->scope->statement_matchers, (Token_Statement_Matcher){
      .proc = token_parse_macro_statement,
      .payload = macro,
    });
  } else {
    scope_add_macro(context->scope, macro);
  }
  MASS_ON_ERROR(assign(context, &view.source_range, result_value, &void_value));
  return peek_index;

  err:
  dyn_array_destroy(pattern);
  return peek_index;
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

  Token_View rest = token_view_rest(&view, peek_index);
  Descriptor *descriptor = token_match_type(context, rest);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, name->source);
  return true;
}

Token *
token_process_bit_type_definition(
  Compilation_Context *context,
  const Token *args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View args_view = { .tokens = &args, .length = 1, .source_range = args->source_range };
  Value *bit_size_value = value_any(context->allocator);
  compile_time_eval(context, args_view, bit_size_value);
  if (!bit_size_value) {
    context_error_snprintf(context, args->source_range, "Could not parse bit type size");
    goto err;
  }

  if (!descriptor_is_integer(bit_size_value->descriptor)) {
    context_error_snprintf(context, args->source_range, "Bit type size must be an integer");
    goto err;
  }
  if (bit_size_value->operand.tag != Operand_Tag_Immediate) {
    context_error_snprintf(context, args->source_range, "Bit type size must be a constant");
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
    Token_View layout_block_children = token_view_from_group_token(layout_block);

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
  return token_value_make(context, result, view.source_range);
}

Value *
token_process_function_literal(
  Compilation_Context *context,
  Scope *scope,
  const Token *args,
  const Token *return_types,
  const Token *body
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Scope *parent_scope = context->scope;
  Scope *function_scope = scope_make(context->allocator, parent_scope);
  function_scope->flags |= Scope_Flags_Labels;

  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Function,
    .Function = {
      .arguments = (Array_Function_Argument){&dyn_array_zero_items},
      .returns = 0,
    },
  };

  if (dyn_array_length(return_types->Group.children) == 0) {
    descriptor->Function.returns = (Function_Return) { .descriptor = &descriptor_void, };
  } else {
    Token_View children = token_view_from_group_token(return_types);
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
      arg_context.scope = function_scope;
      arg_context.builder = 0;
      descriptor->Function.returns = token_match_return_type(&arg_context, arg_view);
    }
  }

  if (dyn_array_length(args->Group.children) != 0) {
    descriptor->Function.arguments = dyn_array_make(
      Array_Function_Argument,
      .allocator = context->allocator,
      .capacity = 4,
    );

    Token_View children = token_view_from_group_token(args);
    Token_View_Split_Iterator it = { .view = children };
    while (!it.done) {
      Token_View arg_view = token_split_next(&it, &token_pattern_comma_operator);
      Compilation_Context arg_context = *context;
      arg_context.scope = function_scope;
      arg_context.builder = 0;
      Function_Argument arg = token_match_argument(&arg_context, arg_view, &descriptor->Function);
      dyn_array_push(descriptor->Function.arguments, arg);
      MASS_ON_ERROR(*context->result) return 0;
    }
  }

  // TODO think about a better way to distinguish imports
  bool is_external = body->tag == Token_Tag_Value;

  Value *result = allocator_allocate(context->allocator, Value);
  *result = (Value) {
    .descriptor = descriptor,
    .compiler_source_location = COMPILER_SOURCE_LOCATION_FIELDS,
  };
  if (is_external) {
    result->descriptor = descriptor_pointer_to(context->allocator, descriptor);
    result->operand = body->Value.value->operand;
  } else {
    descriptor->Function.scope = function_scope;
    descriptor->Function.body = body;
  }

  return result;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

void
compile_time_eval(
  Compilation_Context *context,
  Token_View view,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  const Source_Range *source_range = &view.source_range;

  Jit *jit = context->compile_time_jit;
  Compilation_Context eval_context = *context;
  eval_context.program = jit->program;
  eval_context.scope = scope_make(context->allocator, context->scope);
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor){
    .tag = Descriptor_Tag_Function,
    .Function = {
      .returns = {
        .descriptor = &descriptor_void,
      },
    },
  };
  Label_Index eval_label_index = make_label(jit->program, &jit->program->code_section, slice_literal("compile_time_eval"));
  Value *eval_value = value_make(context->allocator, descriptor, code_label32(eval_label_index));
  Function_Builder eval_builder = {
    .function = &descriptor->Function,
    .label_index = eval_label_index,
    .code_block = {
      .end_label = make_label(jit->program, &jit->program->code_section, slice_literal("compile_time_eval_end")),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };
  eval_context.builder = &eval_builder;
  eval_context.builder->source = slice_sub_range(source_range->file->text, source_range->offsets);

  // FIXME We have to call token_parse_expression here before we figure out
  //       what is the return value because we need to figure out the return type.
  //       Ideally there would be a type-only eval available instead
  Value *expression_result_value = value_any(context->allocator);
  token_parse_expression(&eval_context, view, expression_result_value, Expression_Parse_Mode_Default);
  MASS_ON_ERROR(*eval_context.result) {
    context->result = eval_context.result;
    return;
  }

  // If we didn't generate any instructions there is no point
  // actually running the code, we can just take the resulting value
  if (!dyn_array_length(eval_builder.code_block.instructions)) {
    if (expression_result_value->descriptor->tag == Descriptor_Tag_Function) {
      // It is only allowed to to pass through funciton definitions not compiled ones
      assert(expression_result_value->operand.tag == Operand_Tag_None);
    }
    MASS_ON_ERROR(assign(context, source_range, result_value, expression_result_value));
    return;
  }

  u32 result_byte_size = expression_result_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(context->allocator, result_byte_size, alignment);

  // Load the address of the result
  Register out_register = register_acquire_temp(&eval_builder);
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

  MASS_ON_ERROR(assign(&eval_context, source_range, &out_value_register, &result_address)) {
    context->result = eval_context.result;
    return;
  }

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

  MASS_ON_ERROR(assign(&eval_context, source_range, out_value, expression_result_value)) {
    context->result = eval_context.result;
    return;
  }
  fn_end(jit->program, &eval_builder);
  dyn_array_push(jit->program->functions, eval_builder);

  program_jit(jit);

  fn_type_opaque jitted_code = value_as_function(jit, eval_value);
  jitted_code();

  Value *temp_result = allocator_allocate(context->allocator, Value);
  *temp_result = (Value) {
    .descriptor = out_value->descriptor,
  };
  switch(out_value->descriptor->tag) {
    case Descriptor_Tag_Void: {
      temp_result->operand = (Operand){0};
      break;
    }
    case Descriptor_Tag_Any: {
      panic("Internal Error: We should never get Any type from comp time eval");
      break;
    }
    case Descriptor_Tag_Pointer: {
      panic("TODO move to data section or maybe we should allocate from there right away above?");
      break;
    };
    case Descriptor_Tag_Struct:
    case Descriptor_Tag_Fixed_Size_Array:
    case Descriptor_Tag_Opaque: {
      temp_result->operand = (Operand){
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
  MASS_ON_ERROR(assign(context, source_range, result_value, temp_result));
}

typedef struct {
  Slice source;
  Source_Range source_range;
  Scope_Entry_Operator scope_entry;
} Operator_Stack_Entry;
typedef dyn_array_type(Operator_Stack_Entry) Array_Operator_Stack_Entry;

typedef void (*Operator_Dispatch_Proc)(
  Compilation_Context *,
  Array_Const_Token_Ptr *token_stack,
  Operator_Stack_Entry *operator
);

void
token_handle_operand_variant_of(
  Compilation_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  if (dyn_array_length(args) != 1) {
    context_error_snprintf(
      context, *source_range,
      "operand_variant_of expects a single argument"
    );
    return;
  }

  Value *value = *dyn_array_get(args, 0);

  Value *operand_value;
  switch(value->operand.tag) {
    default:
    case Operand_Tag_None:
    case Operand_Tag_Any:
    case Operand_Tag_Immediate:
    case Operand_Tag_Eflags:
    case Operand_Tag_Xmm:
    case Operand_Tag_Memory: {
      panic("TODO implement operand reflection for more types");
      operand_value = 0;
      break;
    }
    case Operand_Tag_Register: {
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
      operand_value = value_make(
        context->allocator,
        result_descriptor,
        imm8(context->allocator, value->operand.Register.index)
      );
    }
  }
  MASS_ON_ERROR(assign(context, source_range, result_value, operand_value));
}

void
token_handle_cast(
  Compilation_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  Descriptor *cast_to_descriptor =
    value_ensure_type(context, type, *source_range, slice_literal("TODO cast source"));

  assert(descriptor_is_integer(cast_to_descriptor));
  assert(value->descriptor->tag == cast_to_descriptor->tag);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *after_cast_value = value;
  if (cast_to_byte_size != original_byte_size) {
    after_cast_value = allocator_allocate(context->allocator, Value);

    if (value->operand.tag == Operand_Tag_Immediate) {
      if (descriptor_is_signed_integer(cast_to_descriptor)) {
        s64 integer = operand_immediate_value_up_to_s64(&value->operand);
        switch(cast_to_byte_size) {
          case 1: {
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm8(context->allocator, (s8)integer),
            };
            break;
          }
          case 2: {
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm16(context->allocator, (s16)integer),
            };
            break;
          }
          case 4: {
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm32(context->allocator, (s32)integer),
            };
            break;
          }
          case 8: {
            *after_cast_value = (Value) {
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
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm8(context->allocator, (u8)integer),
            };
            break;
          }
          case 2: {
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm16(context->allocator, (u16)integer),
            };
            break;
          }
          case 4: {
            *after_cast_value = (Value) {
              .descriptor = cast_to_descriptor,
              .operand = imm32(context->allocator, (u32)integer),
            };
            break;
          }
          case 8: {
            *after_cast_value = (Value) {
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
      *after_cast_value = (Value) {
        .descriptor = cast_to_descriptor,
        .operand = value->operand,
      };
      after_cast_value->operand.byte_size = cast_to_byte_size;
    } else if (cast_to_byte_size > original_byte_size) {
      panic("Not implemented cast to a larger type");
    }
  }
  MASS_ON_ERROR(assign(context, source_range, result_value, after_cast_value));
}

void
token_handle_negation(
  Compilation_Context *context,
  Token_View args,
  Value *result_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;
  assert(args.length == 1);
  const Token *token = token_view_get(args, 0);

  // FIXME use result_value here
  Value *value = value_any(context->allocator);
  MASS_ON_ERROR(token_force_value(context, token, value)) return;
  Value *negated_value = 0;
  if (descriptor_is_integer(value->descriptor) && value->operand.tag == Operand_Tag_Immediate) {\
    // FIXME this is broken for originally unsigned values larger than their signed counterpart
    switch(value->operand.byte_size) {
      case 1: {
        negated_value = value_from_s8(context->allocator, -operand_immediate_memory_as_s8(&value->operand));
        break;
      }
      case 2: {
        negated_value = value_from_s16(context->allocator, -operand_immediate_memory_as_s16(&value->operand));
        break;
      }
      case 4: {
        negated_value = value_from_s32(context->allocator, -operand_immediate_memory_as_s32(&value->operand));
        break;
      }
      case 8: {
        negated_value = value_from_s64(context->allocator, -operand_immediate_memory_as_s64(&value->operand));
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
  MASS_ON_ERROR(assign(context, &token->source_range, result_value, negated_value));
}

void
token_dispatch_constant_operator(
  Compilation_Context *context,
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
      .source_range = token->source_range,
    };
    Value *negation_result = value_any(context->allocator);
    token_handle_negation(context, args_view, negation_result, 0);
    const Token *new_token = token_value_make(context, negation_result, token->source_range);
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
      Token_View args_children = token_view_from_group_token(args);
      result = token_import_match_arguments(args->source_range, args_children, context);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("bit_type"))
    ) {
      result = token_process_bit_type_definition(context, args);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("c_struct"))
    ) {
      result = token_process_c_struct_definition(context, args);
    } else {
      // TODO somehow generalize this for all operators
      const Token *call_tokens[] = {function, args};
      Token_View call_view = {
        .tokens = call_tokens,
        .length = 2,
        .source_range = {
          .file = function->source_range.file,
          .offsets = {
            .from = function->source_range.offsets.from,
            .to = args->source_range.offsets.to,
          },
        },
      };
      Value *comp_time_result = value_any(context->allocator);
      compile_time_eval(context, call_view, comp_time_result);
      result = token_value_make(context, comp_time_result, call_view.source_range);
    }
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("@"))) {
    const Token *keyword = *dyn_array_pop(*token_stack);
    if (!token_match(keyword, &(Token_Pattern){ .source = slice_literal("scope") })) {
      panic("TODO");
    }
    Value *scope_value = value_make(
      context->allocator,
      &descriptor_scope,
      (Operand){
        .tag = Operand_Tag_Immediate,
        .byte_size = sizeof(Scope *),
        .Immediate.memory = context->scope,
      }
    );
    const Token *result = token_value_make(context, scope_value, keyword->source_range);
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("->"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    const Token *return_types = *dyn_array_pop(*token_stack);
    const Token *arguments = *dyn_array_pop(*token_stack);
    Value *function_value = token_process_function_literal(
      context, context->scope, arguments, return_types, body
    );
    Token *result = token_value_make(context, function_value, arguments->source_range);
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("macro"))) {
    const Token *function = *dyn_array_last(*token_stack);
    Value *function_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, function, function_value)) return;
    if (function_value) {
      if (function_value->descriptor->tag == Descriptor_Tag_Function) {
        Descriptor_Function *descriptor = &function_value->descriptor->Function;
        descriptor->flags |= Descriptor_Function_Flags_Macro;
      } else {
        context_error_snprintf(
          context, function->source_range,
          "Only literal functions (with a body) can be marked as macro"
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

  Scope_Entry *scope_entry = scope_lookup(context->scope, new_operator);

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
    if (scope_entry->tag != Scope_Entry_Tag_Operator) {
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
    dispatch_proc(context, token_stack, last_operator);
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
        Scope_Entry *scope_entry = scope_lookup(context->scope, token->source);
        if (scope_entry && scope_entry->tag == Scope_Entry_Tag_Operator) {
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
    token_dispatch_constant_operator(context, &token_stack, entry);
  }
  if (dyn_array_length(token_stack) == 1) {
    const Token *token = *dyn_array_last(token_stack);
    result = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, token, result)) goto err;
  } else {
    // FIXME user error
  }

  err:

  dyn_array_destroy(token_stack);
  dyn_array_destroy(operator_stack);

  return result;
}

u64
token_parse_constant_definitions(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;

  u64 statement_length = 0;
  view = token_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal("::"), &lhs, &rhs, &operator)) {
    return 0;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) {
    panic("TODO user error");
    goto err;
  }
  const Token *name = token_view_get(lhs, 0);
  if (name->tag != Token_Tag_Id) {
    panic("TODO user error");
    goto err;
  }

  scope_define(context->scope, name->source, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Lazy_Expression,
    .Lazy_Expression = {
      .tokens = rhs,
      .scope = context->scope,
    },
  });

  err:
  return statement_length;
}

void
token_handle_function_call(
  Compilation_Context *context,
  const Token *target_token,
  const Token *args_token,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *target = value_any(context->allocator);
  MASS_ON_ERROR(token_force_value(context, target_token, target)) return;
  assert(token_match(args_token, &(Token_Pattern){.group_tag = Token_Group_Tag_Paren}));

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

  Descriptor *target_descriptor = maybe_unwrap_pointer_descriptor(target->descriptor);
  if (target_descriptor->tag != Descriptor_Tag_Function) {
    context_error_snprintf(
      context, target_token->source_range,
      "%"PRIslice" is not a function",
      SLICE_EXPAND_PRINTF(target_token->source)
    );
    return;
  }
  const Source_Range *source_range = &target_token->source_range;

  struct Overload_Match { Value *value; s64 score; } match = { .score = -1 };
  for (Value *to_call = target; to_call; to_call = to_call->next_overload) {
    Descriptor *to_call_descriptor = maybe_unwrap_pointer_descriptor(to_call->descriptor);
    assert(to_call_descriptor->tag == Descriptor_Tag_Function);
    Descriptor_Function *descriptor = &to_call_descriptor->Function;
    if (dyn_array_length(args) != dyn_array_length(descriptor->arguments)) continue;
    s64 score = calculate_arguments_match_score(descriptor, args);
    if (score == match.score) {
      // TODO improve error message
      // TODO provide names of matched overloads
      context_error_snprintf(
        context, target_token->source_range,
        "Could not decide which overload to pick"
      );
      return;
    } else if (score > match.score) {
      match.value = to_call;
      match.score = score;
    } else {
      // Skip a worse match
    }
  }

  Value *overload = match.value;
  if (overload) {
    Descriptor_Function *function = &overload->descriptor->Function;

    if (function->flags & Descriptor_Function_Flags_Macro) {
      assert(function->scope->parent);
      // We make a nested scope based on function's original parent scope
      // instead of current scope for hygiene reasons. I.e. function body
      // should not have access to locals inside the call scope.
      Scope *body_scope = (function->flags & Descriptor_Function_Flags_No_Own_Scope)
        ? function->scope
        : scope_make(context->allocator, function->scope);

      for (u64 i = 0; i < dyn_array_length(function->arguments); ++i) {
        Function_Argument *arg = dyn_array_get(function->arguments, i);
        switch(arg->tag) {
          case Function_Argument_Tag_Exact: {
            // There is no name so nothing to do
            break;
          }
          case Function_Argument_Tag_Any_Of_Type: {
            Value *arg_value = *dyn_array_get(args, i);
            scope_define(body_scope, arg->Any_Of_Type.name, (Scope_Entry) {
              .tag = Scope_Entry_Tag_Value,
              .Value.value = arg_value,
            });
            break;
          }
        }
      }

      // Define a new return target label and value so that explicit return statements
      // jump to correct location and put value in the right place
      Program *program = context->program;
      Label_Index fake_return_label_index = make_label(program, &program->data_section, MASS_RETURN_LABEL_NAME);

      Value return_label = {
        .descriptor = &descriptor_void,
        .operand = code_label32(fake_return_label_index),
        .compiler_source_location = COMPILER_SOURCE_LOCATION_FIELDS,
      };
      if (!(function->flags & Descriptor_Function_Flags_No_Own_Return)) {
        scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
          .tag = Scope_Entry_Tag_Value,
          .Value.value = &return_label,
        });
        scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
          .tag = Scope_Entry_Tag_Value,
          .Value.value = result_value,
        });
      }

      const Token *body = function->body;
      {
        Compilation_Context body_context = *context;
        body_context.scope = body_scope;
        token_parse_block_no_scope(&body_context, body, result_value);
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
      call_function_overload(context, source_range, overload, args, result_value);
    }


  } else {
    // TODO add better error message
    context_error_snprintf(
      context, target_token->source_range,
      "Could not find matching overload"
    );
  }
  dyn_array_destroy(args);
}

static inline Value *
extend_integer_value(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *value,
  Descriptor *target_descriptor
) {
  assert(descriptor_is_integer(value->descriptor));
  assert(descriptor_is_integer(target_descriptor));
  assert(descriptor_byte_size(target_descriptor) > descriptor_byte_size(value->descriptor));
  Value *result = reserve_stack(context->allocator, context->builder, target_descriptor);
  move_value(context->allocator, context->builder, source_range, &result->operand, &value->operand);
  return result;
}

static inline Value *
extend_signed_integer_value_to_next_size(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *value
) {
  assert(descriptor_is_signed_integer(value->descriptor));
  assert(value->descriptor->tag == Descriptor_Tag_Opaque);
  Descriptor *one_size_larger;
  switch(value->descriptor->Opaque.bit_size) {
    case 8: {
      one_size_larger = &descriptor_s16;
      break;
    }
    case 16: {
      one_size_larger = &descriptor_s32;
      break;
    }
    case 32: {
      one_size_larger = &descriptor_s64;
      break;
    }
    default: {
      context_error_snprintf(
        context, *source_range,
        "Could not find large enough signed integer type to fit both operands"
      );
      return 0;
    }
  }
  return extend_integer_value(context, source_range, value, one_size_larger);
}

Value *
maybe_coerce_immediate_value_to_signed(
  Compilation_Context *context,
  Value *value,
  Descriptor *target_descriptor
) {
  if (!descriptor_is_signed_integer(target_descriptor)) return value;
  if (!descriptor_is_unsigned_integer(value->descriptor)) return value;
  if (value->operand.tag != Operand_Tag_Immediate) return value;
  u64 integer = operand_immediate_value_up_to_u64(&value->operand);
  if (target_descriptor == &descriptor_s8) {
    if (u64_fits_into_s8(integer)) {
      return value_from_s8(context->allocator, u64_to_s8(integer));
    }
    return value;
  } else if (target_descriptor == &descriptor_s16) {
    if (u64_fits_into_s16(integer)) {
      return value_from_s16(context->allocator, u64_to_s16(integer));
    }
    return value;
  } else if (target_descriptor == &descriptor_s32) {
    if (u64_fits_into_s32(integer)) {
      return value_from_s32(context->allocator, u64_to_s32(integer));
    }
    return value;
  } else if (target_descriptor == &descriptor_s64) {
    if (u64_fits_into_s64(integer)) {
      return value_from_s64(context->allocator, u64_to_s64(integer));
    }
    return value;
  }
  return value;
}

void
maybe_resize_values_for_integer_math_operation(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value **lhs_pointer,
  Value **rhs_pointer
) {
  *lhs_pointer = maybe_coerce_immediate_value_to_signed(
    context, *lhs_pointer, (*rhs_pointer)->descriptor
  );
  *rhs_pointer = maybe_coerce_immediate_value_to_signed(
    context, *rhs_pointer, (*lhs_pointer)->descriptor
  );

  Descriptor *ld = (*lhs_pointer)->descriptor;
  Descriptor *rd = (*rhs_pointer)->descriptor;

  bool ld_signed = descriptor_is_signed_integer(ld);
  bool rd_signed = descriptor_is_signed_integer(rd);

  u32 ld_size = descriptor_byte_size(ld);
  u32 rd_size = descriptor_byte_size(rd);

  if (ld_signed == rd_signed) {
    if (ld_size == rd_size) return;
    Descriptor *larger_descriptor = ld_size > rd_size ? ld : rd;
    if (ld == larger_descriptor) {
      *rhs_pointer = extend_integer_value(context, source_range, *rhs_pointer, larger_descriptor);
    } else {
      *lhs_pointer = extend_integer_value(context, source_range, *lhs_pointer, larger_descriptor);
    }
    return;
  } else {
    // If the signed and unsigned have the same size need to
    // increase the size of the signed one so it fits the unsigned
    if (ld_size == rd_size) {
      if (ld_size == 8) {
        context_error_snprintf(
          context, *source_range,
          "Could not find large enough signed integer type to fit both operands"
        );
        return;
      }
      if (ld_signed) {
        *lhs_pointer =
          extend_signed_integer_value_to_next_size(context, source_range, *lhs_pointer);
        MASS_ON_ERROR(*context->result) return;
      } else {
        assert(rd_signed);
        *rhs_pointer =
          extend_signed_integer_value_to_next_size(context, source_range, *rhs_pointer);
        MASS_ON_ERROR(*context->result) return;
      }
    }

    // Now we know that the signed operand is larger so we move
    if (ld_signed) {
      *rhs_pointer =
        extend_integer_value(context, source_range, *rhs_pointer, (*lhs_pointer)->descriptor);
    } else {
      assert(rd_signed);
      *lhs_pointer =
        extend_integer_value(context, source_range, *lhs_pointer, (*rhs_pointer)->descriptor);
    }
  }
}

void
struct_get_field(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *raw_value,
  Slice name,
  Value *result_value
) {
  Value *struct_value = ensure_memory(context->allocator, raw_value);
  Descriptor *descriptor = struct_value->descriptor;
  assert(descriptor->tag == Descriptor_Tag_Struct);
  for (u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
    if (slice_equal(name, field->name)) {
      Value *field_value = allocator_allocate(context->allocator, Value);
      Operand operand = struct_value->operand;
      // FIXME support more operands
      assert(operand.tag == Operand_Tag_Memory);
      assert(operand.Memory.location.tag == Memory_Location_Tag_Indirect);
      operand.byte_size = descriptor_byte_size(field->descriptor);
      operand.Memory.location.Indirect.offset += field->offset;
      *field_value = (const Value) {
        .descriptor = field->descriptor,
        .operand = operand,
      };

      MASS_ON_ERROR(assign(context, source_range, result_value, field_value));
      return;
    }
  }

  assert(!"Could not find a field with specified name");
  return;
}


void
token_eval_operator(
  Compilation_Context *context,
  Token_View args_view,
  Operator_Stack_Entry *operator_entry,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;

  if (operator_entry->scope_entry.handler) {
    operator_entry->scope_entry.handler(
      context, args_view, result_value, operator_entry->scope_entry.handler_payload
    );
  } else if (slice_equal(operator, slice_literal("[]"))) {
    const Token *target_token = token_view_get(args_view, 0);
    const Token *brackets = token_view_get(args_view, 1);

    Value *array = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, target_token, array)) return;
    Value *index_value = value_any(context->allocator);
    Token_View index_tokens = token_view_from_group_token(brackets);
    token_parse_expression(context, index_tokens, index_value, Expression_Parse_Mode_Default);
    assert(array->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
    assert(array->operand.tag == Operand_Tag_Memory);
    assert(array->operand.Memory.location.tag == Memory_Location_Tag_Indirect);
    assert(!array->operand.Memory.location.Indirect.maybe_index_register.has_value);

    Descriptor *item_descriptor = array->descriptor->Fixed_Size_Array.item;
    u32 item_byte_size = descriptor_byte_size(item_descriptor);

    Value *array_element_value = allocator_allocate(context->allocator, Value);
    *array_element_value = (Value) {
      .descriptor = item_descriptor,
      .operand = array->operand
    };
    array_element_value->operand.byte_size = item_byte_size;
    if (index_value->operand.tag == Operand_Tag_Immediate) {
      s32 index = s64_to_s32(operand_immediate_value_up_to_s64(&index_value->operand));
      array_element_value->operand.Memory.location.Indirect.offset = index * item_byte_size;
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
        &new_base_register->operand,
        &index_value->operand
      );

      // Multiply index by the item byte size
      multiply(
        context,
        &target_token->source_range,
        new_base_register,
        new_base_register,
        value_from_s64(context->allocator, item_byte_size)
      );

      {
        // @InstructionQuality
        // TODO If the source does not have index, on X64 it should be possible to avoid
        //      using an extra register and put the index into SIB

        // Load previous address into a temp register
        Register temp_register = register_acquire_temp(context->builder);
        Value temp_value = {
          .descriptor = &descriptor_s64,
          .operand = operand_register_for_descriptor(temp_register, &descriptor_s64)
        };

        load_address(context, &target_token->source_range, &temp_value, array);

        plus(
          context,
          &target_token->source_range,
          new_base_register,
          new_base_register,
          &temp_value
        );
        register_release(context->builder, temp_register);
      }

      array_element_value->operand = (Operand) {
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
    // FIXME this might actually cause problems in assigning to an array element
    MASS_ON_ERROR(assign(context, &brackets->source_range, result_value, array_element_value)) return;
  } else if (slice_equal(operator, slice_literal("()"))) {
    const Token *target = token_view_get(args_view, 0);
    const Token *args_token = token_view_get(args_view, 1);
    // TODO turn `cast` into a compile-time function call / macro
    if (
      target->tag == Token_Tag_Id &&
      slice_equal(target->source, slice_literal("cast"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      token_handle_cast(context, &args_token->source_range, args, result_value);
      dyn_array_destroy(args);
    } else if (
      target->tag == Token_Tag_Id &&
      slice_equal(target->source, slice_literal("operand_variant_of"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      token_handle_operand_variant_of(context, &args_token->source_range, args, result_value);
      dyn_array_destroy(args);
    } else {
      token_handle_function_call(context, target, args_token, result_value);
    }
  } else if (slice_equal(operator, slice_literal("&"))) {
    const Token *pointee_token = token_view_get(args_view, 0);

    Value *pointee = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, pointee_token, pointee)) return;

    load_address(context, &pointee_token->source_range, result_value, pointee);
  } else if (slice_equal(operator, slice_literal("^"))) {
    const Token *body = token_view_get(args_view, 0);
    if (body->tag == Token_Tag_Group && body->Group.tag == Token_Group_Tag_Curly) {
      token_parse_block_no_scope(context, body, result_value);
    } else {
      context_error_snprintf(
        context, body->source_range,
        "^ operator must be followed by {}"
      );
      return;
    }
  } else if (slice_equal(operator, slice_literal("@"))) {
    const Token *body = token_view_get(args_view, 0);
    if (body->tag == Token_Tag_Group && body->Group.tag == Token_Group_Tag_Paren) {
      Token_View eval_view = token_view_from_group_token(body);
      compile_time_eval(context, eval_view, result_value);
    } else {
      context_error_snprintf(
        context, body->source_range,
        "@ operator must be followed by a parenthesized expression"
      );
      return;
    }
  } else if (slice_equal(operator, slice_literal("."))) {
    const Token *lhs = token_view_get(args_view, 0);
    const Token *rhs = token_view_get(args_view, 1);

    if (rhs->tag == Token_Tag_Id) {
      Value *struct_value = value_any(context->allocator);
      MASS_ON_ERROR(token_force_value(context, lhs, struct_value)) return;
      if (struct_value->descriptor->tag == Descriptor_Tag_Struct) {
        struct_get_field(context, &rhs->source_range, struct_value, rhs->source, result_value);
      } else if (struct_value->descriptor == &descriptor_scope) {
        assert(struct_value->operand.tag == Operand_Tag_Immediate);
        Scope *module_scope = struct_value->operand.Immediate.memory;
        Compilation_Context module_context = *context;
        module_context.scope = module_scope;
        module_context.builder = 0;
        MASS_ON_ERROR(token_force_value(&module_context, rhs, result_value)) return;
      } else {
        context_error_snprintf(
          context, rhs->source_range,
          "Left hand side of the . operator must be a struct"
        );
        return;
      }
    } else {
      context_error_snprintf(
        context, rhs->source_range,
        "Right hand side of the . operator must be an identifier"
      );
      return;
    }
  } else if (
    slice_equal(operator, slice_literal("+")) ||
    slice_equal(operator, slice_literal("-")) ||
    slice_equal(operator, slice_literal("*")) ||
    slice_equal(operator, slice_literal("/")) ||
    slice_equal(operator, slice_literal("%"))
  ) {
    const Token *lhs = token_view_get(args_view, 0);
    const Token *rhs = token_view_get(args_view, 1);

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

    maybe_resize_values_for_integer_math_operation(context, &lhs->source_range, &lhs_value, &rhs_value);
    MASS_ON_ERROR(*context->result) return;

    Function_Builder *builder = context->builder;

    Value *stack_result = reserve_stack(context->allocator, builder, lhs_value->descriptor);
    if (slice_equal(operator, slice_literal("+"))) {
      plus(context, &lhs->source_range, stack_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("-"))) {
      minus(context, &lhs->source_range, stack_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("*"))) {
      multiply(context, &lhs->source_range, stack_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("/"))) {
      divide(context, &lhs->source_range, stack_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("%"))) {
      value_remainder(context, &lhs->source_range, stack_result, lhs_value, rhs_value);
    } else {
      panic("Internal error: Unexpected operator");
    }
    MASS_ON_ERROR(assign(context, &args_view.source_range, result_value, stack_result)) return;
  } else if (
    slice_equal(operator, slice_literal(">")) ||
    slice_equal(operator, slice_literal("<")) ||
    slice_equal(operator, slice_literal(">=")) ||
    slice_equal(operator, slice_literal("<=")) ||
    slice_equal(operator, slice_literal("==")) ||
    slice_equal(operator, slice_literal("!="))
  ) {
    const Token *lhs = token_view_get(args_view, 0);
    const Token *rhs = token_view_get(args_view, 1);

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
    maybe_resize_values_for_integer_math_operation(context, &lhs->source_range, &lhs_value, &rhs_value);
    MASS_ON_ERROR(*context->result) return;

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

    compare(context, compare_type, &lhs->source_range, result_value, lhs_value, rhs_value);
  } else if (slice_equal(operator, slice_literal("->"))) {
    const Token *arguments = token_view_get(args_view, 0);
    const Token *return_types = token_view_get(args_view, 1);
    const Token *body = token_view_get(args_view, 2);
    Value *function_value = token_process_function_literal(
      context, context->scope, arguments, return_types, body
    );
    MASS_ON_ERROR(assign(context, &args_view.source_range, result_value, function_value)) return;
  } else if (slice_equal(operator, slice_literal("macro"))) {
    const Token *function = token_view_get(args_view, 0);
    Value *function_value = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, function, function_value)) return;
    if (function_value) {
      if (function_value->descriptor->tag == Descriptor_Tag_Function) {
        Descriptor_Function *descriptor = &function_value->descriptor->Function;
        descriptor->flags |= Descriptor_Function_Flags_Macro;
      } else {
        context_error_snprintf(
          context, function->source_range,
          "Only literal functions (with a body) can be marked as macro"
        );
      }
    }
    MASS_ON_ERROR(assign(context, &args_view.source_range, result_value, function_value)) return;
  } else {
    panic("TODO: Unknown operator");
  }
}



void
token_dispatch_operator(
  Compilation_Context *context,
  Array_Const_Token_Ptr *token_stack,
  Operator_Stack_Entry *operator_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;

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
  assert(argument_count);
  u64 start_index = dyn_array_length(*token_stack) - argument_count;
  const Token *first_arg = *dyn_array_get(*token_stack, start_index);
  const Token *last_arg = *dyn_array_last(*token_stack);
  Token_View args_view = {
    .tokens = dyn_array_get(*token_stack, start_index),
    .length = argument_count,
    .source_range = {
      .file = last_arg->source_range.file,
      .offsets = {
        .from = first_arg->source_range.offsets.from,
        .to = last_arg->source_range.offsets.to,
      },
    },
  };
  Value *result_value = value_any(context->allocator);
  token_eval_operator(context, args_view, operator_entry, result_value);
  MASS_ON_ERROR(*context->result) return;

  Token *result_token = token_value_make(context, result_value, args_view.source_range);

  // Pop off current arguments and push a new one
  dyn_array_splice_raw(*token_stack, start_index, argument_count, &result_token, 1);
}

const Token *
token_parse_if_expression(
  Compilation_Context *context,
  Token_View view,
  u64 *matched_length
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("if"));

  Token_View condition = {0};
  Token_View then_branch = {0};
  Token_View else_branch = {0};

  enum {
    Parse_State_Condition,
    Parse_State_Then,
    Parse_State_Else
  } parse_state = Parse_State_Condition;

  for (u64 i = peek_index; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    if (token_match(token, &(Token_Pattern){ .source = slice_literal("then") })) {
      Token_View till_here = token_view_slice(&view, peek_index, i);
      if (parse_state != Parse_State_Condition) {
        context_error_snprintf(
          context, till_here.source_range,
          "Expected `else`, encountered `then` inside an `if` expression"
        );
        goto err;
      }
      parse_state = Parse_State_Then;
      condition = till_here;
      peek_index = i + 1;
    } else if (token_match(token, &(Token_Pattern){ .source = slice_literal("else") })) {
      Token_View till_here = token_view_slice(&view, peek_index, i);
      if (parse_state != Parse_State_Then) {
        context_error_snprintf(
          context, till_here.source_range,
          "Expected `then`, encountered `else` inside an `if` expression"
        );
        goto err;
      }
      then_branch = till_here;
      peek_index = i + 1;
      else_branch = token_view_rest(&view, peek_index);
      parse_state = Parse_State_Else;
      break;
    } else if (token_match(token, &token_pattern_semicolon)) {
      break;
    }
  }
  if (!condition.length) {
    context_error_snprintf(
      context, view.source_range,
      "`if` keyword must be followed by an expression"
    );
    goto err;
  }
  if (!then_branch.length) {
    context_error_snprintf(
      context, view.source_range,
      "`then` branch of an if expression must not be empty"
    );
    goto err;
  }
  if (!else_branch.length) {
    context_error_snprintf(
      context, view.source_range,
      "`else` branch of an if expression must not be empty"
    );
    goto err;
  }

  Value *condition_value = value_any(context->allocator);
  token_parse_expression(context, condition, condition_value, Expression_Parse_Mode_Default);

  Label_Index else_label = make_if(
    context, &context->builder->code_block.instructions, &keyword->source_range, condition_value
  );

  Value *if_value = value_any(context->allocator);
  token_parse_expression(context, then_branch, if_value, Expression_Parse_Mode_Default);

  if (if_value->operand.tag == Operand_Tag_Immediate) {
    Value *on_stack = reserve_stack(context->allocator, context->builder, if_value->descriptor);
    MASS_ON_ERROR(assign(context, &view.source_range, on_stack, if_value)) {
      goto err;
    }
    if_value = on_stack;
  }

  Label_Index after_label =
    make_label(context->program, &context->program->code_section, slice_literal("if end"));
  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.assembly = {jmp, {code_label32(after_label), 0, 0}}}
  );

  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = else_label}
  );

  u64 else_length =
    token_parse_expression(context, else_branch, if_value, Expression_Parse_Mode_Default);
  *matched_length = peek_index + else_length;

  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = after_label}
  );
  return token_value_make(context, if_value, view.source_range);

  err:
  return 0;
}

PRELUDE_NO_DISCARD u64
token_parse_expression(
  Compilation_Context *context,
  Token_View view,
  Value *result_value,
  Expression_Parse_Mode mode
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if(!view.length) {
    MASS_ON_ERROR(assign(context, &view.source_range, result_value, &void_value)) return true;
    return true;
  }

  Array_Const_Token_Ptr token_stack = dyn_array_make(Array_Const_Token_Ptr);
  Array_Operator_Stack_Entry operator_stack = dyn_array_make(Array_Operator_Stack_Entry);

  bool is_previous_an_operator = true;
  u64 matched_length = view.length;
  for (u64 i = 0; i < view.length; ++i) {
    Token_View rest = token_view_rest(&view, i);
    {
      // Try to match macros at the current position
      u64 macro_match_length = 0;
      const Token *macro_result = token_parse_macros(context, rest, &macro_match_length);
      MASS_ON_ERROR(*context->result) goto err;
      if (macro_match_length) {
        assert(macro_result);
        dyn_array_push(token_stack, macro_result);
        // Skip over the matched slice
        i += macro_match_length - 1;
        continue;
      }
    }

    {
      u64 if_match_length = 0;
      const Token *if_expression = token_parse_if_expression(context, rest, &if_match_length);
      MASS_ON_ERROR(*context->result) goto err;
      if (if_match_length) {
        assert(if_expression);
        dyn_array_push(token_stack, if_expression);
        // Skip over the matched slice
        i += if_match_length - 1;
        continue;
      }
    }

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
        if (slice_equal(operator, slice_literal(";"))) {
          matched_length = i + 1;
          if (mode == Expression_Parse_Mode_Statement) {
            goto drain;
          } else {
            context_error_snprintf(
              context, token->source_range,
              "Unexpected semicolon in an expression"
            );
            goto err;
          }
        }
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

  drain:
  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_operator(context, &token_stack, entry);
  }
  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(token_stack) == 1) {
      const Token *token = *dyn_array_last(token_stack);
      assert(token);
      MASS_ON_ERROR(token_force_value(context, token, result_value)) goto err;
    } else {
      context_error_snprintf(
        context, view.source_range,
        "Could not parse the expression"
      );
    }
  }

  err:

  dyn_array_destroy(token_stack);
  dyn_array_destroy(operator_stack);

  return matched_length;
}

void
token_parse_block_no_scope(
  Compilation_Context *context,
  const Token *block,
  Value *block_result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  assert(block->tag == Token_Tag_Group);
  assert(block->Group.tag == Token_Group_Tag_Curly);
  Array_Const_Token_Ptr children = block->Group.children;
  if (!dyn_array_length(children)) {
    MASS_ON_ERROR(assign(context, &block->source_range, block_result_value, &void_value));
    return;
  }
  Token_View children_view = token_view_from_group_token(block);

  u64 match_length = 0;
  Value last_result;
  // FIXME deal with terminated vs unterminated statements
  for(u64 start_index = 0; start_index < children_view.length; start_index += match_length) {
    MASS_ON_ERROR(*context->result) return;
    last_result = (Value) {
      .descriptor = &descriptor_any,
      .operand = {.tag = Operand_Tag_Any},
    };
    Token_View rest = token_view_rest(&children_view, start_index);
    // Skipping over empty statements
    if (token_match(token_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length = 1;
      continue;
    }
    for (
      Scope *statement_matcher_scope = context->scope;
      statement_matcher_scope;
      statement_matcher_scope = statement_matcher_scope->parent
    ) {
      if (!dyn_array_is_initialized(statement_matcher_scope->statement_matchers)) {
        continue;
      }
      Array_Token_Statement_Matcher *matchers = &statement_matcher_scope->statement_matchers;
      // Do a reverse iteration because we want statements that are defined later
      // to have higher precedence when parsing
      for (u64 i = dyn_array_length(*matchers) ; i > 0; --i) {
        Token_Statement_Matcher *matcher = dyn_array_get(*matchers, i - 1);
        match_length = matcher->proc(context, rest, &last_result, matcher->payload);
        if (match_length) {
          if (last_result.descriptor->tag == Descriptor_Tag_Any) {
            MASS_ON_ERROR(assign(context, &block->source_range, &last_result, &void_value));
          }
          goto check_match;
        }
      }
    }
    match_length = token_parse_expression(context, rest, &last_result, Expression_Parse_Mode_Statement);

    check_match:
    if (!match_length) {
      const Token *token = token_view_get(rest, 0);
      context_error_snprintf(
        context, token->source_range,
        "Can not parse statement. Unexpected token %"PRIslice".",
        SLICE_EXPAND_PRINTF(token->source)
      );
      return;
    }
  }

  // TODO This is not optimal as we might generate extra temp values
  MASS_ON_ERROR(assign(context, &children_view.source_range, block_result_value, &last_result));
}

void
token_parse_block(
  Compilation_Context *context,
  const Token *block,
  Value *block_result_value
) {
  Compilation_Context body_context = *context;
  Scope *block_scope = scope_make(context->allocator, context->scope);
  body_context.scope = block_scope;
  token_parse_block_no_scope(&body_context, block, block_result_value);
}

u64
token_parse_statement_label(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("label"));
  Token_View rest = token_view_match_till_end_of_statement(view, &peek_index);

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

  if (slice_equal(id->source, slice_literal("scope"))) {
    context->scope->flags |= Scope_Flags_Labels;
    return peek_index;
  }

  // :ForwardLabelRef
  // First try to lookup a label that might have been declared by `goto`
  Scope_Entry *scope_entry = scope_lookup(context->scope, id->source);
  Value *value;
  if (scope_entry) {
    value = scope_entry_force(context, scope_entry);
  } else {
    Scope *label_scope = context->scope;
    while (label_scope) {
      if (label_scope->flags & Scope_Flags_Labels) break;
      label_scope = label_scope->parent;
    }
    if (!label_scope) {
      context_error_snprintf(
        context, id->source_range,
        "Trying to add `label` outside of a label scope"
      );
      goto err;
    }
    Program *program = context->program;
    Label_Index label = make_label(program, &program->code_section, id->source);

    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(label),
    };
    scope_define(label_scope, id->source, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = value,
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
  return peek_index;
}

u64
token_parse_goto(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("goto"));
  Token_View rest = token_view_match_till_end_of_statement(view, &peek_index);

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

  Scope_Entry *scope_entry = scope_lookup(context->scope, id->source);
  Value *value;
  if (scope_entry) {
    value = scope_entry_force(context, scope_entry);
  } else {
    Scope *label_scope = context->scope;
    while (label_scope) {
      if (label_scope->flags & Scope_Flags_Labels) break;
      label_scope = label_scope->parent;
    }
    if (!label_scope) {\
      context_error_snprintf(
        context, id->source_range,
        "Trying to execute `goto` outside of a label scope"
      );
      goto err;
    }
    // :ForwardLabelRef
    // If we didn't find an identifier with this name, declare one and hope
    // that some label will resolve it
    Program *program = context->program;
    Label_Index label = make_label(program, &program->code_section, id->source);
    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = code_label32(label),
    };
    // Label declarations are always done in the function scope as they
    // might need to jump out of a nested block.
    scope_define(label_scope, id->source, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = value,
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
  return peek_index;
}

u64
token_parse_explicit_return(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("return"));
  Token_View rest = token_view_match_till_end_of_statement(view, &peek_index);
  bool has_return_expression = rest.length > 0;

  Scope_Entry *scope_value_entry = scope_lookup(context->scope, MASS_RETURN_VALUE_NAME);
  assert(scope_value_entry);
  Value *fn_return = scope_entry_force(context, scope_value_entry);
  assert(fn_return);

  bool is_any_return = fn_return->descriptor->tag == Descriptor_Tag_Any;
  token_parse_expression(context, rest, fn_return, Expression_Parse_Mode_Default);

  // FIXME with inline functions and explicit returns we can end up with multiple immediate
  //       values that are trying to be moved in the same return value
  if (is_any_return) {
    Value *stack_return = reserve_stack(context->allocator, context->builder, fn_return->descriptor);
    MASS_ON_ERROR(assign(context, &keyword->source_range, stack_return, fn_return)) return true;
    *fn_return = *stack_return;
  }

  bool is_void = fn_return->descriptor->tag == Descriptor_Tag_Void;
  if (!is_void && !has_return_expression) {
    context_error_snprintf(
      context, keyword->source_range,
      "Explicit return from a non-void function requires a value"
    );
  }

  Scope_Entry *scope_label_entry = scope_lookup(context->scope, MASS_RETURN_LABEL_NAME);
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

  return peek_index;
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

  Token_View size_view = token_view_from_group_token(square_brace);
  Value *size_value = value_any(context->allocator);
  compile_time_eval(context, size_view, size_value);
  if (!descriptor_is_unsigned_integer(size_value->descriptor)) {
    context_error_snprintf(
      context, size_view.source_range,
      "Fixed size array size must be an unsigned integer"
    );
    return 0;
  }
  MASS_ON_ERROR(*context->result) return 0;
  if (size_value->operand.tag != Operand_Tag_Immediate) {
    context_error_snprintf(
      context, size_view.source_range,
      "Fixed size array size must be known at compile time"
    );
    return 0;
  }
  u32 length = u64_to_u32(operand_immediate_value_up_to_u64(&size_value->operand));

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

u64
token_parse_inline_machine_code_bytes(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(id_token, .tag = Token_Tag_Id, .source = slice_literal("inline_machine_code_bytes"));
  // TODO improve error reporting and / or transition to compile time functions when available
  Token_Match(args_token, .group_tag = Token_Group_Tag_Paren);
  Token_View rest = token_view_match_till_end_of_statement(view, &peek_index);
  if (rest.length) {
    context_error_snprintf(
      context, rest.source_range,
      "Expected the end of the statement"
    );
    goto err;
  }

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
      goto err;
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
  return peek_index;
}

u64
token_parse_definition(
  Compilation_Context *context,
  Token_View view,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_match_till_end_of_statement(view, &peek_index);
  Descriptor *descriptor = token_match_type(context, rest);
  MASS_ON_ERROR(*context->result) goto err;
  Value *value = reserve_stack(context->allocator, context->builder, descriptor);
  scope_define(context->scope, name->source, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = value,
  });
  MASS_ON_ERROR(assign(context, &define->source_range, result_value, value));

  err:
  return peek_index;
}

u64
token_parse_definitions(
  Compilation_Context *context,
  Token_View state,
  Value *value_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  return token_parse_definition(context, state, value_result);
}

u64
token_parse_definition_and_assignment_statements(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;

  u64 statement_length = 0;
  view = token_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal(":="), &lhs, &rhs, &operator)) {
    return 0;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) {
    panic("TODO user error");
    return false;
  }
  const Token *name = token_view_get(view, 0);

  if (name->tag != Token_Tag_Id) {
    panic("TODO user error");
    goto err;
  }

  Value *value = value_any(context->allocator);
  token_parse_expression(context, rhs, value, Expression_Parse_Mode_Default);

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (descriptor_is_integer(value->descriptor) && value->operand.tag == Operand_Tag_Immediate) {
    value = value_from_s64(context->allocator, operand_immediate_value_up_to_s64(&value->operand));
  } else if (
    value->descriptor->tag == Descriptor_Tag_Opaque &&
    value->operand.tag == Operand_Tag_Immediate
  ) {
    panic("TODO decide how to handle opaque types");
  }
  Value *on_stack;
  if (value->descriptor->tag == Descriptor_Tag_Function) {
    Descriptor *fn_pointer = descriptor_pointer_to(context->allocator, value->descriptor);
    ensure_compiled_function_body(context, value);
    on_stack = reserve_stack(context->allocator, context->builder, fn_pointer);
    load_address(context, &view.source_range, on_stack, value);
  } else {
    on_stack = reserve_stack(context->allocator, context->builder, value->descriptor);
    MASS_ON_ERROR(assign(context, &name->source_range, on_stack, value)) goto err;
  }

  scope_define(context->scope, name->source, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = on_stack,
  });

  err:
  return statement_length;
}

u64
token_parse_assignment(
  Compilation_Context *context,
  Token_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;
  u64 statement_length = 0;
  view = token_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal("="), &lhs, &rhs, &operator)) {
    return 0;
  }

  Value *target = value_any(context->allocator);
  if (!token_parse_definition(context, lhs, target)) {
    token_parse_expression(context, lhs, target, Expression_Parse_Mode_Default);
  }
  token_parse_expression(context, rhs, target, Expression_Parse_Mode_Default);

  return statement_length;
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
    if (token_parse_syntax_definition(context, statement, &void_value, 0)) {
      continue;
    }
    if (token_parse_import_statement(context, statement)) {
      continue;
    }
    if (token_parse_operator_definition(context, statement)) {
      continue;
    }
    if (token_parse_constant_definitions(context, statement, &void_value, 0)) {
      continue;
    }

    // Report unmatched statements
    context_error_snprintf(
      context, statement.source_range,
      "Could not parse a top level statement"
    );
    break;
  }

  return *context->result;
}

void
scope_define_builtins(
  const Allocator *allocator,
  Scope *scope
) {
  scope_define(scope, slice_literal("[]"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 20, .fixity = Operator_Fixity_Postfix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("()"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 20, .fixity = Operator_Fixity_Postfix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("."), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 19, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("->"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 19, .fixity = Operator_Fixity_Infix, .argument_count = 3 }
  });
  scope_define(scope, slice_literal("macro"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 19, .fixity = Operator_Fixity_Prefix, .argument_count = 1 }
  });
  scope_define(scope, slice_literal("@"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 18, .fixity = Operator_Fixity_Prefix, .argument_count = 1 }
  });
  scope_define(scope, slice_literal("^"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 18, .fixity = Operator_Fixity_Prefix, .argument_count = 1 }
  });

  scope_define(scope, slice_literal("-"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = 17,
      .handler = token_handle_negation,
      .argument_count = 1,
      .fixity = Operator_Fixity_Prefix
    }
  });

  scope_define(scope, slice_literal("&"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 16, .fixity = Operator_Fixity_Prefix, .argument_count = 1 }
  });
  scope_define(scope, slice_literal("*"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("/"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("%"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });

  scope_define(scope, slice_literal("+"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 10, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("-"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 10, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("<"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal(">"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("<="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal(">="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });

  scope_define(scope, slice_literal("=="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 7, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("!="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 7, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("&&"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 5, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("||"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 4, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("any"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_any_value
  });

  scope_define(scope, slice_literal("Register_8"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_8_value
  });
  scope_define(scope, slice_literal("Register_16"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_16_value
  });
  scope_define(scope, slice_literal("Register_32"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_32_value
  });
  scope_define(scope, slice_literal("Register_64"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_64_value
  });

  #define MASS_PROCESS_BUILT_IN_TYPE(_NAME_, _BIT_SIZE_)\
    scope_define(scope, slice_literal(#_NAME_), (Scope_Entry) {\
      .tag = Scope_Entry_Tag_Value,\
      .Value.value = type_##_NAME_##_value\
    });
  MASS_ENUMERATE_BUILT_IN_TYPES
  #undef MASS_PROCESS_BUILT_IN_TYPE

  {
    Array_Token_Statement_Matcher matchers =
      dyn_array_make(Array_Token_Statement_Matcher, .allocator = allocator);

    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_constant_definitions});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_goto});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_explicit_return});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_definitions});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_definition_and_assignment_statements});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_assignment});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_inline_machine_code_bytes});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_statement_label});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_syntax_definition});
    scope->statement_matchers = matchers;
  }
}

Mass_Result
program_parse(
  Compilation_Context *context
) {
  assert(context->module);
  Array_Const_Token_Ptr tokens;

  MASS_TRY(tokenize(context->allocator, &context->module->source_file, &tokens));
  Token_View program_token_view = token_view_from_token_array(tokens, &context->module->source_range);
  MASS_TRY(token_parse(context, program_token_view));
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

void
program_module_init(
  Module *module,
  Slice file_path,
  Slice text,
  Scope *scope
) {
  *module = (Module) {
    .source_file = {
      .path = file_path,
      .text = text,
    },
    .scope = scope,
  };
  module->source_range = (Source_Range) {
    .file = &module->source_file,
    .offsets = { .from = 0, .to = module->source_file.text.length },
  };
}

Module *
program_module_from_file(
  Compilation_Context *context,
  Slice file_path,
  Scope *scope
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = program_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) {
    context_error_snprintf(
      context, (Source_Range){0}, "Unable to open the file %"PRIslice, SLICE_EXPAND_PRINTF(file_path)
    );
    return 0;
  }

  Module *module = allocator_allocate(context->allocator, Module);
  program_module_init(module, file_path, fixed_buffer_as_slice(buffer), scope);
  return module;
}

Mass_Result
program_import_module(
  Compilation_Context *context,
  Module *module
) {
  MASS_TRY(*context->result);
  Compilation_Context import_context = *context;
  import_context.module = module;
  import_context.scope = module->scope;
  return program_parse(&import_context);
}

