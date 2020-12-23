#include "prelude.h"
#include "source.h"
#include "function.h"

Array_Const_Token_Ptr
token_clone_token_array_deep(
  Allocator *allocator,
  Array_Const_Token_Ptr source
);

Token *
token_clone_deep(
  Allocator *allocator,
  const Token *token
) {
  Token *clone = allocator_allocate(allocator, Token);
  *clone = *token;
  switch (token->tag) {
    case Token_Tag_None:
    case Token_Tag_Integer:
    case Token_Tag_Hex_Integer:
    case Token_Tag_Binary_Integer:
    case Token_Tag_Operator:
    case Token_Tag_String:
    case Token_Tag_Id: {
      // Nothing to do
      break;
    }
    case Token_Tag_Group: {
      clone->Group.type = token->Group.type;
      clone->Group.children = token_clone_token_array_deep(allocator, token->Group.children);
      break;
    }
    case Token_Tag_Value: {
      panic("Macro definitions should not contain semi-resolved tokens");
      break;
    }
  }
  return clone;
}

Array_Const_Token_Ptr
token_clone_token_array_deep(
  Allocator *allocator,
  Array_Const_Token_Ptr source
) {
  Array_Const_Token_Ptr result = dyn_array_make(Array_Const_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    const Token *token = *dyn_array_get(source, i);
    Token *clone = token_clone_deep(allocator, token);
    dyn_array_push(result, clone);
  }
  return result;
}

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

Scope_Entry *
scope_lookup(
  Scope *scope,
  Slice name
) {
  while (scope) {
    Scope_Entry *result = hash_map_get(scope->map, name);
    if (result) return result;
    scope = scope->parent;
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

Value *
scope_lookup_force(
  Compilation_Context *context,
  Scope *scope,
  Slice name
) {
  Scope_Entry *entry = 0;
  while (scope) {
    entry = hash_map_get(scope->map, name);
    if (entry) break;
    scope = scope->parent;
  }
  if (!entry) {
    return 0;
  }

  // Force lazy entries
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    if (it->type == Scope_Entry_Type_Lazy_Expression) {
      Scope_Lazy_Expression *expr = &it->lazy_expression;
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
      *it = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .value = result,
        .next_overload = it->next_overload,
      };
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
        Token *body = token_clone_deep(context->allocator, function->body);

        Value *return_result_value =
          function->returns->descriptor->tag == Descriptor_Tag_Void
          ? value_any(context->allocator)
          : function->returns;
        {
          Compilation_Context body_context = *context;
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
      overload->descriptor->Function.next_overload = result;
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
      Value *overload = scope_lookup_force(context, parent, name);
      if (!overload) panic("Just checked that hash map has the name so lookup must succeed");
      if (overload->descriptor->tag != Descriptor_Tag_Function) {
        panic("There should only be function overloads");
      }
      while (last->descriptor->Function.next_overload) {
        last = last->descriptor->Function.next_overload;
      }
      last->descriptor->Function.next_overload = overload;
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
  if (!hash_map_has(scope->map, name)) {
    hash_map_set(scope->map, name, entry);
  } else {
    // TODO Consider using a hash map that allows multiple values instead
    Scope_Entry *it = hash_map_get(scope->map, name);
    while (it->next_overload) {
      it = it->next_overload;
    }
    Scope_Entry *allocated = allocator_allocate(scope->map->allocator, Scope_Entry);
    *allocated = entry;
    it->next_overload = allocated;
  }
}

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
) {
  scope_define(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .value = value,
  });
}

#define SCOPE_OPERATOR_NOT_FOUND (-1)

s64
scope_lookup_operator_precedence(
  Scope *scope,
  Slice name
) {
  Scope_Entry *entry = 0;
  while (scope) {
    entry = hash_map_get(scope->map, name);
    if (entry) break;
    scope = scope->parent;
  }
  if (!entry || entry->type != Scope_Entry_Type_Operator) {
    return SCOPE_OPERATOR_NOT_FOUND;
  }
  return entry->Operator.precedence;
}

void
scope_define_operator(
  Scope *scope,
  Slice name,
  s64 precedence
) {
  scope_define(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Operator,
    .Operator = { .precedence = precedence }
  });
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
  if (pattern->group_type) {
    if (token->tag != Token_Tag_Group) return false;
    return token->Group.type == pattern->group_type;
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

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  Allocator *allocator,
  Source_File *file,
  Array_Const_Token_Ptr *out_tokens
) {
  enum Tokenizer_State {
    Tokenizer_State_Default,
    Tokenizer_State_Integer,
    Tokenizer_State_Binary_Integer,
    Tokenizer_State_Hex_Integer,
    Tokenizer_State_Operator,
    Tokenizer_State_Id,
    Tokenizer_State_String,
    Tokenizer_State_String_Escape,
    Tokenizer_State_Single_Line_Comment,
  };

  Array_Const_Token_Ptr parent_stack = dyn_array_make(Array_Const_Token_Ptr);
  Token *root = &(Token){0};
  root->Group.children = dyn_array_make(Array_Const_Token_Ptr);

  assert(!dyn_array_is_initialized(file->line_ranges));
  file->line_ranges = dyn_array_make(Array_Range_u64);

  Range_u64 current_line = {0};
  enum Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;
  Fixed_Buffer *string_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = 4096,
  );

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

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
    if (parent->tag == Token_Tag_None || parent->Group.type == Token_Group_Type_Curly) {\
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
          start_token(Token_Tag_Hex_Integer);
          i++;
          state = Tokenizer_State_Hex_Integer;
        } else if (ch == '0' && peek == 'b') {
          start_token(Token_Tag_Binary_Integer);
          i++;
          state = Tokenizer_State_Binary_Integer;
        } else if (isdigit(ch)) {
          start_token(Token_Tag_Integer);
          state = Tokenizer_State_Integer;
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
          current_token->Group.type =
            ch == '(' ? Token_Group_Type_Paren :
            ch == '{' ? Token_Group_Type_Curly :
            Token_Group_Type_Square;
          current_token->Group.children = dyn_array_make(Array_Const_Token_Ptr, 4);
          dyn_array_push(parent->Group.children, current_token);
          dyn_array_push(parent_stack, parent);
          parent = current_token;
        } else if (ch == ')' || ch == '}' || ch == ']') {
          if (parent->tag != Token_Tag_Group) {
            panic("Tokenizer: unexpected closing char for group");
          }
          s8 expected_paren = 0;
          switch (parent->Group.type) {
            case Token_Group_Type_Paren: {
              expected_paren = ')';
              break;
            }
            case Token_Group_Type_Curly: {
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
            case Token_Group_Type_Square: {
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
      case Tokenizer_State_Integer: {
        if (!isdigit(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Hex_Integer: {
        if (!code_point_is_hex_digit(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Binary_Integer: {
        if (ch != '0' && ch != '1') {
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
scope_lookup_type(
  Compilation_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice type_name
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *value = scope_lookup_force(context, scope, type_name);
  if (!value) return 0;
  if (value->descriptor->tag != Descriptor_Tag_Type) {
    program_error_builder(context, source_range) {
      program_error_append_slice(type_name);
      program_error_append_literal(" is not a type");
    }
    return 0;
  }
  Descriptor *descriptor = value->descriptor->Type.descriptor;
  return descriptor;
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
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Could not find type ");
          program_error_append_slice(token->source);
        }
      }
      break;
    }
    case Token_Tag_Group: {
      if (token->Group.type != Token_Group_Type_Square) {
        panic("TODO");
      }
      if (dyn_array_length(token->Group.children) != 1) {
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Pointer type must have a single type inside");
        }
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
    case Token_Tag_Binary_Integer:
    case Token_Tag_Hex_Integer:
    case Token_Tag_Integer: {
      program_error_builder(context, token->source_range) {
        program_error_append_slice(token->source);
        program_error_append_literal(" is not a type");
      }
      return 0;
    }
    case Token_Tag_Operator:
    case Token_Tag_String:
    case Token_Tag_Value:
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
      case Token_Tag_Integer:
      case Token_Tag_Binary_Integer:
      case Token_Tag_Hex_Integer:
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
        copy->Group.type = token->Group.type;
        copy->Group.children = token_apply_macro_replacements(
          context, map, token_view_from_token_array(token->Group.children)
        );
        dyn_array_push(result, copy);
        break;
      }
      case Token_Tag_Value: {
        panic("Macro definitions should not contain semi-resolved tokens");
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
    program_error_builder(context, token->source_range) {
      program_error_append_literal("Can not resolve type");
    }
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

Token_Match_Arg *
token_match_argument(
  Compilation_Context *context,
  Token_View view,
  Descriptor_Function *function
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Token_Match_Arg *arg = 0;

  Token_View lhs;
  Token_View rhs;
  const Token *operator;
  if (token_maybe_split_on_operator(view, slice_literal(":"), &lhs, &rhs, &operator)) {
    if (lhs.length == 0) {
      program_error_builder(context, operator->source_range) {
        program_error_append_literal("':' operator expects an identifier on the left hand side");
      }
      goto err;
    }
    if (lhs.length > 1 || !token_match(lhs.tokens[0], &(Token_Pattern){ .tag = Token_Tag_Id })) {
      program_error_builder(context, operator->source_range) {
        program_error_append_literal("':' operator expects only a single identifier on the left hand side");
      }
      goto err;
    }
    Descriptor *type_descriptor = token_match_type(context, rhs);
    if (!type_descriptor) {
      goto err;
    }

    arg = allocator_allocate(context->allocator, Token_Match_Arg);
    *arg = (Token_Match_Arg){.name = lhs.tokens[0]->source};

    if (type_descriptor == &descriptor_any) {
      // TODO figure out what should happen here or not use any type for macros
      arg->value = value_any(context->allocator);
    } else {
      arg->value = function_next_argument_value(
        context->allocator, function, type_descriptor
      );
    }
  } else {
    Value *literal_value_type = token_parse_constant_expression(context, view);
    arg = allocator_allocate(context->allocator, Token_Match_Arg);
    *arg = (Token_Match_Arg){
      .name = {0},
      .value = literal_value_type,
    };
  }

  err:
  return arg;
}

Value *
value_from_integer_token(
  Compilation_Context *context,
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  bool ok = false;
  u64 number = slice_parse_u64(token->source, &ok);
  if (!ok) {
    program_error_builder(context, token->source_range) {
      program_error_append_literal("Invalid integer literal: ");
      program_error_append_slice(token->source);
    }
    return 0;
  }
  return value_from_unsigned_immediate(context->allocator, number);
}

Value *
value_from_hex_integer_token(
  Compilation_Context *context,
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  bool ok = false;
  Slice digits = slice_sub(token->source, 2, token->source.length);
  u64 number = slice_parse_hex(digits, &ok);
  if (!ok) {
    program_error_builder(context, token->source_range) {
      program_error_append_literal("Invalid integer hex literal: ");
      program_error_append_slice(token->source);
    }
    return 0;
  }
  // TODO should be unsigned
  return value_from_signed_immediate(context->allocator, number);
}

u64
slice_parse_binary(
  Slice slice,
  bool *ok
) {
  if (!ok) ok = &(bool){0};
  *ok = true;

  u64 integer = 0;
  for (u64 index = 0; index < slice.length; ++index) {
    u64 byte_index = slice.length - index - 1;
    s8 byte = slice.bytes[byte_index];
    u64 digit = 0;
    if (byte == '1') {
      digit = 1;
    } else if (byte != '0') {
      *ok = false;
      return 0;
    }
    integer += digit << index;
  }
  return integer;
}

Value *
value_from_binary_integer_token(
  Compilation_Context *context,
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  bool ok = false;
  Slice digits = slice_sub(token->source, 2, token->source.length);
  u64 number = slice_parse_binary(digits, &ok);
  if (!ok) {
    program_error_builder(context, token->source_range) {
      program_error_append_literal("Invalid integer binary literal: ");
      program_error_append_slice(token->source);
    }
    return 0;
  }
  return value_from_unsigned_immediate(context->allocator, number);
}

Value *
token_force_constant_value(
  Compilation_Context *context,
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  switch(token->tag) {
    case Token_Tag_None: {
      panic("Internal Error: Encountered token with an uninitialized tag");
      break;
    }
    case Token_Tag_Integer: {
      return value_from_integer_token(context, token);
    }
    case Token_Tag_Hex_Integer: {
      return value_from_hex_integer_token(context, token);
    }
    case Token_Tag_Binary_Integer: {
      return value_from_binary_integer_token(context, token);
    }
    case Token_Tag_String: {
      Slice string = token->String.slice;
      Value *string_bytes = value_global_c_string_from_slice(context, string);
      // TODO make a proper string type
      return string_bytes;
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, context->scope, name);
      if (!value) {
        MASS_ON_ERROR(*context->result) return 0;
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Undefined variable ");
          program_error_append_slice(name);
        }
      }
      return value;
    }
    case Token_Tag_Value: {
      return token->Value.value;
    }
    case Token_Tag_Group: {
      if (token->Group.type == Token_Group_Type_Paren) {
        Token_View children_view = token_view_from_token_array(token->Group.children);
        return token_parse_constant_expression(context, children_view);
      } else {
        panic("TODO support other group types in constant context");
      }
      return 0;
    }
    case Token_Tag_Operator: {
      panic("TODO support operator lookup in constant context");
      return 0;
    }
  }
  panic("Internal Error: Unknown token type");
  return 0;
}

PRELUDE_NO_DISCARD Mass_Result
token_force_value(
  Compilation_Context *context,
  const Token *token,
  Value *result_value
) {
  MASS_TRY(*context->result);
  assert(context->builder);

  Scope *scope = context->scope;
  switch(token->tag) {
    case Token_Tag_None: {
      panic("Internal Error: Encountered token with an uninitialized tag");
      break;
    }
    case Token_Tag_Integer: {
      Value *immediate = value_from_integer_token(context, token);
      move_value(context->allocator, context->builder, &token->source_range, result_value, immediate);
      return *context->result;
    }
    case Token_Tag_Hex_Integer: {
      Value *immediate = value_from_hex_integer_token(context, token);
      move_value(context->allocator, context->builder, &token->source_range, result_value, immediate);
      return *context->result;
    }
    case Token_Tag_Binary_Integer: {
      Value *immediate = value_from_binary_integer_token(context, token);
      move_value(context->allocator, context->builder, &token->source_range, result_value, immediate);
      return *context->result;
    }
    case Token_Tag_String: {
      Slice string = token->String.slice;
      Value *string_bytes = value_global_c_string_from_slice(context, string);
      Value *c_string_pointer = value_pointer_to(
        context, context->builder, &token->source_range, string_bytes
      );
      move_value(context->allocator, context->builder, &token->source_range, result_value, c_string_pointer);
      return *context->result;
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name);
      if (!value) {
        MASS_TRY(*context->result);
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Undefined variable ");
          program_error_append_slice(name);
        }
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
      switch(token->Group.type) {
        case Token_Group_Type_Paren: {
          Token_View expression_tokens = token_view_from_token_array(token->Group.children);
          token_parse_expression(context, expression_tokens, result_value);
          return *context->result;
        }
        case Token_Group_Type_Curly: {
          token_parse_block(context, token, result_value);
          return *context->result;
        }
        case Token_Group_Type_Square: {
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

// FIXME pass in the function definition
Array_Value_Ptr
token_match_constant_call_arguments(
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
      Value *result_value = token_parse_constant_expression(context, view);
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

bool
token_parse_syntax_definition(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id, .source = slice_literal("syntax"));

  Token_Maybe_Match(pattern_token, .group_type = Token_Group_Type_Paren);

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
          program_error_builder(context, token->source_range) {
            program_error_append_literal(
              "Nested group matches are not supported in syntax declarations (yet)"
            );
          }
          goto err;
        }
        dyn_array_push(pattern, (Macro_Pattern) {
          .tag = Macro_Pattern_Tag_Single_Token,
          .Single_Token = {
            .token_pattern = {
              .group_type = token->Group.type,
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
            program_error_builder(context, token->source_range) {
              program_error_append_literal(
                "@ operator in a syntax definition requires an id after it"
              );
            }
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
            program_error_builder(context, token->source_range) {
              program_error_append_literal("@ requires a valid pattern before it");
            }
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
            program_error_builder(context, token->source_range) {
              program_error_append_literal(
                "^ operator (statement start match) can only appear at the start of the pattern."
              );
            }
            goto err;
          }
          statement_start = true;
        } else if (slice_equal(token->source, slice_literal("$"))) {
          if (i != definition.length - 1) {
            program_error_builder(context, token->source_range) {
              program_error_append_literal(
                "$ operator (statement end match) can only appear at the end of the pattern."
              );
            }
            goto err;
          }
          statement_end = true;
        } else {
          program_error_builder(context, token->source_range) {
            program_error_append_literal("Unsupported operator ");
            program_error_append_slice(token->source);
            program_error_append_literal(" in a syntax definition");
          }
          goto err;
        }
        break;
      }
      case Token_Tag_Id:
      case Token_Tag_Integer:
      case Token_Tag_Binary_Integer:
      case Token_Tag_Hex_Integer:
      case Token_Tag_Value: {
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Unsupported token tag in a syntax definition");
        }
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
  if (!operand_is_immediate(&bit_size_value->operand)) {
    // TODO err
    goto err;
  }
  u64 bit_size = s64_to_u64(operand_immediate_value_up_to_s64(&bit_size_value->operand));
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Opaque,
    .Opaque = { .bit_size = bit_size },
  };

  Descriptor *value_descriptor = allocator_allocate(context->allocator, Descriptor);
  *value_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Type,
    .Type = { .descriptor = descriptor },
  };
  Value *result = allocator_allocate(context->allocator, Value);
  *result = (Value) {
    .descriptor = value_descriptor,
    .operand = {.tag = Operand_Tag_None },
  };
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

  if (!token_match(args, &(Token_Pattern) { .group_type = Token_Group_Type_Paren })) {
    program_error_builder(context, args->source_range) {
      program_error_append_literal("c_struct must be followed by ()");
    }
    goto err;
  }
  if (dyn_array_length(args->Group.children) != 1) {
    program_error_builder(context, args->source_range) {
      program_error_append_literal("c_struct expects 1 argument, got ");
      program_error_append_number("%" PRIu64, dyn_array_length(args->Group.children));
    }
    goto err;
  }
  const Token *layout_block = *dyn_array_get(args->Group.children, 0);
  if (!token_match(layout_block, &(Token_Pattern) { .group_type = Token_Group_Type_Curly })) {
    program_error_builder(context, args->source_range) {
      program_error_append_literal("c_struct expects a {} block as the argument");
    }
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

  Descriptor *value_descriptor = allocator_allocate(context->allocator, Descriptor);
  *value_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Type,
    .Type = { .descriptor = descriptor },
  };
  *result = (Value) {
    .descriptor = value_descriptor,
    .operand = {.tag = Operand_Tag_None },
  };
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
    program_error_builder(context, source_range) {
      program_error_append_literal("First argument to external() must be a literal string");
    }
    return 0;
  }
  const Token *comma = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_Operator,
    .source = slice_literal(","),
  });
  if (!comma) {
    program_error_builder(context, source_range) {
      program_error_append_literal("external(\"library_name\", \"symbol_name\") requires two arguments");
    }
    return 0;
  }
  const Token *symbol_name_token = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_String,
  });
  if (!symbol_name_token) {
    program_error_builder(context, source_range) {
      program_error_append_literal("Second argument to external() must be a literal string");
    }
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
      .operand = label32(builder->code_block.end_label),
    };
    scope_define_value(function_scope, MASS_RETURN_LABEL_NAME, return_label_value);
  }

  switch (dyn_array_length(return_types->Group.children)) {
    case 0: {
      descriptor->Function.returns = &void_value;
      break;
    }
    case 1: {
      const Token *return_type_token = *dyn_array_get(return_types->Group.children, 0);
      Descriptor *return_descriptor = token_force_type(context, function_scope, return_type_token);
      if (!return_descriptor) return 0;
      function_return_descriptor(context, &descriptor->Function, return_descriptor);
      break;
    }
    default: {
      panic("Multiple return types are not supported at the moment");
      break;
    }
  }
  scope_define_value(function_scope, MASS_RETURN_VALUE_NAME, descriptor->Function.returns);

  if (dyn_array_length(args->Group.children) != 0) {
    Token_View children = token_view_from_token_array(args->Group.children);
    Token_View_Split_Iterator it = { .view = children };

    while (!it.done) {
      Token_View arg_view = token_split_next(&it, &token_pattern_comma_operator);
      Compilation_Context arg_context = *context;
      arg_context.scope = function_scope;
      arg_context.builder = builder;
      Token_Match_Arg *arg = token_match_argument(&arg_context, arg_view, &descriptor->Function);
      if (!arg) return 0;

      // Literal values do not have a name ATM
      if (arg->name.length) {
        scope_define_value(function_scope, arg->name, arg->value);
      }
      dyn_array_push(descriptor->Function.argument_names, arg->name);
      dyn_array_push(descriptor->Function.arguments, arg->value);

      if (!is_external && arg->value->operand.tag == Operand_Tag_Register) {
        register_bitset_set(
          &builder->code_block.register_occupied_bitset,
          arg->value->operand.Register.index
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
  eval_context.scope = eval_program.global_scope;
  eval_context.program = &eval_program;
  eval_context.builder = fn_begin(&eval_context);
  function_return_descriptor(
    context, &eval_context.builder->value->descriptor->Function, &descriptor_void
  );

  Value *expression_result_value = value_any(context->allocator);
  token_parse_expression(&eval_context, view, expression_result_value);

  // We use a something like a C++ reference out parameter for the
  // result to have a consitent function signature on this C side of things.

  // Make it out parameter a pointer to ensure it is passed inside a register according to ABI
  Value *arg_value = function_next_argument_value(
    context->allocator,
    &eval_context.builder->value->descriptor->Function,
    descriptor_pointer_to(context->allocator, expression_result_value->descriptor)
  );
  dyn_array_push(eval_context.builder->value->descriptor->Function.arguments, arg_value);

  // Create a reference Value
  assert(arg_value->operand.tag == Operand_Tag_Register);
  Value *out_value = allocator_allocate(context->allocator, Value);
  *out_value = (Value) {
    .descriptor = expression_result_value->descriptor,
    .operand = (Operand){
      .tag = Operand_Tag_Memory_Indirect,
      .byte_size = expression_result_value->operand.byte_size,
      .Memory_Indirect = { .reg = arg_value->operand.Register.index },
    },
  };

  move_value(context->allocator, eval_context.builder, &first_token->source_range, out_value, expression_result_value);
  fn_end(eval_context.builder);

  program_jit(&eval_context);

  u32 result_byte_size = out_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(context->allocator, result_byte_size, alignment);

  Compile_Time_Eval_Proc jitted_code =
    (Compile_Time_Eval_Proc)value_as_function(&eval_program, eval_context.builder->value);

  jitted_code(result);
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
    case Descriptor_Tag_Tagged_Union:
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
    case Descriptor_Tag_Function:
    case Descriptor_Tag_Type: {
      panic("TODO figure out how that works");
      break;
    }
  }
  return token_value_make(context, token_value, source_range_from_token_view(view));
}

typedef void (*Operator_Dispatch_Proc)(
  Compilation_Context *,
  Token_View,
  Array_Const_Token_Ptr *token_stack,
  Array_Slice *operator_stack,
  Slice operator
);

const Token *
token_handle_cast(
  Compilation_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  assert(type->descriptor->tag == Descriptor_Tag_Type);

  Descriptor *cast_to_descriptor = type->descriptor->Type.descriptor;
  assert(descriptor_is_integer(cast_to_descriptor));
  assert(value->descriptor->tag == cast_to_descriptor->tag);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *result = value;
  if (cast_to_byte_size != original_byte_size) {
    result = allocator_allocate(context->allocator, Value);

    if (operand_is_immediate(&value->operand)) {
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
  const Token *token
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *value = token_force_constant_value(context, token);
  if (descriptor_is_integer(value->descriptor) && operand_is_immediate(&value->operand)) {
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
  Array_Slice *operator_stack,
  Slice operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  if (slice_equal(operator, slice_literal("-x"))) {
    const Token *token = *dyn_array_pop(*token_stack);
    const Token *new_token = token_handle_negation(context, token);
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
      Array_Value_Ptr arg_values = token_match_constant_call_arguments(context, args);
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
    Value *function_value = token_force_constant_value(context, function);
    if (function_value) {
      if (function_value->descriptor->tag == Descriptor_Tag_Function) {
        Descriptor_Function *descriptor = &function_value->descriptor->Function;
        if (descriptor->flags & Descriptor_Function_Flags_External) {
          program_error_builder(context, function->source_range) {
            program_error_append_literal("External functions can not be macro");
          }
        } else {
          descriptor->flags |= Descriptor_Function_Flags_Macro;
        }
      } else {
        program_error_builder(context, function->source_range) {
          program_error_append_literal("Trying to mark a non-function as macro");
        }
      }
    }
  } else if (slice_equal(operator, slice_literal("@"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    Token *result = 0;
    if (body->tag == Token_Tag_Group && body->Group.type == Token_Group_Type_Paren) {
      Token_View eval_view = token_view_from_token_array(body->Group.children);
      result = compile_time_eval(context, eval_view, context->scope);
    } else {
      program_error_builder(context, body->source_range) {
        program_error_append_literal("@ operator must be followed by a parenthesized expression");
      }
      result = token_value_make(context, 0, body->source_range);
    }
    dyn_array_push(*token_stack, result);
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
  Array_Slice *operator_stack,
  Slice new_operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  s64 precedence = scope_lookup_operator_precedence(context->scope, new_operator);
  if (precedence == SCOPE_OPERATOR_NOT_FOUND) {
    Source_Range source_range = source_range_from_token_view(view);
    program_error_builder(context, source_range) {
      program_error_append_literal("Unknown operator ");
      program_error_append_slice(new_operator);
    }
    return false;
  }
  while (dyn_array_length(*operator_stack)) {
    Slice last_operator = *dyn_array_last(*operator_stack);
    s64 last_operator_precedence = scope_lookup_operator_precedence(context->scope, last_operator);
    if (last_operator_precedence <= precedence) {
      break;
    }

    // apply the operator on the stack
    Slice popped_operator = *dyn_array_pop(*operator_stack);
    dispatch_proc(context, view, token_stack, operator_stack, popped_operator);
  }
  dyn_array_push(*operator_stack, new_operator);
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
  Array_Slice operator_stack = dyn_array_make(Array_Slice);

  Value *result = 0;
  bool is_previous_an_operator = true;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);

    switch(token->tag) {
      case Token_Tag_None: {
        panic("Internal Error: Encountered token with an uninitialized tag");
        break;
      }
      case Token_Tag_Integer:
      case Token_Tag_Binary_Integer:
      case Token_Tag_Hex_Integer:
      case Token_Tag_String:
      case Token_Tag_Value: {
        dyn_array_push(token_stack, token);
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Group: {
        dyn_array_push(token_stack, token);
        switch (token->Group.type) {
          case Token_Group_Type_Paren: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_constant_operator,
                &token_stack, &operator_stack, slice_literal("()")
              )) goto err;
            }
            break;
          }
          case Token_Group_Type_Curly: {
            // Nothing special to do for now?
            break;
          }
          case Token_Group_Type_Square: {
            panic("TODO support parsing [] in constant expressions");
            break;
          }
        }
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Id: {
        s64 precedence = scope_lookup_operator_precedence(context->scope, token->source);
        if (precedence == SCOPE_OPERATOR_NOT_FOUND) {
          is_previous_an_operator = false;
          dyn_array_push(token_stack, token);
        } else {
          if (!token_handle_operator(
            context, view, token_dispatch_constant_operator,
            &token_stack, &operator_stack, token->source
          )) goto err;
          is_previous_an_operator = true;
        }
        break;
      }
      case Token_Tag_Operator: {
        Slice operator = token->source;
        // unary minus, i.e x + -5
        if (is_previous_an_operator && slice_equal(operator, slice_literal("-"))) {
          operator = slice_literal("-x");
        }
        if (!token_handle_operator(
          context, view, token_dispatch_constant_operator,
          &token_stack, &operator_stack, operator
        )) goto err;
        is_previous_an_operator = true;
        break;
      }
    }
  }

  while (dyn_array_length(operator_stack)) {
    Slice operator = *dyn_array_pop(operator_stack);
    token_dispatch_constant_operator(
      context, view, &token_stack, &operator_stack, operator
    );
  }
  if (dyn_array_length(token_stack) == 1) {
    const Token *token = *dyn_array_last(token_stack);
    result = token_force_constant_value(context, token);
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

  for (Value *to_call = target; to_call; to_call = to_call->descriptor->Function.next_overload) {
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
      .lazy_expression = {
        .tokens = arg_expr,
        .scope = context->scope,
        .maybe_builder = context->builder,
      },
    });
  }

  // Define a new return target label and value so that explicit return statements
  // jump to correct location and put value in the right place
  Operand fake_return_label =
    label32(make_label(context->program, &context->program->data_section));
  {
    scope_define_value (body_scope, MASS_RETURN_LABEL_NAME, &(Value) {
      .descriptor = &descriptor_void,
      .operand = fake_return_label,
    });
    assert(result_value);
    scope_define_value(body_scope, MASS_RETURN_VALUE_NAME, result_value);
  }

  Token *body = token_clone_deep(context->allocator, function->body);
  {
    Compilation_Context body_context = *context;
    body_context.scope = body_scope;
    token_parse_block(&body_context, body, result_value);
  }

  push_instruction(
    &context->builder->code_block.instructions,
    call_source_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = fake_return_label.Label.index
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
  assert(token_match(args_token, &(Token_Pattern){.group_type = Token_Group_Type_Paren}));

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
    program_error_builder(context, target_token->source_range) {
      program_error_append_slice(target_token->source);
      program_error_append_literal(" is not a function");
    }
    return false;
  }
  Token *result = 0;
  const Source_Range *source_range = &target_token->source_range;

  struct Overload_Match { Value *value; s64 score; } match = { .score = -1 };
  for (Value *to_call = target; to_call; to_call = to_call->descriptor->Function.next_overload) {
    Descriptor_Function *descriptor = &to_call->descriptor->Function;
    if (dyn_array_length(args) != dyn_array_length(descriptor->arguments)) continue;
    s64 score = calculate_arguments_match_score(descriptor, args);
    if (score == match.score) {
      program_error_builder(context, target_token->source_range) {
        // TODO improve error message
        program_error_append_literal("Could not decide which overload to pick");
        // TODO provide names of matched overloads
      }
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
        scope_define_value(body_scope, arg_name, arg_value);
      }
      return_value = result_value;

      // Define a new return target label and value so that explicit return statements
      // jump to correct location and put value in the right place
      Operand fake_return_label =
        label32(make_label(context->program, &context->program->data_section));
      {
        scope_define_value (body_scope, MASS_RETURN_LABEL_NAME, &(Value) {
          .descriptor = &descriptor_void,
          .operand = fake_return_label,
        });
        assert(return_value);
        scope_define_value(body_scope, MASS_RETURN_VALUE_NAME, return_value);
      }

      Token *body = token_clone_deep(context->allocator, function->body);
      {
        Compilation_Context body_context = *context;
        body_context.scope = body_scope;
        token_parse_block(&body_context, body, return_value);
      }

      push_instruction(
        &context->builder->code_block.instructions,
        &target_token->source_range,
        (Instruction) {
          .type = Instruction_Type_Label,
          .label = fake_return_label.Label.index
        }
      );
    } else {
      return_value = call_function_overload(context, source_range, overload, args);
    }

    result = token_value_make(context, return_value, target_token->source_range);
  } else {
    program_error_builder(context, target_token->source_range) {
      // TODO add better error message
      program_error_append_literal("Could not find matching overload");
    }
  }
  dyn_array_destroy(args);
  return result;
}

void
token_dispatch_operator(
  Compilation_Context *context,
  Token_View view,
  Array_Const_Token_Ptr *token_stack,
  Array_Slice *operator_stack,
  Slice operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  const Token *result_token;
  if (slice_equal(operator, slice_literal("-x"))) {
    const Token *token = *dyn_array_pop(*token_stack);
    result_token = token_handle_negation(context, token);
  } else if (slice_equal(operator, slice_literal("[]"))) {
    const Token *brackets = *dyn_array_pop(*token_stack);
    const Token *target_token = *dyn_array_pop(*token_stack);

    Value *array = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, target_token, array)) return;
    Value *index_value = value_any(context->allocator);
    Token_View index_tokens = token_view_from_token_array(brackets->Group.children);
    token_parse_expression(context, index_tokens, index_value);
    assert(array->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
    assert(array->operand.tag == Operand_Tag_Memory_Indirect);

    Descriptor *item_descriptor = array->descriptor->Fixed_Size_Array.item;
    u32 item_byte_size = descriptor_byte_size(item_descriptor);

    Value *result = allocator_allocate(context->allocator, Value);
    if (operand_is_immediate(&index_value->operand)) {
      s32 index = s64_to_s32(operand_immediate_value_up_to_s64(&index_value->operand));
      *result = (Value){
        .descriptor = item_descriptor,
        .operand = {
          .tag = Operand_Tag_Memory_Indirect,
          .byte_size = item_byte_size,
          .Memory_Indirect = (Operand_Memory_Indirect) {
            .reg = array->operand.Memory_Indirect.reg,
            .displacement = array->operand.Memory_Indirect.displacement + index * item_byte_size,
          }
        }
      };

    } else if(
      item_byte_size == 1 ||
      item_byte_size == 2 ||
      item_byte_size == 4 ||
      item_byte_size == 8
    ) {
      SIB_Scale scale = SIB_Scale_1;
      if (item_byte_size == 2) {
        scale = SIB_Scale_2;
      } else if (item_byte_size == 4) {
        scale = SIB_Scale_4;
      } else if (item_byte_size == 8) {
        scale = SIB_Scale_8;
      }
      Value *index_value_in_register =
        value_register_for_descriptor(context->allocator, Register_R10, index_value->descriptor);
      move_value(
        context->allocator,
        context->builder,
        &target_token->source_range,
        index_value_in_register,
        index_value
      );
      *result = (Value){
        .descriptor = item_descriptor,
        .operand = {
          .tag = Operand_Tag_Sib,
          .byte_size = item_byte_size,
          .Sib = (Operand_Sib) {
            .scale = scale,
            .index = index_value_in_register->operand.Register.index,
            .base = array->operand.Memory_Indirect.reg,
            .displacement = array->operand.Memory_Indirect.displacement,
          }
        }
      };
    } else {
      panic("TODO");
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
    } else {
      result_token = token_handle_function_call(
        context, target, args_token, value_any(context->allocator)
      );
    }
  } else if (slice_equal(operator, slice_literal("&"))) {
    const Token *pointee_token = *dyn_array_pop(*token_stack);

    Value *pointee = value_any(context->allocator);
    MASS_ON_ERROR(token_force_value(context, pointee_token, pointee)) return;
    Value *result_value = value_pointer_to(
      context, context->builder, &pointee_token->source_range, pointee
    );

    result_token = token_value_make(context, result_value, pointee_token->source_range);
  } else if (slice_equal(operator, slice_literal("."))) {
    const Token *rhs = *dyn_array_pop(*token_stack);
    const Token *lhs = *dyn_array_pop(*token_stack);

    Value *result_value = 0;
    if (rhs->tag == Token_Tag_Id) {
      Value *struct_value = value_any(context->allocator);
      MASS_ON_ERROR(token_force_value(context, lhs, struct_value)) return;
      result_value = struct_get_field(context->allocator, struct_value, rhs->source);
    } else {
      program_error_builder(context, rhs->source_range) {
        program_error_append_literal("Right hand side of the . operator must be an identifier");
      }
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
      program_error_builder(context, lhs->source_range) {
        program_error_append_literal("Left hand side of the ");
        program_error_append_slice(operator);
        program_error_append_literal(" is not an integer");
      }
      return;
    }
    if (!descriptor_is_integer(rhs_value->descriptor)) {
      program_error_builder(context, lhs->source_range) {
        program_error_append_literal("Right hand side of the ");
        program_error_append_slice(operator);
        program_error_append_literal(" is not an integer");
      }
      return;
    }

    if (!(same_value_type_or_can_implicitly_move_cast(lhs_value, rhs_value))) {
      program_error_builder(context, lhs->source_range) {
        program_error_append_literal("Incompatible integer types in comparison");
      }
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
  Array_Slice operator_stack = dyn_array_make(Array_Slice);

  bool is_previous_an_operator = true;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);

    switch(token->tag) {
      case Token_Tag_None: {
        panic("Internal Error: Encountered token with an uninitialized tag");
        break;
      }
      case Token_Tag_Integer:
      case Token_Tag_Binary_Integer:
      case Token_Tag_Hex_Integer:
      case Token_Tag_String:
      case Token_Tag_Id:
      case Token_Tag_Value: {
        dyn_array_push(token_stack, token);
        is_previous_an_operator = false;
        break;
      }
      case Token_Tag_Group: {
        dyn_array_push(token_stack, token);
        switch (token->Group.type) {
          case Token_Group_Type_Paren: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_operator,
                &token_stack, &operator_stack, slice_literal("()")
              )) goto err;
            }
            break;
          }
          case Token_Group_Type_Curly: {
            // Nothing special to do for now?
            break;
          }
          case Token_Group_Type_Square: {
            if (!is_previous_an_operator) {
              if (!token_handle_operator(
                context, view, token_dispatch_operator,
                &token_stack, &operator_stack, slice_literal("[]")
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
        // unary minus, i.e x + -5
        if (is_previous_an_operator && slice_equal(operator, slice_literal("-"))) {
          operator = slice_literal("-x");
        }
        if (!token_handle_operator(
          context, view, token_dispatch_operator,
          &token_stack, &operator_stack, operator
        )) goto err;
        is_previous_an_operator = true;
        break;
      }
    }
  }

  while (dyn_array_length(operator_stack)) {
    Slice operator = *dyn_array_pop(operator_stack);
    token_dispatch_operator(context, view, &token_stack, &operator_stack, operator);
  }
  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(token_stack) == 1) {
      const Token *token = *dyn_array_last(token_stack);
      assert(token);
      MASS_ON_ERROR(token_force_value(context, token, result_value)) goto err;
    } else {
      program_error_builder(context, source_range_from_token_view(view)) {
        program_error_append_literal("Could not parse the expression");
      }
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
  assert(block->Group.type == Token_Group_Type_Curly);
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

    // If result is a register we need to make sure it is acquired to avoid it being used
    // as temporary when evaluating last statement. This definitely can happen with
    // the function returns but should be safe to do all the time.
    if (is_last_statement && result_value->operand.tag == Operand_Tag_Register) {
      if (!register_bitset_get(
        context->builder->used_register_bitset, result_value->operand.Register.index
      )) {
        register_acquire(context->builder, result_value->operand.Register.index);
      }
    }

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
    program_error_builder(context, keyword->source_range) {
      program_error_append_literal("`label` must be followed by an identifier");
    }
    goto err;
  }

  const Token *id = token_view_get(rest, 0);

  // :ForwardLabelRef
  // First try to lookup a label that might have been declared by `goto`
  // FIXME make sure we don't double declare label
  Value *value = scope_lookup_force(
    context,
    context->builder->value->descriptor->Function.scope,
    id->source
  );

  if (!value) {
    Label_Index label = make_label(context->program, &context->program->code_section);
    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = label32(label),
    };
    // FIXME this should define a label in the function scope, but because
    // the macros are not hygienic we can not do that yet
    //scope_define_value(builder->value->descriptor->Function.scope, id->source, value);
    scope_define_value(context->scope, id->source, value);
  }

  if (
    value->descriptor != &descriptor_void ||
    value->operand.tag != Operand_Tag_Label
  ) {
    program_error_builder(context, keyword->source_range) {
      program_error_append_literal("Trying to redefine variable ");
      program_error_append_slice(id->source);
      program_error_append_literal(" as a label");
    }
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, &keyword->source_range,
    (Instruction) { .type = Instruction_Type_Label, .label = value->operand.Label.index }
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
    program_error_builder(context, keyword->source_range) {
      program_error_append_literal("`if` keyword must be followed by an expression");
    }
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
    &context->builder->code_block.instructions, &keyword->source_range,
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
    program_error_builder(context, keyword->source_range) {
      program_error_append_literal("`goto` keyword must be followed by an identifier");
    }
    goto err;
  }

  if (rest.length > 1) {
    program_error_builder(context, token_view_get(rest, 1)->source_range) {
      program_error_append_literal("Unexpected token");
    }
    goto err;
  }
  const Token *id = token_view_get(rest, 0);
  if (!token_match(id, &(Token_Pattern){.tag = Token_Tag_Id})) {
    program_error_builder(context, id->source_range) {
      program_error_append_literal("`goto` keyword must be followed by an identifier");
    }
    goto err;
  }

  Value *value = scope_lookup_force(context, context->scope, id->source);

  // :ForwardLabelRef
  // If we didn't find an identifier with this name, declare one and hope
  // that some label will resolve it
  // FIXME somehow report unresolved labels
  if (!value) {
    Label_Index label = make_label(context->program, &context->program->code_section);
    value = allocator_allocate(context->allocator, Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = label32(label),
    };
    // Label declarations are always done in the function scope as they
    // might need to jump out of a nested block.
    scope_define_value(context->builder->value->descriptor->Function.scope, id->source, value);
  }

  if (
    value->descriptor != &descriptor_void ||
    value->operand.tag != Operand_Tag_Label
  ) {
    program_error_builder(context, keyword->source_range) {
      program_error_append_slice(id->source);
      program_error_append_literal(" is not a label");
    }
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, &keyword->source_range,
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

  Value *fn_return = scope_lookup_force(context, context->scope, MASS_RETURN_VALUE_NAME);
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
    program_error_builder(context, keyword->source_range) {
      program_error_append_literal("Explicit return from a non-void function requires a value");
    }
  }

  Value *return_label = scope_lookup_force(context, context->scope, MASS_RETURN_LABEL_NAME);
  assert(return_label);
  assert(return_label->descriptor == &descriptor_void);
  assert(return_label->operand.tag == Operand_Tag_Label);

  push_instruction(
    &context->builder->code_block.instructions,
    &keyword->source_range,
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
  Token_Match(square_brace, .group_type = Token_Group_Type_Square);
  Descriptor *descriptor =
    scope_lookup_type(context, context->scope, type->source_range, type->source);

  Token_View size_view = token_view_from_token_array(square_brace->Group.children);
  Value *size_value = token_parse_constant_expression(context, size_view);
  if (!size_value) return 0;
  if (!descriptor_is_integer(size_value->descriptor)) {
    program_error_builder(context, square_brace->source_range) {
      program_error_append_literal("Fixed size array size is not an integer");
    }
    return 0;
  }
  if (!operand_is_immediate(&size_value->operand)) {
    program_error_builder(context, square_brace->source_range) {
      program_error_append_literal("Fixed size array size must be known at compile time");
    }
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
  Token_Match(args_token, .group_type = Token_Group_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(context, args_token);

  Instruction_Bytes bytes = {
    .label_offset_in_instruction = INSTRUCTION_BYTES_NO_LABEL,
  };

  for (u64 i = 0; i < dyn_array_length(args); ++i) {
    if (bytes.length >= 15) {
      program_error_builder(context, args_token->source_range) {
        program_error_append_literal("Expected a maximum of 15 bytes");
      }
    }
    Value *value = *dyn_array_get(args, i);
    if (!value) continue;
    if (value->operand.tag == Operand_Tag_Label) {
      if (bytes.label_offset_in_instruction != INSTRUCTION_BYTES_NO_LABEL) {
        program_error_builder(context, args_token->source_range) {
          program_error_append_literal("inline_machine_code_bytes only supports one label");
        }
        goto err;
      }
      bytes.label_index = value->operand.Label.index;
      bytes.label_offset_in_instruction = u64_to_u8(i);
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
    } else {
      if (!descriptor_is_integer(value->descriptor)) {
        program_error_builder(context, args_token->source_range) {
          program_error_append_literal("inline_machine_code_bytes expects arguments to be integers");
        }
        goto err;
      }
      if (!operand_is_immediate(&value->operand)) {
        program_error_builder(context, args_token->source_range) {
          program_error_append_literal("inline_machine_code_bytes expects arguments to be compile-time known");
        }
        goto err;
      }
      s64 byte = operand_immediate_value_up_to_s64(&value->operand);
      if (!u64_fits_into_u8(byte)) {
        program_error_builder(context, args_token->source_range) {
          program_error_append_literal("Expected integer between 0 and 255, got ");
          program_error_append_number("%" PRIi64, byte);
        }
        goto err;
      }
      bytes.memory[bytes.length++] = s64_to_u8(byte);
    }
  }

  push_instruction(
    &context->builder->code_block.instructions, &id_token->source_range,
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
    program_error_builder(context, define->source_range) {
      program_error_append_literal("Could not find type");
    }
    goto err;
  }
  Value *value = reserve_stack(context->allocator, context->builder, descriptor);
  scope_define_value(context->scope, name->source, value);
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
  if (descriptor_is_integer(value->descriptor) && operand_is_immediate(&value->operand)) {
    value = value_from_s64(context->allocator, operand_immediate_value_up_to_s64(&value->operand));
  } else if (
    value->descriptor->tag == Descriptor_Tag_Opaque &&
    operand_is_immediate(&value->operand)
  ) {
    panic("TODO decide how to handle opaque types");
  }
  Value *on_stack = reserve_stack(context->allocator, context->builder, value->descriptor);
  move_value(context->allocator, context->builder, &name->source_range, on_stack, value);

  scope_define_value(context->scope, name->source, on_stack);
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
    if (token_parse_constant_definitions(context, statement, 0)) {
      continue;
    }

    // Report unmatched statements
    Source_Range source_range = source_range_from_token_view(statement);
    program_error_builder(context, source_range) {
      program_error_append_literal("Could not parse a top level statement");
    }
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

