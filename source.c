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
    case Token_Tag_Newline:
    case Token_Tag_Integer:
    case Token_Tag_Hex_Integer:
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

Array_Scope_Entry *
scope_lookup(
  Scope *scope,
  Slice name
) {
  while (scope) {
    Array_Scope_Entry *result = hash_map_get(scope->map, name);
    if (result) return result;
    scope = scope->parent;
  }
  return 0;
}

Value *
token_rewrite_constant_expression(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
);

void
token_force_value(
  Compilation_Context *context,
  const Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
);

Value *
scope_lookup_force(
  Compilation_Context *context,
  Scope *scope,
  Slice name
) {
  Array_Scope_Entry *entries = 0;
  while (scope) {
    entries = hash_map_get(scope->map, name);
    if (entries) break;
    scope = scope->parent;
  }
  if (!entries) {
    return 0;
  }
  assert(dyn_array_length(*entries));

  // Force lazy entries
  for (u64 i = 0; i < dyn_array_length(*entries); ++i) {
    Scope_Entry *entry = dyn_array_get(*entries, i);
    if (entry->type == Scope_Entry_Type_Lazy_Constant_Expression) {
      Scope_Lazy_Constant_Expression *expr = &entry->lazy_constant_expression;
      Value *result = token_rewrite_constant_expression(context, expr->tokens, expr->scope);
      *entry = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .value = result,
      };
    }
  }

  Value *result = 0;
  for (u64 i = 0; i < dyn_array_length(*entries); ++i) {
    Scope_Entry *entry = dyn_array_get(*entries, i);
    assert(entry->type == Scope_Entry_Type_Value);

    // To support recursive functions without a hack like `self` we
    // do forcing of the lazy value in two steps. First creates a valid Value
    // the second one, here, actually processes function body
    if (entry->value && entry->value->descriptor->type == Descriptor_Type_Function) {
      Descriptor_Function *function = &entry->value->descriptor->function;
      if (function->flags & Descriptor_Function_Flags_Pending_Body_Compilation) {
        function->flags &= ~Descriptor_Function_Flags_Pending_Body_Compilation;
        Token *body = token_clone_deep(context->allocator, function->body);

        Value *return_result_value =
          function->returns->descriptor->type == Descriptor_Type_Void
          ? value_any(context->allocator)
          : function->returns;
        token_parse_block(context, body, function->scope, function->builder, return_result_value);
        fn_end(function->builder);
      }
    }

    if (!result) {
      result = entry->value;
    } else {
      if (entry->value->descriptor->type != Descriptor_Type_Function) {
        panic("Only functions support overloading");
      }
      Value *overload = entry->value;
      overload->descriptor->function.next_overload = result;
      result = overload;
    }
  }

  // For functions we need to gather up overloads from all parent scopes
  if (result && result->descriptor->type == Descriptor_Type_Function) {
    Value *last = result;
    Scope *parent = scope;
    for (;;) {
      parent = parent->parent;
      if (!parent) break;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(context, parent, name);
      if (!overload) panic("Just checked that hash map has the name so lookup must succeed");
      if (overload->descriptor->type != Descriptor_Type_Function) {
        panic("There should only be function overloads");
      }
      while (last->descriptor->function.next_overload) {
        last = last->descriptor->function.next_overload;
      }
      last->descriptor->function.next_overload = overload;
    };
  }
  return result;
}

static inline void
scope_define_internal(
  Scope *scope,
  Slice name,
  Scope_Entry entry
) {
  if (!hash_map_has(scope->map, name)) {
    hash_map_set(scope->map, name, dyn_array_make(Array_Scope_Entry));
  }
  Array_Scope_Entry *entries = hash_map_get(scope->map, name);
  dyn_array_push(*entries, entry);
}

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
) {
  scope_define_internal(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .value = value,
  });
}

void
scope_define_lazy(
  Scope *scope,
  Slice name,
  Token_View tokens
) {
  scope_define_internal(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Lazy_Constant_Expression,
    .lazy_constant_expression = {
      .tokens = tokens,
      .scope = scope,
    },
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

Tokenizer_Result
tokenize(
  Allocator *allocator,
  Source_File *file
) {
  Array_Const_Token_Ptr parent_stack = dyn_array_make(Array_Const_Token_Ptr);
  Token *root = &(Token){0};
  root->Group.children = dyn_array_make(Array_Const_Token_Ptr);

  assert(!dyn_array_is_initialized(file->lines));
  file->lines = dyn_array_make(Array_Range_u64);

  Range_u64 current_line = {0};
  Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;
  Fixed_Buffer *string_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = 4096,
  );

  Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);

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

#define push_error(_MESSAGE_)\
  dyn_array_push(errors, (Parse_Error) {\
    .message = slice_literal(_MESSAGE_),\
    .source_range = {\
      .file = file,\
      .offsets = {.from = i, .to = i},\
    }\
  })
#define push_line()\
  do {\
    current_line.to = i + 1;\
    dyn_array_push(file->lines, current_line);\
    current_line.from = current_line.to;\
  } while(0)

  u64 i = 0;
  for (; i < file->text.length; ++i) {
    s8 ch = file->text.bytes[i];
    s8 peek = i + 1 < file->text.length ? file->text.bytes[i + 1] : 0;

    retry: switch(state) {
      case Tokenizer_State_Default: {
        if (ch == '\n') {
          start_token(Token_Tag_Newline);
          push_line();
          accept_and_push;
        } else if (ch == '\r') {
          start_token(Token_Tag_Newline);
          if (peek == '\n') i++;
          push_line();
          accept_and_push;
        } else if (isspace(ch)) {
          continue;
        } else if (ch == '0' && peek == 'x') {
          start_token(Token_Tag_Hex_Integer);
          i++;
          state = Tokenizer_State_Hex_Integer;
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
              expected_paren = '}';
              break;
            }
            case Token_Group_Type_Square: {
              expected_paren = ']';
              break;
            }
          }
          if (ch != expected_paren) {
            push_error("Mismatched closing brace");
            goto end;
          }
          parent->source_range.offsets.to = i + 1;
          parent->source = slice_sub_range(file->text, parent->source_range.offsets);
          if (!dyn_array_length(parent_stack)) {
            push_error("Encountered a closing brace without a matching open one");
            goto end;
          }
          parent = (Token *)*dyn_array_pop(parent_stack);
          current_token = 0;
        } else {
          push_error("Unpexpected input");
          goto end;
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
          u8 *string = allocator_allocate_bytes(allocator, string_buffer->occupied, 1);
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
  dyn_array_push(file->lines, current_line);

  if (parent != root) {
    push_error("Unexpected end of file. Expected a closing brace.");
  }
  // current_token can be null in case of an empty input
  if (current_token) {
    // Strings need to be terminated with a '"'
    if (state == Tokenizer_State_String) {
      push_error("Unexpected end of file. Expected a \".");
    } else {
      accept_and_push;
    }
  }
  end:
#undef push_error
#undef start_token
#undef push_and_retry
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(parent_stack);
  if (dyn_array_length(errors)) {
    return (Tokenizer_Result){
      .tag = Tokenizer_Result_Tag_Error,
      .Error = { errors },
    };
  }
  return (Tokenizer_Result){
    .tag = Tokenizer_Result_Tag_Success,
    .Success = {root->Group.children}
  };
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

bool
token_match(
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

Array_Token_View
token_split(
  Token_View view,
  const Token_Pattern *separator
) {
  Array_Token_View result = dyn_array_make(Array_Token_View);

  u64 start_index = 0;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    if (token_match(token, separator)) {
      token_view_array_push(&result, (Token_View) {
        .tokens = view.tokens + start_index,
        .length = i - start_index,
      });
      start_index = i + 1;
    }
  }
  token_view_array_push(&result, token_view_rest(view, start_index));
  return result;
}

Array_Token_View
token_split_by_newlines_and_semicolons(
  Token_View view
) {
  Array_Token_View result = dyn_array_make(Array_Token_View);

  u64 start_index = 0;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);
    if (
      token->tag == Token_Tag_Newline ||
      (token->tag == Token_Tag_Operator && slice_equal(token->source, slice_literal(";")))
    ) {
      if (start_index != i) {
        assert(i > start_index);
        token_view_array_push(&result, (Token_View) {
          .tokens = view.tokens + start_index,
          .length = i - start_index,
        });
      }
      start_index = i + 1;
    }
  }
  token_view_array_push(&result, (Token_View) {
    .tokens = view.tokens + start_index,
    .length = view.length - start_index,
  });
  return result;
}

Descriptor *
scope_lookup_type(
  Compilation_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice type_name
) {
  Value *value = scope_lookup_force(context, scope, type_name);
  if (!value) return 0;
  if (value->descriptor->type != Descriptor_Type_Type) {
    program_error_builder(context, source_range) {
      program_error_append_slice(type_name);
      program_error_append_literal(" is not a type");
    }
    return 0;
  }
  Descriptor *descriptor = value->descriptor->type_descriptor;
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
  Slice arg_name;
  Descriptor *type_descriptor;
} Token_Match_Arg;

Descriptor *
token_force_type(
  Compilation_Context *context,
  Scope *scope,
  const Token *token
) {
  Descriptor *descriptor = 0;
  switch (token->tag) {
    case Token_Tag_Id: {
      descriptor = scope_lookup_type(context, scope, token->source_range, token->source);
      if (!descriptor) {
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Could not find type ");
          program_error_append_slice(token->source);
        }
        return 0;
      }
      break;
    }
    case Token_Tag_Group: {
      if (token->Group.type != Token_Group_Type_Square) {
        panic("TODO");
      }
      if (dyn_array_length(token->Group.children) != 1) {
        program_push_error_from_slice(
          context->program,
          token->source_range,
          slice_literal("Pointer type must have a single type inside")
        );
        return 0;
      }
      const Token *child = *dyn_array_get(token->Group.children, 0);
      if (child->tag != Token_Tag_Id) {
        panic("TODO: should be recursive");
      }
      descriptor = allocator_allocate(context->allocator, Descriptor);
      *descriptor = (Descriptor) {
        .type = Descriptor_Type_Pointer,
        .pointer_to = scope_lookup_type(context, scope, child->source_range, child->source),
      };
      break;
    }
    case Token_Tag_Hex_Integer:
    case Token_Tag_Integer: {
      program_error_builder(context, token->source_range) {
        program_error_append_slice(token->source);
        program_error_append_literal(" is not a type");
      }
      return 0;
    }
    case Token_Tag_Newline: {
      program_push_error_from_slice(
        context->program,
        token->source_range,
        slice_literal("Unexpected newline token")
      );
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

typedef Array_Const_Token_Ptr (*token_pattern_callback)(
  Compilation_Context *context,
  Token_View match,
  Scope *scope,
  Function_Builder *builder_
);

bool
token_match_pattern(
  Token_View view,
  Array_Token_Pattern pattern_array
) {
  u64 pattern_length = dyn_array_length(pattern_array);
  if (!pattern_length) panic("Zero-length pattern does not make sense");
  for (u64 i = 0; i < pattern_length; ++i) {
    Token_Pattern *pattern = dyn_array_get(pattern_array, i);
    const Token *token = token_peek_match(view, i, pattern);
    if (!token) {
      return false;
    }
  }
  return true;
}

Array_Const_Token_Ptr
token_apply_macro_replacements(
  Compilation_Context *context,
  Macro_Replacement_Map *map,
  Array_Const_Token_Ptr source
) {
  Array_Const_Token_Ptr result = dyn_array_make(Array_Const_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    const Token *token = *dyn_array_get(source, i);
    Token *copy = allocator_allocate(context->allocator, Token);
    *copy = *token;
    switch (token->tag) {
      case Token_Tag_Id: {
        Slice name = token->source;
        const Token **replacement_ptr = hash_map_get(map, name);
        if (replacement_ptr) {
          *copy = **replacement_ptr;
        }
        break;
      }
      case Token_Tag_Newline:
      case Token_Tag_Integer:
      case Token_Tag_Hex_Integer:
      case Token_Tag_Operator:
      case Token_Tag_String: {
        // Nothing to do
        break;
      }
      case Token_Tag_Group: {
        copy->Group.type = token->Group.type;
        copy->Group.children =
          token_apply_macro_replacements(context, map, token->Group.children);
        break;
      }
      case Token_Tag_Value: {
        panic("Macro definitions should not contain semi-resolved tokens");
        break;
      }
    }
    dyn_array_push(result, copy);
  }
  return result;
}

Array_Const_Token_Ptr
token_rewrite_macro_match(
  Compilation_Context *context,
  Token_View match,
  Macro *macro
) {
  Macro_Replacement_Map *map = hash_map_make(Macro_Replacement_Map);
  if (dyn_array_length(macro->pattern_names) != match.length) {
    panic("Should not have chosen the macro if pattern length do not match");
  }
  for (u64 i = 0; i < dyn_array_length(macro->pattern_names); ++i) {
    Slice *name = dyn_array_get(macro->pattern_names, i);
    if (name->length) {
      hash_map_set(map, *name, token_view_get(match, i));
    }
  }
  Array_Const_Token_Ptr replacement = token_apply_macro_replacements(context, map, macro->replacement);
  hash_map_destroy(map);
  return replacement;
}

void
token_rewrite_macros(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Scope *scope,
  Function_Builder *builder
) {
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);

      start: for (;;) {
        for (u64 i = 0; i < dyn_array_length(*tokens); ++i) {
          Token_View sub_view = token_view_rest(token_view_from_token_array(*tokens), i);
          if (token_match_pattern(sub_view, macro->pattern)) {
            Array_Const_Token_Ptr replacement = token_rewrite_macro_match(context, sub_view, macro);
            dyn_array_splice(*tokens, i, dyn_array_length(macro->pattern), replacement);
            dyn_array_destroy(replacement);
            goto start;
          }
        }
        break;
      }
    }
  }
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
);

Descriptor *
token_match_type(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  Descriptor *descriptor = token_match_fixed_array_type(context, view, scope);
  if (descriptor) return descriptor;
  if (!view.length) panic("Caller must not call token_match_type with empty token list");
  const Token *token = token_view_get(view, 0);
  if (view.length > 1) {
    program_push_error_from_slice(
      context->program,
      token->source_range,
      slice_literal("Can not resolve type")
    );
    return 0;
  }
  return token_force_type(context, scope, token);
}

static inline Token_View
token_view_trim_newlines(
  Token_View view
) {
  const Token **tokens = view.tokens;
  u64 length = view.length;
  while (length) {
    if (tokens[0]->tag == Token_Tag_Newline) {
      tokens++;
      length--;
      continue;
    } else if (tokens[length - 1]->tag == Token_Tag_Newline) {
      length--;
      continue;
    }
    break;
  }
  return (Token_View) {
    .tokens = tokens,
    .length = length,
  };
}

Token_Match_Arg *
token_match_argument(
  Compilation_Context *context,
  Token_View raw_view,
  Scope *scope
) {
  // FIXME take care of this in proper parser
  Token_View view = token_view_trim_newlines(raw_view);
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_rest(view, peek_index);
  Descriptor *type_descriptor = token_match_type(context, rest, scope);
  if (!type_descriptor) return 0;
  Token_Match_Arg *arg = allocator_allocate(context->allocator, Token_Match_Arg);
  *arg = (Token_Match_Arg){name->source, type_descriptor};
  return arg;
}

bool
token_match_expression(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Scope *scope,
  Function_Builder *builder,
  Value *target
);

Value *
value_from_integer_token(
  Compilation_Context *context,
  Scope *scope,
  const Token *token
) {
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
  Scope *scope,
  const Token *token
) {
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

Value *
token_force_constant_value(
  Compilation_Context *context,
  Scope *scope,
  const Token *token
) {
  switch(token->tag) {
    case Token_Tag_Integer: {
      return value_from_integer_token(context, scope, token);
    }
    case Token_Tag_Hex_Integer: {
      return value_from_hex_integer_token(context, scope, token);
    }
    case Token_Tag_String: {
      Slice string = token->String.slice;
      Value *string_bytes = value_global_c_string_from_slice(context, string);
      // TODO make a proper string type
      return string_bytes;
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name);
      if (!value) {
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
        return token_rewrite_constant_expression(context, children_view, scope);
      } else {
        panic("TODO support other group types in constant context");
      }
      return 0;
    }
    case Token_Tag_Operator: {
      panic("TODO support operator lookup in constant context");
      return 0;
    }
    case Token_Tag_Newline: {
      program_push_error_from_slice(
        context->program,
        token->source_range,
        slice_literal("Unexpected newline token")
      );
      return 0;
    }
  }
  panic("Internal Error: Unknown token type");
  return 0;
}

void
token_force_value(
  Compilation_Context *context,
  const Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  switch(token->tag) {
    case Token_Tag_Integer: {
      Value *immediate = value_from_integer_token(context, scope, token);
      move_value(context->allocator, builder, &token->source_range, result_value, immediate);
      return;
    }
    case Token_Tag_Hex_Integer: {
      Value *immediate = value_from_hex_integer_token(context, scope, token);
      move_value(context->allocator, builder, &token->source_range, result_value, immediate);
      return;
    }
    case Token_Tag_String: {
      Slice string = token->String.slice;
      Value *string_bytes = value_global_c_string_from_slice(context, string);
      Value *c_string_pointer = value_pointer_to(context, builder, &token->source_range, string_bytes);
      move_value(context->allocator, builder, &token->source_range, result_value, c_string_pointer);
      return;
    }
    case Token_Tag_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name);
      if (!value) {
        program_error_builder(context, token->source_range) {
          program_error_append_literal("Undefined variable ");
          program_error_append_slice(name);
        }
      } else {
        move_value(context->allocator, builder, &token->source_range, result_value, value);
      }
      return;
    }
    case Token_Tag_Value: {
      if (token->Value.value) {
        move_value(
          context->allocator, builder, &token->source_range, result_value, token->Value.value
        );
      } else {
        // TODO consider what should happen here
      }
      return;
    }
    case Token_Tag_Group: {
      if (!builder) panic("Caller should only force (...) in a builder context");
      switch(token->Group.type) {
        case Token_Group_Type_Paren: {
          Array_Const_Token_Ptr expression_tokens =
            dyn_array_copy(Array_Const_Token_Ptr, token->Group.children);
          token_match_expression(context, &expression_tokens, scope, builder, result_value);
          dyn_array_destroy(expression_tokens);
          return;
        }
        case Token_Group_Type_Curly: {
          token_parse_block(context, token, scope, builder, result_value);
          return;
        }
        case Token_Group_Type_Square: {
          panic("TODO");
          return;
        }
      }
      break;
    }

    case Token_Tag_Operator: {
      panic("TODO");
      return;
    }
    case Token_Tag_Newline: {
      program_push_error_from_slice(
        context->program,
        token->source_range,
        slice_literal("Unexpected newline token")
      );
      return;
    }
  }
  panic("Not reached");
  return;
}


// FIXME pass in the function definition
Array_Value_Ptr
token_match_call_arguments(
  Compilation_Context *context,
  const Token *token,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (dyn_array_length(token->Group.children) != 0) {
    Token_View children = token_view_from_token_array(token->Group.children);
    Array_Token_View argument_states = token_split(children, &(Token_Pattern){
      .tag = Token_Tag_Operator,
      .source = slice_literal(","),
    });
    // TODO :TargetValue
    // There is an interesting conundrum here that we need to know the types of the
    // arguments for overload resolution, but then we need the exact function definition
    // to know the result_value definition to do the evaluation. Proper solution would
    // be to introduce :TypeOnlyEvalulation, but for now we will just create a special
    // target value that can be anything that will behave like type inference and is
    // needed regardless for something like x := (...)
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_View view = *dyn_array_get(argument_states, i);
      Value *result_value = value_any(context->allocator);
      Array_Const_Token_Ptr expression_tokens = token_array_from_view(allocator_system, view);
      token_match_expression(context, &expression_tokens, scope, builder, result_value);
      dyn_array_destroy(expression_tokens);
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
  Token *result_token = allocator_allocate(context->allocator, Token);
  *result_token = (Token){
    .tag = Token_Tag_Value,
    .source_range = source_range,
    .source = slice_sub_range(source_range.file->text, source_range.offsets),
    .Value = { result },
  };
  return result_token;
}

bool
token_state_clear_newlines(
  Array_Const_Token_Ptr *tokens
) {
  for (u64 i = 0; i < dyn_array_length(*tokens); ++i) {
    const Token *token = *dyn_array_get(*tokens, i);
    if (token->tag == Token_Tag_Newline) {
      dyn_array_delete(*tokens, i);
      --i;
    }
  }
  return true;
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
token_rewrite_macro_definitions(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id, .source = slice_literal("macro"));
  Token_Match(pattern_token, .group_type = Token_Group_Type_Paren);
  Token_Match(replacement_token, .group_type = Token_Group_Type_Paren);

  Array_Token_Pattern pattern = dyn_array_make(Array_Token_Pattern);
  Array_Slice pattern_names = dyn_array_make(Array_Slice);

  for (u64 i = 0; i < dyn_array_length(pattern_token->Group.children); ++i) {
    const Token *token = *dyn_array_get(pattern_token->Group.children, i);
    if (token->tag == Token_Tag_Id && slice_starts_with(token->source, slice_literal("_"))) {
      Slice pattern_name = slice_sub(token->source, 1, token->source.length);
      dyn_array_push(pattern_names, pattern_name);
      dyn_array_push(pattern, (Token_Pattern) {0});
    } else {
      dyn_array_push(pattern_names, (Slice){0});
      if (token->tag == Token_Tag_Id) {
        dyn_array_push(pattern, (Token_Pattern) {
          .tag = token->tag,
          .source = token->source,
        });
      } else {
        dyn_array_push(pattern, (Token_Pattern) {
          .tag = token->tag,
        });
      }
    }
  }

  Macro *macro = allocator_allocate(context->allocator, Macro);
  *macro = (Macro){
    .pattern = pattern,
    .pattern_names = pattern_names,
    .replacement = replacement_token->Group.children,
  };

  scope_add_macro(scope, macro);

  return true;
}

bool
token_match_struct_field(
  Compilation_Context *context,
  Descriptor *struct_descriptor,
  Token_View view,
  Scope *scope
) {
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_rest(view, peek_index);
  Descriptor *descriptor = token_match_type(context, rest, scope);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, name->source);
  return true;
}

Token *
token_process_type_definition(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  const Token *layout_token
) {
  const Token *layout_block = 0;
  Value *bit_size_value = 0;

  Token_View children = token_view_from_token_array(layout_token->Group.children);
  Array_Token_View argument_states = token_split(children, &(Token_Pattern){
    .tag = Token_Tag_Operator,
    .source = slice_literal(","),
  });

  switch(dyn_array_length(argument_states)) {
    case 1: {
      Token_View bit_size_view = *dyn_array_get(argument_states, 0);
      bit_size_value = token_rewrite_constant_expression(context, bit_size_view, scope);
      if (!bit_size_value) {
        // TODO print error
        goto err;
      }
      break;
    }
    case 2: {
      Token_View layout_type_view = *dyn_array_get(argument_states, 0);
      if (layout_type_view.length != 1) {
        // TODO print error
        goto err;
      }
      const Token *layout_type = token_view_get(layout_type_view, 0);
      if (
        layout_type->tag == Token_Tag_Id &&
        slice_equal(layout_type->source, slice_literal("c_struct"))
      ) {
        Token_View layout_block_state = *dyn_array_get(argument_states, 1);
        if (layout_block_state.length != 1) {
          // TODO print error
          goto err;
        }
        layout_block = token_view_get(layout_block_state, 0);
      } else {
        // TODO print error
        goto err;
      }
      break;
    }
    default: {
      // TODO print error
      goto err;
    }
  }

  assert(layout_block || bit_size_value);
  Value *result = allocator_allocate(context->allocator, Value);

  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  if (bit_size_value) {
    if (!descriptor_is_integer(bit_size_value->descriptor)) {
      // TODO err
      goto err;
    }
    if (!operand_is_immediate(&bit_size_value->operand)) {
      // TODO err
      goto err;
    }
    u64 bit_size = s64_to_u64(operand_immediate_as_s64(&bit_size_value->operand));
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Opaque,
      .opaque = { .bit_size = bit_size },
    };
  } else {
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Struct,
      .struct_ = {
        .fields = dyn_array_make(Array_Descriptor_Struct_Field),
      },
    };

    if (dyn_array_length(layout_block->Group.children) != 0) {
      Token_View layout_block_children =
        token_view_from_token_array(layout_block->Group.children);
      Array_Token_View definitions = token_split_by_newlines_and_semicolons(layout_block_children);
      for (u64 i = 0; i < dyn_array_length(definitions); ++i) {
        Token_View field_view = *dyn_array_get(definitions, i);
        token_match_struct_field(context, descriptor, field_view, scope);
      }
      dyn_array_destroy(definitions);
    }
  }

  Descriptor *value_descriptor = allocator_allocate(context->allocator, Descriptor);
  *value_descriptor = (Descriptor) {
    .type = Descriptor_Type_Type,
    .type_descriptor = descriptor,
  };
  *result = (Value) {
    .descriptor = value_descriptor,
    .operand = {.type = Operand_Type_None },
  };
  return token_value_make(context, result, layout_token->source_range);

  err:
  return 0;
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

Token *
token_import_match_arguments(
  Source_Range source_range,
  Token_View view,
  Compilation_Context *context
) {
  u64 peek_index = 0;
  const Token *library_name_token = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_String,
  });
  if (!library_name_token) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("First argument to external() must be a literal string")
    );
    return 0;
  }
  const Token *comma = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_Operator,
    .source = slice_literal(","),
  });
  if (!comma) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("external(\"library_name\", \"symbol_name\") requires two arguments")
    );
    return 0;
  }
  const Token *symbol_name_token = token_peek_match(view, peek_index++, &(Token_Pattern) {
    .tag = Token_Tag_String,
  });
  if (!symbol_name_token) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("Second argument to external() must be a literal string")
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
  Scope *,
  Function_Builder *,
  Value *result_value,
  u64 *replacement_count
);

void
token_rewrite_expression(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  token_rewrite_expression_callback callback
) {
  // FIXME speed
  Array_Const_Token_Ptr replacement = dyn_array_make(Array_Const_Token_Ptr);
  start: for (;;) {
    for (u64 i = 0; i < dyn_array_length(*tokens); ++i) {
      Token_View rewrite_view = token_view_rest(token_view_from_token_array(*tokens), i);
      u64 replacement_count = 0;
      const Token *token = callback(context, rewrite_view, scope, builder, result_value, &replacement_count);
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
  bool is_inline,
  const Token *args,
  const Token *return_types,
  const Token *body
) {
  Scope *function_scope = scope_make(context->allocator, context->program->global_scope);

  Function_Builder *builder = 0;
  Descriptor *descriptor = 0;

  if(!body) return 0;

  // TODO think about a better way to distinguish imports
  bool is_external = body->tag == Token_Tag_Value;

  if (is_external) {
    if (is_inline) {
      program_error_builder(context, body->source_range) {
        program_error_append_literal("External functions can not be inline");
      }
      is_inline = false;
    }
    if(!body->Value.value) return 0;
    descriptor = allocator_allocate(context->allocator, Descriptor);
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Function,
      .function = {
        .arguments = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator),
        .argument_names = dyn_array_make(Array_Slice, .allocator = context->allocator),
        .returns = 0,
      },
    };
  } else {
    builder = fn_begin(context);
    descriptor = builder->descriptor;
  }

  switch (dyn_array_length(return_types->Group.children)) {
    case 0: {
      descriptor->function.returns = &void_value;
      break;
    }
    case 1: {
      const Token *return_type_token = *dyn_array_get(return_types->Group.children, 0);
      Descriptor *return_descriptor = token_force_type(context, function_scope, return_type_token);
      if (!return_descriptor) return 0;
      function_return_descriptor(context, &descriptor->function, return_descriptor);
      break;
    }
    default: {
      panic("Multiple return types are not supported at the moment");
      break;
    }
  }

  if (dyn_array_length(args->Group.children) != 0) {
    Token_View children = token_view_from_token_array(args->Group.children);
    Array_Token_View argument_states = token_split(children, &(Token_Pattern){
      .tag = Token_Tag_Operator,
      .source = slice_literal(","),
    });
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_View arg_view = *dyn_array_get(argument_states, i);
      Token_Match_Arg *arg = token_match_argument(context, arg_view, function_scope);
      if (!arg) return 0;
      Value *arg_value =
        function_push_argument(context->allocator, &descriptor->function, arg->type_descriptor);
      if (!is_external && arg_value->operand.type == Operand_Type_Register) {
        register_bitset_set(&builder->code_block.register_occupied_bitset, arg_value->operand.reg);
      }
      dyn_array_push(descriptor->function.argument_names, arg->arg_name);
      scope_define_value(function_scope, arg->arg_name, arg_value);
    }
    assert(
      dyn_array_length(descriptor->function.argument_names) ==
      dyn_array_length(descriptor->function.arguments)
    );
  }

  Value *result = 0;
  if (is_external) {
    body->Value.value->descriptor = descriptor;
    result = body->Value.value;
  } else {
    descriptor->function.scope = function_scope;
    descriptor->function.body = body;
    descriptor->function.builder = builder;
    descriptor->function.flags |= Descriptor_Function_Flags_Pending_Body_Compilation;
    if (is_inline) {
      descriptor->function.flags |= Descriptor_Function_Flags_Inline;
    }
    result = builder->value;
  }
  return result;
}

const Token *
token_rewrite_negative_literal(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *unused_result_value, // FIXME actually use this
  u64 *replacement_count
) {
  u64 peek_index = 0;
  // FIXME Allow unary minus on any expression
  Token_Match_Operator(define, "-");
  Token_Match(integer, .tag = Token_Tag_Integer);
  Value *result = value_any(context->allocator);
  token_force_value(context, integer, scope, builder, result);
  if (result->operand.type == Operand_Type_Immediate_8) {
    result->operand.s8 = -result->operand.s8;
  } else if (result->operand.type == Operand_Type_Immediate_16) {
    result->operand.s16 = -result->operand.s16;
  } else if (result->operand.type == Operand_Type_Immediate_32) {
    result->operand.s32 = -result->operand.s32;
  } else if (result->operand.type == Operand_Type_Immediate_64) {
    result->operand.s64 = -result->operand.s64;
  } else {
    panic("Internal error, expected an immediate");
  }

  *replacement_count = 2;
  Token *value_token = token_value_make(context, result, source_range_from_token_view(view));
  return value_token;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

Token *
compile_time_eval(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  const Token *first_token = token_view_get(view, 0);

  Program eval_program = {
    .import_libraries = dyn_array_copy(Array_Import_Library, context->program->import_libraries),
    // FIXME this is probably broken now because code ones should point to a different section
    .labels = dyn_array_copy(Array_Label, context->program->labels),
    .patch_info_array =
      dyn_array_copy(Array_Label_Location_Diff_Patch_Info, context->program->patch_info_array),
    .functions = dyn_array_copy(Array_Function_Builder, context->program->functions),
    .global_scope = scope_make(context->allocator, context->program->global_scope),
    .errors = dyn_array_make(Array_Parse_Error),
    .data_section = context->program->data_section,
    .code_section = {
      .buffer = bucket_buffer_make(.allocator = allocator_system),
      .permissions = Section_Permissions_Execute,
    }
  };

  Compilation_Context eval_context = *context;
  eval_context.program = &eval_program;

  Function_Builder *eval_builder = fn_begin(&eval_context);
  function_return_descriptor(context, &eval_builder->descriptor->function, &descriptor_void);

  Array_Const_Token_Ptr tokens = token_array_from_view(allocator_system, view);
  Value *expression_result_value = value_any(context->allocator);
  token_match_expression(
    &eval_context, &tokens, eval_program.global_scope, eval_builder, expression_result_value
  );

  // We use a something like a C++ reference out parameter for the
  // result to have a consitent function signature on this C side of things.

  // Make it out parameter a pointer to ensure it is passed inside a register according to ABI
  Value *arg_value = function_push_argument(
    context->allocator,
    &eval_builder->value->descriptor->function,
    descriptor_pointer_to(context->allocator, expression_result_value->descriptor)
  );

  // Create a reference Value
  assert(arg_value->operand.type == Operand_Type_Register);
  Value *out_value = allocator_allocate(context->allocator, Value);
  *out_value = (Value) {
    .descriptor = expression_result_value->descriptor,
    .operand = (Operand){
      .type = Operand_Type_Memory_Indirect,
      .byte_size = expression_result_value->operand.byte_size,
      .indirect = { .reg = arg_value->operand.reg },
    },
  };

  move_value(context->allocator, eval_builder, &first_token->source_range, out_value, expression_result_value);
  fn_end(eval_builder);

  program_jit(&eval_context);

  u32 result_byte_size = out_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(context->allocator, result_byte_size, alignment);

  Compile_Time_Eval_Proc jitted_code =
    (Compile_Time_Eval_Proc)value_as_function(&eval_program, eval_builder->value);

  jitted_code(result);
  Value *token_value = allocator_allocate(context->allocator, Value);
  *token_value = (Value) {
    .descriptor = out_value->descriptor,
  };
  switch(out_value->descriptor->type) {
    case Descriptor_Type_Void: {
      token_value->operand = (Operand){0};
      break;
    }
    case Descriptor_Type_Any: {
      panic("Internal Error: We should never get Any type from comp time eval");
      break;
    }
    case Descriptor_Type_Tagged_Union:
    case Descriptor_Type_Fixed_Size_Array:
    case Descriptor_Type_Pointer:
    case Descriptor_Type_Struct: {
      panic("TODO move to data section or maybe we should allocate from there right away above?");
      break;
    };
    case Descriptor_Type_Opaque: {
      if (descriptor_is_integer(out_value->descriptor)) {
        switch (result_byte_size) {
          case 8: {
            token_value->operand = imm64(*(s64 *)result);
            break;
          }
          case 4: {
            token_value->operand = imm32(*(s32 *)result);
            break;
          }
          case 2: {
            token_value->operand = imm16(*(s16 *)result);
            break;
          }
          case 1: {
            token_value->operand = imm8(*(s8 *)result);
            break;
          }
          default: {
            panic("Unsupported immediate size");
            break;
          }
        }
      }
      break;
    }
    case Descriptor_Type_Function:
    case Descriptor_Type_Type: {
      panic("TODO figure out how that works");
      break;
    }
  }
  dyn_array_destroy(tokens);
  return token_value_make(context, token_value, source_range_from_token_view(view));
}

u32
precedence_for_operator(
  Slice operator
) {
  assert(operator.length);
  if (slice_equal(operator, slice_literal("()"))) {
    return 20;
  } else if (slice_equal(operator, slice_literal("@"))) {
    return 15;
  } else if (slice_equal(operator, slice_literal("unary -"))) {
    return 10;
  } else {
    return 1;
  }
}

void
token_do_handle_operator(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Array_Const_Token_Ptr *token_stack,
  Array_Slice *operator_stack,
  Slice operator
) {
  if (slice_equal(operator, slice_literal("unary -"))) {
    const Token *token = *dyn_array_pop(*token_stack);
    Value *value = token_force_constant_value(context, scope, token);
    if (descriptor_is_integer(value->descriptor) && operand_is_immediate(&value->operand)) {
      if (value->operand.type == Operand_Type_Immediate_8) {
        value->operand.s8 = -value->operand.s8;
      } else if (value->operand.type == Operand_Type_Immediate_16) {
        value->operand.s16 = -value->operand.s16;
      } else if (value->operand.type == Operand_Type_Immediate_32) {
        value->operand.s32 = -value->operand.s32;
      } else if (value->operand.type == Operand_Type_Immediate_64) {
        value->operand.s64 = -value->operand.s64;
      } else {
        panic("Internal error, expected an immediate");
      }
    } else {
      panic("TODO");
    }
    Token *new_token = token_value_make(context, value, token->source_range);
    dyn_array_push(*token_stack, new_token);
  } else if (slice_equal(operator, slice_literal("()"))) {
    const Token *args = *dyn_array_pop(*token_stack);
    const Token *function = *dyn_array_pop(*token_stack);

    Token *result;
    // TODO turn `external` into a compile-time function call
    if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("external"))
    ) {
      Token_View args_children = token_view_from_token_array(args->Group.children);
      result = token_import_match_arguments(args->source_range, args_children, context);
    } else if (
      function->tag == Token_Tag_Id &&
      slice_equal(function->source, slice_literal("type"))
    ) {
      result = token_process_type_definition(context, view, scope, args);
    } else {
      Token_View call_view = {
        .tokens = (const Token *[]){function, args},
        .length = 2,
      };
      result = compile_time_eval(context, call_view, scope);
    }
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("->"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    const Token *return_types = *dyn_array_pop(*token_stack);
    const Token *arguments = *dyn_array_pop(*token_stack);
    const Token **maybe_inline = dyn_array_last(*token_stack);
    bool is_inline = false;
    if (
      maybe_inline &&
      (*maybe_inline)->tag == Token_Tag_Id &&
      slice_equal((*maybe_inline)->source, slice_literal("inline"))
    ) {
      is_inline = true;
      dyn_array_pop(*token_stack);
    }
    Value *function_value = token_process_function_literal(
      context, view, scope,
      is_inline, arguments, return_types, body
    );
    Token *result = token_value_make(context, function_value, arguments->source_range);
    dyn_array_push(*token_stack, result);
  } else if (slice_equal(operator, slice_literal("@"))) {
    const Token *body = *dyn_array_pop(*token_stack);
    Token *result = 0;
    if (body->tag == Token_Tag_Group && body->Group.type == Token_Group_Type_Paren) {
      Token_View eval_view = token_view_from_token_array(body->Group.children);
      result = compile_time_eval(context, eval_view, scope);
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

void
token_handle_operator(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Array_Const_Token_Ptr *token_stack,
  Array_Slice *operator_stack,
  Slice new_operator
) {
  u32 precedence = precedence_for_operator(new_operator);
  while (
    dyn_array_length(*operator_stack) &&
    precedence_for_operator(*dyn_array_last(*operator_stack)) > precedence
  ) {
    // apply the operator on the stack
    Slice popped_operator = *dyn_array_pop(*operator_stack);
    token_do_handle_operator(
      context, view, scope,
      token_stack, operator_stack, popped_operator
    );
  }
  dyn_array_push(*operator_stack, new_operator);
}

Value *
token_rewrite_constant_expression(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  if (!view.length) {
    return &void_value;
  }
  Array_Const_Token_Ptr token_stack = dyn_array_make(Array_Const_Token_Ptr);
  Array_Slice operator_stack = dyn_array_make(Array_Slice);

  bool is_previous_an_operator = true;
  for (u64 i = 0; i < view.length; ++i) {
    const Token *token = token_view_get(view, i);

    switch(token->tag) {
      case Token_Tag_Newline: {
        continue;
      }
      case Token_Tag_Integer:
      case Token_Tag_Hex_Integer:
      case Token_Tag_String:
      case Token_Tag_Id:
      case Token_Tag_Value: {
        dyn_array_push(token_stack, token);
        // TODO figure out how to handle this better
        if (token->tag != Token_Tag_Id || !slice_equal(token->source, slice_literal("inline"))) {
          is_previous_an_operator = false;
        } else {
          is_previous_an_operator = true;
        }
        break;
      }
      case Token_Tag_Group: {
        dyn_array_push(token_stack, token);
        switch (token->Group.type) {
          case Token_Group_Type_Paren: {
            if (!is_previous_an_operator) {
              token_handle_operator(
                context, view, scope, &token_stack, &operator_stack, slice_literal("()")
              );
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
      case Token_Tag_Operator: {
        Slice operator = token->source;
        // unary minus, i.e x + -5
        if (is_previous_an_operator && slice_equal(operator, slice_literal("-"))) {
          operator = slice_literal("unary -");
        }
        token_handle_operator(
          context, view, scope, &token_stack, &operator_stack, operator
        );
        is_previous_an_operator = true;
        break;
      }
    }
  }

  while (dyn_array_length(operator_stack)) {
    Slice operator = *dyn_array_pop(operator_stack);
    token_do_handle_operator(
      context, view, scope, &token_stack, &operator_stack, operator
    );
  }

  Value *result = 0;
  if (dyn_array_length(token_stack) == 1) {
    const Token *token = *dyn_array_last(token_stack);
    result = token_force_constant_value(context, scope, token);
  } else {
    // FIXME user error
  }

  dyn_array_destroy(token_stack);
  dyn_array_destroy(operator_stack);

  return result;
}

bool
token_rewrite_constant_definitions(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  Token_View lhs;
  Token_View rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(view, slice_literal("::"), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) return false;
  const Token *name = token_view_get(lhs, 0);
  if (name->tag != Token_Tag_Id) return false;
  scope_define_lazy(scope, name->source, rhs);
  return true;
}

bool
token_parse_statement(
  Compilation_Context *context,
  Token_View view,
  const Source_Range *source_range,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
);

bool
token_parse_block(
  Compilation_Context *context,
  const Token *block,
  Scope *scope,
  Function_Builder *builder,
  Value *block_result_value
) {
  assert(block->tag == Token_Tag_Group);
  assert(block->Group.type == Token_Group_Type_Curly);
  Array_Const_Token_Ptr children = block->Group.children;
  if (!dyn_array_length(children)) return false;
  Scope *block_scope = scope_make(context->allocator, scope);

  // Newlines at the end of the block do not count as semicolons otherwise this:
  // { 42
  // }
  // is being interpreted as:
  // { 42 ; }
  while ((*dyn_array_last(children))->tag == Token_Tag_Newline) dyn_array_pop(children);

  Token_View children_view = token_view_from_token_array(children);
  Array_Token_View block_statements = token_split_by_newlines_and_semicolons(children_view);
  for (u64 i = 0; i < dyn_array_length(block_statements); ++i) {
    Token_View view = *dyn_array_get(block_statements, i);
    if (!view.length) continue;
    bool is_last_statement = i + 1 == dyn_array_length(block_statements);
    Value *result_value = is_last_statement ? block_result_value : value_any(context->allocator);

    // If result is a register we need to make sure it is acquired to avoid it being used
    // as temporary when evaluating last statement. This definitely can happen with
    // the function returns but should be safe to do all the time.
    if (is_last_statement && result_value->operand.type == Operand_Type_Register) {
      if (!register_bitset_get(builder->used_register_bitset, result_value->operand.reg)) {
        register_acquire(builder, result_value->operand.reg);
      }
    }

    token_parse_statement(context, view, &block->source_range, block_scope, builder, result_value);
  }
  return true;
}

bool
token_rewrite_statement_if(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
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

  Array_Const_Token_Ptr condition_tokens = token_array_from_view(allocator_system, condition_view);
  Value *condition_value = value_any(context->allocator);
  token_match_expression(context, &condition_tokens, scope, builder, condition_value);
  dyn_array_destroy(condition_tokens);
  if (condition_value->descriptor->type == Descriptor_Type_Any) {
    goto err;
  }

  Label_Index else_label = make_if(
    context, &builder->code_block.instructions, &keyword->source_range, condition_value
  );
  token_parse_block(context, body, scope, builder, value_any(context->allocator));
  push_instruction(
    &builder->code_block.instructions, &keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = else_label}
  );

  err:
  return true;
}

bool
token_rewrite_goto(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("goto"));
  // TODO improve error reporting here.
  Token_Match(label_name, .tag = Token_Tag_Id);
  Value *value = scope_lookup_force(context, scope, label_name->source);
  if (value) {
    if (
      value->descriptor->type == Descriptor_Type_Void &&
      value->operand.type == Operand_Type_Label_32
    ) {
      push_instruction(
        &builder->code_block.instructions, &keyword->source_range,
        (Instruction) {.assembly = {jmp, {value->operand, 0, 0}}}
      );
    } else {
      program_error_builder(context, label_name->source_range) {
        program_error_append_slice(label_name->source);
        program_error_append_literal(" is not a label");
      }
    }
  }
  return true;
}

bool
token_rewrite_explicit_return(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("return"));
  Token_View rest = token_view_rest(view, peek_index);
  bool has_return_expression = rest.length > 0;
  Value *fn_return = builder->descriptor->function.returns;

  Array_Const_Token_Ptr expression_tokens = token_array_from_view(allocator_system, rest);
  token_match_expression(context, &expression_tokens, scope, builder, fn_return);
  dyn_array_destroy(expression_tokens);

  bool is_void = fn_return->descriptor->type == Descriptor_Type_Void;
  if (!is_void && !has_return_expression) {
    program_push_error_from_slice(
      context->program, keyword->source_range,
      slice_literal("Explicit return from a non-void function requires a value")
    );
  }

  push_instruction(
    &builder->code_block.instructions,
    &keyword->source_range,
    (Instruction) {.assembly = {jmp, {label32(builder->code_block.end_label), 0, 0}}}
  );

  return true;
}

const Token *
token_rewrite_pointer_to(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *unused_result_value, // FIXME actually use this
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match_Operator(operator, "&");
  Token_Match(value_token, 0);

  Value *pointee = value_any(context->allocator);
  token_force_value(context, value_token, scope, builder, pointee);
  Value *result = value_pointer_to(context, builder, &operator->source_range, pointee);

  *replacement_count = 2;
  Token *token_value = token_value_make(context, result, source_range_from_token_view(view));
  return token_value;
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_View view,
  Scope *scope
) {
  u64 peek_index = 0;
  Token_Match(type, .tag = Token_Tag_Id);
  Token_Match(square_brace, .group_type = Token_Group_Type_Square);
  Descriptor *descriptor = scope_lookup_type(context, scope, type->source_range, type->source);

  Token_View size_view = token_view_from_token_array(square_brace->Group.children);
  Value *size_value = token_rewrite_constant_expression(context, size_view, scope);
  if (!size_value) return 0;
  if (!descriptor_is_integer(size_value->descriptor)) {
    program_push_error_from_slice(
      context->program,
      square_brace->source_range,
      slice_literal("Fixed size array size is not an integer")
    );
    return 0;
  }
  if (!operand_is_immediate(&size_value->operand)) {
    program_push_error_from_slice(
      context->program,
      square_brace->source_range,
      slice_literal("Fixed size array size must be known at compile time")
    );
    return 0;
  }
  u32 length = s64_to_u32(operand_immediate_as_s64(&size_value->operand));

  // TODO extract into a helper
  Descriptor *array_descriptor = allocator_allocate(context->allocator, Descriptor);
  *array_descriptor = (Descriptor) {
    .type = Descriptor_Type_Fixed_Size_Array,
    .array = {
      .item = descriptor,
      .length = length,
    },
  };
  return array_descriptor;
}

bool
token_rewrite_inline_machine_code_bytes(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  u64 peek_index = 0;
  Token_Match(id_token, .tag = Token_Tag_Id, .source = slice_literal("inline_machine_code_bytes"));
  // TODO improve error reporting and / or transition to compile time functions when available
  Token_Match(args_token, .group_type = Token_Group_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(context, args_token, scope, builder);

  u64 byte_count = dyn_array_length(args);
  if (byte_count > 15) {
    program_error_builder(context, args_token->source_range) {
      program_error_append_literal("Expected a maximum of 15 arguments, got ");
      program_error_append_number("%lld", byte_count);
    }
    goto err;
  }

  Fixed_Buffer *buffer = fixed_buffer_make(.allocator = context->allocator, .capacity = byte_count);
  for (u64 i = 0; i < byte_count; ++i) {
    Value *value = *dyn_array_get(args, i);
    if (!value) continue;
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
    s64 byte = operand_immediate_as_s64(&value->operand);
    if (!u64_fits_into_u8(byte)) {
      program_error_builder(context, args_token->source_range) {
        program_error_append_literal("Expected integer between 0 and 255, got ");
        program_error_append_number("%lld", byte);
      }
      goto err;
    }
    fixed_buffer_append_u8(buffer, s64_to_u8(byte));
  }

  push_instruction(
    &builder->code_block.instructions, &id_token->source_range,
    (Instruction) {
      .type = Instruction_Type_Bytes,
      .bytes = fixed_buffer_as_slice(buffer),
     }
  );

  err:
  return true;
}

const Token *
token_rewrite_cast(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *unused_result_value, // FIXME use this
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(cast, .tag = Token_Tag_Id, .source = slice_literal("cast"));
  Token_Match(value_token, .group_type = Token_Group_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(context, value_token, scope, builder);
  assert(dyn_array_length(args) == 2);
  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  assert(type->descriptor->type == Descriptor_Type_Type);

  Descriptor *cast_to_descriptor = type->descriptor->type_descriptor;
  assert(descriptor_is_integer(cast_to_descriptor));
  assert(value->descriptor->type == cast_to_descriptor->type);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *result = value;
  if (cast_to_byte_size != original_byte_size) {
    result = allocator_allocate(context->allocator, Value);
    if (cast_to_byte_size < original_byte_size) {
      *result = (Value) {
        .descriptor = cast_to_descriptor,
        .operand = value->operand,
      };
      result->operand.byte_size = cast_to_byte_size;
    } else if (cast_to_byte_size > original_byte_size) {
      result = reserve_stack(context->allocator, builder, cast_to_descriptor);
      move_value(context->allocator, builder, &cast->source_range, result, value);
    }
  }

  *replacement_count = 2;
  Token *token_value = token_value_make(context, result, source_range_from_token_view(view));
  return token_value;
}

Value *
token_match_label(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Tag_Id, .source = slice_literal("label"));

  if (peek_index != view.length) {
    const Token *extra_token = token_view_get(view, peek_index);
    program_error_builder(context, extra_token->source_range) {
      program_error_append_literal("Unexpected token");
    }
    return 0;
  }

  Label_Index label = make_label(context->program, &context->program->code_section);
  push_instruction(
    &builder->code_block.instructions, &keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = label }
  );
  Value *value = allocator_allocate(context->allocator, Value);
  *value = (Value) {
    .descriptor = &descriptor_void,
    .operand = label32(label),
  };

  return value;
}

Value *
token_parse_definition(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder
) {
  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Tag_Id);
  Token_Match_Operator(define, ":");

  Token_View rest = token_view_rest(view, peek_index);
  Value *value = token_match_label(context, rest, scope, builder);
  if (!value) {
    Descriptor *descriptor = token_match_type(context, rest, scope);
    if (!descriptor) {
      program_error_builder(context, define->source_range) {
        program_error_append_literal("Could not find type");
      }
      goto err;
    }
    value = reserve_stack(context->allocator, builder, descriptor);
  }
  scope_define_value(scope, name->source, value);
  return value;

  err:
  return 0;
}

bool
token_rewrite_definitions(
  Compilation_Context *program,
  Token_View state,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  return !!token_parse_definition(program, state, scope, builder);
}

bool
token_rewrite_definition_and_assignment_statements(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  Token_View lhs;
  Token_View rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(view, slice_literal(":="), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (lhs.length > 1) return false;
  const Token *name = token_view_get(view, 0);

  if (name->tag != Token_Tag_Id) return false;

  Value *value = value_any(context->allocator);
  Array_Const_Token_Ptr expression_tokens = token_array_from_view(allocator_system, rhs);
  token_match_expression(context, &expression_tokens, scope, builder, value);
  dyn_array_destroy(expression_tokens);

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (descriptor_is_integer(value->descriptor) && operand_is_immediate(&value->operand)) {
    value = value_from_s64(context->allocator, operand_immediate_as_s64(&value->operand));
  } else if (
    value->descriptor->type == Descriptor_Type_Opaque &&
    operand_is_immediate(&value->operand)
  ) {
    panic("TODO decide how to handle opaque types");
  }
  Value *on_stack = reserve_stack(context->allocator, builder, value->descriptor);
  move_value(context->allocator, builder, &name->source_range, on_stack, value);

  scope_define_value(scope, name->source, on_stack);
  return true;
}

const Token *
token_rewrite_array_index(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *target,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(brackets, .group_type = Token_Group_Type_Square);

  Value *array = value_any(context->allocator);
  token_force_value(context, target_token, scope, builder, array);
  Value *index_value = value_any(context->allocator);
  Array_Const_Token_Ptr index_tokens = dyn_array_copy(Array_Const_Token_Ptr, brackets->Group.children);
  token_match_expression(context, &index_tokens, scope, builder, index_value);
  dyn_array_destroy(index_tokens);
  assert(array->descriptor->type == Descriptor_Type_Fixed_Size_Array);
  assert(array->operand.type == Operand_Type_Memory_Indirect);

  Descriptor *item_descriptor = array->descriptor->array.item;
  u32 item_byte_size = descriptor_byte_size(item_descriptor);

  Value *result = allocator_allocate(context->allocator, Value);
  if (operand_is_immediate(&index_value->operand)) {
    s32 index = s64_to_s32(operand_immediate_as_s64(&index_value->operand));
    *result = (Value){
      .descriptor = item_descriptor,
      .operand = {
        .type = Operand_Type_Memory_Indirect,
        .byte_size = item_byte_size,
        .indirect = (Operand_Memory_Indirect) {
          .reg = array->operand.indirect.reg,
          .displacement = array->operand.indirect.displacement + index * item_byte_size,
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
    move_value(context->allocator, builder, &target_token->source_range, index_value_in_register, index_value);
    *result = (Value){
      .descriptor = item_descriptor,
      .operand = {
        .type = Operand_Type_Sib,
        .byte_size = item_byte_size,
        .sib = (Operand_Sib) {
          .scale = scale,
          .index = index_value_in_register->operand.reg,
          .base = array->operand.indirect.reg,
          .displacement = array->operand.indirect.displacement,
        }
      }
    };
  } else {
    panic("TODO");
  }

  *replacement_count = 2;
  Token *token_value = token_value_make(context, result, source_range_from_token_view(view));
  return token_value;
}

const Token *
token_rewrite_struct_field(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *unused_result_value, // FIXME actually use this
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(struct_token, 0);
  Token_Match_Operator(dot, ".");
  Token_Match(field_name, .tag = Token_Tag_Id);

  Value *struct_value = value_any(context->allocator);
  token_force_value(context, struct_token, scope, builder, struct_value);
  Value *result = struct_get_field(context->allocator, struct_value, field_name->source);

  *replacement_count = 3;
  Token *token_value = token_value_make(context, result, source_range_from_token_view(view));
  return token_value;
}

bool
token_rewrite_assignment(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  void *unused_payload
) {
  Token_View lhs;
  Token_View rhs;
  Token *operator;
  if (!token_maybe_split_on_operator(view, slice_literal("="), &lhs, &rhs, &operator)) {
    return false;
  }

  Value *target = token_parse_definition(context, lhs, scope, builder);
  if (!target) {
    target = value_any(context->allocator);
    Array_Const_Token_Ptr lhs_tokens = token_array_from_view(allocator_system, lhs);
    token_match_expression(context, &lhs_tokens, scope, builder, target);
    dyn_array_destroy(lhs_tokens);
  }

  Array_Const_Token_Ptr rhs_tokens = token_array_from_view(allocator_system, rhs);
  token_match_expression(context, &rhs_tokens, scope, builder, target);
  dyn_array_destroy(rhs_tokens);

  return true;
}

const Token *
token_rewrite_function_calls(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(args_token, .group_type = Token_Group_Type_Paren);
  if (
    target_token->tag != Token_Tag_Id &&
    !token_match(target_token, &(Token_Pattern){ .group_type = Token_Group_Type_Paren })
  ) {
    return false;
  }
  *replacement_count = 2;

  Value *target = value_any(context->allocator);
  token_force_value(context, target_token, scope, builder, target);

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
      if (!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
        register_acquire(builder, reg_index);
        acquired_registers[i] = true;
      }
    }

    args = token_match_call_arguments(context, args_token, scope, builder);

    // Release any registers that we fake acquired to make sure that the actual call
    // does not unnecessarily store them to stack
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      if (acquired_registers[i]) {
        Register reg_index = arg_registers[i];
        register_release(builder, reg_index);
      }
    }
  }

  if (target->descriptor->type != Descriptor_Type_Function) {
    program_error_builder(context, target_token->source_range) {
      program_error_append_slice(target_token->source);
      program_error_append_literal(" is not a function");
    }
    return false;
  }
  Token *result = 0;
  const Source_Range *source_range = &target_token->source_range;
  Value *overload = find_matching_function_overload(builder, target, args);
  if (overload) {
    Value *return_value;
    Descriptor_Function *function = &overload->descriptor->function;
    if (function->flags & Descriptor_Function_Flags_Inline) {
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

      // We need to have a fake builder so that return target label and
      // the return types are correct
      Function_Builder inline_builder = *builder;
      {
        inline_builder.code_block.end_label =
          make_label(context->program, &context->program->code_section);
        inline_builder.descriptor = &(Descriptor) {0};
        *inline_builder.descriptor = *builder->descriptor;
        inline_builder.descriptor->function.returns = result_value;
      }
      Token *body = token_clone_deep(context->allocator, function->body);
      token_parse_block(context, body, body_scope, &inline_builder, return_value);

      // Because instructions are stored in a dynamic array it might have been
      // reallocated which means we need to copy it. It might be better to
      // switch to a bucket array.
      builder->code_block.instructions = inline_builder.code_block.instructions;

      push_instruction(
        &builder->code_block.instructions,
        &target_token->source_range,
        (Instruction) {
          .type = Instruction_Type_Label,
          .label = inline_builder.code_block.end_label
        }
      );
    } else {
      return_value = call_function_overload(context, builder, source_range, overload, args);
    }
    result = token_value_make(context, return_value, source_range_from_token_view(view));
  } else {
    program_error_builder(context, target_token->source_range) {
      // TODO add better error message
      program_error_append_literal("Could not find matching overload");
    }
  }
  dyn_array_destroy(args);
  return result;
}

const Token *
token_rewrite_plus(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "+");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any(context->allocator);
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any(context->allocator);
  token_force_value(context, rhs, scope, builder, rhs_value);
  plus(context->allocator, builder, &op_token->source_range, result_value, lhs_value, rhs_value);

  *replacement_count = 3;
  Token *token_value = token_value_make(context, result_value, source_range_from_token_view(view));
  return token_value;
}

const Token *
token_rewrite_minus(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "-");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any(context->allocator);
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any(context->allocator);
  token_force_value(context, rhs, scope, builder, rhs_value);
  minus(context->allocator, builder, &op_token->source_range, result_value, lhs_value, rhs_value);

  *replacement_count = 3;
  Token *token_value = token_value_make(context, result_value, source_range_from_token_view(view));
  return token_value;
}

const Token *
token_rewrite_divide(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "/");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any(context->allocator);
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any(context->allocator);
  token_force_value(context, rhs, scope, builder, rhs_value);
  divide(context->allocator, builder, &op_token->source_range, result_value, lhs_value, rhs_value);

  *replacement_count = 3;
  Token *token_value = token_value_make(context, result_value, source_range_from_token_view(view));
  return token_value;
}

const Token *
token_rewrite_remainder(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "%");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any(context->allocator);
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any(context->allocator);
  token_force_value(context, rhs, scope, builder, rhs_value);
  Value *temp = reserve_stack(context->allocator, builder, lhs_value->descriptor);
  value_remainder(context->allocator, builder, &op_token->source_range, temp, lhs_value, rhs_value);

  *replacement_count = 3;
  Token *token_value = token_value_make(context, temp, source_range_from_token_view(view));
  return token_value;
}

const Token *
token_rewrite_compare(
  Compilation_Context *context,
  Token_View view,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  u64 *replacement_count
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match(operator, .tag = Token_Tag_Operator);
  Compare_Type compare_type = 0;
  switch(operator->source.length) {
    case 1: {
      s8 byte1 = operator->source.bytes[0];
      if (byte1 == '<') {
        compare_type = Compare_Type_Signed_Less;
      } else if (byte1 == '>') {
        compare_type = Compare_Type_Signed_Greater;
      } else {
        return 0;
      }
      break;
    }
    case 2: {
      s8 byte1 = operator->source.bytes[0];
      s8 byte2 = operator->source.bytes[1];
      if (byte1 == '<' && byte2 == '=') {
        compare_type = Compare_Type_Signed_Less_Equal;
      } else if (byte1 == '>' && byte2 == '=') {
        compare_type = Compare_Type_Signed_Greater_Equal;
      } else if (byte1 == '=' && byte2 == '=') {
        compare_type = Compare_Type_Equal;
      } else if (byte1 == '!' && byte2 == '=') {
        compare_type = Compare_Type_Equal;
      } else {
        return 0;
      }
      break;
    }
    default: {
      return 0;
    }
  }
  Token_Match(rhs, 0);

  Value *lhs_value = value_any(context->allocator);
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any(context->allocator);
  token_force_value(context, rhs, scope, builder, rhs_value);

  // FIXME add implicit unsigned to signed conversion
  if (
    !descriptor_is_integer(lhs_value->descriptor) ||
    !descriptor_is_integer(rhs_value->descriptor)
  ) {
    panic("FIXME handle errors here");
  }

  bool is_lhs_signed = descriptor_is_signed_integer(lhs_value->descriptor);
  bool is_rhs_signed = descriptor_is_signed_integer(rhs_value->descriptor);
  if (is_lhs_signed != is_rhs_signed) {
    // FIXME solve generally
    if (
      descriptor_is_unsigned_integer(rhs_value->descriptor) &&
      operand_is_immediate(&rhs_value->operand)
    ) {
      switch(lhs_value->operand.byte_size) {
        case 1: {
          if (u8_fits_into_s8(rhs_value->operand.u8)) {
            Value *adjusted = allocator_allocate(context->allocator, Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s8;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 2: {
          if (u16_fits_into_s16(rhs_value->operand.u16)) {
            Value *adjusted = allocator_allocate(context->allocator, Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s16;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 4: {
          if (u32_fits_into_s32(rhs_value->operand.u32)) {
            Value *adjusted = allocator_allocate(context->allocator, Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s32;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 8: {
          if (u64_fits_into_s64(rhs_value->operand.u64)) {
            Value *adjusted = allocator_allocate(context->allocator, Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s64;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        default: {
          panic("Internal Error: Unexpected integer size");
          break;
        }
      }
    } else {
      panic("FIXME handle errors here");
    }
  }

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

  compare(
    context->allocator, compare_type, builder,
    &operator->source_range, result_value, lhs_value, rhs_value
  );

  *replacement_count = 3;
  Token *token_value = token_value_make(context, result_value, source_range_from_token_view(view));
  return token_value;
}

bool
token_parse_statement(
  Compilation_Context *context,
  Token_View view,
  const Source_Range *source_range,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  Array_Const_Token_Ptr statement_tokens = token_array_from_view(allocator_system, view);
  // TODO consider how this should work
  token_rewrite_macros(context, &statement_tokens, scope, builder);
  view = token_view_from_token_array(statement_tokens);
  for (
    Scope *statement_matcher_scope = scope;
    statement_matcher_scope;
    statement_matcher_scope = statement_matcher_scope->parent
  ) {
    if (!dyn_array_is_initialized(statement_matcher_scope->statement_matchers)) {
      continue;
    }
    for (u64 i = 0 ; i < dyn_array_length(statement_matcher_scope->statement_matchers); ++i) {
      Token_Statement_Matcher matcher =
        *dyn_array_get(statement_matcher_scope->statement_matchers, i);
      if (matcher.proc(context, view, scope, builder, matcher.payload)) {
        return true;
      }
    }
  }

  bool result = token_match_expression(context, &statement_tokens, scope, builder, result_value);
  dyn_array_destroy(statement_tokens);
  return result;
}

bool
token_match_expression(
  Compilation_Context *context,
  Array_Const_Token_Ptr *tokens,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  token_state_clear_newlines(tokens);

  if (dyn_array_length(*tokens)) {
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_cast);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_struct_field);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_negative_literal);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_function_calls);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_array_index);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_pointer_to);

    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_divide);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_remainder);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_plus);
    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_minus);

    token_rewrite_expression(context, tokens, scope, builder, result_value, token_rewrite_compare);
  }

  bool full_match = true;
  switch(dyn_array_length(*tokens)) {
    case 0: {
      // What should happen here?
      break;
    }
    case 1: {
      const Token *token = *dyn_array_get(*tokens, 0);
      token_force_value(context, token, scope, builder, result_value);
      break;
    }
    default: {
      const Token *token = *dyn_array_get(*tokens, 0);
      program_error_builder(context, token->source_range) {
        program_error_append_literal("Could not parse the expression");
      }
      full_match = false;
      break;
    }
  }
  return full_match;
}

bool
token_parse(
  Compilation_Context *context,
  Token_View view
) {
  if (!view.length) return true;

  Function_Builder global_builder = { 0 };

  Array_Token_View module_statements = token_split_by_newlines_and_semicolons(view);
  for (u64 i = 0; i < dyn_array_length(module_statements); ++i) {
    Token_View statement = *dyn_array_get(module_statements, i);
    if (!statement.length) continue;
    if (token_rewrite_macro_definitions(context, statement, context->program->global_scope)) {
      continue;
    }
    if (token_rewrite_constant_definitions(
      context, statement, context->program->global_scope, &global_builder, 0
    )) {
      continue;
    }

    // Report unmatched statements
    Source_Range source_range = source_range_from_token_view(statement);
    program_error_builder(context, source_range) {
      program_error_append_literal("Could not parse a top level statement");
    }
    return false;
  }

  return true;
}

Parse_Result
program_parse(
  Compilation_Context *context,
  Source_File *file
) {
  Tokenizer_Result tokenizer_result = tokenize(context->allocator, file);
  if (tokenizer_result.tag != Tokenizer_Result_Tag_Success) {
    return (Parse_Result) {
      .tag = Parse_Result_Tag_Error,
      .Error = { tokenizer_result.Error.errors },
    };
  }
  bool ok = token_parse(context, token_view_from_token_array(tokenizer_result.Success.tokens));
  return (Parse_Result) {
    .tag = ok ? Parse_Result_Tag_Success
      : Parse_Result_Tag_Error
  };
}

Fixed_Buffer *
win32_absolute_path(
  Slice raw_path
) {
  Slice result_path = raw_path;
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
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = result_path.length + 1024
  );

  fixed_buffer_append_slice(result_buffer, result_path);

  if (sys_buffer) fixed_buffer_destroy(sys_buffer);
  return result_buffer;
}

Parse_Result
program_import_file(
  Compilation_Context *context,
  Slice file_path
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = win32_absolute_path(file_path);

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
    Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);
    dyn_array_push(errors, (Parse_Error) {
      .message = slice_literal("Unable to open the file"),
      .source_range = {
        .file = file,
      },
    });
    return (Parse_Result) {
      .tag = Parse_Result_Tag_Error,
      .Error = {errors},
    };
  }
  file->text = fixed_buffer_as_slice(buffer);
  return program_parse(context, file);
}

