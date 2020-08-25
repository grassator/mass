#include "prelude.h"
#include "source.h"
#include "function.h"

Scope *
scope_make(
  Scope *parent
) {
  Scope *scope = temp_allocate(Scope);
  *scope = (Scope) {
    .parent = parent,
    .map = hash_map_make(Scope_Map),
  };
  return scope;
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
token_force_value(
  Token *token,
  Scope *scope,
  Function_Builder *builder
);

Value *
scope_lookup_force(
  Scope *scope,
  Slice name,
  Function_Builder *builder
) {
  Scope_Entry *entry = 0;
  while (scope) {
    entry = hash_map_get(scope->map, name);
    if (entry) break;
    scope = scope->parent;
  }
  if (!entry) {
    printf("Could not resolve identifier: ");
    slice_print(name);
    printf("\n");
  }
  assert(entry);
  Value *result = 0;
  switch(entry->type) {
    case Scope_Entry_Type_Value: {
      result = entry->value;
      break;
    }
    case Scope_Entry_Type_Lazy: {
      Array_Token_Ptr tokens = entry->tokens;
      for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
        Token *token = *dyn_array_get(tokens, i);
        if (!result) {
          result = token_force_value(token, scope, builder);
        } else {
          assert(result->descriptor->type == Descriptor_Type_Function);
          Value *overload = token_force_value(token, scope, builder);
          overload->descriptor->function.next_overload = result;
          result = overload;
        }
      }
      assert(result);
      *entry = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .value = result,
      };
    }
  }
  // For functions we need to gather up overloads from all parent scopes
  if (result->descriptor->type == Descriptor_Type_Function) {
    Value *last = result;
    Scope *parent = scope;
    for (;;) {
      parent = parent->parent;
      if (!parent) break;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(parent, name, builder);
      assert(overload->descriptor->type == Descriptor_Type_Function);
      while (last->descriptor->function.next_overload) {
        last = last->descriptor->function.next_overload;
      }
      last->descriptor->function.next_overload = overload;
    };
  }
  assert(result);
  return result;
}

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
) {
  // TODO think about what should happen when trying to redefine existing thing
  Scope_Entry entry = {
    .type = Scope_Entry_Type_Value,
    .value = value,
  };
  hash_map_set(scope->map, name, entry);
}

void
scope_define_lazy(
  Scope *scope,
  Slice name,
  Token *token
) {
  // For overloads with only check current scope and allow multiple overloads
  // in multiple nested scopes
  if (!hash_map_has(scope->map, name)) {
    Scope_Entry entry = {
      .type = Scope_Entry_Type_Lazy,
      .tokens = dyn_array_make(Array_Token_Ptr),
    };
    hash_map_set(scope->map, name, entry);
  }
  Scope_Entry *entry = hash_map_get(scope->map, name);
  dyn_array_push(entry->tokens, token);
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
    case '~':
    case '>':
    case '<':
      return true;
    default:
      return false;
  }
}

typedef struct {
  Slice filename;
  u64 line;
  u64 column;
} Source_Location;

typedef struct {
  const char *message;
  Source_Location location;
} Tokenizer_Error;
typedef dyn_array_type(Tokenizer_Error) Array_Tokenizer_Error;

typedef enum {
  Tokenizer_Result_Type_Error,
  Tokenizer_Result_Type_Success,
} Tokenizer_Result_Type;

typedef struct {
  Tokenizer_Result_Type type;
  union {
    Token *root;
    Array_Tokenizer_Error errors;
  };
} Tokenizer_Result;

void
print_message_with_location(
  const char *message,
  Source_Location *location
) {
  printf(
    "%.*s(%llu:%llu): %s\n",
    u64_to_s32(location->filename.length),
    location->filename.bytes,
    location->line,
    location->column,
    message
  );
}

Tokenizer_Result
tokenize(
  Slice filename,
  Slice source
) {
  Token *root = temp_allocate(Token);
  root->parent = 0;
  root->type = Token_Type_Module;
  root->children = dyn_array_make(Array_Token_Ptr);
  root->source = source;

  Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;

  Array_Tokenizer_Error errors = dyn_array_make(Array_Tokenizer_Error);

#define start_token(_type_)\
  do {\
    current_token = temp_allocate(Token);\
    *current_token = (Token) {\
      .type = (_type_),\
      .parent = parent,\
      .source = {\
        .bytes = &source.bytes[i],\
        .length = 1,\
      },\
    };\
  } while(0)

#define push\
  do {\
    dyn_array_push(parent->children, current_token);\
    current_token = 0;\
    state = Tokenizer_State_Default;\
  } while(0)

#define push_error(_message_)\
  dyn_array_push(errors, (Tokenizer_Error) {\
    .message = (_message_),\
    .location = {\
      .filename = filename,\
      .line = line,\
      .column = column,\
    }\
  })

  u64 line = 1;
  u64 column = 0;
  for (u64 i = 0; i < source.length; ++i) {
    s8 ch = source.bytes[i];
    s8 peek = i + 1 < source.length ? source.bytes[i + 1] : 0;

    if (ch == '\r') {
      if (peek == '\n') {
        continue;
      }
      ch = '\n';
    }

    if (ch == '\n') {
      line++;
      column = 1;
    } else {
      column++;
    }

    retry: switch(state) {
      case Tokenizer_State_Default: {
        if (isspace(ch)) continue;
        if (isdigit(ch)) {
          start_token(Token_Type_Integer);
          state = Tokenizer_State_Integer;
        } else if (isalpha(ch) || ch == '_') {
          start_token(Token_Type_Id);
          state = Tokenizer_State_Id;
        } else if(ch == '/' && peek == '/') {
          state = Tokenizer_State_Single_Line_Comment;
        } else if (code_point_is_operator(ch)) {
          start_token(Token_Type_Operator);
          state = Tokenizer_State_Operator;
        } else if (ch == '"') {
          start_token(Token_Type_String);
          state = Tokenizer_State_String;
        } else if (ch == '(' || ch == '{' || ch == '[') {
          Token_Type type =
            ch == '(' ? Token_Type_Paren :
            ch == '{' ? Token_Type_Curly :
            Token_Type_Square;
          start_token(type);
          current_token->children = dyn_array_make(Array_Token_Ptr, 4);
          dyn_array_push(parent->children, current_token);
          parent = current_token;
        } else if (ch == ')' || ch == '}' || ch == ']') {
          switch (parent->type) {
            case Token_Type_Paren: {
              assert(ch == ')');
              break;
            }
            case Token_Type_Curly: {
              assert(ch == '}');
              break;
            }
            case Token_Type_Square: {
              assert(ch == ']');
              break;
            }
            case Token_Type_Value:
            case Token_Type_Id:
            case Token_Type_Integer:
            case Token_Type_Operator:
            case Token_Type_Lazy_Function_Definition:
            case Token_Type_String:
            case Token_Type_Module: {
              assert(!"Internal Tokenizer Error: Unexpected closing char for group");
              break;
            }
          }
          parent->source.length = &source.bytes[i] - parent->source.bytes + 1;
          parent = parent->parent;
          current_token = 0;
          if (!parent) {
            push_error("Encountered a closing brace without a matching open one");
            goto end;
          }
        } else {
          push_error("Unpexpected input");
          goto end;
        }
        break;
      }
      case Tokenizer_State_Integer: {
        if (isdigit(ch)) {
          current_token->source.length++;
        } else {
          push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Id: {
        if (isalpha(ch) || isdigit(ch) || ch == '_') {
          current_token->source.length++;
        } else {
          push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Operator: {
        if (code_point_is_operator(ch)) {
          current_token->source.length++;
        } else {
          push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_String: {
        current_token->source.length++;
        if (ch == '"') {
          push;
        }
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

  if (parent != root) {
    push_error("Unexpected end of file. Expected a closing brace.");
  }
  // current_token can be null in case of an empty input
  if (current_token) {
    // Strings need to be terminated with a '"'
    if (state == Tokenizer_State_String) {
      push_error("Unexpected end of file. Expected a \".");
    } else {
      dyn_array_push(root->children, current_token);
    }
  }
  end:
#undef push_error
#undef start_token
#undef push_and_retry
  if (dyn_array_length(errors)) {
    return (Tokenizer_Result){.type = Tokenizer_Result_Type_Error, .errors = errors};
  }
  return (Tokenizer_Result){.type = Tokenizer_Result_Type_Success, .root = root};
}

typedef struct {
  Array_Token_Ptr tokens;
  u64 start_index;
} Token_Matcher_State;
typedef dyn_array_type(Token_Matcher_State) Array_Token_Matcher_State;

Token *
token_peek(
  Token_Matcher_State *state,
  u64 delta
) {
  u64 index = state->start_index + delta;
  Token **peek = dyn_array_peek(state->tokens, index);
  if (!peek) return 0;
  return *peek;
}

bool
token_match(
  const Token *source,
  const Token *pattern
) {
  if (pattern->type && pattern->type != source->type) return false;
  if (pattern->source.length && !slice_equal(pattern->source, source->source)) return false;
  return true;
}

Token *
token_peek_match(
  Token_Matcher_State *state,
  u64 delta,
  Token *pattern_token
) {
  Token *source_token = token_peek(state, delta);
  if (!source_token) return 0;
  if (!token_match(source_token, pattern_token)) return 0;
  return source_token;
}

Array_Token_Matcher_State
token_split(
  Array_Token_Ptr tokens,
  const Token *separator
) {
  Array_Token_Matcher_State result = dyn_array_make(Array_Token_Matcher_State);

  Array_Token_Ptr sequence = dyn_array_make(Array_Token_Ptr);
  for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
    Token *token = *dyn_array_get(tokens, i);
    if (token_match(token, separator)) {
      dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
      sequence = dyn_array_make(Array_Token_Ptr);
    } else {
      dyn_array_push(sequence, token);
    }
  }
  dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
  return result;
}

Descriptor *
scope_lookup_type(
  Scope *scope,
  Slice type_name
) {
  Scope_Entry *entry = scope_lookup(scope, type_name);
  assert(entry);
  assert(entry->type == Scope_Entry_Type_Value);
  assert(entry->value->descriptor->type == Descriptor_Type_Type);
  Descriptor *descriptor = entry->value->descriptor->type_descriptor;
  assert(descriptor);
  return descriptor;
}

#define Maybe_Token_Match(_id_, ...)\
  Token *(_id_) = token_peek_match(state, peek_index++, &(Token) { __VA_ARGS__ });\

#define Token_Match(_id_, ...)\
  Maybe_Token_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .type = Token_Type_Operator, .source = slice_literal(_op_))

#define Token_Match_End()\
  assert(peek_index == dyn_array_length(state->tokens))

typedef struct {
  Slice arg_name;
  Descriptor *type_descriptor;
} Token_Match_Arg;

Descriptor *
token_force_type(
  Scope *scope,
  Token *token
) {
  Descriptor *descriptor = 0;
  switch (token->type) {
    case Token_Type_Id: {
      descriptor = scope_lookup_type(scope, token->source);
      break;
    }
    case Token_Type_Square: {
      assert(dyn_array_length(token->children) == 1);
      Token *child = *dyn_array_get(token->children, 0);
      // FIXME should be recursive
      assert(child->type == Token_Type_Id);
      descriptor = temp_allocate(Descriptor);
      *descriptor = (Descriptor) {
        .type = Descriptor_Type_Pointer,
        .pointer_to = scope_lookup_type(scope, child->source),
      };
      break;
    }
    case Token_Type_Integer:
    case Token_Type_Operator:
    case Token_Type_String:
    case Token_Type_Paren:
    case Token_Type_Curly:
    case Token_Type_Module:
    case Token_Type_Value:
    case Token_Type_Lazy_Function_Definition:
    default: {
      assert(!"Not implemented");
      break;
    }
  }
  return descriptor;
}

typedef Array_Token_Ptr (*token_pattern_callback)(
  Array_Token_Ptr match,
  Scope *scope,
  Function_Builder *builder_
);

Array_Token_Ptr
token_match_pattern(
  Token_Matcher_State *state,
  Array_Token_Ptr pattern
) {
  u64 pattern_length = dyn_array_length(pattern);
  assert(pattern_length);
  for (u64 i = 0; i < pattern_length; ++i) {
    Token *token = token_peek_match(state, i, *dyn_array_get(pattern, i));
    if (!token) return (Array_Token_Ptr){0};
  }
  Array_Token_Ptr match = dyn_array_make(Array_Token_Ptr, .capacity = pattern_length);
  for (u64 i = 0; i < pattern_length; ++i) {
    Token *token = token_peek(state, i);
    dyn_array_push(match, token);
  }
  return match;
}

inline void
token_replace_length_with_tokens(
  Token_Matcher_State *state,
  u64 pattern_length,
  Array_Token_Ptr replacement
) {
  dyn_array_splice(state->tokens, state->start_index, pattern_length, replacement);
}

bool
token_rewrite_pattern(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Array_Token_Ptr pattern,
  token_pattern_callback callback
) {
  Array_Token_Ptr match = token_match_pattern(state, pattern);
  if (!dyn_array_is_initialized(match)) return false;
  Array_Token_Ptr replacement = callback(match, scope, builder);
  token_replace_length_with_tokens(state, dyn_array_length(pattern), replacement);
  dyn_array_destroy(match);
  return true;
}

Array_Token_Ptr
token_clone_token_array_deep(
  Array_Token_Ptr source
) {
  Array_Token_Ptr result = dyn_array_make(Array_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    Token *token = *dyn_array_get(source, i);
    Token *copy = temp_allocate(Token);
    *copy = *token;
    switch (token->type) {
      case Token_Type_Integer:
      case Token_Type_Operator:
      case Token_Type_String:
      case Token_Type_Id: {
        // Nothing to do
        break;
      }
      case Token_Type_Square:
      case Token_Type_Paren:
      case Token_Type_Curly:
      case Token_Type_Module: {
        copy->children = token_clone_token_array_deep(token->children);
        break;
      }
      case Token_Type_Value:
      case Token_Type_Lazy_Function_Definition: {
        assert(!"Not reached");
        break;
      }
    }
    dyn_array_push(result, token);
  }
  return result;
}

Array_Token_Ptr
token_apply_macro_replacements(
  Macro_Replacement_Map *map,
  Array_Token_Ptr source
) {
  Array_Token_Ptr result = dyn_array_make(Array_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    Token *token = *dyn_array_get(source, i);
    Token *copy = temp_allocate(Token);
    *copy = *token;
    switch (token->type) {
      case Token_Type_Id: {
        Slice name = token->source;
        Token **replacement_ptr = hash_map_get(map, name);
        if (replacement_ptr) {
          *copy = **replacement_ptr;
        }
        break;
      }
      case Token_Type_Integer:
      case Token_Type_Operator:
      case Token_Type_String: {
        // Nothing to do
        break;
      }
      case Token_Type_Square:
      case Token_Type_Paren:
      case Token_Type_Curly:
      case Token_Type_Module: {
        copy->children = token_apply_macro_replacements(map, token->children);
        break;
      }
      case Token_Type_Value:
      case Token_Type_Lazy_Function_Definition: {
        assert(!"Not reached");
        break;
      }
    }
    dyn_array_push(result, copy);
  }
  return result;
}

void
token_rewrite_macro_match(
  Token_Matcher_State *state,
  Macro *macro,
  Array_Token_Ptr match
) {
  Macro_Replacement_Map *map = hash_map_make(Macro_Replacement_Map);
  assert(dyn_array_length(macro->pattern_names) == dyn_array_length(match));
  for (u64 i = 0; i < dyn_array_length(macro->pattern_names); ++i) {
    Slice *name = dyn_array_get(macro->pattern_names, i);
    if (name->length) {
      hash_map_set(map, *name, *dyn_array_get(match, i));
    }
  }
  Array_Token_Ptr replacement = token_apply_macro_replacements(map, macro->replacement);
  token_replace_length_with_tokens(
    state,
    dyn_array_length(macro->pattern),
    replacement
  );
  dyn_array_destroy(match);
  hash_map_destroy(map);
}

void
token_rewrite_macros(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);
      start: for (;;) {
        for (u64 i = 0; i < dyn_array_length(state->tokens); ++i) {
          state->start_index = i;
          Array_Token_Ptr match = token_match_pattern(state, macro->pattern);
          if (dyn_array_is_initialized(match)) {
            token_rewrite_macro_match(state, macro, match);
            goto start;
          }
        }
        break;
      }
    }
  }
}

Token_Match_Arg *
token_match_argument(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(arg_id, .type = Token_Type_Id);
  Token_Match_Operator(arg_colon, ":");
  Token_Match(arg_type, 0);
  Token_Match_End();

  Token_Match_Arg *result = temp_allocate(Token_Match_Arg);
  *result = (Token_Match_Arg) {
    .arg_name = arg_id->source,
    .type_descriptor = token_force_type(scope, arg_type),
  };
  return result;
}

Value *
token_force_lazy_function_definition(
  Lazy_Function_Definition *lazy_function_definition
);

Value *
token_match_expression(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
);

Slice
token_string_to_slice(
  Token *token
) {
  assert(token->type == Token_Type_String);
  assert(token->source.length >= 2);
  return (Slice) {
    .bytes = token->source.bytes + 1,
    .length = token->source.length - 2
  };
}

Value *
token_force_value(
  Token *token,
  Scope *scope,
  Function_Builder *builder
) {
  Value *result_value = 0;
  if (token->type == Token_Type_Integer) {
    Slice_Parse_S64_Result parse_result = slice_parse_s64(token->source);
    assert(parse_result.success);
    result_value = value_from_signed_immediate(parse_result.value);
  } else if (token->type == Token_Type_String) {
    Slice string = token_string_to_slice(token);
    result_value = value_pointer_to(
      builder, value_global_c_string_from_slice(builder->program, string)
    );
  } else if (token->type == Token_Type_Id) {
    result_value = scope_lookup_force(scope, token->source, builder);
  } else if (token->type == Token_Type_Value) {
    result_value = token->value;
  } else if (token->type == Token_Type_Lazy_Function_Definition) {
    return token_force_lazy_function_definition(&token->lazy_function_definition);
  } else if (token->type == Token_Type_Paren) {
    assert(builder);
    Token_Matcher_State state = {.tokens = token->children};
    return token_match_expression(&state, scope, builder);
  } else if (token->type == Token_Type_Curly) {
    assert(builder);
    return token_parse_block(token, scope, builder);
  } else {
    assert(!"Not implemented");
  }
  assert(result_value);
  return result_value;
}

Array_Value_Ptr
token_match_call_arguments(
  Token *token,
  Scope *scope,
  Function_Builder *builder_
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (dyn_array_length(token->children) != 0) {
    Array_Token_Matcher_State argument_states = token_split(token->children, &(Token){
      .type = Token_Type_Operator,
      .source = slice_literal(","),
    });
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_Matcher_State *state = dyn_array_get(argument_states, i);
      Value *value = token_match_expression(state, scope, builder_);
      dyn_array_push(result, value);
    }
  }
  return result;
}

Token *
token_value_make(
  Token *original,
  Value *result
) {
  Token *result_token = temp_allocate(Token);
  *result_token = (Token){
    .type = Token_Type_Value,
    .parent = original->parent,
    .source = original->source, // TODO should encompass the whole sub-expression
    .value = result,
  };
  return result_token;
}

void
token_replace_tokens_in_state(
  Token_Matcher_State *state,
  u64 length,
  Token *token
) {
  u64 from = state->start_index;
  if (token) {
    *dyn_array_get(state->tokens, from) = token;
    from += 1;
    length -= 1;
  }
  dyn_array_delete_range(state->tokens, (Range_u64){from, from + length});
}

Array_Token_Ptr
token_rewrite_functions_pattern_callback(
  Array_Token_Ptr match,
  Scope *scope,
  Function_Builder *builder_
) {
  Token *args = *dyn_array_get(match, 0);
  Token *return_types = *dyn_array_get(match, 2);
  Token *body = *dyn_array_get(match, 3);

  Token *result_token = temp_allocate(Token);
  *result_token = (Token) {
    .type = Token_Type_Lazy_Function_Definition,
    .parent = body->parent,
    .source = body->source,
    .lazy_function_definition = {
      .args = args,
      .return_types = return_types,
      .body = body,
      .program = builder_->program,
    },
  };
  dyn_array_clear(match);
  dyn_array_push(match, result_token);
  return match;
}

bool
token_rewrite_functions(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  static Array_Token_Ptr pattern = {0};
  if (!dyn_array_is_initialized(pattern)) {
    pattern = dyn_array_make(Array_Token_Ptr);
    static Token args = { .type = Token_Type_Paren };
    dyn_array_push(pattern, &args);
    static Token arrow = { .type = Token_Type_Operator, .source = slice_literal_fields("->") };
    dyn_array_push(pattern, &arrow);
    static Token return_types = { .type = Token_Type_Paren };
    dyn_array_push(pattern, &return_types);
    static Token body = {0};
    dyn_array_push(pattern, &body);
  }

  return token_rewrite_pattern(state, scope, builder_, pattern, token_rewrite_functions_pattern_callback);
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
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("macro"));
  Token_Match(pattern_token, .type = Token_Type_Paren);
  Token_Match(replacement_token, .type = Token_Type_Paren);
  token_replace_tokens_in_state(state, 3, 0);

  Array_Token_Ptr pattern = dyn_array_make(Array_Token_Ptr);
  Array_Slice pattern_names = dyn_array_make(Array_Slice);

  for (u64 i = 0; i < dyn_array_length(pattern_token->children); ++i) {
    Token *token = *dyn_array_get(pattern_token->children, i);
    if (token->type == Token_Type_Id && slice_starts_with(token->source, slice_literal("_"))) {
      Slice name = slice_sub(token->source, 1, token->source.length);
      dyn_array_push(pattern_names, name);
      Token *item = temp_allocate(Token);
      *item = (Token){0};
      dyn_array_push(pattern, item);
    } else {
      dyn_array_push(pattern_names, (Slice){0});
      dyn_array_push(pattern, token);
    }
  }

  Macro *macro = temp_allocate(Macro);
  *macro = (Macro){
    .pattern = pattern,
    .pattern_names = pattern_names,
    .replacement = replacement_token->children,
  };

  scope_add_macro(scope, macro);

  return true;
}

bool
token_rewrite_constant_definitions(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, "::");
  Token_Match(value, 0);
  scope_define_lazy(scope, name->source, value);

  // FIXME definition should rewrite with a token so that we can do proper
  // checking inside statements and maybe pass it around.
  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

Token *
token_import_match_arguments(
  Token *paren,
  Program *program
) {
  assert(paren->type == Token_Type_Paren);
  Token_Matcher_State *state = &(Token_Matcher_State) {.tokens = paren->children };

  Token *library_name_string = token_peek_match(state, 0, &(Token) {
    .type = Token_Type_String,
  });
  assert(library_name_string);
  Token *comma = token_peek_match(state, 1, &(Token) {
    .type = Token_Type_Operator,
    .source = slice_literal(","),
  });
  assert(comma);
  Token *symbol_name_string = token_peek_match(state, 2, &(Token) {
    .type = Token_Type_String,
  });
  assert(symbol_name_string);
  Slice library_name = token_string_to_slice(library_name_string);
  Slice symbol_name = token_string_to_slice(symbol_name_string);

  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = 0,
    .operand = import_symbol(
      program,
      library_name,
      symbol_name
    ),
  };
  return token_value_make(paren, result);
}

bool
token_rewrite_dll_imports(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("import"));
  Token_Match(args, .type = Token_Type_Paren);
  Token *result_token = token_import_match_arguments(args, builder_->program);
  assert(result_token);

  token_replace_tokens_in_state(state, 2, result_token);
  return true;
}

Value *
token_parse_block(
  Token *block,
  Scope *scope,
  Function_Builder *builder_
) {
  // TODO push an extra scope
  assert(block->type == Token_Type_Curly);
  Value *block_result = 0;
  if (dyn_array_length(block->children) != 0) {
    Array_Token_Matcher_State block_statements = token_split(block->children, &(Token){
      .type = Token_Type_Operator,
      .source = slice_literal(";"),
    });
    for (u64 i = 0; i < dyn_array_length(block_statements); ++i) {
      Token_Matcher_State *state = dyn_array_get(block_statements, i);
      block_result = token_match_expression(state, scope, builder_);
    }
  }
  return block_result;
}

bool
token_rewrite_statement_if(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("if"));
  Token_Match(condition, .type = Token_Type_Paren);
  Token_Match(body, .type = Token_Type_Curly);
  Token_Match_End();

  If(token_force_value(condition, scope, builder_)) {
    (void)token_parse_block(body, scope, builder_);
  }

  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

bool
token_rewrite_goto(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("goto"));
  Token_Match(label_name, .type = Token_Type_Id);
  Token_Match_End();
  Value *value = scope_lookup_force(scope, label_name->source, builder_);
  assert(value->descriptor->type == Descriptor_Type_Void);
  assert(value->operand.type == Operand_Type_Label_32);

  push_instruction(builder_, (Instruction) {jmp, {value->operand, 0, 0}});

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_explicit_return(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("return"));
  Token_Match(to_return, 0);
  Token_Match_End();
  Value *result = token_force_value(to_return, scope, builder_);
  Return(result);

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_negative_literal(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  // FIXME distinguish unary and binary minus
  Token_Match_Operator(define, "-");
  Token_Match(integer, .type = Token_Type_Integer);
  Value *result = token_force_value(integer, scope, builder_);
  if (result->operand.type == Operand_Type_Immediate_8) {
    result->operand.imm8 = -result->operand.imm8;
  } else if (result->operand.type == Operand_Type_Immediate_32) {
    result->operand.imm32 = -result->operand.imm32;
  } else if (result->operand.type == Operand_Type_Immediate_64) {
    result->operand.imm64 = -result->operand.imm64;
  } else {
    assert(!"Internal error, expected an immediate");
  }

  token_replace_tokens_in_state(state, 2, token_value_make(integer, result));
  return true;
}

bool
token_rewrite_pointer_to(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match_Operator(define, "&");
  Token_Match(value_token, 0);

  Value *result = value_pointer_to(builder_, token_force_value(value_token, scope, builder_));
  token_replace_tokens_in_state(state, 2, token_value_make(value_token, result));
  return true;
}

bool
token_rewrite_set_array_item(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(id, .type = Token_Type_Id, .source = slice_literal("set_array_item"));
  Token_Match(value_token, .type = Token_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(value_token, scope, builder_);
  assert(dyn_array_length(args) == 3);
  Value *array = *dyn_array_get(args, 0);
  Value *index_value = *dyn_array_get(args, 1);
  Value *value = *dyn_array_get(args, 2);
  assert(array->descriptor->type == Descriptor_Type_Fixed_Size_Array);
  assert(array->operand.type == Operand_Type_Memory_Indirect);

  Descriptor *item_descriptor = array->descriptor->array.item;
  u32 item_byte_size = descriptor_byte_size(item_descriptor);

  //if (operand_is_immediate(&index_value->operand)) {
    //s32 index = s64_to_s32(operand_immediate_as_s64(&index_value->operand));
//
    //Value *target_value = temp_allocate(Value);
    //*target_value = (Value){
      //.descriptor = item_descriptor,
      //.operand = {
        //.type = Operand_Type_Memory_Indirect,
        //.byte_size = item_byte_size,
        //.indirect = (Operand_Memory_Indirect) {
          //.reg = array->operand.indirect.reg,
          //.displacement = array->operand.indirect.displacement + index * item_byte_size,
        //}
      //}
    //};
//
    //move_value(builder_, target_value, value);
  //} else
  if(
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
    Value *index_value_in_register = ensure_register(builder_, index_value, Register_R10);
    Value *target_value = temp_allocate(Value);
    *target_value = (Value){
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
    move_value(builder_, target_value, value);
  } else {
    assert(!"Not implemented");
  }

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

Label *
token_match_label(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("label"));
  Token_Match_End();

  return make_label();
}

Descriptor *
token_match_fixed_array_type(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(type, .type = Token_Type_Id);
  Token_Match(square_brace, .type = Token_Type_Square);

  Descriptor *descriptor = scope_lookup_type(scope, type->source);

  Token_Matcher_State size_state = {.tokens = square_brace->children};
  Value *size_value = token_match_expression(&size_state, scope, builder_);
  assert(size_value->descriptor->type == Descriptor_Type_Integer);
  assert(operand_is_immediate(&size_value->operand));
  u32 length = s64_to_u32(operand_immediate_as_s64(&size_value->operand));

  // TODO extract into a helper
  Descriptor *array_descriptor = temp_allocate(Descriptor);
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
token_rewrite_definitions(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":");

  u64 start_index = state->start_index + peek_index;
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){start_index, dyn_array_length(state->tokens)}
  );
  Token_Matcher_State rest_state = {.tokens = rest};
  state->tokens.data->length = state->start_index;

  Label *label = token_match_label(&rest_state, scope, builder_);
  if (label) {
    push_instruction(builder_, (Instruction) { .maybe_label = label });
    Value *value = temp_allocate(Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = label32(label),
    };
    scope_define_value(scope, name->source, value);
    return true;
  }

  Descriptor *descriptor = token_match_fixed_array_type(&rest_state, scope, builder_);
  if (!descriptor) {
    assert(dyn_array_length(rest) == 1);
    Token *type = *dyn_array_get(rest, 0);
    assert(type->type == Token_Type_Id);
    descriptor = scope_lookup_type(scope, type->source);
  }

  Value *value = reserve_stack(builder_, descriptor);
  scope_define_value(scope, name->source, value);

  return true;
}

bool
token_rewrite_definition_and_assignment_statements(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":=");
  Token_Match(token_value, 0);
  Value *value = token_force_value(token_value, scope, builder_);
  Stack(var, value->descriptor, value);
  scope_define_value(scope, name->source, var);

  // FIXME definition should rewrite with a token so that we can do proper
  // checking inside statements and maybe pass it around.
  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

bool
token_rewrite_assignments(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, "=");
  Token_Match(token_value, 0);

  Value *value = token_force_value(token_value, scope, builder_);
  Value *target = scope_lookup_force(scope, name->source, builder_);
  move_value(builder_, target,  value);

  // FIXME definition should rewrite with a token so that we can do proper
  // checking inside statements and maybe pass it around.
  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

bool
token_rewrite_function_calls(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(args_token, .type = Token_Type_Paren);
  if (target_token->type != Token_Type_Id && target_token->type != Token_Type_Paren) return false;

  Value *target = token_force_value(target_token, scope, builder);
  Array_Value_Ptr args = token_match_call_arguments(args_token, scope, builder);

  Value *return_value = call_function_value_array(builder, target, args);
  token_replace_tokens_in_state(state, 2, token_value_make(args_token, return_value));
  return true;
}

bool
token_rewrite_plus(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(plus_token, "+");
  Token_Match(rhs, 0);

  Value *value = Plus(
    token_force_value(lhs, scope, builder_),
    token_force_value(rhs, scope, builder_)
  );
  token_replace_tokens_in_state(state, 3, token_value_make(plus_token, value));
  return true;
}
bool
token_rewrite_less_than(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(plus_token, "<");
  Token_Match(rhs, 0);

  Value *value = Less(
    token_force_value(lhs, scope, builder_),
    token_force_value(rhs, scope, builder_)
  );
  token_replace_tokens_in_state(state, 3, token_value_make(plus_token, value));
  return true;
}

bool
token_rewrite_greater_than(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(plus_token, ">");
  Token_Match(rhs, 0);

  Value *value = Greater(
    token_force_value(lhs, scope, builder_),
    token_force_value(rhs, scope, builder_)
  );
  token_replace_tokens_in_state(state, 3, token_value_make(plus_token, value));
  return true;
}

typedef bool (*token_rewrite_expression_callback)(Token_Matcher_State *, Scope *, Function_Builder *);

void
token_rewrite_expression(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  token_rewrite_expression_callback callback
) {
  start: for (;;) {
    for (u64 i = 0; i < dyn_array_length(state->tokens); ++i) {
      state->start_index = i;
      if (callback(state, scope, builder)) goto start;
    }
    break;
  }
}

Value *
token_match_expression(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  if (!dyn_array_length(state->tokens)) {
    return 0;
  }
  token_rewrite_macros(state, scope, builder);
  token_rewrite_expression(state, scope, builder, token_rewrite_statement_if);
  token_rewrite_expression(state, scope, builder, token_rewrite_definitions);
  token_rewrite_expression(state, scope, builder, token_rewrite_set_array_item);


  token_rewrite_expression(state, scope, builder, token_rewrite_functions);
  token_rewrite_expression(state, scope, builder, token_rewrite_negative_literal);
  token_rewrite_expression(state, scope, builder, token_rewrite_function_calls);
  token_rewrite_expression(state, scope, builder, token_rewrite_pointer_to);
  token_rewrite_expression(state, scope, builder, token_rewrite_plus);
  token_rewrite_expression(state, scope, builder, token_rewrite_less_than);
  token_rewrite_expression(state, scope, builder, token_rewrite_greater_than);

  switch(dyn_array_length(state->tokens)) {
    case 0: {
      return &void_value;
    }
    case 1: {
      return token_force_value(*dyn_array_get(state->tokens, 0), scope, builder);
    }
    default: {
      // Statement handling
      token_rewrite_expression(state, scope, builder, token_rewrite_definition_and_assignment_statements);
      token_rewrite_expression(state, scope, builder, token_rewrite_assignments);
      token_rewrite_expression(state, scope, builder, token_rewrite_explicit_return);
      token_rewrite_expression(state, scope, builder, token_rewrite_goto);
      token_rewrite_expression(state, scope, builder, token_rewrite_constant_definitions);
      if (dyn_array_length(state->tokens)) {
        Token *token = *dyn_array_get(state->tokens, 0);
        printf("Could not reduce an expression: \n");
        slice_print(token->source);
        printf("\n");
        assert(!"Could not reduce an expression");
      }
      return 0;
    }
  }
}

Value *
token_force_lazy_function_definition(
  Lazy_Function_Definition *lazy_function_definition
) {
  Token *args = lazy_function_definition->args;
  Token *return_types = lazy_function_definition->return_types;
  Token *body = lazy_function_definition->body;
  Program *program_ = lazy_function_definition->program;
  Scope *function_scope = scope_make(program_->global_scope);
  Function(value) {
    if (dyn_array_length(args->children) != 0) {
      Array_Token_Matcher_State argument_states = token_split(args->children, &(Token){
        .type = Token_Type_Operator,
        .source = slice_literal(","),
      });
      for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
        Token_Matcher_State *state = dyn_array_get(argument_states, i);
        Token_Match_Arg *arg = token_match_argument(state, function_scope, builder_);
        assert(arg);
        Arg(arg_value, arg->type_descriptor);
        scope_define_value(function_scope, arg->arg_name, arg_value);
      }
    }

    switch (dyn_array_length(return_types->children)) {
      case 0: {
        value->descriptor->function.returns = &void_value;
        break;
      }
      case 1: {
        Token *return_type_token = *dyn_array_get(return_types->children, 0);
        fn_return_descriptor(
          builder_,
          token_force_type(function_scope, return_type_token),
          Function_Return_Type_Explicit
        );
        break;
      }
      default: {
        assert(!"Multiple return types are not supported at the moment");
        break;
      }
    }
    fn_freeze(builder_);

    // FIXME figure out a better way to distinguish imports
    if (body->type == Token_Type_Value && !body->value->descriptor) {
      body->value->descriptor = builder_->descriptor;
      return body->value;
    } else {
      Value *body_result = token_parse_block(body, function_scope, builder_);
      if (body_result) {
        fn_return(builder_, body_result, Function_Return_Type_Implicit);
      }
    }
  }
  return value;
}

void
token_match_module(
  Token *token,
  Program *program
) {
  assert(token->type == Token_Type_Module);
  if (!dyn_array_length(token->children)) return;

  Token_Matcher_State *state = &(Token_Matcher_State) {.tokens = token->children};
  Function_Builder global_builder = { .program = program };

  token_rewrite_expression(state, program->global_scope, &global_builder, token_rewrite_macro_definitions);
  token_rewrite_expression(state, program->global_scope, &global_builder, token_rewrite_dll_imports);
  token_rewrite_expression(state, program->global_scope, &global_builder, token_rewrite_functions);
  token_rewrite_expression(state, program->global_scope, &global_builder, token_rewrite_constant_definitions);

  assert(dyn_array_length(state->tokens) == 0);
}

bool
program_import_file(
  Program *program,
  Slice file_path
) {
  Slice extension = slice_literal(".mass");
  if (!slice_ends_with(file_path, extension)) {
    Fixed_Buffer *buffer = fixed_buffer_make(
      .allocator = temp_allocator,
      .capacity = file_path.length + extension.length
    );
    fixed_buffer_append_slice(buffer, file_path);
    fixed_buffer_append_slice(buffer, extension);
    file_path = fixed_buffer_as_slice(buffer);
  }
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) return false;
  Slice source = fixed_buffer_as_slice(buffer);

  Tokenizer_Result result = tokenize(file_path, source);
  if (result.type != Tokenizer_Result_Type_Success) {
    for (u64 i = 0; i < dyn_array_length(result.errors); ++i) {
      Tokenizer_Error *error = dyn_array_get(result.errors, i);
      print_message_with_location(error->message, &error->location);
    }
    return false;
  }

  token_match_module(result.root, program);
  return true;
}




