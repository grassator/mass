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

Token *
token_rewrite_constant_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
);

void
token_force_value(
  Program *program,
  Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
);

Value *
scope_lookup_force(
  Program *program,
  Scope *scope,
  Slice name,
  Function_Builder *builder
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
      Value *result = value_any();
      Token *token = token_rewrite_constant_expression(
        program, &(Token_Matcher_State){entry->lazy_constant_expression}, scope, builder
      );
      token_force_value(program, token, scope, builder, result);
      *entry = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .value = result,
      };
    }
  }



  Value *result = 0;
  if (dyn_array_length(*entries) == 1) {
    Scope_Entry *entry = dyn_array_get(*entries, 0);
    assert(entry->type == Scope_Entry_Type_Value);
    result = entry->value;
  } else {
    // Must be a function overload
    for (u64 i = 0; i < dyn_array_length(*entries); ++i) {
      Scope_Entry *entry = dyn_array_get(*entries, i);
      assert(entry->type == Scope_Entry_Type_Value);
      if (!result) {
        result = entry->value;
      } else {
        Value *overload = entry->value;
        overload->descriptor->function.next_overload = result;
        result = overload;
      }
      if (result->descriptor->type != Descriptor_Type_Function) {
        panic("Only functions should be lazy values");
      }
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
      Value *overload = scope_lookup_force(program, parent, name, builder);
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
  Array_Token_Ptr tokens
) {
  scope_define_internal(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Lazy_Constant_Expression,
    .lazy_constant_expression = tokens,
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

void
print_message_with_location(
  Slice message,
  Source_Location *location
) {
  printf(
    "%.*s(%llu:%llu): %.*s\n",
    u64_to_s32(location->filename.length),
    location->filename.bytes,
    location->line,
    location->column,
    u64_to_s32(message.length),
    message.bytes
  );
}

Tokenizer_Result
tokenize(
  Slice filename,
  Slice source
) {
  Array_Token_Ptr parent_stack = dyn_array_make(Array_Token_Ptr);
  Token *root = temp_allocate(Token);
  root->type = Token_Type_Module;
  root->children = dyn_array_make(Array_Token_Ptr);
  root->source = source;

  Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;

  Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);

#define start_token(_type_)\
  do {\
    current_token = temp_allocate(Token);\
    *current_token = (Token) {\
      .type = (_type_),\
      .source = {\
        .bytes = &source.bytes[i],\
        .length = 1,\
      },\
      .location = {\
        .filename = filename,\
        .line = line,\
        .column = column,\
      }\
    };\
  } while(0)

#define push\
  do {\
    dyn_array_push(parent->children, current_token);\
    current_token = 0;\
    state = Tokenizer_State_Default;\
  } while(0)

#define push_error(_message_)\
  dyn_array_push(errors, (Parse_Error) {\
    .message = slice_literal(_message_),\
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
        if (ch == '\n') {
          start_token(Token_Type_Newline);
          push;
        } else if (isspace(ch)) {
          continue;
        } else if (ch == '0' && peek == 'x') {
          start_token(Token_Type_Hex_Integer);
          i++;
          column++;
          current_token->source.length++;
          state = Tokenizer_State_Hex_Integer;
        } else if (isdigit(ch)) {
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
          dyn_array_push(parent_stack, parent);
          parent = current_token;
        } else if (ch == ')' || ch == '}' || ch == ']') {
          s8 expected_paren = 0;

          switch (parent->type) {
            case Token_Type_Paren: {
              expected_paren = ')';
              break;
            }
            case Token_Type_Curly: {
              expected_paren = '}';
              break;
            }
            case Token_Type_Square: {
              expected_paren = ']';
              break;
            }
            case Token_Type_Newline:
            case Token_Type_Value:
            case Token_Type_Id:
            case Token_Type_Integer:
            case Token_Type_Hex_Integer:
            case Token_Type_Operator:
            case Token_Type_Lazy_Function_Definition:
            case Token_Type_String:
            case Token_Type_Module: {
              panic("Tokenizer: unexpected closing char for group");
              break;
            }
          }
          if (ch != expected_paren) {
            push_error("Mismatched closing brace");
            goto end;
          }
          parent->source.length = &source.bytes[i] - parent->source.bytes + 1;
          if (!dyn_array_length(parent_stack)) {
            push_error("Encountered a closing brace without a matching open one");
            goto end;
          }
          parent = *dyn_array_pop(parent_stack);
          current_token = 0;
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
      case Tokenizer_State_Hex_Integer: {
        if (code_point_is_hex_digit(ch)) {
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
  dyn_array_destroy(parent_stack);
  if (dyn_array_length(errors)) {
    return (Tokenizer_Result){.type = Tokenizer_Result_Type_Error, .errors = errors};
  }
  return (Tokenizer_Result){.type = Tokenizer_Result_Type_Success, .root = root};
}

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
  Program *program,
  Scope *scope,
  Source_Location location,
  Slice type_name,
  Function_Builder *builder
) {
  Value *value = scope_lookup_force(program, scope, type_name, builder);
  if (!value) return 0;
  if (value->descriptor->type != Descriptor_Type_Type) {
    program_error_builder(program, location) {
      program_error_append_slice(type_name);
      program_error_append_literal(" is not a type");
    }
    return 0;
  }
  Descriptor *descriptor = value->descriptor->type_descriptor;
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
  if(peek_index != dyn_array_length(state->tokens)) return 0

typedef struct {
  Slice arg_name;
  Descriptor *type_descriptor;
} Token_Match_Arg;

Descriptor *
token_force_type(
  Program *program,
  Scope *scope,
  Token *token,
  Function_Builder *builder
) {
  Descriptor *descriptor = 0;
  switch (token->type) {
    case Token_Type_Id: {
      descriptor =
        scope_lookup_type(program, scope, token->location, token->source, builder);
      if (!descriptor) {
        program_error_builder(program, token->location) {
          program_error_append_literal("Could not find type ");
          program_error_append_slice(token->source);
        }
        return 0;
      }
      break;
    }
    case Token_Type_Square: {
      if (dyn_array_length(token->children) != 1) {
        program_push_error_from_slice(
          program,
          token->location,
          slice_literal("Pointer type must have a single type inside")
        );
        return 0;
      }
      Token *child = *dyn_array_get(token->children, 0);
      if (child->type != Token_Type_Id) {
        panic("TODO: should be recursive");
      }
      descriptor = temp_allocate(Descriptor);
      *descriptor = (Descriptor) {
        .type = Descriptor_Type_Pointer,
        .pointer_to =
          scope_lookup_type(program, scope, child->location, child->source, builder),
      };
      break;
    }
    case Token_Type_Hex_Integer:
    case Token_Type_Integer: {
      program_error_builder(program, token->location) {
        program_error_append_slice(token->source);
        program_error_append_literal(" is not a type");
      }
      return 0;
    }
    case Token_Type_Newline: {
      program_push_error_from_slice(
        program,
        token->location,
        slice_literal("Unexpected newline token")
      );
      return 0;
    }
    case Token_Type_Operator:
    case Token_Type_String:
    case Token_Type_Paren:
    case Token_Type_Curly:
    case Token_Type_Module:
    case Token_Type_Value:
    case Token_Type_Lazy_Function_Definition:
    default: {
      panic("TODO");
      break;
    }
  }
  return descriptor;
}

typedef Array_Token_Ptr (*token_pattern_callback)(
  Program *program,
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
  if (!pattern_length) panic("Zero-length pattern does not make sense");
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
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Array_Token_Ptr pattern,
  token_pattern_callback callback
) {
  Array_Token_Ptr match = token_match_pattern(state, pattern);
  if (!dyn_array_is_initialized(match)) return false;
  Array_Token_Ptr replacement = callback(program, match, scope, builder);
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
      case Token_Type_Newline:
      case Token_Type_Integer:
      case Token_Type_Hex_Integer:
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
        panic("Macro definitions should not contain semi-resolved tokens");
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
      case Token_Type_Newline:
      case Token_Type_Integer:
      case Token_Type_Hex_Integer:
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
        panic("Macro definitions should not contain semi-resolved tokens");
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
  if (dyn_array_length(macro->pattern_names) != dyn_array_length(match)) {
    panic("Should not have chosen the macro if pattern length do not match");
  }
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

Descriptor *
token_match_fixed_array_type(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
);

Descriptor *
token_match_type(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Descriptor *descriptor = token_match_fixed_array_type(program, state, scope, builder);
  if (descriptor) return descriptor;
  u64 length = dyn_array_length(state->tokens);
  if (!length) panic("Caller must not call token_match_type with empty token list");
  Token *token = *dyn_array_get(state->tokens, 0);
  if (length > 1) {
    program_push_error_from_slice(
      program,
      token->location,
      slice_literal("Can not resolve type")
    );
    return 0;
  }
  return token_force_type(program, scope, token, builder);
}

Token_Match_Arg *
token_match_argument(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
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
  Descriptor *type_descriptor = token_match_type(program, &rest_state, scope, builder);
  if (!type_descriptor) return 0;
  Token_Match_Arg *arg = temp_allocate(Token_Match_Arg);
  *arg = (Token_Match_Arg){name->source, type_descriptor};
  return arg;
}

Value *
token_force_lazy_function_definition(
  Program *program,
  Lazy_Function_Definition *lazy_function_definition,
  Function_Builder *builder
);

void
token_match_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *target
);

Slice
token_string_to_slice(
  Token *token
) {
  if (token->type != Token_Type_String) {
    panic("Caller is expected to only use this on string tokens");
  }
  if (token->source.length <= 2) {
    panic("Tokenizer (or somebody else) managed to produce a string without quotes");
  }
  return (Slice) {
    .bytes = token->source.bytes + 1,
    .length = token->source.length - 2
  };
}

void
token_force_value(
  Program *program,
  Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  switch(token->type) {
    case Token_Type_Integer: {
      bool ok = false;
      s64 number = slice_parse_s64(token->source, &ok);
      if (!ok) {
        program_error_builder(program, token->location) {
          program_error_append_literal("Invalid integer literal: ");
          program_error_append_slice(token->source);
        }
        return;
      }
      Value *immediate = value_from_signed_immediate(number);
      move_value(builder, &token->location, result_value, immediate);
      return;
    }
    case Token_Type_Hex_Integer: {
      bool ok = false;
      Slice digits = slice_sub(token->source, 2, token->source.length);
      u64 number = slice_parse_hex(digits, &ok);
      if (!ok) {
        program_error_builder(program, token->location) {
          program_error_append_literal("Invalid integer hex literal: ");
          program_error_append_slice(token->source);
        }
        return;
      }
      // TODO should be unsigned
      Value *immediate = value_from_signed_immediate(number);
      move_value(builder, &token->location, result_value, immediate);
      return;
    }
    case Token_Type_String: {
      Slice string = token_string_to_slice(token);
      Value *string_bytes = value_global_c_string_from_slice(program, string);
      Value *c_string_pointer = value_pointer_to(builder, &token->location, string_bytes);
      move_value(builder, &token->location, result_value, c_string_pointer);
      return;
    }
    case Token_Type_Id: {
      Value *value = scope_lookup_force(program, scope, token->source, builder);
      move_value(builder, &token->location, result_value, value);
      return;
    }
    case Token_Type_Value: {
      move_value(builder, &token->location, result_value, token->value);
      return;
    }
    case Token_Type_Lazy_Function_Definition: {
      Value *fn = token_force_lazy_function_definition(program, &token->lazy_function_definition, builder);
      if (fn) {
        move_value(builder, &token->location, result_value, fn);
      }
      return;
    }
    case Token_Type_Paren: {
      if (!builder) panic("Caller should only force (...) in a builder context");
      Token_Matcher_State state = {.tokens = token->children};
      token_match_expression(program, &state, scope, builder, result_value);
      return;
    }
    case Token_Type_Curly: {
      if (!builder) panic("Caller should only force {...} in a builder context");
      token_parse_block(program, token->children, scope, builder, result_value);
      return;
    }
    case Token_Type_Module:
    case Token_Type_Square:
    case Token_Type_Operator: {
      panic("TODO");
      return;
    }
    case Token_Type_Newline: {
      program_push_error_from_slice(
        program,
        token->location,
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
  Program *program,
  Token *token,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (dyn_array_length(token->children) != 0) {
    Array_Token_Matcher_State argument_states = token_split(token->children, &(Token){
      .type = Token_Type_Operator,
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
      Token_Matcher_State *state = dyn_array_get(argument_states, i);
      Value *result_value = value_any();
      token_match_expression(program, state, scope, builder, result_value);
      dyn_array_push(result, result_value);
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
  Program *program,
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
    .source = body->source,
    .lazy_function_definition = {
      .args = args,
      .return_types = return_types,
      .body = body,
      .program = program,
    },
  };
  dyn_array_clear(match);
  dyn_array_push(match, result_token);
  return match;
}

bool
token_rewrite_functions(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  Array_Token_Ptr pattern = {0};
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

  return token_rewrite_pattern(
    program, state, scope, builder_, pattern, token_rewrite_functions_pattern_callback
  );
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
  Scope *scope
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
token_match_struct_field(
  Program *program,
  Descriptor *struct_descriptor,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
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

  Descriptor *descriptor = token_match_type(program, &rest_state, scope, builder);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, name->source);
  return true;
}

void
token_rewrite_newlines_with_semicolons(
  Array_Token_Ptr children
) {
  // TODO remove this and instead support multiple matches in token_split?
  static Token fake_semicolon = {
    .type = Token_Type_Operator,
    .source = slice_literal_fields(";"),
  };
  for(u64 i = 0; i < dyn_array_length(children); ++i) {
    Token **token = dyn_array_get(children, i);
    if ((*token)->type == Token_Type_Newline) {
      *token = &fake_semicolon;
    }
  }
}

bool
token_rewrite_struct_definitions(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("struct"));
  Token_Match(body, .type = Token_Type_Curly);

  Value *result = temp_allocate(Value);
  Descriptor *struct_descriptor = temp_allocate(Descriptor);
  *struct_descriptor = (Descriptor) {
    .type = Descriptor_Type_Struct,
    .struct_ = {
      .fields = dyn_array_make(Array_Descriptor_Struct_Field),
    },
  };

  if (dyn_array_length(body->children) != 0) {
    token_rewrite_newlines_with_semicolons(body->children);
    Array_Token_Matcher_State definitions = token_split(body->children, &(Token){
      .type = Token_Type_Operator,
      .source = slice_literal(";"),
    });
    for (u64 i = 0; i < dyn_array_length(definitions); ++i) {
      Token_Matcher_State *field_state = dyn_array_get(definitions, i);
      token_match_struct_field(program, struct_descriptor, field_state, scope, builder);
    }
  }

  Descriptor *value_descriptor = temp_allocate(Descriptor);
  *value_descriptor = (Descriptor) {
    .type = Descriptor_Type_Type,
    .type_descriptor = struct_descriptor,
  };
  *result = (Value) {
    .descriptor = value_descriptor,
    .operand = {.type = Operand_Type_None },
  };

  token_replace_tokens_in_state(state, 2, token_value_make(body, result));

  return true;
}

bool
token_maybe_split_on_operator(
  Array_Token_Ptr tokens,
  Slice operator,
  Array_Token_Ptr *lhs,
  Array_Token_Ptr *rhs,
  Token **operator_token
) {
  u64 lhs_end = 0;
  u64 rhs_start = 0;
  for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
    Token *token = *dyn_array_get(tokens, i);
    if (token->type == Token_Type_Operator && slice_equal(token->source, operator)) {
      *operator_token = token;
      lhs_end = i;
      rhs_start = i + 1;
      break;
    }
  }

  if (lhs_end == 0) {
    *lhs = *rhs = (Array_Token_Ptr){0};
    return false;
  }

  *lhs = dyn_array_sub(Array_Token_Ptr, tokens, (Range_u64){ 0, lhs_end });
  *rhs = dyn_array_sub(Array_Token_Ptr, tokens, (Range_u64){ rhs_start, dyn_array_length(tokens) });

  return true;
}

Token *
token_import_match_arguments(
  Source_Location location,
  Token_Matcher_State *state,
  Program *program
) {
  Token *library_name_string = token_peek_match(state, 0, &(Token) {
    .type = Token_Type_String,
  });
  if (!library_name_string) {
    program_push_error_from_slice(
      program, location,
      slice_literal("First argument to external() must be a literal string")
    );
    return 0;
  }
  Token *comma = token_peek_match(state, 1, &(Token) {
    .type = Token_Type_Operator,
    .source = slice_literal(","),
  });
  if (!comma) {
    program_push_error_from_slice(
      program, location,
      slice_literal("external(\"library_name\", \"symbol_name\") requires two arguments")
    );
    return 0;
  }
  Token *symbol_name_string = token_peek_match(state, 2, &(Token) {
    .type = Token_Type_String,
  });
  if (!symbol_name_string) {
    program_push_error_from_slice(
      program, location,
      slice_literal("Second argument to external() must be a literal string")
    );
    return 0;
  }
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
  return token_value_make(library_name_string, result);
}

bool
token_rewrite_external_import(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("external"));
  Token_Match(args, .type = Token_Type_Paren);
  Token_Matcher_State *args_state = &(Token_Matcher_State) {.tokens = args->children };
  Token *result_token = token_import_match_arguments(args->location, args_state, program);
  if (!result_token) result_token = token_value_make(args, 0);

  token_replace_tokens_in_state(state, 2, result_token);
  return true;
}

inline Token_Matcher_State*
token_reset_state_start_index(
  Token_Matcher_State *state
) {
  state->start_index = 0;
  return state;
}

typedef bool (*token_rewrite_expression_callback)
(Program *program, Token_Matcher_State *, Scope *, Function_Builder *, Value *result_value);

void
token_rewrite_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  token_rewrite_expression_callback callback
) {
  start: for (;;) {
    for (u64 i = 0; i < dyn_array_length(state->tokens); ++i) {
      state->start_index = i;
      if (callback(program, state, scope, builder, result_value)) goto start;
    }
    break;
  }
  state->start_index = 0;
}

typedef bool (*token_rewrite_statement_callback)
(Program *program, Token_Matcher_State *, Scope *, Function_Builder *);

void
token_rewrite_statement(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  token_rewrite_statement_callback callback
) {
  start: for (;;) {
    for (u64 i = 0; i < dyn_array_length(state->tokens); ++i) {
      state->start_index = i;
      if (callback(program, state, scope, builder)) goto start;
    }
    break;
  }
  state->start_index = 0;
}

bool
token_rewrite_negative_literal(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  // FIXME Allow unary minus on any expression
  Token_Match_Operator(define, "-");
  Token_Match(integer, .type = Token_Type_Integer);
  Value *result = value_any();
  token_force_value(program, integer, scope, builder, result);
  if (result->operand.type == Operand_Type_Immediate_8) {
    result->operand.imm8 = -result->operand.imm8;
  } else if (result->operand.type == Operand_Type_Immediate_32) {
    result->operand.imm32 = -result->operand.imm32;
  } else if (result->operand.type == Operand_Type_Immediate_64) {
    result->operand.imm64 = -result->operand.imm64;
  } else {
    panic("Internal error, expected an immediate");
  }

  token_replace_tokens_in_state(state, 2, token_value_make(integer, result));
  return true;
}

bool
token_clear_newlines(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(newline, .type = Token_Type_Newline);
  token_replace_tokens_in_state(state, 1, 0);
  return true;
}

bool
token_rewrite_constant_sub_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(paren, .type = Token_Type_Paren);

  Token_Matcher_State sub_state = {paren->children};
  Token *result_token = token_rewrite_constant_expression(program, &sub_state, scope, builder);
  if (!result_token) {
    return false;
  }
  token_replace_tokens_in_state(state, 1, result_token);
  return true;
}

bool
token_rewrite_compile_time_eval(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *unused_builder
) {
  u64 peek_index = 0;
  Token_Match_Operator(at, "@");
  Token_Match(paren, .type = Token_Type_Paren);

  Token_Matcher_State sub_state = {paren->children};

  Program eval_program = {
    .data_buffer = program->data_buffer,
    .import_libraries = dyn_array_copy(Array_Import_Library, program->import_libraries),
    .functions = dyn_array_copy(Array_Function_Builder, program->functions),
    .global_scope = scope_make(program->global_scope),
    .errors = dyn_array_make(Array_Parse_Error),
  };
  Function_Builder *eval_builder = fn_begin(&eval_program);
  function_return_descriptor(&eval_builder->descriptor->function, &descriptor_void);

  Value *expression_result_value = value_any();
  token_match_expression(
    &eval_program, &sub_state, eval_program.global_scope, eval_builder, expression_result_value
  );

  // We use a something like a C++ reference out parameter for the
  // result to have a consitent function signature on this C side of things.

  // Make it out parameter a pointer to ensure it is passed inside a register according to ABI
  Value *arg_value = function_push_argument(
    &eval_builder->value->descriptor->function,
    descriptor_pointer_to(expression_result_value->descriptor)
  );

  // Create a reference Value
  assert(arg_value->operand.type == Operand_Type_Register);
  Value *out_value = temp_allocate(Value);
  *out_value = (Value) {
    .descriptor = expression_result_value->descriptor,
    .operand = (Operand){
      .type = Operand_Type_Memory_Indirect,
      .byte_size = expression_result_value->operand.byte_size,
      .indirect = { .reg = arg_value->operand.reg },
    },
  };

  move_value(eval_builder, &paren->location, out_value, expression_result_value);
  fn_end(eval_builder);

  program_jit(&eval_program);

  u32 result_byte_size = out_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(temp_allocator, result_byte_size, alignment);

  fn_type_voidp_to_void jitted_code =
    (fn_type_voidp_to_void)helper_value_as_function(&eval_program, eval_builder->value);

  jitted_code(result);
  Value *token_value = temp_allocate(Value);
  *token_value = (Value) {
    .descriptor = out_value->descriptor,
  };
  switch(out_value->descriptor->type) {
    case Descriptor_Type_Float:
    case Descriptor_Type_Integer: {
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
      break;
    }
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
    case Descriptor_Type_Function:
    case Descriptor_Type_Type: {
      panic("TODO figure out how that works");
      break;
    }
  }

  Token *result_token = token_value_make(paren, token_value);

  if (!result_token) {
    return false;
  }
  token_replace_tokens_in_state(state, 2, result_token);
  return true;
}

Token *
token_rewrite_constant_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  token_rewrite_statement(program, state, scope, builder, token_clear_newlines);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_negative_literal);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_external_import);

  token_rewrite_struct_definitions(program, token_reset_state_start_index(state), scope, builder) ||
  token_rewrite_functions(program, token_reset_state_start_index(state), scope, builder) ||
  token_rewrite_compile_time_eval(program, token_reset_state_start_index(state), scope, builder);

  token_rewrite_statement(program, state, scope, builder, token_rewrite_constant_sub_expression);

  if (dyn_array_length(state->tokens) != 1) {
    // FIXME :LocationInfoMissing
    program_push_error_from_slice(
      program, (Source_Location){0}, slice_literal("Invalid constant definition")
    );
    return 0;
  }
  return *dyn_array_get(state->tokens, 0);
}

bool
token_rewrite_constant_definitions(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(state->tokens, slice_literal("::"), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (dyn_array_length(lhs) > 1) return false;
  Token *name = *dyn_array_get(lhs, 0);
  if (name->type != Token_Type_Id) return false;

  Token_Matcher_State rhs_state = {rhs};
  Token *token_value = token_rewrite_constant_expression(program, &rhs_state, scope, builder);
  if (!token_value) {
    return false;
  }

  // TODO turn all constant definitions into lazy scope definitions
  if (token_value->type == Token_Type_Lazy_Function_Definition) {
    Array_Token_Ptr tokens = dyn_array_make(Array_Token_Ptr);
    dyn_array_push(tokens, token_value);
    scope_define_lazy(scope, name->source, tokens);
  } else {
    Value *result = value_any();
    token_force_value(program, token_value, scope, builder, result);
    scope_define_value(scope, name->source, result);
  }

  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

void
token_match_statement(
  Program *program,
  Token_Matcher_State *state,
  const Source_Location *location,
  Scope *scope,
  Function_Builder *builder,
  Value *statement_result_value
);

void
token_parse_block(
  Program *program,
  Array_Token_Ptr children,
  Scope *scope,
  Function_Builder *builder,
  Value *block_result_value
) {
  if (!dyn_array_length(children)) return;
  Scope *block_scope = scope_make(scope);

  // Newlines at the end of the block do not count as semucolons otherwise this:
  // { 42
  // }
  // is being interpreted as:
  // { 42 ; }
  while ((*dyn_array_last(children))->type == Token_Type_Newline) dyn_array_pop(children);

  token_rewrite_newlines_with_semicolons(children);

  Array_Token_Matcher_State block_statements = token_split(children, &(Token){
    .type = Token_Type_Operator,
    .source = slice_literal(";"),
  });
  for (u64 i = 0; i < dyn_array_length(block_statements); ++i) {
    Token_Matcher_State *state = dyn_array_get(block_statements, i);
    if (!dyn_array_length(state->tokens)) continue;
    bool is_last_statement = i + 1 == dyn_array_length(block_statements);
    Value *result_value = is_last_statement ? block_result_value : value_any();
    // FIXME get the real location
    token_match_statement(program, state, &(Source_Location){0}, block_scope, builder, result_value);
  }
  return;
}

bool
token_rewrite_statement_if(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("if"));
  Token_Match(condition, .type = Token_Type_Paren);
  Token_Match(body, .type = Token_Type_Curly);
  Token_Match_End();

  Value *condition_value = value_any();
  token_force_value(program, condition, scope, builder, condition_value);
  Label *else_label = make_if(&builder->code_block.instructions, &keyword->location, condition_value);
  token_parse_block(program, body->children, scope, builder, value_any());
  push_instruction(
    &builder->code_block.instructions, &keyword->location, (Instruction) {.maybe_label = else_label}
  );

  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

bool
token_rewrite_goto(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("goto"));
  Token_Match(label_name, .type = Token_Type_Id);
  Token_Match_End();
  Value *value = scope_lookup_force(program, scope, label_name->source, builder);
  if (value) {
    if (
      value->descriptor->type == Descriptor_Type_Void &&
      value->operand.type == Operand_Type_Label_32
    ) {
      push_instruction(
        &builder->code_block.instructions, &keyword->location, (Instruction) {jmp, {value->operand, 0, 0}}
      );
    } else {
      program_error_builder(program, label_name->location) {
        program_error_append_slice(label_name->source);
        program_error_append_literal(" is not a label");
      }
    }
  }

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_explicit_return(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("return"));
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){peek_index, dyn_array_length(state->tokens)}
  );
  bool has_return_expression = dyn_array_length(rest) > 0;
  Value *fn_return = builder->descriptor->function.returns;
  token_match_expression(program, &(Token_Matcher_State){rest}, scope, builder, fn_return);

  bool is_void = fn_return->descriptor->type == Descriptor_Type_Void;
  if (!is_void && !has_return_expression) {
    program_push_error_from_slice(
      program, keyword->location,
      slice_literal("Explicit return from a non-void function requires a value")
    );
  }

  push_instruction(
    &builder->code_block.instructions,
    &keyword->location,
    (Instruction){jmp, {label32(builder->code_block.end_label), 0, 0}}
  );

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_pointer_to(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match_Operator(operator, "&");
  Token_Match(value_token, 0);

  Value *pointee = value_any();
  token_force_value(program, value_token, scope, builder, pointee);
  Value *result = value_pointer_to(builder, &operator->location, pointee);
  token_replace_tokens_in_state(state, 2, token_value_make(value_token, result));
  return true;
}

Descriptor *
token_match_fixed_array_type(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(type, .type = Token_Type_Id);
  Token_Match(square_brace, .type = Token_Type_Square);
  Descriptor *descriptor = scope_lookup_type(program, scope, type->location, type->source, builder);

  Token_Matcher_State size_state = {.tokens = square_brace->children};
  // FIXME :TargetValue Make a convention to have this as a constant / immediate
  Value *size_value = value_any();
  token_match_expression(program, &size_state, scope, builder, size_value);
  if (size_value->descriptor->type != Descriptor_Type_Integer) {
    program_push_error_from_slice(
      program,
      square_brace->location,
      slice_literal("Fixed size array size is not an integer")
    );
    return 0;
  }
  if (!operand_is_immediate(&size_value->operand)) {
    program_push_error_from_slice(
      program,
      square_brace->location,
      slice_literal("Fixed size array size must be known at compile time")
    );
    return 0;
  }
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
token_rewrite_cast(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(cast, .type = Token_Type_Id, .source = slice_literal("cast"));
  Token_Match(value_token, .type = Token_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(program, value_token, scope, builder);
  assert(dyn_array_length(args) == 2);
  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  assert(type->descriptor->type == Descriptor_Type_Type);

  Descriptor *cast_to_descriptor = type->descriptor->type_descriptor;
  assert(cast_to_descriptor->type == Descriptor_Type_Integer);
  assert(value->descriptor->type == cast_to_descriptor->type);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *result = value;
  if (cast_to_byte_size != original_byte_size) {
    result = temp_allocate(Value);
    if (cast_to_byte_size < original_byte_size) {
      *result = (Value) {
        .descriptor = cast_to_descriptor,
        .operand = value->operand,
      };
      result->operand.byte_size = cast_to_byte_size;
    } else if (cast_to_byte_size > original_byte_size) {
      result = reserve_stack(builder, cast_to_descriptor);
      move_value(builder, &cast->location, result, value);
    }
  }

  token_replace_tokens_in_state(state, 2, token_value_make(value_token, result));
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

bool
token_rewrite_definitions(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":");

  u64 start_index = state->start_index + peek_index;
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){start_index, dyn_array_length(state->tokens)}
  );
  Token_Matcher_State rest_state = {.tokens = rest};
  u64 size_to_replace = dyn_array_length(state->tokens) - state->start_index;

  Label *label = token_match_label(&rest_state, scope, builder);
  Value *value = 0;
  if (label) {
    push_instruction(
      &builder->code_block.instructions, &name->location, (Instruction) { .maybe_label = label }
    );
    value = temp_allocate(Value);
    *value = (Value) {
      .descriptor = &descriptor_void,
      .operand = label32(label),
    };
  } else {
    Descriptor *descriptor = token_match_type(program, &rest_state, scope, builder);
    value = reserve_stack(builder, descriptor);
  }
  scope_define_value(scope, name->source, value);
  token_replace_tokens_in_state(state, size_to_replace, token_value_make(name, value));

  return true;
}

bool
token_rewrite_definition_and_assignment_statements(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(state->tokens, slice_literal(":="), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (dyn_array_length(lhs) > 1) return false;
  Token *name = *dyn_array_get(lhs, 0);

  if (name->type != Token_Type_Id) return false;

  Token_Matcher_State rhs_state = {rhs};
  Value *value = value_any();
  token_match_expression(program, &rhs_state, scope, builder, value);

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (
    value->descriptor->type == Descriptor_Type_Integer &&
    operand_is_immediate(&value->operand)
  ) {
    value = value_from_s64(operand_immediate_as_s64(&value->operand));
  } else if (
    value->descriptor->type == Descriptor_Type_Float &&
    operand_is_immediate(&value->operand)
  ) {
    panic("TODO decide how to handle floats");
  }
  Value *on_stack = reserve_stack(builder, value->descriptor);
  move_value(builder, &name->location, on_stack, value);

  scope_define_value(scope, name->source, on_stack);

  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

bool
token_rewrite_array_index(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *target
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(brackets, .type = Token_Type_Square);

  Value *array = value_any();
  token_force_value(program, target_token, scope, builder, array);
  Token_Matcher_State *index_state = &(Token_Matcher_State) {brackets->children};
  Value *index_value = value_any();
  token_match_expression(program, index_state, scope, builder, index_value);
  assert(array->descriptor->type == Descriptor_Type_Fixed_Size_Array);
  assert(array->operand.type == Operand_Type_Memory_Indirect);

  Descriptor *item_descriptor = array->descriptor->array.item;
  u32 item_byte_size = descriptor_byte_size(item_descriptor);

  Value *result = temp_allocate(Value);
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
      value_register_for_descriptor(Register_R10, index_value->descriptor);
    move_value(builder, &target_token->location, index_value_in_register, index_value);
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

  token_replace_tokens_in_state(state, 2, token_value_make(target_token, result));
  return true;
}

bool
token_rewrite_struct_field(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(struct_token, 0);
  Token_Match_Operator(dot, ".");
  Token_Match(field_name, .type = Token_Type_Id);

  Value *struct_value = value_any();
  token_force_value(program, struct_token, scope, builder, struct_value);
  Value *result = struct_get_field(struct_value, field_name->source);

  token_replace_tokens_in_state(state, 3, token_value_make(struct_token, result));
  return true;
}

bool
token_rewrite_assignment(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;
  if (!token_maybe_split_on_operator(state->tokens, slice_literal("="), &lhs, &rhs, &operator)) {
    return false;
  }

  Token_Matcher_State lhs_state = {lhs};
  token_rewrite_expression(program, &lhs_state, scope, builder, 0, token_rewrite_array_index);
  token_rewrite_statement(program, &lhs_state, scope, builder, token_rewrite_struct_field);
  token_rewrite_statement(program, &lhs_state, scope, builder, token_rewrite_definitions);
  if (!dyn_array_length(lhs_state.tokens)) {
    panic("Left hand side is checked to be non-empty when matched so something went wrong");
  }
  Value *target = value_any();
  if (dyn_array_length(lhs_state.tokens) == 1) {
    Token *token = *dyn_array_get(lhs_state.tokens, 0);
    token_force_value(program, token, scope, builder, target);
  } else {
    Token *first_token = *dyn_array_get(lhs_state.tokens, 0);
    program_push_error_from_slice(
      program,
      first_token->location,
      slice_literal("Could not parse the target of the assignment")
    );
  }

  Token_Matcher_State rhs_state = {rhs};
  token_match_expression(program, &rhs_state, scope, builder, target);

  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

bool
token_rewrite_function_calls(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(args_token, .type = Token_Type_Paren);
  if (target_token->type != Token_Type_Id && target_token->type != Token_Type_Paren) return false;

  Value *target = value_any();
  token_force_value(program, target_token, scope, builder, target);
  Array_Value_Ptr args = token_match_call_arguments(program, args_token, scope, builder);

  if (target->descriptor->type != Descriptor_Type_Function) {
    program_error_builder(program, target_token->location) {
      program_error_append_slice(target_token->source);
      program_error_append_literal(" is not a function");
    }
    return false;
  }
  Value *return_value = call_function_value_array(builder, &target_token->location, target, args);
  dyn_array_destroy(args);
  token_replace_tokens_in_state(state, 2, token_value_make(args_token, return_value));
  return true;
}

bool
token_rewrite_plus(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "+");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(program, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(program, rhs, scope, builder, rhs_value);
  //Value *temp = result_value ? result_value : reserve_stack(builder, lhs_value->descriptor);
  plus(builder, &op_token->location, result_value, lhs_value, rhs_value);
  token_replace_tokens_in_state(state, 3, token_value_make(op_token, result_value));
  return true;
}

bool
token_rewrite_minus(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "-");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(program, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(program, rhs, scope, builder, rhs_value);
  minus(builder, &op_token->location, result_value, lhs_value, rhs_value);
  token_replace_tokens_in_state(state, 3, token_value_make(op_token, result_value));
  return true;
}


bool
token_rewrite_divide(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(operator, "/");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(program, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(program, rhs, scope, builder, rhs_value);
  divide(builder, &operator->location, result_value, lhs_value, rhs_value);
  token_replace_tokens_in_state(state, 3, token_value_make(operator, result_value));
  return true;
}

bool
token_rewrite_remainder(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(operator, "%");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(program, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(program, rhs, scope, builder, rhs_value);
  Value *temp = reserve_stack(builder, lhs_value->descriptor);
  value_remainder(builder, &operator->location, temp, lhs_value, rhs_value);
  token_replace_tokens_in_state(state, 3, token_value_make(operator, temp));
  return true;
}

bool
token_rewrite_compare(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match(operator, .type = Token_Type_Operator);
  Compare_Type compare_type = 0;
  switch(operator->source.length) {
    case 1: {
      s8 byte1 = operator->source.bytes[0];
      if (byte1 == '<') {
        compare_type = Compare_Type_Less;
      } else if (byte1 == '>') {
        compare_type = Compare_Type_Greater;
      } else {
        return 0;
      }
      break;
    }
    case 2: {
      s8 byte1 = operator->source.bytes[0];
      s8 byte2 = operator->source.bytes[1];
      if (byte1 == '<' && byte2 == '=') {
        compare_type = Compare_Type_Less_Equal;
      } else if (byte1 == '>' && byte2 == '=') {
        compare_type = Compare_Type_Greater_Equal;
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

  Value *lhs_value = value_any();
  token_force_value(program, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(program, rhs, scope, builder, rhs_value);
  compare(compare_type, builder, &operator->location, result_value, lhs_value, rhs_value);
  token_replace_tokens_in_state(state, 3, token_value_make(operator, result_value));
  return true;
}

void
token_match_statement(
  Program *program,
  Token_Matcher_State *state,
  const Source_Location *location,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  // TODO consider how this should work
  token_rewrite_macros(token_reset_state_start_index(state), scope, builder);

  if (
    token_rewrite_assignment(program, token_reset_state_start_index(state), scope, builder) ||
    token_rewrite_definition_and_assignment_statements(
      program, token_reset_state_start_index(state), scope, builder
    ) ||
    token_rewrite_definitions(program, token_reset_state_start_index(state), scope, builder) ||
    token_rewrite_explicit_return(program, token_reset_state_start_index(state), scope, builder) ||
    token_rewrite_goto(program, token_reset_state_start_index(state), scope, builder) ||
    token_rewrite_constant_definitions(program, token_reset_state_start_index(state), scope, builder)||
    token_rewrite_statement_if(program, token_reset_state_start_index(state), scope, builder)
  ) {
    return;
  }
  token_match_expression(program, state, scope, builder, result_value);
}

void
token_match_expression(
  Program *program,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  if (!dyn_array_length(state->tokens)) {
    return;
  }
  token_rewrite_statement(program, state, scope, builder, token_clear_newlines);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_cast);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_struct_field);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_functions);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_negative_literal);
  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_function_calls);
  token_rewrite_statement(program, state, scope, builder, token_rewrite_pointer_to);

  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_divide);
  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_remainder);
  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_plus);
  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_minus);

  token_rewrite_expression(program, state, scope, builder, result_value, token_rewrite_compare);

  switch(dyn_array_length(state->tokens)) {
    case 0: {
      return;
    }
    case 1: {
      Token *token = *dyn_array_get(state->tokens, 0);
      token_force_value(program, token, scope, builder, result_value);
      return;
    }
    default: {
      Token *token = *dyn_array_get(state->tokens, 0);
      program_push_error_from_slice(
        program, token->location, slice_literal("Could not parse the expression")
      );
      return;
    }
  }
}

Value *
token_force_lazy_function_definition(
  Program *program,
  Lazy_Function_Definition *lazy_function_definition,
  Function_Builder *outer_builder
) {
  Token *args = lazy_function_definition->args;
  Token *return_types = lazy_function_definition->return_types;
  Token *body = lazy_function_definition->body;
  //Program *program = lazy_function_definition->program;
  Scope *function_scope = scope_make(program->global_scope);

  Function_Builder *builder = 0;
  // TODO think about a better way to distinguish imports
  bool is_external = body->type == Token_Type_Value;
  Descriptor *descriptor = 0;

  if (is_external) {
    if(!body->value) return 0;
    descriptor = temp_allocate(Descriptor);
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Function,
      .function = {
        .arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator),
        .returns = 0,
      },
    };
  } else {
    builder = fn_begin(program);
    descriptor = builder->descriptor;
  }

  switch (dyn_array_length(return_types->children)) {
    case 0: {
      descriptor->function.returns = &void_value;
      break;
    }
    case 1: {
      Token *return_type_token = *dyn_array_get(return_types->children, 0);
      Descriptor *return_descriptor =
        token_force_type(program, function_scope, return_type_token, outer_builder);
      if (!return_descriptor) return 0;
      function_return_descriptor(&descriptor->function, return_descriptor);
      break;
    }
    default: {
      panic("Multiple return types are not supported at the moment");
      break;
    }
  }

  if (dyn_array_length(args->children) != 0) {
    Array_Token_Matcher_State argument_states = token_split(args->children, &(Token){
      .type = Token_Type_Operator,
      .source = slice_literal(","),
    });
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_Matcher_State *state = dyn_array_get(argument_states, i);
      token_rewrite_statement(program, state, function_scope, builder, token_clear_newlines);
      Token_Match_Arg *arg = token_match_argument(program, state, function_scope, outer_builder);
      if (!arg) return 0;
      Value *arg_value = function_push_argument(&descriptor->function, arg->type_descriptor);
      if (!is_external && arg_value->operand.type == Operand_Type_Register) {
        register_bitset_set(&builder->code_block.register_occupied_bitset, arg_value->operand.reg);
      }
      scope_define_value(function_scope, arg->arg_name, arg_value);
    }
  }

  if (is_external) {
    body->value->descriptor = descriptor;
    return body->value;
  }

  Value *return_result_value =
    descriptor->function.returns->descriptor->type == Descriptor_Type_Void
      ? value_any()
      : descriptor->function.returns;
  token_parse_block(program, body->children, function_scope, builder, return_result_value);

  fn_end(builder);
  return builder->value;
}

bool
token_match_module(
  Token *token,
  Program *program
) {
  if (token->type != Token_Type_Module) {
    panic("Caller must provide a value known to be a module");
  }
  if (!dyn_array_length(token->children)) return true;

  Function_Builder global_builder = { 0 };

  token_rewrite_newlines_with_semicolons(token->children);

  Array_Token_Matcher_State module_statements = token_split(token->children, &(Token){
    .type = Token_Type_Operator,
    .source = slice_literal(";"),
  });
  for (u64 i = 0; i < dyn_array_length(module_statements); ++i) {
    Token_Matcher_State *state = dyn_array_get(module_statements, i);
    if (!dyn_array_length(state->tokens)) continue;
    token_rewrite_macro_definitions(state, program->global_scope);
    token_rewrite_statement(
      program, state, program->global_scope, &global_builder, token_rewrite_constant_definitions
    );
    // FIXME detect unmatched statements
  }

  return true;
}

Parse_Result
program_parse(
  Program *program,
  Slice file_path,
  Slice source
) {
  Tokenizer_Result tokenizer_result = tokenize(file_path, source);
  if (tokenizer_result.type != Tokenizer_Result_Type_Success) {
    return (Parse_Result) {
      .type = Parse_Result_Type_Error,
      .errors = tokenizer_result.errors,
    };
  }
  ;
  return (Parse_Result) {
    .type = token_match_module(tokenizer_result.root, program)
      ? Parse_Result_Type_Success
      : Parse_Result_Type_Error
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
    result_path = utf16_null_terminated_to_utf8(temp_allocator, wide_string);
  }
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = result_path.length + 1024
  );

  fixed_buffer_append_slice(result_buffer, result_path);

  if (sys_buffer) fixed_buffer_destroy(sys_buffer);
  return result_buffer;
}

#define STRINGIFY(x) #x

Parse_Result
program_import_file(
  Program *program,
  Slice file_path
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = win32_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) {
    Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);
    dyn_array_push(errors, (Parse_Error) {
      .message = slice_literal("Unable to open the file"),
      .location = {
        .filename = file_path,
      },
    });
    return (Parse_Result) {
      .type = Parse_Result_Type_Error,
      .errors = errors,
    };
  }
  Slice source = fixed_buffer_as_slice(buffer);

  return program_parse(program, file_path, source);
}

