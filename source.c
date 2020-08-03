#include "prelude.h"
#include "value.h"
#include "function.h"

typedef enum {
  Token_Type_Id = 1,
  Token_Type_Integer,
  Token_Type_Operator,
  Token_Type_String,
  Token_Type_Paren,
  Token_Type_Square,
  Token_Type_Curly,
  Token_Type_Module,
  Token_Type_Value,
  Token_Type_Lazy_Function_Definition,
} Token_Type;

typedef struct Token Token;
typedef dyn_array_type(Token *) Array_Token_Ptr;

typedef struct {
  Token *args;
  Token *return_types;
  Token *body;
  Program *program;
} Lazy_Function_Definition;

typedef struct Token {
  struct Token *parent;
  Token_Type type;
  Slice source;
  union {
    Array_Token_Ptr children;
    Value *value;
    Lazy_Function_Definition lazy_function_definition;
  };
} Token;
typedef dyn_array_type(Token) Array_Token;

typedef enum {
  Tokenizer_State_Default,
  Tokenizer_State_Integer,
  Tokenizer_State_Operator,
  Tokenizer_State_Id,
  Tokenizer_State_String,
  Tokenizer_State_Single_Line_Comment,
} Tokenizer_State;

typedef enum {
  Scope_Entry_Type_Value = 1,
  Scope_Entry_Type_Lazy,
} Scope_Entry_Type;

typedef struct {
  Scope_Entry_Type type;
  union {
    Value *value;
    Token *token;
  };
} Scope_Entry;

hash_map_slice_template(Scope_Map, Scope_Entry)

typedef struct Scope {
  struct Scope *parent;
  Scope_Map *map;
} Scope;


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
  Scope *scope
);

Value *
scope_lookup_force(
  Scope *scope,
  Slice name
) {
  Scope_Entry *entry = scope_lookup(scope, name);
  assert(entry);
  switch(entry->type) {
    case Scope_Entry_Type_Value: {
      return entry->value;
      break;
    }
    case Scope_Entry_Type_Lazy: {
      return token_force_value(entry->token, scope);
    }
  }
  assert(!"Not reached");
  return 0;
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
  // TODO think about what should happen when trying to redefine existing thing
  Scope_Entry entry = {
    .type = Scope_Entry_Type_Lazy,
    .token = token,
  };
  hash_map_set(scope->map, name, entry);
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
  const char *filename;
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
  printf("%s(%llu:%llu): %s\n", location->filename, location->line, location->column, message);
}

Tokenizer_Result
tokenize(
  const char *filename,
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
        } else if (isalpha(ch)) {
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
        if (isalpha(ch) || isdigit(ch)) {
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
  Token *root;
  u64 child_index;
} Token_Matcher_State;

Token *
token_peek(
  Token_Matcher_State *state,
  u64 delta
) {
  u64 index = state->child_index + delta;
  Token **peek = dyn_array_peek(state->root->children, index);
  if (!peek) return 0;
  return *peek;
}

Token *
token_peek_match(
  Token_Matcher_State *state,
  u64 delta,
  Token *pattern_token
) {
  Token *source_token = token_peek(state, delta);
  if (!source_token) return 0;
  if (pattern_token->type && pattern_token->type != source_token->type) {
    return 0;
  }
  if (
    pattern_token->source.length &&
    !slice_equal(pattern_token->source, source_token->source)
  ) {
    return 0;
  }
  return source_token;
}

Descriptor *
program_lookup_type(
  Program *program,
  Slice type_name
) {
  Scope_Entry *entry = scope_lookup(program->global_scope, type_name);
  assert(entry);
  assert(entry->type == Scope_Entry_Type_Value);
  assert(entry->value->descriptor->type == Descriptor_Type_Type);
  Descriptor *descriptor = entry->value->descriptor->type_descriptor;
  assert(descriptor);
  return descriptor;
}

typedef struct {
  Slice arg_name;
  Slice type_name;
} Token_Match_Arg;

#define Maybe_Token_Match(_id_, ...)\
  Token *(_id_) = token_peek_match(state, peek_index++, &(Token) { __VA_ARGS__ });\

#define Token_Match(_id_, ...)\
  Maybe_Token_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .type = Token_Type_Operator, .source = slice_literal(_op_))


Token_Match_Arg *
token_match_argument(
  Token_Matcher_State *state,
  Program *program_
) {
  u64 peek_index = 0;
  Token_Match(arg_id, .type = Token_Type_Id);
  Token_Match_Operator(arg_colon, ":");
  Token_Match(arg_type, .type = Token_Type_Id);
  Maybe_Token_Match(comma, .type = Token_Type_Operator, .source = slice_literal(","));
  if (!comma) peek_index--;
  state->child_index += peek_index;

  Token_Match_Arg *result = temp_allocate(Token_Match_Arg);
  *result = (Token_Match_Arg) {
    .arg_name = arg_id->source,
    .type_name = arg_type->source,
  };
  return result;
}

Value *
token_force_lazy_function_definition(
  Lazy_Function_Definition *lazy_function_definition
);

Value *
token_force_value(
  Token *token,
  Scope *scope
) {
  Value *result_value = 0;
  if (token->type == Token_Type_Integer) {
    Slice_Parse_S64_Result parse_result = slice_parse_s64(token->source);
    assert(parse_result.success);
    // FIXME We should be able to size immediates automatically
    result_value = value_from_s32((s32)parse_result.value);
  } else if (token->type == Token_Type_Id) {
    result_value = scope_lookup_force(scope, token->source);
  } else if (token->type == Token_Type_Value) {
    result_value = token->value;
  } else if (token->type == Token_Type_Lazy_Function_Definition) {
    return token_force_lazy_function_definition(&token->lazy_function_definition);
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
  if (dyn_array_length(token->children) == 0) {
    // Nothing to do
  } else if (dyn_array_length(token->children) == 1) {
    Value *value = token_force_value(*dyn_array_get(token->children, 0), scope);
    dyn_array_push(result, value);
  } else {
    // FIXME implement this;
    assert(!"Not implemented");
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

Value *
token_match_expression(
  Token *token,
  Scope *scope,
  Function_Builder *builder_
) {
  assert(token->type == Token_Type_Paren || token->type == Token_Type_Curly);

  if (!dyn_array_length(token->children)) {
    return 0;
  }

  // Match Function Calls
  Token_Matcher_State *state = &(Token_Matcher_State) {.root = token};
  for (bool did_replace = true; did_replace;) {
    did_replace = false;
    for (u64 i = 0; i < dyn_array_length(token->children); ++i) {
      state->child_index = i;
      Token *args_token = token_peek_match(state, 1, &(Token) { .type = Token_Type_Paren, });
      if (!args_token) continue;
      Token *maybe_target = token_peek(state, 0);
      if (maybe_target->type != Token_Type_Id && maybe_target->type != Token_Type_Paren) continue;

      Value *target = token_force_value(maybe_target, scope);
      Array_Value_Ptr args = token_match_call_arguments(args_token, scope, builder_);

      Value *result = call_function_value_array(builder_, target, args);
      Token *result_token = token_value_make(args_token, result);

      *dyn_array_get(token->children, i) = result_token;
      dyn_array_delete(token->children, i + 1);
      did_replace |= true;
    }
  };

  state = &(Token_Matcher_State) {.root = token};
  for (bool did_replace = true; did_replace;) {
    did_replace = false;
    for (u64 i = 0; i < dyn_array_length(token->children); ++i) {
      state->child_index = i;
      Token *plus_token = token_peek_match(state, 1, &(Token) {
        .type = Token_Type_Operator,
        .source = slice_literal("+"),
      });
      if (!plus_token) continue;

      Value *lhs = token_force_value(token_peek(state, 0), scope);
      Value *rhs = token_force_value(token_peek(state, 2), scope);

      Value *result = Plus(lhs, rhs);
      Token *result_token = token_value_make(plus_token, result);
      *dyn_array_get(token->children, i) = result_token;
      dyn_array_delete_range(token->children, (Range_u64){i + 1, i + 3});
      did_replace |= true;
    }
  };

  assert(dyn_array_length(token->children) == 1);
  return token_force_value(*dyn_array_get(token->children, 0), scope);
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
      Token_Matcher_State args_state = { .root = args };

      u64 prev_child_index = 0;
      do {
        prev_child_index = args_state.child_index;
        Token_Match_Arg *arg = token_match_argument(&args_state, program_);
        if (arg) {
          Arg(arg_value, program_lookup_type(program_, arg->type_name));
          scope_define_value(function_scope, arg->arg_name, arg_value);
        }
      } while (prev_child_index != args_state.child_index);
      assert(args_state.child_index == dyn_array_length(args->children));
    }

    switch (dyn_array_length(return_types->children)) {
      case 0: {
        value->descriptor->function.returns = &void_value;
        break;
      }
      case 1: {
        Token *return_type_token = *dyn_array_get(return_types->children, 0);
        assert(return_type_token->type == Token_Type_Id);

        fn_return_descriptor(
          builder_,
          program_lookup_type(program_, return_type_token->source),
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
      Value *body_result = token_match_expression(body, function_scope, builder_);
      if (body_result) {
        fn_return(builder_, body_result, Function_Return_Type_Implicit);
      }
    }
  }
  return value;
}

Slice
token_string_to_slice(
  Token *token
) {
  assert(token->source.length >= 2);
  return (Slice) {
    .bytes = token->source.bytes + 1,
    .length = token->source.length - 2
  };
}

Token *
token_import_match_arguments(
  Token *paren,
  Program *program
) {
  assert(paren->type == Token_Type_Paren);
  Token_Matcher_State *state = &(Token_Matcher_State) {.root = paren };

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

void
token_match_module(
  Token *token,
  Program *program_
) {
  assert(token->type == Token_Type_Module);
  if (!dyn_array_length(token->children)) return;

  Token_Matcher_State *state = &(Token_Matcher_State) {.root = token};

  // Matching symbol imports
  for (bool did_replace = true; did_replace;) {
    did_replace = false;
    for (u64 i = 0; i < dyn_array_length(token->children); ++i) {
      state->child_index = i;

      Token *import = token_peek_match(state, 0, &(Token) {
        .type = Token_Type_Id,
        .source = slice_literal("import"),
      });
      if (!import) continue;
      Token *args = token_peek_match(state, 1, &(Token) { .type = Token_Type_Paren });
      if (!args) continue;
      Token *result_token = token_import_match_arguments(args, program_);

      *dyn_array_get(token->children, i) = result_token;
      dyn_array_delete(token->children, i + 1);
      did_replace |= true;
    }
  }


  for (bool did_replace = true; did_replace;) {
    did_replace = false;
    for (u64 i = 0; i < dyn_array_length(token->children); ++i) {
      state->child_index = i;

      Token *arrow = token_peek_match(state, 1, &(Token) {
        .type = Token_Type_Operator,
        .source = slice_literal("->"),
      });
      if (!arrow) continue;

      Token *args = token_peek_match(state, 0, &(Token) { .type = Token_Type_Paren });
      Token *return_types = token_peek_match(state, 2, &(Token) { .type = Token_Type_Paren });
      Token *body = token_peek(state, 3);
      // TODO show proper error to the user
      assert(args);
      assert(return_types);
      assert(body);

      Token *result_token = temp_allocate(Token);
      *result_token = (Token){
        .type = Token_Type_Lazy_Function_Definition,
        .parent = token->parent,
        .source = token->source,
        .lazy_function_definition = {
          .args = args,
          .return_types = return_types,
          .body = body,
          .program = program_,
        },
      };

      *dyn_array_get(token->children, i) = result_token;
      dyn_array_delete_range(token->children, (Range_u64){i + 1, i + 4});
      did_replace |= true;
    }
  };

  for (bool did_replace = true; did_replace;) {
    did_replace = false;
    for (u64 i = 0; i < dyn_array_length(token->children); ++i) {
      state->child_index = i;

      Token *define = token_peek_match(state, 1, &(Token) {
        .type = Token_Type_Operator,
        .source = slice_literal("::"),
      });
      if (!define) continue;
      Token *name = token_peek_match(state, 0, &(Token) { .type = Token_Type_Id });
      Token *value = token_peek(state, 2);
      assert(value);
      scope_define_lazy(program_->global_scope, name->source, value);

      dyn_array_delete_range(token->children, (Range_u64){i, i + 3});
      did_replace |= true;
    }
  }

  assert(dyn_array_length(token->children) == 0);
}
