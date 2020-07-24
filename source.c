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
} Token_Type;

struct Token;
typedef dyn_array_type(struct Token *) Array_Token_Ptr;

typedef struct Token {
  struct Token *parent;
  Token_Type type;
  Slice source;
  Array_Token_Ptr children;
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
            case Token_Type_Id:
            case Token_Type_Integer:
            case Token_Type_Operator:
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
  return *dyn_array_peek(state->root->children, index);
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

typedef struct {
  bool match;
  Slice name;
  Value *value;
} Token_Match_Function;

Descriptor *
lookup_descriptor_type(
  Slice name
) {
  if (slice_equal(slice_literal("s64"), name)) {
    return &descriptor_s64;
  } else if (slice_equal(slice_literal("s32"), name)) {
    return &descriptor_s32;
  }
  return 0;
}

Token_Match_Function
token_match_function_definition(
  Token_Matcher_State *state,
  Program *program_
) {
  Token_Match_Function result = {0};

  u64 delta = 0;
  Token *id = token_peek_match(state, delta++, &(Token) { .type = Token_Type_Id });
  if (!id) return result;

  Token *colon_colon = token_peek_match(state, delta++, &(Token) {
    .type = Token_Type_Operator,
    .source = slice_literal("::"),
  });
  if (!colon_colon) return result;

  Token *args = token_peek_match(state, delta++, &(Token) { .type = Token_Type_Paren });
  if (!args) return result;

  Token *arrow = token_peek_match(state, delta++, &(Token) {
    .type = Token_Type_Operator,
    .source = slice_literal("->"),
  });
  if (!arrow) return result;

  Token *return_types = token_peek_match(state, delta++, &(Token) { .type = Token_Type_Paren });
  if (!return_types) return result;

  Token *body = token_peek_match(state, delta++, &(Token) { .type = Token_Type_Curly });
  if (!body) return result;

  result.match = true;
  result.name = id->source;

  Function(value) {
    if (dyn_array_length(args->children) != 0) {
      assert(!"Not implemented");
    }

    switch (dyn_array_length(return_types->children)) {
      case 0: {
        value->descriptor->function.returns = &void_value;
        break;
      }
      case 1: {
        Token *return_type_token = *dyn_array_get(return_types->children, 0);
        assert(return_type_token->type == Token_Type_Id);
        Descriptor *descriptor = lookup_descriptor_type(return_type_token->source);
        assert(descriptor);
        fn_return_descriptor(builder_, descriptor);
        break;
      }
      default: {
        assert(!"Multiple return types are not supported at the moment");
        break;
      }
    }
    fn_freeze(builder_);

    Value *body_result = 0;
    if (dyn_array_length(body->children) == 1) {
      Token *expr = *dyn_array_get(body->children, 0);
      if (expr->type == Token_Type_Integer) {
        Slice_Parse_S64_Result parse_result = slice_parse_s64(expr->source);
        assert(parse_result.success);
        assert(parse_result.value == 42);
        body_result = value_from_s64(parse_result.value);
      } else {
        assert(!"Unexpected value");
      }
    } else {
        assert(!"Expression of more than 1 item are not implemented");
    }

    // Patterns in precedence order
    // _*_
    // _+_
    // Integer | Paren

    //
    // 42 + 3 * 2
    //      _ * _
    //
    // 3
    if (body_result) {
      Return(body_result);
    }
  }
  result.value = value;
  return result;
}
