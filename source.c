#include "prelude.h"
#include "value.h"

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
  union {
    Array_Token_Ptr children;
  };
} Token;

typedef enum {
  Tokenizer_State_Default,
  Tokenizer_State_Integer,
  Tokenizer_State_Operator,
  Tokenizer_State_Id,
} Tokenizer_State;

Token *
tokenize(
  Slice source
) {
  Token *root = temp_allocate(Token);
  root->parent = 0;
  root->type = Token_Type_Module;
  root->children = dyn_array_make(Array_Token_Ptr, 32);
  root->source = source;

  Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;

  for (u64 i = 0; i < source.length; ++i) {
    s8 ch = source.bytes[i];

    if (ch == ')') {
      dyn_array_push(parent->children, current_token);
      state = Tokenizer_State_Default;
      parent->source.length = &source.bytes[i] - parent->source.bytes + 1;
      parent = parent->parent;
      current_token = 0;
      assert(parent);
      continue;
    }

    switch(state) {
      case Tokenizer_State_Default: {
        if (isspace(ch)) continue;
        if (isdigit(ch)) {
          current_token = temp_allocate(Token);
          *current_token = (Token) {
            .type = Token_Type_Integer,
            .parent = parent,
            .source = {
              .bytes = &source.bytes[i],
              .length = 1,
            },
          };
          state = Tokenizer_State_Integer;
        } else if (isalpha(ch)) {
          current_token = temp_allocate(Token);
          *current_token = (Token) {
            .type = Token_Type_Id,
            .parent = parent,
            .source = {
              .bytes = &source.bytes[i],
              .length = 1,
            },
          };
          state = Tokenizer_State_Id;
        } else if (ch == '+') {
          current_token = temp_allocate(Token);
          *current_token = (Token) {
            .type = Token_Type_Operator,
            .parent = parent,
            .source = {
              .bytes = &source.bytes[i],
              .length = 1,
            },
          };
          state = Tokenizer_State_Operator;
        } else if (ch == '(') {
          current_token = temp_allocate(Token);
          *current_token = (Token) {
            .type = Token_Type_Paren,
            .parent = parent,
            .source = {
              .bytes = &source.bytes[i],
              .length = 1,
            },
            .children = dyn_array_make(Array_Token_Ptr, 4),
          };
          dyn_array_push(parent->children, current_token);
          parent = current_token;
        } else {
          assert(!"Unable to tokenize input");
        }
        break;
      }
      case Tokenizer_State_Integer: {
        if (isspace(ch)) {
          dyn_array_push(parent->children, current_token);
          state = Tokenizer_State_Default;
        } else if (isdigit(ch)) {
          current_token->source.length++;
        } else {
          assert(!"Unable to tokenize input");
        }
        break;
      }
      case Tokenizer_State_Id: {
        if (isspace(ch)) {
          dyn_array_push(parent->children, current_token);
          state = Tokenizer_State_Default;
        } else if (isalpha(ch) || isdigit(ch)) {
          current_token->source.length++;
        } else {
          assert(!"Unable to tokenize input");
        }
        break;
      }
      case Tokenizer_State_Operator: {
        if (isspace(ch)) {
          dyn_array_push(parent->children, current_token);
          state = Tokenizer_State_Default;
        } else {
          assert(!"Unable to tokenize input");
        }
        break;
      }
    }
  }

  assert(parent == root);
  // current_token can be null in case of an empty input
  if (current_token) {
    dyn_array_push(root->children, current_token);
  }

  return root;
}