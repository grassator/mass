#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

typedef enum {
  Token_Type_Id = 1,
  Token_Type_Newline,
  Token_Type_Integer,
  Token_Type_Hex_Integer,
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
  Token_Type type;
  Slice source;
  Source_Location location;
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
  Tokenizer_State_Hex_Integer,
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
    Array_Token_Ptr tokens;
  };
} Scope_Entry;

hash_map_slice_template(Scope_Map, Scope_Entry)
hash_map_slice_template(Macro_Replacement_Map, Token *)

typedef struct {
  Array_Token_Ptr pattern;
  Array_Token_Ptr replacement;
  Array_Slice pattern_names;
} Macro;
typedef dyn_array_type(Macro *) Array_Macro_Ptr;

typedef struct Scope {
  struct Scope *parent;
  Scope_Map *map;
  Array_Macro_Ptr macros;
} Scope;

Scope *
scope_make(
  Scope *parent
);

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
);

void
token_parse_block(
  Program *program,
  Array_Token_Ptr children,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
);

#endif SOURCE_H
