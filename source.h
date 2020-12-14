#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"


#define MASS_RETURN_LABEL_NAME slice_literal("@return_label")
#define MASS_RETURN_VALUE_NAME slice_literal("@return_value")
typedef enum {
  Scope_Entry_Type_Value = 1,
  Scope_Entry_Type_Lazy_Expression,
  Scope_Entry_Type_Operator,
} Scope_Entry_Type;

typedef struct {
  Token_View tokens;
  Scope *scope;
  Function_Builder *maybe_builder;
} Scope_Lazy_Expression;

typedef struct {
  s64 precedence;
} Scope_Entry_Operator;

typedef struct {
  Scope_Entry_Type type;
  union {
    Value *value;
    Scope_Lazy_Expression lazy_expression;
    Scope_Entry_Operator Operator;
  };
} Scope_Entry;
typedef dyn_array_type(Scope_Entry) Array_Scope_Entry;

hash_map_slice_template(Scope_Map, Array_Scope_Entry)
hash_map_slice_template(Macro_Replacement_Map, const Token *)

typedef struct {
  Array_Token_Pattern pattern;
  Array_Const_Token_Ptr replacement;
  Array_Slice pattern_names;
} Macro;
typedef dyn_array_type(Macro *) Array_Macro_Ptr;

typedef bool (*Token_Statement_Matcher_Proc)
(Compilation_Context *context, Token_View, Function_Builder *, void *payload);
typedef struct {
  Token_Statement_Matcher_Proc proc;
  void *payload;
} Token_Statement_Matcher;

typedef dyn_array_type(Token_Statement_Matcher) Array_Token_Statement_Matcher;

typedef struct Scope {
  struct Scope *parent;
  Scope_Map *map;
  Array_Macro_Ptr macros;
  Array_Token_Statement_Matcher statement_matchers;
} Scope;

Scope *
scope_make(
  Allocator *allocator,
  Scope *parent
);

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
);

void
scope_define_operator(
  Scope *scope,
  Slice name,
  s64 precedence
);

bool
token_parse_expression(
  Compilation_Context *context,
  Token_View view,
  Function_Builder *builder,
  Value *result_value
);

bool
token_parse_block(
  Compilation_Context *program,
  const Token *block,
  Function_Builder *builder,
  Value *result_value
);

bool
token_parse_statement_label(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_statement_if(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_inline_machine_code_bytes(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_assignment(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_definition_and_assignment_statements(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_definitions(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_explicit_return(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_goto(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);
bool
token_parse_constant_definitions(
  Compilation_Context *program,
  Token_View state,
  Function_Builder *builder,
  void *unused_payload
);

void
program_push_error_from_bucket_buffer(
  Compilation_Context *context,
  Source_Range source_range,
  Bucket_Buffer *buffer
);

void
program_push_error_from_slice(
  Program *program,
  Source_Range source_range,
  Slice message
);

#define program_error_builder(_program_, _location_)\
  for(\
    struct {\
      Bucket_Buffer *buffer;\
      u8 number_print_buffer[32];\
    } _error_builder = {\
      .buffer = bucket_buffer_make(.allocator = allocator_system),\
    };\
    _error_builder.buffer;\
    program_push_error_from_bucket_buffer((_program_), (_location_), _error_builder.buffer),\
    _error_builder.buffer = 0\
  )
#define program_error_append_number(_format_, _number_)\
  do {\
    snprintf(\
      _error_builder.number_print_buffer,\
      countof(_error_builder.number_print_buffer),\
      _format_,\
      (_number_)\
    );\
    Slice _number_slice = slice_from_c_string(_error_builder.number_print_buffer);\
    bucket_buffer_append_slice(_error_builder.buffer, _number_slice);\
  } while (0)

#define program_error_append_slice(_slice_)\
  bucket_buffer_append_slice(_error_builder.buffer, (_slice_))

#define program_error_append_literal(_message_)\
  program_error_append_slice(slice_literal(_message_))

#endif SOURCE_H
