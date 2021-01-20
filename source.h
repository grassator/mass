#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

#define MASS_RETURN_LABEL_NAME slice_literal("@return_label")
#define MASS_RETURN_VALUE_NAME slice_literal("@return_value")

typedef struct {
  Operator_Fixity fixity;
  u8 argument_count;
  Slice argument_names[2];
  const Token *body;
  Scope *scope;
} User_Defined_Operator;

hash_map_slice_template(Scope_Map, Scope_Entry *)
hash_map_slice_template(Macro_Replacement_Map, Token_View)

typedef enum {
  Macro_Pattern_Tag_Single_Token,
  Macro_Pattern_Tag_Any_Token_Sequence,
} Macro_Pattern_Tag;

typedef struct {
  Token_Pattern token_pattern;
  Slice capture_name;
} Macro_Pattern_Single_Token;

typedef struct {
  Slice capture_name;
} Macro_Pattern_Any_Token_Sequence;

typedef struct {
  Macro_Pattern_Tag tag;
  union {
    Macro_Pattern_Any_Token_Sequence Any_Token_Sequence;
    Macro_Pattern_Single_Token Single_Token;
  };
} Macro_Pattern;
typedef dyn_array_type(Macro_Pattern) Array_Macro_Pattern;

typedef struct {
  Array_Macro_Pattern pattern;
  Token_View replacement;
  bool is_statement;
  Scope *scope;
} Macro;
typedef dyn_array_type(Macro *) Array_Macro_Ptr;

typedef bool (*Token_Statement_Matcher_Proc)
(Compilation_Context *context, Token_View, void *payload);
typedef struct {
  Token_Statement_Matcher_Proc proc;
  void *payload;
} Token_Statement_Matcher;

typedef dyn_array_type(Token_Statement_Matcher) Array_Token_Statement_Matcher;

typedef struct Scope {
  const Allocator *allocator;
  struct Scope *parent;
  Scope_Map *map;
  Array_Macro_Ptr macros;
  Array_Token_Statement_Matcher statement_matchers;
} Scope;

PRELUDE_NO_DISCARD Mass_Result
assign(
  Compilation_Context *context,
  const Source_Range *source_range,
  Value *target,
  Value *source
);

Scope *
scope_make(
  const Allocator *allocator,
  Scope *parent
);

static inline void
scope_define(
  Scope *scope,
  Slice name,
  Scope_Entry entry
);

void
scope_define_builtins(
  const Allocator *allocator,
  Scope *scope
);

void
compile_time_eval(
  Compilation_Context *context,
  Token_View view,
  Value *result_value
);

void
token_handle_negation(
  Compilation_Context *context,
  Token_View args,
  Value *result_value,
  void *unused_payload
);

bool
token_parse_expression(
  Compilation_Context *context,
  Token_View view,
  Value *result_value
);

bool
token_parse_block_no_scope(
  Compilation_Context *context,
  const Token *block,
  Value *block_result_value
);

bool
token_parse_block(
  Compilation_Context *program,
  const Token *block,
  Value *result_value
);

bool
token_parse_statement_label(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_statement_if(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_inline_machine_code_bytes(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_assignment(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_definition_and_assignment_statements(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_definitions(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_explicit_return(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_goto(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);
bool
token_parse_constant_definitions(
  Compilation_Context *program,
  Token_View state,
  void *unused_payload
);

Module *
program_module_from_file(
  Compilation_Context *context,
  Slice file_path,
  Scope *scope
);

Mass_Result
program_import_module(
  Compilation_Context *context,
  Module *module
);

void
program_push_error_from_bucket_buffer(
  Compilation_Context *context,
  Source_Range source_range,
  Bucket_Buffer *buffer
);

typedef struct {
  Bucket_Buffer *buffer;
  char number_print_buffer[32];
} Program_Error_Builder;

#define context_error_snprintf(_CONTEXT_, _SOURCE_RANGE_, ...)\
  do {\
    assert((_CONTEXT_)->result->tag != Mass_Result_Tag_Error);\
    /* Calculating the size of the buffer not including null termination */ \
    int context_error_snprintf_buffer_size = snprintf(0, 0, ##__VA_ARGS__);\
    /* negative value is an error when encoding a string */ \
    assert(context_error_snprintf_buffer_size >= 0); \
    context_error_snprintf_buffer_size += 1; \
    char *context_error_snprintf_buffer = allocator_allocate_array(\
      (_CONTEXT_)->allocator, u8, context_error_snprintf_buffer_size\
    );\
    assert(0 <= snprintf(context_error_snprintf_buffer, context_error_snprintf_buffer_size, ##__VA_ARGS__));\
    *(_CONTEXT_)->result = (Mass_Result) {\
      .tag = Mass_Result_Tag_Error,\
      .Error.details = {\
        .message = slice_from_c_string(context_error_snprintf_buffer),\
        .source_range = (_SOURCE_RANGE_)\
      }\
    };\
  } while(0)

#endif
