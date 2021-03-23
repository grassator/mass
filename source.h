#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

#define MASS_RETURN_LABEL_NAME slice_literal("@return_label")
#define MASS_RETURN_VALUE_NAME slice_literal("@return_value")

#define PACK_AS_VOID_POINTER(_TARGET_, _SOURCE_)\
  do {\
    static_assert(sizeof(_SOURCE_) <= sizeof(_TARGET_), "Value too large to pack into a pointer");\
    memcpy(&(_TARGET_), &(_SOURCE_), sizeof(_SOURCE_));\
  } while(0)

#define UNPACK_FROM_VOID_POINTER(_TARGET_, _SOURCE_)\
  do {\
    static_assert(sizeof(_SOURCE_) >= sizeof(_TARGET_), "Value too large to unpack from a pointer");\
    memcpy(&(_TARGET_), &(_SOURCE_), sizeof(_TARGET_));\
  } while(0)

typedef struct {
  Operator_Fixity fixity;
  u8 argument_count;
  Slice argument_names[2];
  Value *body;
  Scope *scope;
} User_Defined_Operator;

hash_map_slice_template(Scope_Map, Scope_Entry *)
hash_map_slice_template(Macro_Replacement_Map, Value_View)

typedef enum {
  Macro_Pattern_Tag_Single_Token,
  Macro_Pattern_Tag_Any_Token_Sequence,
} Macro_Pattern_Tag;

typedef struct {
  Token_Pattern token_pattern;
} Macro_Pattern_Single_Token;

typedef struct {
  Macro_Pattern_Tag tag;
  Slice capture_name;
  union {
    Macro_Pattern_Single_Token Single_Token;
  };
} Macro_Pattern;
typedef dyn_array_type(Macro_Pattern) Array_Macro_Pattern;

typedef struct {
  Array_Macro_Pattern pattern;
  Value_View replacement;
  Scope *scope;
} Macro;
typedef dyn_array_type(Macro *) Array_Macro_Ptr;

typedef u64 (*Token_Statement_Matcher_Proc)
(Execution_Context *context, Value_View, Lazy_Value *out_lazy_value, void *payload);
typedef struct {
  Token_Statement_Matcher_Proc proc;
  void *payload;
} Token_Statement_Matcher;

typedef dyn_array_type(Token_Statement_Matcher) Array_Token_Statement_Matcher;

typedef enum {
  Scope_Flags_None = 0,
} Scope_Flags;

typedef struct Scope {
  const Allocator *allocator;
  u64 id;
  Scope_Flags flags;
  const struct Scope *parent;
  Scope_Map *map;
  Array_Macro_Ptr macros;
  Array_Token_Statement_Matcher statement_matchers;
} Scope;

MASS_DEFINE_OPAQUE_C_TYPE(scope, Scope);

PRELUDE_NO_DISCARD Mass_Result
assign(
  Execution_Context *context,
  Value *target,
  Value *source
);

static inline u64
get_new_epoch();

Scope *
scope_make(
  const Allocator *allocator,
  const Scope *parent
);

static inline void
scope_define_value(
  Scope *scope,
  Source_Range source_range,
  Slice name,
  Value *value
);

static inline void
scope_define_operator(
  Execution_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice name,
  Operator *operator
);

void
scope_define_builtins(
  const Allocator *allocator,
  Scope *scope
);

void
compile_time_eval(
  Execution_Context *context,
  Value_View view,
  Value *result_value
);

PRELUDE_NO_DISCARD Value *
token_parse_expression(
  Execution_Context *context,
  Value_View view,
  u64 *out_match_length,
  const Token_Pattern *end_pattern
);

PRELUDE_NO_DISCARD Mass_Result
value_force(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *token,
  Value *result_value
);

const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
);

PRELUDE_NO_DISCARD Value *
token_parse_block_no_scope(
  Execution_Context *context,
  Value *block
);

PRELUDE_NO_DISCARD Value *
token_parse_block(
  Execution_Context *program,
  Value *block
);

Module *
program_module_from_file(
  Execution_Context *context,
  Slice file_path,
  Scope *scope
);

Mass_Result
program_import_module(
  Execution_Context *context,
  Module *module
);

void
program_push_error_from_bucket_buffer(
  Execution_Context *context,
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
      (_CONTEXT_)->allocator, char, context_error_snprintf_buffer_size\
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
