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

static PRELUDE_NO_DISCARD Mass_Result
assign(
  Execution_Context *context,
  Value *target,
  Value *source
);

static inline const Descriptor *
expected_result_descriptor(
  const Expected_Result *expected_result
);

static PRELUDE_NO_DISCARD Value *
expected_result_ensure_value_or_temp(
  Execution_Context *context,
  const Expected_Result *expected_result,
  Value *value
);

static inline u64
get_new_epoch();

static inline Value *
maybe_coerce_number_literal_to_integer(
  Execution_Context *context,
  Value *value,
  const Descriptor *target_descriptor
);

static inline Scope *
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

static void
scope_define_builtins(
  const Allocator *allocator,
  Scope *scope
);

static void
scope_print_names(
  const Scope *scope
);

static inline Scope_Entry *
scope_lookup(
  const Scope *scope,
  Slice name
);

static Value *
compile_time_eval(
  Execution_Context *context,
  Value_View view
);

static PRELUDE_NO_DISCARD Value *
token_parse_expression(
  Execution_Context *context,
  Value_View view,
  u64 *out_match_length,
  const Token_Pattern *end_pattern
);

static PRELUDE_NO_DISCARD Value *
value_force(
  Execution_Context *context,
  const Expected_Result *expected_result,
  Value *value
);

static void
value_force_exact(
  Execution_Context *context,
  Value *target,
  Value *source
);

static inline Expected_Result
expected_result_from_value(
  Value *value
);

static inline const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
);

static PRELUDE_NO_DISCARD Value *
token_parse_block_no_scope(
  Execution_Context *context,
  Value *block
);

static PRELUDE_NO_DISCARD Value *
token_parse_block(
  Execution_Context *program,
  Value *block
);

static Module *
program_module_from_file(
  Execution_Context *context,
  Slice file_path,
  Scope *scope
);

static Mass_Result
program_import_module(
  Execution_Context *context,
  Module *module
);

static inline void
context_error(
  Execution_Context *context,
  Mass_Error error
) {
  assert(context->result->tag != Mass_Result_Tag_Error);
  *context->result = (Mass_Result) { .tag = Mass_Result_Tag_Error, .Error.error = error };
}

#endif
