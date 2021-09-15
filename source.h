#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

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

static PRELUDE_NO_DISCARD Mass_Result
assign(
  Execution_Context *context,
  Function_Builder *builder,
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
  Function_Builder *builder,
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

static void
ensure_parameter_descriptors(
  const Execution_Context *context,
  Function_Info *info
);

static inline const Symbol *
mass_ensure_symbol(
  Compilation *compilation,
  Slice name,
  Symbol_Type type
);

static Overload_Match
mass_match_overload(
  Value *value,
  Value_View args
);

static inline Scope *
scope_make(
  const Allocator *allocator,
  const Scope *parent
);

static inline void
scope_define_value(
  Scope *scope,
  u64 epoch,
  Source_Range source_range,
  const Symbol *symbol,
  Value *value
);

PRELUDE_NO_DISCARD static inline Mass_Result
scope_define_operator(
  Scope *scope,
  Source_Range source_range,
  Slice name,
  Operator *operator
);

static void
module_compiler_init(
  Compilation *compilation,
  Module *out_module
);

static void
scope_define_builtins(
  Compilation *compilation,
  Scope *scope,
  const Calling_Convention *calling_convention
);

static void
scope_print_names(
  const Scope *scope
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
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
);

static void
value_force_exact(
  Execution_Context *context,
  Function_Builder *builder,
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
token_parse_block_view(
  Execution_Context *program,
  Value_View view
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

static inline Mass_Result
mass_error(
  Mass_Error error
) {
  return (Mass_Result){ .tag = Mass_Result_Tag_Error, .Error.error = error };
}

static inline Mass_Result
mass_success() {
  return (Mass_Result){ .tag = Mass_Result_Tag_Success };
}

static inline void
context_error(
  Execution_Context *context,
  Mass_Error error
) {
  assert(context->result->tag != Mass_Result_Tag_Error);
  *context->result = mass_error(error);
}

static void
scope_define_enum(
  Compilation *compilation,
  Scope *scope,
  Source_Range source_range,
  Slice enum_name,
  Value *enum_type_value,
  C_Enum_Item *items,
  u64 item_count
);

#endif
