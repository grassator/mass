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
  Compilation *compilation,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Source_Range *source_range
);

static void
load_address(
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Storage storage
);

static inline const Descriptor *
expected_result_descriptor(
  const Expected_Result *expected_result
);

static PRELUDE_NO_DISCARD Value *
expected_result_ensure_value_or_temp(
  Compilation *compilation,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
);

static inline u64
get_new_epoch();

static void
ensure_parameter_descriptors(
  const Execution_Context *context,
  Function_Info *info,
  Scope *arguments_scope
);

static inline const Symbol *
mass_ensure_symbol(
  Compilation *compilation,
  Slice name
);

static Overload_Match
mass_match_overload(
  Value *value,
  Value_View args
);

static bool
mass_match_overload_or_error(
  Compilation *compilation,
  Value *target,
  Value_View args_view,
  Overload_Match_Found *match_found
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

static void
module_compiler_init(
  Compilation *compilation,
  Module *out_module
);

static void
scope_define_builtins(
  Compilation *compilation,
  Scope *scope
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
  u32 *out_match_length,
  const Symbol *end_symbol
);

static Value *
token_handle_function_call(
  Execution_Context *context,
  Value *target_expression,
  Value_View args_view,
  Source_Range source_range
);

static PRELUDE_NO_DISCARD Value *
value_force(
  Compilation *compilation,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
);

static void
value_force_exact(
  Compilation *compilation,
  Function_Builder *builder,
  Value *target,
  Value *source
);

static inline Expected_Result
expected_result_exact(
  const Descriptor *descriptor,
  Storage storage
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
  const Group_Curly *group
);

static PRELUDE_NO_DISCARD Value *
token_parse_block(
  Execution_Context *program,
  const Group_Curly *group
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
compilation_error(
  Compilation *compilation,
  Mass_Error error
) {
  assert(compilation->result->tag != Mass_Result_Tag_Error);
  *compilation->result = mass_error(error);
}

static inline void
context_error(
  Execution_Context *context,
  Mass_Error error
) {
  compilation_error(context->compilation, error);
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

static Value *
mass_define_stack_value_from_typed_symbol(
  Execution_Context *context,
  const Typed_Symbol *typed_symbol,
  Source_Range source_range
);

#endif
