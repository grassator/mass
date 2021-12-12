#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

typedef struct {
  const Descriptor *target;
  Value *expression;
} Mass_Cast_Lazy_Payload;

static Value *
mass_handle_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Cast_Lazy_Payload *payload
);

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

static inline bool
mass_descriptor_is_void(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Void;
}

static void
mass_assign_helper(
  Mass_Context *context,
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

static Storage
value_maybe_dereference(
  Mass_Context *context,
  Function_Builder *builder,
  Value *value
);

static inline const Descriptor *
mass_expected_result_descriptor(
  const Expected_Result *expected_result
);

static Value *
mass_expected_result_ensure_value_or_temp(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
);

static inline Epoch
get_new_epoch() {
  static Atomic_u64 next_epoch = {0};
  Epoch epoch = {atomic_u64_increment(&next_epoch)};
  return epoch;
}

static void
ensure_parameter_descriptors(
  Mass_Context *context,
  Scope *arguments_scope,
  Array_Function_Parameter *parameters,
  Function_Return *returns
);

static bool
mass_match_overload_or_error(
  Mass_Context *context,
  Value *target,
  Value_View args_view,
  Overload_Match_Found *match_found
);

static inline Scope *
scope_make(
  const Allocator *allocator,
  const Scope *parent
);

static inline bool
value_is_intrinsic(
  Value *value
);

static inline void
scope_define_value(
  Scope *scope,
  Epoch epoch,
  Source_Range source_range,
  const Symbol *symbol,
  Value *value
);

static void
module_compiler_init(
  Compilation *compilation,
  Module *out_module
);

static Value *
mass_handle_apply_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View operands_view,
  const Operator *operator
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
  Mass_Context *context,
  Parser *parser,
  Value_View view
);

static Value *
token_parse_expression(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *out_match_length,
  const Symbol *end_symbol
);

static Value *
token_handle_function_call(
  Mass_Context *context,
  Parser *parser,
  Value *target_expression,
  Value_View args_view,
  Source_Range source_range
);

static const Descriptor *
deduce_runtime_descriptor_for_value(
  Mass_Context *context,
  Value *value,
  const Descriptor *maybe_desired_descriptor
);

static Value *
value_force(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
);

static void
value_force_exact(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
);

static Value *
scope_entry_force_value(
  Mass_Context *context,
  Scope_Entry *entry
);

static inline Expected_Result
mass_expected_result_exact(
  const Descriptor *descriptor,
  Storage storage
);

static inline const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
);

static Value *
token_parse_block_statements(
  Mass_Context *program,
  Parser *parser,
  Array_Value_View statements,
  const Source_Range *source_range
);

static Value *
token_parse_block(
  Mass_Context *program,
  Parser *parser,
  const Ast_Block *block,
  const Source_Range *source_range
);

static Module *
program_module_from_file(
  Mass_Context *context,
  Slice file_path,
  Scope *scope
);

static void
program_import_module(
  Mass_Context *context,
  Module *module
);

static inline Mass_Result
mass_success() {
  return (Mass_Result){ .tag = Mass_Result_Tag_Success };
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
  Mass_Context *context,
  Parser *parser,
  const Typed_Symbol *typed_symbol,
  Source_Range source_range
);

#endif
