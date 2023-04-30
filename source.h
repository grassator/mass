#ifndef SOURCE_H
#define SOURCE_H
#include "prelude.h"
#include "value.h"

typedef struct {
  const Descriptor *target;
  Value *expression;
} Mass_Cast_Lazy_Payload;

static Value *
mass_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
);

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
mass_function_info_init_for_header_and_maybe_body(
  Mass_Context *context,
  const Scope *arguments_scope,
  const Function_Header *header,
  Value *maybe_body,
  Function_Info *out_info
);

static Overload_Match
mass_match_overload(
  Mass_Context *context,
  Value *value,
  Array_Resolved_Function_Parameter parameters
);

static bool
mass_match_overload_or_error(
  Mass_Context *context,
  Value *target,
  Array_Resolved_Function_Parameter arg_parameters,
  Overload_Match_Found *match_found,
  const Source_Range *source_range
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

static inline const Descriptor *
value_ensure_type(
  Mass_Context *context,
  const Scope *scope,
  Value *value,
  Source_Range source_range
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
mass_compilation_init_scopes(
  Compilation *compilation
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

static inline Value *
mass_make_lazy_value(
  Mass_Context *context,
  Parser *parser,
  Source_Range source_range,
  const void *payload,
  const Descriptor *descriptor,
  Lazy_Value_Proc proc
);

static Value *
mass_implicit_function_parameter_factory_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Function_Call_Parameter *param
);

static Value *
mass_module_get_impl(
  Mass_Context *context,
  const Module *module,
  const Symbol *symbol,
  const Source_Range *source_range
);

static Value *
token_parse_block_statements(
  Mass_Context *program,
  Parser *parser,
  const Ast_Statement *statement,
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

typedef enum {
    Stack_Init_Mode_Zero,
    Stack_Init_Mode_Uninitialized,
} Stack_Init_Mode;

static Value *
mass_define_stack_value_from_typed_symbol(
  Mass_Context *context,
  Parser *parser,
  const Typed_Symbol *typed_symbol,
  Stack_Init_Mode init_mode,
  Source_Range source_range
);

#endif
