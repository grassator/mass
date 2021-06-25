#include "source.h"

static void
compiler_scope_define_exports(
  Compilation *compilation,
  Scope *scope
) {
  const Allocator *allocator = compilation->allocator;
  (void)allocator;
  const Calling_Convention *calling_convention =
    compilation->jit.program->default_calling_convention;
  (void)calling_convention;
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("External_Symbol"), type_external_symbol_value
  );
  scope_define_enum(
    compilation->allocator, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Operator_Fixity"), type_operator_fixity_value,
    operator_fixity_items, countof(operator_fixity_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Execution_Context"), type_execution_context_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Value"), type_value_value
  );
  MASS_DEFINE_COMPILE_TIME_FUNCTION_TYPE(
    lazy_value_proc, "Lazy_Value_Proc", &descriptor_value_pointer,
    MASS_FN_ARG("context", &descriptor_execution_context_pointer),
    MASS_FN_ARG("builder", &descriptor_function_builder_pointer),
    MASS_FN_ARG("expected_result", &descriptor_expected_result_pointer),
    MASS_FN_ARG("payload", &descriptor_void_pointer)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor"), type_descriptor_value
  );
  scope_define_enum(
    compilation->allocator, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Tag"), type_descriptor_tag_value,
    descriptor_tag_items, countof(descriptor_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Function_Instance"), type_descriptor_function_instance_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Fixed_Size_Array"), type_descriptor_fixed_size_array_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Struct"), type_descriptor_struct_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Pointer_To"), type_descriptor_pointer_to_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error"), type_mass_error_value
  );
  scope_define_enum(
    compilation->allocator, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Tag"), type_mass_error_tag_value,
    mass_error_tag_items, countof(mass_error_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_User_Defined"), type_mass_error_user_defined_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Integer_Range"), type_mass_error_integer_range_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_File_Open"), type_mass_error_file_open_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Unexpected_Token"), type_mass_error_unexpected_token_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Operator_Infix_Suffix_Conflict"), type_mass_error_operator_infix_suffix_conflict_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Operator_Prefix_Conflict"), type_mass_error_operator_prefix_conflict_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Undefined_Variable"), type_mass_error_undefined_variable_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Redifinition"), type_mass_error_redifinition_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Unknown_Field"), type_mass_error_unknown_field_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Invalid_Identifier"), type_mass_error_invalid_identifier_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Type_Mismatch"), type_mass_error_type_mismatch_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_No_Matching_Overload"), type_mass_error_no_matching_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Error_Undecidable_Overload"), type_mass_error_undecidable_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Result"), type_mass_result_value
  );
  scope_define_enum(
    compilation->allocator, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Result_Tag"), type_mass_result_tag_value,
    mass_result_tag_items, countof(mass_result_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Mass_Result_Error"), type_mass_result_error_value
  );
}

static void
global_scope_define_exports(
  Scope *scope
) {
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Number_Literal"), type_number_literal_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("u8"), type_u8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("u16"), type_u16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("u32"), type_u32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("u64"), type_u64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("s8"), type_s8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("s16"), type_s16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("s32"), type_s32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("s64"), type_s64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("f32"), type_f32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("f64"), type_f64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Slice"), type_slice_value
  );
}

