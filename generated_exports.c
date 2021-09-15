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
    mass_ensure_symbol(compilation, slice_literal("Value_View"), Symbol_Type_Id_Like),
    type_value_view_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Symbol"), Symbol_Type_Id_Like),
    type_symbol_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("External_Symbol"), Symbol_Type_Id_Like),
    type_external_symbol_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Syscall"), Symbol_Type_Id_Like),
    type_syscall_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Execution_Context"), Symbol_Type_Id_Like),
    type_execution_context_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Declaration"), Symbol_Type_Id_Like),
    type_declaration_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Value"), Symbol_Type_Id_Like),
    type_value_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor"), Symbol_Type_Id_Like),
    type_descriptor_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Tag"), type_descriptor_tag_value,
    descriptor_tag_items, countof(descriptor_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Function_Instance"), Symbol_Type_Id_Like),
    type_descriptor_function_instance_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Fixed_Size_Array"), Symbol_Type_Id_Like),
    type_descriptor_fixed_size_array_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Struct"), Symbol_Type_Id_Like),
    type_descriptor_struct_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Pointer_To"), Symbol_Type_Id_Like),
    type_descriptor_pointer_to_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Reference_To"), Symbol_Type_Id_Like),
    type_descriptor_reference_to_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error"), Symbol_Type_Id_Like),
    type_mass_error_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Error_Tag"), type_mass_error_tag_value,
    mass_error_tag_items, countof(mass_error_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_User_Defined"), Symbol_Type_Id_Like),
    type_mass_error_user_defined_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Circular_Dependency"), Symbol_Type_Id_Like),
    type_mass_error_circular_dependency_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Integer_Range"), Symbol_Type_Id_Like),
    type_mass_error_integer_range_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Open"), Symbol_Type_Id_Like),
    type_mass_error_file_open_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Too_Large"), Symbol_Type_Id_Like),
    type_mass_error_file_too_large_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Load"), Symbol_Type_Id_Like),
    type_mass_error_dynamic_library_load_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Symbol_Not_Found"), Symbol_Type_Id_Like),
    type_mass_error_dynamic_library_symbol_not_found_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Unexpected_Token"), Symbol_Type_Id_Like),
    type_mass_error_unexpected_token_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Operator_Fixity_Conflict"), Symbol_Type_Id_Like),
    type_mass_error_operator_fixity_conflict_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Undefined_Variable"), Symbol_Type_Id_Like),
    type_mass_error_undefined_variable_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Redifinition"), Symbol_Type_Id_Like),
    type_mass_error_redifinition_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Unknown_Field"), Symbol_Type_Id_Like),
    type_mass_error_unknown_field_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Invalid_Identifier"), Symbol_Type_Id_Like),
    type_mass_error_invalid_identifier_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Type_Mismatch"), Symbol_Type_Id_Like),
    type_mass_error_type_mismatch_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_No_Matching_Overload"), Symbol_Type_Id_Like),
    type_mass_error_no_matching_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Undecidable_Overload"), Symbol_Type_Id_Like),
    type_mass_error_undecidable_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Result"), Symbol_Type_Id_Like),
    type_mass_result_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Result_Tag"), type_mass_result_tag_value,
    mass_result_tag_items, countof(mass_result_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Result_Error"), Symbol_Type_Id_Like),
    type_mass_result_error_value
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    tokenize, "tokenize", &descriptor_mass_result,
    function_parameter(slice_literal("compilation"), &descriptor_compilation_pointer),
    function_parameter(slice_literal("file"), &descriptor_source_file_pointer),
    function_parameter(slice_literal("out_tokens"), &descriptor_value_view_pointer)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_address_of, "address_of", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_type_of, "type_of", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_size_of, "size_of", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_compile_time_error, "compile_time_error", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    allocator_allocate_bytes, "allocator_allocate_bytes", &descriptor_void_pointer,
    function_parameter(slice_literal("allocator"), &descriptor_allocator_pointer),
    function_parameter(slice_literal("byte_size"), &descriptor_u64),
    function_parameter(slice_literal("byte_alignment"), &descriptor_u64)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    descriptor_pointer_to, "descriptor_pointer_to", &descriptor_descriptor_pointer,
    function_parameter(slice_literal("allocator"), &descriptor_allocator_pointer),
    function_parameter(slice_literal("descriptor"), &descriptor_descriptor_pointer)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_number_literal_logical_shift_left, "number_literal_logical_shift_left", &descriptor_number_literal,
    function_parameter(slice_literal("input"), &descriptor_number_literal),
    function_parameter(slice_literal("shift"), &descriptor_number_literal)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_number_literal_logical_shift_right, "number_literal_logical_shift_right", &descriptor_number_literal,
    function_parameter(slice_literal("input"), &descriptor_number_literal),
    function_parameter(slice_literal("shift"), &descriptor_number_literal)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_number_literal_bitwise_or, "number_literal_bitwise_or", &descriptor_number_literal,
    function_parameter(slice_literal("a"), &descriptor_number_literal),
    function_parameter(slice_literal("b"), &descriptor_number_literal)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_number_literal_bitwise_and, "number_literal_bitwise_and", &descriptor_number_literal,
    function_parameter(slice_literal("a"), &descriptor_number_literal),
    function_parameter(slice_literal("b"), &descriptor_number_literal)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_add, "add", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_subtract, "subtract", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_multiply, "multiply", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_divide, "divide", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_remainder, "remainder", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_less, "less", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_greater, "greater", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_less_equal, "less_equal", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_greater_equal, "greater_equal", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_equal, "equal", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_not_equal, "not_equal", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Allocator"), Symbol_Type_Id_Like),
    type_allocator_value
  );
}

static void
global_scope_define_exports(
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
    mass_ensure_symbol(compilation, slice_literal("Number_Literal"), Symbol_Type_Id_Like),
    type_number_literal_value
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_import, "import", &descriptor_value_pointer,
    function_parameter(slice_literal("context"), &descriptor_execution_context_pointer),
    function_parameter(slice_literal("args"), &descriptor_value_view)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u8"), Symbol_Type_Id_Like),
    type_u8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u16"), Symbol_Type_Id_Like),
    type_u16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u32"), Symbol_Type_Id_Like),
    type_u32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u64"), Symbol_Type_Id_Like),
    type_u64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s8"), Symbol_Type_Id_Like),
    type_s8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s16"), Symbol_Type_Id_Like),
    type_s16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s32"), Symbol_Type_Id_Like),
    type_s32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s64"), Symbol_Type_Id_Like),
    type_s64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("f32"), Symbol_Type_Id_Like),
    type_f32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("f64"), Symbol_Type_Id_Like),
    type_f64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Slice"), Symbol_Type_Id_Like),
    type_slice_value
  );
}

