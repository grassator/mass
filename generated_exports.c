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
    mass_ensure_symbol(compilation, slice_literal("Value_View")),
    type_value_view_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Symbol")),
    type_symbol_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Group_Paren")),
    type_group_paren_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Group_Curly")),
    type_group_curly_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Group_Square")),
    type_group_square_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Quoted")),
    type_quoted_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("External_Symbol")),
    type_external_symbol_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Syscall")),
    type_syscall_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Execution_Context")),
    type_execution_context_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Scope")),
    type_scope_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Declaration")),
    type_declaration_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Value")),
    type_value_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Tuple")),
    type_tuple_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Code_Fragment")),
    type_code_fragment_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor")),
    type_descriptor_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Descriptor_Tag"), type_descriptor_tag_value,
    descriptor_tag_items, countof(descriptor_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Function_Instance")),
    type_descriptor_function_instance_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Fixed_Size_Array")),
    type_descriptor_fixed_size_array_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Struct")),
    type_descriptor_struct_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Pointer_To")),
    type_descriptor_pointer_to_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Reference_To")),
    type_descriptor_reference_to_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error")),
    type_mass_error_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Error_Tag"), type_mass_error_tag_value,
    mass_error_tag_items, countof(mass_error_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_User_Defined")),
    type_mass_error_user_defined_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Circular_Dependency")),
    type_mass_error_circular_dependency_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Integer_Range")),
    type_mass_error_integer_range_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Open")),
    type_mass_error_file_open_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Too_Large")),
    type_mass_error_file_too_large_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Load")),
    type_mass_error_dynamic_library_load_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Symbol_Not_Found")),
    type_mass_error_dynamic_library_symbol_not_found_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Unexpected_Token")),
    type_mass_error_unexpected_token_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Operator_Fixity_Conflict")),
    type_mass_error_operator_fixity_conflict_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Undefined_Variable")),
    type_mass_error_undefined_variable_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Redifinition")),
    type_mass_error_redifinition_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Unknown_Field")),
    type_mass_error_unknown_field_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Invalid_Identifier")),
    type_mass_error_invalid_identifier_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Type_Mismatch")),
    type_mass_error_type_mismatch_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_No_Matching_Overload")),
    type_mass_error_no_matching_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Error_Undecidable_Overload")),
    type_mass_error_undecidable_overload_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Result")),
    type_mass_result_value
  );
  scope_define_enum(
    compilation, scope, COMPILER_SOURCE_RANGE,
    slice_literal("Result_Tag"), type_mass_result_tag_value,
    mass_result_tag_items, countof(mass_result_tag_items)
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Result_Error")),
    type_mass_result_error_value
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    tokenize, "tokenize", &descriptor_mass_result,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("compilation")),
&descriptor_compilation_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("source_range")),
&descriptor_source_range
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("out_tokens")),
&descriptor_value_view_pointer
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_intrinsic, "intrinsic", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_call, "call", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_goto, "goto", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_import, "import", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_pointer_to, "pointer_to", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_eval, "eval", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_fragment, "fragment", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_inline_module, "inline_module", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_quote, "quote", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_unquote, "unquote", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_c_struct, "c_struct", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_exports, "exports", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_cast, "cast", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_type_of, "type_of", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_size_of, "size_of", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_startup, "startup", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_inline_machine_code_bytes, "inline_machine_code_bytes", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_compile_time_error, "compile_time_error", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    allocator_allocate_bytes, "allocator_allocate_bytes", &descriptor_void_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("allocator")),
&descriptor_allocator_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("byte_size")),
&descriptor_u64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("byte_alignment")),
&descriptor_u64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    descriptor_pointer_to, "descriptor_pointer_to", &descriptor_descriptor_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("allocator")),
&descriptor_allocator_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("descriptor")),
&descriptor_descriptor_pointer
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_logical_shift_left, "i64_logical_shift_left", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_logical_shift_right, "i64_logical_shift_right", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_bitwise_and, "i64_bitwise_and", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_bitwise_or, "i64_bitwise_or", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_add, "i64_add", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_subtract, "i64_subtract", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_equal, "i64_equal", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time,
    mass_i64_not_equal, "i64_not_equal", &descriptor_i64,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("a")),
&descriptor_i64
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("b")),
&descriptor_i64
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_add, "add", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_subtract, "subtract", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_multiply, "multiply", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_divide, "divide", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_remainder, "remainder", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_less, "less", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_greater, "greater", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_less_equal, "less_equal", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_greater_equal, "greater_equal", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_equal, "equal", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Compile_Time | Function_Info_Flags_Intrinsic,
    mass_not_equal, "not_equal", &descriptor_value_pointer,
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("context")),
&descriptor_execution_context_pointer
    ),
    function_parameter(
      mass_ensure_symbol(compilation, slice_literal("args")),
&descriptor_value_view
    )
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("Allocator")),
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
    mass_ensure_symbol(compilation, slice_literal("i64")),
    type_i64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u8")),
    type_u8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u16")),
    type_u16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u32")),
    type_u32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("u64")),
    type_u64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s8")),
    type_s8_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s16")),
    type_s16_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s32")),
    type_s32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("s64")),
    type_s64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("f32")),
    type_f32_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("f64")),
    type_f64_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    mass_ensure_symbol(compilation, slice_literal("String")),
    type_slice_value
  );
}

