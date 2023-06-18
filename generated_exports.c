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
  Source_Range Bits__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Bits__source_range, "Bits");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Bits__source_range,
    mass_ensure_symbol(compilation, slice_literal("Bits")),
    type_bits_value
  );
  Source_Range Source_Range__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Source_Range__source_range, "Source_Range");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Source_Range__source_range,
    mass_ensure_symbol(compilation, slice_literal("Source_Range")),
    type_source_range_value
  );
  Source_Range Assignment__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Assignment__source_range, "Assignment");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Assignment__source_range,
    mass_ensure_symbol(compilation, slice_literal("Assignment")),
    type_assignment_value
  );
  Source_Range Module_Exports__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Module_Exports__source_range, "Module_Exports");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Module_Exports__source_range,
    mass_ensure_symbol(compilation, slice_literal("Module_Exports")),
    type_module_exports_value
  );
  Source_Range Module_Exports_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Module_Exports_Tag__source_range, "Module_Exports_Tag");
  scope_define_enum(
    compilation, scope, Module_Exports_Tag__source_range,
    slice_literal("Module_Exports_Tag"), type_module_exports_tag_value,
    module_exports_tag_items, countof(module_exports_tag_items)
  );
  Source_Range Module_Exports_Selective__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Module_Exports_Selective__source_range, "Module_Exports_Selective");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Module_Exports_Selective__source_range,
    mass_ensure_symbol(compilation, slice_literal("Module_Exports_Selective")),
    type_module_exports_selective_value
  );
  Source_Range Module__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Module__source_range, "Module");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Module__source_range,
    mass_ensure_symbol(compilation, slice_literal("Module")),
    type_module_value
  );
  Source_Range Value_View__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value_View__source_range, "Value_View");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Value_View__source_range,
    mass_ensure_symbol(compilation, slice_literal("Value_View")),
    type_value_view_value
  );
  Source_Range Symbol__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Symbol__source_range, "Symbol");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Symbol__source_range,
    mass_ensure_symbol(compilation, slice_literal("Symbol")),
    type_symbol_value
  );
  Source_Range Group_Paren__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Group_Paren__source_range, "Group_Paren");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Group_Paren__source_range,
    mass_ensure_symbol(compilation, slice_literal("Group_Paren")),
    type_group_paren_value
  );
  Source_Range Ast_Return__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Ast_Return__source_range, "Ast_Return");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Ast_Return__source_range,
    mass_ensure_symbol(compilation, slice_literal("Ast_Return")),
    type_ast_return_value
  );
  Source_Range Ast_Statement__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Ast_Statement__source_range, "Ast_Statement");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Ast_Statement__source_range,
    mass_ensure_symbol(compilation, slice_literal("Ast_Statement")),
    type_ast_statement_value
  );
  Source_Range Ast_Block__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Ast_Block__source_range, "Ast_Block");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Ast_Block__source_range,
    mass_ensure_symbol(compilation, slice_literal("Ast_Block")),
    type_ast_block_value
  );
  Source_Range Group_Square__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Group_Square__source_range, "Group_Square");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Group_Square__source_range,
    mass_ensure_symbol(compilation, slice_literal("Group_Square")),
    type_group_square_value
  );
  Source_Range Register__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Register__source_range, "Register");
  scope_define_enum(
    compilation, scope, Register__source_range,
    slice_literal("Register"), type_register_value,
    register_items, countof(register_items)
  );
  Source_Range Quoted__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Quoted__source_range, "Quoted");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Quoted__source_range,
    mass_ensure_symbol(compilation, slice_literal("Quoted")),
    type_quoted_value
  );
  Source_Range Spread__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Spread__source_range, "Spread");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Spread__source_range,
    mass_ensure_symbol(compilation, slice_literal("Spread")),
    type_spread_value
  );
  Source_Range Named_Accessor__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Named_Accessor__source_range, "Named_Accessor");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Named_Accessor__source_range,
    mass_ensure_symbol(compilation, slice_literal("Named_Accessor")),
    type_named_accessor_value
  );
  Source_Range External_Symbol__source_range;
  INIT_LITERAL_SOURCE_RANGE(&External_Symbol__source_range, "External_Symbol");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, External_Symbol__source_range,
    mass_ensure_symbol(compilation, slice_literal("External_Symbol")),
    type_external_symbol_value
  );
  Source_Range Compare_Type__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Compare_Type__source_range, "Compare_Type");
  scope_define_enum(
    compilation, scope, Compare_Type__source_range,
    slice_literal("Compare_Type"), type_compare_type_value,
    compare_type_items, countof(compare_type_items)
  );
  Source_Range Storage_Flags__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Flags__source_range, "Storage_Flags");
  scope_define_enum(
    compilation, scope, Storage_Flags__source_range,
    slice_literal("Storage_Flags"), type_storage_flags_value,
    storage_flags_items, countof(storage_flags_items)
  );
  Source_Range Storage__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage__source_range, "Storage");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage")),
    type_storage_value
  );
  Source_Range Storage_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Tag__source_range, "Storage_Tag");
  scope_define_enum(
    compilation, scope, Storage_Tag__source_range,
    slice_literal("Storage_Tag"), type_storage_tag_value,
    storage_tag_items, countof(storage_tag_items)
  );
  Source_Range Storage_Immediate__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Immediate__source_range, "Storage_Immediate");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Immediate__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Immediate")),
    type_storage_immediate_value
  );
  Source_Range Storage_Eflags__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Eflags__source_range, "Storage_Eflags");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Eflags__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Eflags")),
    type_storage_eflags_value
  );
  Source_Range Storage_Register__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Register__source_range, "Storage_Register");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Register__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Register")),
    type_storage_register_value
  );
  Source_Range Storage_Xmm__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Xmm__source_range, "Storage_Xmm");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Xmm__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Xmm")),
    type_storage_xmm_value
  );
  Source_Range Storage_Static__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Static__source_range, "Storage_Static");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Static__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Static")),
    type_storage_static_value
  );
  Source_Range Storage_Memory__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Memory__source_range, "Storage_Memory");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Memory__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Memory")),
    type_storage_memory_value
  );
  Source_Range Storage_Disjoint__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Storage_Disjoint__source_range, "Storage_Disjoint");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Storage_Disjoint__source_range,
    mass_ensure_symbol(compilation, slice_literal("Storage_Disjoint")),
    type_storage_disjoint_value
  );
  Source_Range Instruction__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction__source_range, "Instruction");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction")),
    type_instruction_value
  );
  Source_Range Instruction_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Tag__source_range, "Instruction_Tag");
  scope_define_enum(
    compilation, scope, Instruction_Tag__source_range,
    slice_literal("Instruction_Tag"), type_instruction_tag_value,
    instruction_tag_items, countof(instruction_tag_items)
  );
  Source_Range Instruction_Label__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Label__source_range, "Instruction_Label");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction_Label__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction_Label")),
    type_instruction_label_value
  );
  Source_Range Instruction_Bytes__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Bytes__source_range, "Instruction_Bytes");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction_Bytes__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction_Bytes")),
    type_instruction_bytes_value
  );
  Source_Range Instruction_Label_Patch__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Label_Patch__source_range, "Instruction_Label_Patch");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction_Label_Patch__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction_Label_Patch")),
    type_instruction_label_patch_value
  );
  Source_Range Instruction_Stack_Patch__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Stack_Patch__source_range, "Instruction_Stack_Patch");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction_Stack_Patch__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction_Stack_Patch")),
    type_instruction_stack_patch_value
  );
  Source_Range Instruction_Location__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Instruction_Location__source_range, "Instruction_Location");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Instruction_Location__source_range,
    mass_ensure_symbol(compilation, slice_literal("Instruction_Location")),
    type_instruction_location_value
  );
  Source_Range Context__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Context__source_range, "Context");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Context__source_range,
    mass_ensure_symbol(compilation, slice_literal("Context")),
    type_mass_context_value
  );
  Source_Range Parser__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Parser__source_range, "Parser");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Parser__source_range,
    mass_ensure_symbol(compilation, slice_literal("Parser")),
    type_parser_value
  );
  Source_Range Scope__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Scope__source_range, "Scope");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Scope__source_range,
    mass_ensure_symbol(compilation, slice_literal("Scope")),
    type_scope_value
  );
  Source_Range Scope_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Scope_Tag__source_range, "Scope_Tag");
  scope_define_enum(
    compilation, scope, Scope_Tag__source_range,
    slice_literal("Scope_Tag"), type_scope_tag_value,
    scope_tag_items, countof(scope_tag_items)
  );
  Source_Range Scope_Imperative__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Scope_Imperative__source_range, "Scope_Imperative");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Scope_Imperative__source_range,
    mass_ensure_symbol(compilation, slice_literal("Scope_Imperative")),
    type_scope_imperative_value
  );
  Source_Range Scope_Declarative__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Scope_Declarative__source_range, "Scope_Declarative");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Scope_Declarative__source_range,
    mass_ensure_symbol(compilation, slice_literal("Scope_Declarative")),
    type_scope_declarative_value
  );
  Source_Range Value_Flags__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value_Flags__source_range, "Value_Flags");
  scope_define_enum(
    compilation, scope, Value_Flags__source_range,
    slice_literal("Value_Flags"), type_value_flags_value,
    value_flags_items, countof(value_flags_items)
  );
  Source_Range Value__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value__source_range, "Value");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Value__source_range,
    mass_ensure_symbol(compilation, slice_literal("Value")),
    type_value_value
  );
  Source_Range Value_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value_Tag__source_range, "Value_Tag");
  scope_define_enum(
    compilation, scope, Value_Tag__source_range,
    slice_literal("Value_Tag"), type_value_tag_value,
    value_tag_items, countof(value_tag_items)
  );
  Source_Range Value_Lazy__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value_Lazy__source_range, "Value_Lazy");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Value_Lazy__source_range,
    mass_ensure_symbol(compilation, slice_literal("Value_Lazy")),
    type_value_lazy_value
  );
  Source_Range Value_Forced__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Value_Forced__source_range, "Value_Forced");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Value_Forced__source_range,
    mass_ensure_symbol(compilation, slice_literal("Value_Forced")),
    type_value_forced_value
  );
  Source_Range Function_Builder__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Function_Builder__source_range, "Function_Builder");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Function_Builder__source_range,
    mass_ensure_symbol(compilation, slice_literal("Function_Builder")),
    type_function_builder_value
  );
  Source_Range Expected_Result__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Expected_Result__source_range, "Expected_Result");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Expected_Result__source_range,
    mass_ensure_symbol(compilation, slice_literal("Expected_Result")),
    type_expected_result_value
  );
  Source_Range Expected_Result_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Expected_Result_Tag__source_range, "Expected_Result_Tag");
  scope_define_enum(
    compilation, scope, Expected_Result_Tag__source_range,
    slice_literal("Expected_Result_Tag"), type_expected_result_tag_value,
    expected_result_tag_items, countof(expected_result_tag_items)
  );
  Source_Range Expected_Result_Exact__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Expected_Result_Exact__source_range, "Expected_Result_Exact");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Expected_Result_Exact__source_range,
    mass_ensure_symbol(compilation, slice_literal("Expected_Result_Exact")),
    type_expected_result_exact_value
  );
  Source_Range Expected_Result_Flexible__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Expected_Result_Flexible__source_range, "Expected_Result_Flexible");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Expected_Result_Flexible__source_range,
    mass_ensure_symbol(compilation, slice_literal("Expected_Result_Flexible")),
    type_expected_result_flexible_value
  );
  Source_Range Intrinsic_Proc__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Intrinsic_Proc__source_range, "Intrinsic_Proc");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Intrinsic_Proc__source_range,
    mass_ensure_symbol(compilation, slice_literal("Intrinsic_Proc")),
    type_mass_intrinsic_proc_value
  );
  Source_Range Function_Info__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Function_Info__source_range, "Function_Info");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Function_Info__source_range,
    mass_ensure_symbol(compilation, slice_literal("Function_Info")),
    type_function_info_value
  );
  Source_Range Function_Header__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Function_Header__source_range, "Function_Header");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Function_Header__source_range,
    mass_ensure_symbol(compilation, slice_literal("Function_Header")),
    type_function_header_value
  );
  Source_Range Function_Literal__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Function_Literal__source_range, "Function_Literal");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Function_Literal__source_range,
    mass_ensure_symbol(compilation, slice_literal("Function_Literal")),
    type_function_literal_value
  );
  Source_Range Function_Call_Lazy_Payload__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Function_Call_Lazy_Payload__source_range, "Function_Call_Lazy_Payload");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Function_Call_Lazy_Payload__source_range,
    mass_ensure_symbol(compilation, slice_literal("Function_Call_Lazy_Payload")),
    type_mass_function_call_lazy_payload_value
  );
  Source_Range Tuple__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Tuple__source_range, "Tuple");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Tuple__source_range,
    mass_ensure_symbol(compilation, slice_literal("Tuple")),
    type_tuple_value
  );
  Source_Range List_Node__source_range;
  INIT_LITERAL_SOURCE_RANGE(&List_Node__source_range, "List_Node");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, List_Node__source_range,
    mass_ensure_symbol(compilation, slice_literal("List_Node")),
    type_list_node_value
  );
  Source_Range Descriptor__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor__source_range, "Descriptor");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor")),
    type_descriptor_value
  );
  Source_Range Descriptor_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Tag__source_range, "Descriptor_Tag");
  scope_define_enum(
    compilation, scope, Descriptor_Tag__source_range,
    slice_literal("Descriptor_Tag"), type_descriptor_tag_value,
    descriptor_tag_items, countof(descriptor_tag_items)
  );
  Source_Range Descriptor_Integer__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Integer__source_range, "Descriptor_Integer");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor_Integer__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Integer")),
    type_descriptor_integer_value
  );
  Source_Range Descriptor_Function_Instance__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Function_Instance__source_range, "Descriptor_Function_Instance");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor_Function_Instance__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Function_Instance")),
    type_descriptor_function_instance_value
  );
  Source_Range Descriptor_Fixed_Array__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Fixed_Array__source_range, "Descriptor_Fixed_Array");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor_Fixed_Array__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Fixed_Array")),
    type_descriptor_fixed_array_value
  );
  Source_Range Descriptor_Struct__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Struct__source_range, "Descriptor_Struct");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor_Struct__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Struct")),
    type_descriptor_struct_value
  );
  Source_Range Descriptor_Pointer_To__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Descriptor_Pointer_To__source_range, "Descriptor_Pointer_To");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Descriptor_Pointer_To__source_range,
    mass_ensure_symbol(compilation, slice_literal("Descriptor_Pointer_To")),
    type_descriptor_pointer_to_value
  );
  Source_Range Error__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error__source_range, "Error");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error")),
    type_mass_error_value
  );
  Source_Range Error_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Tag__source_range, "Error_Tag");
  scope_define_enum(
    compilation, scope, Error_Tag__source_range,
    slice_literal("Error_Tag"), type_mass_error_tag_value,
    mass_error_tag_items, countof(mass_error_tag_items)
  );
  Source_Range Error_User_Defined__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_User_Defined__source_range, "Error_User_Defined");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_User_Defined__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_User_Defined")),
    type_mass_error_user_defined_value
  );
  Source_Range Error_Circular_Dependency__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Circular_Dependency__source_range, "Error_Circular_Dependency");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Circular_Dependency__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Circular_Dependency")),
    type_mass_error_circular_dependency_value
  );
  Source_Range Error_Integer_Range__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Integer_Range__source_range, "Error_Integer_Range");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Integer_Range__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Integer_Range")),
    type_mass_error_integer_range_value
  );
  Source_Range Error_File_Open__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_File_Open__source_range, "Error_File_Open");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_File_Open__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Open")),
    type_mass_error_file_open_value
  );
  Source_Range Error_File_Too_Large__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_File_Too_Large__source_range, "Error_File_Too_Large");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_File_Too_Large__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_File_Too_Large")),
    type_mass_error_file_too_large_value
  );
  Source_Range Error_Dynamic_Library_Load__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Dynamic_Library_Load__source_range, "Error_Dynamic_Library_Load");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Dynamic_Library_Load__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Load")),
    type_mass_error_dynamic_library_load_value
  );
  Source_Range Error_Dynamic_Library_Symbol_Not_Found__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Dynamic_Library_Symbol_Not_Found__source_range, "Error_Dynamic_Library_Symbol_Not_Found");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Dynamic_Library_Symbol_Not_Found__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Dynamic_Library_Symbol_Not_Found")),
    type_mass_error_dynamic_library_symbol_not_found_value
  );
  Source_Range Error_Operator_Fixity_Conflict__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Operator_Fixity_Conflict__source_range, "Error_Operator_Fixity_Conflict");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Operator_Fixity_Conflict__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Operator_Fixity_Conflict")),
    type_mass_error_operator_fixity_conflict_value
  );
  Source_Range Error_Undefined_Variable__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Undefined_Variable__source_range, "Error_Undefined_Variable");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Undefined_Variable__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Undefined_Variable")),
    type_mass_error_undefined_variable_value
  );
  Source_Range Error_Redefinition__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Redefinition__source_range, "Error_Redefinition");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Redefinition__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Redefinition")),
    type_mass_error_redefinition_value
  );
  Source_Range Error_Unknown_Field__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Unknown_Field__source_range, "Error_Unknown_Field");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Unknown_Field__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Unknown_Field")),
    type_mass_error_unknown_field_value
  );
  Source_Range Error_Invalid_Identifier__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Invalid_Identifier__source_range, "Error_Invalid_Identifier");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Invalid_Identifier__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Invalid_Identifier")),
    type_mass_error_invalid_identifier_value
  );
  Source_Range Error_Type_Mismatch__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Type_Mismatch__source_range, "Error_Type_Mismatch");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Type_Mismatch__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Type_Mismatch")),
    type_mass_error_type_mismatch_value
  );
  Source_Range Error_No_Matching_Overload__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_No_Matching_Overload__source_range, "Error_No_Matching_Overload");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_No_Matching_Overload__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_No_Matching_Overload")),
    type_mass_error_no_matching_overload_value
  );
  Source_Range Error_Undecidable_Overload__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Error_Undecidable_Overload__source_range, "Error_Undecidable_Overload");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Error_Undecidable_Overload__source_range,
    mass_ensure_symbol(compilation, slice_literal("Error_Undecidable_Overload")),
    type_mass_error_undecidable_overload_value
  );
  Source_Range Result__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Result__source_range, "Result");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Result__source_range,
    mass_ensure_symbol(compilation, slice_literal("Result")),
    type_mass_result_value
  );
  Source_Range Result_Tag__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Result_Tag__source_range, "Result_Tag");
  scope_define_enum(
    compilation, scope, Result_Tag__source_range,
    slice_literal("Result_Tag"), type_mass_result_tag_value,
    mass_result_tag_items, countof(mass_result_tag_items)
  );
  Source_Range Result_Error__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Result_Error__source_range, "Result_Error");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Result_Error__source_range,
    mass_ensure_symbol(compilation, slice_literal("Result_Error")),
    type_mass_result_error_value
  );
  Source_Range Os__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Os__source_range, "Os");
  scope_define_enum(
    compilation, scope, Os__source_range,
    slice_literal("Os"), type_os_value,
    os_items, countof(os_items)
  );
  Source_Range Calling_Convention_Call_Setup_Proc__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Calling_Convention_Call_Setup_Proc__source_range, "Calling_Convention_Call_Setup_Proc");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Calling_Convention_Call_Setup_Proc__source_range,
    mass_ensure_symbol(compilation, slice_literal("Calling_Convention_Call_Setup_Proc")),
    type_calling_convention_call_setup_proc_value
  );
  Source_Range Calling_Convention__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Calling_Convention__source_range, "Calling_Convention");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Calling_Convention__source_range,
    mass_ensure_symbol(compilation, slice_literal("Calling_Convention")),
    type_calling_convention_value
  );
  Source_Range Compilation__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Compilation__source_range, "Compilation");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Compilation__source_range,
    mass_ensure_symbol(compilation, slice_literal("Compilation")),
    type_compilation_value
  );
  Source_Range Lazy_Value_Proc__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Lazy_Value_Proc__source_range, "Lazy_Value_Proc");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Lazy_Value_Proc__source_range,
    mass_ensure_symbol(compilation, slice_literal("Lazy_Value_Proc")),
    type_lazy_value_proc_value
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    push_instruction, "push_instruction", &descriptor_void,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("code_block")),
      .descriptor = &descriptor_code_block_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("instruction")),
      .descriptor = &descriptor_instruction
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_get, "get", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_get_from_descriptor_module, "get_from_descriptor_module", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_import, "import", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_intrinsic, "intrinsic", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_apply, "apply", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_call, "call", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_using, "using", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_return, "return", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_operator_assignment, "operator_assignment", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_define_inferred, "define_inferred", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_comma, "comma", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_function_literal, "function_literal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_dereference, "dereference", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_array_like_get, "unchecked_get_at_index", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_struct_get, "struct_get", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_named_accessor, "named_accessor", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_spread, "spread", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_typed_symbol, "typed_symbol", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_pointer_to, "pointer_to", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_pointer_to_type, "pointer_to_type", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_eval, "eval", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_inline_module, "inline_module", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_c_struct, "c_struct", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_exports, "exports", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_cast, "cast", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_zero_extend, "zero_extend", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_parse_type, "parse_type", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_type_of, "type_of", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_size_of, "size_of", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_static_assert, "static_assert", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    allocator_allocate_bytes, "allocator_allocate_bytes", &descriptor_void_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("byte_size")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("byte_alignment")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_integer_type, "constraint_integer_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_float_type, "constraint_float_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_pointer_type, "constraint_pointer_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_struct_type, "constraint_struct_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_fixed_array_type, "constraint_fixed_array_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_constraint_function_instance_type, "constraint_function_instance_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_tuple_length, "tuple_length", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("tuple")),
      .descriptor = &descriptor_tuple_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_tuple_get, "tuple_get", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("tuple")),
      .descriptor = &descriptor_tuple_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("index")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    descriptor_pointer_to, "descriptor_pointer_to", &descriptor_descriptor_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("compilation")),
      .descriptor = &descriptor_compilation_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("descriptor")),
      .descriptor = &descriptor_descriptor_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    scope_make_imperative, "scope_make_imperative", &descriptor_scope_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parent")),
      .descriptor = &descriptor_scope_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("entry")),
      .descriptor = &descriptor_scope_entry_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    scope_make_declarative, "scope_make_declarative", &descriptor_scope_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parent")),
      .descriptor = &descriptor_scope_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_ensure_symbol, "ensure_symbol", &descriptor_symbol_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("compilation")),
      .descriptor = &descriptor_compilation_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("name")),
      .descriptor = &descriptor_slice
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    scope_define_value, "scope_define_value", &descriptor_void,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("scope")),
      .descriptor = &descriptor_scope_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("epoch")),
      .descriptor = &descriptor_epoch
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("source_range")),
      .descriptor = &descriptor_source_range
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("symbol")),
      .descriptor = &descriptor_symbol_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("value")),
      .descriptor = &descriptor_value_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    same_type, "same_type", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_descriptor_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_descriptor_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    register_acquire_temp, "register_acquire_temp", &descriptor_register,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    register_release, "register_release", &descriptor_void,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("reg")),
      .descriptor = &descriptor_register
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    storage_register, "storage_register", &descriptor_storage,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("reg")),
      .descriptor = &descriptor_register
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("bit_size")),
      .descriptor = &descriptor_bits
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    storage_register_temp, "storage_register_temp", &descriptor_storage,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("bit_size")),
      .descriptor = &descriptor_bits
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    storage_release_if_temporary, "storage_release_if_temporary", &descriptor_void,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("storage")),
      .descriptor = &descriptor_storage_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_expected_result_exact, "expected_result_exact", &descriptor_expected_result,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("descriptor")),
      .descriptor = &descriptor_descriptor_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("storage")),
      .descriptor = &descriptor_storage
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_expected_result_exact_type, "expected_result_exact_type", &descriptor_expected_result,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("type")),
      .descriptor = &descriptor_type
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("storage")),
      .descriptor = &descriptor_storage
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    value_force, "value_force", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("scope")),
      .descriptor = &descriptor_scope_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("expected_result")),
      .descriptor = &descriptor_expected_result_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("value")),
      .descriptor = &descriptor_value_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_module_get_impl, "module_get", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("module")),
      .descriptor = &descriptor_module_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("symbol")),
      .descriptor = &descriptor_symbol_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("source_range")),
      .descriptor = &descriptor_source_range_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_forward_call_to_alias, "forward_call_to_alias", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("symbol")),
      .descriptor = &descriptor_symbol_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_logical_shift_left, "i64_logical_shift_left", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_logical_shift_right, "i64_logical_shift_right", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_bitwise_and, "i64_bitwise_and", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_bitwise_or, "i64_bitwise_or", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_add, "i64_add", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_subtract, "i64_subtract", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_multiply, "i64_signed_multiply", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_multiply, "i64_unsigned_multiply", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_divide, "i64_signed_divide", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_divide, "i64_unsigned_divide", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_remainder, "i64_signed_remainder", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_remainder, "i64_unsigned_remainder", &descriptor_i64,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_less, "i64_signed_less", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_less, "i64_unsigned_less", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_less_equal, "i64_signed_less_equal", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_less_equal, "i64_unsigned_less_equal", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_greater, "i64_signed_greater", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_greater, "i64_unsigned_greater", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_signed_greater_equal, "i64_signed_greater_equal", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_i64_unsigned_greater_equal, "i64_unsigned_greater_equal", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("a")),
      .descriptor = &descriptor_i64
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("b")),
      .descriptor = &descriptor_i64
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    value_make, "value_make", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("descriptor")),
      .descriptor = &descriptor_descriptor_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("storage")),
      .descriptor = &descriptor_storage
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("source_range")),
      .descriptor = &descriptor_source_range
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    descriptor_function_instance, "descriptor_function_instance", &descriptor_descriptor_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("info")),
      .descriptor = &descriptor_function_info_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("call_setup")),
      .descriptor = &descriptor_function_call_setup
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("program")),
      .descriptor = &descriptor_program_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_result_is_error, "result_is_error", &descriptor__bool,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("result")),
      .descriptor = &descriptor_mass_result_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    calling_convention_x86_64_system_v_call_setup_proc, "calling_convention_x86_64_system_v_call_setup_proc", &descriptor_function_call_setup,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("function")),
      .descriptor = &descriptor_function_info_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    calling_convention_x86_64_system_v_syscall_setup_proc, "calling_convention_x86_64_system_v_syscall_setup_proc", &descriptor_function_call_setup,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("allocator")),
      .descriptor = &descriptor_allocator_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("function")),
      .descriptor = &descriptor_function_info_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    mass_function_info_init_for_header_and_maybe_body, "function_info_init_for_header_and_maybe_body", &descriptor_void,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("scope")),
      .descriptor = &descriptor_scope_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("header")),
      .descriptor = &descriptor_function_header_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("maybe_body")),
      .descriptor = &descriptor_value_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("out_info")),
      .descriptor = &descriptor_function_info_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None,
    call_function_overload, "call_function_overload", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("builder")),
      .descriptor = &descriptor_function_builder_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("expected_result")),
      .descriptor = &descriptor_expected_result_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("scope")),
      .descriptor = &descriptor_scope_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("source_range")),
      .descriptor = &descriptor_source_range_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("payload")),
      .descriptor = &descriptor_mass_function_call_lazy_payload_pointer
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_add, "integer_add", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_subtract, "integer_subtract", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_multiply, "integer_multiply", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_divide, "integer_divide", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_remainder, "integer_remainder", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_less, "integer_less", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_greater, "integer_greater", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_less_equal, "integer_less_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_greater_equal, "integer_greater_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_equal, "integer_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_integer_not_equal, "integer_not_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_generic_equal, "generic_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  MASS_DEFINE_FUNCTION(
    Function_Info_Flags_None | Function_Info_Flags_Intrinsic,
    mass_generic_not_equal, "generic_not_equal", &descriptor_value_pointer,
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("context")),
      .descriptor = &descriptor_mass_context_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("parser")),
      .descriptor = &descriptor_parser_pointer
    },
    (Resolved_Function_Parameter) {
      .symbol = mass_ensure_symbol(compilation, slice_literal("args")),
      .descriptor = &descriptor_value_view
    }
  );
  Source_Range Allocator__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Allocator__source_range, "Allocator");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Allocator__source_range,
    mass_ensure_symbol(compilation, slice_literal("Allocator")),
    type_allocator_value
  );
  Source_Range Dyn_Array_Internal__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Dyn_Array_Internal__source_range, "Dyn_Array_Internal");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Dyn_Array_Internal__source_range,
    mass_ensure_symbol(compilation, slice_literal("Dyn_Array_Internal")),
    type_dyn_array_internal_value
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
  Source_Range Type__source_range;
  INIT_LITERAL_SOURCE_RANGE(&Type__source_range, "Type");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, Type__source_range,
    mass_ensure_symbol(compilation, slice_literal("Type")),
    type_type_value
  );
  Source_Range bool__source_range;
  INIT_LITERAL_SOURCE_RANGE(&bool__source_range, "bool");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, bool__source_range,
    mass_ensure_symbol(compilation, slice_literal("bool")),
    type__bool_value
  );
  Source_Range i8__source_range;
  INIT_LITERAL_SOURCE_RANGE(&i8__source_range, "i8");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, i8__source_range,
    mass_ensure_symbol(compilation, slice_literal("i8")),
    type_i8_value
  );
  Source_Range i16__source_range;
  INIT_LITERAL_SOURCE_RANGE(&i16__source_range, "i16");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, i16__source_range,
    mass_ensure_symbol(compilation, slice_literal("i16")),
    type_i16_value
  );
  Source_Range i32__source_range;
  INIT_LITERAL_SOURCE_RANGE(&i32__source_range, "i32");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, i32__source_range,
    mass_ensure_symbol(compilation, slice_literal("i32")),
    type_i32_value
  );
  Source_Range i64__source_range;
  INIT_LITERAL_SOURCE_RANGE(&i64__source_range, "i64");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, i64__source_range,
    mass_ensure_symbol(compilation, slice_literal("i64")),
    type_i64_value
  );
  Source_Range u8__source_range;
  INIT_LITERAL_SOURCE_RANGE(&u8__source_range, "u8");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, u8__source_range,
    mass_ensure_symbol(compilation, slice_literal("u8")),
    type_u8_value
  );
  Source_Range u16__source_range;
  INIT_LITERAL_SOURCE_RANGE(&u16__source_range, "u16");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, u16__source_range,
    mass_ensure_symbol(compilation, slice_literal("u16")),
    type_u16_value
  );
  Source_Range u32__source_range;
  INIT_LITERAL_SOURCE_RANGE(&u32__source_range, "u32");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, u32__source_range,
    mass_ensure_symbol(compilation, slice_literal("u32")),
    type_u32_value
  );
  Source_Range u64__source_range;
  INIT_LITERAL_SOURCE_RANGE(&u64__source_range, "u64");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, u64__source_range,
    mass_ensure_symbol(compilation, slice_literal("u64")),
    type_u64_value
  );
  Source_Range s8__source_range;
  INIT_LITERAL_SOURCE_RANGE(&s8__source_range, "s8");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, s8__source_range,
    mass_ensure_symbol(compilation, slice_literal("s8")),
    type_s8_value
  );
  Source_Range s16__source_range;
  INIT_LITERAL_SOURCE_RANGE(&s16__source_range, "s16");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, s16__source_range,
    mass_ensure_symbol(compilation, slice_literal("s16")),
    type_s16_value
  );
  Source_Range s32__source_range;
  INIT_LITERAL_SOURCE_RANGE(&s32__source_range, "s32");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, s32__source_range,
    mass_ensure_symbol(compilation, slice_literal("s32")),
    type_s32_value
  );
  Source_Range s64__source_range;
  INIT_LITERAL_SOURCE_RANGE(&s64__source_range, "s64");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, s64__source_range,
    mass_ensure_symbol(compilation, slice_literal("s64")),
    type_s64_value
  );
  Source_Range f32__source_range;
  INIT_LITERAL_SOURCE_RANGE(&f32__source_range, "f32");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, f32__source_range,
    mass_ensure_symbol(compilation, slice_literal("f32")),
    type_f32_value
  );
  Source_Range f64__source_range;
  INIT_LITERAL_SOURCE_RANGE(&f64__source_range, "f64");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, f64__source_range,
    mass_ensure_symbol(compilation, slice_literal("f64")),
    type_f64_value
  );
  Source_Range String__source_range;
  INIT_LITERAL_SOURCE_RANGE(&String__source_range, "String");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, String__source_range,
    mass_ensure_symbol(compilation, slice_literal("String")),
    type_slice_value
  );
}

