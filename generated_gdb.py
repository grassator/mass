MASS_TYPES = {
  'Bits': 'struct',
  'Source_Position': 'struct',
  'Source_File': 'struct',
  'Source_Range': 'struct',
  'Mass_While': 'struct',
  'Assignment': 'struct',
  'Module_Exports': 'tagged_union',
  'Module': 'struct',
  'Parse_Error': 'struct',
  'Value_View': 'struct',
  'Symbol': 'struct',
  'Group_Paren': 'struct',
  'Ast_Return': 'struct',
  'Ast_Statement': 'struct',
  'Ast_Block': 'struct',
  'Group_Square': 'struct',
  'Ast_Using': 'struct',
  'Section_Permissions': 'enum',
  'Section': 'struct',
  'Program_Memory': 'struct',
  'Register': 'enum',
  'Label': 'struct',
  'Label_Location_Diff_Patch_Info': 'struct',
  'Number_Base': 'enum',
  'Quoted': 'struct',
  'Spread': 'struct',
  'Named_Accessor': 'struct',
  'External_Symbol': 'struct',
  'Import_Symbol': 'struct',
  'Import_Library': 'struct',
  'Compare_Type': 'enum',
  'Stack_Area': 'enum',
  'Memory_Location': 'tagged_union',
  'Storage_Flags': 'enum',
  'Storage': 'tagged_union',
  'Relocation': 'struct',
  'Instruction_Assembly': 'struct',
  'Instruction': 'tagged_union',
  'Instruction_Bucket': 'struct',
  'Code_Block': 'struct',
  'Epoch': 'struct',
  'Function_Layout': 'struct',
  'Mass_Context': 'struct',
  'Parser_Flags': 'enum',
  'Parser': 'struct',
  'Operator_Fixity': 'enum',
  'Operator_Associativity': 'enum',
  'Operator_Flags': 'enum',
  'Operator': 'tagged_union',
  'Scope_Entry': 'struct',
  'Operator_Map': 'hash_map',
  'Operator_Symbol_Map': 'hash_map',
  'Scope': 'tagged_union',
  'Overload': 'struct',
  'Undecidable_Match': 'struct',
  'Overload_Match': 'tagged_union',
  'Overload_Match_Summary': 'struct',
  'Overload_Match_State': 'struct',
  'Value_Flags': 'enum',
  'Value': 'tagged_union',
  'Register_Bitset': 'struct',
  'Function_Builder': 'struct',
  'Expected_Result': 'tagged_union',
  'Lazy_Static_Value': 'struct',
  'Function_Parameter': 'tagged_union',
  'Resolved_Function_Parameter': 'tagged_union',
  'Function_Info_Flags': 'enum',
  'Function_Return': 'tagged_union',
  'Function_Info': 'struct',
  'Function_Header_Flags': 'enum',
  'Function_Specialization': 'struct',
  'Function_Header': 'struct',
  'Function_Literal': 'struct',
  'Function_Call_Parameter_Flags': 'enum',
  'Function_Call_Parameter': 'struct',
  'Function_Call_Setup': 'struct',
  'SYSTEM_V_ARGUMENT_CLASS': 'enum',
  'System_V_Classification': 'struct',
  'System_V_Registers': 'struct',
  'System_V_Register_State': 'struct',
  'System_V_Eightbyte_Array': 'struct',
  'Mass_Function_Call_Lazy_Payload': 'struct',
  'Tuple': 'struct',
  'List_Node': 'struct',
  'Typed_Symbol': 'struct',
  'Struct_Field': 'struct',
  'Descriptor': 'tagged_union',
  'Type': 'struct',
  'Mass_Error': 'tagged_union',
  'Mass_Result': 'tagged_union',
  'Os': 'enum',
  'Program': 'struct',
  'Calling_Convention': 'struct',
  'Mass_Trampoline': 'struct',
  'Struct_Field_Set': 'hash_map',
  'Slice_Set': 'hash_map',
  'Symbol_Map': 'hash_map',
  'Trampoline_Map': 'hash_map',
  'Scope_Map': 'hash_map',
  'Macro_Replacement_Map': 'hash_map',
  'Jit_Import_Library_Handle_Map': 'hash_map',
  'Imported_Module_Map': 'hash_map',
  'Jit_Counters': 'struct',
  'Jit': 'struct',
  'Static_Pointer_Length_Map': 'hash_map',
  'Descriptor_Pointer_To_Cache_Map': 'hash_map',
  'Intrinsic_Proc_Cache_Map': 'hash_map',
  'Common_Symbols': 'struct',
  'Compilation': 'struct',
  'Instruction_Extension_Type': 'enum',
  'Operand_Encoding_Type': 'enum',
  'Operand_Encoding': 'struct',
  'Instruction_Encoding': 'struct',
  'X64_Mnemonic': 'struct',
  'Range_u8': 'struct',
  'Range_u16': 'struct',
  'Range_u32': 'struct',
  'Range_u64': 'struct',
  'Range_s8': 'struct',
  'Range_s16': 'struct',
  'Range_s32': 'struct',
  'Range_s64': 'struct',
  'Range_f32': 'struct',
  'Range_f64': 'struct',
  'Slice': 'struct',
  'Dyn_Array_Internal': 'struct',
}