#ifndef C_MACRO_H
#define C_MACRO_H
#include <stddef.h>
#include <limits.h>

#define MASS_DESCRIPTOR_STATIC_ARRAY(_C_TYPE_, _LENGTH_, ...)\
  {\
    .tag = Descriptor_Tag_Fixed_Size_Array,\
    .bit_size = {sizeof(_C_TYPE_) * CHAR_BIT * (_LENGTH_)},\
    .bit_alignment = _Alignof(_C_TYPE_) * CHAR_BIT,\
    .Fixed_Size_Array = {\
      .item = (__VA_ARGS__),\
      .length = (_LENGTH_),\
    },\
  }

#define MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_)\
  static Descriptor descriptor_##_NAME_##_pointer = {\
    .tag = Descriptor_Tag_Pointer_To,\
    .name = slice_literal_fields(#_NAME_),\
    .bit_size = {sizeof(void *) * CHAR_BIT},\
    .bit_alignment = _Alignof(void *) * CHAR_BIT,\
    .Pointer_To.descriptor = &descriptor_##_NAME_,\
  }

#define MASS_DEFINE_OPAQUE_DESCRIPTOR(_NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, ...)\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Opaque,\
    .name = slice_literal_fields(#_NAME_),\
    .bit_size = {_BIT_SIZE_},\
    .bit_alignment = (_BIT_ALIGNMENT_),\
    __VA_ARGS__\
  };\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_);\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_##_pointer)

#define MASS_DEFINE_STRUCT_DESCRIPTOR(_NAME_, _C_TYPE_, ...)\
  dyn_array_struct(Memory_Layout_Item) descriptor_##_NAME_##_fields = {\
    .length = countof((const Memory_Layout_Item[]){__VA_ARGS__}),\
    .items = {__VA_ARGS__},\
  };\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Struct,\
    .name = slice_literal_fields(#_NAME_),\
    .bit_size = {sizeof(_C_TYPE_) * CHAR_BIT},\
    .bit_alignment = _Alignof(_C_TYPE_) * CHAR_BIT,\
    .Struct = {\
      .memory_layout = {\
        .items = {(Dyn_Array_Internal *)&descriptor_##_NAME_##_fields},\
      }\
    },\
  };\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_);\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_##_pointer)

#define VALUE_STATIC_EPOCH 0

#define MASS_TYPE_VALUE(_DESCRIPTOR_)\
  (Value) {\
    .descriptor = &descriptor_descriptor_pointer,\
    .storage = {\
      .tag = Storage_Tag_Static,\
      .bit_size = {sizeof(Descriptor *) * CHAR_BIT},\
      .Static.memory = {.tag = Static_Memory_Tag_U64, .U64 = {(u64)(_DESCRIPTOR_)}},\
    },\
  }

#define MASS_DEFINE_TYPE_VALUE(_NAME_)\
  static Value *type_##_NAME_##_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_);\
  static Value *type_##_NAME_##_pointer_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_##_pointer);

#define MASS_DEFINE_OPAQUE_TYPE(_NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, ...)\
  MASS_DEFINE_OPAQUE_DESCRIPTOR(_NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, __VA_ARGS__);\
  MASS_DEFINE_TYPE_VALUE(_NAME_);

#define MASS_DEFINE_OPAQUE_C_TYPE(_NAME_, _C_TYPE_, ...)\
  MASS_DEFINE_OPAQUE_TYPE(_NAME_, sizeof(_C_TYPE_) * CHAR_BIT, _Alignof(_C_TYPE_) * CHAR_BIT, __VA_ARGS__)

#define MASS_FN_ARG_DEFAULT_EXPRESSION(_VAR_NAME_, _EXPR_)\
  Value_View _VAR_NAME_;\
  {\
    Source_File *fake_file = allocator_allocate(allocator, Source_File);\
    *fake_file = (Source_File){\
      .path = COMPILER_SOURCE_FILE.path,\
      .text = slice_literal(_EXPR_),\
    };\
    Source_Range fake_range = {\
      .source_file = fake_file,\
      .offsets = {.from = 0, .to = fake_file->text.length},\
    };\
    MASS_ON_ERROR(tokenize(compilation, fake_range, &_VAR_NAME_)) panic("unreached");\
  }

#define MASS_DEFINE_FUNCTION_INFO_HELPER(_FLAGS_, _RETURN_DESCRIPTOR_, ...)\
  Function_Parameter raw_parameters[] = {__VA_ARGS__};\
  u64 arg_length = countof(raw_parameters);\
  Array_Function_Parameter parameters = \
    dyn_array_make(Array_Function_Parameter, .allocator = allocator, .capacity = arg_length);\
  for (u64 i = 0; i < arg_length; ++i) {\
    dyn_array_push(parameters, raw_parameters[i]);\
  }\
  Function_Info *function = allocator_allocate(allocator, Function_Info);\
  *function = (Function_Info){\
    .flags = (_FLAGS_),\
    .returns.declaration.descriptor = (_RETURN_DESCRIPTOR_),\
    .parameters = parameters,\
  };

#define MASS_DEFINE_FUNCTION_TYPE(_FLAGS_, _FN_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
do {\
  MASS_DEFINE_FUNCTION_INFO_HELPER((_FLAGS_), (_RETURN_DESCRIPTOR_), ##__VA_ARGS__)\
  Value *info_value = value_init(\
    allocator_allocate(allocator, Value),\
    &descriptor_function_info, storage_static(function), COMPILER_SOURCE_RANGE\
  );\
  const Symbol *fn_symbol = mass_ensure_symbol(compilation, slice_literal(_NAME_));\
  scope_define_value(scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE, fn_symbol, info_value);\
} while(0)

#define MASS_DEFINE_FUNCTION(_FLAGS_, _FN_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
do {\
  MASS_DEFINE_FUNCTION_INFO_HELPER((_FLAGS_), (_RETURN_DESCRIPTOR_), ##__VA_ARGS__)\
  Function_Call_Setup call_setup = calling_convention->call_setup_proc(allocator, function);\
  const Descriptor *instance_descriptor = descriptor_function_instance(\
    allocator, slice_literal(_NAME_), function, call_setup\
  );\
  Value *instance_value = value_init(\
    allocator_allocate(allocator, Value),\
    instance_descriptor, imm64((u64)_FN_), COMPILER_SOURCE_RANGE\
  );\
  const Symbol *fn_symbol = mass_ensure_symbol(compilation, slice_literal(_NAME_));\
  scope_define_value(scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE, fn_symbol, instance_value);\
} while(0)

typedef struct {
  Slice name;
  s32 value;
} C_Enum_Item;

#include "generated_types.h"

static Descriptor descriptor_void = {
  .tag = Descriptor_Tag_Opaque,
  .name = slice_literal_fields("void"),
};
MASS_DEFINE_TYPE_VALUE(void);
MASS_DEFINE_POINTER_DESCRIPTOR(void);

#endif // C_MACRO_H
