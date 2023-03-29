#ifndef C_MACRO_H
#define C_MACRO_H
#include <stddef.h>
#include <limits.h>

// :TypeStruct
#define MASS_TYPE_VALUE(_DESCRIPTOR_)\
  (Value) {\
    .tag = Value_Tag_Forced,\
    .descriptor = &descriptor_type,\
    .Forced = {\
      .storage = { \
        .tag = Storage_Tag_Immediate,\
        .bit_size = {sizeof(Descriptor *) * CHAR_BIT},\
        .Immediate.bits = (u64)(_DESCRIPTOR_),\
      },\
    },\
  }

#define VALUE_STATIC_EPOCH (const Epoch){0}

#define MASS_DEFINE_TYPE_VALUE(_NAME_)\
  static Value *type_##_NAME_##_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_);\
  static Value *type_##_NAME_##_pointer_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_##_pointer);

#define _INIT_LITERAL_SOURCE_RANGE_STRINGIFY(X) #X
#define _INIT_LITERAL_SOURCE_RANGE_PROXY(X) _INIT_LITERAL_SOURCE_RANGE_STRINGIFY(X)
#define INIT_LITERAL_SOURCE_RANGE(_TO_INIT_, _C_STRING_)\
  do {\
    static const Source_File FAKE_SOURCE_FILE = {\
      .path = slice_literal_fields(__FILE__ ":" _INIT_LITERAL_SOURCE_RANGE_PROXY(__LINE__)),\
      .text = slice_literal_fields(_C_STRING_),\
    };\
    *(_TO_INIT_) = (Source_Range){\
      .file = &FAKE_SOURCE_FILE,\
      .offsets = {.from = 0, .to = (u32)FAKE_SOURCE_FILE.text.length}\
    };\
  } while(0)

#define MASS_DESCRIPTOR_STATIC_ARRAY(_C_TYPE_, _LENGTH_, ...)\
  {\
    .tag = Descriptor_Tag_Fixed_Array,\
    .bit_size = {sizeof(_C_TYPE_) * CHAR_BIT * (_LENGTH_)},\
    .bit_alignment = _Alignof(_C_TYPE_) * CHAR_BIT,\
    .Fixed_Array = {\
      .item = (__VA_ARGS__),\
      .length = (_LENGTH_),\
    },\
  }

#define MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_)\
  static Descriptor descriptor_##_NAME_##_pointer = {\
    .tag = Descriptor_Tag_Pointer_To,\
    .bit_size = {sizeof(void *) * CHAR_BIT},\
    .bit_alignment = _Alignof(void *) * CHAR_BIT,\
    .Pointer_To.descriptor = &descriptor_##_NAME_,\
  }

#define MASS_DEFINE_DESCRIPTOR_BASE(_TAG_, _NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, ...)\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = (_TAG_),\
    .bit_size = {_BIT_SIZE_},\
    .bit_alignment = (_BIT_ALIGNMENT_),\
    __VA_ARGS__\
  };\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_);\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_##_pointer)

#define MASS_DEFINE_DESCRIPTOR(_TAG_, _NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, ...)\
  static const Symbol mass_meta_brand_##_NAME_ = {.name = slice_literal_fields(#_NAME_) };\
  MASS_DEFINE_DESCRIPTOR_BASE(\
    (_TAG_), _NAME_, (_BIT_SIZE_), (_BIT_ALIGNMENT_),\
    .brand = &(mass_meta_brand_##_NAME_),\
    __VA_ARGS__\
  )

#define MASS_DEFINE_OPAQUE_DESCRIPTOR(...)\
  MASS_DEFINE_DESCRIPTOR(Descriptor_Tag_Raw, __VA_ARGS__)

#define MASS_DEFINE_EMPTY_STRUCT_DESCRIPTOR_WITH_BRAND(_NAME_, _BRAND_)\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Struct,\
    .brand = (_BRAND_),\
    .bit_size = {0},\
    .bit_alignment = 0,\
    .Struct = {\
      .fields = {.internal = &dyn_array_static_empty_internal},\
    },\
  };\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_);\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_##_pointer)

#define MASS_DEFINE_STRUCT_DESCRIPTOR_WITH_BRAND(_NAME_, _C_TYPE_, _BRAND_, ...)\
  dyn_array_struct(Struct_Field) descriptor_##_NAME_##_fields = {\
    .length = countof((const Struct_Field[]){__VA_ARGS__}),\
    .items = {__VA_ARGS__},\
  };\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Struct,\
    .brand = (_BRAND_),\
    .bit_size = {sizeof(_C_TYPE_) * CHAR_BIT},\
    .bit_alignment = _Alignof(_C_TYPE_) * CHAR_BIT,\
    .Struct = {\
      .fields = {(Dyn_Array_Internal *)&descriptor_##_NAME_##_fields},\
    },\
  };\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_);\
  MASS_DEFINE_POINTER_DESCRIPTOR(_NAME_##_pointer)

#define MASS_DEFINE_STRUCT_DESCRIPTOR(_NAME_, _C_TYPE_, ...)\
  static const Symbol mass_meta_brand_##_NAME_ = {.name = slice_literal_fields(#_NAME_) };\
  MASS_DEFINE_STRUCT_DESCRIPTOR_WITH_BRAND(_NAME_, _C_TYPE_, &(mass_meta_brand_##_NAME_), __VA_ARGS__)

#define MASS_DEFINE_C_DYN_ARRAY_TYPE(_NAME_, _ITEM_, _C_TYPE_)\
  MASS_DEFINE_STRUCT_DESCRIPTOR(_NAME_,  _C_TYPE_, \
    {\
      .descriptor = &descriptor_dyn_array_internal_pointer,\
      .name = slice_literal_fields("internal"),\
      .offset = 0,\
    },\
  )

#define MASS_DEFINE_FUNCTION_DESCRIPTOR(_NAME_, _RETURN_DESCRIPTOR_, ...)\
  dyn_array_struct(Resolved_Function_Parameter) descriptor_##_NAME_##__parameters = {\
    .length = countof((const Resolved_Function_Parameter[]){__VA_ARGS__}),\
    .items = {__VA_ARGS__},\
  };\
  static Function_Info descriptor_##_NAME_##__info = {\
    .flags = Function_Info_Flags_None,\
    .return_descriptor = (_RETURN_DESCRIPTOR_),\
    .parameters = {(Dyn_Array_Internal *)&descriptor_##_NAME_##__parameters,}\
  };\
  static Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Function_Instance,\
    .bit_size = {sizeof(void *) * CHAR_BIT},\
    .bit_alignment = _Alignof(void *) * CHAR_BIT,\
    .Function_Instance = { .info = &descriptor_##_NAME_##__info, .call_setup = 0/*FIXME*/, },\
  };\
  static Value *type_##_NAME_##_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_);

#define MASS_DEFINE_OPAQUE_TYPE(_NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, ...)\
  MASS_DEFINE_OPAQUE_DESCRIPTOR(_NAME_, _BIT_SIZE_, _BIT_ALIGNMENT_, __VA_ARGS__);\
  MASS_DEFINE_TYPE_VALUE(_NAME_);

#define MASS_DEFINE_OPAQUE_C_TYPE(_NAME_, _C_TYPE_, ...)\
  MASS_DEFINE_OPAQUE_TYPE(_NAME_, sizeof(_C_TYPE_) * CHAR_BIT, _Alignof(_C_TYPE_) * CHAR_BIT, __VA_ARGS__)

#define MASS_DEFINE_FLOAT_C_TYPE(_NAME_, _C_TYPE_)\
  MASS_DEFINE_DESCRIPTOR(\
    Descriptor_Tag_Float, _NAME_, sizeof(_C_TYPE_) * CHAR_BIT, _Alignof(_C_TYPE_) * CHAR_BIT\
  );\
  MASS_DEFINE_TYPE_VALUE(_NAME_);

#define MASS_DEFINE_RAW_C_TYPE(_NAME_, _C_TYPE_)\
  MASS_DEFINE_DESCRIPTOR_BASE(\
    Descriptor_Tag_Raw, _NAME_, sizeof(_C_TYPE_) * CHAR_BIT, _Alignof(_C_TYPE_) * CHAR_BIT\
  );\
  MASS_DEFINE_TYPE_VALUE(_NAME_);

#define MASS_DEFINE_INTEGER_C_TYPE(_NAME_, _C_TYPE_, _IS_SIGNED_)\
  MASS_DEFINE_DESCRIPTOR(\
    Descriptor_Tag_Integer, _NAME_, sizeof(_C_TYPE_) * CHAR_BIT, _Alignof(_C_TYPE_) * CHAR_BIT,\
    .Integer.is_signed = (_IS_SIGNED_),\
  );\
  MASS_DEFINE_TYPE_VALUE(_NAME_);

#define MASS_FN_ARG_DEFAULT_EXPRESSION(_VAR_NAME_, _EXPR_)\
  Value_View _VAR_NAME_;\
  {\
    Source_Range source_range;\
    INIT_LITERAL_SOURCE_RANGE(&source_range, _EXPR_);\
    MASS_ON_ERROR(tokenize(compilation, source_range, &_VAR_NAME_)) panic("unreached");\
  }

#define MASS_DEFINE_FUNCTION_INFO_HELPER(_FLAGS_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
  Resolved_Function_Parameter raw_parameters[] = {__VA_ARGS__};\
  u64 arg_length = countof(raw_parameters);\
  Array_Resolved_Function_Parameter parameters = \
    dyn_array_make(Array_Resolved_Function_Parameter, .allocator = allocator, .capacity = arg_length);\
  for (u64 i = 0; i < arg_length; ++i) {\
    dyn_array_push(parameters, raw_parameters[i]);\
  }\
  Source_Range return_range;\
  INIT_LITERAL_SOURCE_RANGE(&return_range, _NAME_);\
  Function_Info *function = allocator_allocate(allocator, Function_Info);\
  *function = (Function_Info){\
    .flags = (_FLAGS_),\
    .return_descriptor = (_RETURN_DESCRIPTOR_),\
    .parameters = parameters,\
  };

#define MASS_DEFINE_FUNCTION_TYPE(_FLAGS_, _FN_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
do {\
  Source_Range source_range;\
  INIT_LITERAL_SOURCE_RANGE(&source_range, _NAME_);\
  MASS_DEFINE_FUNCTION_INFO_HELPER((_FLAGS_), (_NAME_), (_RETURN_DESCRIPTOR_), ##__VA_ARGS__)\
  Value *info_value = value_init(\
    allocator_allocate(allocator, Value),\
    &descriptor_function_info, storage_static(function), source_range\
  );\
  const Symbol *fn_symbol = mass_ensure_symbol(compilation, slice_literal(_NAME_));\
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, fn_symbol, info_value);\
} while(0)

#define MASS_DEFINE_FUNCTION(_FLAGS_, _FN_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
do {\
  Source_Range source_range;\
  INIT_LITERAL_SOURCE_RANGE(&source_range, _NAME_);\
  MASS_DEFINE_FUNCTION_INFO_HELPER((_FLAGS_), (_NAME_), (_RETURN_DESCRIPTOR_), ##__VA_ARGS__)\
  Function_Call_Setup call_setup = calling_convention->call_setup_proc(allocator, function);\
  const Descriptor *instance_descriptor = descriptor_function_instance(\
    allocator, function, call_setup, compilation->jit.program\
  );\
  Value *instance_value = value_init(\
    allocator_allocate(allocator, Value),\
    instance_descriptor, imm64((u64)_FN_), source_range\
  );\
  const Symbol *fn_symbol = mass_ensure_symbol(compilation, slice_literal(_NAME_));\
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, fn_symbol, instance_value);\
} while(0)

typedef struct {
  Slice name;
  s32 value;
} C_Enum_Item;


// Need to forward-declare this for the value helpers below
typedef struct Storage Storage;
static const void *
storage_static_memory(const Storage *);

#define DEFINE_VALUE_IS_AS_HELPERS(_C_TYPE_, _SUFFIX_)\
  static inline bool\
  value_is_##_SUFFIX_(\
    const Value *value\
  ) {\
    if (!value) return false;\
    if (value->tag != Value_Tag_Forced) return false;\
    const Storage *storage = &value->Forced.storage;\
    if (storage->tag != Storage_Tag_Static && storage->tag != Storage_Tag_Immediate) return false;\
    return same_type(value->descriptor, &descriptor_##_SUFFIX_);\
  }\
  static inline  _C_TYPE_ const *\
  value_as_##_SUFFIX_(\
    const Value *value\
  ) {\
    assert(value_is_##_SUFFIX_(value));\
    assert(value->Forced.storage.bit_size.as_u64 == value->descriptor->bit_size.as_u64);\
    return (_C_TYPE_ const *)storage_static_memory(&value->Forced.storage);\
  }

#include "generated_types.h"

static Descriptor descriptor_void = {
  .tag = Descriptor_Tag_Void,
};
MASS_DEFINE_TYPE_VALUE(void);
MASS_DEFINE_POINTER_DESCRIPTOR(void);

static Descriptor descriptor_never = {
  .tag = Descriptor_Tag_Never,
};
MASS_DEFINE_TYPE_VALUE(never);
MASS_DEFINE_POINTER_DESCRIPTOR(never);

typedef struct {
  void *bytes;
  u64 length;
} Mass_Byte_Slice;

MASS_DEFINE_STRUCT_DESCRIPTOR_WITH_BRAND(byte_slice, Mass_Byte_Slice, 0 /*no brand*/,
  {
    .descriptor = &descriptor_i8_pointer,
    .name = slice_literal_fields("bytes"),
    .offset = offsetof(Mass_Byte_Slice, bytes),
  },
  {
    .descriptor = &descriptor_i64,
    .name = slice_literal_fields("length"),
    .offset = offsetof(Mass_Byte_Slice, length),
  },
);
DEFINE_VALUE_IS_AS_HELPERS(Mass_Byte_Slice, byte_slice)

#endif // C_MACRO_H
