#ifndef C_MACRO_H
#define C_MACRO_H

#define COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS\
  {\
    .filename = __FILE__,\
    .function_name = "GLOBAL",\
    .line_number = __LINE__,\
  }

#define COMPILER_SOURCE_LOCATION_FIELDS\
  {\
    .filename = __FILE__,\
    .function_name = __func__,\
    .line_number = __LINE__,\
  }
#define COMPILER_SOURCE_LOCATION\
  ((Compiler_Source_Location)COMPILER_SOURCE_LOCATION_FIELDS)

#define MASS_DEFINE_OPAQUE_DESCRIPTOR(_NAME_, _BIT_SIZE_)\
  Descriptor descriptor_##_NAME_ = {\
    .tag = Descriptor_Tag_Opaque,\
    .name = slice_literal_fields(#_NAME_),\
    .Opaque = { .bit_size = (_BIT_SIZE_) },\
  }

#define MASS_DEFINE_OPAQUE_TYPE(_NAME_, _BIT_SIZE_)\
  MASS_DEFINE_OPAQUE_DESCRIPTOR(_NAME_, _BIT_SIZE_);\
  Value *type_##_NAME_##_value = &MASS_TYPE_VALUE(&descriptor_##_NAME_);

#define MASS_TYPE_VALUE(_DESCRIPTOR_)\
  (Value) {\
    .descriptor = &descriptor_type,\
    .operand = {\
      .tag = Operand_Tag_Immediate,\
      .byte_size = sizeof(Descriptor),\
      .Immediate.memory = (_DESCRIPTOR_),\
    },\
    .compiler_source_location = COMPILER_SOURCE_LOCATION_GLOBAL_FIELDS,\
  }

#define MASS_DEFINE_OPAQUE_C_TYPE(_NAME_, _C_TYPE_)\
  MASS_DEFINE_OPAQUE_TYPE(_NAME_, sizeof(_C_TYPE_) * 8)

#include "generated_types.h"

#endif // C_MACRO_H
