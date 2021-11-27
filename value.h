#ifndef VALUE_H
#define VALUE_H
#include "prelude.h"
#include <inttypes.h>
#include "types.h"

static inline void
mass_result_set_error(
  Mass_Result *result,
  Mass_Error error
) {
  assert(result->tag != Mass_Result_Tag_Error);
  *result = (Mass_Result){ .tag = Mass_Result_Tag_Error, .Error.error = error };
}

// Having this macro allows to do `return mass_error(..)`
// for any function that would accept literal 0 return.
#define mass_error(_CONTEXT_, ...) (mass_result_set_error((_CONTEXT_)->result, __VA_ARGS__), 0)

static inline bool
mass_result_is_error(
  Mass_Result *result
) {
  return result->tag != Mass_Result_Tag_Success;
}
#define mass_has_error(_CONTEXT_) mass_result_is_error((_CONTEXT_)->result)

static inline Function_Parameter
function_parameter_with_default(
  const Symbol *symbol,
  Value *default_value,
  const Descriptor *descriptor
) {
   return (Function_Parameter){
    .tag = Function_Parameter_Tag_Runtime,
    .symbol = symbol,
    .descriptor = descriptor,
    .source_range = {0}, // FIXME provide this
    .maybe_default_value = default_value,
  };
}

static  inline Function_Parameter
function_parameter(
  const Symbol *symbol,
  const Descriptor *descriptor
) {
  return function_parameter_with_default(symbol, 0, descriptor);
}

typedef struct {
  u64 occupied;
} Temp_Mark;

static inline Temp_Mark
compilation_temp_mark(
  Compilation *compilation
) {
  return (Temp_Mark){compilation->temp_buffer.occupied};
}

static inline void
compilation_temp_reset_to_mark(
  Compilation *compilation,
  Temp_Mark mark
) {
  compilation->temp_buffer.occupied = mark.occupied;
}

static inline void
compilation_temp_reset(
  Compilation *compilation
) {
  compilation->temp_buffer.occupied = 0;
}

static inline Temp_Mark
context_temp_mark(
  Execution_Context *context
) {
  return compilation_temp_mark(context->compilation);
}

static inline void
context_temp_reset_to_mark(
  Execution_Context *context,
  Temp_Mark mark
) {
  compilation_temp_reset_to_mark(context->compilation, mark);
}

static inline void
context_temp_reset(
  Execution_Context *context
) {
  compilation_temp_reset(context->compilation);
}

static inline Value_View
value_view_single(
  Value **value
) {
  return (Value_View) {
    .values = value,
    .length = 1,
    .source_range = (*value)->source_range,
  };
}

static inline Value_View
value_view_make_single(
  const Allocator *allocator,
  Value *value
) {
  Value **values = allocator_allocate(allocator, Value *);
  values[0] = value;
  return (Value_View) {
    .values = values,
    .length = 1,
    .source_range = value->source_range,
  };
}

static inline Value *
value_view_peek(
  Value_View view,
  u64 index
) {
  return index < view.length ? view.values[index] : 0;
}

static inline Value *
value_view_get(
  Value_View view,
  u64 index
) {
  assert(index < view.length);
  return view.values[index];
}

static inline Value *
value_view_next(
  Value_View view,
  u32 *peek_index
) {
  Value *peek = value_view_peek(view, *peek_index);
  if (peek) *peek_index += 1;
  return peek;
}

static inline Value *
value_view_last(
  Value_View view
) {
  assert(view.length);
  return value_view_get(view, view.length - 1);
}

static Value_View
value_view_slice(
  const Value_View *view,
  u32 start_index,
  u32 end_index
) {
  assert(end_index <= view->length);
  assert(start_index <= end_index);

  Source_Range source_range = view->source_range;
  source_range.offsets.to = end_index == view->length
    ? view->source_range.offsets.to
    : view->values[end_index]->source_range.offsets.from;
  source_range.offsets.from = start_index == end_index
    ? source_range.offsets.to
    : view->values[start_index]->source_range.offsets.from;

  assert(source_range.offsets.from <= source_range.offsets.to);

  return (Value_View) {
    .values = view->values + start_index,
    .length = end_index - start_index,
    .source_range = source_range,
  };
}

static inline Value_View
value_view_rest(
  const Value_View *view,
  u32 index
) {
  return value_view_slice(view, index, view->length);
}

static inline Value_View
value_view_from_value_array(
  Array_Value_Ptr value_array,
  const Source_Range *source_range
) {
  return (Value_View) {
    .values = dyn_array_raw(value_array),
    .length = u64_to_u32(dyn_array_length(value_array)),
    .source_range = *source_range
  };
}

#define dyn_array_copy_from_raw_memory(_TYPE_, _ALLOCATOR_, _TARGET_, _SOURCE_PTR_, _SOURCE_COUNT_)\
  do {\
    u64 copy_count = (_SOURCE_COUNT_);\
    if (copy_count) {\
      _TYPE_ copy_target = dyn_array_make(_TYPE_, .allocator = (_ALLOCATOR_), .capacity = copy_count);\
      copy_target.data->length = copy_count;\
      memcpy(dyn_array_raw(copy_target), (_SOURCE_PTR_), sizeof((_SOURCE_PTR_)[0]) * copy_count);\
      *(_TARGET_) = copy_target;\
    } else {\
      *(_TARGET_) = dyn_array_static_empty(_TYPE_);\
    }\
  } while(0)

#define dyn_array_copy_from_temp(_TYPE_, _CONTEXT_, _TARGET_, _SOURCE_)\
  do {\
    _TYPE_ copy_source = (_SOURCE_);\
    dyn_array_copy_from_raw_memory(\
      _TYPE_, (_CONTEXT_)->allocator, (_TARGET_), dyn_array_raw(copy_source), dyn_array_length(copy_source)\
    );\
  } while(0)

static inline Array_Value_Ptr
value_view_to_value_array(
  const Allocator *allocator,
  Value_View view
) {
  Array_Value_Ptr result;
  dyn_array_copy_from_raw_memory(Array_Value_Ptr, allocator, &result, view.values, view.length);
  return result;
}

static inline bool
storage_is_stack(
  const Storage *operand
);

static inline Storage
storage_stack(
  s32 offset,
  Bits bit_size,
  Stack_Area area
);

static inline Storage
storage_register(
  Register reg,
  Bits bit_size
);

static inline bool
register_is_xmm(
  Register reg
) {
  return !!(reg & Register_Xmm0);
}

static inline bool
storage_equal(
  const Storage *a,
  const Storage *b
);

#define storage_none ((Storage){.tag = Storage_Tag_None })

static Fixed_Buffer *
mass_error_to_string(
  Compilation *compilation,
  Mass_Error const* error
);

Value void_value = {
  .descriptor = &descriptor_void,
  .storage = { .tag = Storage_Tag_None },
};

static inline bool
descriptor_is_integer(
  const Descriptor *descriptor
) {
  if (!descriptor) return false;
  if (descriptor->tag != Descriptor_Tag_Integer) {
    return false;
  }
  return true;
}

static inline bool
descriptor_is_unsigned_integer(
  const Descriptor *descriptor
) {
  return descriptor_is_integer(descriptor) && !descriptor->Integer.is_signed;
}

static inline bool
descriptor_is_signed_integer(
  const Descriptor *descriptor
) {
  return descriptor_is_integer(descriptor) && descriptor->Integer.is_signed;
}

static inline bool
descriptor_is_float(
  const Descriptor *descriptor
) {
  if (!descriptor) return false;
  if (descriptor->tag != Descriptor_Tag_Float) {
    return false;
  }
  return true;
}

static bool
descriptor_is_implicit_pointer(
  const Descriptor *descriptor
) {
  return descriptor && descriptor->tag == Descriptor_Tag_Pointer_To && descriptor->Pointer_To.is_implicit;
}

static inline const Descriptor *
maybe_unwrap_pointer_descriptor(
  const Descriptor *descriptor
) {
  if (descriptor->tag == Descriptor_Tag_Pointer_To) {
    return descriptor->Pointer_To.descriptor;
  }
  return descriptor;
}

static inline u64
descriptor_byte_size(
  const Descriptor *descriptor
);

static inline Descriptor *
descriptor_array_of(
  const Allocator *allocator,
  const Descriptor *item_descriptor,
  u64 length
);

static inline bool
same_value_type(
  const Value *a,
  const Value *b
);

static void
source_range_print_start_position(
  Compilation *compilation,
  const Source_Range *source_range
);

static Slice
source_from_source_range(
  Compilation *compilation,
  const Source_Range *source_range
);

static void *
rip_value_pointer(
  Value *value
);

static inline bool
storage_is_label(
  const Storage *operand
);

static inline bool
storage_is_register_index(
  const Storage *storage,
  Register reg_index
);

static fn_type_opaque
value_as_function(
  Program *program,
  Value *value
);

static inline void *
rip_value_pointer_from_label(
  const Label *label
);

static void
print_storage(
  const Storage *operand
);


#endif
