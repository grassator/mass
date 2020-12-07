#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

Value *
reserve_stack(
  Allocator *allocator,
  Function_Builder *fn,
  Descriptor *descriptor
);

static inline Instruction  *
instruction_add_compiler_location_internal(
  const Compiler_Source_Location compiler_source_location,
  Instruction *instruction
) {
  instruction->compiler_source_location = compiler_source_location;
  return instruction;
}

static inline Instruction *
instruction_add_source_location_internal(
  const Source_Range *source_range,
  Instruction *instruction
) {
  instruction->source_range = source_range;
  return instruction;
}

#define instruction_add_compiler_location(...)\
  instruction_add_compiler_location_internal(COMPILER_SOURCE_LOCATION, __VA_ARGS__)

#define push_instruction(_array_ptr_, _location_, ...)\
  dyn_array_push(\
    *(_array_ptr_),\
    *instruction_add_source_location_internal(\
      (_location_), \
      instruction_add_compiler_location_internal(COMPILER_SOURCE_LOCATION, &(__VA_ARGS__))\
    )\
  )

#define encode_instruction_with_compiler_location(_program_, _buffer_, ...)\
  encode_instruction(\
    (_program_),\
    (_buffer_),\
    instruction_add_compiler_location_internal(COMPILER_SOURCE_LOCATION, (__VA_ARGS__))\
  )

#define MAX_ESTIMATED_TRAMPOLINE_SIZE 32
u32
make_trampoline(
  Program *program,
  Fixed_Buffer *buffer,
  s64 address
);

void
move_value(
  Allocator* allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
);

// FIXME Instead of passing win32 debug structures directly
// `fn_encode` should return a struct with relevant information
#include "win32.h"
void
fn_encode(
  Program *program,
  Fixed_Buffer *buffer,
  Function_Builder *builder,
  RUNTIME_FUNCTION *function_exception_info,
  UNWIND_INFO *unwind_info,
  u32 unwind_data_rva
);

typedef struct Struct_Builder_Field {
  Descriptor_Struct_Field struct_field;
  struct Struct_Builder_Field *next;
} Struct_Builder_Field;

typedef struct {
  u32 offset;
  u32 field_count;
  Struct_Builder_Field *field_list;
} Struct_Builder;


void
plus(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

void
minus(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

void
multiply(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *x,
  Value *y
);

void
divide(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

void
value_remainder(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

void
compare(
  Allocator *allocator,
  Compare_Type operation,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

Value *
value_pointer_to(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *value
);

Value *
call_function_value_array(
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *to_call,
  Array_Value_Ptr arguments
);

Value *
make_and(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
);

Value *
make_or(
  Compilation_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
);

#endif










