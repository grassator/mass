#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

Value *
reserve_stack(
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
  const Source_Location *source_location,
  Instruction *instruction
) {
  instruction->source_location = source_location;
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
  const Program *program,
  Fixed_Buffer *buffer,
  s64 address
);

void
move_value(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
);

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

typedef struct {
  bool done;
  Label *label_start;
  Label *label_end;
} Loop_Builder;

Loop_Builder
loop_start(
  Array_Instruction *instructions,
  const Source_Location *location
);

void
loop_end(
  Array_Instruction *instructions,
  const Source_Location *location,
  Loop_Builder *loop
);

void
plus(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
);

void
minus(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
);

void
multiply(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *x,
  Value *y
);

void
divide(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
);

void
value_remainder(
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
);

void
compare(
  Compare_Type operation,
  Function_Builder *builder,
  const Source_Location *location,
  Value *result_value,
  Value *a,
  Value *b
);

Value *
value_pointer_to(
  Function_Builder *builder,
  const Source_Location *location,
  Value *value
);

Value *
call_function_value_array(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  Array_Value_Ptr arguments
);

Value *
call_function_value(
  Function_Builder *builder,
  const Source_Location *location,
  Value *to_call,
  ...
);

Value *
make_and(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
);

Value *
make_or(
  Function_Builder *builder,
  const Source_Location *location,
  Value *a,
  Value *b
);

#endif










