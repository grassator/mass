#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

Value *
reserve_stack(
  Function_Builder *fn,
  Descriptor *descriptor
);

inline void
push_instruction_internal(
  const char *filename,
  u32 line_number,
  Array_Instruction *instructions,
  Instruction instruction
);

#define push_instruction(_array_ptr_, ...)\
  push_instruction_internal(__FILE__, __LINE__, _array_ptr_, __VA_ARGS__)

void
move_value(
  Array_Instruction *instructions,
  Value *a,
  Value *b
);

Function_Builder *
fn_begin(
  Value **result,
  Program *program
);

void
fn_end(
  Function_Builder *builder
);

void
fn_encode(
  Fixed_Buffer *buffer,
  Function_Builder *builder,
  RUNTIME_FUNCTION *function_exception_info,
  UNWIND_INFO *unwind_info,
  u32 unwind_data_rva
);

Value *
fn_arg(
  Function_Builder *builder,
  Descriptor *descriptor
);

typedef enum {
  Function_Return_Type_Implicit,
  Function_Return_Type_Explicit,
} Function_Return_Type;

void
fn_return(
  Function_Builder *builder,
  Value *to_return,
  Function_Return_Type return_type
);

Label *make_if(
  Array_Instruction *instructions,
  Value *value
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
  Array_Instruction *instructions
);

void
loop_end(
  Array_Instruction *instructions,
  Loop_Builder *loop
);

void
plus(
  Array_Instruction *instructions,
  Value *result_value,
  Value *a,
  Value *b
);

void
minus(
  Array_Instruction *instructions,
  Value *result_value,
  Value *a,
  Value *b
);

Value *
multiply(
  Function_Builder *builder,
  Value *x,
  Value *y
);

Value *
divide(
  Function_Builder *builder,
  Value *a,
  Value *b
);

Value *
remainder(
  Function_Builder *builder,
  Value *a,
  Value *b
);

typedef enum {
  Compare_Equal = 1,
  Compare_Not_Equal,
  Compare_Less,
  Compare_Greater,
} Compare;

Value *
compare(
  Function_Builder *builder,
  Compare operation,
  Value *a,
  Value *b
);

Value *
value_pointer_to(
  Function_Builder *builder,
  Value *value
);

Value *
call_function_value_array(
  Function_Builder *builder,
  Value *to_call,
  Array_Value_Ptr arguments
);

Value *
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  ...
);

Value *
make_and(
  Function_Builder *builder,
  Value *a,
  Value *b
);

Value *
make_or(
  Function_Builder *builder,
  Value *a,
  Value *b
);

#endif










