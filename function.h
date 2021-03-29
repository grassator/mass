#ifndef FUNCTION_H
#define FUNCTION_H

#include "prelude.h"
#include "value.h"

Value *
reserve_stack(
  Allocator *allocator,
  Function_Builder *fn,
  Descriptor *descriptor,
  Source_Range source_range
);

void
program_init_startup_code(
  Execution_Context *context
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
  Source_Range source_range,
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

// TODO properly support unsigned numbers
#define maybe_constant_fold(_context_, _loc_, _result_, _a_, _b_, _operator_)\
  do {\
    Execution_Context *fold_context = (_context_);\
    Value *fold_a = (_a_);\
    Value *fold_b = (_b_);\
    if (\
      fold_a->descriptor != &descriptor_lazy_value &&\
      fold_b->descriptor != &descriptor_lazy_value &&\
      fold_a->storage.tag == Storage_Tag_Static &&\
      fold_b->storage.tag == Storage_Tag_Static\
    ) {\
      fold_a = maybe_coerce_number_literal_to_integer(fold_context, fold_a, &descriptor_s64);\
      fold_b = maybe_coerce_number_literal_to_integer(fold_context, fold_b, &descriptor_s64);\
      s64 a_s64 = storage_static_value_up_to_s64(&fold_a->storage);\
      s64 b_s64 = storage_static_value_up_to_s64(&fold_b->storage);\
      s64 constant_result = a_s64 _operator_ b_s64;\
      maybe_constant_fold_internal(fold_context, fold_a, constant_result, (_result_), (_loc_));\
      return 0;\
    }\
  } while(0)

#define MAX_ESTIMATED_TRAMPOLINE_SIZE 32
u32
make_trampoline(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  s64 address
);

void
move_value(
  Allocator *allocator,
  Function_Builder *builder,
  const Source_Range *source_range,
  const Storage *target,
  const Storage *source
);

void
fn_encode(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  const Function_Builder *builder,
  Function_Layout *out_layout
);

Value *
compare(
  Execution_Context *context,
  Compare_Type operation,
  const Source_Range *source_range,
  Value *result_value,
  Value *a,
  Value *b
);

void
load_address(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *result_value,
  const Value *memory
);

Value *
make_and(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
);

Value *
make_or(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *a,
  Value *b
);

#endif










