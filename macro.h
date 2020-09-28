#ifndef MACRO_H
#define MACRO_H

#define Plus(_a_, _b_) plus(builder_, _a_, _b_)
#define Minus(_a_, _b_) minus(builder_, _a_, _b_)
#define Multiply(_a_, _b_) multiply(builder_, _a_, _b_)
#define Divide(_a_, _b_) divide(builder_, _a_, _b_)
#define Remainder(_a_, _b_) remainder(builder_, _a_, _b_)

#define SizeOfDescriptor(_descriptor_) value_from_s32(descriptor_byte_size(_descriptor_))
#define SizeOf(_value_) value_byte_size(_value_)

#define ReflectDescriptor (_descriptor_) fn_reflect(builder_, _descriptor_)

#define IfBuilder(_builder_, _value_) \
  for (\
    Label *label__ = make_if(_builder_, _value_), *dummy__ = 0; \
    label__ && !(dummy__++); \
    push_instruction(_builder_, (Instruction) {.maybe_label = label__})\
  )
#define If(_value_) IfBuilder(builder_, _value_)

#define Match\
  for (\
    Label *match_end_label__ = make_label(), *dummy__ = 0; \
    !(dummy__++); \
    push_instruction(builder_, (Instruction) {.maybe_label = match_end_label__})\
  )
#define Case(_value_)\
  for (\
    Label *label__ = make_if(builder_, _value_), *dummy__ = 0; \
    !(dummy__++); \
    push_instruction(builder_, (Instruction) {jmp, {label32(match_end_label__), 0, 0}}),\
    push_instruction(builder_, (Instruction) {.maybe_label = label__})\
  )
#define CaseAny

#define Loop \
  for ( \
    Loop_Builder loop_builder_ = loop_start(builder_); \
    !loop_builder_.done; \
    loop_end(builder_, &loop_builder_) \
  )

#define Continue \
  push_instruction(builder_, (Instruction) {jmp, {label32(loop_builder_.label_start), 0, 0}})
#define Break \
  push_instruction(builder_, (Instruction) {jmp, {label32(loop_builder_.label_end), 0, 0}})

#define NotEq(_a_, _b_) compare(builder_, Compare_Not_Equal, (_a_), (_b_))
#define Eq(_a_, _b_) compare(builder_, Compare_Equal, (_a_), (_b_))
#define Less(_a_, _b_) compare(builder_, Compare_Less, (_a_), (_b_))
#define Greater(_a_, _b_) compare(builder_, Compare_Greater, (_a_), (_b_))

#define Function(_id_) \
  Value *_id_ = 0; \
  for (\
    Function_Builder *builder_ = fn_begin(&_id_, program_);\
    !fn_is_frozen(builder_);\
    fn_end(builder_)\
  )

#define Return(_value_) \
  fn_return(builder_, _value_, Function_Return_Type_Explicit)

#define Arg(_id_, _descriptor_) \
  Value *_id_ = fn_arg(builder_, (_descriptor_))

#define Arg_s8(_id_) Arg((_id_), &descriptor_s8)
#define Arg_s32(_id_) Arg((_id_), &descriptor_s32)
#define Arg_s64(_id_) Arg((_id_), &descriptor_s64)

#define Stack(_id_, _descriptor_, _value_) \
  Value *_id_ = reserve_stack(builder_, (_descriptor_)); \
  move_value(builder_, _id_, (_value_))

#define Stack_s32(_id_, _value_) Stack((_id_), &descriptor_s32, _value_)
#define Stack_s64(_id_, _value_) Stack((_id_), &descriptor_s64, _value_)

// FIXME use null-terminated list
#define Call(...)\
  call_function_value(\
    builder_,\
    __VA_ARGS__,\
    0\
  )

#define And(_a_, _b_) make_and(builder_, (_a_), (_b_))
#define Or(_a_, _b_) make_or(builder_, (_a_), (_b_))

#endif










