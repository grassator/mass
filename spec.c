#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "instructions.c"

typedef enum {
  SIB_Scale_1 = 0b00,
  SIB_Scale_2 = 0b01,
  SIB_Scale_4 = 0b10,
  SIB_Scale_8 = 0b11,
} SIB_Scale;

typedef enum {
  MOD_Displacement_0   = 0b00,
  MOD_Displacement_s8  = 0b01,
  MOD_Displacement_s32 = 0b10,
  MOD_Register         = 0b11,
} MOD;

typedef enum {
  REX   = 0b01000000,
  REX_W = 0b01001000, // 0 = Operand size determined by CS.D; 1 = 64 Bit Operand Size
  REX_R = 0b01000100, // Extension of the ModR/M reg field
  REX_X = 0b01000010, // Extension of the SIB index field
  REX_B = 0b01000001, // Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
} REX_BYTE;

typedef enum {
  Operand_Type_None,
  Operand_Type_Register,
  Operand_Type_Immediate_8,
  Operand_Type_Immediate_32,
  Operand_Type_Immediate_64,
  Operand_Type_Memory_Indirect,
} Operand_Type;

typedef struct {
  u8 index;
} Register;

typedef struct {
  Register reg;
  s32 displacement;
} Operand_Memory_Indirect;

typedef struct {
  Operand_Type type;
  u32 byte_size;
  union {
    Register reg;
    s8 imm8;
    s32 imm32;
    s64 imm64;
    Operand_Memory_Indirect indirect;
  };
} Operand;

typedef enum {
  Descriptor_Type_Integer,
  Descriptor_Type_Function,
} Descriptor_Type;

struct Value;

typedef struct {
  struct Value *argument_list;
  s64 argument_count;

  struct Value *returns;
} Descriptor_Function;

typedef struct {
  Descriptor_Type type;
  union {
    Descriptor_Function function;
  };
} Descriptor;

typedef struct Value {
  Descriptor descriptor;
  Operand operand;
} Value;


#define define_register(reg_name, reg_index, reg_byte_size) \
const Operand reg_name = { \
  .type = Operand_Type_Register, \
  .byte_size = (reg_byte_size), \
  .reg = { .index = (reg_index) }, \
};

define_register(rax, 0, 8); // 0b0000
define_register(rcx, 1, 8);
define_register(rdx, 2, 8);
define_register(rbx, 3, 8);
define_register(rsp, 4, 8);
define_register(rbp, 5, 8);
define_register(rsi, 6, 8);
define_register(rdi, 7, 8);

define_register(eax, 0, 4);
define_register(ecx, 1, 4);
define_register(edx, 2, 4);
define_register(ebx, 3, 4);
define_register(esp, 4, 4);
define_register(ebp, 5, 4);
define_register(esi, 6, 4);
define_register(edi, 7, 4);

define_register(r8,  8, 8);
define_register(r9,  9, 8);
define_register(r10, 10, 8);
define_register(r11, 11, 8);
define_register(r12, 12, 8);
define_register(r13, 13, 8);
define_register(r14, 14, 8);
define_register(r15, 15, 8);

define_register(r8d,  8, 4);
define_register(r9d,  9, 4);
define_register(r10d, 10, 4);
define_register(r11d, 11, 4);
define_register(r12d, 12, 4);
define_register(r13d, 13, 4);
define_register(r14d, 14, 4);
define_register(r15d, 15, 4);
#undef define_register

typedef struct {
  const X64_Mnemonic mnemonic;
  Operand operands[2];
} Instruction;

inline Operand
imm8(
  s8 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_8,
    .byte_size = 1,
    .imm8 = value
  };
}

inline Operand
imm32(
  s32 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_32,
    .byte_size = 4,
    .imm32 = value
  };
}

inline Operand
imm64(
  s64 value
) {
  return (const Operand) {
    .type = Operand_Type_Immediate_64,
    .byte_size = 8,
    .imm64 = value
  };
}

inline Operand
stack(
  s32 offset,
  u32 byte_size
) {
  return (const Operand) {
    .type = Operand_Type_Memory_Indirect,
    .byte_size = byte_size,
    .indirect = (const Operand_Memory_Indirect) {
      .reg = rsp.reg.index,
      .displacement = offset,
    }
  };
}

void
encode(
  Buffer *buffer,
  Instruction instruction
) {
  for (u32 index = 0; index < instruction.mnemonic.encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction.mnemonic.encoding_list[index];
    bool match = true;
    // FIXME remove hardcoded 2 for operand count
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand_Encoding_Type encoding_type = encoding->operand_encoding_types[operand_index];
      Operand_Type operand_type = instruction.operands[operand_index].type;
      if (operand_type == Operand_Type_None && encoding_type == Operand_Encoding_Type_None) {
        continue;
      }
      if (operand_type == Operand_Type_Register && encoding_type == Operand_Encoding_Type_Register) {
        continue;
      }
      if (operand_type == Operand_Type_Register && encoding_type == Operand_Encoding_Type_Op_Code_Plus_Register) {
        continue;
      }
      if (operand_type == Operand_Type_Register && encoding_type == Operand_Encoding_Type_Register_Memory) {
        continue;
      }
      if (operand_type == Operand_Type_Memory_Indirect && encoding_type == Operand_Encoding_Type_Register_Memory) {
        continue;
      }
      if (operand_type == Operand_Type_Immediate_8 && encoding_type == Operand_Encoding_Type_Immediate_8) {
        continue;
      }
      if (operand_type == Operand_Type_Immediate_32 && encoding_type == Operand_Encoding_Type_Immediate_32) {
        continue;
      }
      if (operand_type == Operand_Type_Immediate_64 && encoding_type == Operand_Encoding_Type_Immediate_64) {
        continue;
      }
      match = false;
    }

    if (!match) continue;

    bool needs_mod_r_m = false;
    u8 reg_or_op_code = 0;
    u8 rex_byte = 0;
    u8 r_m = 0;
    u8 mod = MOD_Register;
    u8 op_code = (u8) encoding->op_code;
    bool needs_sib = false;
    u8 sib_byte = 0;
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      Operand_Encoding_Type encoding_type = encoding->operand_encoding_types[operand_index];

      if (operand->byte_size == 8) {
        rex_byte |= REX_W;
      }

      if (operand->type == Operand_Type_Register) {
        if (encoding_type == Operand_Encoding_Type_Register) {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = operand->reg.index;
          if (operand->reg.index & 0b1000) {
            rex_byte |= REX_R;
          }
        } else if (encoding_type == Operand_Encoding_Type_Op_Code_Plus_Register) {
          op_code += operand->reg.index & 0b111;
          if (operand->reg.index & 0b1000) {
            rex_byte |= REX_B;
          }
        }
      }
      if(encoding_type == Operand_Encoding_Type_Register_Memory) {
        needs_mod_r_m = true;
        if (operand->type == Operand_Type_Register) {
          r_m = operand->reg.index;
          if (operand->reg.index & 0b1000) {
            rex_byte |= REX_B;
          }
          mod = MOD_Register;
        } else {
          mod = MOD_Displacement_s32;
          s32 displacement = operand->indirect.displacement;
          if (displacement == 0) {
            mod = MOD_Displacement_0;
          } else if (displacement <= 127 || displacement >= -128) {
            mod = MOD_Displacement_s8;
          }
          assert(operand->type == Operand_Type_Memory_Indirect);
          r_m = operand->indirect.reg.index;
          if (r_m == rsp.reg.index) {
            needs_sib = true;
            // FIXME support proper SIB for non-rsp registers
            sib_byte = (
              (SIB_Scale_1 << 6) |
              (r_m << 3) |
              (r_m)
            );
          }
        }
      }
    }

    if (encoding->extension_type == Instruction_Extension_Type_Op_Code) {
      reg_or_op_code = encoding->op_code_extension;
    }

    if (rex_byte) {
      buffer_append_u8(buffer, rex_byte);
    }

    // FIXME if op code is 2 bytes need different append
    buffer_append_u8(buffer, op_code);

    // FIXME Implement proper mod support
    // FIXME mask register index
    if (needs_mod_r_m) {
      u8 mod_r_m = (
        (mod << 6) |
        (reg_or_op_code << 3) |
        (r_m)
      );
      buffer_append_u8(buffer, mod_r_m);
    }

    if (needs_sib) {
      buffer_append_u8(buffer, sib_byte);
    }

    // Write out displacement
    if (needs_mod_r_m && mod != MOD_Register) {
      for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
        Operand *operand = &instruction.operands[operand_index];
        if (operand->type == Operand_Type_Memory_Indirect) {
          if (mod == MOD_Displacement_s32) {
            buffer_append_s32(buffer, operand->indirect.displacement);
          } else if (mod == MOD_Displacement_s8) {
            buffer_append_s8(buffer, (s8)operand->indirect.displacement);
          } else {
            assert(mod == MOD_Displacement_0);
          }
        }
      }
    }
    // Write out immediate operand(s?)
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      if (operand->type == Operand_Type_Immediate_8) {
        buffer_append_s8(buffer, operand->imm8);
      }
      if (operand->type == Operand_Type_Immediate_32) {
        buffer_append_s32(buffer, operand->imm32);
      }
      if (operand->type == Operand_Type_Immediate_64) {
        buffer_append_s64(buffer, operand->imm64);
      }
    }
    return;
  }
  // Didn't find any encoding
  assert(!"Did not find acceptable encoding");
}

typedef struct {
  // TODO make it s32
  u8 stack_reserve;
  u8 next_argument_index;
  Buffer buffer;

  Descriptor_Function descriptor;
} Function_Builder;

Operand
declare_variable(
  Function_Builder *fn,
  u32 byte_size
) {
  Operand result = stack(fn->stack_reserve, byte_size);
  fn->stack_reserve += 0x10;
  return result;
}

void
assign(
  Function_Builder *fn,
  Operand a,
  Operand b
) {
  encode(&fn->buffer, (Instruction) {mov, {a, b}});
}

Operand
mutating_plus(
  Function_Builder *fn,
  Operand a,
  Operand b
) {
  // TODO Create a temp value for result
  encode(&fn->buffer, (Instruction) {add, {a, b}});
  return a;
}

Function_Builder
fn_begin() {
  Function_Builder fn = {
    .stack_reserve = 0x0,
    .buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE),
    .descriptor = (const Descriptor_Function) {0}
  };

  // @Volatile @ArgumentCount
  fn.descriptor.argument_list = malloc(sizeof(Value) * 4);
  fn.descriptor.returns = malloc(sizeof(Value));

  // @Volatile @ReserveStack
  encode(&fn.buffer, (Instruction) {sub, {rsp, imm8(0xcc)}});
  return fn;
}

Value
fn_end(
  Function_Builder *builder
) {
  builder->descriptor.argument_count = builder->next_argument_index;
  return (const Value) {
    .descriptor = {
      .type = Descriptor_Type_Function,
      .function = builder->descriptor,
    },
    .operand = imm64((s64) builder->buffer.memory)
  };
}

Value
fn_arg(
  Function_Builder *fn,
  Descriptor descriptor
) {
  //assert(byte_size == 8);
  switch (fn->next_argument_index++) {
    case 0: {
      Value arg = {
        .descriptor = descriptor,
        .operand = rcx,
      };
      fn->descriptor.argument_list[0] = arg;
      return arg;
    }
    case 1: {
      Value arg = {
        .descriptor = descriptor,
        .operand = rdx,
      };
      fn->descriptor.argument_list[1] = arg;
      return arg;
    }
    case 2: {
      Value arg = {
        .descriptor = descriptor,
        .operand = r8,
      };
      fn->descriptor.argument_list[2] = arg;
      return arg;
    }
    case 3: {
      Value arg = {
        .descriptor = descriptor,
        .operand = r9,
      };
      fn->descriptor.argument_list[3] = arg;
      return arg;
    }
  }
  // @Volatile @ArgumentCount
  assert(!"More than 4 arguments are not supported at the moment.");
  return (const Value){0};
}

void
fn_return(
  Function_Builder *fn,
  Value to_return
) {
  u8 alignment = 0x8;
  u8 stack_size = fn->stack_reserve + alignment;

  { // Override stack reservation
    u64 save_occupied = fn->buffer.occupied;
    fn->buffer.occupied = 0;

    // @Volatile @ReserveStack
    encode(&fn->buffer, (Instruction) {sub, {rsp, imm8(stack_size)}});
    fn->buffer.occupied = save_occupied;
  }

  *fn->descriptor.returns = to_return;

  if (memcmp(&rax, &to_return, sizeof(rax)) != 0) {
    encode(&fn->buffer, (Instruction) {mov, {rax, to_return.operand}});
  }
  encode(&fn->buffer, (Instruction) {add, {rsp, imm8(stack_size)}});
  encode(&fn->buffer, (Instruction) {ret, {0}});
}

Value
make_constant_s32(
  s32 value
) {
  Function_Builder builder = fn_begin();
  {
    fn_return(&builder, (const Value){
      .descriptor = { .type = Descriptor_Type_Integer },
      .operand = imm32(value),
    });
  }
  return fn_end(&builder);
}

Value
make_identity_s64() {
  Function_Builder builder = fn_begin();
  {
    Value arg0 = fn_arg(&builder, (const Descriptor){.type = Descriptor_Type_Integer});
    fn_return(&builder, arg0);
  }
  return fn_end(&builder);
}

Value
make_increment_s64() {
  Function_Builder builder = fn_begin();
  {
    Operand x = declare_variable(&builder, sizeof(s64));
    assign(&builder, x, imm32(1));
    Operand y = declare_variable(&builder, sizeof(s64));
    assign(&builder, y, imm32(2));
    Value arg0 = fn_arg(&builder, (const Descriptor){.type = Descriptor_Type_Integer});
    mutating_plus(&builder, arg0.operand, x);
    mutating_plus(&builder, arg0.operand, y);
    fn_return(&builder, arg0);
  }
  return fn_end(&builder);
}

Value
call_function_value(
  Function_Builder *builder,
  Value *to_call,
  Value *argument_list,
  s64 argument_count
) {
  assert(to_call->descriptor.type == Descriptor_Type_Function);
  Descriptor_Function *descriptor = &to_call->descriptor.function;
  assert(descriptor->argument_count == argument_count);

  for (s64 i = 0; i < argument_count; ++i) {
    // FIXME add proper type checks for arguments
    assert(descriptor->argument_list[i].descriptor.type == argument_list[i].descriptor.type);
    encode(
      &builder->buffer,
      (Instruction) {mov, {descriptor->argument_list[i].operand, argument_list[i].operand}}
     );
  }

  encode(&builder->buffer, (Instruction) {mov, {rax, to_call->operand}});
  encode(&builder->buffer, (Instruction) {call, {rax, 0}});

  return *descriptor->returns;
}

Value
make_call_no_arg_return_s32() {
  Function_Builder builder = fn_begin();
  {
    Value *returns = malloc(sizeof(Value));
    *returns = (const Value){0};
    returns->descriptor = (const Descriptor){.type = Descriptor_Type_Integer};
    returns->operand = rax;

    Value arg0 = fn_arg(&builder, (const Descriptor){
      .type = Descriptor_Type_Function,
      .function = {
        .argument_list = 0,
        .argument_count = 0,
        .returns = returns,
      }
    });

    Value result = call_function_value(&builder, &arg0, 0, 0);
    fn_return(&builder, result);
  }
  return fn_end(&builder);
}

Value
make_partial_application_s64(
  Value *original_fn,
  s64 arg
) {
  Function_Builder builder = fn_begin();
  {
    Value applied_arg0 = {
      .descriptor = (const Descriptor){.type = Descriptor_Type_Integer},
      .operand = imm64(arg),
    };
    Value result = call_function_value(&builder, original_fn, &applied_arg0, 1);
    fn_return(&builder, result);
  }
  return fn_end(&builder);
}

typedef struct {
  u8 *location;
  u64 ip;
} Patch;

Patch
make_jnz(
  Function_Builder *fn
) {
  encode(&fn->buffer, (Instruction) {jnz, {imm8(0xcc), 0}});
  u64 ip = fn->buffer.occupied;
  u8 *location = fn->buffer.memory + fn->buffer.occupied - 1;
  return (const Patch) { .location = location, .ip = ip };
}

void
patch_jump_to_here(
  Function_Builder *fn,
  Patch patch
) {
  u64 diff = fn->buffer.occupied - patch.ip;
  assert(diff < 0x80);
  *patch.location = (u8) diff;
}

Value
make_is_non_zero() {
  Function_Builder builder = fn_begin();
  {
    encode(&builder.buffer, (Instruction) {cmp, {ecx, imm32(0)}});
    Patch patch = make_jnz(&builder);

    fn_return(&builder, (const Value){
      .descriptor = { .type = Descriptor_Type_Integer },
      .operand = imm64(0),
    });
    patch_jump_to_here(&builder, patch);

    fn_return(&builder, (const Value){
      .descriptor = { .type = Descriptor_Type_Integer },
      .operand = imm64(1),
    });
  }
  return fn_end(&builder);
}

s64
helper_value_as_function(
  Value *value
) {
  assert(value->operand.type == Operand_Type_Immediate_64);
  return value->operand.imm64;
}

#define value_as_function(_value_, _type_) \
  ((_type_)helper_value_as_function(_value_))

spec("mass") {
  it("should create function that will return 42") {
    Value the_answer = make_constant_s32(42);
    s32 result = value_as_function(&the_answer, fn_type_void_to_s32)();
    check(result == 42);
  }

  it("should create function that returns s64 value that was passed") {
    Value id_value = make_identity_s64();
    fn_type_s64_to_s64 id_s64 = (fn_type_s64_to_s64)id_value.operand.imm64;
    s64 result = id_s64(42);
    check(result == 42);
  }

  it("should create function increments s64 value passed to it") {
    Value add_3_s64 = make_increment_s64();
    s64 result = value_as_function(&add_3_s64, fn_type_s64_to_s64)(42);
    check(result == 45);
  }

  it("should create a function to call a no argument fn") {
    Value the_answer = make_constant_s32(42);
    Value caller= make_call_no_arg_return_s32();
    s32 result = value_as_function(&caller, fn_type__void_to_s32__to_s32)(
      value_as_function(&the_answer, fn_type_void_to_s32)
    );
    check(result == 42);
  }

  it("should create a partially applied function") {
    Value id_value = make_identity_s64();
    Value partial_fn_value = make_partial_application_s64(&id_value, 42);
    fn_type_void_to_s64 the_answer = (fn_type_void_to_s64) partial_fn_value.operand.imm64;
    s64 result = the_answer();
    check(result == 42);
  }

  it("should have a function that returns 0 if arg is zero, 1 otherwise") {
    Value is_non_zero_value = make_is_non_zero();
    fn_type_s32_to_s32 is_non_zero = value_as_function(&is_non_zero_value, fn_type_s32_to_s32);
    s64 result = is_non_zero(0);
    check(result == 0);
    result = is_non_zero(42);
    check(result == 1);
  }

  it("should return 3rd argument") {
    Function_Builder builder = fn_begin();
    {
      (void)fn_arg(&builder, (const Descriptor){.type = Descriptor_Type_Integer});
      (void)fn_arg(&builder, (const Descriptor){.type = Descriptor_Type_Integer});
      Value arg2 = fn_arg(&builder, (const Descriptor){.type = Descriptor_Type_Integer});
      fn_return(&builder, arg2);
    }
    fn_type_s64_s64_s64_to_s64 third = (fn_type_s64_s64_s64_to_s64)fn_end(&builder).operand.imm64;

    s64 result = third(1, 2, 3);
    check(result == 3);
  }
}
