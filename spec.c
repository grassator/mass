#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"
#include "encoding.c"

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
  union {
    Register reg;
    s8 imm8;
    s32 imm32;
    s64 imm64;
    Operand_Memory_Indirect indirect;
  };
} Operand;

#define define_register(reg_name, reg_index) \
const Operand reg_name = { .type = Operand_Type_Register, .reg = { .index = (reg_index) } };

define_register(rax, 0);
define_register(rcx, 1);
define_register(rdx, 2);
define_register(rbx, 3);
define_register(rsp, 4);
define_register(rbp, 5);
define_register(rsi, 6);
define_register(rdi, 7);

define_register(r8,  8);
define_register(r9,  9);
define_register(r10, 10);
define_register(r11, 11);
define_register(r12, 12);
define_register(r13, 13);
define_register(r14, 14);
define_register(r15, 15);
#undef define_register

typedef struct {
  const X64_Mnemonic mnemonic;
  Operand operands[2];
} Instruction;

inline Operand
imm8(
  s8 value
) {
  return (const Operand) { .type = Operand_Type_Immediate_8, .imm8 = value };
}

inline Operand
imm32(
  s32 value
) {
  return (const Operand) { .type = Operand_Type_Immediate_32, .imm32 = value };
}

inline Operand
imm64(
  s64 value
) {
  return (const Operand) { .type = Operand_Type_Immediate_64, .imm64 = value };
}

inline Operand
stack(
  s32 offset
) {
  return (const Operand) {
    .type = Operand_Type_Memory_Indirect,
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

      if (operand->type == Operand_Type_Register) {
        // FIXME add REX.W prefix only if 64 bit
        rex_byte |= REX_W;
        if (encoding_type == Operand_Encoding_Type_Register) {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = operand->reg.index;
        } else if (encoding_type == Operand_Encoding_Type_Op_Code_Plus_Register) {
          // TODO use REX bit for extended register like r9
          op_code += operand->reg.index & 0b111;
        }
      }
      if(encoding_type == Operand_Encoding_Type_Register_Memory) {
        needs_mod_r_m = true;
        if (operand->type == Operand_Type_Register) {
          r_m = operand->reg.index;
          mod = MOD_Register;
        } else {
          mod = MOD_Displacement_s32;
          assert(operand->type == Operand_Type_Memory_Indirect);
		  // FIXME add REX.W prefix only if 64 bit
		  rex_byte |= REX_W;
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
      buffer_append_u8(buffer, REX_W);
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
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      if (operand->type == Operand_Type_Memory_Indirect) {
        buffer_append_s32(buffer, operand->indirect.displacement);
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

fn_type_void_to_s32
make_constant_s32(
  s32 value
) {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  encode(&buffer, (Instruction) {mov, {rax, imm32(value)}});
  encode(&buffer, (Instruction) {ret, {0}});
  return (fn_type_void_to_s32)buffer.memory;
}

fn_type_s64_to_s64
make_identity_s64() {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  encode(&buffer, (Instruction) {mov, {rax, rcx}});
  encode(&buffer, (Instruction) {ret, {0}});
  return (fn_type_s64_to_s64)buffer.memory;
}

// 16 byte alignment
// 0x0000_0000_1000_2340
// push ...
// jmp our_function_address
// 0x0000_0000_1000_2348
// sub rsp, 8
// 0x0000_0000_1000_2350
// sub rsp, 16

typedef struct {
  // TODO args
  // TODO make it s32
  u8 stack_reserve;
  Buffer buffer;
} Fn;

Operand
declare_variable(
  Fn *fn
) {
  Operand result = stack(fn->stack_reserve);
  fn->stack_reserve += 0x10;
  return result;
}

void
assign(
  Fn *fn,
  Operand a,
  Operand b
) {
  encode(&fn->buffer, (Instruction) {mov, {a, b}});
}

Operand
mutating_plus(
  Fn *fn,
  Operand a,
  Operand b
) {
  // TODO Create a temp value for result
  encode(&fn->buffer, (Instruction) {add, {a, b}});
  return a;
}

Fn
fn_begin() {
  Fn fn = {
    .stack_reserve = 0x0,
    .buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE),
  };
  // @Volatile @ReserveStack
  encode(&fn.buffer, (Instruction) {sub, {rsp, imm8(0xcc)}});
  return fn;
}

void
fn_return(
  Fn *fn,
  Operand to_return
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


  if (memcmp(&rax, &to_return, sizeof(rax)) != 0) {
    encode(&fn->buffer, (Instruction) {mov, {rax, to_return}});
  }
  encode(&fn->buffer, (Instruction) {add, {rsp, imm8(stack_size)}});
  encode(&fn->buffer, (Instruction) {ret, {0}});

}

fn_type_s64_to_s64
make_increment_s64() {
  Fn fn = fn_begin();
  Operand x = declare_variable(&fn);
  assign(&fn, x, imm32(1));
  Operand y = declare_variable(&fn);
  assign(&fn, y, imm32(2));
  mutating_plus(&fn, rcx, x);
  mutating_plus(&fn, rcx, y);
  fn_return(&fn, rcx);
  return (fn_type_s64_to_s64)fn.buffer.memory;
}

fn_type__void_to_s32__to_s32
make_proxy_no_arg_return_s32() {
  Fn fn = fn_begin();
  encode(&fn.buffer, (Instruction) {call, {rcx, 0}});

  fn_return(&fn, rax);
  return (fn_type__void_to_s32__to_s32)fn.buffer.memory;
}

fn_type_void_to_s64
make_partial_application_s64(
  fn_type_s64_to_s64 original_fn,
  s64 arg
) {
  Fn fn = fn_begin();

  encode(&fn.buffer, (Instruction) {mov, {rcx, imm64(arg)}});

  encode(&fn.buffer, (Instruction) {mov, {rax, imm64((s64) original_fn)}});
  encode(&fn.buffer, (Instruction) {call, {rax, 0}});

  fn_return(&fn, rax);
  return (fn_type_void_to_s64)fn.buffer.memory;
}

spec("mass") {
  it("should create function that will return 42") {
    fn_type_void_to_s32 the_answer = make_constant_s32(42);
    s32 result = the_answer();
    check(result == 42);
    check(the_answer() == 42);
  }
  it("should create function that will return 21") {
    fn_type_void_to_s32 not_the_answer = make_constant_s32(21);
    s32 result = not_the_answer();
    check(result == 21);
  }

  it("should create function that returns s64 value that was passed") {
    fn_type_s64_to_s64 id_s64 = make_identity_s64();
    s64 result = id_s64(42);
    check(result == 42);
  }

  it("should create function increments s64 value passed to it") {
    fn_type_s64_to_s64 add_3_s64 = make_increment_s64();
    s64 result = add_3_s64(42);
    check(result == 45);
  }

  it("should create a function to call a no argument fn") {
    fn_type_void_to_s32 the_answer = make_constant_s32(42);
    fn_type__void_to_s32__to_s32 proxy = make_proxy_no_arg_return_s32();
    s32 result = proxy(the_answer);
    check(result == 42);
  }

  it("should create a partially applied function") {
    fn_type_s64_to_s64 id_s64 = make_identity_s64();
    fn_type_void_to_s64 the_answer = make_partial_application_s64(id_s64, 42);
    s64 result = the_answer();
    check(result == 42);
  }
}
