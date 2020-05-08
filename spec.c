#include "bdd-for-c.h"
#include "windows.h"
#include <stdio.h>

#include "prelude.c"

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
} Operand_Type;

typedef struct {
  u8 index;
} Register;

typedef struct {
  Operand_Type type;
  union {
    Register reg;
    s8 imm8;
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

typedef enum {
  Instruction_Extension_Type_Register = 1,
  Instruction_Extension_Type_Op_Code,
  Instruction_Extension_Type_Plus_Register,
} Instruction_Extension_Type;

typedef enum {
  Operand_Encoding_Type_None,
  Operand_Encoding_Type_Register,
  Operand_Encoding_Type_Register_Memory,
  Operand_Encoding_Type_Immediate_8,
} Operand_Encoding_Type;

typedef struct {
  u16 op_code;
  Instruction_Extension_Type extension_type;
  u8 op_code_extension;
  Operand_Encoding_Type operand_encoding_types[2];
} Instruction_Encoding;


typedef struct {
  const Instruction_Encoding *encoding_list;
  u32 encoding_count;
} X64_Mnemonic;

typedef struct {
  const X64_Mnemonic mnemonic;
  Operand operands[2];
} Instruction;

// mov
const Instruction_Encoding mov_encoding_list[] = {
  {
    .op_code = 0x89,
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Register
    },
  },
};

const X64_Mnemonic mov = {
  .encoding_list = (const Instruction_Encoding *)mov_encoding_list,
  .encoding_count = static_array_size(mov_encoding_list),
};

// ret
const Instruction_Encoding ret_encoding_list[] = {
  {
    .op_code = 0xc3,
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic ret = {
  .encoding_list = (const Instruction_Encoding *)ret_encoding_list,
  .encoding_count = static_array_size(ret_encoding_list),
};

// add
const Instruction_Encoding add_encoding_list[] = {
  {
    .op_code = 0x83,
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 0,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_8
    },
  },
};
const X64_Mnemonic add = {
  .encoding_list = (const Instruction_Encoding *)add_encoding_list,
  .encoding_count = static_array_size(add_encoding_list),
};

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
      if (operand_type == Operand_Type_Register && encoding_type == Operand_Encoding_Type_Register_Memory) {
        continue;
      }
      if (operand_type == Operand_Type_Immediate_8 && encoding_type == Operand_Encoding_Type_Immediate_8) {
        continue;
      }
      match = false;
    }

    if (!match) continue;

    bool needs_mod_r_m = false;
    u8 reg_or_op_code = 0;
    u8 rex_byte = 0;
    u8 r_m = 0;
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      Operand_Encoding_Type encoding_type = encoding->operand_encoding_types[operand_index];

      if (operand->type == Operand_Type_Register) {
        // FIXME check is this is actuall true
        needs_mod_r_m = true;
        // FIXME add REX.W prefix only if 64 bit
        rex_byte |= REX_W;
        if (encoding_type == Operand_Encoding_Type_Register) {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = operand->reg.index;
        } else if (encoding_type == Operand_Encoding_Type_Register_Memory) {
          r_m = operand->reg.index;
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
    buffer_append_u8(buffer, (u8) encoding->op_code);

    // FIXME Implement proper mod support
    // FIXME mask register index
    if (needs_mod_r_m) {
      u8 mod_r_m = (
        (MOD_Register << 6) |
        (reg_or_op_code << 3) |
        (r_m)
      );
      buffer_append_u8(buffer, mod_r_m);
    }
    // Write out immediate operand(s?)
    for (u32 operand_index = 0; operand_index < 2; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      if (operand->type == Operand_Type_Immediate_8) {
        buffer_append_u8(buffer, operand->imm8);
      }
    }
    return;
  }
  // Didn't find any encoding
  assert(false);
}

void
buffer_append_sub_rsp_imm_8(
  Buffer *buffer,
  s8 value
) {
  buffer_append_u8(buffer, 0x48);
  buffer_append_u8(buffer, 0x83);
  buffer_append_u8(buffer, 0xec);
  buffer_append_u8(buffer, value);
}

void
buffer_append_add_rsp_imm_8(
  Buffer *buffer,
  s8 value
) {
  const Operand imm8 = { .type = Operand_Type_Immediate_8, .imm8 = value };
  encode(buffer, (Instruction) {add, {rsp, imm8}});
}

void
buffer_append_mov_to_stack_offset_imm_32(
  Buffer *buffer,
  s8 offset,
  s32 value
) {
  buffer_append_u8(buffer, 0xc7);
  buffer_append_u8(buffer, 0x44);
  buffer_append_u8(buffer, 0x24);
  buffer_append_u8(buffer, offset);
  buffer_append_s32(buffer, value);
}

void
buffer_append_add_to_ecx_value_at_stack_offset(
  Buffer *buffer,
  s8 value
) {
  buffer_append_u8(buffer, 0x03);
  buffer_append_u8(buffer, 0x4C);
  buffer_append_u8(buffer, 0x24);
  buffer_append_u8(buffer, value);
}

typedef s32 (*constant_s32)();
constant_s32
make_constant_s32(
  s32 value
) {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  buffer_append_u8(&buffer, 0x48);
  buffer_append_u8(&buffer, 0xc7);
  buffer_append_u8(&buffer, 0xc0);
  buffer_append_s32(&buffer, value);
  encode(&buffer, (Instruction) {ret, {0}});
  return (constant_s32)buffer.memory;
}

typedef s64 (*identity_s64)();
identity_s64
make_identity_s64() {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  encode(&buffer, (Instruction) {mov, {rax, rcx}});
  encode(&buffer, (Instruction) {ret, {0}});
  return (identity_s64)buffer.memory;
}

typedef s64 (*increment_s64)();
increment_s64
make_increment_s64() {
  Buffer buffer = make_buffer(1024, PAGE_EXECUTE_READWRITE);
  buffer_append_sub_rsp_imm_8(&buffer, 24);
  buffer_append_mov_to_stack_offset_imm_32(&buffer, 0, 1);
  buffer_append_add_to_ecx_value_at_stack_offset(&buffer, 0);
  encode(&buffer, (Instruction) {mov, {rax, rcx}});
  buffer_append_add_rsp_imm_8(&buffer, 24);
  encode(&buffer, (Instruction) {ret, {0}});
  return (increment_s64)buffer.memory;
}

spec("mass") {
  it("should create function that will return 42") {
    constant_s32 the_answer = make_constant_s32(42);
    s32 result = the_answer();
    check(result == 42);
    check(the_answer() == 42);
  }
  it("should create function that will return 21") {
    constant_s32 not_the_answer = make_constant_s32(21);
    s32 result = not_the_answer();
    check(result == 21);
  }
  it("should create function that returns s64 value that was passed") {
    identity_s64 id_s64 = make_identity_s64();
    s64 result = id_s64(42);
    check(result == 42);
  }

  it("should create function increments s64 value passed to it") {
    identity_s64 inc_s64 = make_increment_s64();
    s64 result = inc_s64(42);
    check(result == 43);
  }
}










