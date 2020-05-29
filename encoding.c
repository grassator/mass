#include "value.h"

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

void
encode(
  Function_Builder *builder,
  Instruction instruction
) {
  Buffer *buffer = &builder->buffer;
  u32 operand_count = sizeof(instruction.operands) / sizeof(instruction.operands[0]);
  for (u32 index = 0; index < instruction.mnemonic.encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction.mnemonic.encoding_list[index];
    bool match = true;
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
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
    u8 op_code[2] = { encoding->op_code[0], encoding->op_code[1] };
    bool needs_sib = false;
    u8 sib_byte = 0;

    bool encoding_stack_operand = false;

    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
      Operand *operand = &instruction.operands[operand_index];
      Operand_Encoding_Type encoding_type = encoding->operand_encoding_types[operand_index];

      if (operand->byte_size == 8) {
        rex_byte |= REX_W;
      }

      if (operand->type == Operand_Type_Register) {
        if (encoding_type == Operand_Encoding_Type_Register) {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = operand->reg;
          if (operand->reg & 0b1000) {
            rex_byte |= REX_R;
          }
        } else if (encoding_type == Operand_Encoding_Type_Op_Code_Plus_Register) {
          op_code[1] += operand->reg & 0b111;
          if (operand->reg & 0b1000) {
            rex_byte |= REX_B;
          }
        }
      }
      if(encoding_type == Operand_Encoding_Type_Register_Memory) {
        needs_mod_r_m = true;
        if (operand->type == Operand_Type_Register) {
          r_m = operand->reg;
          if (operand->reg & 0b1000) {
            rex_byte |= REX_B;
          }
          mod = MOD_Register;
        } else {
          mod = MOD_Displacement_s32;
          assert(operand->type == Operand_Type_Memory_Indirect);
          r_m = operand->indirect.reg;
          if (r_m == rsp.reg) {
            encoding_stack_operand = true;
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

    if (op_code[0]) {
      buffer_append_u8(buffer, op_code[0]);
    }
    buffer_append_u8(buffer, op_code[1]);

    if (needs_mod_r_m) {
      u8 mod_r_m = (
        (mod << 6) |
        ((reg_or_op_code & 0b111) << 3) |
        ((r_m & 0b111))
      );
      buffer_append_u8(buffer, mod_r_m);
    }

    if (needs_sib) {
      buffer_append_u8(buffer, sib_byte);
    }

    u64 offset_of_displacement = 0;
    u32 stack_byte_size = 0;

    // Write out displacement
    if (needs_mod_r_m && mod != MOD_Register) {
      for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
        Operand *operand = &instruction.operands[operand_index];
        if (operand->type == Operand_Type_Memory_Indirect) {
          if (mod == MOD_Displacement_s32) {
            if (encoding_stack_operand) {
              offset_of_displacement = buffer->occupied;
              stack_byte_size = operand->byte_size;
            }
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
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
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
    if (offset_of_displacement) {
      assert(builder->stack_displacement_count < MAX_DISPLACEMENT_COUNT);
      s32 *location = (s32 *)(buffer->memory + offset_of_displacement);
      builder->stack_displacements[builder->stack_displacement_count] = (const Stack_Patch) {
        .location = location,
        .byte_size = stack_byte_size,
      };
      builder->stack_displacement_count++;
    }
    return;
  }
  printf("%s", instruction.mnemonic.name);
  for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
    Operand *operand = &instruction.operands[operand_index];
    printf(" %s", operand_type_string(operand->type));
  }
  printf("\n");
  // Didn't find any encoding
  assert(!"Did not find acceptable encoding");
}
