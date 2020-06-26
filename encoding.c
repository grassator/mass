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
encode_instruction(
  Function_Builder *builder,
  Instruction instruction
) {
  Buffer *buffer = builder->buffer;

  if (instruction.maybe_label) {
    Label *label = instruction.maybe_label;
    assert(!label->target);
    label->target = buffer->memory + buffer->occupied;

    for (
      Label_Location *label_location = array_begin(label->locations);
      label_location != array_end(label->locations);
      ++label_location
    ) {
      s32 diff = (s32)(label->target - label_location->from_offset);
      assert(diff >= 0);
      *label_location->patch_target = diff;
    }
    return;
  }

  u32 operand_count = sizeof(instruction.operands) / sizeof(instruction.operands[0]);
  for (u32 index = 0; index < instruction.mnemonic.encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction.mnemonic.encoding_list[index];
    bool match = true;
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
      const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];
      Operand *operand = &instruction.operands[operand_index];

      if (operand_encoding->size != Operand_Size_Any) {
        if (operand->byte_size != (u32)operand_encoding->size) {
          match = false;
          break;
        }
      }

      if (
        operand->type == Operand_Type_None &&
        operand_encoding->type == Operand_Encoding_Type_None
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Register &&
        operand_encoding->type == Operand_Encoding_Type_Register
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Register &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_RIP_Relative &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Memory_Indirect &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_RIP_Relative &&
        operand_encoding->type == Operand_Encoding_Type_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Memory_Indirect &&
        operand_encoding->type == Operand_Encoding_Type_Memory
      ) {
        continue;
      }
      if (operand_encoding->type == Operand_Encoding_Type_Immediate) {
        Operand_Size encoding_size = operand_encoding->size;
        if (operand->type == Operand_Type_Immediate_8 && encoding_size == Operand_Size_8) {
          continue;
        }
        if (
          operand->type == Operand_Type_Label_32 &&
          encoding_size == Operand_Size_32
        ) {
          continue;
        }
        if (
          operand->type == Operand_Type_Immediate_32 &&
          encoding_size == Operand_Size_32
        ) {
          continue;
        }
        if (operand->type == Operand_Type_Immediate_64 && encoding_size == Operand_Size_64) {
          continue;
        }
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
      const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];

      if (operand->byte_size == 8) {
        rex_byte |= REX_W;
      }

      if (operand->type == Operand_Type_Register) {
        if (operand_encoding->type == Operand_Encoding_Type_Register) {
          if (encoding->extension_type == Instruction_Extension_Type_Plus_Register) {
            op_code[1] += operand->reg & 0b111;
            if (operand->reg & 0b1000) {
              rex_byte |= REX_B;
            }
          } else {
            assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
            reg_or_op_code = operand->reg;
            if (operand->reg & 0b1000) {
              rex_byte |= REX_R;
            }
          }
        }
      }
      if(
        operand_encoding->type == Operand_Encoding_Type_Memory ||
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        needs_mod_r_m = true;
        if (operand->type == Operand_Type_RIP_Relative) {
          r_m = 0b101;
          mod = 0;
        } else if (operand->type == Operand_Type_Register) {
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

    // Write out displacement
    if (needs_mod_r_m && mod != MOD_Register) {
      for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
        Operand *operand = &instruction.operands[operand_index];
        if (operand->type == Operand_Type_RIP_Relative) {
          s64 start_address = (s64) buffer->memory;
          s64 next_instruction_address = start_address + buffer->occupied + sizeof(s32);

          s64 diff = operand->imm64 - next_instruction_address;
          assert(diff <= (s32)0x7FFFFFFF && diff >= (s32)0xFFFFFFFF);
          s32 displacement = (s32)(diff);

          buffer_append_s32(buffer, displacement);
        }  else if (operand->type == Operand_Type_Memory_Indirect) {
          s32 displacement = operand->indirect.displacement;


          if (encoding_stack_operand) {
            // Negative diplacement is used to encode local variables
            if (displacement < 0) {
              displacement += builder->stack_reserve;
            } else
            // Positive values larger than max_call_parameters_stack_size
            if (displacement >= (s32)builder->max_call_parameters_stack_size) {
              // Return address will be pushed on the stack by the caller
              // and we need to account for that
              s32 return_address_size = 8;
              displacement += builder->stack_reserve + return_address_size;
            }
          }
          if (mod == MOD_Displacement_s32) {
            buffer_append_s32(buffer, displacement);
          } else if (mod == MOD_Displacement_s8) {
            buffer_append_s8(buffer, (s8)displacement);
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
      if (operand->type == Operand_Type_Label_32) {
        if (operand->label32->target) {
          u8 *from = buffer->memory + buffer->occupied + sizeof(s32);
          s32 diff = (s32)(operand->label32->target - from);
          assert(diff < 0);
          buffer_append_s32(buffer, diff);
        } else {
          s32 *patch_target = (s32 *)(buffer->memory + buffer->occupied);
          buffer_append_s32(buffer, 0xCCCCCCCC);

          array_push(operand->label32->locations, (Label_Location) {
            .patch_target = patch_target,
            .from_offset = buffer->memory + buffer->occupied,
          });
        }

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
  printf("%s", instruction.mnemonic.name);
  for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
    Operand *operand = &instruction.operands[operand_index];
    printf(" ");
    print_operand(operand);
  }
  printf("\n");
  // Didn't find any encoding
  assert(!"Did not find acceptable encoding");
}
