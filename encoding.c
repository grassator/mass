#include "value.h"

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
encode_instruction_internal(
  const Program *program,
  Fixed_Buffer *buffer,
  Instruction *instruction,
  const Instruction_Encoding *encoding,
  u32 operand_count
) {
  u64 original_buffer_length = buffer->occupied;

  s8 mod_r_m_operand_index = -1;
  u8 reg_or_op_code = 0;
  u8 rex_byte = 0;
  bool needs_16_bit_prefix = false;
  u8 r_m = 0;
  u8 mod = MOD_Register;
  u8 op_code[4] = {
    encoding->op_code[0],
    encoding->op_code[1],
    encoding->op_code[2],
    encoding->op_code[3],
  };
  bool needs_sib = false;
  u8 sib_byte = 0;
  s32 displacement = 0;

  for (u8 operand_index = 0; operand_index < operand_count; ++operand_index) {
    Operand *operand = &instruction->operands[operand_index];
    const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];

    if (operand->byte_size == 2) {
      needs_16_bit_prefix = true;
    }

    if (
      operand->byte_size == 8 &&
      !(
        operand_encoding->type == Operand_Encoding_Type_Xmm ||
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      )
    ) {
      rex_byte |= REX_W;
    }

    if (operand->type == Operand_Type_Register) {
      if (operand_encoding->type == Operand_Encoding_Type_Register) {
        if (encoding->extension_type == Instruction_Extension_Type_Plus_Register) {
          op_code[3] += operand->reg & 0b111;
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

    if (
      operand->type == Operand_Type_Xmm &&
      operand_encoding->type == Operand_Encoding_Type_Xmm &&
      encoding->extension_type == Instruction_Extension_Type_Register
    ) {
      reg_or_op_code = operand->reg;
    }

    if(
      operand_encoding->type == Operand_Encoding_Type_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Register_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
    ) {
      if (mod_r_m_operand_index != -1) {
        panic("Multiple MOD R/M operands are not supported in an instruction");
      }
      mod_r_m_operand_index = operand_index;
      if (operand->type == Operand_Type_RIP_Relative) {
        r_m = 0b101;
        mod = 0;
      } else if (operand->type == Operand_Type_Register) {
        r_m = operand->reg;
        if (operand->reg & 0b1000) {
          rex_byte |= REX_B;
        }
        mod = MOD_Register;
      } else if (operand->type == Operand_Type_Xmm) {
        r_m = operand->reg;
        mod = MOD_Register;
      } else {
        if (operand->type == Operand_Type_Memory_Indirect) {
          // :OperandNormalization
          assert(operand->indirect.reg != rsp.reg);
          displacement = operand->indirect.displacement;
          r_m = operand->indirect.reg;
        } else if (operand->type == Operand_Type_Sib) {
          displacement = operand->sib.displacement;
          needs_sib = true;
          r_m = R_M_SIB;

          if (operand->sib.index & 0b1000) {
            rex_byte |= REX_X;
          }
          sib_byte = (
            ((operand->sib.scale & 0b11) << 6) |
            ((operand->sib.index & 0b111) << 3) |
            ((operand->sib.base & 0b111) << 0)
          );
        } else {
          assert(!"Unsupported operand type");
        }
        if (displacement == 0) {
          mod = MOD_Displacement_0;
        } else if (s32_fits_into_s8(displacement)) {
          mod = MOD_Displacement_s8;
        } else {
          mod = MOD_Displacement_s32;
        }
      }
    }
  }

  if (encoding->extension_type == Instruction_Extension_Type_Op_Code) {
    reg_or_op_code = encoding->op_code_extension;
  }

  if (rex_byte) {
    fixed_buffer_append_u8(buffer, rex_byte);
  }

  if (needs_16_bit_prefix) {
    fixed_buffer_append_u8(buffer, 0x66);
  }

  if (op_code[0]) {
    fixed_buffer_append_u8(buffer, op_code[0]);
  }
  if (op_code[1]) {
    fixed_buffer_append_u8(buffer, op_code[1]);
  }
  if (op_code[2]) {
    fixed_buffer_append_u8(buffer, op_code[2]);
  }
  fixed_buffer_append_u8(buffer, op_code[3]);

  if (mod_r_m_operand_index != -1) {
    u8 mod_r_m = (
      (mod << 6) |
      ((reg_or_op_code & 0b111) << 3) |
      ((r_m & 0b111))
    );
    fixed_buffer_append_u8(buffer, mod_r_m);
  }

  if (needs_sib) {
    fixed_buffer_append_u8(buffer, sib_byte);
  }

  // :AfterInstructionPatch
  // Some operands are encoded as diff from a certain machine code byte
  // to the byte *after* the end of the instruction being encoded.
  // These operands can appear before the immediates, which means that
  // we do not know what the diff should be before the immediate is
  // encoded as well. To solve this we store the patch locations and do
  // a loop over them after the instruction has been encoded.
  s32 *after_instruction_diff_patches[3];
  u8 after_instruction_diff_patch_count = 0;

  // Write out displacement
  if (mod_r_m_operand_index != -1 && mod != MOD_Register) {
    Operand *operand = &instruction->operands[mod_r_m_operand_index];
    // :OperandNormalization
    if (operand->type == Operand_Type_RIP_Relative) {
      s64 operand_rva = program->data_base_rva + operand->rip_offset_in_data;
      // :AfterInstructionPatch
      after_instruction_diff_patches[after_instruction_diff_patch_count++] =
        fixed_buffer_append_s32(buffer, s64_to_s32(operand_rva));
    } else if (
      operand->type == Operand_Type_Memory_Indirect ||
      operand->type == Operand_Type_Sib
    ) {
      if (mod == MOD_Displacement_s32) {
        fixed_buffer_append_s32(buffer, displacement);
      } else if (mod == MOD_Displacement_s8) {
        fixed_buffer_append_s8(buffer, s32_to_s8(displacement));
      } else {
        assert(mod == MOD_Displacement_0);
      }
    }
  }

  // Write out immediate operand(s?)
  for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
    const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];
    if (operand_encoding->type != Operand_Encoding_Type_Immediate) {
      continue;
    }
    Operand *operand = &instruction->operands[operand_index];
    if (operand->type == Operand_Type_Label_32) {
      if (operand->label32->target) {
        s64 target_rva = operand->label32->target - buffer->memory;
        // :AfterInstructionPatch
        after_instruction_diff_patches[after_instruction_diff_patch_count++] =
          fixed_buffer_append_s32(buffer, s64_to_s32(target_rva));
      } else {
        s32 *patch_target = fixed_buffer_append_s32(buffer, 0xCCCCCCCC);
        dyn_array_push(operand->label32->locations, (Label_Location) {
          .patch_target = patch_target,
          .from_offset = buffer->memory + buffer->occupied,
        });
      }

    } else if (operand->type == Operand_Type_Immediate_8) {
      fixed_buffer_append_s8(buffer, operand->imm8);
    } else if (operand->type == Operand_Type_Immediate_16) {
      fixed_buffer_append_s16(buffer, operand->imm16);
    } else if (operand->type == Operand_Type_Immediate_32) {
      fixed_buffer_append_s32(buffer, operand->imm32);
    } else if (operand->type == Operand_Type_Immediate_64) {
      fixed_buffer_append_s64(buffer, operand->imm64);
    } else {
      panic("Unexpected mismatched operand type for immediate encoding.");
    }
  }

  instruction->encoded_byte_size = u64_to_u8(buffer->occupied - original_buffer_length);

  // :AfterInstructionPatch
  for (u64 i = 0; i < after_instruction_diff_patch_count; ++i){
    s32 *patch_target = after_instruction_diff_patches[i];
    s32 next_instruction_rva = u64_to_s32(program->code_base_rva + buffer->occupied);
    *patch_target -= next_instruction_rva;
  }
}

void
encode_instruction(
  const Program *program,
  Fixed_Buffer *buffer,
  Instruction *instruction
) {
  if (instruction->maybe_label) {
    Label *label = instruction->maybe_label;
    assert(!label->target);
    label->target = buffer->memory + buffer->occupied;

    for (u64 i = 0; i < dyn_array_length(label->locations); ++i) {
      Label_Location *label_location = dyn_array_get(label->locations, i);
      s64 diff = (label->target - label_location->from_offset);
      assert(diff >= 0);
      *label_location->patch_target = s64_to_s32(diff);
    }
    instruction->encoded_byte_size = 0;
    return;
  }

  u32 operand_count = sizeof(instruction->operands) / sizeof(instruction->operands[0]);
  for (u32 index = 0; index < instruction->mnemonic->encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction->mnemonic->encoding_list[index];
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
      const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];
      Operand *operand = &instruction->operands[operand_index];

      if (operand_encoding->size != Operand_Size_Any) {
        if (operand->byte_size != s32_to_u32(operand_encoding->size)) {
          encoding = 0;
          break;
        }
      }
      if (
        operand->type == Operand_Type_Eflags &&
        operand_encoding->type == Operand_Encoding_Type_Eflags
      ) {
        continue;
      }

      if (
        operand->type == Operand_Type_None &&
        operand_encoding->type == Operand_Encoding_Type_None
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Register &&
        operand->reg == Register_A &&
        operand_encoding->type == Operand_Encoding_Type_Register_A
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
      if (
        operand->type == Operand_Type_Sib &&
        operand_encoding->type == Operand_Encoding_Type_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Sib &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Xmm &&
        operand_encoding->type == Operand_Encoding_Type_Xmm
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Xmm &&
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_Memory_Indirect &&
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      ) {
        continue;
      }
      if (
        operand->type == Operand_Type_RIP_Relative &&
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
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
          operand->type == Operand_Type_Immediate_16 &&
          encoding_size == Operand_Size_16
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
      encoding = 0;
      break;
    }

    if (encoding) {
      encode_instruction_internal(program, buffer, instruction, encoding, operand_count);
      return;
    }
  }
  const Compiler_Source_Location *compiler_location = instruction->compiler_source_location;
  if (compiler_location) {
    printf(
      "Added in compiler at %s:%u (fn: %s)\n",
      compiler_location->filename,
      compiler_location->line_number,
      compiler_location->function_name
    );
  } else {
    printf("Unknown compiler location\n");
  }
  const Source_Location *source_location = instruction->source_location;
  if (source_location) {
    printf(
      "Source code at %.*s:(%llu:%llu)\n",
      u64_to_s32(source_location->filename.length),
      source_location->filename.bytes,
      source_location->line,
      source_location->column
    );
  } else {
    printf("Unknown source location\n");
  }
  printf("%s", instruction->mnemonic->name);
  for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
    Operand *operand = &instruction->operands[operand_index];
    printf(" ");
    print_operand(operand);
  }
  printf("\n");
  assert(!"Did not find acceptable encoding");
}
