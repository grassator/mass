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

s32
adjust_stack_displacement(
  Function_Builder *builder,
  s32 displacement
) {
  // Negative diplacement is used to encode local variables
  if (displacement < 0) {
    displacement += builder->stack_reserve;
  } else
  // Positive values larger than max_call_parameters_stack_size
  if (displacement >= u32_to_s32(builder->max_call_parameters_stack_size)) {
    // Return address will be pushed on the stack by the caller
    // and we need to account for that
    s32 return_address_size = 8;
    displacement += builder->stack_reserve + return_address_size;
  }
  return displacement;
}

void
encode_instruction(
  Fixed_Buffer *buffer,
  Function_Builder *builder,
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

  u64 original_buffer_length = buffer->occupied;
  u32 operand_count = sizeof(instruction->operands) / sizeof(instruction->operands[0]);
  for (u32 index = 0; index < instruction->mnemonic->encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction->mnemonic->encoding_list[index];
    bool match = true;
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
      const Operand_Encoding *operand_encoding = &encoding->operands[operand_index];
      Operand *operand = &instruction->operands[operand_index];

      if (operand_encoding->size != Operand_Size_Any) {
        if (operand->byte_size != s32_to_u32(operand_encoding->size)) {
          match = false;
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
        operand->type == Operand_Type_RIP_Relative_Import &&
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
        operand->type == Operand_Type_RIP_Relative_Import &&
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
      match = false;
    }

    if (!match) continue;

    bool needs_mod_r_m = false;
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

    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
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
        needs_mod_r_m = true;
        if (
          operand->type == Operand_Type_RIP_Relative ||
          operand->type == Operand_Type_RIP_Relative_Import
        ) {
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
            displacement = operand->indirect.displacement;
            if (operand->indirect.reg == rsp.reg) {
              r_m = R_M_SIB;
              displacement = adjust_stack_displacement(builder, displacement);
              needs_sib = true;
              sib_byte = (
                (SIB_Scale_1 << 6) |
                (r_m << 3) |
                (r_m)
              );
            } else {
              r_m = operand->indirect.reg;
            }
          } else if (operand->type == Operand_Type_Sib) {
            displacement = operand->sib.displacement;
            needs_sib = true;
            r_m = R_M_SIB;

            if (operand->sib.index & 0b1000) {
              rex_byte |= REX_X;
            }
            if (operand->sib.base == rsp.reg) {
              displacement = adjust_stack_displacement(builder, displacement);
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

    if (needs_mod_r_m) {
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

    // Write out displacement
    if (needs_mod_r_m && mod != MOD_Register) {
      for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
        Operand *operand = &instruction->operands[operand_index];
        if (operand->type == Operand_Type_RIP_Relative_Import) {
          Program *program = builder->program;
          s64 next_instruction_rva = program->code_base_rva + buffer->occupied + sizeof(s32);
          Import_Symbol *symbol = program_find_import(
            program,
            operand->import.library_name,
            operand->import.symbol_name
          );
          assert(symbol);
          s64 diff = program->data_base_rva + symbol->offset_in_data - next_instruction_rva;
          fixed_buffer_append_s32(buffer, s64_to_s32(diff));
        } else if (operand->type == Operand_Type_RIP_Relative) {
          Program *program = builder->program;
          s64 next_instruction_rva = program->code_base_rva + buffer->occupied + sizeof(s32);

          s64 operand_rva = program->data_base_rva + operand->rip_offset_in_data;
          s64 diff = operand_rva - next_instruction_rva;
          fixed_buffer_append_s32(buffer, s64_to_s32(diff));
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
    }
    // Write out immediate operand(s?)
    for (u32 operand_index = 0; operand_index < operand_count; ++operand_index) {
      Operand *operand = &instruction->operands[operand_index];
      if (operand->type == Operand_Type_Label_32) {
        if (operand->label32->target) {
          u8 *from = buffer->memory + buffer->occupied + sizeof(s32);
          s32 diff = s64_to_s32(operand->label32->target - from);
          assert(diff < 0);
          fixed_buffer_append_s32(buffer, diff);
        } else {
          s32 *patch_target = (s32 *)(buffer->memory + buffer->occupied);
          fixed_buffer_append_s32(buffer, 0xCCCCCCCC);

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
      }
    }

    instruction->encoded_byte_size = u64_to_u8(
      buffer->occupied - original_buffer_length
    );

    return;
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
