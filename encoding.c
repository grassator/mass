#include "value.h"

typedef enum {
  MOD_Displacement_0   = 0b00,
  MOD_Displacement_s8  = 0b01,
  MOD_Displacement_s32 = 0b10,
  MOD_Register         = 0b11,
} MOD;

typedef enum {
  REX   = 0b01000000,
  REX_W = 0b01001000, // 0 = Storage size determined by CS.D; 1 = 64 Bit Storage Size
  REX_R = 0b01000100, // Extension of the ModR/M reg field
  REX_X = 0b01000010, // Extension of the SIB index field
  REX_B = 0b01000001, // Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
} REX_BYTE;

static inline void
instruction_bytes_append_bytes(
  Instruction_Bytes *instruction,
  const u8 *bytes,
  u64 length
) {
  assert(instruction->length + length <= countof(instruction->memory));
  memcpy(instruction->memory + instruction->length, bytes, length);
  instruction->length += u64_to_u8(length);
}

static void
eager_encode_instruction_assembly(
  Array_Instruction *instructions,
  const Instruction *instruction,
  const Instruction_Encoding *encoding
) {
  assert(instruction->tag == Instruction_Tag_Assembly);

  Instruction result = (Instruction){
    .tag = Instruction_Tag_Bytes,
    .Bytes = {0},
    .source_range = instruction->source_range,
    .compiler_source_location = instruction->compiler_source_location,
  };

  assert(instruction->tag == Instruction_Tag_Assembly);

  s8 mod_r_m_storage_index = -1;
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

  u32 storage_count = countof(instruction->Assembly.operands);
  for (u8 storage_index = 0; storage_index < storage_count; ++storage_index) {
    const Storage *storage = &instruction->Assembly.operands[storage_index];
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];

    if (storage->byte_size == 2) {
      needs_16_bit_prefix = true;
    }

    if (
      storage->byte_size == 8 &&
      !(
        operand_encoding->type == Operand_Encoding_Type_Xmm ||
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      )
    ) {
      rex_byte |= REX_W;
    }

    if (storage->tag == Storage_Tag_Register) {
      assert(storage->Register.offset_in_bits == 0);

      if (storage->byte_size == 1) {
        // :64bitMode8BitOperations
        // These registers are inaccessible in 32bit mode and AH, BH, CH, and DH
        // are targeted instead. To solve this we force REX prefix.
        if (
          storage->Register.index == Register_SI ||
          storage->Register.index == Register_DI ||
          storage->Register.index == Register_SP ||
          storage->Register.index == Register_BP
        ) {
          rex_byte |= REX;
        }
      }

      if (operand_encoding->type == Operand_Encoding_Type_Register) {
        if (encoding->extension_type == Instruction_Extension_Type_Plus_Register) {
          op_code[3] += storage->Register.index & 0b111;
          if (storage->Register.index & 0b1000) {
            rex_byte |= REX_B;
          }
        } else {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = storage->Register.index;
          if (storage->Register.index & 0b1000) {
            rex_byte |= REX_R;
          }
        }
      }
    }

    if (
      storage->tag == Storage_Tag_Xmm &&
      operand_encoding->type == Operand_Encoding_Type_Xmm &&
      encoding->extension_type == Instruction_Extension_Type_Register
    ) {
      reg_or_op_code = storage->Register.index;
    }

    if(
      operand_encoding->type == Operand_Encoding_Type_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Register_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
    ) {
      if (mod_r_m_storage_index != -1) {
        panic("Multiple MOD R/M operands are not supported in an instruction");
      }
      mod_r_m_storage_index = storage_index;
      if (storage->tag == Storage_Tag_Register) {
        r_m = storage->Register.index;
        mod = MOD_Register;
      } else if (storage->tag == Storage_Tag_Xmm) {
        r_m = storage->Register.index;
        mod = MOD_Register;
      } else if (storage->tag == Storage_Tag_Memory) {
        Memory_Location location = storage->Memory.location;
        bool can_have_zero_displacement = true;
        enum Sib_Scale { Sib_Scale_1 = 0b00, Sib_Scale_2 = 0b01, Sib_Scale_4 = 0b10, Sib_Scale_8 = 0b11,};
        enum { Sib_Index_None = 0b100,};
        switch(location.tag) {
          case Memory_Location_Tag_Instruction_Pointer_Relative: {
            r_m = 0b101;
            mod = 0b00;
            break;
          }
          case Memory_Location_Tag_Stack: {
            needs_sib = true;
            r_m = 0b0100; // SIB
            sib_byte = (((Sib_Index_None & 0b111) << 3) | Register_SP);
            mod = MOD_Displacement_s32;
            break;
          }
          case Memory_Location_Tag_Indirect: {
            // Right now the compiler does not support SIB scale other than 1.
            // From what I can tell there are two reasons that only matter in *extremely*
            // performance-sensitive code which probably would be written by hand anyway:
            // 1) `add rax, 8` is three bytes longer than `inc rax`. With current instruction
            //    cache sizes it is very unlikely to be problematic.
            // 2) Loop uses the same index for arrays of values of different sizes that could
            //    be represented with SIB scale. In cases of extreme register pressure this
            //    can cause spilling. To avoid that we could try to use temporary shifts
            //    to adjust the offset between different indexes, but it is not implemented ATM.
            enum Sib_Scale sib_scale_bits = Sib_Scale_1;
            Register base = location.Indirect.base_register;
            // TODO enable this when the compiler makes use of indexed access
            /*
            if (location.Indirect.maybe_index_register.has_value) {
              needs_sib = true;
              r_m = 0b0100; // SIB
              Register sib_index = location.Indirect.maybe_index_register.index;
              sib_byte = (
                ((sib_scale_bits & 0b11) << 6) |
                ((sib_index & 0b111) << 3) |
                ((base & 0b111) << 0)
              );
              if (sib_index & 0b1000) {
                rex_byte |= REX_X;
              }
            } else
            */
            if (base == Register_SP || base == Register_R12) {
              // [RSP + X] and [R12 + X] always needs to be encoded as SIB because
              // 0b100 register index in MOD R/M is occupied by SIB byte indicator
              needs_sib = true;
              r_m = 0b0100; // SIB
              sib_byte = (
                ((sib_scale_bits & 0b11) << 6) |
                ((Sib_Index_None & 0b111) << 3) |
                ((base & 0b111) << 0)
              );
            } else {
              r_m = base;
            }
            // :RipRelativeEncoding
            // 0b101 value is occupied RIP-relative encoding indicator
            // when mod is 00, so for the (RBP / R13) always use disp8 (mod 01)
            if (base == Register_BP || base == Register_R13) {
              can_have_zero_displacement = false;
            }
            displacement = s64_to_s32(location.Indirect.offset);
            // :RipRelativeEncoding
            if (can_have_zero_displacement && displacement == 0) {
              mod = MOD_Displacement_0;
            } else if (s32_fits_into_s8(displacement)) {
              mod = MOD_Displacement_s8;
            } else {
              mod = MOD_Displacement_s32;
            }
            break;
          }
        }
      } else {
        panic("Unsupported operand type");
      }
    }
  }

  if (encoding->extension_type == Instruction_Extension_Type_Op_Code) {
    reg_or_op_code = encoding->op_code_extension;
  }

  if (r_m & 0b1000) {
    rex_byte |= REX_B;
  }

  if (needs_16_bit_prefix) {
    instruction_bytes_append_bytes(&result.Bytes, &(u8){0x66}, 1);
  }

  if (rex_byte) {
    instruction_bytes_append_bytes(&result.Bytes, &rex_byte, 1);
  }

  if (op_code[0]) {
    instruction_bytes_append_bytes(&result.Bytes, &op_code[0], 1);
  }
  if (op_code[1]) {
    instruction_bytes_append_bytes(&result.Bytes, &op_code[1], 1);
  }
  if (op_code[2]) {
    instruction_bytes_append_bytes(&result.Bytes, &op_code[2], 1);
  }
  instruction_bytes_append_bytes(&result.Bytes, &op_code[3], 1);

  Instruction_Stack_Patch stack_patch = {
    .mod_r_m_offset_in_previous_instruction = result.Bytes.length,
  };
  bool has_stack_patch = false;
  if (mod_r_m_storage_index != -1) {
    u8 mod_r_m = (
      (mod << 6) |
      ((reg_or_op_code & 0b111) << 3) |
      ((r_m & 0b111))
    );
    instruction_bytes_append_bytes(&result.Bytes, &mod_r_m, 1);
  }

  if (needs_sib) {
    instruction_bytes_append_bytes(&result.Bytes, &sib_byte, 1);
  }

  enum {MAX_PATCH_COUNT = 2};
  Instruction_Label_Patch patches[MAX_PATCH_COUNT] = {0};
  s32 patch_count = 0;

  // Write out displacement
  if (mod_r_m_storage_index != -1 && mod != MOD_Register) {
    const Storage *storage = &instruction->Assembly.operands[mod_r_m_storage_index];
    assert (storage->tag == Storage_Tag_Memory);
    const Memory_Location *location = &storage->Memory.location;
    switch(location->tag) {
      case Memory_Location_Tag_Instruction_Pointer_Relative: {
        Label_Index label_index = location->Instruction_Pointer_Relative.label_index;
        u8 offset_in_instruction = result.Bytes.length;
        patches[patch_count++] = (Instruction_Label_Patch){
          .offset = offset_in_instruction,
          .label_index = label_index
        };
        u8 empty_patch_bytes[4] = {0};
        instruction_bytes_append_bytes(&result.Bytes, empty_patch_bytes, countof(empty_patch_bytes));
      } break;
      case Memory_Location_Tag_Indirect: {
        if (mod == MOD_Displacement_s32) {
          instruction_bytes_append_bytes(&result.Bytes, (u8 *)&displacement, sizeof(displacement));
        } else if (mod == MOD_Displacement_s8) {
          u8 byte = (u8)s32_to_s8(displacement);
          instruction_bytes_append_bytes(&result.Bytes, &byte, sizeof(byte));
        } else {
          assert(mod == MOD_Displacement_0);
        }
      } break;
      case Memory_Location_Tag_Stack: {
        stack_patch.stack_area = location->Stack.area;
        has_stack_patch = true;

        // :OversizedStackOffsets
        // Here we reserve full 4 bytes for stack offset but it might be patched
        // to a smaller size when total stack size is known
        s32 stack_offset = location->Stack.offset;
        instruction_bytes_append_bytes(&result.Bytes, (u8 *)&stack_offset, sizeof(stack_offset));
      } break;
    }
  }

  // Write out immediate operand(s?)
  for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];
    if (operand_encoding->type != Operand_Encoding_Type_Immediate) {
      continue;
    }
    const Storage *storage = &instruction->Assembly.operands[storage_index];
    if (storage_is_label(storage)) {
      Label_Index label_index = storage->Memory.location.Instruction_Pointer_Relative.label_index;
      u8 offset_in_instruction = result.Bytes.length;
      patches[patch_count++] = (Instruction_Label_Patch){
        .offset = offset_in_instruction,
        .label_index = label_index
      };
      u8 empty_patch_bytes[4] = {0};
      instruction_bytes_append_bytes(&result.Bytes, empty_patch_bytes, countof(empty_patch_bytes));
    } else if (storage->tag == Storage_Tag_Static) {
      const u8 *bytes = storage_static_as_c_type_internal(storage, storage->byte_size);
      instruction_bytes_append_bytes(&result.Bytes, bytes, storage->byte_size);
    } else {
      panic("Unexpected mismatched operand type for immediate encoding.");
    }
  }

  // It is important to push instruction all at once here to make sure array
  // doesn't get resized invalidating the pointer to one of the instructions
  // that still needs to be updated like the patch offsets below.
  dyn_array_push(*instructions, result);

  // Stack patch MUST go before label patches as it might change the size of the instruction
  if (has_stack_patch) {
    dyn_array_push(*instructions, instruction->source_range, (Instruction) {
      .tag = Instruction_Tag_Stack_Patch,
      .Stack_Patch = stack_patch,
      .source_range = instruction->source_range,
      .compiler_source_location = instruction->compiler_source_location,
    });
  }

  assert(patch_count <= MAX_PATCH_COUNT);
  for (s32 i = 0; i < patch_count; i += 1) {
    patches[i].offset -= result.Bytes.length;
    dyn_array_push(*instructions, instruction->source_range, (Instruction) {
      .tag = Instruction_Tag_Label_Patch,
      .Label_Patch = patches[i],
      .source_range = instruction->source_range,
      .compiler_source_location = instruction->compiler_source_location,
    });
  }

  result.encoded_byte_size = result.Bytes.length;
}

static void
encode_instruction_assembly(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  Instruction *instruction,
  const Instruction_Encoding *encoding,
  u32 storage_count
) {
  assert(instruction->tag == Instruction_Tag_Assembly);
  u64 original_buffer_length = buffer->occupied;

  s8 mod_r_m_storage_index = -1;
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

  for (u8 storage_index = 0; storage_index < storage_count; ++storage_index) {
    Storage *storage = &instruction->Assembly.operands[storage_index];
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];

    if (storage->byte_size == 2) {
      needs_16_bit_prefix = true;
    }

    if (
      storage->byte_size == 8 &&
      !(
        operand_encoding->type == Operand_Encoding_Type_Xmm ||
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      )
    ) {
      rex_byte |= REX_W;
    }

    if (storage->tag == Storage_Tag_Register) {
      assert(storage->Register.offset_in_bits == 0);

      if (storage->byte_size == 1) {
        // :64bitMode8BitOperations
        // These registers are inaccessible in 32bit mode and AH, BH, CH, and DH
        // are targeted instead. To solve this we force REX prefix.
        if (
          storage->Register.index == Register_SI ||
          storage->Register.index == Register_DI ||
          storage->Register.index == Register_SP ||
          storage->Register.index == Register_BP
        ) {
          rex_byte |= REX;
        }
      }

      if (operand_encoding->type == Operand_Encoding_Type_Register) {
        if (encoding->extension_type == Instruction_Extension_Type_Plus_Register) {
          op_code[3] += storage->Register.index & 0b111;
          if (storage->Register.index & 0b1000) {
            rex_byte |= REX_B;
          }
        } else {
          assert(encoding->extension_type != Instruction_Extension_Type_Op_Code);
          reg_or_op_code = storage->Register.index;
          if (storage->Register.index & 0b1000) {
            rex_byte |= REX_R;
          }
        }
      }
    }

    if (
      storage->tag == Storage_Tag_Xmm &&
      operand_encoding->type == Operand_Encoding_Type_Xmm &&
      encoding->extension_type == Instruction_Extension_Type_Register
    ) {
      reg_or_op_code = storage->Register.index;
    }

    if(
      operand_encoding->type == Operand_Encoding_Type_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Register_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
    ) {
      if (mod_r_m_storage_index != -1) {
        panic("Multiple MOD R/M operands are not supported in an instruction");
      }
      mod_r_m_storage_index = storage_index;
      if (storage->tag == Storage_Tag_Register) {
        r_m = storage->Register.index;
        mod = MOD_Register;
      } else if (storage->tag == Storage_Tag_Xmm) {
        r_m = storage->Register.index;
        mod = MOD_Register;
      } else if (storage->tag == Storage_Tag_Memory) {
        const Memory_Location *location = &storage->Memory.location;
        bool can_have_zero_displacement = true;
        switch(location->tag) {
          case Memory_Location_Tag_Instruction_Pointer_Relative: {
            r_m = 0b101;
            break;
          }
          case Memory_Location_Tag_Indirect: {
            // Right now the compiler does not support SIB scale other than 1.
            // From what I can tell there are two reasons that only matter in *extremely*
            // performance-sensitive code which probably would be written by hand anyway:
            // 1) `add rax, 8` is three bytes longer than `inc rax`. With current instruction
            //    cache sizes it is very unlikely to be problematic.
            // 2) Loop uses the same index for arrays of values of different sizes that could
            //    be represented with SIB scale. In cases of extreme register pressure this
            //    can cause spilling. To avoid that we could try to use temporary shifts
            //    to adjust the offset between different indexes, but it is not implemented ATM.
            enum { Sib_Scale_1 = 0b00, Sib_Scale_2 = 0b01, Sib_Scale_4 = 0b10, Sib_Scale_8 = 0b11,};
            enum { Sib_Index_None = 0b100,};
            u8 sib_scale_bits = Sib_Scale_1;
            Register base = storage->Memory.location.Indirect.base_register;
            // TODO enable this when the compiler makes use of indexed access
            /*
            if (storage->Memory.location.Indirect.maybe_index_register.has_value) {
              needs_sib = true;
              r_m = 0b0100; // SIB
              Register sib_index = storage->Memory.location.Indirect.maybe_index_register.index;
              sib_byte = (
                ((sib_scale_bits & 0b11) << 6) |
                ((sib_index & 0b111) << 3) |
                ((base & 0b111) << 0)
              );
              if (sib_index & 0b1000) {
                rex_byte |= REX_X;
              }
            } else
            */
            if (base == Register_SP || base == Register_R12) {
              // [RSP + X] and [R12 + X] always needs to be encoded as SIB because
              // 0b100 register index in MOD R/M is occupied by SIB byte indicator
              needs_sib = true;
              r_m = 0b0100; // SIB
              sib_byte = (
                ((sib_scale_bits & 0b11) << 6) |
                ((Sib_Index_None & 0b111) << 3) |
                ((base & 0b111) << 0)
              );
            } else {
              r_m = base;
            }
            // :RipRelativeEncoding
            // 0b101 value is occupied RIP-relative encoding indicator
            // when mod is 00, so for the (RBP / R13) always use disp8 (mod 01)
            if (base == Register_BP || base == Register_R13) {
              can_have_zero_displacement = false;
            }
            displacement = s64_to_s32(storage->Memory.location.Indirect.offset);
            break;
          }
          case Memory_Location_Tag_Stack: {
            panic("Stack locations must be resolved beforehand");
          }
        }
        // :RipRelativeEncoding
        if (can_have_zero_displacement && displacement == 0) {
          mod = MOD_Displacement_0;
        } else if (s32_fits_into_s8(displacement)) {
          mod = MOD_Displacement_s8;
        } else {
          mod = MOD_Displacement_s32;
        }
      } else {
        panic("Unsupported operand type");
      }
    }
  }

  if (encoding->extension_type == Instruction_Extension_Type_Op_Code) {
    reg_or_op_code = encoding->op_code_extension;
  }

  if (r_m & 0b1000) {
    rex_byte |= REX_B;
  }

  if (needs_16_bit_prefix) {
    virtual_memory_buffer_append_u8(buffer, 0x66);
  }

  if (rex_byte) {
    virtual_memory_buffer_append_u8(buffer, rex_byte);
  }

  if (op_code[0]) {
    virtual_memory_buffer_append_u8(buffer, op_code[0]);
  }
  if (op_code[1]) {
    virtual_memory_buffer_append_u8(buffer, op_code[1]);
  }
  if (op_code[2]) {
    virtual_memory_buffer_append_u8(buffer, op_code[2]);
  }
  virtual_memory_buffer_append_u8(buffer, op_code[3]);

  if (mod_r_m_storage_index != -1) {
    u8 mod_r_m = (
      (mod << 6) |
      ((reg_or_op_code & 0b111) << 3) |
      ((r_m & 0b111))
    );
    virtual_memory_buffer_append_u8(buffer, mod_r_m);
  }

  if (needs_sib) {
    virtual_memory_buffer_append_u8(buffer, sib_byte);
  }

  // :AfterInstructionPatch
  // Some operands are encoded as diff from a certain machine code byte
  // to the byte *after* the end of the instruction being encoded.
  // These operands can appear before the immediates, which means that
  // we do not know what the diff should be before the immediate is
  // encoded as well. To solve this we store the patch locations and do
  // a loop over them after the instruction has been encoded.
  Label_Location_Diff_Patch_Info *mod_r_m_patch_info = 0;
  Label_Location_Diff_Patch_Info *immediate_label_patch_info = 0;

  // Write out displacement
  if (mod_r_m_storage_index != -1 && mod != MOD_Register) {
    const Storage *storage = &instruction->Assembly.operands[mod_r_m_storage_index];
    assert (storage->tag == Storage_Tag_Memory);
    const Memory_Location *location = &storage->Memory.location;
    switch(location->tag) {
      case Memory_Location_Tag_Instruction_Pointer_Relative: {
        Label_Index label_index =
          storage->Memory.location.Instruction_Pointer_Relative.label_index;
        // :StorageNormalization
        s32 *patch_target = virtual_memory_buffer_allocate_unaligned(buffer, s32);
        // :AfterInstructionPatch
        mod_r_m_patch_info =
          dyn_array_push(program->patch_info_array, (Label_Location_Diff_Patch_Info) {
            .target_label_index = label_index,
            .from = {.section = &program->memory.code},
            .patch_target = patch_target,
          });
        break;
      }
      case Memory_Location_Tag_Indirect: {
        if (mod == MOD_Displacement_s32) {
          virtual_memory_buffer_append_s32(buffer, displacement);
        } else if (mod == MOD_Displacement_s8) {
          virtual_memory_buffer_append_s8(buffer, s32_to_s8(displacement));
        } else {
          assert(mod == MOD_Displacement_0);
        }
        break;
      }
      case Memory_Location_Tag_Stack: {
        panic("Stack locations must be resolved beforehand");
      }
    }
  }

  // Write out immediate operand(s?)
  for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];
    if (operand_encoding->type != Operand_Encoding_Type_Immediate) {
      continue;
    }
    Storage *storage = &instruction->Assembly.operands[storage_index];
    if (storage_is_label(storage)) {
      Label_Index label_index = storage->Memory.location.Instruction_Pointer_Relative.label_index;
      s32 *patch_target = virtual_memory_buffer_allocate_unaligned(buffer, s32);
      // :AfterInstructionPatch
      immediate_label_patch_info =
        dyn_array_push(program->patch_info_array, (Label_Location_Diff_Patch_Info) {
          .target_label_index = label_index,
          .from = {.section = &program->memory.code},
          .patch_target = patch_target,
        });
    } else if (storage->tag == Storage_Tag_Static) {
      Slice slice = {
        .bytes = storage_static_as_c_type_internal(storage, storage->byte_size),
        .length = storage->byte_size,
      };
      virtual_memory_buffer_append_slice(buffer, slice);
    } else {
      panic("Unexpected mismatched operand type for immediate encoding.");
    }
  }

  instruction->encoded_byte_size = u64_to_u8(buffer->occupied - original_buffer_length);

  // :AfterInstructionPatch
  u32 next_instruction_offset = u64_to_u32(buffer->occupied);
  if (mod_r_m_patch_info) {
    mod_r_m_patch_info->from.offset_in_section = next_instruction_offset;
  }
  if (immediate_label_patch_info) {
    immediate_label_patch_info->from.offset_in_section = next_instruction_offset;
  }
}

static const Instruction_Encoding *
encoding_match(
  const Instruction *instruction
) {
  assert(instruction->tag == Instruction_Tag_Assembly);
  u32 storage_count = countof(instruction->Assembly.operands);
  for (u32 index = 0; index < instruction->Assembly.mnemonic->encoding_count; ++index) {
    const Instruction_Encoding *encoding = &instruction->Assembly.mnemonic->encoding_list[index];
    for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
      const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];
      const Storage *storage = &instruction->Assembly.operands[storage_index];
      u32 encoding_size = s32_to_u32(operand_encoding->size);

      if (operand_encoding->size != Operand_Size_Any) {
        if (storage->byte_size != encoding_size) {
          encoding = 0;
          break;
        }
      }
      if (
        storage->tag == Storage_Tag_Eflags &&
        operand_encoding->type == Operand_Encoding_Type_Eflags
      ) {
        continue;
      }

      if (
        storage->tag == Storage_Tag_None &&
        operand_encoding->type == Operand_Encoding_Type_None
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Register &&
        storage->Register.index == Register_A &&
        operand_encoding->type == Operand_Encoding_Type_Register_A
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Register &&
        operand_encoding->type == Operand_Encoding_Type_Register
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Register &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Memory &&
        operand_encoding->type == Operand_Encoding_Type_Register_Memory
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Memory &&
        operand_encoding->type == Operand_Encoding_Type_Memory
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Xmm &&
        operand_encoding->type == Operand_Encoding_Type_Xmm
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Xmm &&
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Memory &&
        operand_encoding->type == Operand_Encoding_Type_Xmm_Memory
      ) {
        continue;
      }
      if (operand_encoding->type == Operand_Encoding_Type_Immediate) {
        if (storage->tag == Storage_Tag_Static) {
          assert(encoding_size == storage->byte_size);
          continue;
        } else if (storage_is_label(storage)) {
          assert(encoding_size == Operand_Size_32);
          continue;
        }
      }
      encoding = 0;
      break;
    }
    if (encoding) return encoding;
  }
  return 0;
}

static inline void
push_eagerly_encoded_instruction(
  Array_Instruction *instructions,
  Instruction instruction
) {
  if (instruction.tag == Instruction_Tag_Assembly) {
    if (instruction.Assembly.mnemonic == lea) {
      //DebugBreak();
    }
    const Instruction_Encoding *encoding = encoding_match(&instruction);
    eager_encode_instruction_assembly(instructions, &instruction, encoding);
    return;
  }
  dyn_array_push(*instructions, instruction);
}

void
encode_instruction(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  Instruction *instruction
) {
  switch(instruction->tag) {
    case Instruction_Tag_Label: {
      Label *label = program_get_label(program, instruction->Label.index);
      assert(!label->resolved);
      label->section = &program->memory.code;
      label->offset_in_section = u64_to_u32(buffer->occupied);
      label->resolved = true;
      instruction->encoded_byte_size = 0;
      return;
    }
    case Instruction_Tag_Bytes: {
      Slice slice = {
        .bytes = (char *)instruction->Bytes.memory,
        .length = instruction->Bytes.length,
      };
      instruction->encoded_byte_size = instruction->Bytes.length;
      virtual_memory_buffer_append_slice(buffer, slice);
      return;
    }
    case Instruction_Tag_Label_Patch: {
      Instruction_Label_Patch *label_patch = &instruction->Label_Patch;
      u64 patch_offset_in_buffer = buffer->occupied + label_patch->offset;
      s32 *patch_target = (s32 *)(buffer->memory + patch_offset_in_buffer);
      assert(*patch_target == 0);
      dyn_array_push(program->patch_info_array, (Label_Location_Diff_Patch_Info) {
        .target_label_index = instruction->Label_Patch.label_index,
        .from = {
          .section = &program->memory.code,
          .offset_in_section = u64_to_u32(buffer->occupied),
        },
        .patch_target = patch_target,
      });
      return;
    }
    case Instruction_Tag_Stack_Patch: {
      // Handled in :StackPatch
      return;
    }
    case Instruction_Tag_Assembly: {
      // Handled below
      break;
    }
  }

  u32 storage_count = countof(instruction->Assembly.operands);
  const Instruction_Encoding *encoding = encoding_match(instruction);
  if (encoding) {
    encode_instruction_assembly(program, buffer, instruction, encoding, storage_count);
    return;
  }
  const Compiler_Source_Location *compiler_location = &instruction->compiler_source_location;
  printf(
    "Added in compiler at %s:%"PRIu64" (fn: %s)\n",
    compiler_location->filename,
    compiler_location->line_number,
    compiler_location->function_name
  );
  const Source_Range *source_range = &instruction->source_range;
  printf("Source code at ");
  source_range_print_start_position(source_range);
  printf("%s", instruction->Assembly.mnemonic->name);
  for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
    Storage *storage = &instruction->Assembly.operands[storage_index];
    printf(" ");
    print_storage(storage);
  }
  printf("\n");
  assert(!"Did not find acceptable encoding");
}
