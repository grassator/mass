#include "value.h"
#include "program.h"

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

typedef struct {
  bool has_stack_patch;
  u8 label_patch_count;
  Instruction_Bytes bytes;
  Instruction_Stack_Patch maybe_stack_patch;
  Instruction_Label_Patch label_patches[2];
} Eager_Encoding_Result;

// TODO implement this in the userland
static i8
mod_reg_rm(
  Register a,
  Register b
) {
  u8 r_m = (u8)a;
  u8 reg = (u8)b;
  u8 mod = MOD_Register;
  return (i8){(u8)(
    (mod << 6) |
    ((reg & 0b111) << 3) |
    ((r_m & 0b111))
  )};
}

static Eager_Encoding_Result
eager_encode_instruction_assembly(
  const Instruction_Assembly *assembly,
  const Instruction_Encoding *encoding
) {
  Eager_Encoding_Result result = {0};

  s8 mod_r_m_storage_index = -1;
  u8 reg_or_op_code = 0;
  u8 rex_byte = 0;
  bool needs_16_bit_prefix = false;
  u8 r_m = 0;
  u8 mod = MOD_Register;
  u8 op_code[4];
  memcpy(op_code, encoding->op_code, sizeof(op_code));
  bool needs_sib = false;
  u8 sib_byte = 0;
  s32 displacement = 0;

  u32 storage_count = countof(assembly->operands);
  for (u8 storage_index = 0; storage_index < storage_count; ++storage_index) {
    const Storage *storage = &assembly->operands[storage_index];
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];

    if (storage->bit_size.as_u64 == 16) {
      needs_16_bit_prefix = true;
    }

    if (
      storage->bit_size.as_u64 == 64 &&
      operand_encoding->type != Operand_Encoding_Type_Xmm
    ) {
      rex_byte |= REX_W;
    }

    if (storage->tag == Storage_Tag_Register) {
      assert(storage->Register.offset_in_bits == 0);

      if (storage->bit_size.as_u64 == 8) {
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
      reg_or_op_code = storage->Xmm.index;
    }

    if(
      operand_encoding->type == Operand_Encoding_Type_Memory ||
      operand_encoding->type == Operand_Encoding_Type_Register_Or_Memory
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
            if (storage->Memory.location.Instruction_Pointer_Relative.offset) {
              panic("TODO support encoding a relative pointer with an offset");
            }
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
    instruction_bytes_append_bytes(&result.bytes, &(u8){0x66}, 1);
  }

  if (rex_byte) {
    instruction_bytes_append_bytes(&result.bytes, &rex_byte, 1);
  }

  if (op_code[0]) {
    instruction_bytes_append_bytes(&result.bytes, &op_code[0], 1);
  }
  if (op_code[1]) {
    instruction_bytes_append_bytes(&result.bytes, &op_code[1], 1);
  }
  if (op_code[2]) {
    instruction_bytes_append_bytes(&result.bytes, &op_code[2], 1);
  }
  instruction_bytes_append_bytes(&result.bytes, &op_code[3], 1);

  result.maybe_stack_patch = (Instruction_Stack_Patch) {
    .mod_r_m_offset_in_previous_instruction = result.bytes.length,
  };
  if (mod_r_m_storage_index != -1) {
    u8 mod_r_m = (
      (mod << 6) |
      ((reg_or_op_code & 0b111) << 3) |
      ((r_m & 0b111))
    );
    instruction_bytes_append_bytes(&result.bytes, &mod_r_m, 1);
  }

  if (needs_sib) {
    instruction_bytes_append_bytes(&result.bytes, &sib_byte, 1);
  }

  // Write out displacement
  if (mod_r_m_storage_index != -1 && mod != MOD_Register) {
    const Storage *storage = &assembly->operands[mod_r_m_storage_index];
    assert (storage->tag == Storage_Tag_Memory);
    const Memory_Location *location = &storage->Memory.location;
    switch(location->tag) {
      case Memory_Location_Tag_Instruction_Pointer_Relative: {
        Label *label = location->Instruction_Pointer_Relative.label;
        if (storage->Memory.location.Instruction_Pointer_Relative.offset) {
          panic("TODO support encoding a relative pointer with an offset");
        }
        u8 offset_in_instruction = result.bytes.length;
        result.label_patches[result.label_patch_count++] = (Instruction_Label_Patch){
          .offset = offset_in_instruction,
          .label = label
        };
        u8 empty_patch_bytes[4] = {0};
        instruction_bytes_append_bytes(&result.bytes, empty_patch_bytes, countof(empty_patch_bytes));
      } break;
      case Memory_Location_Tag_Indirect: {
        if (mod == MOD_Displacement_s32) {
          instruction_bytes_append_bytes(&result.bytes, (u8 *)&displacement, sizeof(displacement));
        } else if (mod == MOD_Displacement_s8) {
          u8 byte = (u8)s32_to_s8(displacement);
          instruction_bytes_append_bytes(&result.bytes, &byte, sizeof(byte));
        } else {
          assert(mod == MOD_Displacement_0);
        }
      } break;
      case Memory_Location_Tag_Stack: {
        result.maybe_stack_patch.stack_area = location->Stack.area;
        result.has_stack_patch = true;

        // :OversizedStackOffsets
        // Here we reserve full 4 bytes for stack offset but it might be patched
        // to a smaller size when total stack size is known
        s32 stack_offset = location->Stack.offset;
        instruction_bytes_append_bytes(&result.bytes, (u8 *)&stack_offset, sizeof(stack_offset));
      } break;
    }
  }

  // Write out immediate operand(s?)
  for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
    const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];
    if (operand_encoding->type != Operand_Encoding_Type_Immediate) {
      continue;
    }
    const Storage *storage = &assembly->operands[storage_index];
    if (storage_is_label(storage)) {
      Label *label = storage->Memory.location.Instruction_Pointer_Relative.label;
      if (storage->Memory.location.Instruction_Pointer_Relative.offset) {
        panic("TODO support encoding a relative pointer with an offset");
      }
      u8 offset_in_instruction = result.bytes.length;
      result.label_patches[result.label_patch_count++] = (Instruction_Label_Patch){
        .offset = offset_in_instruction,
        .label = label,
      };
      u8 empty_patch_bytes[4] = {0};
      instruction_bytes_append_bytes(&result.bytes, empty_patch_bytes, countof(empty_patch_bytes));
    } else if (storage->tag == Storage_Tag_Immediate) {
      instruction_bytes_append_bytes(&result.bytes, (u8 const *)&storage->Immediate.bits, storage->bit_size.as_u64 / 8);
    } else {
      panic("Unexpected mismatched operand type for immediate encoding.");
    }
  }

  assert(result.label_patch_count <= countof(result.label_patches));
  for (u8 i = 0; i < result.label_patch_count; i += 1) {
    result.label_patches[i].offset -= result.bytes.length;
  }

  return result;
}

static const Instruction_Encoding *
encoding_match(
  const Instruction_Assembly *assembly
) {
  u32 storage_count = countof(assembly->operands);
  for (u32 index = 0; index < assembly->mnemonic->encoding_count; ++index) {
    const Instruction_Encoding *encoding = &assembly->mnemonic->encoding_list[index];
    for (u32 storage_index = 0; storage_index < storage_count; ++storage_index) {
      const Operand_Encoding *operand_encoding = &encoding->operands[storage_index];
      const Storage *storage = &assembly->operands[storage_index];
      u32 storage_bit_size = u64_to_u32(storage->bit_size.as_u64);

      if (storage_bit_size != operand_encoding->bit_size) {
        encoding = 0;
        break;
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
        operand_encoding->type == Operand_Encoding_Type_Register_Or_Memory
      ) {
        continue;
      }
      if (
        storage->tag == Storage_Tag_Memory &&
        operand_encoding->type == Operand_Encoding_Type_Register_Or_Memory
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
      if (operand_encoding->type == Operand_Encoding_Type_Immediate) {
        if (storage->tag == Storage_Tag_Immediate) {
          assert(operand_encoding->bit_size == storage_bit_size);
          continue;
        } else if (storage_is_label(storage)) {
          assert(operand_encoding->bit_size == 32);
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
encode_and_write_assembly(
  Virtual_Memory_Buffer *buffer,
  const Instruction_Assembly *assembly
) {
  const Instruction_Encoding *encoding = encoding_match(assembly);
  Eager_Encoding_Result result =
    eager_encode_instruction_assembly(assembly, encoding);
  assert(!result.has_stack_patch);
  assert(!result.label_patch_count);
  Slice bytes = {.bytes = (char *)result.bytes.memory, .length = result.bytes.length};
  virtual_memory_buffer_append_slice(buffer, bytes);
}

static inline void
push_instruction(
  Code_Block *code_block,
  Instruction instruction
) {
  if (!code_block->first_bucket) {
    code_block->first_bucket =
      allocator_allocate(code_block->allocator, Instruction_Bucket);
    code_block->last_bucket = code_block->first_bucket;
  }
  if (code_block->last_bucket->length >= countof(code_block->last_bucket->items)) {
    Instruction_Bucket *next =
      allocator_allocate(code_block->allocator, Instruction_Bucket);
    code_block->last_bucket->next = next;
    code_block->last_bucket = next;
  }
  code_block->last_bucket->items[code_block->last_bucket->length++] = instruction;
}

static inline void
push_eagerly_encoded_assembly_no_source_range(
  Code_Block *code_block,
  Source_Range source_range,
  const Instruction_Assembly *assembly
) {
  const Instruction_Encoding *encoding = encoding_match(assembly);
  if (!encoding) panic("Did not find acceptable encoding");
  Eager_Encoding_Result result = eager_encode_instruction_assembly(assembly, encoding);

  push_instruction(code_block, (Instruction) {
    .tag = Instruction_Tag_Bytes,
    .Bytes = result.bytes,
  });

  // Stack patch MUST go before label patches as it might change the size of the instruction
  if (result.has_stack_patch) {
    push_instruction(code_block, (Instruction) {
      .tag = Instruction_Tag_Stack_Patch,
      .Stack_Patch = result.maybe_stack_patch,
    });
  }

  for (s32 i = 0; i < result.label_patch_count; i += 1) {
    push_instruction(code_block, (Instruction) {
      .tag = Instruction_Tag_Label_Patch,
      .Label_Patch = result.label_patches[i],
    });
  }
}

static inline void
push_eagerly_encoded_assembly(
  Code_Block *code_block,
  Source_Range source_range,
  const Instruction_Assembly *assembly
) {
  push_instruction(code_block, (Instruction) {
    .tag = Instruction_Tag_Location,
    .Location = { .source_range = source_range },
  });

  push_eagerly_encoded_assembly_no_source_range(code_block, source_range, assembly);
}

static void
encode_instruction(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  Instruction *instruction
) {
  switch(instruction->tag) {
    case Instruction_Tag_Label: {
      program_resolve_label(program, buffer, instruction->Label.pointer);
      return;
    }
    case Instruction_Tag_Bytes: {
      Slice slice = {
        .bytes = (char *)instruction->Bytes.memory,
        .length = instruction->Bytes.length,
      };
      virtual_memory_buffer_append_slice(buffer, slice);
      return;
    }
    case Instruction_Tag_Label_Patch: {
      Instruction_Label_Patch *label_patch = &instruction->Label_Patch;
      u64 patch_offset_in_buffer = buffer->occupied + label_patch->offset;
      void *patch32_at = buffer->memory + patch_offset_in_buffer;
      assert(memcmp(patch32_at, &(u32){0}, sizeof(u32)) == 0);
      dyn_array_push(program->patch_info_array, (Label_Location_Diff_Patch_Info) {
        .target = instruction->Label_Patch.label,
        .from = {
          .section = &program->memory.code,
          .offset_in_section = u64_to_u32(buffer->occupied),
        },
        .patch32_at = patch32_at,
      });
      return;
    }
    case Instruction_Tag_Stack_Patch: {
      // Handled in :StackPatch
      return;
    }
    case Instruction_Tag_Location: {
      // Nothing to do
      return;
    }
  }
  panic("Unexpected instruction tag");
}
