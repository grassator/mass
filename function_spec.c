#include "bdd-for-c.h"

#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

spec("function") {
  static Source_File test_source_file = {
    .path = slice_literal_fields(__FILE__),
    .text = slice_literal_fields(""),
  };
  static Source_Range test_range = {
    .file = &test_source_file,
    .offsets = {0},
  };
  static Function_Builder *builder = 0;
  static Bucket_Buffer *temp_buffer = 0;
  static Allocator *temp_allocator = 0;

  before() {
    builder = malloc(sizeof(Function_Builder));
    *builder = (Function_Builder){
      .code_block.instructions = dyn_array_make(Array_Instruction),
    };
  }

  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_allocator_make(temp_buffer);
    dyn_array_clear(builder->code_block.instructions);
    *builder = (Function_Builder){
      .code_block.instructions = builder->code_block.instructions,
    };
  }

  after_each() {
    bucket_buffer_destroy(temp_buffer);
  }

  describe("move_value") {
    it("should not add any instructions when moving value to itself (pointer equality)") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
      move_value(temp_allocator, builder, &test_range, reg_a, reg_a);
      check(dyn_array_length(builder->code_block.instructions) == 0);
    }
    it("should not add any instructions when moving value to itself (value equality)") {
      Value *reg_a1 = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
      Value *reg_a2 = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
      move_value(temp_allocator, builder, &test_range, reg_a1, reg_a2);
      check(dyn_array_length(builder->code_block.instructions) == 0);
    }
    it("should set the descriptor of `source` type if `target` is Any but still copy") {
      Value *reg_a = &(Value){ .descriptor = &descriptor_any, .operand = al };
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s8);
      move_value(temp_allocator, builder, &test_range, reg_a, reg_b);
      check(reg_a->descriptor == reg_b->descriptor);
      check(dyn_array_length(builder->code_block.instructions) == 1);
    }
    it("should just set the operand of `target` to be the same as operand of `source`") {
      Value *reg_a = &(Value){
        .descriptor = &descriptor_s8,
        .operand = { .tag = Operand_Tag_Any },
      };
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s8);
      move_value(temp_allocator, builder, &test_range, reg_a, reg_b);
      check(operand_equal(&reg_a->operand, &reg_b->operand));
      check(dyn_array_length(builder->code_block.instructions) == 0);
    }
    it("should be use `xor` to clear the register instead of move of 0") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_allocator, 0),
        value_from_s16(temp_allocator, 0),
        value_from_s32(temp_allocator, 0),
        value_from_s64(temp_allocator, 0),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, descriptors[i]);
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates)); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, reg_a, immediates[j]);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction_equal(
            instruction, &(Instruction){.assembly = {xor, reg_a->operand, reg_a->operand}}
          ));
        }
      }
    }
    it("should be able to do a single instruction move for imm to a register") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_allocator, 42),
        value_from_s16(temp_allocator, 4200),
        value_from_s32(temp_allocator, 42000),
        value_from_s64(temp_allocator, 42ll << 32ll),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, descriptors[i]);
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates) - 1); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, reg_a, immediates[j]);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction->assembly.mnemonic == mov);
          check(operand_equal(&instruction->assembly.operands[0], &reg_a->operand));
          s64 actual_immediate = operand_immediate_value_up_to_s64(&instruction->assembly.operands[1]);
          s64 expected_immediate = operand_immediate_value_up_to_s64(&immediates[j]->operand);
          check(actual_immediate == expected_immediate);
        }
      }
    }
    it("should be able to do a single instruction move for non-64bit imm to memory") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_allocator, 42),
        value_from_s16(temp_allocator, 4200),
        value_from_s32(temp_allocator, 42000),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Value *memory = &(Value){descriptors[i], stack(0, descriptor_byte_size(descriptors[i]))};
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates) - 1); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, memory, immediates[j]);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction->assembly.mnemonic == mov);
          check(operand_equal(&instruction->assembly.operands[0], &memory->operand));
          s64 actual_immediate = operand_immediate_value_up_to_s64(&instruction->assembly.operands[1]);
          s64 expected_immediate = operand_immediate_value_up_to_s64(&immediates[j]->operand);
          check(actual_immediate == expected_immediate);
        }
      }
    }
    it("should use a 32bit immediate when the value fits for a move to a 64bit value") {
      Value *memory = &(Value){&descriptor_s64, stack(0, 8)};
      move_value(temp_allocator, builder, &test_range, memory, value_from_s64(temp_allocator, 42000));
      check(dyn_array_length(builder->code_block.instructions) == 1);
      Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
      check(instruction->assembly.mnemonic == mov);
      check(operand_equal(&instruction->assembly.operands[0], &memory->operand));
      check(instruction->assembly.operands[1].byte_size == 4);
    }
    it("should use a temp register for a imm64 to memory move") {
      Value *memory = &(Value){&descriptor_s64, stack(0, 8)};
      Value *immediate = value_from_s64(temp_allocator, 42ll << 32);
      move_value(temp_allocator, builder, &test_range, memory, immediate);
      check(dyn_array_length(builder->code_block.instructions) == 2);
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s64);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, reg_a->operand, immediate->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {mov, memory->operand, reg_a->operand}}
      ));
    }
    it("should use appropriate setCC instruction when moving from eflags") {
      struct { Compare_Type compare_type; const X64_Mnemonic *mnemonic; } tests[] = {
        { Compare_Type_Equal, sete },
        { Compare_Type_Signed_Less, setl },
        { Compare_Type_Signed_Greater_Equal, setge },
      };
      for (u64 i = 0; i < countof(tests); ++i) {
        dyn_array_clear(builder->code_block.instructions);
        Value *memory = &(Value){&descriptor_s8, stack(0, 1)};
        Value *eflags = value_from_compare(temp_allocator, tests[i].compare_type);
        move_value(temp_allocator, builder, &test_range, memory, eflags);
        check(dyn_array_length(builder->code_block.instructions) == 1);
        Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
        check(instruction->assembly.mnemonic == tests[i].mnemonic);
        check(operand_equal(&instruction->assembly.operands[0], &memory->operand));
        check(operand_equal(&instruction->assembly.operands[1], &eflags->operand));
      }
    }
    it("should use a temporary register for setCC to more than a byte") {
      struct { Descriptor *descriptor; } tests[] = {
        { &descriptor_s16 },
        { &descriptor_s32 },
        { &descriptor_s64 },
      };
      for (u64 i = 0; i < countof(tests); ++i) {
        dyn_array_clear(builder->code_block.instructions);
        Value *memory = &(Value){
          tests[i].descriptor,
          stack(0, descriptor_byte_size(tests[i].descriptor))
        };
        Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
        Value *resized_reg_a = value_register_for_descriptor(temp_allocator, Register_A, tests[i].descriptor);
        Value *eflags = value_from_compare(temp_allocator, Compare_Type_Equal);
        move_value(temp_allocator, builder, &test_range, memory, eflags);
        check(dyn_array_length(builder->code_block.instructions) == 3);
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 0),
          &(Instruction){.assembly = {sete, reg_a->operand, eflags->operand}}
        ));
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 1),
          &(Instruction){.assembly = {movsx, resized_reg_a->operand, reg_a->operand}}
        ));
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 2),
          &(Instruction){.assembly = {mov, memory->operand, resized_reg_a->operand}}
        ));
      }
    }
  }
  describe("plus") {
    it("should fold s8 immediates and move them to the result value") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
      plus(temp_allocator, builder, &test_range, reg_a, value_from_s8(temp_allocator, 30), value_from_s8(temp_allocator, 12));
      check(dyn_array_length(builder->code_block.instructions) == 1);
      Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
      check(instruction_equal(
        instruction, &(Instruction){.assembly = {mov, reg_a->operand, imm8(temp_allocator, 42)}}
      ));
    }
    it("should move `a` to result and add `b` to it when result is neither `a` or `b`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(temp_allocator, Register_C, &descriptor_s32);

      plus(temp_allocator, builder, &test_range, reg_a, reg_b, reg_c);
      check(dyn_array_length(builder->code_block.instructions) == 2);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, reg_a->operand, reg_b->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {add, reg_a->operand, reg_c->operand}}
      ));
    }
    it("should avoid moving `a` to result when result is also `a`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);

      plus(temp_allocator, builder, &test_range, reg_a, reg_a, reg_b);
      check(dyn_array_length(builder->code_block.instructions) == 1);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {add, reg_a->operand, reg_b->operand}}
      ));
    }
    it("should flip operands and avoid moving `b` to result when result is also `b`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);

      plus(temp_allocator, builder, &test_range, reg_b, reg_a, reg_b);
      check(dyn_array_length(builder->code_block.instructions) == 1);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {add, reg_b->operand, reg_a->operand}}
      ));
    }
    it("should use the larger operand's size for the result") {
      Value *r32 = value_register_for_descriptor(temp_allocator, register_acquire_temp(builder), &descriptor_s32);
      Value *result_m32 = &(Value){&descriptor_s32, stack(0, 4)};
      Value *m8 = &(Value){&descriptor_s8, stack(0, 1)};
      plus(temp_allocator, builder, &test_range, result_m32, r32, m8);
      check(dyn_array_length(builder->code_block.instructions) == 3);
      Value *temp = value_register_for_descriptor(temp_allocator, register_acquire_temp(builder), &descriptor_s32);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {movsx, temp->operand, m8->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {add, temp->operand, r32->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 2),
        &(Instruction){.assembly = {mov, result_m32->operand, temp->operand}}
      ));
    }
    it("should use a temp register when result is also `a`, but both operands are memory") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *m_a = &(Value){&descriptor_s32, stack(0, 4)};
      Value *m_b = &(Value){&descriptor_s32, stack(4, 4)};

      plus(temp_allocator, builder, &test_range, m_a, m_a, m_b);
      check(dyn_array_length(builder->code_block.instructions) == 3);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, reg_a->operand, m_a->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {add, reg_a->operand, m_b->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 2),
        &(Instruction){.assembly = {mov, m_a->operand, reg_a->operand}}
      ));
    }
  }
  describe("minus") {
    it("should fold s8 immediates and move them to the result value") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s8);
      minus(temp_allocator, builder, &test_range, reg_a, value_from_s8(temp_allocator, 52), value_from_s8(temp_allocator, 10));
      check(dyn_array_length(builder->code_block.instructions) == 1);
      Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
      check(instruction_equal(
        instruction, &(Instruction){.assembly = {mov, reg_a->operand, imm8(temp_allocator, 42)}}
      ));
    }
    it("should move `a` to result and sub `b` from it when result is neither `a` or `b`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(temp_allocator, Register_C, &descriptor_s32);

      minus(temp_allocator, builder, &test_range, reg_a, reg_b, reg_c);
      check(dyn_array_length(builder->code_block.instructions) == 2);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, reg_a->operand, reg_b->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {sub, reg_a->operand, reg_c->operand}}
      ));
    }
    it("should avoid moving `a` to result when result is also `a`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);

      minus(temp_allocator, builder, &test_range, reg_a, reg_a, reg_b);
      check(dyn_array_length(builder->code_block.instructions) == 1);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {sub, reg_a->operand, reg_b->operand}}
      ));
    }
    it("should use a temp A register when result is also `b`") {
      Value *reg_a = value_register_for_descriptor(temp_allocator, Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(temp_allocator, Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(temp_allocator, Register_C, &descriptor_s32);

      minus(temp_allocator, builder, &test_range, reg_c, reg_b, reg_c);
      check(dyn_array_length(builder->code_block.instructions) == 3);
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, reg_a->operand, reg_b->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {sub, reg_a->operand, reg_c->operand}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 2),
        &(Instruction){.assembly = {mov, reg_c->operand, reg_a->operand}}
      ));
    }
  }
}
