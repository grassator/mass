#include "bdd-for-c.h"

#include "value.c"
#include "instruction.c"
#include "encoding.c"
#include "function.c"
#include "source.c"

spec("function") {
  static Array_Instruction instructions = {0};

  before() {
    instructions = dyn_array_make(Array_Instruction);
  }

  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_allocator_make(temp_buffer);
    dyn_array_clear(instructions);
  }

  after_each() {
    bucket_buffer_destroy(temp_buffer);
  }

  describe("move_value") {
    it("should not add any instructions when moving value to itself (pointer equality)") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s8);
      move_value(&instructions, reg_a, reg_a);
      check(dyn_array_length(instructions) == 0);
    }
    it("should not add any instructions when moving value to itself (value equality)") {
      Value *reg_a1 = value_register_for_descriptor(Register_A, &descriptor_s8);
      Value *reg_a2 = value_register_for_descriptor(Register_A, &descriptor_s8);
      move_value(&instructions, reg_a1, reg_a2);
      check(dyn_array_length(instructions) == 0);
    }
  }
  describe("plus") {
    it("should fold s8 immediates and move them to the result value") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s8);
      plus(&instructions, reg_a, value_from_s8(30), value_from_s8(12));
      check(dyn_array_length(instructions) == 1);
      Instruction *instruction = dyn_array_get(instructions, 0);
      check(instruction_equal(
        instruction, &(Instruction){mov, reg_a->operand, imm8(42)}
      ));
    }
    it("should move `a` to result and add `b` to it when result is neither `a` or `b`") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(Register_C, &descriptor_s32);

      plus(&instructions, reg_a, reg_b, reg_c);
      check(dyn_array_length(instructions) == 2);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){mov, reg_a->operand, reg_b->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 1), &(Instruction){add, reg_a->operand, reg_c->operand}
      ));
    }
    it("should avoid moving `a` to result when result is also `a`") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);

      plus(&instructions, reg_a, reg_a, reg_b);
      check(dyn_array_length(instructions) == 1);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){add, reg_a->operand, reg_b->operand}
      ));
    }
    it("should flip operands and avoid moving `b` to result when result is also `b`") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);

      plus(&instructions, reg_b, reg_a, reg_b);
      check(dyn_array_length(instructions) == 1);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){add, reg_b->operand, reg_a->operand}
      ));
    }
    it("should use a temp register when result is also `a`, but both operands are memory") {
      Value *reg_r11 = value_register_for_descriptor(Register_R11, &descriptor_s32);
      Value *m_a = &(Value){&descriptor_s32, stack(0, 4)};
      Value *m_b = &(Value){&descriptor_s32, stack(4, 4)};

      plus(&instructions, m_a, m_a, m_b);
      check(dyn_array_length(instructions) == 3);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){mov, reg_r11->operand, m_a->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 1), &(Instruction){add, reg_r11->operand, m_b->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 2), &(Instruction){mov, m_a->operand, reg_r11->operand}
      ));
    }
  }
  describe("minus") {
    it("should fold s8 immediates and move them to the result value") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s8);
      minus(&instructions, reg_a, value_from_s8(52), value_from_s8(10));
      check(dyn_array_length(instructions) == 1);
      Instruction *instruction = dyn_array_get(instructions, 0);
      check(instruction_equal(
        instruction, &(Instruction){mov, reg_a->operand, imm8(42)}
      ));
    }
    it("should move `a` to result and sub `b` from it when result is neither `a` or `b`") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(Register_C, &descriptor_s32);

      minus(&instructions, reg_a, reg_b, reg_c);
      check(dyn_array_length(instructions) == 2);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){mov, reg_a->operand, reg_b->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 1), &(Instruction){sub, reg_a->operand, reg_c->operand}
      ));
    }
    it("should avoid moving `a` to result when result is also `a`") {
      Value *reg_a = value_register_for_descriptor(Register_A, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);

      minus(&instructions, reg_a, reg_a, reg_b);
      check(dyn_array_length(instructions) == 1);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){sub, reg_a->operand, reg_b->operand}
      ));
    }
    it("should use a temp A register when result is also `b`") {
      Value *reg_r11 = value_register_for_descriptor(Register_R11, &descriptor_s32);
      Value *reg_b = value_register_for_descriptor(Register_B, &descriptor_s32);
      Value *reg_c = value_register_for_descriptor(Register_C, &descriptor_s32);

      minus(&instructions, reg_c, reg_b, reg_c);
      check(dyn_array_length(instructions) == 3);
      check(instruction_equal(
        dyn_array_get(instructions, 0), &(Instruction){mov, reg_r11->operand, reg_b->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 1), &(Instruction){sub, reg_r11->operand, reg_c->operand}
      ));
      check(instruction_equal(
        dyn_array_get(instructions, 2), &(Instruction){mov, reg_c->operand, reg_r11->operand}
      ));
    }
  }
}
