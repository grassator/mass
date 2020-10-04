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
}
