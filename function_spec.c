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
  static Execution_Context *temp_context = 0;

  before() {
    builder = malloc(sizeof(Function_Builder));
    *builder = (Function_Builder){
      .code_block.instructions = dyn_array_make(Array_Instruction),
    };
  }

  before_each() {
    temp_buffer = bucket_buffer_make(.allocator = allocator_system);
    temp_allocator = bucket_buffer_allocator_make(temp_buffer);
    temp_context = allocator_allocate(temp_allocator, Execution_Context);
    dyn_array_clear(builder->code_block.instructions);
    *builder = (Function_Builder){
      .code_block.instructions = builder->code_block.instructions,
    };
    Mass_Result *result = allocator_allocate(temp_allocator, Mass_Result);
    *result = (Mass_Result){0};
    *temp_context = (Execution_Context) {
      .allocator = temp_allocator,
      .builder = builder,
      .result = result,
    };
  }

  after_each() {
    bucket_buffer_destroy(temp_buffer);
  }

  describe("stack") {
    it("should correctly align allocated values") {
      Value *a = reserve_stack(temp_context, builder, &descriptor_s8, (Source_Range){0});
      check(a->storage.tag == Storage_Tag_Memory);
      check(a->storage.Memory.location.tag == Memory_Location_Tag_Indirect);
      check(a->storage.Memory.location.Indirect.offset == -1);
      Value *b = reserve_stack(temp_context, builder, &descriptor_s32, (Source_Range){0});
      check(b->storage.tag == Storage_Tag_Memory);
      check(b->storage.Memory.location.tag == Memory_Location_Tag_Indirect);
      check(b->storage.Memory.location.Indirect.offset == -8);
    }
  }

  describe("move_value") {
    it("should not add any instructions when moving value to itself (pointer equality)") {
      Value *reg_a = value_register_for_descriptor(temp_context, Register_A, &descriptor_s8, (Source_Range){0});
      move_value(temp_allocator, builder, &test_range, &reg_a->storage, &reg_a->storage);
      check(dyn_array_length(builder->code_block.instructions) == 0);
    }
    it("should not add any instructions when moving value to itself (value equality)") {
      Value *reg_a1 = value_register_for_descriptor(temp_context, Register_A, &descriptor_s8, (Source_Range){0});
      Value *reg_a2 = value_register_for_descriptor(temp_context, Register_A, &descriptor_s8, (Source_Range){0});
      move_value(temp_allocator, builder, &test_range, &reg_a1->storage, &reg_a2->storage);
      check(dyn_array_length(builder->code_block.instructions) == 0);
    }
    it("should be use `xor` to clear the register instead of move of 0") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_context, 0, (Source_Range){0}),
        value_from_s16(temp_context, 0, (Source_Range){0}),
        value_from_s32(temp_context, 0, (Source_Range){0}),
        value_from_s64(temp_context, 0, (Source_Range){0}),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Storage reg_a = storage_register_for_descriptor(Register_A, descriptors[i]);
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates)); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, &reg_a, &immediates[j]->storage);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction_equal(
            instruction, &(Instruction){.assembly = {xor, reg_a, reg_a}}
          ));
        }
      }
    }
    it("should be able to do a single instruction move for imm to a register") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_context, 42, (Source_Range){0}),
        value_from_s16(temp_context, 4200, (Source_Range){0}),
        value_from_s32(temp_context, 42000, (Source_Range){0}),
        value_from_s64(temp_context, 42ll << 32ll, (Source_Range){0}),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Value *reg_a = value_register_for_descriptor(temp_context, Register_A, descriptors[i], (Source_Range){0});
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates) - 1); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, &reg_a->storage, &immediates[j]->storage);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction->assembly.mnemonic == mov);
          check(storage_equal(&instruction->assembly.operands[0], &reg_a->storage));
          s64 actual_immediate = storage_static_value_up_to_s64(&instruction->assembly.operands[1]);
          s64 expected_immediate = storage_static_value_up_to_s64(&immediates[j]->storage);
          check(actual_immediate == expected_immediate);
        }
      }
    }
    it("should be able to do a single instruction move for non-64bit imm to memory") {
      Descriptor *descriptors[] = {
        &descriptor_s8, &descriptor_s16, &descriptor_s32, &descriptor_s64,
      };
      Value *immediates[] = {
        value_from_s8(temp_context, 42, (Source_Range){0}),
        value_from_s16(temp_context, 4200, (Source_Range){0}),
        value_from_s32(temp_context, 42000, (Source_Range){0}),
      };
      for (u64 i = 0; i < countof(descriptors); ++i) {
        Value *memory = &(Value){descriptors[i], stack(0, descriptor_byte_size(descriptors[i]))};
        // We end at the current descriptor index to make sure we do not
        // try to move larger immediate into a smaller register
        for (u64 j = 0; j <= u64_min(i, countof(immediates) - 1); ++j) {
          dyn_array_clear(builder->code_block.instructions);
          move_value(temp_allocator, builder, &test_range, &memory->storage, &immediates[j]->storage);
          check(dyn_array_length(builder->code_block.instructions) == 1);
          Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
          check(instruction->assembly.mnemonic == mov);
          check(storage_equal(&instruction->assembly.operands[0], &memory->storage));
          s64 actual_immediate = storage_static_value_up_to_s64(&instruction->assembly.operands[1]);
          s64 expected_immediate = storage_static_value_up_to_s64(&immediates[j]->storage);
          check(actual_immediate == expected_immediate);
        }
      }
    }
    it("should use a 32bit immediate when the value fits for a move to a 64bit value") {
      Value *memory = &(Value){&descriptor_s64, stack(0, 8)};
      Storage immediate = imm64(42000);
      move_value(temp_allocator, builder, &test_range, &memory->storage, &immediate);
      check(dyn_array_length(builder->code_block.instructions) == 1);
      Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
      check(instruction->assembly.mnemonic == mov);
      check(storage_equal(&instruction->assembly.operands[0], &memory->storage));
      check(instruction->assembly.operands[1].byte_size == 4);
    }
    it("should use a temp register for a imm64 to memory move") {
      Value *memory = &(Value){&descriptor_s64, stack(0, 8)};
      Value *immediate = value_from_s64(temp_context, 42ll << 32, (Source_Range){0});
      move_value(temp_allocator, builder, &test_range, &memory->storage, &immediate->storage);
      check(dyn_array_length(builder->code_block.instructions) == 2);
      Value *temp_reg = value_register_for_descriptor(temp_context, Register_C, &descriptor_s64, (Source_Range){0});
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 0),
        &(Instruction){.assembly = {mov, temp_reg->storage, immediate->storage}}
      ));
      check(instruction_equal(
        dyn_array_get(builder->code_block.instructions, 1),
        &(Instruction){.assembly = {mov, memory->storage, temp_reg->storage}}
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
        Value *eflags = value_from_compare(temp_context, tests[i].compare_type, (Source_Range){0});
        move_value(temp_allocator, builder, &test_range, &memory->storage, &eflags->storage);
        check(dyn_array_length(builder->code_block.instructions) == 1);
        Instruction *instruction = dyn_array_get(builder->code_block.instructions, 0);
        check(instruction->assembly.mnemonic == tests[i].mnemonic);
        check(storage_equal(&instruction->assembly.operands[0], &memory->storage));
        check(storage_equal(&instruction->assembly.operands[1], &eflags->storage));
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
        Value *temp_reg = value_register_for_descriptor(temp_context, Register_C, &descriptor_s8, (Source_Range){0});
        Value *resized_temp_reg = value_register_for_descriptor(temp_context, Register_C, tests[i].descriptor, (Source_Range){0});
        Value *eflags = value_from_compare(temp_context, Compare_Type_Equal, (Source_Range){0});
        move_value(temp_allocator, builder, &test_range, &memory->storage, &eflags->storage);
        check(dyn_array_length(builder->code_block.instructions) == 3);
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 0),
          &(Instruction){.assembly = {sete, temp_reg->storage, eflags->storage}}
        ));
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 1),
          &(Instruction){.assembly = {movsx, resized_temp_reg->storage, temp_reg->storage}}
        ));
        check(instruction_equal(
          dyn_array_get(builder->code_block.instructions, 2),
          &(Instruction){.assembly = {mov, memory->storage, resized_temp_reg->storage}}
        ));
      }
    }
  }
}
