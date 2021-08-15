#include "prelude.h"
#include "source.h"
#include "function.h"

#include "generated_exports.c"

static inline Value *
value_from_exact_expected_result(
  const Expected_Result *expected_result
) {
  assert(expected_result->tag == Expected_Result_Tag_Exact);
  return expected_result->Exact.value;
}


static inline const Descriptor *
expected_result_descriptor(
  const Expected_Result *expected_result
) {
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: return expected_result->Exact.value->descriptor;
    case Expected_Result_Tag_Flexible: return expected_result->Flexible.descriptor;
  }
  panic("Unknown Expected_Result tag");
  return 0;
}

static inline Expected_Result
expected_result_from_value(
  Value *value
) {
  return (Expected_Result) {
    .tag = Expected_Result_Tag_Exact,
    .Exact = { .value = value },
  };
}

static inline Expected_Result
expected_result_any(
  const Descriptor *descriptor
) {
  return (Expected_Result){
    .tag = Expected_Result_Tag_Flexible,
    .Flexible = {
      .descriptor = descriptor,
      .storage
        = Expected_Result_Storage_Static
        | Expected_Result_Storage_Memory
        | Expected_Result_Storage_Unpacked
        | Expected_Result_Storage_Register
        | Expected_Result_Storage_Xmm
        | Expected_Result_Storage_Eflags,
      .register_bitset = 0xffffffffffffffffllu,
    },
  };
}

static inline Expected_Result
expected_result_static(
  const Descriptor *descriptor
) {
  return (Expected_Result){
    .tag = Expected_Result_Tag_Flexible,
    .Flexible = {
      .descriptor = descriptor,
      .storage = Expected_Result_Storage_Static
    },
  };
}

static Value *
expected_result_validate(
  const Expected_Result *expected_result,
  Value *actual_value
) {
  if (!actual_value) return 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Value *expected_value = expected_result->Exact.value;
      assert(same_value_type(expected_value, actual_value));
      assert(storage_equal(&expected_value->storage, &actual_value->storage));
      break;
    }
    case Expected_Result_Tag_Flexible: {
      const Expected_Result_Flexible *flexible = &expected_result->Flexible;
      if (flexible->descriptor) {
        assert(same_type(flexible->descriptor, actual_value->descriptor));
      }
      switch(actual_value->storage.tag) {
        case Storage_Tag_None: break;
        case Storage_Tag_Any: break;
        case Storage_Tag_Eflags: {
          assert(flexible->storage & Expected_Result_Storage_Eflags);
          break;
        }
        case Storage_Tag_Register: {
          assert(flexible->storage & Expected_Result_Storage_Register);
          break;
        }
        case Storage_Tag_Xmm: {
          assert(flexible->storage & Expected_Result_Storage_Xmm);
          break;
        }
        case Storage_Tag_Static: {
          assert(flexible->storage & Expected_Result_Storage_Static);
          break;
        }
        case Storage_Tag_Memory: {
          assert(flexible->storage & Expected_Result_Storage_Memory);
          break;
        }
        case Storage_Tag_Unpacked: {
          assert(flexible->storage & Expected_Result_Storage_Unpacked);
          break;
        }
        default: {
          panic("Unknown Storage tag");
          break;
        }
      }
      break;
    }
  }
  return actual_value;
}

static inline Scope *
scope_make(
  const Allocator *allocator,
  const Scope *parent
) {
  static Atomic_u64 next_id = {0};
  u64 id = atomic_u64_increment(&next_id);

  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .id = id,
    .allocator = allocator,
    .parent = parent,
    .map = 0,
  };
  return scope;
}

static inline const Scope *
scope_maybe_find_common_ancestor(
  const Scope *a,
  const Scope *b
) {
  while (a && b) {
    if (a->id > b->id) a = a->parent;
    else if (b->id > a->id) b = b->parent;
    else return a;
  }
  return 0;
}

static inline u64
token_statement_matcher_in_scopes(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  const Scope *scope,
  u64 common_ancestor_id
) {
  for (; scope && scope->id != common_ancestor_id; scope = scope->parent) {
    const Scope_Using *using = scope->maybe_using;
    for (; using; using = using->next) {
      u64 match_length = token_statement_matcher_in_scopes(
        context, view, out_lazy_value, using->scope, using->common_ancestor_id
      );
      if (match_length) return match_length;
    }
    // Do a reverse iteration because we want statements that are defined later
    // to have higher precedence when parsing
    const Token_Statement_Matcher *matcher = scope->statement_matcher;
    for (; matcher; matcher = matcher->previous) {
      u64 match_length = matcher->proc(context, view, out_lazy_value, matcher->payload);
      MASS_ON_ERROR(*context->result) return 0;
      if (match_length) return match_length;
    }
  }
  return 0;
}

static void
use_scope(
  Execution_Context *context,
  Scope *scope_to_use
) {
  // This code injects a proxy scope that just uses the same data as the other
  const Scope *common_ancestor = scope_maybe_find_common_ancestor(context->scope, scope_to_use);
  assert(common_ancestor);

  Scope_Using *using_entry = allocator_allocate(context->allocator, Scope_Using);
  *using_entry = (Scope_Using) {
    .scope = scope_to_use,
    .common_ancestor_id = common_ancestor->id,
    .next = context->scope->maybe_using,
  };
  context->scope->maybe_using = using_entry;
  Scope *new_scope = scope_make(context->allocator, context->scope);

  // FIXME This kind of hackish, probably there is a better way
  if (context->module && context->module->own_scope == context->scope) {
    context->module->own_scope = new_scope;
  }
  // FIXME introduce a more generic mechanism for the statements to introduce a new scope
  context->scope = new_scope;
}

static void
scope_print_names(
  const Scope *scope
) {
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    for (u64 i = 0; i < scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &scope->map->entries[i];
      if (entry->occupied) {
        slice_print(entry->key);
        printf(" ; ");
      }
    }
  }
  printf("\n");
}

static inline Scope_Entry *
scope_lookup_shallow_hashed(
  const Scope *scope,
  s32 hash,
  Slice name
) {
  if (!scope->map) return 0;
  Scope_Entry **entry_pointer = Scope_Map__get_by_hash(scope->map, hash, name);
  if (!entry_pointer) return 0;
  return *entry_pointer;
}


static inline Scope_Entry *
scope_lookup_shallow(
  const Scope *scope,
  Slice name
) {
  s32 hash = Scope_Map__hash(name);
  return scope_lookup_shallow_hashed(scope, hash, name);
}

static inline Scope_Entry *
scope_lookup_hashed(
  const Scope *scope,
  s32 hash,
  Slice name,
  u64 till_id
) {
  for (; scope && scope->id != till_id; scope = scope->parent) {
    const Scope_Using *using = scope->maybe_using;
    for (; using; using = using->next) {
      Scope_Entry *entry = scope_lookup_hashed(using->scope, hash, name, using->common_ancestor_id);
      if (entry) return entry;
    }
    Scope_Entry *entry = scope_lookup_shallow_hashed(scope, hash, name);
    if (entry) return entry;
  }
  return 0;
}

static inline Scope_Entry *
scope_lookup(
  const Scope *scope,
  Slice name
) {
  s32 hash = Scope_Map__hash(name);
  return scope_lookup_hashed(scope, hash, name, 0);
}

static Value *
token_value_force_immediate_integer(
  Execution_Context *context,
  Value *value,
  const Descriptor *target_descriptor
) {
  MASS_ON_ERROR(*context->result) return 0;
  const Source_Range *source_range = &value->source_range;

  assert(descriptor_is_integer(target_descriptor));
  if (value_is_static_number_literal(value)) {
    u64 bits = 0xCCccCCccCCccCCcc;
    u64 bit_size = 0xCCccCCccCCccCCcc;
    Literal_Cast_Result cast_result =
      value_number_literal_cast_to(value, target_descriptor, &bits, &bit_size);
    switch(cast_result) {
      case Literal_Cast_Result_Success: {
        // Always copy full value. Truncation is handled by the byte_size of the immediate
        u64 byte_size = u64_to_u32(bit_size / 8);
        return value_init(
          allocator_allocate(context->allocator, Value),
          target_descriptor,
          storage_static_internal(&bits, byte_size),
          value->source_range
        );
      }
      case Literal_Cast_Result_Target_Not_An_Integer: {
        panic("We already checked that target is an integer");
        return 0;
      }
      case Literal_Cast_Result_Target_Too_Small: {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Integer_Range,
          .source_range = *source_range,
          .Integer_Range = { .descriptor = target_descriptor },
          .detailed_message = "Literal value does not fit into the target integer size",
        });
        return 0;
      }
      case Literal_Cast_Result_Target_Too_Big: {
        panic("Integers larger than 64 bits are not supported");
        return 0;
      }
      case Literal_Cast_Result_Unsigned_Target_For_Negative_Literal: {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Integer_Range,
          .source_range = *source_range,
          .Integer_Range = { .descriptor = target_descriptor },
          .detailed_message = "Can not convert a negative literal to an unsigned number",
        });
        return 0;
      }
    }
    panic("Unexpected literal cast result");
  }
  if (!same_value_type_or_can_implicitly_move_cast(target_descriptor, value)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = target_descriptor, .actual = value->descriptor },
    });
    return 0;
  }

  return value;
}

static inline Value *
maybe_coerce_number_literal_to_integer(
  Execution_Context *context,
  Value *value,
  const Descriptor *target_descriptor
) {
  if (!descriptor_is_integer(target_descriptor)) return value;
  if (value->descriptor != &descriptor_number_literal) return value;
  return token_value_force_immediate_integer(context, value, target_descriptor);
}

static bool
assign_from_static(
  Execution_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
) {
  Source_Range source_range = target->source_range;
  if (
    !context_is_compile_time_eval(context) &&
    (
      source->descriptor->tag == Descriptor_Tag_Pointer_To ||
      source->descriptor->tag == Descriptor_Tag_Reference_To
    )
  ) {
    // If a static value contains a pointer, we expect an entry in a special map used to track
    // whether the target memory is also already copied to the compiled binary.
    // This is done to only include static values actually used at runtime.
    void *source_memory = *storage_static_as_c_type(&source->storage, void *);
    Value *static_pointer = hash_map_get(context->compilation->static_pointer_map, source_memory);
    assert(static_pointer);
    if (static_pointer->storage.tag == Storage_Tag_None) {
      // TODO should depend on constness of the static value I guess?
      //      Do not forget to make memory readable for ro_dar
      Section *section = &context->program->memory.rw_data;
      u64 byte_size = descriptor_byte_size(static_pointer->descriptor);
      u64 alignment = descriptor_byte_alignment(static_pointer->descriptor);

      // TODO this should also be deduped
      Label_Index label_index = allocate_section_memory(context->program, section, byte_size, alignment);
      static_pointer->storage = data_label32(label_index, byte_size);

      Value static_source_value = {
        .descriptor = static_pointer->descriptor,
        .storage = storage_static_heap(source_memory, byte_size),
        .source_range = source->source_range,
      };

      // It is important to call assign here to make sure we recursively handle
      // any complex types such as structs and arrays
      MASS_ON_ERROR(assign(context, builder, static_pointer, &static_source_value)) return true;
    }
    assert(storage_is_label(&static_pointer->storage));
    if (storage_is_label(&target->storage)) {
      dyn_array_push(context->program->relocations, (Relocation) {
        .patch_at = target->storage,
        .address_of = static_pointer->storage,
      });
    } else {
      load_address(context, builder, &source_range, target, static_pointer->storage);
    }
    return true;
  } else if (storage_is_label(&target->storage)) {
    Label_Index label_index =
      target->storage.Memory.location.Instruction_Pointer_Relative.label_index;
    void *section_memory = rip_value_pointer_from_label_index(context->program, label_index);
    u64 byte_size = source->storage.byte_size;
    const void *source_memory = storage_static_as_c_type_internal(&source->storage, byte_size);
    memcpy(section_memory, source_memory, byte_size);
    return true;
  }
  return false;
}

static PRELUDE_NO_DISCARD Value *
value_indirect_from_reference(
  Execution_Context *context,
  Function_Builder *builder,
  Value *source
) {
  assert(source->descriptor->tag == Descriptor_Tag_Reference_To);
  const Descriptor *referenced_descriptor = source->descriptor->Reference_To.descriptor;
  u64 byte_size = descriptor_byte_size(referenced_descriptor);
  switch(source->storage.tag) {
    case Storage_Tag_Register: {
      Register reg = source->storage.Register.index;
      Storage referenced_storage = storage_indirect(byte_size, reg);
      return value_make(
        context, referenced_descriptor, referenced_storage, source->source_range
      );
    }
    case Storage_Tag_Memory: {
      Register reg = register_find_available(builder, 0);
      Value *temp = value_temporary_acquire_indirect_for_descriptor(
        context, builder, reg, referenced_descriptor, source->source_range
      );
      Storage reg_storage = storage_register_for_descriptor(reg, source->descriptor);
      move_value(context->allocator, builder, &source->source_range, &reg_storage, &source->storage);
      return temp;
    }
    default:
    case Storage_Tag_Unpacked:
    case Storage_Tag_Static:
    case Storage_Tag_None:
    case Storage_Tag_Any:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm:{
      panic("Unexpected storage for a reference");
      return 0;
    }
  }
}


static inline const Descriptor *
signed_integer_next_size_descriptor(
  const Descriptor *descriptor
) {
  assert(descriptor_is_signed_integer(descriptor));
  assert(descriptor->tag == Descriptor_Tag_Opaque);
  if (descriptor == &descriptor_s8) {
    return &descriptor_s16;
  } else if (descriptor == &descriptor_s16) {
    return &descriptor_s32;
  } else if (descriptor == &descriptor_s32) {
    return &descriptor_s64;
  } else {
    panic("Unexpected iteger size");
    return 0;
  }
}

static const Descriptor *
large_enough_common_integer_descriptor_for_values(
  Execution_Context *context,
  const Value *left_value,
  const Value *right_value
) {
  const Descriptor *left = value_or_lazy_value_descriptor(left_value);
  const Descriptor *right = value_or_lazy_value_descriptor(right_value);

  bool left_is_integer = descriptor_is_integer(left);
  bool right_is_integer = descriptor_is_integer(right);

  bool left_is_literal = value_is_static_number_literal(left_value);
  bool right_is_literal = value_is_static_number_literal(right_value);

  if (!left_is_integer && !left_is_literal) {
    // TODO :GenericIntegerType
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = left_value->source_range,
      .Type_Mismatch = { .expected = &descriptor_s64, .actual = left },
    });
    return 0;
  }
  if (!right_is_integer && !right_is_literal) {
    // TODO :GenericIntegerType
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = right_value->source_range,
      .Type_Mismatch = { .expected = &descriptor_s64, .actual = right },
    });
    return 0;
  }

  if (left_is_literal) {
    if (right_is_literal) {
      // TODO consider if this should support large unsigned numbers
      return &descriptor_s64;
    } else {
      return right;
    }
  } else {
    if (right_is_literal) {
      return left;
    }
  }

  bool left_signed = descriptor_is_signed_integer(left);
  bool right_signed = descriptor_is_signed_integer(right);

  u64 left_size = descriptor_byte_size(left);
  u64 right_size = descriptor_byte_size(right);

  if (left_signed == right_signed) {
    if (left_size == right_size) return left;
    return left_size > right_size ? left : right;
  } else {
    const Descriptor *signed_side = left_signed ? left : right;
    const Descriptor *other_side = left_signed ? right : left;
    // If the signed and unsigned have the same size need to
    // increase the size of the signed one so it fits the unsigned
    if (left_size == right_size) {
      if (left_size == 8) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = left_value->source_range,
          .Type_Mismatch = { .expected = signed_side, .actual = other_side },
          .detailed_message = "Could not find large enough signed integer to fit both operands"
        });
        return 0;
      }
      return signed_integer_next_size_descriptor(signed_side);
    }

    // Now we know that the signed operand is large enough to fit the unsigned one
    return signed_side;
  }
}

static PRELUDE_NO_DISCARD Mass_Result
assign_integers(
  Execution_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
) {
  const Descriptor *descriptor =
    large_enough_common_integer_descriptor_for_values(context, source, target);
  MASS_TRY(*context->result);

  if (descriptor_byte_size(target->descriptor) < descriptor_byte_size(descriptor)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = target->source_range,
      .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
      .detailed_message = "Target integer is too small to fit the range of source values"
    });
    return *context->result;
  }
  bool is_temp = false;
  Storage adjusted_source;
  if (
    descriptor_byte_size(source->descriptor) == descriptor_byte_size(target->descriptor) ||
    source->storage.tag == Storage_Tag_Static
  ) {
    adjusted_source = source->storage;
  } else {
    if (source->storage.tag == Storage_Tag_Register) {
      adjusted_source = source->storage;
    } else {
      is_temp = true;
      Register reg = register_acquire_temp(builder);
      adjusted_source = storage_register_for_descriptor(reg, target->descriptor);
    }

    if (descriptor_is_signed_integer(source->descriptor)) {
      assert(!descriptor_is_unsigned_integer(target->descriptor));
      push_eagerly_encoded_assembly(
        &builder->code_block, source->source_range,
        &(Instruction_Assembly){movsx, {adjusted_source, source->storage}}
      );
    } else {
      push_eagerly_encoded_assembly(
        &builder->code_block, source->source_range,
        &(Instruction_Assembly){movzx, {adjusted_source, source->storage}}
      );
    }
  }
  move_value(context->allocator, builder, &target->source_range, &target->storage, &adjusted_source);

  if (is_temp) {
    assert(adjusted_source.tag == Storage_Tag_Register);
    register_release(builder, adjusted_source.Register.index);
  }

  return *context->result;
}

static PRELUDE_NO_DISCARD Mass_Result
assign_tuple(
  Execution_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
) {
  assert(source->descriptor == &descriptor_tuple);
  const Tuple *tuple = storage_static_as_c_type(&source->storage, Tuple);
  if (target->descriptor->tag != Descriptor_Tag_Struct) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = target->source_range,
      .Type_Mismatch = { .expected = &descriptor_s64, .actual = source->descriptor },
      .detailed_message = slice_literal("Trying to assign a tuple to something that is not a struct"),
    });
    return *context->result;
  }
  const Memory_Layout *layout = &target->descriptor->Struct.memory_layout;
  if ((dyn_array_length(layout->items) != dyn_array_length(tuple->items))) {
    Slice message = dyn_array_length(layout->items) > dyn_array_length(tuple->items)
      ? slice_literal("Tuple does not have enough fields to match the struct it is assigned to")
      : slice_literal("Tuple has too many fields for the struct it is assigned to");
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = source->source_range,
      .Type_Mismatch = { .expected = &descriptor_s64, .actual = source->descriptor },
      .detailed_message = message,
    });
    return *context->result;
  }

  u64 index = 0;
  DYN_ARRAY_FOREACH(Memory_Layout_Item, field, layout->items) {
    Value *tuple_item = *dyn_array_get(tuple->items, index);
    Value target_field = {
      .descriptor = field->declaration.descriptor,
      .storage = memory_layout_item_storage_at_index(&target->storage, layout, index),
      .source_range = target->source_range,
    };
    assign(context, builder, &target_field, tuple_item);
    index += 1;
  }
  return *context->result;
}

static PRELUDE_NO_DISCARD Mass_Result
assign(
  Execution_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
) {
  MASS_TRY(*context->result);

  if (target->storage.tag == Storage_Tag_Eflags) {
    panic("Internal Error: Trying to move into Eflags");
  }
  if (target->descriptor == &descriptor_void) {
    return *context->result;
  }

  if (source->descriptor == &descriptor_lazy_value) {
    value_force_exact(context, builder, target, source);
    return *context->result;
  }

  if (source->descriptor == &descriptor_tuple) {
    assign_tuple(context, builder, target, source);
    return *context->result;
  }

  Source_Range source_range = target->source_range;
  if (
    source->descriptor->tag == Descriptor_Tag_Reference_To &&
    target->descriptor->tag != Descriptor_Tag_Reference_To
  ) {
    Value *referenced_value = value_indirect_from_reference(context, builder, source);
    MASS_TRY(assign(context, builder, target, referenced_value));
    value_release_if_temporary(builder, referenced_value);
    return *context->result;
  } else if (
    target->descriptor->tag == Descriptor_Tag_Reference_To &&
    source->descriptor->tag != Descriptor_Tag_Reference_To
  ) {
    const Descriptor *reference_descriptor = target->descriptor->Reference_To.descriptor;
    if (!same_value_type_or_can_implicitly_move_cast(reference_descriptor, source)) goto err;
    if (source->storage.tag == Storage_Tag_Static) {
      if (!assign_from_static(context, builder, target, source)) {
        Value *referenced_value = value_indirect_from_reference(context, builder, target);
        MASS_TRY(assign(context, builder, referenced_value, source));
        value_release_if_temporary(builder, referenced_value);
      }
    } else {
      load_address(context, builder, &source_range, target, source->storage);
    }
    return *context->result;
  } else if (
    target->descriptor->tag == Descriptor_Tag_Reference_To &&
    source->descriptor->tag == Descriptor_Tag_Reference_To
  ) {
    if (!same_type_or_can_implicitly_move_cast(target->descriptor, source->descriptor)) goto err;
    if (!storage_equal(&target->storage, &source->storage)) {
      Value *referenced_source = value_indirect_from_reference(context, builder, source);
      Value *referenced_target = value_indirect_from_reference(context, builder, target);
      MASS_TRY(assign(context, builder, referenced_target, referenced_source));
      value_release_if_temporary(builder, referenced_source);
      value_release_if_temporary(builder, referenced_target);
    }
    return *context->result;
  } else if (value_is_static_number_literal(source)) {
    if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
      const Number_Literal *literal = storage_static_as_c_type(&source->storage, Number_Literal);
      if (literal->bits == 0) {
        source = token_value_force_immediate_integer(context, source, &descriptor_u64);
        source->descriptor = target->descriptor;
        move_value(context->allocator, builder, &source_range, &target->storage, &source->storage);
        return *context->result;
      } else {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = source_range,
          .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
          .detailed_message = "Trying to assign a non-zero literal number to a pointer",
        });
        return *context->result;
      }
    } else if (descriptor_is_integer(target->descriptor)) {
      source = token_value_force_immediate_integer(context, source, target->descriptor);
    } else if (source->descriptor != target->descriptor) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = source_range,
        .Type_Mismatch = { .expected = &descriptor_s64, .actual = source->descriptor },
        .detailed_message = "Trying to assign a literal number to a non-integer value",
      });
      return *context->result;
    }
  }
  if (source->descriptor->tag == Descriptor_Tag_Struct) {
    if (!same_value_type_or_can_implicitly_move_cast(target->descriptor, source)) goto err;

    for (u64 i = 0; i < dyn_array_length(source->descriptor->Struct.memory_layout.items); ++i) {
      Memory_Layout_Item *field = dyn_array_get(source->descriptor->Struct.memory_layout.items, i);
      Value source_field = {
        .descriptor = field->declaration.descriptor,
        .storage = memory_layout_item_storage_at_index(
          &source->storage, &source->descriptor->Struct.memory_layout, i
        ),
        .source_range = source->source_range,
      };
      Value target_field = {
        .descriptor = field->declaration.descriptor,
        .storage = memory_layout_item_storage_at_index(
          &target->storage, &target->descriptor->Struct.memory_layout, i
        ),
        .source_range = target->source_range,
      };
      MASS_TRY(assign(context, builder, &target_field, &source_field));
    }
    return *context->result;
  }
  if (source->storage.tag == Storage_Tag_Static) {
    if (assign_from_static(context, builder, target, source)) {
      return *context->result;
    }
  }

  if (descriptor_is_integer(source->descriptor) || descriptor_is_integer(target->descriptor)) {
    return assign_integers(context, builder, target, source);
  }

  MASS_TRY(*context->result);
  if (same_value_type_or_can_implicitly_move_cast(target->descriptor, source)) {
    move_value(context->allocator, builder, &source_range, &target->storage, &source->storage);
    return *context->result;
  }

  err:
  context_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Type_Mismatch,
    .source_range = source_range,
    .Type_Mismatch = {
      .expected = target->descriptor,
      .actual = source->descriptor,
    },
  });
  return *context->result;
}

static inline Value *
value_wrap_in_overload_set(
  Scope *scope,
  Value *value,
  Slice name,
  const Source_Range *lookup_range
) {
  Overload_Set *set = allocator_allocate(scope->allocator, Overload_Set);
  *set = (Overload_Set) {
    .items = dyn_array_make(Array_Value_Ptr), // @Leak should use proper allocator
  };
  dyn_array_push(set->items, value);
  Value *overload_set_value = allocator_allocate(scope->allocator, Value);
  value_init(overload_set_value, &descriptor_overload_set, storage_static(set), value->source_range);
  return overload_set_value;
}

static inline Value *
value_force_lazy_static(
  Value *value,
  Slice name
) {
  assert(value->descriptor == &descriptor_lazy_static_value);
  Lazy_Static_Value *lazy = storage_static_as_c_type(&value->storage, Lazy_Static_Value);
  if (lazy->resolving) {
    context_error(&lazy->context, (Mass_Error) {
      .tag = Mass_Error_Tag_Circular_Dependency,
      .source_range = value->source_range,
      .Circular_Dependency = { .name = name },
    });
    return 0;
  }
  lazy->resolving = true;
  Value *result = compile_time_eval(&lazy->context, lazy->expression);
  if (!result) return 0;
  lazy->resolving = false;
  *value = *result;
  return value;
}

static Value *
scope_entry_force_value(
  Execution_Context *context,
  Scope_Entry *entry
) {
  if (entry->forced) {
    return entry->value;
  }

  if (entry->value->descriptor == &descriptor_lazy_static_value) {
    entry->value = value_force_lazy_static(entry->value, entry->name);
  }

  // mark the entry as "forced" and avoid extra checks and overload set iteration on each lookup
  entry->forced = true;

  if (!entry->value) return 0;

  if (entry->value->descriptor == &descriptor_overload_set) {

    const Overload_Set *set = storage_static_as_c_type(&entry->value->storage, Overload_Set);
    for (u64 i = 0; i < dyn_array_length(set->items); ++i) {
      Value **overload_pointer = dyn_array_get(set->items, i);
      Value *overload = *overload_pointer;
      if (overload->descriptor == &descriptor_lazy_static_value) {
        overload = value_force_lazy_static(overload, entry->name);
        *overload_pointer = overload;
      }
      if (!overload) return 0;
      if (
        overload->descriptor->tag != Descriptor_Tag_Function_Instance &&
        overload->descriptor != &descriptor_function_literal &&
        overload->descriptor != &descriptor_overload_set
      ) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Non_Function_Overload,
          .source_range = overload->source_range,
        });
        return 0;
      }
    }
  }

  return entry->value;
}

static inline Value *
scope_lookup_force(
  Execution_Context *context,
  const Scope *scope,
  const Symbol *symbol,
  const Source_Range *lookup_range
) {
  Scope_Entry *entry = scope_lookup_hashed(scope, symbol->hash, symbol->name, 0);

  if (!entry) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Undefined_Variable,
      .Undefined_Variable = { .name = symbol->name },
      .source_range = *lookup_range,
    });
    return 0;
  }
  if (entry->epoch != VALUE_STATIC_EPOCH && entry->epoch != context->epoch) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Epoch_Mismatch,
      .source_range = *lookup_range,
    });
    return 0;
  }
  return scope_entry_force_value(context, entry);
}

static inline void
scope_define_value(
  Scope *scope,
  u64 epoch,
  Source_Range source_range,
  Slice name,
  Value *value
) {
  assert(name.length);
  if (!scope->map) {
    scope->map = hash_map_make(Scope_Map, scope->allocator);
  }
  s32 hash = Scope_Map__hash(name);
  Scope_Entry *it = scope_lookup_shallow_hashed(scope, hash, name);
  if (it) {
    if (it->value->descriptor != &descriptor_overload_set) {
      it->value = value_wrap_in_overload_set(scope, it->value, name, &source_range);
    }
    Overload_Set *set = storage_static_as_c_type(&it->value->storage, Overload_Set);
    dyn_array_push(set->items, value);
  } else {
    Scope_Entry *allocated = allocator_allocate(scope->allocator, Scope_Entry);
    *allocated = (Scope_Entry) {
      .value = value,
      .name = name,
      .epoch = epoch,
      .source_range = source_range,
    };
    hash_map_set_by_hash(scope->map, hash, name, allocated);
  }
}

PRELUDE_NO_DISCARD static inline Mass_Result
scope_define_operator(
  Scope *scope,
  Source_Range source_range,
  Slice name,
  Operator *operator
) {
  Operator_Map **operator_map_pointer = operator->fixity == Operator_Fixity_Prefix
    ? &scope->prefix_operator_map
    : &scope->infix_or_suffix_operator_map;
  Operator **operator_entry = 0;
  if (*operator_map_pointer) {
    operator_entry = hash_map_get(*operator_map_pointer, name);
    if (operator_entry) {
      return mass_error((Mass_Error) {
        .tag = Mass_Error_Tag_Operator_Fixity_Conflict,
        .source_range = source_range,
        .Operator_Fixity_Conflict = {
          .fixity = operator->fixity,
          .symbol = name,
        },
      });
    }
  } else {
    *operator_map_pointer = hash_map_make(Operator_Map);
  }
  hash_map_set(*operator_map_pointer, name, operator);
  return mass_success();
}

static inline Operator *
scope_lookup_operator_shallow(
  const Scope *scope,
  Slice name,
  Operator_Fixity fixity
) {
  Operator **operator_entry = 0;
  Operator_Map * const *operator_map_pointer = fixity == Operator_Fixity_Prefix
    ? &scope->prefix_operator_map
    : &scope->infix_or_suffix_operator_map;
  if (*operator_map_pointer) {
    operator_entry = hash_map_get(*operator_map_pointer, name);
  }
  return operator_entry ? *operator_entry : 0;
}

static inline Operator *
scope_lookup_operator(
  const Scope *scope,
  Slice name,
  Operator_Fixity fixity
) {
  for (; scope; scope = scope->parent) {
    Operator *operator = scope_lookup_operator_shallow(scope, name, fixity);
    if (operator) return operator;
  }
  return 0;
}

static inline Value *
token_make_symbol(
  Symbol_Map *cache_map,
  const Allocator *allocator,
  Slice name,
  Symbol_Type type,
  Source_Range source_range
) {
  s32 hash = Symbol_Map__hash(name);
  const Symbol *symbol = 0;
  if (!symbol) {
    // Symbol type is derived from name anyway so it does not need to be part of the key
    Symbol **cache_entry = hash_map_get_by_hash(cache_map, hash, name);
    if (cache_entry) {
      symbol = *cache_entry;
    }
  }
  if (!symbol) {
    // FIXME for some reason using non-default allocator here makes Linux unhappy
    Symbol *heap_symbol = allocator_allocate(allocator, Symbol);
    *heap_symbol = (Symbol){
      .type = type,
      .name = name,
      .hash = hash,
    };
    symbol = heap_symbol;
    hash_map_set_by_hash(cache_map, hash, name, heap_symbol);
  }

  return value_init(
    allocator_allocate(allocator, Value),
    &descriptor_symbol, storage_static(symbol), source_range
  );
}

const Token_Pattern token_pattern_comma_operator = {
  .tag = Token_Pattern_Tag_Symbol,
  .Symbol.name = slice_literal_fields(","),
};

const Token_Pattern token_pattern_semicolon = {
  .tag = Token_Pattern_Tag_Symbol,
  .Symbol.name = slice_literal_fields(";"),
};

static inline bool
value_match(
  const Value *value,
  const Token_Pattern *pattern
) {
  if (!value) return false;
  switch(pattern->tag) {
    case Token_Pattern_Tag_Invalid: {
      panic("Invalid pattern tag");
      break;
    }
    case Token_Pattern_Tag_Any: {
      return true;
    }
    case Token_Pattern_Tag_Symbol: {
      if (!value_is_symbol(value)) return false;
      if (pattern->Symbol.name.length) {
        Slice name = value_as_symbol(value)->name;
        if (!slice_equal(pattern->Symbol.name, name)) return false;
      }
      return true;
    }
    case Token_Pattern_Tag_Group: {
      if (!value_is_group(value)) return false;
      return value_as_group(value)->tag == pattern->Group.tag;
    }
    case Token_Pattern_Tag_Or: {
      return value_match(value, pattern->Or.a) || value_match(value, pattern->Or.b);
    }
  }
  return true;
}

static inline bool
value_match_symbol(
  Value *token,
  Slice name
) {
  if (!value_is_symbol(token)) return false;
  if (!name.length) return true;
  return slice_equal(value_as_symbol(token)->name, name);
}

static inline bool
value_match_group(
  Value *token,
  Group_Tag tag
) {
  if (!value_is_group(token)) return false;
  return value_as_group(token)->tag == tag;
}

static inline Value_View
temp_token_array_into_value_view(
  const Allocator *allocator,
  Value **children,
  u64 child_count,
  Source_Range children_range
) {
  Value_View result = { .values = 0, .length = child_count, .source_range = children_range };
  if (child_count) {
    Value **tokens = allocator_allocate_array(allocator, Value *, child_count);
    memcpy(tokens, children, child_count * sizeof(tokens[0]));
    result.values = tokens;
  }
  return result;
}

typedef struct {
  Group *group;
  u64 index;
} Tokenizer_Parent;
typedef dyn_array_type(Tokenizer_Parent) Array_Tokenizer_Parent;

static inline void
tokenizer_maybe_push_fake_semicolon(
  Symbol_Map *cache_map,
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Source_Range source_range
) {
  bool has_children = dyn_array_length(*stack) != 0;
  if (dyn_array_length(*parent_stack)) {
    Tokenizer_Parent *parent = dyn_array_last(*parent_stack);
    Value *parent_value = *dyn_array_get(*stack, parent->index);
    if(value_as_group(parent_value)->tag != Group_Tag_Curly) return;
    has_children = parent->index + 1 != dyn_array_length(*stack);
  }
  // Do not treat leading newlines as semicolons
  if (!has_children) return;
  dyn_array_push(*stack, token_make_symbol(
    cache_map, allocator, slice_literal(";"), Symbol_Type_Operator_Like, source_range
  ));
}

static inline void
tokenizer_group_start(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Group_Tag tag,
  Source_Range source_range
) {
  Group *group = allocator_allocate(allocator, Group);
  group->tag = tag;
  Value *value = value_init(
    allocator_allocate(allocator, Value),
    &descriptor_group, storage_static(group), source_range
  );
  dyn_array_push(*parent_stack, (Tokenizer_Parent){
    .group = group,
    .index = dyn_array_length(*stack)
  });
  dyn_array_push(*stack, value);
}

static inline bool
tokenizer_group_end(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  char actual_paren,
  u64 offset
) {
  Value *parent_value = 0;
  Tokenizer_Parent *parent = 0;
  if (dyn_array_length(*parent_stack)) {
    parent = dyn_array_last(*parent_stack);
    parent_value = *dyn_array_get(*stack, parent->index);
  }
  if (!parent_value || !value_is_group(parent_value)) {
    panic("Tokenizer: unexpected parent stack entry");
  }

  Slice expected_paren = {0};
  switch (parent->group->tag) {
    case Group_Tag_Paren: {
      expected_paren = slice_literal(")");
      break;
    }
    case Group_Tag_Curly: {
      // Newlines at the end of the block do not count as semicolons otherwise this:
      // { 42
      // }
      // is being interpreted as:
      // { 42 ; }
      while (parent->index + 1 < dyn_array_length(*stack)) {
        Value *last = *dyn_array_last(*stack);
        // :FakeSemicolon
        // We detect fake semicolons with range_length == 0
        // so it needs to be done like that in the tokenizer
        bool is_last_token_a_fake_semicolon = (
          range_length(last->source_range.offsets) == 0 &&
          value_is_symbol(last) &&
          slice_equal(value_as_symbol(last)->name, slice_literal(";"))
        );
        if (!is_last_token_a_fake_semicolon) break;
        dyn_array_pop(*stack);
      }

      expected_paren = slice_literal("}");
      break;
    }
    case Group_Tag_Square: {
      expected_paren = slice_literal("]");
      break;
    }
  }
  if (actual_paren != expected_paren.bytes[0]) {
    return false;
  }

  parent_value->source_range.offsets.to = u64_to_u32(offset);
  Source_Range children_range = parent_value->source_range;
  children_range.offsets.to -= 1;
  children_range.offsets.from += 1;
  Value **children_values = dyn_array_raw(*stack) + parent->index + 1;
  u64 child_count = dyn_array_length(*stack) - parent->index - 1;
  parent->group->children = temp_token_array_into_value_view(
    allocator, children_values, child_count, children_range
  );
  stack->data->length = parent->index + 1; // pop the children
  dyn_array_pop(*parent_stack);

  return true;
}

static inline void
tokenizer_push_string_literal(
  Compilation *compilation,
  Fixed_Buffer **string_buffer,
  Array_Value_Ptr *stack,
  Slice raw_bytes,
  Source_Range source_range
) {
  (*string_buffer)->occupied = 0;
  Slice remainder = raw_bytes;
  for(s64 escape_index; ; ) {
    escape_index = slice_index_of_char(remainder, '\\');
    if (escape_index == -1) {
      break;
    }
    Slice to_copy = slice_sub(remainder, 0, escape_index);
    remainder = slice_sub(remainder, escape_index + 2, remainder.length);
    fixed_buffer_resizing_ensure_capacity(string_buffer, to_copy.length);
    fixed_buffer_append_slice(*string_buffer, to_copy);
    char ch = raw_bytes.bytes[escape_index + 1];
    s8 escaped_character;
    switch (ch) {
      case 'n': escaped_character = '\n'; break;
      case 'r': escaped_character = '\r'; break;
      case 't': escaped_character = '\t'; break;
      case 'v': escaped_character = '\v'; break;
      case '0': escaped_character = '\0'; break;
      default: escaped_character = ch; break;
    }
    fixed_buffer_resizing_append_s8(string_buffer, escaped_character);
  }
  {
    fixed_buffer_resizing_ensure_capacity(string_buffer, remainder.length);
    fixed_buffer_append_slice(*string_buffer, remainder);
  }

  u64 length = (*string_buffer)->occupied;
  char *bytes = allocator_allocate_bytes(compilation->allocator, length, 1);
  memcpy(bytes, (*string_buffer)->memory, length);
  {
    Descriptor *bytes_descriptor =
      descriptor_array_of(compilation->allocator, &descriptor_u8, u64_to_u32(length));
    Value *pointer_value = hash_map_set(compilation->static_pointer_map, bytes, (Value){0});
    value_init(pointer_value, bytes_descriptor, storage_none, source_range);
  }

  Slice *string = allocator_allocate(compilation->allocator, Slice);
  *string = (Slice){bytes, (*string_buffer)->occupied};
  Value *string_value = value_init(
    allocator_allocate(compilation->allocator, Value),
    &descriptor_slice, storage_static(string), source_range
  );
  dyn_array_push(*stack, string_value);
}

#include "generated_tokenizer.c"

static inline Value *
token_peek_match(
  Value_View view,
  u64 index,
  const Token_Pattern *pattern
) {
  Value *token = value_view_peek(view, index);
  if (!token) return 0;
  if (!value_match(token, pattern)) return 0;
  return token;
}

static inline const Value_View *
value_view_array_push(
  Array_Value_View *array,
  Value_View to_push
) {
  const Value_View *view = dyn_array_push_uninitialized(*array);
  // Need to cast here because we need to initialize somehow
  // a const pointer and that is not allowed
  *(Value_View *)view = to_push;
  return view;
}

typedef struct {
  Value_View view;
  u64 index;
  bool done;
} Value_View_Split_Iterator;

static Value_View
token_split_next(
  Value_View_Split_Iterator *it,
  const Token_Pattern *separator
) {
  if (it->done) return (Value_View){0};
  u64 start_index = it->index;
  for (
    ;
    it->index < it->view.length;
    it->index++
  ) {
    Value *token = value_view_get(it->view, it->index);
    if (value_match(token, separator)) {
      Value_View result = value_view_slice(&it->view, start_index, it->index);
      // Skip over the separator
      it->index++;
      return result;
    }
  }
  it->done = true;
  return value_view_rest(&it->view, start_index);
}

static inline const Descriptor *
value_ensure_type(
  Execution_Context *context,
  Value *value,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  if (!value) return 0;
  if (!same_type(value->descriptor, &descriptor_descriptor_pointer)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = source_range,
      .Type_Mismatch = {
        .expected = &descriptor_descriptor_pointer,
        .actual = value->descriptor,
      },
    });
    return 0;
  }
  return *storage_static_as_c_type(&value->storage, const Descriptor *);
}

static inline Value_View
value_view_match_till(
  Value_View view,
  u64 *peek_index,
  const Token_Pattern *end_pattern
) {
  u64 start_index = *peek_index;
  if (!end_pattern) {
    *peek_index = view.length;
    return value_view_rest(&view, start_index);
  }
  for (; *peek_index < view.length; *peek_index += 1) {
    Value *token = value_view_get(view, *peek_index);
    if (value_match(token, end_pattern)) {
      *peek_index += 1;
      return value_view_slice(&view, start_index, *peek_index - 1);
    }
  }
  return value_view_slice(&view, start_index, *peek_index);
}

static inline Value_View
value_view_match_till_end_of_statement(
  Value_View view,
  u64 *peek_index
) {
  return value_view_match_till(view, peek_index, &token_pattern_semicolon);
}

#define Token_Maybe_Match(_id_, ...)\
  Value *(_id_) = token_peek_match(view, peek_index, &(Token_Pattern) { __VA_ARGS__ });\
  if (_id_) (++peek_index)

static inline Value *
token_expect_match(
  Execution_Context *context,
  Value_View view,
  u64 *peek_index,
  const Token_Pattern *pattern
) {
  Value *result = token_peek_match(view, *peek_index, pattern);
  if (result) {
    (*peek_index) += 1;
  } else {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_slice(&view, *peek_index, *peek_index).source_range,
    });
  }
  return result;
}

#define Token_Expect_Match(_id_, ...)\
  Value *(_id_) = token_expect_match(context, view, &peek_index, &(Token_Pattern) { __VA_ARGS__ });\
  (void)(_id_);\
  MASS_ON_ERROR(*context->result) return 0

#define Token_Match(_id_, ...)\
  Token_Maybe_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal(_op_))

typedef struct {
  Slice name;
  Value *value;
} Token_Match_Arg;

static inline const Descriptor *
token_match_type(
  Execution_Context *context,
  Value_View view
);

static Value *
token_parse_tuple(
  Execution_Context *context,
  Value_View view
) {
  Value_View remaining = view;
  u64 match_length = 0;

  Value *result_value = 0;

  // TODO use temp allocator
  Array_Value_Ptr items = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator);
  for (; remaining.length; remaining = value_view_rest(&remaining, match_length)) {
    MASS_ON_ERROR(*context->result) goto err;
    Value *item = token_parse_expression(context, remaining, &match_length, &token_pattern_comma_operator);
    dyn_array_push(items, item);
  }
  Tuple *tuple = allocator_allocate(context->allocator, Tuple);
  *tuple = (Tuple){
    .items = items,
  };
  result_value = value_make(context, &descriptor_tuple, storage_static(tuple), view.source_range);
  err:
  return result_value;
}

static Value *
token_parse_single(
  Execution_Context *context,
  Value *value
) {
  if (value_is_group(value)) {
    const Group *group = value_as_group(value);
    switch(group->tag) {
      case Group_Tag_Paren: {
        return token_parse_expression(context, group->children, &(u64){0}, 0);
      }
      case Group_Tag_Curly: {
        return token_parse_block(context, value);
      }
      case Group_Tag_Square: {
        return token_parse_tuple(context, group->children);
      }
    }
  } else if(value_is_symbol(value)) {
    return scope_lookup_force(context, context->scope, value_as_symbol(value), &value->source_range);
  }
  return value;
}

typedef enum {
  Macro_Match_Mode_Expression,
  Macro_Match_Mode_Statement
} Macro_Match_Mode;

static u64
token_match_pattern(
  Value_View view,
  Array_Macro_Pattern macro_pattern,
  Array_Value_View *out_match,
  Macro_Match_Mode mode
) {
  u64 pattern_length = dyn_array_length(macro_pattern);
  if (!pattern_length) panic("Zero-length pattern does not make sense");

  u64 pattern_index = 0;
  u64 view_index = 0;

  for (; pattern_index < pattern_length && view_index < view.length; pattern_index++) {
    Macro_Pattern *pattern = dyn_array_get(macro_pattern, pattern_index);
    switch(pattern->tag) {
      case Macro_Pattern_Tag_Single_Token: {
        Value *token = token_peek_match(view, view_index, &pattern->Single_Token.token_pattern);
        if (!token) {
          return 0;
        }
        dyn_array_push(*out_match, value_view_slice(&view, view_index, view_index + 1));
        view_index++;
        break;
      }
      case Macro_Pattern_Tag_Any_Token_Sequence: {
        u64 any_token_start_view_index = view_index;
        Macro_Pattern *peek = pattern_index + 1 < pattern_length
          ? dyn_array_get(macro_pattern, pattern_index + 1)
          : 0;
        assert(!peek || peek->tag == Macro_Pattern_Tag_Single_Token);
        const Token_Pattern *end_pattern = 0;
        if (peek) {
          end_pattern = &peek->Single_Token.token_pattern;
        } else if (mode == Macro_Match_Mode_Statement) {
          end_pattern = &token_pattern_semicolon;
        }

        for (; view_index < view.length; ++view_index) {
          Value *token = value_view_get(view, view_index);
          if (end_pattern && value_match(token, end_pattern)) {
            break;
          }
        }
        dyn_array_push(*out_match, value_view_slice(&view, any_token_start_view_index, view_index));
        break;
      }
    }
  }

  // Did not match full pattern
  if (
    pattern_index != pattern_length &&
    !(
      pattern_index == pattern_length - 1 &&
      dyn_array_last(macro_pattern)->tag == Macro_Pattern_Tag_Any_Token_Sequence
    )
  ) {
    return 0;
  }

  return view_index;
}

static Value *
token_apply_macro_syntax(
  Execution_Context *context,
  Array_Value_View match,
  Macro *macro,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  assert(macro->scope);

  // All captured token sequences need to have access to the same base scope
  // to support implementing disjointed syntax, such as for (;;) loop
  // or switch / pattern matching.
  // Symboleally there should be a way to control this explicitly somehow.
  Scope *captured_scope = scope_make(context->allocator, context->scope);
  Scope *expansion_scope = scope_make(context->allocator, macro->scope);

  for (u64 i = 0; i < dyn_array_length(macro->pattern); ++i) {
    Macro_Pattern *item = dyn_array_get(macro->pattern, i);
    Slice capture_name = item->capture_name;

    if (!capture_name.length) continue;
    Value_View capture_view = *dyn_array_get(match, i);
    Macro_Capture *capture = allocator_allocate(context->allocator, Macro_Capture);
    *capture = (Macro_Capture){
      .name = item->capture_name,
      .view = capture_view,
      .scope = captured_scope,
    };
    Value *result = value_init(
      allocator_allocate(context->allocator, Value),
      &descriptor_macro_capture, storage_static(capture), capture_view.source_range
    );
    scope_define_value(expansion_scope, VALUE_STATIC_EPOCH, capture_view.source_range, capture_name, result);
  }

  Execution_Context body_context = *context;
  body_context.scope = expansion_scope;

  Value *result;
  if (body_context.flags & Execution_Context_Flags_Global) {
    result = compile_time_eval(&body_context, macro->replacement);
  } else {
    result = token_parse_expression(&body_context, macro->replacement, &(u64){0}, 0);
  }
  // The result of the expansion should map to the source range of the match
  if (result) result->source_range = source_range;
  return result;
}

static Value *
mass_handle_statement_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *lazy_value
) {
  assert(expected_result_descriptor(expected_result) == &descriptor_void);
  return value_force(context, builder, expected_result, lazy_value);
}

static u64
token_parse_macro_statement(
  Execution_Context *context,
  Value_View value_view,
  Lazy_Value *out_lazy_value,
  Macro *macro
) {
  assert(macro);
  if (!value_view.length) return 0;

  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_View match = dyn_array_make(
    Array_Value_View,
    .allocator = context->temp_allocator,
    .capacity = 32,
  );

  u64 match_length = token_match_pattern(value_view, macro->pattern, &match, Macro_Match_Mode_Statement);
  if (!match_length) goto defer;

  Value_View rest = value_view_rest(&value_view, match_length);
  if (rest.length) {
    if (value_match(value_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length += 1;
    } else {
      match_length = 0;
      goto defer;
    }
  }

  Value_View match_view = value_view_slice(&value_view, 0, match_length);
  Value *expansion_value = token_apply_macro_syntax(context, match, macro, match_view.source_range);
  if (expansion_value) {
    assert(value_is_lazy_or_static(expansion_value));

    out_lazy_value->payload = expansion_value;
    out_lazy_value->proc = mass_handle_statement_lazy_proc;
  }

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return match_length;
}

static Value *
token_parse_block_view(
  Execution_Context *context,
  Value_View children_view
);

static Descriptor *
token_match_fixed_array_type(
  Execution_Context *context,
  Value_View view
);

static inline const Descriptor *
token_match_type(
  Execution_Context *context,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *type_value = compile_time_eval(context, view);
  return value_ensure_type(context, type_value, view.source_range);
}

static inline bool
token_maybe_split_on_operator(
  Value_View view,
  Slice operator,
  Value_View *lhs,
  Value_View *rhs,
  Value **operator_token
) {
  u64 lhs_end = 0;
  u64 rhs_start = 0;
  bool found = false;
  for (u64 i = 0; i < view.length; ++i) {
    Value *token = value_view_get(view, i);
    if (value_match_symbol(token, operator)) {
      *operator_token = token;
      lhs_end = i;
      rhs_start = i + 1;
      found = true;
      break;
    }
  }
  if (!found) return false;

  *lhs = value_view_slice(&view, 0, lhs_end);
  *rhs = value_view_rest(&view, rhs_start);

  return true;
}

static Function_Parameter
token_match_argument(
  Execution_Context *context,
  Value_View view,
  Function_Info *function
) {
  Function_Parameter arg = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return arg;

  Value_View default_expression;
  Value_View static_expression;
  Value_View definition;
  Value *equals;
  bool is_inferred_type = false;

  if (token_maybe_split_on_operator(
    view, slice_literal("::"), &definition, &static_expression, &equals
  )) {
    if (static_expression.length == 0) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = "Expected an expression after `::`"
      });
      goto err;
    }
    // TODO @CopyPaste
    if (definition.length != 1 || !value_is_symbol(value_view_get(definition, 0))) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = "Expected an argument name",
      });
      goto err;
    }
    Value *name_token = value_view_get(definition, 0);
    Value *static_value = compile_time_eval(context, static_expression);
    MASS_ON_ERROR(*context->result) goto err;
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Exact_Static,
      .Exact_Static = {
        .storage = static_value->storage,
      },
      .declaration = {
        .name = value_as_symbol(name_token)->name,
        .descriptor = static_value->descriptor,
        .source_range = definition.source_range,
      },
    };
  }

  if (token_maybe_split_on_operator(
    view, slice_literal("="), &definition, &default_expression, &equals
  )) {
    if (default_expression.length == 0) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = "Expected an expression after `=`"
      });
      goto err;
    }
  } else if (token_maybe_split_on_operator(
    view, slice_literal(":="), &definition, &default_expression, &equals
  )) {
    is_inferred_type = true;
    if (default_expression.length == 0) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = "Expected an expression after `:=`"
      });
      goto err;
    }
  } else {
    definition = view;
    default_expression = (Value_View){0};
  }

  const Descriptor *descriptor = 0;
  Value *name_token;
  Value_View maybe_type_expression = {0};
  bool generic = false;
  if (is_inferred_type) {
    if (definition.length != 1 || !value_is_symbol(value_view_get(definition, 0))) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = "Expected an argument name",
      });
      goto err;
    }
    name_token = value_view_get(definition, 0);
    Value *parsed_default_expression =
      token_parse_expression(context, default_expression, &(u64){0}, 0);
    descriptor = value_or_lazy_value_descriptor(parsed_default_expression);
    if (value_is_static_number_literal(parsed_default_expression)) descriptor = &descriptor_s64;
  } else {
    Value_View name_tokens;
    Value *operator;
    generic = !token_maybe_split_on_operator(
      definition, slice_literal(":"), &name_tokens, &maybe_type_expression, &operator
    );
    if (generic) {
      name_tokens = definition;
    }
    if (name_tokens.length == 0) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = "':' operator expects an identifier on the left hand side",
      });
      goto err;
    }
    name_token = value_view_get(name_tokens, 0);
    if (name_tokens.length > 1 || !value_is_symbol(name_token)) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = name_tokens.source_range,
      });
      goto err;
    }
  }

  arg = (Function_Parameter) {
    .tag = generic ? Function_Parameter_Tag_Generic : Function_Parameter_Tag_Runtime,
    .maybe_default_expression = default_expression,
    .maybe_type_expression = maybe_type_expression,
    .declaration = {
      .name = value_as_symbol(name_token)->name,
      .descriptor = descriptor,
      .source_range = definition.source_range,
    },
  };

  err:
  return arg;
}

static Function_Return
token_match_return_type(
  Execution_Context *context,
  Value_View view
) {
  Function_Return returns = {
    .declaration.source_range = view.source_range,
  };
  if (context->result->tag != Mass_Result_Tag_Success) return returns;

  Value_View lhs;
  Value_View rhs;
  Value *operator;
  if (token_maybe_split_on_operator(view, slice_literal(":"), &lhs, &rhs, &operator)) {
    if (lhs.length == 0) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = operator->source_range,
        .detailed_message = "':' operator expects an identifier on the left hand side"
      });
      goto err;
    }
    if (lhs.length > 1 || !value_is_symbol(value_view_get(lhs, 0))) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = lhs.source_range,
      });
      goto err;
    }

    returns.maybe_type_expression = rhs;
    returns.declaration.source_range = rhs.source_range,
    returns.declaration.name = value_as_symbol(value_view_get(lhs, 0))->name;
  } else {
    returns.maybe_type_expression = view;
  }

  err:
  return returns;
}

static inline bool
storage_is_indirect(
  const Storage *storage
) {
  if (storage->tag != Storage_Tag_Memory) return false;
  return storage->Memory.location.tag == Memory_Location_Tag_Indirect;
}

static inline bool
storage_occupies_same_memory(
  const Storage *a,
  const Storage *b
) {
  if (storage_equal(a, b)) return true;
  if (a->tag == Storage_Tag_Register) {
    if (!storage_is_indirect(b)) return false;
    return a->Register.index == b->Memory.location.Indirect.base_register;
  }
  if (b->tag == Storage_Tag_Register) {
    return storage_occupies_same_memory(b, a);
  }
  return false;
}

static PRELUDE_NO_DISCARD Value *
expected_result_ensure_value_or_temp(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
) {
  MASS_ON_ERROR(*context->result) return 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Value *result_value = value_from_exact_expected_result(expected_result);
      MASS_ON_ERROR(assign(context, builder, result_value, value)) return 0;
      // @Hack there should be a better and more robust way to do this
      if (
        value->storage.tag != Storage_Tag_Static &&
        !storage_occupies_same_memory(&result_value->storage, &value->storage)
      ) {
        value_release_if_temporary(builder, value);
      }
      return result_value;
    }
    case Expected_Result_Tag_Flexible: {
      if (!value) return 0;
      const Expected_Result_Flexible *flexible = &expected_result->Flexible;
      const Descriptor *expected_descriptor =
        flexible->descriptor ? flexible->descriptor : value->descriptor;
      if (value->storage.tag == Storage_Tag_None) {
        return value;
      }
      if (
        value->storage.tag == Storage_Tag_Static &&
        (flexible->storage & Expected_Result_Storage_Static)
      ) {
        value = maybe_coerce_number_literal_to_integer(context, value, expected_descriptor);
        if (!same_type_or_can_implicitly_move_cast(expected_descriptor, value->descriptor)) {
          panic("Unexpected type mismatch");
        }
        return value;
      }
      if (
        value->storage.tag == Storage_Tag_Eflags &&
        (flexible->storage & Expected_Result_Storage_Eflags)
      ) {
        return value;
      }
      if (
        value->storage.tag == Storage_Tag_Memory &&
        (flexible->storage & Expected_Result_Storage_Memory)
      ) {
        return value;
      }
      if (
        value->storage.tag == Storage_Tag_Unpacked &&
        (flexible->storage & Expected_Result_Storage_Unpacked)
      ) {
        return value;
      }
      if (flexible->storage & Expected_Result_Storage_Register) {
        if (
          value->storage.tag == Storage_Tag_Register &&
          (flexible->register_bitset & (1llu << value->storage.Register.index))
        ) {
          return value;
        }
        Value *temp_result = value_temporary_acquire_register_for_descriptor(
          context, builder, register_find_available(builder, 0), expected_descriptor, value->source_range
        );
        value_release_if_temporary(builder, value);
        MASS_ON_ERROR(assign(context, builder, temp_result, value)) return 0;
        return temp_result;
      }
      if (flexible->storage & Expected_Result_Storage_Memory) {
        assert(value->storage.tag != Storage_Tag_Register); // checked above
        Value *temp_result = reserve_stack(context, builder, expected_descriptor, value->source_range);
        MASS_ON_ERROR(assign(context, builder, temp_result, value)) return 0;
        return temp_result;
      }
      // FIXME support floats
      assert(value->storage.tag != Storage_Tag_Xmm);
      panic("Unable to put the value into the expected storage");
      return value;
    }
    default: {
      panic("Unknown Expected_Result tag");
      return 0;
    }
  }
}

static PRELUDE_NO_DISCARD Value *
value_force(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *value
) {
  MASS_ON_ERROR(*context->result) return 0;
  if (!value) return 0;

  value = token_parse_single(context, value);
  MASS_ON_ERROR(*context->result) return 0;

  if (value->descriptor == &descriptor_lazy_value) {
    Lazy_Value *lazy = storage_static_as_c_type(&value->storage, Lazy_Value);
    if (lazy->epoch != VALUE_STATIC_EPOCH && lazy->epoch != context->epoch) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Epoch_Mismatch,
        .source_range = value->source_range,
      });
      return 0;
    }
    Value *result = lazy->proc(&lazy->context, builder, expected_result, lazy->payload);
    allocator_deallocate(context->allocator, lazy, sizeof(Lazy_Value));
    MASS_ON_ERROR(*context->result) return 0;
    // TODO is there a better way to cache the result?
    *value = *result;
    return result;
  }

  return expected_result_ensure_value_or_temp(context, builder, expected_result, value);
}

static inline void
value_force_exact(
  Execution_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source
) {
  Expected_Result expected_result = expected_result_from_value(target);
  Value *forced = value_force(context, builder, &expected_result, source);
  MASS_ON_ERROR(*context->result) return;
  assert(forced->descriptor == target->descriptor);
  if (target->descriptor != &descriptor_void) assert(forced == target);
}

static void
token_match_call_arguments(
  Execution_Context *context,
  Value *token,
  Array_Value_Ptr *out_args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  dyn_array_clear(*out_args);

  const Group *group = value_as_group(token);
  if (group->children.length == 0) return;

  Value_View_Split_Iterator it = { .view = group->children };

  while (!it.done) {
    if (context->result->tag != Mass_Result_Tag_Success) return;
    Value_View view = token_split_next(&it, &token_pattern_comma_operator);
    Value *parse_result = token_parse_expression(context, view, &(u64){0}, 0);
    dyn_array_push(*out_args, parse_result);
  }
}

static Value *
token_handle_user_defined_operator_proc(
  Execution_Context *context,
  Value_View args,
  User_Defined_Operator *operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // We make a nested scope based on the original scope
  // instead of current scope for hygiene reasons.
  Scope *body_scope = scope_make(context->allocator, operator->scope);
  assert(operator->argument_count == args.length);

  for (u8 i = 0; i < operator->argument_count; ++i) {
    Slice arg_name = operator->argument_names[i];
    Value *arg = token_parse_single(context, value_view_get(args, i));
    MASS_ON_ERROR(*context->result) return 0;

    // FIXME this should probably use the epoch from the definition, but we do not capture it yet
    scope_define_value(body_scope, VALUE_STATIC_EPOCH, arg->source_range, arg_name, arg);
  }

  Execution_Context body_context = *context;
  body_context.scope = body_scope;
  return token_parse_expression(&body_context, operator->body, &(u64){0}, 0);
}

static inline Value *
mass_make_lazy_value_with_epoch(
  Execution_Context *context,
  Source_Range source_range,
  void *payload,
  const Descriptor *descriptor,
  u64 epoch,
  Lazy_Value_Proc proc
) {
  Lazy_Value *lazy = allocator_allocate(context->allocator, Lazy_Value);
  *lazy = (Lazy_Value) {
    .context = *context,
    .descriptor = descriptor,
    .proc = proc,
    .payload = payload,
    .epoch = epoch,
  };
  return value_init(
    allocator_allocate(context->allocator, Value),
    &descriptor_lazy_value, storage_static(lazy), source_range
  );
}

static inline Value *
mass_make_lazy_value(
  Execution_Context *context,
  Source_Range source_range,
  void *payload,
  const Descriptor *descriptor,
  Lazy_Value_Proc proc
) {
  return mass_make_lazy_value_with_epoch(
    context, source_range, payload, descriptor, context->epoch, proc
  );
}

static inline void
scope_define_lazy_compile_time_expression(
  Execution_Context *context,
  Scope *scope,
  Slice name,
  Value_View view
) {
  Lazy_Static_Value *lazy_static_value = allocator_allocate(context->allocator, Lazy_Static_Value);
  *lazy_static_value = (Lazy_Static_Value){
    .context = *context,
    .expression = view,
  };
  Value *value = value_init(
    allocator_allocate(context->allocator, Value),
    &descriptor_lazy_static_value,
    storage_static(lazy_static_value),
    view.source_range
  );

  scope_define_value(scope, VALUE_STATIC_EPOCH, view.source_range, name, value);
}

static u64
token_parse_exports(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_data
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  u64 peek_index = 0;
  Token_Match(keyword_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("exports"));
  Token_Expect_Match(block, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Curly);

  if (context->module->export.tag != Module_Export_Tag_None) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = keyword_token->source_range,
      .detailed_message = "A module can not have multiple exports statements. Original declaration at:",
      .other_source_range = context->module->export.source_range,
    });
    goto err;
  }

  context->module->export.source_range = keyword_token->source_range;

  Value_View children = value_as_group(block)->children;
  if (children.length == 1) {
    if (value_match_symbol(value_view_get(children, 0), slice_literal(".."))) {
      context->module->export.tag = Module_Export_Tag_All;
      return peek_index;
    }
  }

  context->module->export = (Module_Export) {
    .tag = Module_Export_Tag_Selective,
    .Selective = {
      .symbols = dyn_array_make(Array_Value_Ptr, .capacity = children.length / 2 + 1 ),
    },
  };

  if (children.length != 0) {
    Value_View_Split_Iterator it = { .view = children };

    while (!it.done) {
      if (context->result->tag != Mass_Result_Tag_Success) goto err;
      Value_View item = token_split_next(&it, &token_pattern_comma_operator);
      if (item.length != 1 || !value_is_symbol(value_view_get(item, 0))) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = item.source_range,
          .detailed_message = "Exports {} block must contain a comma-separated identifier list"
        });
        goto err;
      }
      dyn_array_push(context->module->export.Selective.symbols, value_view_get(item, 0));
    }
  }

  err:
  return peek_index;
}

static u64
token_parse_operator_definition(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_data
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  User_Defined_Operator *user_defined_operator = 0;

  u64 peek_index = 0;
  Token_Match(keyword_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("operator"));

  Token_Maybe_Match(precedence_token, .tag = Token_Pattern_Tag_Any);

  if (!precedence_token) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = keyword_token->source_range,
      .detailed_message = "'operator' keyword must be followed by a precedence number"
    });
    goto err;
  }

  Source_Range precedence_source_range = precedence_token->source_range;
  Value *precedence_value = token_parse_single(context, precedence_token);
  precedence_value = token_value_force_immediate_integer(context, precedence_value, &descriptor_u64);
  MASS_ON_ERROR(*context->result) goto err;

  assert(precedence_value->storage.tag == Storage_Tag_Static);
  u64 precendence = storage_static_value_up_to_u64(&precedence_value->storage);

  Token_Maybe_Match(pattern_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);

  if (!pattern_token) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = precedence_source_range,
      .detailed_message ="Operator definition must have a pattern in () following the precedence"
    });
    goto err;
  }

  u64 body_length = 0;
  Value_View rest = value_view_rest(&view, peek_index);
  Value_View body_view = value_view_match_till_end_of_statement(rest, &body_length);
  peek_index += body_length;

  Value_View definition = value_as_group(pattern_token)->children;

  user_defined_operator = allocator_allocate(context->allocator, User_Defined_Operator);
  *user_defined_operator = (User_Defined_Operator) {
    .body = body_view,
    .scope = context->scope,
  };

  Value *operator_token;
  Value *arguments[2] = {0};

  // prefix and postfix
  if (definition.length == 2) {
    Value *first =  value_view_get(definition, 0);
    bool is_first_operator_like =
      value_is_symbol(first) && value_as_symbol(first)->type == Symbol_Type_Operator_Like;
    user_defined_operator->fixity = is_first_operator_like ? Operator_Fixity_Prefix : Operator_Fixity_Postfix;
    user_defined_operator->argument_count = 1;
    if (user_defined_operator->fixity == Operator_Fixity_Prefix) {
      operator_token = value_view_get(definition, 0);
      arguments[0] = value_view_get(definition, 1);
    } else {
      operator_token = value_view_get(definition, 1);
      arguments[0] = value_view_get(definition, 0);
    }
  } else if (definition.length == 3) { // infix
    user_defined_operator->argument_count = 2;
    user_defined_operator->fixity = Operator_Fixity_Infix;
    operator_token = value_view_get(definition, 1);
    arguments[0] = value_view_get(definition, 0);
    arguments[1] = value_view_get(definition, 2);
  } else {
    operator_token = 0;
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = pattern_token->source_range,
      .detailed_message ="Expected the pattern to have two (for prefix / postfix) or three tokens"
    });
    goto err;
  }

  for (u8 i = 0; i < user_defined_operator->argument_count; ++i) {
    if (!value_is_symbol(arguments[i])) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = arguments[i]->source_range,
      });
      goto err;
    }
    user_defined_operator->argument_names[i] = value_as_symbol(arguments[i])->name;
  }

  Operator *operator = allocator_allocate(context->allocator, Operator);
  *operator = (Operator){
    .fixity = user_defined_operator->fixity,
    .precedence = precendence,
    .argument_count = user_defined_operator->argument_count,
    .handler = token_handle_user_defined_operator_proc,
    .handler_payload = user_defined_operator,
  };

  Slice operator_name = value_as_symbol(operator_token)->name;
  *context->result =
    scope_define_operator(context->scope, keyword_token->source_range, operator_name, operator);

  return peek_index;

  err:
  if (user_defined_operator) {
    allocator_deallocate(context->allocator, user_defined_operator, sizeof(*user_defined_operator));
  }
  return peek_index;
}

static inline Slice
mass_normalize_import_path(
  const Allocator *allocator,
  Slice raw
) {
  // @Speed
  char *bytes = allocator_allocate_bytes(allocator, raw.length, _Alignof(char));
  Slice normalized_slashes = {
    .bytes = bytes,
    .length = raw.length
  };
  // Copy and normalize the slashes
  for (u64 i = 0; i < raw.length; ++i) {
    bytes[i] = (raw.bytes[i] == '\\') ? '/' : raw.bytes[i];
  }
  return slice_normalize_path(allocator, normalized_slashes);
}

static Value *
mass_import(
  Execution_Context *context,
  Value_View args
) {
  if (args.length != 1) goto parse_err;
  Value *file_path_value = value_view_get(args, 0);
  if (file_path_value->descriptor != &descriptor_slice) goto parse_err;
  Slice file_path = *storage_static_as_c_type(&file_path_value->storage, Slice);

  Module *module;
  if (slice_equal(file_path, slice_literal("mass"))) {
    module = &context->compilation->compiler_module;
  } else {
    file_path = mass_normalize_import_path(context->allocator, file_path);
    Module **module_pointer = hash_map_get(context->compilation->module_map, file_path);
    if (module_pointer) {
      module = *module_pointer;
    } else {
      const Scope *root_scope = context->compilation->root_scope;
      Scope *module_scope = scope_make(context->allocator, root_scope);
      module = program_module_from_file(context, file_path, module_scope);
      Mass_Result module_result = program_import_module(context, module);
      MASS_ON_ERROR(module_result) {
        *context->result = module_result;
        return 0;
      }
      hash_map_set(context->compilation->module_map, file_path, module);
    }
  }

  return value_make(context, &descriptor_scope, storage_static(module->export.scope), args.source_range);

  parse_err:
  context_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = args.source_range,
    .detailed_message ="import() expects a single string argument"
  });
  return 0;
}

static u64
token_parse_syntax_definition(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("syntax"));
  Token_Expect_Match(statement_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("statement"));

  Token_Expect_Match(pattern_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);

  Value_View replacement = value_view_match_till_end_of_statement(view, &peek_index);
  Value_View definition = value_as_group(pattern_token)->children;

  Array_Macro_Pattern pattern = dyn_array_make(Array_Macro_Pattern);

  for (u64 i = 0; i < definition.length; ++i) {
    Value *value = value_view_get(definition, i);
    if (value_is_slice(value)) {
      const Slice *slice = value_as_slice(value);
      dyn_array_push(pattern, (Macro_Pattern) {
        .tag = Macro_Pattern_Tag_Single_Token,
        .Single_Token = {
          .token_pattern = {
            .tag = Token_Pattern_Tag_Symbol,
            .Symbol.name = *slice,
          }
        },
      });
    } else if (value_is_group(value)) {
      const Group *group = value_as_group(value);
      if (group->children.length) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = value->source_range,
          .detailed_message = "Nested group matches are not supported in syntax declarations"
        });
        goto err;
      }
      dyn_array_push(pattern, (Macro_Pattern) {
        .tag = Macro_Pattern_Tag_Single_Token,
        .Single_Token = {
          .token_pattern = {
            .tag = Token_Pattern_Tag_Group,
            .Group.tag = group->tag,
          }
        },
      });
    } else if (
      value_match_symbol(value, slice_literal("..@")) ||
      value_match_symbol(value, slice_literal(".@")) ||
      value_match_symbol(value, slice_literal("@"))
    ) {
      Value *symbol_token = value_view_peek(definition, ++i);
      if (!symbol_token || !value_is_symbol(symbol_token)) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Invalid_Identifier,
          .source_range = value->source_range,
        });
        goto err;
      }
      Macro_Pattern *last_pattern = 0;
      Slice symbol_name = value_as_symbol(value)->name;
      if (slice_equal(symbol_name, slice_literal("@"))) {
        last_pattern = dyn_array_last(pattern);
      } else if (slice_equal(symbol_name, slice_literal(".@"))) {
        last_pattern = dyn_array_push(pattern, (Macro_Pattern) {
          .tag = Macro_Pattern_Tag_Single_Token,
          .Single_Token = { .token_pattern = { .tag = Token_Pattern_Tag_Any } },
        });
      } else if (slice_equal(symbol_name, slice_literal("..@"))) {
        last_pattern = dyn_array_push(pattern, (Macro_Pattern) {
          .tag = Macro_Pattern_Tag_Any_Token_Sequence,
        });
      } else {
        panic("Internal Error: Unexpected @-like operator");
      }
      if (!last_pattern) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = value->source_range,
          .detailed_message = "@ requires a valid pattern before it"
        });
        goto err;
      }
      last_pattern->capture_name = value_as_symbol(symbol_token)->name;
    } else {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = value->source_range,
        .detailed_message = "Expected a string token"
      });
      goto err;
    }
  }
  Macro *macro = allocator_allocate(context->allocator, Macro);
  *macro = (Macro){
    .pattern = pattern,
    .replacement = replacement,
    .scope = context->scope
  };
  Token_Statement_Matcher *matcher =
    allocator_allocate(context->allocator, Token_Statement_Matcher);
  *matcher = (Token_Statement_Matcher){
    .previous = context->scope->statement_matcher,
    .proc = token_parse_macro_statement,
    .payload = macro,
  };
  context->scope->statement_matcher = matcher;
  return peek_index;

  err:
  dyn_array_destroy(pattern);
  return peek_index;
}

static bool
token_match_struct_field(
  Execution_Context *context,
  Value_View view,
  Slice *out_name,
  const Descriptor **out_descriptor
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(symbol, .tag = Token_Pattern_Tag_Symbol);
  Token_Match_Operator(define, ":");

  Value_View rest = value_view_rest(&view, peek_index);
  const Descriptor *field_descriptor = token_match_type(context, rest);
  if (!field_descriptor) return false;

  *out_name = value_as_symbol(symbol)->name;
  *out_descriptor = field_descriptor;

  return true;
}

static Value *
token_process_c_struct_definition(
  Execution_Context *context,
  Value *args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!value_match_group(args, Group_Tag_Paren)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args->source_range,
      .detailed_message = "c_struct must be followed by ()"
    });
    goto err;
  }
  const Group *args_group = value_as_group(args);
  if (args_group->children.length != 1) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args->source_range,
      .detailed_message = "c_struct expects one argument"
    });
    goto err;
  }
  Value *layout_block = value_view_get(args_group->children, 0);
  if (!value_match_group(layout_block, Group_Tag_Curly)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args->source_range,
      .detailed_message = "c_struct expects a {} block as the argument"
    });
    goto err;
  }

  Value *result = allocator_allocate(context->allocator, Value);

  u64 struct_bit_size = 0;
  u64 struct_bit_alignment = 0;
  Array_Memory_Layout_Item fields = dyn_array_make(Array_Memory_Layout_Item);

  const Group *layout_group = value_as_group(layout_block);
  if (layout_group->children.length != 0) {
    Value_View_Split_Iterator it = { .view = layout_group->children };
    while (!it.done) {
      Value_View field_view = token_split_next(&it, &token_pattern_semicolon);
      if (!field_view.length) continue;
      Slice field_name;
      const Descriptor *field_descriptor;
      if (!token_match_struct_field(context, field_view, &field_name, &field_descriptor)) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = field_view.source_range,
          .detailed_message = "Invalid field definition"
        });
        return 0;
      }

      u64 field_bit_alignment = descriptor_bit_alignment(field_descriptor);
      struct_bit_size = u64_align(struct_bit_size, field_bit_alignment);
      u64 field_bit_offset = struct_bit_size;
      struct_bit_size += descriptor_bit_size(field_descriptor);
      struct_bit_alignment = u64_max(struct_bit_alignment, field_bit_alignment);

      u64 field_byte_offset = (field_bit_offset + (CHAR_BIT - 1)) / CHAR_BIT;
      if (field_byte_offset * CHAR_BIT != field_bit_offset) {
        panic("TODO support non-byte aligned sizes");
      }

      dyn_array_push(fields, (Memory_Layout_Item) {
        .tag = Memory_Layout_Item_Tag_Base_Relative,
        .declaration = {
          // TODO provide source_range
          .name = field_name,
          .descriptor = field_descriptor,
        },
        .Base_Relative.offset = field_byte_offset,
      });
    }
  }

  struct_bit_size = u64_align(struct_bit_size, struct_bit_alignment);

  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .bit_size = struct_bit_size,
    .bit_alignment = struct_bit_alignment,
    .Struct = {
      .memory_layout = {
        .items = fields,
      }
    },
  };

  *result = type_value_for_descriptor(descriptor);
  return result;

  err:
  return 0;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

static inline u64
get_new_epoch() {
  static Atomic_u64 next_epoch = {0};
  u64 epoch = atomic_u64_increment(&next_epoch);
  return epoch;
}

static inline const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
) {
  if (value->descriptor == &descriptor_lazy_value) {
    Lazy_Value *lazy = storage_static_as_c_type(&value->storage, Lazy_Value);
    return lazy->descriptor;
  }
  return value->descriptor;
}

static Value *
compile_time_eval(
  Execution_Context *context,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  const Source_Range *source_range = &view.source_range;

  Jit *jit = &context->compilation->jit;
  Execution_Context eval_context = *context;
  eval_context.flags &= ~Execution_Context_Flags_Global;
  eval_context.epoch = get_new_epoch();
  eval_context.program = jit->program;
  eval_context.scope = scope_make(context->allocator, context->scope);

  static Slice eval_name = slice_literal_fields("$compile_time_eval$");
  Function_Info fn_info;
  function_info_init(&fn_info, eval_context.scope);

  const Calling_Convention *calling_convention = jit->program->default_calling_convention;
  Label_Index eval_label_index = make_label(jit->program, &jit->program->memory.code, slice_literal("compile_time_eval"));
  Function_Builder eval_builder = {
    .function = &fn_info,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .code_block = {
      .allocator = context->allocator,
      .start_label = eval_label_index,
      .end_label = make_label(jit->program, &jit->program->memory.code, slice_literal("compile_time_eval_end")),
    },
    .source = source_from_source_range(source_range),
  };

  Value *expression_result_value = token_parse_expression(&eval_context, view, &(u64){0}, 0);
  MASS_ON_ERROR(*eval_context.result) {
    context->result = eval_context.result;
    return 0;
  }
  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(expression_result_value);
  // Lazy evaluation should not generate any instructions
  assert(!eval_builder.code_block.first_bucket);

  Expected_Result expected_result = expected_result_any(result_descriptor);
  Value *forced_value = value_force(
    &eval_context, &eval_builder, &expected_result, expression_result_value
  );
  MASS_ON_ERROR(*context->result) return 0;

  // If we didn't generate any instructions there is no point
  // actually running the code, we can just take the resulting value
  if (!eval_builder.code_block.first_bucket) {
    return forced_value;
  }

  u64 result_byte_size = descriptor_byte_size(result_descriptor);
  u64 result_alignment = descriptor_byte_alignment(result_descriptor);
  void *result = result_byte_size // void type has zero size
    ? allocator_allocate_bytes(context->allocator, result_byte_size, result_alignment)
    : 0;

  // Load the address of the result
  Register out_register = register_acquire_temp(&eval_builder);
  Value out_value_register = {
    .descriptor = &descriptor_s64,
    .storage = storage_register_for_descriptor(out_register, &descriptor_void_pointer),
  };
  Value result_address = {
    .descriptor = &descriptor_s64,
    .storage = imm64((u64)result),
  };

  // Use memory-indirect addressing to copy
  Storage out_storage = storage_indirect(result_byte_size, out_register);
  Value *out_value = value_make(&eval_context, result_descriptor, out_storage, view.source_range);

  MASS_ON_ERROR(assign(&eval_context, &eval_builder, &out_value_register, &result_address)) {
    context->result = eval_context.result;
    return 0;
  }

  MASS_ON_ERROR(assign(&eval_context, &eval_builder, out_value, forced_value)) {
    context->result = eval_context.result;
    return 0;
  }

  calling_convention_x86_64_common_end_proc(jit->program, &eval_builder);
  dyn_array_push(jit->program->functions, eval_builder);

  Mass_Result jit_result = program_jit(context->compilation, jit);
  MASS_ON_ERROR(jit_result) {
    context_error(context, jit_result.Error.error);
    return 0;
  }

  fn_type_opaque jitted_code = c_function_from_label(jit->program, eval_label_index);
  jitted_code();

  return value_init(
    allocator_allocate(context->allocator, Value),
    out_value->descriptor,
    storage_static_internal(result, result_byte_size),
    view.source_range
  );
}

typedef struct {
  Source_Range source_range;
  Operator *operator;
} Operator_Stack_Entry;
typedef dyn_array_type(Operator_Stack_Entry) Array_Operator_Stack_Entry;

static Value *
token_handle_c_string(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *args_token
) {
  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_Ptr args = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = 16,
  );

  token_match_call_arguments(context, args_token, &args);
  Value *result_value = 0;
  if (dyn_array_length(args) != 1) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args_token->source_range,
      .detailed_message = "c_string expects a single compile-time known string"
    });
    goto defer;
  }
  Value *arg_value = *dyn_array_get(args, 0);
  const Slice *c_string = value_as_slice(arg_value);
  if (!c_string) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args_token->source_range,
      .detailed_message = "c_string expects a single compile-time known string"
    });
    goto defer;
  }

  const Value *c_string_bytes =
    value_global_c_string_from_slice(context, *c_string, arg_value->source_range);
  result_value = value_from_exact_expected_result(expected_result);
  load_address(context, builder, &arg_value->source_range, result_value, c_string_bytes->storage);

  defer:
  context_temp_reset_to_mark(context, temp_mark);

  return result_value;
}

static Number_Literal
mass_number_literal_logical_shift_left(
  Number_Literal input,
  Number_Literal shift
) {
  // TODO use proper bigint / bigdecimal library
  Number_Literal result = input;
  result.bits = input.bits << shift.bits;
  return result;
}

static Number_Literal
mass_number_literal_logical_shift_right(
  Number_Literal input,
  Number_Literal shift
) {
  // TODO use proper bigint / bigdecimal library
  Number_Literal result = input;
  result.bits = input.bits >> shift.bits;
  return result;
}

static Number_Literal
mass_number_literal_bitwise_and(
  Number_Literal a,
  Number_Literal b
) {
  // TODO use proper bigint / bigdecimal library
  Number_Literal result = a;
  result.bits = a.bits & b.bits;
  result.negative = a.negative & b.negative;
  return result;
}

static Number_Literal
mass_number_literal_bitwise_or(
  Number_Literal a,
  Number_Literal b
) {
  // TODO use proper bigint / bigdecimal library
  Number_Literal result = a;
  result.bits = a.bits | b.bits;
  result.negative = a.negative | b.negative;
  return result;
}

typedef struct {
  const Descriptor *target;
  Value *expression;
} Mass_Cast_Lazy_Payload;

static Value *
mass_handle_cast_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *target_descriptor = payload->target;
  Value *expression = payload->expression;
  const Descriptor *source_descriptor = value_or_lazy_value_descriptor(expression);
  const Source_Range *source_range = &expression->source_range;

  Expected_Result expected_source = expected_result_any(source_descriptor);
  Value *value = value_force(context, builder, &expected_source, expression);
  MASS_ON_ERROR(*context->result) return 0;

  u64 cast_to_byte_size = descriptor_byte_size(target_descriptor);
  u64 original_byte_size = descriptor_byte_size(source_descriptor);

  Value *result_value = value;
  if (value_is_static_number_literal(expression)) {
    result_value = token_value_force_immediate_integer(context, value, target_descriptor);
  } else if (cast_to_byte_size < original_byte_size) {
    result_value = value_make(context, target_descriptor, value->storage, *source_range);
    if (result_value->storage.tag == Storage_Tag_Static) {
      // TODO this is quite awkward and unsafe. There is probably a better way
      void *memory = (void *)storage_static_as_c_type_internal(&value->storage, original_byte_size);
      result_value->storage = storage_static_internal(memory, cast_to_byte_size);
    } else {
      result_value->storage.byte_size = cast_to_byte_size;
    }
    // TODO This is awkward and there might be a better way.
    //      It is also might be necessary to somehow mark the original value as invalid maybe?
    result_value->is_temporary = value->is_temporary;
  }

  return expected_result_ensure_value_or_temp(context, builder, expected_result, result_value);
}

static Value *
token_handle_cast(
  Execution_Context *context,
  Value *args_token
) {
  MASS_ON_ERROR(*context->result) return 0;
  Value_View_Split_Iterator it = { .view = value_as_group(args_token)->children };

  // First argument is treated as a type
  Value_View type_view = token_split_next(&it, &token_pattern_comma_operator);
  const Descriptor *target_descriptor = token_match_type(context, type_view);

  if (!descriptor_is_integer(target_descriptor)) {
    // TODO support saying that we want any integer type
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = it.view.source_range,
      .Type_Mismatch = { .expected = &descriptor_s64, .actual = target_descriptor },
    });
    return 0;
  }

  if (it.done) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = it.view.source_range,
      .detailed_message = "cast() expects two arguments"
    });
    return 0;
  }

  Value_View expression_view = token_split_next(&it, &token_pattern_comma_operator);
  Value *expression = token_parse_expression(context, expression_view, &(u64){0}, 0);

  if (!it.done) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = it.view.source_range,
      .detailed_message = "cast() expects two arguments"
    });
    return 0;
  }

  Mass_Cast_Lazy_Payload lazy_payload = {
    .target = target_descriptor,
    .expression = expression,
  };

  if (value_is_non_lazy_static(expression)) {
    Expected_Result expected_result = expected_result_static(target_descriptor);
    return mass_handle_cast_lazy_proc(context, 0, &expected_result, &lazy_payload);
  } else {
    Mass_Cast_Lazy_Payload *heap_payload = allocator_allocate(context->allocator, Mass_Cast_Lazy_Payload);
    *heap_payload = lazy_payload;

    return mass_make_lazy_value(
      context, it.view.source_range, heap_payload, target_descriptor, mass_handle_cast_lazy_proc
    );
  }
}

static void
token_dispatch_operator(
  Execution_Context *context,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *operator_entry
);

static bool
token_handle_operator(
  Execution_Context *context,
  Value_View view,
  Array_Value_Ptr *stack,
  Array_Operator_Stack_Entry *operator_stack,
  Operator *operator,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  while (dyn_array_length(*operator_stack)) {
    Operator_Stack_Entry *last_operator = dyn_array_last(*operator_stack);

    if (last_operator->operator->precedence < operator->precedence) break;
    if (last_operator->operator->precedence == operator->precedence) {
      if (last_operator->operator->associativity != Operator_Associativity_Left) {
        break;
      }
    }

    dyn_array_pop(*operator_stack);

    // apply the operator on the stack
    token_dispatch_operator(context, stack, last_operator);
  }
  dyn_array_push(*operator_stack, (Operator_Stack_Entry) {
    .source_range = source_range,
    .operator = operator,
  });
  return true;
}

static u64
token_parse_constant_definitions(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value_View lhs;
  Value_View rhs;
  Value *operator;

  u64 statement_length = 0;
  view = value_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal("::"), &lhs, &rhs, &operator)) {
    return 0;
  }
  if (lhs.length > 1) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unimplemented,
      .source_range = lhs.source_range,
      .detailed_message = "Multiple assignment are not supported at the moment"
    });
    goto err;
  }
  Value *symbol = value_view_get(view, 0);

  if (!value_is_symbol(symbol)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Invalid_Identifier,
      .source_range = symbol->source_range,
    });
    goto err;
  }

  Slice name = value_as_symbol(symbol)->name;
  scope_define_lazy_compile_time_expression(context, context->scope, name, rhs);

  err:
  return statement_length;
}

static u64
register_bitset_from_storage(
  const Storage *storage
) {
  u64 result = 0;
  switch(storage->tag) {
    case Storage_Tag_None:
    case Storage_Tag_Static: {
      // Nothing to do
      break;
    }
    default:
    case Storage_Tag_Any:
    case Storage_Tag_Eflags: {
      panic("Internal Error: Unexpected storage type for a function argument");
      break;
    }
    case Storage_Tag_Register:
    case Storage_Tag_Xmm: {
      Register reg_index = storage->Register.index;
      register_bitset_set(&result, reg_index);
    } break;
    case Storage_Tag_Memory: {
      if (storage->Memory.location.tag == Memory_Location_Tag_Indirect) {
        Register reg_index = storage->Memory.location.Indirect.base_register;
        register_bitset_set(&result, reg_index);
      }
    } break;
    case Storage_Tag_Unpacked: {
      register_bitset_set(&result, storage->Unpacked.registers[0]);
      register_bitset_set(&result, storage->Unpacked.registers[1]);
    } break;
  }
  return result;
}

static Value *
mass_macro_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *body_value
) {
  Value *result_value = 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      result_value = value_from_exact_expected_result(expected_result);
      break;
    }
    case Expected_Result_Tag_Flexible: {
      const Descriptor *return_descriptor = expected_result_descriptor(expected_result);
      // FIXME :ExpectedStack
      result_value = return_descriptor == &descriptor_void
        ? &void_value
        : reserve_stack(context, builder, return_descriptor, body_value->source_range);
      break;
    }
  }

  Label_Index saved_return_label = builder->code_block.end_label;
  Value *saved_return_value = builder->return_value;
  {
    builder->code_block.end_label =
      make_label(context->program, &context->program->memory.code, slice_literal("macro return"));
    builder->return_value = result_value;
    result_value = value_force(context, builder, expected_result, body_value);
    MASS_ON_ERROR(*context->result) return 0;

    push_label(
      &builder->code_block,
      body_value->source_range,
      builder->code_block.end_label
    );
  }
  builder->code_block.end_label = saved_return_label;
  builder->return_value = saved_return_value;

  return result_value;
}

static inline Value *
mass_handle_macro_call(
  Execution_Context *context,
  Value *overload,
  Value_View args_view,
  Source_Range source_range
) {
  const Function_Literal *literal = value_as_function_literal(overload);
  assert(literal->flags & Function_Literal_Flags_Macro);

  // We make a nested scope based on function's original scope
  // instead of current scope for hygiene reasons. I.e. function body
  // should not have access to locals inside the call scope.
  Scope *body_scope = scope_make(context->allocator, literal->info->scope);

  for(u64 i = 0; i < dyn_array_length(literal->info->parameters); ++i) {
    MASS_ON_ERROR(*context->result) return 0;
    Function_Parameter *arg = dyn_array_get(literal->info->parameters, i);
    if (arg->declaration.name.length) {
      Value *arg_value;
      if (i >= args_view.length) {
        // We should catch the missing default expression in the matcher
        Value_View default_expression = arg->maybe_default_expression;
        assert(default_expression.length);
        Execution_Context arg_context = *context;
        arg_context.scope = body_scope;
        arg_value = token_parse_expression(&arg_context, default_expression, &(u64){0}, 0);
      } else {
        arg_value = value_view_get(args_view, i);
      }

      arg_value = maybe_coerce_number_literal_to_integer(context, arg_value, arg->declaration.descriptor);
      u64 arg_epoch = value_is_non_lazy_static(arg_value) ? VALUE_STATIC_EPOCH : context->epoch;
      scope_define_value(body_scope, arg_epoch, arg_value->source_range, arg->declaration.name, arg_value);
    }
  }

  Execution_Context body_context = *context;
  body_context.scope = body_scope;
  Value *body_value = token_parse_block_no_scope(&body_context, literal->body);
  MASS_ON_ERROR(*context->result) return 0;

  const Descriptor *return_descriptor = value_or_lazy_value_descriptor(body_value);
  if (
    literal->info->returns.declaration.descriptor &&
    !same_type_or_can_implicitly_move_cast(literal->info->returns.declaration.descriptor, return_descriptor)
  ) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = body_value->source_range,
      .Type_Mismatch = {
        .expected = literal->info->returns.declaration.descriptor,
        .actual = return_descriptor,
      },
    });
    return 0;
  }

  if (value_is_non_lazy_static(body_value)) {
    return body_value;
  }
  return mass_make_lazy_value(
    context, source_range, body_value, return_descriptor, mass_macro_lazy_proc
  );
}

typedef struct {
  Storage reg;
  Storage stack;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

typedef struct {
  Array_Value_Ptr args;
  Value *overload;
  Source_Range source_range;
} Mass_Function_Call_Lazy_Payload;

static Value *
call_function_overload(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_Function_Call_Lazy_Payload *payload
) {
  const Source_Range *source_range = &payload->source_range;
  Value *to_call = payload->overload;
  Array_Value_Ptr arguments = payload->args;

  Value_View args_view = value_view_from_value_array(arguments, source_range);
  Value *instance = ensure_function_instance(context, to_call, args_view);
  MASS_ON_ERROR(*context->result) return 0;
  assert(instance->descriptor->tag == Descriptor_Tag_Function_Instance);
  const Descriptor_Function_Instance *instance_descriptor = &instance->descriptor->Function_Instance;
  const Function_Info *fn_info = instance_descriptor->info;

  Value *fn_return_value = instance_descriptor->call_setup.caller_return_value;
  if (fn_info->returns.declaration.descriptor != &descriptor_void) {
    fn_return_value->is_temporary = true;
  }

  const Storage *maybe_expected_storage = 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      maybe_expected_storage = &value_from_exact_expected_result(expected_result)->storage;
      break;
    }
    case Expected_Result_Tag_Flexible: {
      break;
    }
  }

  const Function_Call_Setup *call_setup = &instance_descriptor->call_setup;

  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_Ptr temp_arguments = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = dyn_array_length(call_setup->arguments_layout.items),
  );
  Array_Value target_params = dyn_array_make(
    Array_Value,
    .allocator = context->temp_allocator,
    .capacity = dyn_array_length(call_setup->arguments_layout.items),
  );
  Array_Saved_Register stack_saved_registers = dyn_array_make(
    Array_Saved_Register,
    .allocator = context->temp_allocator,
    .capacity = 32,
  );

  Scope *default_arguments_scope = scope_make(context->allocator, fn_info->scope);
  Storage stack_argument_base = storage_stack(0, 1, Stack_Area_Call_Target_Argument);

  u64 all_used_arguments_register_bitset = 0;
  DYN_ARRAY_FOREACH(Memory_Layout_Item, target_item, call_setup->arguments_layout.items) {
    Storage storage = memory_layout_item_storage(
      &stack_argument_base, &call_setup->arguments_layout, target_item
    );
    if (storage_is_stack(&storage)) {
      assert(storage.Memory.location.Stack.area != Stack_Area_Local);
      storage.Memory.location.Stack.area = Stack_Area_Call_Target_Argument;
    }
    Value *param = dyn_array_push_uninitialized(target_params);
    value_init(param, target_item->declaration.descriptor, storage, target_item->declaration.source_range);

    // TODO avoid doing this twice - here and below
    u64 target_arg_register_bitset = register_bitset_from_storage(&storage);
    if(all_used_arguments_register_bitset & target_arg_register_bitset) {
      panic("Found overlapping register usage in arguments");
    }
    all_used_arguments_register_bitset |= target_arg_register_bitset;
  }

  u64 argument_register_bitset = 0;
  u64 copied_straight_to_param_bitset = 0;
  u64 temp_register_argument_bitset = 0;
  for (u64 i = 0; i < dyn_array_length(target_params); ++i) {
    Memory_Layout_Item *target_item = dyn_array_get(call_setup->arguments_layout.items, i);
    Value *target_arg = dyn_array_get(target_params, i);
    Value *source_arg;
    if (i >= dyn_array_length(arguments)) {
      if (target_item->flags & Memory_Layout_Item_Flags_Uninitialized) {
        source_arg = &void_value;
      } else {
        Function_Parameter *declared_argument = dyn_array_get(fn_info->parameters, i);
        Value_View default_expression = declared_argument->maybe_default_expression;
        assert(default_expression.length);
        Execution_Context arg_context = *context;
        arg_context.scope = default_arguments_scope;
        source_arg = token_parse_expression(&arg_context, default_expression, &(u64){0}, 0);
        MASS_ON_ERROR(*arg_context.result) return 0;
      }
    } else {
      source_arg = *dyn_array_get(arguments, i);
    }
    source_arg = maybe_coerce_number_literal_to_integer(
      context, source_arg, target_item->declaration.descriptor
    );
    const Descriptor *stack_descriptor = target_item->declaration.descriptor;
    if (stack_descriptor->tag == Descriptor_Tag_Reference_To) {
      stack_descriptor = stack_descriptor->Reference_To.descriptor;
    }
    bool source_is_stack = (
      storage_is_stack(&source_arg->storage) &&
      source_arg->descriptor->tag != Descriptor_Tag_Reference_To
    );
    bool should_assign = !(target_item->flags & Memory_Layout_Item_Flags_Uninitialized);

    u64 target_arg_register_bitset = register_bitset_from_storage(&target_arg->storage);
    if (target_arg_register_bitset >> 16) {
      panic("Found XMM usage");
    }

    argument_register_bitset |= target_arg_register_bitset;
    bool target_arg_registers_are_free =
      !(builder->register_occupied_bitset & target_arg_register_bitset);
    bool can_assign_straight_to_target = (
      target_arg_registers_are_free &&
      target_item->declaration.descriptor->tag != Descriptor_Tag_Reference_To
    );

    bool can_use_source_registers = false;
    u64 source_registers_bitset = 0;
    if (
      source_arg->descriptor != &descriptor_lazy_value &&
      source_arg->storage.tag != Storage_Tag_Static &&
      target_arg->descriptor->tag != Descriptor_Tag_Reference_To
    ) {
      source_registers_bitset = register_bitset_from_storage(&source_arg->storage);
      if (!(all_used_arguments_register_bitset & source_registers_bitset)) {
        if (!(temp_register_argument_bitset & source_registers_bitset)) {
          can_use_source_registers = true;
        }
      }
    }

    Value *arg_value;
    if (storage_is_stack(&target_arg->storage)) {
      arg_value = value_make(context, stack_descriptor, target_arg->storage, *source_range);
    } else if (source_is_stack) {
      arg_value = source_arg;
      should_assign = false;
    } else if (can_use_source_registers) {
      arg_value = source_arg;
      should_assign = false;
      register_acquire_bitset(builder, source_registers_bitset);
    } else if (
      value_is_non_lazy_static(source_arg) &&
      target_item->declaration.descriptor->tag != Descriptor_Tag_Reference_To
    ) {
      arg_value = source_arg;
      should_assign = false;
    } else if (can_assign_straight_to_target) {
      arg_value = target_arg;
      copied_straight_to_param_bitset |= target_arg_register_bitset;
      register_acquire_bitset(builder, target_arg_register_bitset);
    } else {
      u64 prohibited_registers
        = temp_register_argument_bitset
        | all_used_arguments_register_bitset
        | builder->register_occupied_bitset;
      u64 allowed_temp_registers = registers_that_can_be_temp & ~prohibited_registers;
      u64 required_register_count = register_bitset_occupied_count(target_arg_register_bitset);
      if (
        // TODO it should be possible to do this for unpacked structs as well,
        //      but it will be quite gnarly
        target_item->declaration.descriptor->tag != Descriptor_Tag_Reference_To &&
        required_register_count == 1 &&
        register_bitset_occupied_count(allowed_temp_registers) > 1
      ) {
        u64 temp_register = register_find_available(builder, prohibited_registers);
        register_acquire(builder, temp_register);
        register_bitset_set(&temp_register_argument_bitset, temp_register);
        arg_value = value_register_for_descriptor(
          context, temp_register, target_item->declaration.descriptor, target_item->declaration.source_range
        );
        arg_value->is_temporary = true;
      } else {
        // The code below is useful to check how many spills to stack happen
        //static int stack_counter = 0;
        //printf(" > stack %i\n", stack_counter++);
        arg_value = reserve_stack(context, builder, stack_descriptor, *source_range);
        arg_value->is_temporary = true;
      }
    };
    if (should_assign) {
      MASS_ON_ERROR(assign(context, builder, arg_value, source_arg)) return 0;
    }
    dyn_array_push(temp_arguments, arg_value);
    Slice name = target_item->declaration.name;
    if (name.length) {
      scope_define_value(default_arguments_scope, context->epoch, arg_value->source_range, name, arg_value);
    }
  }

  u64 target_volatile_registers_bitset = call_setup->calling_convention->register_volatile_bitset;
  u64 saved_registers_bitset = 0;
  u64 expected_result_bitset = maybe_expected_storage
    ? register_bitset_from_storage(maybe_expected_storage)
    : 0;

  for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
    if (!register_bitset_get(target_volatile_registers_bitset, reg_index)) continue;
    if (!register_bitset_get(builder->register_occupied_bitset, reg_index)) continue;
    if (register_bitset_get(copied_straight_to_param_bitset, reg_index)) continue;
    if (register_bitset_get(temp_register_argument_bitset, reg_index)) continue;

    register_bitset_set(&saved_registers_bitset, reg_index);

    // We must not save the register that we will overwrite with the result
    // otherwise we will overwrite it with the restored value
    if (register_bitset_get(expected_result_bitset, reg_index)) continue;

    Saved_Register *saved = dyn_array_push(stack_saved_registers, (Saved_Register) {
      .reg = storage_register_for_descriptor(reg_index, &descriptor_void_pointer),
      .stack = reserve_stack_storage(builder, descriptor_byte_size(&descriptor_void_pointer)),
    });

    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mov, {saved->stack, saved->reg}}
    );
  }

  register_release_bitset(builder, saved_registers_bitset);
  u64 spilled_param_register_bitset = argument_register_bitset & ~copied_straight_to_param_bitset;
  register_acquire_bitset(builder, spilled_param_register_bitset);

  for (u64 i = 0; i < dyn_array_length(target_params); ++i) {
    Value *param = dyn_array_get(target_params, i);

    if (storage_is_stack(&param->storage)) continue;

    Value *source_arg = *dyn_array_get(temp_arguments, i);
    MASS_ON_ERROR(assign(context, builder, param, source_arg)) return 0;
  }

  builder->max_call_parameters_stack_size = u32_max(
    builder->max_call_parameters_stack_size,
    call_setup->parameters_stack_size
  );


  switch(call_setup->jump.tag) {
    case Function_Call_Jump_Tag_Call: {
      if (instance->storage.tag == Storage_Tag_Static) {
        Register temp_reg = register_acquire_temp(builder);
        Storage reg = storage_register_for_descriptor(temp_reg, &descriptor_void_pointer);
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){mov, {reg, instance->storage}}
        );
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){call, {reg}}
        );
        register_release(builder, temp_reg);
      } else {
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){call, {instance->storage}}
        );
      }
    } break;
    case Function_Call_Jump_Tag_Syscall: {
      assert(instance->storage.tag == Storage_Tag_None);
      Storage syscal_number_storage = storage_register_for_descriptor(Register_A, &descriptor_s64);
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){mov, {syscal_number_storage, imm64(call_setup->jump.Syscall.number)}}
      );
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){asm_syscall}
      );
    } break;
  }

  register_release_bitset(builder, argument_register_bitset | temp_register_argument_bitset);

  u64 return_value_bitset = register_bitset_from_storage(&fn_return_value->storage);
  register_acquire_bitset(builder, return_value_bitset);

  Value *expected_value =
    expected_result_ensure_value_or_temp(context, builder, expected_result, fn_return_value);


  // This is awkward that we need to manually clear return bitset because
  // it *might* be set depending on whether the expected result it a temp.
  // The whole thing feels a bit fishy but not sure what is the fix.
  if (expected_result->tag == Expected_Result_Tag_Exact) {
    builder->register_occupied_bitset &= ~return_value_bitset;
  }

  DYN_ARRAY_FOREACH(Saved_Register, saved, stack_saved_registers) {
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mov, {saved->reg, saved->stack}}
    );
  }

  register_acquire_bitset(builder, saved_registers_bitset);

  context_temp_reset_to_mark(context, temp_mark);

  return expected_value;
}

struct Overload_Match_State {
  Value *value;
  const Function_Info *info;
  s64 score;
};

static void
ensure_parameter_descriptors(
  const Execution_Context *context,
  Function_Info *info
) {
  Execution_Context temp_context = *context;
  Temp_Mark temp_mark = context_temp_mark(&temp_context);

  temp_context.scope = scope_make(temp_context.temp_allocator, temp_context.scope);

  DYN_ARRAY_FOREACH(Function_Parameter, param, info->parameters) {
    if (!param->declaration.descriptor) {
      assert(param->maybe_type_expression.length);
      param->declaration.descriptor =
        token_match_type(&temp_context, param->maybe_type_expression);
      MASS_ON_ERROR(*temp_context.result) goto err;
    }
    Source_Range source_range = param->declaration.source_range;
    Value *param_value = value_init(
      allocator_allocate(temp_context.temp_allocator, Value),
      param->declaration.descriptor,
      storage_none,
      source_range
    );
    scope_define_value(
      temp_context.scope,
      VALUE_STATIC_EPOCH,
      source_range,
      param->declaration.name,
      param_value
    );
  }
  if (!info->returns.declaration.descriptor) {
    assert(info->returns.maybe_type_expression.length);
    info->returns.declaration.descriptor =
      token_match_type(&temp_context, info->returns.maybe_type_expression);
    assert(info->returns.declaration.descriptor);
    MASS_ON_ERROR(*temp_context.result) goto err;
  }

  err:
  context_temp_reset_to_mark(&temp_context, temp_mark);
}

static bool
match_overload_argument_count(
  const Function_Info *descriptor,
  u64 actual_count
) {
  if (actual_count > dyn_array_length(descriptor->parameters)) return false;
  for (u64 arg_index = 0; arg_index < dyn_array_length(descriptor->parameters); ++arg_index) {
    Function_Parameter *param = dyn_array_get(descriptor->parameters, arg_index);
    if (arg_index < actual_count) continue;
    if (!param->maybe_default_expression.length) return false;
  }
  return true;
}

static void
mass_match_overload_candidate(
  Value *candidate,
  Value_View args,
  struct Overload_Match_State *match,
  struct Overload_Match_State *best_conflict_match
) {
  if (candidate->descriptor == &descriptor_overload_set) {
    const Overload_Set *set = storage_static_as_c_type(&candidate->storage, Overload_Set);
    for (u64 i = 0; i < dyn_array_length(set->items); i += 1) {
      Value *overload = *dyn_array_get(set->items, i);
      mass_match_overload_candidate(overload, args, match, best_conflict_match);
    }
  } else {
    const Function_Info *overload_info = 0;
    s64 score;

    // If the literal wouldn't match based on the number of arguments
    // then there is no point to try to specialize it
    if (
      value_is_function_literal(candidate) &&
      !match_overload_argument_count(value_as_function_literal(candidate)->info, args.length)
    ) {
      score = -1;
    } else {
      overload_info = maybe_function_info_from_value(candidate, args);
      if (overload_info->flags & Descriptor_Function_Flags_Intrinsic) {
        score = 0;
      } else {
        score = calculate_arguments_match_score(overload_info, args);
      }
    }
    if (score > match->score) {
      match->info = overload_info;
      match->value = candidate;
      match->score = score;
    } else {
      if (score == match->score && score > best_conflict_match->score) {
        *best_conflict_match = *match;
        match->info = overload_info;
        match->value = candidate;
      }
    }
  }
}

static Overload_Match
mass_match_overload(
  Value *value,
  Value_View args
) {
  struct Overload_Match_State match = { .score = -1 };
  struct Overload_Match_State best_conflict_match = match;
  mass_match_overload_candidate(value, args, &match, &best_conflict_match);

  if (match.score == -1) {
    return (Overload_Match){.tag = Overload_Match_Tag_No_Match};
  }
  if (match.score == best_conflict_match.score) {
    return (Overload_Match){
      .tag = Overload_Match_Tag_Undecidable,
      .Undecidable = { match.info, best_conflict_match.info },
    };
  }
  return (Overload_Match){
    .tag = Overload_Match_Tag_Found,
    .Found = {
      .value = match.value,
      .info = match.info,
    },
  };
}

static Value *
mass_intrinsic_call(
  Execution_Context *context,
  Value *overload,
  Value_View args_view
) {
  Jit *jit = &context->compilation->jit;
  Execution_Context eval_context = *context;
  eval_context.flags &= ~Execution_Context_Flags_Global;
  eval_context.program = jit->program;
  eval_context.scope = context->scope;

  Value *instance = ensure_function_instance(&eval_context, overload, (Value_View){0});
  MASS_ON_ERROR(*eval_context.result) return 0;

  Mass_Result jit_result = program_jit(context->compilation, jit);
  MASS_ON_ERROR(jit_result) {
    context_error(context, jit_result.Error.error);
    return 0;
  }
  fn_type_opaque jitted_code;
  if (storage_is_label(&instance->storage)) {
    Label_Index jit_label_index =
      instance->storage.Memory.location.Instruction_Pointer_Relative.label_index;
    jitted_code = c_function_from_label(jit->program, jit_label_index);
  } else {
    u64 absolute_address = storage_static_value_up_to_u64(&instance->storage);
    jitted_code = (fn_type_opaque)absolute_address;
  }

  // @Volatile :IntrinsicFunctionSignature
  Value *(*jitted_intrinsic)(Execution_Context *, Value_View) =
    (Value *(*)(Execution_Context *, Value_View))jitted_code;
  Value *result = jitted_intrinsic(&eval_context, args_view);

  // The value that comes out of an intrinsic is consider to originate in the same
  // epoch as the function, which is true since we evaluate right at this point.
  if (result && result->descriptor == &descriptor_lazy_value) {
    // TODO get rid of this cast somehow
    Lazy_Value *lazy = (Lazy_Value *)storage_static_as_c_type(&result->storage, Lazy_Value);
    lazy->epoch = context->epoch;
  }
  return result;
}

static bool
value_is_intrinsic(
  Value *value
) {
  const Function_Info *info = maybe_function_info_from_value(value, (Value_View){0});
  return !!(info && info->flags & Descriptor_Function_Flags_Intrinsic);
}

static Value *
token_handle_function_call(
  Execution_Context *context,
  Value *target_expression,
  Value_View args_view,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (target_expression->descriptor == &descriptor_macro_capture) {
    Macro_Capture *capture = storage_static_as_c_type(&target_expression->storage, Macro_Capture);
    Execution_Context capture_context = *context;
    capture_context.scope = capture->scope;
    if (args_view.length == 0) {
      // Nothing to do
    } else if (args_view.length == 1) {
      Value *scope_arg = value_view_get(args_view, 0);
      if (scope_arg->descriptor != &descriptor_scope) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = scope_arg->source_range,
          .Type_Mismatch = { .expected = &descriptor_scope, .actual = scope_arg->descriptor },
          .detailed_message = "Macro capture can only accept an optional Scope argument",
        });
        return 0;
      }
      Scope *argument_scope = storage_static_as_c_type(&scope_arg->storage, Scope);
      use_scope(&capture_context, argument_scope);
    } else {
      Array_Value_Ptr error_args = value_view_to_value_array(context->allocator, args_view);
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_No_Matching_Overload,
        .source_range = source_range,
        .No_Matching_Overload = { .target = target_expression, .arguments = error_args },
        .detailed_message = "Macro capture can only accept an optional Scope argument"
      });
      return 0;
    }
    return token_parse_block_view(&capture_context, capture->view);
  }

  Overload_Match match = mass_match_overload(target_expression, args_view);

  Value *overload;
  const Function_Info *info;
  switch(match.tag) {
    case Overload_Match_Tag_No_Match: {
      Array_Value_Ptr error_args = value_view_to_value_array(context->allocator, args_view);
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_No_Matching_Overload,
        .source_range = source_range,
        .No_Matching_Overload = { .target = target_expression, .arguments = error_args },
      });
      return 0;
    }
    case Overload_Match_Tag_Undecidable: {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Undecidable_Overload,
        .Undecidable_Overload = { match.Undecidable.a, match.Undecidable.b },
        .source_range = source_range,
      });
      return 0;
    }
    case Overload_Match_Tag_Found: {
      overload = match.Found.value;
      info = match.Found.info;
      break;
    }
    default: {
      overload = 0;
      info = 0;
      panic("Unexpected Overload_Match_Tag");
      break;
    }
  }

  const Function_Literal *maybe_literal = 0;
  if (value_is_function_literal(overload)) maybe_literal = value_as_function_literal(overload);

  MASS_ON_ERROR(*context->result) return 0;
  if (
    overload != context->current_compile_time_function_call_target &&
    info && (info->flags & Descriptor_Function_Flags_Compile_Time)
  ) {
    // It is important to create a new value with the range of the original expression,
    // otherwise Value_View slicing will not work correctly
    Value *temp_overload = value_make(context, overload->descriptor, overload->storage, source_range);
    // This is necessary to avoid infinite recursion as the compile_time_eval called below
    // will end up here as well. Indirect calls are allowed so we do not need a full stack
    const Value *saved_call_target = context->current_compile_time_function_call_target;
    context->current_compile_time_function_call_target = temp_overload;
    Value *result;
    if (info->flags & Descriptor_Function_Flags_Intrinsic) {
      result = mass_intrinsic_call(context, overload, args_view);
    } else {
      if (maybe_literal && value_is_intrinsic(maybe_literal->body)) {
        result = mass_intrinsic_call(context, maybe_literal->body, args_view);
      } else {
        Value *fake_args_token = value_make(
          context, &descriptor_value_view, storage_static(&args_view), args_view.source_range
        );
        Value_View fake_eval_view = {
          .values = (Value *[]){temp_overload, fake_args_token},
          .length = 2,
          .source_range = source_range,
        };
        result = compile_time_eval(context, fake_eval_view);
      }
    }
    context->current_compile_time_function_call_target = saved_call_target;
    return result;
  }

  if (maybe_literal && (maybe_literal->flags & Function_Literal_Flags_Macro)) {
    return mass_handle_macro_call(context, overload, args_view, source_range);
  }

  Mass_Function_Call_Lazy_Payload *call_payload =
    allocator_allocate(context->allocator, Mass_Function_Call_Lazy_Payload);
  *call_payload = (Mass_Function_Call_Lazy_Payload){
    .overload = overload,
    .args = value_view_to_value_array(context->allocator, args_view),
    .source_range = source_range,
  };

  return mass_make_lazy_value(
    context, source_range, call_payload, info->returns.declaration.descriptor, call_function_overload
  );
}

static Value *
token_handle_parsed_function_call(
  Execution_Context *context,
  Value *target_token,
  Value *args_token,
  Source_Range source_range
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *call_return_value = 0;
  Temp_Mark temp_mark = context_temp_mark(context);

  Value *target_expression = token_parse_single(context, target_token);
  MASS_ON_ERROR(*context->result) goto defer;

  Value_View args_view;
  if(value_match_group(args_token, Group_Tag_Paren)) {
    Array_Value_Ptr temp_args = dyn_array_make(
      Array_Value_Ptr,
      .allocator = context->temp_allocator,
      .capacity = 32,
    );
    token_match_call_arguments(context, args_token, &temp_args);
    MASS_ON_ERROR(*context->result) goto defer;
    args_view = value_view_from_value_array(temp_args, &source_range);
  } else if (args_token->descriptor == &descriptor_value_view) {
    if (!value_is_non_lazy_static(args_token)) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Expected_Static,
        .source_range = args_token->source_range,
        .detailed_message = "Expected a static Value_View",
      });
      goto defer;
    }
    args_view = *storage_static_as_c_type(&args_token->storage, Value_View);
  } else {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args_token->source_range,
      .detailed_message = "Expected a list of arguments in ()",
    });
    goto defer;
  }

  call_return_value = token_handle_function_call(context, target_expression, args_view, source_range);

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return call_return_value;
}

static Storage
storage_load_index_address(
  Execution_Context *context,
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *target,
  const Descriptor *item_descriptor,
  Value *index_value
) {
  // @InstructionQuality
  // This code is very general in terms of the operands where the base
  // or the index are stored, but it is

  s32 item_byte_size = u64_to_s32(descriptor_byte_size(item_descriptor));

  if (target->storage.tag == Storage_Tag_Static) {
    index_value = token_value_force_immediate_integer(context, index_value, &descriptor_u64);
    MASS_ON_ERROR(*context->result) return (Storage){0};
    u64 index = *storage_static_as_c_type(&index_value->storage, u64);
    // TODO do bounds checking if possible
    s8 *target_bytes = 0;
    if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
      target_bytes =
        *(s8**)storage_static_as_c_type_internal(&target->storage, target->storage.byte_size);
    } else {
      panic("TODO support more static dereferences");
    }
    return storage_static_internal(target_bytes + index, item_byte_size);
  }

  // @Volatile :TemporaryRegisterForIndirectMemory
  Value *new_base = value_temporary_acquire_register_for_descriptor(
    context, builder, register_find_available(builder, 0),  &descriptor_s64, index_value->source_range
  );

  // Move the index into the register
  move_value(context->allocator, builder, source_range, &new_base->storage, &index_value->storage);

  // Multiplication by 1
  if (item_byte_size != 1) {
    Value *byte_size_value = value_from_s32(context, item_byte_size, index_value->source_range);

    // Multiply index by the item byte size
    Value *reg_byte_size_value = value_temporary_acquire_register_for_descriptor(
      context, builder, register_find_available(builder, 0), &descriptor_s64, index_value->source_range
    );

    move_value(
      context->allocator, builder, source_range,
      &reg_byte_size_value->storage, &byte_size_value->storage
    );

    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){imul, {new_base->storage, reg_byte_size_value->storage}}
    );
    value_release_if_temporary(builder, reg_byte_size_value);
  }

  {
    // @InstructionQuality
    // TODO If the source does not have index, on X64 it should be possible to avoid
    //      using an extra register and put the index into SIB

    // Load previous address into a temp register
    Value *temp_value = value_temporary_acquire_register_for_descriptor(
      context, builder, register_find_available(builder, 0), &descriptor_void_pointer, index_value->source_range
    );

    if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
      move_value(context->allocator, builder, source_range, &temp_value->storage, &target->storage);
    } else {
      assert(target->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
      load_address(context, builder, source_range, temp_value, target->storage);
    }

    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){add, {new_base->storage, temp_value->storage}}
    );
    value_release_if_temporary(builder, temp_value);
  }

  return storage_indirect(item_byte_size, new_base->storage.Register.index);
}

typedef enum {
  Mass_Arithmetic_Operator_Add = 1,
  Mass_Arithmetic_Operator_Subtract = 2,
  Mass_Arithmetic_Operator_Multiply = 3,
  Mass_Arithmetic_Operator_Divide = 4,
  Mass_Arithmetic_Operator_Remainder = 5,
} Mass_Arithmetic_Operator;

typedef struct {
  Mass_Arithmetic_Operator operator;
  Value *lhs;
  Value *rhs;
} Mass_Arithmetic_Operator_Lazy_Payload;

static Value *
mass_handle_arithmetic_operation_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_Arithmetic_Operator_Lazy_Payload *payload
) {
  const Descriptor *descriptor = expected_result_descriptor(expected_result);
  assert(descriptor_is_integer(descriptor));

  const Source_Range result_range = payload->lhs->source_range;
  Value *lhs = payload->lhs;
  Value *rhs = payload->rhs;

  switch(payload->operator) {
    case Mass_Arithmetic_Operator_Add:
    case Mass_Arithmetic_Operator_Subtract: {
      if (payload->operator == Mass_Arithmetic_Operator_Add) {
        maybe_constant_fold(context, builder, &result_range, expected_result, lhs, rhs, +);
      } else {
        maybe_constant_fold(context, builder, &result_range, expected_result, lhs, rhs, -);
      }

      // Try to reuse result_value if we can
      // TODO should be able to reuse memory and register operands
      Value *temp_lhs = value_temporary_acquire_register_for_descriptor(
        context, builder, register_find_available(builder, 0), descriptor, lhs->source_range
      );

      Expected_Result expected_a = expected_result_from_value(temp_lhs);
      temp_lhs = value_force(context, builder, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Value *temp_rhs = value_temporary_acquire_register_for_descriptor(
        context, builder, register_find_available(builder, 0), descriptor, rhs->source_range
      );
      Expected_Result expected_b = expected_result_from_value(temp_rhs);
      temp_rhs = value_force(context, builder, &expected_b, payload->rhs);

      MASS_ON_ERROR(*context->result) return 0;

      const X64_Mnemonic *mnemonic = payload->operator == Mass_Arithmetic_Operator_Add ? add : sub;

      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_lhs->storage, temp_rhs->storage}}
      );
      value_release_if_temporary(builder, temp_rhs);

      // temp_a is used as a result so it is intentionnaly not released
      return expected_result_ensure_value_or_temp(context, builder, expected_result, temp_lhs);
    }
    case Mass_Arithmetic_Operator_Multiply: {
      maybe_constant_fold(context, builder, &result_range, expected_result, lhs, rhs, *);

      Allocator *allocator = context->allocator;

      // We need both D and A for this operation so using either as temp will not work
      u64 disallowed_temp_registers = 0;
      register_bitset_set(&disallowed_temp_registers, Register_D);
      register_bitset_set(&disallowed_temp_registers, Register_A);

      // Save RDX as it will be used for the result overflow
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Maybe_Saved_Register maybe_saved_rdx = {0};
      if (
        expected_result->tag != Expected_Result_Tag_Exact ||
        !storage_is_register_index(
          &value_from_exact_expected_result(expected_result)->storage,
          Register_D
        )
      ) {
        maybe_saved_rdx = register_acquire_maybe_save_if_already_acquired(
          allocator, builder, &result_range, Register_D, disallowed_temp_registers
        );
      }

      Value *temp_a = value_temporary_acquire_register_for_descriptor(
        context, builder, Register_A, descriptor, result_range
      );
      temp_a->is_temporary = true;
      Expected_Result expected_a = expected_result_from_value(temp_a);
      temp_a = value_force(context, builder, &expected_a, payload->lhs);

      // TODO we do not acquire here because it is done by maybe_saved_rdx,
      //      but it is awkward that it is disconnected so need to think about
      Value *temp_b = value_register_for_descriptor(
        context, Register_D, descriptor, result_range
      );
      Expected_Result expected_b = expected_result_from_value(temp_b);
      temp_b = value_force(context, builder, &expected_b, payload->rhs);

      MASS_ON_ERROR(*context->result) return 0;

      const X64_Mnemonic *mnemonic = descriptor_is_signed_integer(descriptor) ? imul : mul;
      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_b->storage}}
      );
      register_release_maybe_restore(builder, &maybe_saved_rdx);

      // temp_a is used as a result so it is intentionnaly not released
      return expected_result_ensure_value_or_temp(context, builder, expected_result, temp_a);
    }
    case Mass_Arithmetic_Operator_Divide:
    case Mass_Arithmetic_Operator_Remainder: {
      Allocator *allocator = context->allocator;
      u64 byte_size = descriptor_byte_size(descriptor);

      if (payload->operator == Mass_Arithmetic_Operator_Divide) {
        maybe_constant_fold(context, builder, &result_range, expected_result, lhs, rhs, /);
      } else {
        maybe_constant_fold(context, builder, &result_range, expected_result, lhs, rhs, %);
      }

      // We need both D and A for this operation so using either as temp will not work
      u64 disallowed_temp_registers = 0;
      register_bitset_set(&disallowed_temp_registers, Register_D);
      register_bitset_set(&disallowed_temp_registers, Register_A);

      // Save RDX as it will be used for the result overflow
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Maybe_Saved_Register maybe_saved_rdx = {0};
      if (
        expected_result->tag != Expected_Result_Tag_Exact ||
        !storage_is_register_index(
          &value_from_exact_expected_result(expected_result)->storage,
          Register_D
        )
      ) {
        maybe_saved_rdx = register_acquire_maybe_save_if_already_acquired(
          allocator, builder, &result_range, Register_D, disallowed_temp_registers
        );
      }

      Value *temp_dividend = value_temporary_acquire_register_for_descriptor(
        context, builder, Register_A, descriptor, result_range
      );
      Expected_Result expected_dividend = expected_result_from_value(temp_dividend);
      temp_dividend = value_force(context, builder, &expected_dividend, payload->lhs);

      Value *temp_divisor = value_temporary_acquire_register_for_descriptor(
        context, builder, register_find_available(builder, 0), descriptor, payload->rhs->source_range
      );
      Expected_Result expected_divisor = expected_result_from_value(temp_divisor);
      temp_divisor = value_force(context, builder, &expected_divisor, payload->rhs);

      MASS_ON_ERROR(*context->result) return 0;

      if (descriptor_is_signed_integer(descriptor)){
        const X64_Mnemonic *widen = 0;
        switch (byte_size) {
          case 8: widen = cqo; break;
          case 4: widen = cdq; break;
          case 2: widen = cwd; break;
          case 1: widen = cbw; break;
        }
        assert(widen);
        push_eagerly_encoded_assembly(
          &builder->code_block, result_range, &(Instruction_Assembly){widen}
        );
        push_eagerly_encoded_assembly(
          &builder->code_block, result_range, &(Instruction_Assembly){idiv, {temp_divisor->storage}}
        );
      } else {
        if (byte_size == 1) {
          Storage reg_ax = storage_register_for_descriptor(Register_A, &descriptor_s16);
          push_eagerly_encoded_assembly(
            &builder->code_block, result_range, &(Instruction_Assembly){movzx, {reg_ax, temp_dividend->storage}}
          );
        } else {
          // We need to zero-extend A to D which means just clearing D register
          Storage reg_d = storage_register_for_descriptor(Register_D, &descriptor_s64);
          push_eagerly_encoded_assembly(
            &builder->code_block, result_range, &(Instruction_Assembly){xor, {reg_d, reg_d}}
          );
        }
        push_eagerly_encoded_assembly(
          &builder->code_block, result_range, &(Instruction_Assembly){asm_div, {temp_divisor->storage}}
        );
      }

      if (payload->operator == Mass_Arithmetic_Operator_Remainder) {
        if (byte_size == 1) {
          // :64bitMode8BitOperations
          // The encoder does not support access to AH so we hardcode byte of `mov AL, AH`
          // This is not optimal, but it should do for now.
          push_instruction(&builder->code_block, (Instruction) {
            .tag = Instruction_Tag_Bytes,
            .Bytes = {.memory = {0x88, 0xe0}, .length = 2},
            .source_range = result_range,
            .compiler_source_location = COMPILER_SOURCE_LOCATION,
          });
        } else {
          Storage reg_d = storage_register_for_descriptor(Register_D, descriptor);
          move_value(context->allocator, builder, &result_range, &temp_dividend->storage, &reg_d);
        }
      }

      value_release_if_temporary(builder, temp_divisor);
      register_release_maybe_restore(builder, &maybe_saved_rdx);

      return expected_result_ensure_value_or_temp(context, builder, expected_result, temp_dividend);
    }
    default: {
      panic("Internal error: Unexpected operator");
      return 0;
    }
  }
}

static Value *
mass_handle_arithmetic_operation(
  Execution_Context *context,
  Value_View arguments,
  void *operator_payload
) {
  Value *lhs = token_parse_single(context, value_view_get(arguments, 0));
  Value *rhs = token_parse_single(context, value_view_get(arguments, 1));

  MASS_ON_ERROR(*context->result) return 0;
  Mass_Arithmetic_Operator operator = (Mass_Arithmetic_Operator)(u64)operator_payload;

  const Descriptor *descriptor = large_enough_common_integer_descriptor_for_values(context, lhs, rhs);

  if (value_is_non_lazy_static(lhs) && value_is_non_lazy_static(rhs)) {
    Expected_Result expected_result = expected_result_static(descriptor);
    Mass_Arithmetic_Operator_Lazy_Payload lazy_payload =
      { .lhs = lhs, .rhs = rhs, .operator = operator };
    return mass_handle_arithmetic_operation_lazy_proc(context, 0, &expected_result, &lazy_payload);
  } else {
    Mass_Arithmetic_Operator_Lazy_Payload *lazy_payload =
      allocator_allocate(context->allocator, Mass_Arithmetic_Operator_Lazy_Payload);
    *lazy_payload = (Mass_Arithmetic_Operator_Lazy_Payload) {
      .lhs = lhs, .rhs = rhs, .operator = operator,
    };
    return mass_make_lazy_value(
      context, arguments.source_range, lazy_payload, descriptor, mass_handle_arithmetic_operation_lazy_proc
    );
  }
}

static inline Value *mass_add(Execution_Context *context, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, arguments, (void*)Mass_Arithmetic_Operator_Add);
}
static inline Value *mass_subtract(Execution_Context *context, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, arguments, (void*)Mass_Arithmetic_Operator_Subtract);
}
static inline Value *mass_multiply(Execution_Context *context, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, arguments, (void*)Mass_Arithmetic_Operator_Multiply);
}
static inline Value *mass_divide(Execution_Context *context, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, arguments, (void*)Mass_Arithmetic_Operator_Divide);
}
static inline Value *mass_remainder(Execution_Context *context, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, arguments, (void*)Mass_Arithmetic_Operator_Remainder);
}

typedef struct {
  Compare_Type compare_type;
  Value *lhs;
  Value *rhs;
} Mass_Comparison_Operator_Lazy_Payload;

static Value *
mass_handle_comparison_operation_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  void *raw_payload
) {
  Mass_Comparison_Operator_Lazy_Payload *payload = raw_payload;
  Compare_Type compare_type = payload->compare_type;
  const Source_Range *source_range = &payload->lhs->source_range;

  const Descriptor *descriptor =
    large_enough_common_integer_descriptor_for_values(context, payload->lhs, payload->rhs);
  assert(descriptor_is_integer(descriptor));

  if (descriptor_is_unsigned_integer(descriptor)) {
    switch(compare_type) {
      case Compare_Type_Equal:
      case Compare_Type_Not_Equal: {
        break;
      }

      case Compare_Type_Unsigned_Below:
      case Compare_Type_Unsigned_Below_Equal:
      case Compare_Type_Unsigned_Above:
      case Compare_Type_Unsigned_Above_Equal: {
        panic("Internal error. Expected to parse operators as signed compares");
        break;
      }

      case Compare_Type_Signed_Less: {
        compare_type = Compare_Type_Unsigned_Below;
        break;
      }
      case Compare_Type_Signed_Less_Equal: {
        compare_type = Compare_Type_Unsigned_Below_Equal;
        break;
      }
      case Compare_Type_Signed_Greater: {
        compare_type = Compare_Type_Unsigned_Above;
        break;
      }
      case Compare_Type_Signed_Greater_Equal: {
        compare_type = Compare_Type_Unsigned_Above_Equal;
        break;
      }
      default: {
        assert(!"Unsupported comparison");
        break;
      }
    }
  }

  switch(compare_type) {
    case Compare_Type_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, ==);
      break;
    }
    case Compare_Type_Not_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, !=);
      break;
    }

    case Compare_Type_Unsigned_Below: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, <);
      break;
    }
    case Compare_Type_Unsigned_Below_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, <=);
      break;
    }
    case Compare_Type_Unsigned_Above: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, >);
      break;
    }
    case Compare_Type_Unsigned_Above_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, >=);
      break;
    }

    case Compare_Type_Signed_Less: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, <);
      break;
    }
    case Compare_Type_Signed_Less_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, <=);
      break;
    }
    case Compare_Type_Signed_Greater: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, >);
      break;
    }
    case Compare_Type_Signed_Greater_Equal: {
      maybe_constant_fold(context, builder, source_range, expected_result, payload->lhs, payload->rhs, >=);
      break;
    }
    default: {
      assert(!"Unsupported comparison");
    }
  }

  // Try to reuse result_value if we can
  // TODO should also be able to reuse memory operands
  Value *temp_a = value_temporary_acquire_register_for_descriptor(
    context, builder, register_find_available(builder, 0), descriptor, *source_range
  );

  Expected_Result expected_a = expected_result_from_value(temp_a);
  temp_a = value_force(context, builder, &expected_a, payload->lhs);

  // TODO This can be optimized in cases where one of the operands is an immediate
  Value *temp_b = value_temporary_acquire_register_for_descriptor(
    context, builder, register_find_available(builder, 0), descriptor, *source_range
  );
  Expected_Result expected_b = expected_result_from_value(temp_b);
  temp_b = value_force(context, builder, &expected_b, payload->rhs);

  MASS_ON_ERROR(*context->result) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){cmp, {temp_a->storage, temp_b->storage}}
  );

  Value *comparison_value = value_from_compare(context, compare_type, *source_range);

  value_release_if_temporary(builder, temp_a);
  value_release_if_temporary(builder, temp_b);

  return expected_result_ensure_value_or_temp(context, builder, expected_result, comparison_value);
}

static inline Value *
mass_handle_comparison_operation(
  Execution_Context *context,
  Value_View arguments,
  void *raw_payload
) {
  Value *lhs = token_parse_single(context, value_view_get(arguments, 0));
  Value *rhs = token_parse_single(context, value_view_get(arguments, 1));
  MASS_ON_ERROR(*context->result) return 0;

  Compare_Type compare_type = (Compare_Type)(u64)raw_payload;

  const Descriptor *descriptor =
    large_enough_common_integer_descriptor_for_values(context, lhs, rhs);

  if (value_is_non_lazy_static(lhs) && value_is_non_lazy_static(rhs)) {
    Expected_Result expected_result = expected_result_static(descriptor);
    Mass_Comparison_Operator_Lazy_Payload lazy_payload =
      { .lhs = lhs, .rhs = rhs, .compare_type = compare_type };
    return mass_handle_comparison_operation_lazy_proc(context, 0, &expected_result, &lazy_payload);
  } else {
    Mass_Comparison_Operator_Lazy_Payload *payload =
      allocator_allocate(context->allocator, Mass_Comparison_Operator_Lazy_Payload);
    *payload = (Mass_Comparison_Operator_Lazy_Payload) {
      .lhs = lhs, .rhs = rhs, .compare_type = compare_type,
    };
    return mass_make_lazy_value(
      context, arguments.source_range, payload, &descriptor_s8, mass_handle_comparison_operation_lazy_proc
    );
  }
}

static inline Value *mass_less(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Signed_Less);
}
static inline Value *mass_greater(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Signed_Greater);
}
static inline Value *mass_less_equal(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Signed_Less_Equal);
}
static inline Value *mass_greater_equal(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Signed_Greater_Equal);
}
static inline Value *mass_equal(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Equal);
}
static inline Value *mass_not_equal(Execution_Context *context, Value_View arguments) {
  return mass_handle_comparison_operation(context, arguments, (void*)Compare_Type_Not_Equal);
}

static Value *
mass_handle_startup_call_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *args_token
) {
  Value_View expression = value_as_group(args_token)->children;
  Value *startup_function = token_parse_expression(context, expression, &(u64){0}, 0);
  MASS_ON_ERROR(*context->result) return 0;
  const Descriptor *descriptor = startup_function->descriptor;
  if (descriptor != &descriptor_function_literal) goto err;
  const Function_Literal *literal = value_as_function_literal(startup_function);

  if (dyn_array_length(literal->info->parameters)) goto err;
  if (literal->info->returns.declaration.descriptor != &descriptor_void) goto err;

  ensure_function_instance(context, startup_function, (Value_View){0});
  dyn_array_push(context->program->startup_functions, startup_function);

  return expected_result_validate(expected_result, &void_value);

  err:
  context_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = args_token->source_range,
    .detailed_message = "`startup` expects a () -> () {...} function as an argument"
  });
  return 0;
}

static Value *
mass_handle_address_of_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *pointee
) {
  // FIXME :ExpectedExact
  Value *result_value = value_from_exact_expected_result(expected_result);
  load_address(context, builder, &result_value->source_range, result_value, pointee->storage);
  return result_value;
}

static Value *
mass_type_of(
  Execution_Context *context,
  Value_View args
) {
  assert(args.length == 1);
  Value *expression = value_view_get(args, 0);
  const Descriptor *descriptor = value_or_lazy_value_descriptor(expression);

  return value_init(
    allocator_allocate(context->allocator, Value),
    &descriptor_descriptor_pointer,
    storage_static_inline(&descriptor),
    args.source_range
  );
}

static Value *
mass_size_of(
  Execution_Context *context,
  Value_View args
) {
  assert(args.length == 1);
  Value *expression = value_view_get(args, 0);
  const Descriptor *descriptor = value_or_lazy_value_descriptor(expression);
  u64 byte_size = descriptor_byte_size(descriptor);

  Number_Literal *literal = allocator_allocate(context->allocator, Number_Literal);
  *literal = (Number_Literal) {
    .base = 10,
    .negative = false,
    .bits = byte_size,
  };

  return value_make(context, &descriptor_number_literal, storage_static(literal), args.source_range);
}

static Value *
mass_address_of(
  Execution_Context *context,
  Value_View args
) {
  assert(args.length == 1);
  Value *pointee = value_view_get(args, 0);
  const Descriptor *pointee_descriptor = value_or_lazy_value_descriptor(pointee);
  const Descriptor *descriptor = descriptor_pointer_to(context->allocator, pointee_descriptor);
  return mass_make_lazy_value(
    context, args.source_range, pointee, descriptor, mass_handle_address_of_lazy_proc
  );
}

static Value *
mass_compile_time_error(
  Execution_Context *context,
  Value_View args
) {
  if (args.length != 2) goto err;
  Value *name_value = value_view_get(args, 0);
  Value *message_value = value_view_get(args, 1);
  if (name_value->descriptor != &descriptor_slice) goto err;
  if (message_value->descriptor != &descriptor_slice) goto err;
  Slice name = *storage_static_as_c_type(&name_value->storage, Slice);
  Slice message = *storage_static_as_c_type(&message_value->storage, Slice);

  context_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_User_Defined,
    .User_Defined.name = name,
    .detailed_message = message,
    .source_range = args.source_range,
  });
  return &void_value;

  err:
  context_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = args.source_range,
    .detailed_message = "compile_time_error() expects a two string arguments"
  });
  return 0;
}

static Value *
mass_handle_paren_operator(
  Execution_Context *context,
  Value_View args_view,
  void *unused_payload
) {
  Value *target = value_view_get(args_view, 0);
  Value *args_token = value_view_get(args_view, 1);
  Source_Range args_range = args_token->source_range;
  Slice target_name = {0};
  if (value_is_symbol(target)) {
    target_name = value_as_symbol(target)->name;
  }
  if (slice_equal(target_name, slice_literal("cast"))) {
    return token_handle_cast(context, args_token);
  } else if (slice_equal(target_name, slice_literal("c_string"))) {
    return mass_make_lazy_value(
      context, args_range, args_token, &descriptor_u8_pointer, token_handle_c_string
    );
  } else if (slice_equal(target_name, slice_literal("c_struct"))) {
    return token_process_c_struct_definition(context, args_token);
  } else if (slice_equal(target_name, slice_literal("startup"))) {
    return mass_make_lazy_value(
      context, args_range, args_token, &descriptor_void, mass_handle_startup_call_lazy_proc
    );
  } else {
    return token_handle_parsed_function_call(context, target, args_token, args_view.source_range);
  }
}

static Value *
mass_handle_apply_operator(
  Execution_Context *context,
  Value_View operands_view,
  void *unused_payload
) {
  Value *lhs_value = value_view_get(operands_view, 0);
  Value *rhs_value = value_view_get(operands_view, 1);
  Source_Range source_range = operands_view.source_range;

  if (value_match_group(rhs_value, Group_Tag_Paren)) {
    return mass_handle_paren_operator(context, operands_view, 0);
  }

  if (rhs_value->descriptor == &descriptor_value_view) {
    Value_View args_view = *storage_static_as_c_type(&rhs_value->storage, Value_View);
    return token_handle_function_call(context, lhs_value, args_view, source_range);
  }

  Scope_Entry *apply_entry = scope_lookup(context->scope, slice_literal("apply"));
  if (!apply_entry) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = rhs_value->source_range,
      .detailed_message = "Expected an operator"
    });
    return 0;
  }

  Value *apply_function = scope_entry_force_value(context, apply_entry);
  return token_handle_function_call(context, apply_function, operands_view, source_range);
}

static Value *
mass_handle_reflect_operator(
  Execution_Context *context,
  Value_View args_view,
  void *unused_payload
) {
  assert(args_view.length == 1);
  Value *source_value = value_view_get(args_view, 0);
  value_init(
    hash_map_set(context->compilation->static_pointer_map, source_value, (Value){0}),
    &descriptor_value,
    storage_none,
    source_value->source_range
  );

  return value_init(
    allocator_allocate(context->allocator, Value),
    &descriptor_value_pointer,
    storage_static_inline(&source_value),
    args_view.source_range
  );
}

static Value *
mass_handle_reify_operator(
  Execution_Context *context,
  Value_View args_view,
  void *unused_payload
) {
  assert(args_view.length == 1);
  Value *reflected_value = compile_time_eval(context, args_view);
  if (reflected_value->descriptor != &descriptor_value_pointer) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = reflected_value->source_range,
      .Type_Mismatch = {
        .expected = &descriptor_value_pointer,
        .actual = reflected_value->descriptor
      },
    });
    return 0;
  }
  return *storage_static_as_c_type(&reflected_value->storage, Value *);
}

static Value *
mass_handle_at_operator(
  Execution_Context *context,
  Value_View args_view,
  void *unused_payload
) {
  assert(args_view.length == 1);
  Value *body = value_view_get(args_view, 0);
  Source_Range body_range = body->source_range;
  if (value_match_symbol(body, slice_literal("scope"))) {
    return value_init(
      allocator_allocate(context->allocator, Value),
      &descriptor_scope, storage_static(context->scope), body_range
    );
  } else if (value_match_group(body, Group_Tag_Paren)) {
    return compile_time_eval(context, value_as_group(body)->children);
  } else if (value_match_group(body, Group_Tag_Curly)) {
    return compile_time_eval(context, args_view);
  } else {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = body_range,
      .detailed_message = "@ operator must be followed by an expression in () or {}"
    });
    return 0;
  }
}

static inline Memory_Layout_Item *
struct_find_field_by_name(
  const Descriptor *descriptor,
  Slice field_name
) {
  assert(descriptor->tag == Descriptor_Tag_Struct);
  for (u64 i = 0; i < dyn_array_length(descriptor->Struct.memory_layout.items); ++i) {
    Memory_Layout_Item *field = dyn_array_get(descriptor->Struct.memory_layout.items, i);
    if (slice_equal(field->declaration.name, field_name)) {
      return field;
    }
  }
  return 0;
}

typedef struct {
  Value *struct_;
  Memory_Layout_Item *field;
} Mass_Field_Access_Lazy_Payload;

static inline Value *
mass_handle_field_access_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  void *raw_payload
) {
  Mass_Field_Access_Lazy_Payload *payload = raw_payload;
  Memory_Layout_Item *field = payload->field;

  Expected_Result expected_struct =
    expected_result_any(value_or_lazy_value_descriptor(payload->struct_));
  Value *struct_ = value_force(context, builder, &expected_struct, payload->struct_);

  const Descriptor *struct_descriptor = struct_->descriptor;
  const Descriptor *unwrapped_descriptor = maybe_unwrap_pointer_descriptor(struct_descriptor);

  // Auto dereference pointers to structs
  if (struct_descriptor != unwrapped_descriptor) {
    assert(unwrapped_descriptor->tag == Descriptor_Tag_Struct);
    Storage base_storage;
    bool is_temporary;
    if (struct_->storage.tag == Storage_Tag_Register) {
      base_storage = struct_->storage;
      is_temporary = false;
    } else {
      base_storage = storage_register_for_descriptor(
        register_acquire_temp(builder), struct_->descriptor
      );
      move_value(
        context->allocator,
        builder,
        &struct_->source_range,
        &base_storage,
        &struct_->storage
      );
      is_temporary = true;
    }

    Storage pointee_storage =
      storage_indirect(descriptor_byte_size(unwrapped_descriptor), base_storage.Register.index);
    struct_ = value_make(context, unwrapped_descriptor, pointee_storage, struct_->source_range);
    struct_->is_temporary = is_temporary;
  }

  assert(unwrapped_descriptor->tag == Descriptor_Tag_Struct);
  Storage field_storage = memory_layout_item_storage(
    &struct_->storage, &unwrapped_descriptor->Struct.memory_layout, field
  );
  Value *field_value =
    value_make(context, field->declaration.descriptor, field_storage, struct_->source_range);
  // Since storage_field_access reuses indirect memory storage of the struct
  // the release of memory will be based on the field value release and we need
  // to propagate the temporary flag correctly
  field_value->is_temporary = struct_->is_temporary;

  return expected_result_ensure_value_or_temp(context, builder, expected_result, field_value);
}

typedef struct {
  Value *array;
  Value *index;
} Mass_Array_Access_Lazy_Payload;

static Value *
mass_handle_array_access_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  void *raw_payload
) {
  Mass_Array_Access_Lazy_Payload *payload = raw_payload;
  const Source_Range *array_range = &payload->array->source_range;
  Expected_Result expected_array =
    expected_result_any(value_or_lazy_value_descriptor(payload->array));
  Value *array = value_force(context, builder, &expected_array, payload->array);
  Expected_Result expected_index =
    expected_result_any(value_or_lazy_value_descriptor(payload->index));
  Value *index = value_force(context, builder, &expected_index, payload->index);

  MASS_ON_ERROR(*context->result) return 0;

  index = maybe_coerce_number_literal_to_integer(context, index, &descriptor_u64);
  Value *array_element_value;
  if (array->descriptor->tag == Descriptor_Tag_Pointer_To) {
    const Descriptor *item_descriptor = array->descriptor->Pointer_To.descriptor;
    Storage storage =
      storage_load_index_address(context, builder, array_range, array, item_descriptor, index);
    array_element_value = value_make(context, item_descriptor, storage, *array_range);
    // @Volatile :TemporaryRegisterForIndirectMemory
    // We have to mark this value as temporary so it is correctly released, but the whole
    // setup is very awkward. Firstly it only works base registers. Secondly, the setting
    // of temporary is disconnected from the creation of storage making it extra problematic.
    if (storage.tag != Storage_Tag_Static) array_element_value->is_temporary = true;
  } else {
    assert(array->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);

    const Descriptor *item_descriptor = array->descriptor->Fixed_Size_Array.item;

    u64 item_byte_size = descriptor_byte_size(item_descriptor);

    Storage element_storage;
    if (index->storage.tag == Storage_Tag_Static) {
      s32 index_number = s64_to_s32(storage_static_value_up_to_s64(&index->storage));
      s32 offset = index_number * s64_to_s32(item_byte_size);
      element_storage = storage_with_offset_and_byte_size(&array->storage, offset, item_byte_size);
    } else {
      // @Volatile :TemporaryRegisterForIndirectMemory
      element_storage =
        storage_load_index_address(context, builder, array_range, array, item_descriptor, index);
    }

    array_element_value = value_make(context, item_descriptor, element_storage, array->source_range);
    array_element_value->is_temporary = true;
  }

  return expected_result_ensure_value_or_temp(context, builder, expected_result, array_element_value);
}

static Value *
mass_handle_dot_operator(
  Execution_Context *context,
  Value_View args_view,
  void *unused_payload
) {
  Value *lhs = token_parse_single(context, value_view_get(args_view, 0));
  Value *rhs = value_view_get(args_view, 1);
  MASS_ON_ERROR(*context->result) return 0;

  Source_Range rhs_range = rhs->source_range;
  Source_Range lhs_range = lhs->source_range;
  const Descriptor *lhs_forced_descriptor = value_or_lazy_value_descriptor(lhs);
  const Descriptor *unwrapped_descriptor =
    maybe_unwrap_pointer_descriptor(lhs_forced_descriptor);

  if (
    unwrapped_descriptor->tag == Descriptor_Tag_Struct ||
    lhs_forced_descriptor == &descriptor_scope
  ) {
    if (value_is_number_literal(rhs)) {
      assert(unwrapped_descriptor->tag == Descriptor_Tag_Struct); // FIXME user error
      u64 index = value_as_number_literal(rhs)->bits; // TODO make a nicer wrapper
      if (index >= dyn_array_length(unwrapped_descriptor->Struct.memory_layout.items)) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Unknown_Field,
          .source_range = rhs_range,
          .Unknown_Field = {
            .name = source_from_source_range(&rhs_range),
            .type = unwrapped_descriptor,
          },
        });
        return 0;
      }
      Memory_Layout_Item *field =
        dyn_array_get(unwrapped_descriptor->Struct.memory_layout.items, index);

      // FIXME @CopyPaste from below
      Mass_Field_Access_Lazy_Payload *lazy_payload =
        allocator_allocate(context->allocator, Mass_Field_Access_Lazy_Payload);
      *lazy_payload = (Mass_Field_Access_Lazy_Payload) {
        .struct_ = lhs,
        .field = field,
      };

      return mass_make_lazy_value(
        context, lhs_range, lazy_payload, field->declaration.descriptor, mass_handle_field_access_lazy_proc
      );
    }
    if (!value_is_symbol(rhs)) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = rhs_range,
        .detailed_message = "Right hand side of the . operator on structs must be an identifier"
      });
      return 0;
    }
    const Symbol *field_symbol = value_as_symbol(rhs);
    Slice field_name = field_symbol->name;

    if (lhs_forced_descriptor == &descriptor_scope) {
      if (!value_is_non_lazy_static(lhs)) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Expected_Static,
          .source_range = lhs_range,
        });
        return 0;
      }
      const Scope *module_scope = storage_static_as_c_type(&lhs->storage, Scope);
      Value *lookup = scope_lookup_force(context, module_scope, field_symbol, &args_view.source_range);
      if (!lookup) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Unknown_Field,
          .source_range = rhs_range,
          .Unknown_Field = {
            .name = field_name,
            .type = lhs_forced_descriptor,
          },
        });
        return 0;
      }
      return lookup;
    } else {
      Memory_Layout_Item *field = struct_find_field_by_name(unwrapped_descriptor, field_name);
      if (!field) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Unknown_Field,
          .source_range = rhs_range,
          .Unknown_Field = {
            .name = field_name,
            .type = unwrapped_descriptor,
          },
        });
        return 0;
      }

      Mass_Field_Access_Lazy_Payload *lazy_payload =
        allocator_allocate(context->allocator, Mass_Field_Access_Lazy_Payload);
      *lazy_payload = (Mass_Field_Access_Lazy_Payload) {
        .struct_ = lhs,
        .field = field,
      };

      return mass_make_lazy_value(
        context, lhs_range, lazy_payload, field->declaration.descriptor, mass_handle_field_access_lazy_proc
      );
    }
  } else if (
    lhs_forced_descriptor->tag == Descriptor_Tag_Fixed_Size_Array ||
    lhs_forced_descriptor->tag == Descriptor_Tag_Pointer_To
  ) {
    if (value_match_group(rhs, Group_Tag_Paren) || value_is_number_literal(rhs)) {
      const Descriptor *descriptor = lhs_forced_descriptor;
      if (descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
        descriptor = descriptor->Fixed_Size_Array.item;
      } else {
        descriptor = descriptor->Pointer_To.descriptor;
      }
      Mass_Array_Access_Lazy_Payload *lazy_payload =
        allocator_allocate(context->allocator, Mass_Array_Access_Lazy_Payload);
      *lazy_payload = (Mass_Array_Access_Lazy_Payload) { .array = lhs, .index = rhs };

      return mass_make_lazy_value(
        context, lhs_range, lazy_payload, descriptor, mass_handle_array_access_lazy_proc
      );
    } else {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = rhs_range,
        .detailed_message =
          "Right hand side of the . operator for an array must be an (expr) or a literal number"
      });
      return 0;
    }
  } else {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = lhs_range,
      .detailed_message = "Left hand side of the . operator must be a struct or an array"
    });
    return 0;
  }
}

static void
token_dispatch_operator(
  Execution_Context *context,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *stack_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Operator *operator = stack_entry->operator;

  u64 argument_count = operator->argument_count;

  if (dyn_array_length(*stack) < argument_count) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = stack_entry->source_range,
      .detailed_message = "Not enough arguments for operator",
    });
    return;
  }
  assert(argument_count);
  u64 start_index = dyn_array_length(*stack) - argument_count;
  Value *first_arg = *dyn_array_get(*stack, start_index);
  Value *last_arg = *dyn_array_last(*stack);
  Value_View args_view = {
    .values = dyn_array_get(*stack, start_index),
    .length = argument_count,
    .source_range = {
      .file = last_arg->source_range.file,
      .offsets = {
        .from = first_arg->source_range.offsets.from,
        .to = last_arg->source_range.offsets.to,
      },
    },
  };

  assert(operator->handler);
  Value *result_value = operator->handler(context, args_view, operator->handler_payload);
  MASS_ON_ERROR(*context->result) return;

  // Pop off current arguments and push a new one
  dyn_array_splice_raw(*stack, start_index, argument_count, &result_value, 1);
}

typedef struct {
  Value *condition;
  Value *then;
  Value *else_;
} Mass_If_Expression_Lazy_Payload;

static Value *
mass_handle_if_expression_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_If_Expression_Lazy_Payload *payload
) {
  Value *condition = payload->condition;
  Value *then = payload->then;
  Value *else_ = payload->else_;

  const Source_Range *dummy_range = &payload->condition->source_range;

  if (value_is_static_number_literal(condition)) {
    condition = token_value_force_immediate_integer(context, condition, &descriptor_s64);
  } else if (condition->descriptor == &descriptor_lazy_value) {
    // TODO support any If-able descriptors instead of accepting literally anything
    Expected_Result expected_condition = expected_result_any(0);
    Value *temp_condition = value_force(context, builder, &expected_condition, condition);
    MASS_ON_ERROR(*context->result) return 0;
    condition = temp_condition;
  }

  Label_Index else_label = make_if(context, builder, &condition->source_range, condition);

  Value *result_value = value_force(context, builder, expected_result, then);
  MASS_ON_ERROR(*context->result) return 0;

  if (result_value->storage.tag == Storage_Tag_Static) {
    Value *stack_result =
      reserve_stack(context, builder, result_value->descriptor, result_value->source_range);
    MASS_ON_ERROR(assign(context, builder, stack_result, result_value)) return 0;
    result_value = stack_result;
  }

  Label_Index after_label =
    make_label(context->program, &context->program->memory.code, slice_literal("if end"));

  push_eagerly_encoded_assembly(
    &builder->code_block, *dummy_range,
    &(Instruction_Assembly){jmp, {code_label32(after_label)}}
  );

  push_label(&builder->code_block, *dummy_range, else_label);

  Expected_Result expected_else = expected_result_from_value(result_value);
  result_value = value_force(context, builder, &expected_else, else_);
  MASS_ON_ERROR(*context->result) return 0;

  push_label(&builder->code_block, *dummy_range, after_label);

  return result_value;
}

static Value *
token_parse_if_expression(
  Execution_Context *context,
  Value_View view,
  u64 *matched_length,
  const Token_Pattern *end_pattern
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("if"));

  Source_Range dummy_range = keyword->source_range;

  Mass_If_Expression_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_If_Expression_Lazy_Payload);
  *payload = (Mass_If_Expression_Lazy_Payload){0};

  {
    static const Token_Pattern then_pattern = {
      .tag = Token_Pattern_Tag_Symbol,
      .Symbol.name = slice_literal_fields("then")
    };
    Value_View condition_view = value_view_slice(&view, peek_index, view.length);
    u64 condition_length;
    payload->condition =
      token_parse_expression(context, condition_view, &condition_length, &then_pattern);
    peek_index += condition_length;
    MASS_ON_ERROR(*context->result) return 0;
  }

  {
    static const Token_Pattern else_pattern = {
      .tag = Token_Pattern_Tag_Symbol,
      .Symbol.name = slice_literal_fields("else"),
    };
    Value_View then_view = value_view_slice(&view, peek_index, view.length);
    u64 then_length;
    payload->then = token_parse_expression(context, then_view, &then_length, &else_pattern);
    peek_index += then_length;
    MASS_ON_ERROR(*context->result) return 0;
  }

  {
    Value_View else_view = value_view_slice(&view, peek_index, view.length);
    u64 else_length;
    payload->else_ = token_parse_expression(context, else_view, &else_length, end_pattern);
    peek_index += else_length;
  }

  *matched_length = peek_index;

  // TODO probably want to unify then and else branch before returning
  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(payload->then);

  return mass_make_lazy_value(
    context, dummy_range, payload, result_descriptor, mass_handle_if_expression_lazy_proc
  );
}

static Function_Literal *
mass_make_fake_function_literal(
  Execution_Context *context,
  Value *body,
  const Descriptor *returns,
  const Source_Range *source_range
) {
  Scope *function_scope = scope_make(context->allocator, context->scope);
  Execution_Context function_context = *context;
  function_context.scope = function_scope;

  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info, function_scope);
  fn_info->returns = (Function_Return) {
    .declaration = {
      .descriptor = returns,
      .source_range = *source_range,
    },
  };

  Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
  *literal = (Function_Literal){
    .info = fn_info,
    .body = body,
    .context = *context,
  };
  return literal;
}

static Value *
token_parse_intrinsic_literal(
  Execution_Context *context,
  Value_View view,
  const Function_Info *info
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(at, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("@"));
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("intrinsic"));
  Token_Expect_Match(body, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Curly);
  if (peek_index != view.length) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
    });
    return 0;
  }

  const u64 tokens_per_argument = 8;
  Array_Value_Ptr wrapped_body_children = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->allocator,
    .capacity = tokens_per_argument * dyn_array_length(info->parameters),
  );

  // TODO would be nice to somehow make it non syntax-dependent
  // This adds local definitions for each of the arguments
  Value *arguments_symbol = token_make_symbol(
    context->compilation->symbol_cache_map,
    context->allocator, slice_literal("arguments"), Symbol_Type_Id_Like, view.source_range
  );
  Value *values_symbol = token_make_symbol(
    context->compilation->symbol_cache_map,
    context->allocator, slice_literal("values"), Symbol_Type_Id_Like, view.source_range
  );
  Value *colon_equal_symbol = token_make_symbol(
    context->compilation->symbol_cache_map,
    context->allocator, slice_literal(":="), Symbol_Type_Operator_Like, view.source_range
  );
  Value *dot_symbol = token_make_symbol(
    context->compilation->symbol_cache_map,
    context->allocator, slice_literal("."), Symbol_Type_Operator_Like, view.source_range
  );
  Value *semicolon_symbol = token_make_symbol(
    context->compilation->symbol_cache_map,
    context->allocator, slice_literal(";"), Symbol_Type_Operator_Like, view.source_range
  );
  for (u64 param_index = 0; param_index < dyn_array_length(info->parameters); ++param_index) {
    Function_Parameter *param = dyn_array_get(info->parameters, param_index);
    Value *param_name_symbol = token_make_symbol(
      context->compilation->symbol_cache_map,
      context->allocator, param->declaration.name, Symbol_Type_Id_Like, view.source_range
    );
    dyn_array_push(wrapped_body_children, param_name_symbol);
    dyn_array_push(wrapped_body_children, colon_equal_symbol);
    dyn_array_push(wrapped_body_children, arguments_symbol);
    dyn_array_push(wrapped_body_children, dot_symbol);
    dyn_array_push(wrapped_body_children, values_symbol);
    dyn_array_push(wrapped_body_children, dot_symbol);
    Number_Literal *literal = allocator_allocate(context->allocator, Number_Literal);
    *literal = (Number_Literal){.base = Number_Base_10, .bits = param_index};
    Value *literal_value = value_make(
      context, &descriptor_number_literal, storage_static(literal), view.source_range
    );
    dyn_array_push(wrapped_body_children, literal_value);
    dyn_array_push(wrapped_body_children, semicolon_symbol);
  }
  assert(dyn_array_length(wrapped_body_children) % tokens_per_argument == 0);


  dyn_array_push(wrapped_body_children, body);

  Group *wrapped_body_group = allocator_allocate(context->allocator, Group);
  wrapped_body_group->tag = Group_Tag_Curly;
  wrapped_body_group->children = value_view_from_value_array(wrapped_body_children, &body->source_range);

  Value *wrapped_body_value = value_make(
    context, &descriptor_group, storage_static(wrapped_body_group), body->source_range
  );

  Function_Literal *literal = mass_make_fake_function_literal(
    context, wrapped_body_value, &descriptor_value_pointer, &keyword->source_range
  );

  // @Volatile :IntrinsicFunctionSignature
  // These arguments must match how we call it.
  literal->info->parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = 2
  );
  dyn_array_push(literal->info->parameters, (Function_Parameter) {
    .declaration = {
      .name = slice_literal("context"),
      .descriptor = &descriptor_execution_context_pointer,
      .source_range = keyword->source_range,
    },
  });
  dyn_array_push(literal->info->parameters, (Function_Parameter) {
    .declaration = {
      .name = slice_literal("arguments"),
      .descriptor = &descriptor_value_view,
      .source_range = keyword->source_range,
    },
  });
  literal->info->flags |= Descriptor_Function_Flags_Compile_Time;
  literal->info->flags |= Descriptor_Function_Flags_Intrinsic;

  return value_make(context, &descriptor_function_literal, storage_static(literal), view.source_range);
}

static Function_Info *
function_info_from_parameters_and_return_type(
  Execution_Context *context,
  Value_View args_view,
  Value *return_types
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 function_epoch = get_new_epoch();
  Scope *function_scope = scope_make(context->allocator, context->scope);

  Execution_Context arg_context = *context;
  arg_context.scope = function_scope;
  arg_context.epoch = function_epoch;

  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info, function_scope);

  Temp_Mark temp_mark = context_temp_mark(context);


  if (args_view.length != 0) {
    bool previous_argument_has_default_value = false;

    Array_Function_Parameter temp_params = dyn_array_make(
      Array_Function_Parameter,
      .allocator = context->temp_allocator,
      .capacity = 32,
    );

    for (Value_View_Split_Iterator it = { .view = args_view }; !it.done;) {
      Value_View param_view = token_split_next(&it, &token_pattern_comma_operator);
      Function_Parameter param = token_match_argument(&arg_context, param_view, fn_info);
      MASS_ON_ERROR(*context->result) goto defer;
      dyn_array_push(temp_params, param);
      if (previous_argument_has_default_value) {
        if (!param.maybe_default_expression.length ) {
          context_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Non_Trailing_Default_Argument,
            .source_range = return_types->source_range,
          });
          goto defer;
        }
      } else {
        previous_argument_has_default_value = !!param.maybe_default_expression.length;
      }
    }
    dyn_array_copy_from_temp(Array_Function_Parameter, context, &fn_info->parameters, temp_params);
  }

  if (value_is_group(return_types)) {
    const Group *return_types_group = value_as_group(return_types);
    if (return_types_group->tag != Group_Tag_Paren) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .detailed_message = "Return type can only be a type name or a parenthesized list",
        .source_range = return_types->source_range,
      });
      return 0;
    }
    Value_View return_types_view = return_types_group->children;
    if (return_types_view.length == 0) {
      fn_info->returns = (Function_Return) { .declaration.descriptor = &descriptor_void, };
    } else {
      Value_View_Split_Iterator it = { .view = return_types_view };

      for (u64 i = 0; !it.done; ++i) {
        if (i > 0) {
          context_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Unimplemented,
            .detailed_message = "Multiple return types are not supported at the moment",
            .source_range = return_types->source_range,
          });
          return 0;
        }
        Value_View arg_view = token_split_next(&it, &token_pattern_comma_operator);

        fn_info->returns = token_match_return_type(&arg_context, arg_view);
      }
    }
  } else {
    fn_info->returns = token_match_return_type(
      &arg_context, value_view_make_single(context->allocator, return_types)
    );
  }
  MASS_ON_ERROR(*context->result) return 0;

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return fn_info;
}

static Value *
token_parse_function_literal(
  Execution_Context *context,
  Value_View view,
  u64 *matched_length,
  const Token_Pattern *end_pattern
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  bool is_macro = false;
  Token_Maybe_Match(at, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("@"));
  Token_Maybe_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("fn"));
  if (!keyword) {
    Token_Maybe_Match(macro, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("macro"));
    keyword = macro;
    is_macro = true;
  }
  if (!keyword) return 0;

  if (is_macro && at) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = at->source_range,
      .detailed_message = "Function-like macro can not be marked compile time"
    });
    return 0;
  }

  Token_Maybe_Match(maybe_name, .tag = Token_Pattern_Tag_Symbol);
  Token_Expect_Match(args, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);

  Value *returns;
  Token_Maybe_Match(arrow, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("->"));
  if (arrow) {
    Token_Expect_Match(raw_return, .tag = Token_Pattern_Tag_Any);
    returns = raw_return;
  } else {
    Group *group = allocator_make(context->allocator, Group, .tag = Group_Tag_Paren);
    returns = value_init(
      allocator_allocate(context->allocator, Value),
      &descriptor_group, storage_static(group), keyword->source_range
    );
  }

  Slice name = maybe_name ? value_as_symbol(maybe_name)->name : (Slice){0};
  Value_View args_view = value_as_group(args)->children;
  Function_Info *fn_info =
    function_info_from_parameters_and_return_type(context, args_view, returns);
  MASS_ON_ERROR(*context->result) return 0;

  Token_Maybe_Match(body_value, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Curly);
  if (!body_value) {
    Value_View rest = value_view_match_till(view, &peek_index, end_pattern);
    if (is_macro) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = rest.source_range,
        .detailed_message = "Function-like macro must have a literal body in {}"
      });
      return 0;
    }
    if (rest.length) {
      // FIXME should match as a compile time expression right away
      body_value = token_parse_intrinsic_literal(context, rest, fn_info);
      if (!body_value) body_value = compile_time_eval(context, rest);
    }
  }
  MASS_ON_ERROR(*context->result) return 0;

  *matched_length = view.length;

  if (at) {
    fn_info->flags |= Descriptor_Function_Flags_Compile_Time;
  }
  bool is_syscall = body_value && body_value->descriptor == &descriptor_syscall;
  if (is_syscall) {
    ensure_parameter_descriptors(context, fn_info);
    // TODO support this on non-Linux systems
    Descriptor *fn_descriptor = descriptor_function_instance(
      context->allocator, name, fn_info, &calling_convention_x86_64_system_v_syscall
    );
    // TODO this patching after the fact feels awkward and brittle
    if (is_syscall) {
      s64 syscall_number = storage_static_as_c_type(&body_value->storage, Syscall)->number;
      assert(fn_descriptor->tag == Descriptor_Tag_Function_Instance);
      assert(fn_descriptor->Function_Instance.call_setup.jump.tag == Function_Call_Jump_Tag_Syscall);
      fn_descriptor->Function_Instance.call_setup.jump.Syscall.number = syscall_number;
    }

    return value_init(
      allocator_allocate(context->allocator, Value),
      fn_descriptor, storage_none, view.source_range
    );
  } else if (body_value) {
    if (value_is_intrinsic(body_value) && !(fn_info->flags & Descriptor_Function_Flags_Compile_Time)) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = body_value->source_range,
        .detailed_message = "A function literal with the intrinsic body must be marked compile time"
      });
      return 0;
    }
    Function_Literal_Flags flags = Function_Literal_Flags_None;

    DYN_ARRAY_FOREACH(Function_Parameter, param, fn_info->parameters) {
      if (param->tag == Function_Parameter_Tag_Generic) {
        flags |= Function_Literal_Flags_Generic;
        break;
      }
    }
    if (is_macro) flags |= Function_Literal_Flags_Macro;
    if (!(flags & Function_Literal_Flags_Generic)) {
      ensure_parameter_descriptors(context, fn_info);
      MASS_ON_ERROR(*context->result) return 0;
    }
    Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
    *literal = (Function_Literal){
      .flags = flags,
      .info = fn_info,
      .body = body_value,
      .context = *context,
    };
    return value_make(context, &descriptor_function_literal, storage_static(literal), view.source_range);
  } else {
    ensure_parameter_descriptors(context, fn_info);
    MASS_ON_ERROR(*context->result) return 0;

    const Calling_Convention *calling_convention =
      context->compilation->runtime_program->default_calling_convention;
    Descriptor *fn_descriptor = descriptor_function_instance(
      context->allocator, name, fn_info, calling_convention
    );

    return value_init(
      allocator_allocate(context->allocator, Value),
      &descriptor_descriptor_pointer, storage_static_inline(&fn_descriptor), view.source_range
    );
  }
}

typedef Value *(*Expression_Matcher_Proc)(
  Execution_Context *context,
  Value_View view,
  u64 *out_match_length,
  const Token_Pattern *end_pattern
);

static PRELUDE_NO_DISCARD Value *
token_parse_expression(
  Execution_Context *context,
  Value_View view,
  u64 *out_match_length,
  const Token_Pattern *end_pattern
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if(!view.length) return &void_value;
  Value *result = 0;

  Temp_Mark temp_mark = context_temp_mark(context);

  u64 capacity = u64_min(view.length, 128);

  Array_Value_Ptr value_stack = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = capacity,
  );
  Array_Operator_Stack_Entry operator_stack = dyn_array_make(
    Array_Operator_Stack_Entry,
    .allocator = context->temp_allocator,
    .capacity = capacity,
  );

  bool is_previous_an_operator = true;
  u64 matched_length = view.length;

  static Expression_Matcher_Proc expression_matchers[] = {
    token_parse_if_expression,
    token_parse_function_literal
  };

  for (u64 i = 0; ; ++i) {
    repeat:
    if (i >= view.length) break;

    Value_View rest = value_view_rest(&view, i);
    for (u64 matcher_index = 0; matcher_index < countof(expression_matchers); matcher_index += 1) {
      Expression_Matcher_Proc matcher = expression_matchers[matcher_index];
      u64 match_length = 0;
      Value *match_result = matcher(context, rest, &match_length, end_pattern);
      MASS_ON_ERROR(*context->result) goto defer;
      if (match_length) {
        assert(match_result);
        dyn_array_push(value_stack, match_result);
        // Skip over the matched slice
        i += match_length;
        goto repeat;
      }
    }

    Operator_Fixity fixity_mask = is_previous_an_operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Infix | Operator_Fixity_Postfix;

    Value *value = value_view_get(view, i);

    if (end_pattern && value_match(value, end_pattern)) {
      matched_length = i + 1;
      goto drain;
    }

    if (value_is_symbol(value)) {
      Slice symbol_name = value_as_symbol(value)->name;

      Operator *maybe_operator = scope_lookup_operator(context->scope, symbol_name, fixity_mask);
      if (maybe_operator) {
        if (!token_handle_operator(
          context, view, &value_stack, &operator_stack, maybe_operator, value->source_range
        )) goto defer;
        is_previous_an_operator = true;
        continue;
      }
    }

    if (!is_previous_an_operator) {
      Operator *empty_space_operator =
        scope_lookup_operator(context->scope, slice_literal(" "), Operator_Fixity_Infix);
      assert(empty_space_operator);
      if (!token_handle_operator(
        context, view, &value_stack, &operator_stack, empty_space_operator, value->source_range
      )) goto defer;
    }
    dyn_array_push(value_stack, value);
    is_previous_an_operator = false;
  }

  drain:
  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_operator(context, &value_stack, entry);
  }

  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(value_stack) == 1) {
      result = *dyn_array_last(value_stack);
      result = token_parse_single(context, result);
    } else {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = view.source_range,
      });
    }
  }

  defer:
  context_temp_reset_to_mark(context, temp_mark);

  *out_match_length = matched_length;
  return result;
}

static Value *
mass_handle_block_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  void *raw_payload
) {
  Array_Value_Ptr lazy_statements;
  UNPACK_FROM_VOID_POINTER(lazy_statements, raw_payload);
  u64 statement_count = dyn_array_length(lazy_statements);
  assert(statement_count);
  Value *result_value = 0;
  for (u64 i = 0; i < statement_count; ++i) {
    MASS_ON_ERROR(*context->result) return 0;
    u64 registers_before = builder ? builder->register_occupied_bitset : 0;
    Value *lazy_statement = *dyn_array_get(lazy_statements, i);
    Source_Range debug_source_range = lazy_statement->source_range;
    Slice debug_source = source_from_source_range(&debug_source_range);
    // This is an easy way to break on the statement based on source text
    if (slice_starts_with(debug_source, slice_literal("")) && false) {
      printf("%"PRIslice"\n", SLICE_EXPAND_PRINTF(debug_source));
    }
    if (i == statement_count - 1) {
      // TODO extract this into a helper and use for any "return" scenarios
      const Descriptor *expected_descriptor = expected_result_descriptor(expected_result);
      if (
        expected_descriptor &&
        !same_value_type_or_can_implicitly_move_cast(expected_descriptor, lazy_statement)
      ) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = lazy_statement->source_range,
          .Type_Mismatch = {
            .expected = expected_descriptor,
            .actual = value_or_lazy_value_descriptor(lazy_statement),
          },
        });
        return 0;
      }
      result_value = value_force(context, builder, expected_result, lazy_statement);
      result_value = expected_result_ensure_value_or_temp(context, builder, expected_result, result_value);
    } else {
      Expected_Result expected_void = expected_result_from_value(&void_value);
      result_value = value_force(context, builder, &expected_void, lazy_statement);
    }
    MASS_ON_ERROR(*context->result) return 0;
    // We do not do cross-statement register allocation so can check that there
    // are no stray registers retained across statement boundaries
    if(builder && registers_before != builder->register_occupied_bitset) {
      for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
        bool before = register_bitset_get(registers_before, reg_index);
        bool after = register_bitset_get(builder->register_occupied_bitset, reg_index);
        if (before != after) {
          if (after) {
            printf("Unreleased %s\n", register_name(reg_index));
          } else {
            printf("Falsly released %s\n", register_name(reg_index));
          }
        }
      }
      panic("Found unreleased registers");
    }
  }
  return result_value;
}

static Value *
token_parse_block_view(
  Execution_Context *context,
  Value_View children_view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!children_view.length) return &void_value;

  Value *block_result = &void_value;

  Temp_Mark temp_mark = context_temp_mark(context);

  Array_Value_Ptr temp_lazy_statements = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = 32,
  );

  u64 match_length = 0;
  for(u64 start_index = 0; start_index < children_view.length; start_index += match_length) {
    MASS_ON_ERROR(*context->result) goto defer;
    Value_View rest = value_view_rest(&children_view, start_index);
    // Skipping over empty statements
    if (value_match(value_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length = 1;
      continue;
    }
    Lazy_Value lazy_value = {
      .context = *context,
      .descriptor = &descriptor_void,
    };
    match_length = token_statement_matcher_in_scopes(context, rest, &lazy_value, context->scope, 0);
    MASS_ON_ERROR(*context->result) goto defer;

    if (match_length) {
      // If the statement did not assign a proc that means that it does not need
      // to output any instructions and there is nothing to force.
      if (lazy_value.proc) {
        assert(lazy_value.descriptor);
        Value_View matched_view = value_view_slice(&rest, 0, match_length);
        Lazy_Value *lazy_value_storage = allocator_allocate(context->allocator, Lazy_Value);
        *lazy_value_storage = lazy_value;
        Value *lazy_statement = allocator_allocate(context->allocator, Value);
        Storage storage = storage_static(lazy_value_storage);
        value_init(lazy_statement, &descriptor_lazy_value, storage, matched_view.source_range);
        dyn_array_push(temp_lazy_statements, lazy_statement);
      }
      continue;
    }
    Value *parse_result =
      token_parse_expression(context, rest, &match_length, &token_pattern_semicolon);
    dyn_array_push(temp_lazy_statements, parse_result);

    if (match_length) continue;
    Value *token = value_view_get(rest, 0);
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = token->source_range,
    });
    goto defer;
  }

  MASS_ON_ERROR(*context->result) goto defer;

  u64 statement_count = dyn_array_length(temp_lazy_statements);
  if (statement_count) {
    Value *last_result = *dyn_array_last(temp_lazy_statements);
    const Descriptor *last_descriptor = value_or_lazy_value_descriptor(last_result);
    if (statement_count == 1) {
      return last_result;
    } else {
      Array_Value_Ptr lazy_statements;
      dyn_array_copy_from_temp(Array_Value_Ptr, context, &lazy_statements, temp_lazy_statements);

      void *payload;
      PACK_AS_VOID_POINTER(payload, lazy_statements);

      return mass_make_lazy_value(
        context, last_result->source_range, payload, last_descriptor, mass_handle_block_lazy_proc
      );
    }
  } else {
    block_result = &void_value;
  }

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return block_result;
}

static inline Value *
token_parse_block_no_scope(
  Execution_Context *context,
  Value *block
) {
  const Group *group = value_as_group(block);
  assert(group->tag == Group_Tag_Curly);

  return token_parse_block_view(context, group->children);
}

static inline Value *
token_parse_block(
  Execution_Context *context,
  Value *block
) {
  Execution_Context body_context = *context;
  Scope *block_scope = scope_make(context->allocator, context->scope);
  body_context.scope = block_scope;
  return token_parse_block_no_scope(&body_context, block);
}

static u64
token_parse_statement_using(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("using"));
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);

  Value *result = compile_time_eval(context, rest);

  if (result->descriptor != &descriptor_scope) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = rest.source_range,
      .Type_Mismatch = { .expected = &descriptor_scope, .actual = result->descriptor },
    });
    goto err;
  }

  if (result->storage.tag != Storage_Tag_Static) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Expected_Static,
      .source_range = rest.source_range,
    });
    goto err;
  }

  Scope *using_scope = storage_static_as_c_type(&result->storage, Scope);
  use_scope(context, using_scope);

  err:
  return peek_index;
}

static Value *
mass_handle_label_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *label_value
) {
  Source_Range source_range = label_value->source_range;
  if (label_value->descriptor != &descriptor_label_index) {
    Slice source = source_from_source_range(&source_range);
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Redifinition,
      .source_range = source_range,
      .Redifinition = { .name = source, .previous_source_range = label_value->source_range },
      .detailed_message = "Trying to redefine a non-label variable as a label",
    });
    return 0;
  }

  Label_Index label_index = *storage_static_as_c_type(&label_value->storage, Label_Index);
  push_label(&builder->code_block, source_range, label_index);

  return expected_result_validate(expected_result, &void_value);
}

static u64
token_parse_statement_label(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("label"));
  Token_Maybe_Match(placeholder, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("placeholder"));

  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);

  if (rest.length != 1 || !value_is_symbol(value_view_get(rest, 0))) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Invalid_Identifier,
      .source_range = rest.source_range,
    });
    goto err;
  }

  Value *symbol_token = value_view_get(rest, 0);
  Source_Range source_range = symbol_token->source_range;
  const Symbol *symbol = value_as_symbol(symbol_token);
  Slice name = symbol->name;

  // :ForwardLabelRef
  // First try to lookup a label that might have been declared by `goto`
  Value *value;
  if (scope_lookup(context->scope, name)) {
    value = scope_lookup_force(context, context->scope, symbol, &source_range);
  } else {
    Scope *label_scope = context->scope;

    Label_Index *label = allocator_allocate(context->allocator, Label_Index);
    *label = make_label(context->program, &context->program->memory.code, name);
    value = value_init(
      allocator_allocate(context->allocator, Value),
      &descriptor_label_index, storage_static(label), source_range
    );
    scope_define_value(label_scope, VALUE_STATIC_EPOCH, source_range, name, value);
    if (placeholder) {
      return peek_index;
    }
  }

  out_lazy_value->proc = mass_handle_label_lazy_proc;
  out_lazy_value->payload = value;

  err:
  return peek_index;
}

static Value *
mass_handle_explicit_return_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *parse_result
) {
  MASS_ON_ERROR(assign(context, builder, builder->return_value, parse_result)) return 0;
  Storage return_label = code_label32(builder->code_block.end_label);

  push_eagerly_encoded_assembly(
    &builder->code_block,
    builder->return_value->source_range,
    &(Instruction_Assembly) {jmp, {return_label}}
  );

  return expected_result_validate(expected_result, &void_value);
}

static u64
token_parse_explicit_return(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("return"));
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  bool has_return_expression = rest.length > 0;

  Value *parse_result = token_parse_expression(context, rest, &(u64){0}, 0);
  const Descriptor *descriptor = value_or_lazy_value_descriptor(parse_result);

  if (descriptor == &descriptor_void && !has_return_expression) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = parse_result->source_range,
      .detailed_message = "Explicit return from a non-void function requires a value"
    });
    return 0;
  }

  out_lazy_value->proc = mass_handle_explicit_return_lazy_proc;
  out_lazy_value->payload = parse_result;

  return peek_index;
}

static Value *
mass_handle_inline_machine_code_bytes_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Value *args_token
) {
  Instruction_Bytes bytes = {0};

  Value_View_Split_Iterator it = { .view = value_as_group(args_token)->children };

  enum {MAX_PATCH_COUNT = 2};
  Instruction_Label_Patch patches[MAX_PATCH_COUNT] = {0};
  s32 patch_count = 0;

  for (u64 argument_count = 0; !it.done; argument_count += 1) {
    if (argument_count >= 15) {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = args_token->source_range,
        .detailed_message = "Expected a maximum of 15 bytes"
      });
      return 0;
    }

    MASS_ON_ERROR(*context->result) return 0;

    Value_View view = token_split_next(&it, &token_pattern_comma_operator);
    Value *value = compile_time_eval(context, view);
    MASS_ON_ERROR(*context->result) return 0;

    if (value->descriptor == &descriptor_label_index) {
      if (patch_count == MAX_PATCH_COUNT) {
        context_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = args_token->source_range,
          .detailed_message = "inline_machine_code_bytes supports no more than 2 labels"
        });
        return 0;
      }
      patches[patch_count++] = (Instruction_Label_Patch) {
        .label_index = *storage_static_as_c_type(&value->storage, Label_Index),
        .offset = bytes.length
      };
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
    } else if (value->storage.tag == Storage_Tag_Static) {
      value = token_value_force_immediate_integer(context, value, &descriptor_u8);
      MASS_ON_ERROR(*context->result) return 0;
      u8 byte = u64_to_u8(storage_static_value_up_to_u64(&value->storage));
      bytes.memory[bytes.length++] = s64_to_u8(byte);
    } else {
      context_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Expected_Static,
        .source_range = value->source_range,
      });
      return 0;
    }
  }

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Bytes,
    .Bytes = bytes,
    .scope = context->scope,
    .source_range = args_token->source_range,
    .compiler_source_location = COMPILER_SOURCE_LOCATION,
  });

  for (s32 i = 0; i < patch_count; i += 1) {
    patches[i].offset -= bytes.length;
    push_instruction(&builder->code_block, (Instruction) {
      .tag = Instruction_Tag_Label_Patch,
      .Label_Patch = patches[i],
      .source_range = args_token->source_range,
      .compiler_source_location = COMPILER_SOURCE_LOCATION,
    });
  }

  return expected_result_validate(expected_result, &void_value);
}

static u64
token_parse_inline_machine_code_bytes(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(id_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("inline_machine_code_bytes"));
  Token_Maybe_Match(args_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);
  if (!args_token) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = id_token->source_range,
      .detailed_message = "Expected and argument list in parens"
    });
    goto err;
  }

  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  if (rest.length) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = rest.source_range,
      .detailed_message = "Expected the end of the statement"
    });
    goto err;
  }

  out_lazy_value->proc = mass_handle_inline_machine_code_bytes_lazy_proc;
  out_lazy_value->payload = args_token;

  err:
  return peek_index;
}

typedef struct {
  Value *name_token;
  const Descriptor *descriptor;
} Mass_Variable_Definition_Lazy_Payload;

static Value *
mass_handle_variable_definition_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_Variable_Definition_Lazy_Payload *payload
) {
  if (payload->descriptor == &descriptor_void) {
    return value_make(context, payload->descriptor, storage_none, payload->name_token->source_range);
  } else {
    return reserve_stack(context, builder, payload->descriptor, payload->name_token->source_range);
  }
}

static u64
token_parse_definition(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *unused_lazy_value,
  Value **out_definition_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name_token, .tag = Token_Pattern_Tag_Symbol);
  Token_Match_Operator(define, ":");

  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  const Descriptor *descriptor = token_match_type(context, rest);
  MASS_ON_ERROR(*context->result) return 0;

  Mass_Variable_Definition_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_Variable_Definition_Lazy_Payload);
  *payload = (Mass_Variable_Definition_Lazy_Payload){
    .name_token = name_token,
    .descriptor = descriptor,
  };

  // We are creating a custom lazy value here instead of a statement because if the value
  // is never referenced we can skip running the lazy proc. This is safe because these
  // definitions do not have an initializer and thus do not produce side effects.
  Value *variable_value = mass_make_lazy_value(
    context, name_token->source_range, payload, descriptor, mass_handle_variable_definition_lazy_proc
  );
  if (out_definition_value) *out_definition_value = variable_value;
  Slice name = value_as_symbol(name_token)->name;
  scope_define_value(context->scope, context->epoch, name_token->source_range, name, variable_value);

  return peek_index;
}

typedef struct {
  Source_Range source_range;
  Value *target;
  Value *expression;
} Mass_Assignment_Lazy_Payload;

static Value *
mass_handle_assignment_lazy_proc(
  Execution_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Mass_Assignment_Lazy_Payload *payload
) {
  const Descriptor *descriptor = value_or_lazy_value_descriptor(payload->expression);
  Expected_Result expected_target = expected_result_any(descriptor);
  Value *target = value_force(context, builder, &expected_target, payload->target);
  MASS_ON_ERROR(*context->result) return 0;

  value_force_exact(context, builder, target, payload->expression);
  value_release_if_temporary(builder, target);

  return expected_result_validate(expected_result, &void_value);
}

static void
token_define_global_variable(
  Execution_Context *context,
  Value *symbol,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = token_parse_expression(context, expression, &(u64){0}, 0);
  MASS_ON_ERROR(*context->result) return;

  const Descriptor *descriptor = value_or_lazy_value_descriptor(value);
  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (value_is_static_number_literal(value)) {
    descriptor = &descriptor_s64;
  }

  Value *global_value;
  if (storage_is_label(&value->storage)) {
    global_value = value;
  } else {
    Section *section = &context->program->memory.rw_data;
    u64 byte_size = descriptor_byte_size(descriptor);
    u64 alignment = descriptor_byte_alignment(descriptor);

    Label_Index label_index = allocate_section_memory(context->program, section, byte_size, alignment);
    Storage global_label = data_label32(label_index, byte_size);
    global_value = value_make(context, descriptor, global_label, expression.source_range);

    if (value_is_non_lazy_static(value)) {
      MASS_ON_ERROR(assign(context, 0, global_value, value)) return;
    } else {
      Mass_Assignment_Lazy_Payload *assignment_payload =
        allocator_allocate(context->allocator, Mass_Assignment_Lazy_Payload);
      *assignment_payload = (Mass_Assignment_Lazy_Payload) {
        .source_range = expression.source_range,
        .target = global_value,
        .expression = value,
      };

      Value *body_value = mass_make_lazy_value(
        context, symbol->source_range, assignment_payload, &descriptor_void,
        mass_handle_assignment_lazy_proc
      );

      Function_Literal *startup_literal = mass_make_fake_function_literal(
        context, body_value, &descriptor_void, &expression.source_range
      );
      Value *startup_function = value_make(
        context, &descriptor_function_literal, storage_static(startup_literal), value->source_range
      );
      ensure_function_instance(context, startup_function, (Value_View){0});
      dyn_array_push(context->program->startup_functions, startup_function);
    }
  }

  Slice scope_name = value_as_symbol(symbol)->name;
  scope_define_value(context->scope, VALUE_STATIC_EPOCH, symbol->source_range, scope_name, global_value);
}

static void
token_define_local_variable(
  Execution_Context *context,
  Value *symbol,
  Lazy_Value *out_lazy_value,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = token_parse_expression(context, expression, &(u64){0}, 0);
  MASS_ON_ERROR(*context->result) return;
  const Descriptor *descriptor = value_or_lazy_value_descriptor(value);

  const Descriptor *variable_descriptor;
  if (descriptor == &descriptor_number_literal) {
    // x := 42 should always be initialized to s64 to avoid weird suprises
    variable_descriptor = &descriptor_s64;
  } else if (descriptor == &descriptor_tuple) {
    const Tuple *tuple = storage_static_as_c_type(&value->storage, Tuple);

    u64 struct_bit_size = 0;
    u64 struct_bit_alignment = 0;
    Array_Memory_Layout_Item fields = dyn_array_make(
      Array_Memory_Layout_Item,
      .allocator = context->allocator,
      .capacity = dyn_array_length(tuple->items),
    );
    for (u64 i = 0; i < dyn_array_length(tuple->items); ++i) {
      Value *item = *dyn_array_get(tuple->items, i);

      char buffer[32];
      // TODO check return type
      int buffer_length = snprintf(buffer, countof(buffer), "%"PRIu64, i);
      char *copy = allocator_allocate_bytes(context->allocator, buffer_length, _Alignof(char));
      memcpy(copy, buffer, buffer_length);

      Slice field_name = {.bytes = copy, .length = s32_to_u64(buffer_length)};
      const Descriptor *field_descriptor = item->descriptor;
      if (field_descriptor == &descriptor_number_literal) {
        field_descriptor = &descriptor_s64;
      }

      u64 field_bit_alignment = descriptor_bit_alignment(field_descriptor);
      struct_bit_size = u64_align(struct_bit_size, field_bit_alignment);
      u64 field_bit_offset = struct_bit_size;
      struct_bit_size += descriptor_bit_size(field_descriptor);
      struct_bit_alignment = u64_max(struct_bit_alignment, field_bit_alignment);

      u64 field_byte_offset = u64_align(field_bit_offset, CHAR_BIT) / CHAR_BIT;
      if (field_byte_offset * CHAR_BIT != field_bit_offset) {
        panic("TODO support non-byte aligned sizes");
      }

      dyn_array_push(fields, (Memory_Layout_Item) {
        .tag = Memory_Layout_Item_Tag_Base_Relative,
        .declaration = {
          // TODO provide source_range
          .name = field_name,
          .descriptor = field_descriptor,
        },
        .Base_Relative.offset = field_byte_offset,
      });
    }

    struct_bit_size = u64_align(struct_bit_size, struct_bit_alignment);

    Descriptor *tuple_descriptor = allocator_allocate(context->allocator, Descriptor);
    *tuple_descriptor = (Descriptor) {
      .tag = Descriptor_Tag_Struct,
      .bit_size = struct_bit_size,
      .bit_alignment = struct_bit_alignment,
      .Struct = {
        .is_tuple = true,
        .memory_layout = {
          .items = fields,
        }
      },
    };

    variable_descriptor = tuple_descriptor;
  } else {
    variable_descriptor = descriptor;
  }

  Mass_Variable_Definition_Lazy_Payload *variable_payload =
    allocator_allocate(context->allocator, Mass_Variable_Definition_Lazy_Payload);
  *variable_payload = (Mass_Variable_Definition_Lazy_Payload){
    .name_token = symbol,
    .descriptor = variable_descriptor,
  };

  Value *variable_value = mass_make_lazy_value(
    context, symbol->source_range, variable_payload, variable_descriptor,
    mass_handle_variable_definition_lazy_proc
  );

  const Source_Range *source_range = &symbol->source_range;

  Slice scope_name = value_as_symbol(symbol)->name;
  scope_define_value(context->scope, context->epoch, *source_range, scope_name, variable_value);

  Mass_Assignment_Lazy_Payload *assignment_payload =
    allocator_allocate(context->allocator, Mass_Assignment_Lazy_Payload);
  *assignment_payload = (Mass_Assignment_Lazy_Payload) {
    .source_range = *source_range,
    .target = variable_value,
    .expression = value,
  };

  out_lazy_value->proc = mass_handle_assignment_lazy_proc;
  out_lazy_value->payload = assignment_payload;
}

static u64
token_parse_definition_and_assignment_statements(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value_View lhs;
  Value_View rhs;
  Value *operator;

  u64 statement_length = 0;
  view = value_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal(":="), &lhs, &rhs, &operator)) {
    return 0;
  }
  if (lhs.length > 1) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unimplemented,
      .source_range = lhs.source_range,
      .detailed_message = slice_literal("multiple assignments"),
    });
    goto err;
  }
  Value *name_token = value_view_get(view, 0);

  if (!value_is_symbol(name_token)) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Invalid_Identifier,
      .source_range = name_token->source_range,
      .Invalid_Identifier = {.id = name_token},
    });
    goto err;
  }

  if (context->flags & Execution_Context_Flags_Global) {
    token_define_global_variable(context, name_token, rhs);
  } else {
    token_define_local_variable(context, name_token, out_lazy_value, rhs);
  }

  err:
  return statement_length;
}

static u64
token_parse_assignment(
  Execution_Context *context,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value_View lhs;
  Value_View rhs;
  Value *operator;
  u64 statement_length = 0;
  view = value_view_match_till_end_of_statement(view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal("="), &lhs, &rhs, &operator)) {
    return 0;
  }

  Mass_Assignment_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_Assignment_Lazy_Payload);

  Value *target = 0;
  token_parse_definition(context, lhs, 0, &target);
  if (target) {
    Value *expression_parse = token_parse_expression(context, rhs, &(u64){0}, 0);
    *payload = (Mass_Assignment_Lazy_Payload) {
      .source_range = operator->source_range,
      .target = target,
      .expression = expression_parse,
    };
  } else {
    Value *target = token_parse_expression(context, lhs, &(u64){0}, 0);
    Value *expression_parse = token_parse_expression(context, rhs, &(u64){0}, 0);

    *payload = (Mass_Assignment_Lazy_Payload) {
      .source_range = operator->source_range,
      .target = target,
      .expression = expression_parse,
    };
  }
  MASS_ON_ERROR(*context->result) return 0;

  out_lazy_value->proc = mass_handle_assignment_lazy_proc;
  out_lazy_value->payload = payload;

  return statement_length;
}

static void
scope_define_enum(
  const Allocator *allocator,
  Scope *scope,
  Source_Range source_range,
  Slice enum_name,
  Value *enum_type_value,
  C_Enum_Item *items,
  u64 item_count
) {
  Scope *enum_scope = scope_make(allocator, 0);
  for (u64 i = 0; i < item_count; ++i) {
    C_Enum_Item *it = &items[i];
    const Descriptor *enum_descriptor =
      *storage_static_as_c_type(&enum_type_value->storage, const Descriptor *);
    Value *item_value = value_init(
      allocator_allocate(allocator, Value),
      enum_descriptor, storage_static(&it->value), source_range
    );
    scope_define_value(enum_scope, VALUE_STATIC_EPOCH, source_range, it->name, item_value);
  }

  Value *enum_value = value_init(
    allocator_allocate(allocator, Value),
    &descriptor_scope, storage_static(enum_scope), source_range
  );
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, enum_name, enum_value);

  scope_define_value(enum_scope, VALUE_STATIC_EPOCH, source_range, slice_literal("_Type"), enum_type_value);
}

static void
module_compiler_init(
  Compilation *compilation,
  Module *out_module
) {
  const Calling_Convention *calling_convention =
    compilation->jit.program->default_calling_convention;
  const Allocator *allocator = compilation->allocator;
  Scope *scope = scope_make(allocator, compilation->root_scope);
  *out_module = (Module) {
    .source_file = {
      .path = slice_literal("__mass_internal__"),
    },
    .own_scope = scope,
    .export = {
      .tag = Module_Export_Tag_All,
      .scope = scope,
    },
  };

  compiler_scope_define_exports(compilation, scope);
  Value *allocator_value = value_init(
    allocator_allocate(allocator, Value),
    &descriptor_allocator_pointer,
    storage_static_inline(&allocator),
    COMPILER_SOURCE_RANGE
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("allocator"), allocator_value
  );

  MASS_DEFINE_FUNCTION(
    Descriptor_Function_Flags_None,
    compile_time_eval, "compile_time_eval", &descriptor_value_pointer,
    MASS_FN_ARG("context", &descriptor_execution_context_pointer),
    MASS_FN_ARG("view", &descriptor_value_view),
  );
}

static void
scope_define_builtins(
  Compilation *compilation,
  Scope *scope,
  const Calling_Convention *calling_convention
) {
  Execution_Context *context = &(Execution_Context){0};
  *context = execution_context_from_compilation(compilation);
  const Allocator *allocator = context->allocator;

  global_scope_define_exports(compilation, scope);
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("void"), type_void_value
  );
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, COMPILER_SOURCE_RANGE,
    slice_literal("Type"), type_descriptor_pointer_value
  );

  MASS_MUST_SUCCEED(scope_define_operator(scope, COMPILER_SOURCE_RANGE, slice_literal("\\"), allocator_make(allocator, Operator,
    .precedence = 30,
    .fixity = Operator_Fixity_Prefix,
    .associativity = Operator_Associativity_Right,
    .argument_count = 1,
    .handler = mass_handle_reflect_operator,
  )));
  MASS_MUST_SUCCEED(scope_define_operator(scope, COMPILER_SOURCE_RANGE, slice_literal("\\"), allocator_make(allocator, Operator,
    .precedence = 29,
    .fixity = Operator_Fixity_Postfix,
    .associativity = Operator_Associativity_Left,
    .argument_count = 1,
    .handler = mass_handle_reify_operator,
  )));
  MASS_MUST_SUCCEED(scope_define_operator(scope, COMPILER_SOURCE_RANGE, slice_literal("."), allocator_make(allocator, Operator,
    .precedence = 20,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .argument_count = 2,
    .handler = mass_handle_dot_operator,
  )));
  MASS_MUST_SUCCEED(scope_define_operator(scope, COMPILER_SOURCE_RANGE, slice_literal(" "), allocator_make(allocator, Operator,
    .precedence = 20,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .argument_count = 2,
    .handler = mass_handle_apply_operator,
  )));
  MASS_MUST_SUCCEED(scope_define_operator(scope, COMPILER_SOURCE_RANGE, slice_literal("@"), allocator_make(allocator, Operator,
    .precedence = 20,
    .fixity = Operator_Fixity_Prefix,
    .associativity = Operator_Associativity_Right,
    .argument_count = 1,
    .handler = mass_handle_at_operator,
  )));

  {
    static const Token_Statement_Matcher default_statement_matchers[] = {
      {.previous = 0,                              .proc = token_parse_constant_definitions},
      {.previous = &default_statement_matchers[0], .proc = token_parse_explicit_return},
      {.previous = &default_statement_matchers[1], .proc = token_parse_definition},
      {.previous = &default_statement_matchers[2], .proc = token_parse_definition_and_assignment_statements},
      {.previous = &default_statement_matchers[3], .proc = token_parse_assignment},
      {.previous = &default_statement_matchers[4], .proc = token_parse_inline_machine_code_bytes},
      {.previous = &default_statement_matchers[5], .proc = token_parse_statement_label},
      {.previous = &default_statement_matchers[6], .proc = token_parse_statement_using},
      {.previous = &default_statement_matchers[7], .proc = token_parse_syntax_definition},
      {.previous = &default_statement_matchers[8], .proc = token_parse_operator_definition},
      {.previous = &default_statement_matchers[9], .proc = token_parse_exports},
    };

    scope->statement_matcher = &default_statement_matchers[countof(default_statement_matchers) - 1];
  }
}

static inline Mass_Result
program_parse(
  Execution_Context *context
) {
  assert(context->module);
  Value_View tokens;

  Performance_Counter perf = system_performance_counter_start();
  MASS_TRY(tokenize(context->compilation, &context->module->source_file, &tokens));
  if (0) {
    u64 usec = system_performance_counter_end(&perf);
    printf("Tokenizer took %"PRIu64" µs\n", usec);
  }

  Value *block_result = token_parse_block_view(context, tokens);
  value_force_exact(context, 0, &void_value, block_result);
  return *context->result;
}

static Fixed_Buffer *
program_absolute_path(
  Slice raw_path
) {
  Slice result_path = raw_path;

  #ifdef _WIN32
  bool is_relative_path = raw_path.length < 2 || raw_path.bytes[1] != ':';

  Fixed_Buffer *sys_buffer = 0;
  if (is_relative_path) {
    sys_buffer = fixed_buffer_make(
      .allocator = allocator_system,
      .capacity = 10 * 1024
    );
    s32 current_dir_size = GetCurrentDirectory(0, 0) * sizeof(wchar_t);
    sys_buffer->occupied =
      GetCurrentDirectory(current_dir_size, (wchar_t *)sys_buffer->memory) * sizeof(wchar_t);
    fixed_buffer_append_s16(sys_buffer, L'\\');
    Allocator *convert_allocator = fixed_buffer_allocator_init(sys_buffer, &(Allocator){0});
    utf8_to_utf16_null_terminated(convert_allocator, raw_path);
    wchar_t *wide_string = (wchar_t *)sys_buffer->memory;
    result_path = utf16_null_terminated_to_utf8(convert_allocator, wide_string);
  }
  #else
  bool is_relative_path = !slice_starts_with(raw_path, slice_literal("/"));
  Fixed_Buffer *sys_buffer = 0;
  if (is_relative_path) {
    char cwd[1024];
    if (getcwd(cwd, sizeof(cwd)) != 0) {
      sys_buffer = fixed_buffer_make(
        .allocator = allocator_system,
        .capacity = 10 * 1024
      );
      fixed_buffer_append_slice(sys_buffer, slice_from_c_string(cwd));
      fixed_buffer_append_u8(sys_buffer, '/');
      fixed_buffer_append_slice(sys_buffer, raw_path);
      result_path = fixed_buffer_as_slice(sys_buffer);
    }
  }
  #endif
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = result_path.length + 1024
  );

  fixed_buffer_append_slice(result_buffer, result_path);

  if (sys_buffer) fixed_buffer_destroy(sys_buffer);
  return result_buffer;
}

static inline void
program_module_init(
  Module *module,
  Slice file_path,
  Slice text,
  Scope *scope
) {
  *module = (Module) {
    .source_file = {
      .path = file_path,
      .text = text,
    },
    .own_scope = scope,
  };
}

static Module *
program_module_from_file(
  Execution_Context *context,
  Slice file_path,
  Scope *scope
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = program_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path);
  if (!buffer) {
    context_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_File_Open,
      .source_range = COMPILER_SOURCE_RANGE,
      .detailed_message = "Unable to open the file",
      .File_Open = {.path = file_path},
    });
    return 0;
  }

  Module *module = allocator_allocate(context->allocator, Module);
  program_module_init(module, file_path, fixed_buffer_as_slice(buffer), scope);
  return module;
}

static Mass_Result
program_import_module(
  Execution_Context *context,
  Module *module
) {
  MASS_TRY(*context->result);
  Execution_Context import_context = execution_context_from_compilation(context->compilation);
  import_context.module = module;
  import_context.scope = module->own_scope;
  Mass_Result parse_result = program_parse(&import_context);
  MASS_TRY(parse_result);
  switch(module->export.tag) {
    case Module_Export_Tag_None: {
      module->export.scope = scope_make(context->allocator, module->own_scope->parent);
      break;
    }
    case Module_Export_Tag_All: {
      module->export.scope = module->own_scope;
      break;
    }
    case Module_Export_Tag_Selective: {
      module->export.scope = scope_make(context->allocator, module->own_scope->parent);
      Array_Value_Ptr symbols = module->export.Selective.symbols;
      for(u64 i = 0; i < dyn_array_length(symbols); i += 1) {
        Value **symbol_pointer = dyn_array_get(symbols, i);
        Value *symbol = *symbol_pointer;
        assert(value_is_symbol(symbol));
        Slice name = value_as_symbol(symbol)->name;
        Scope_Entry *entry = scope_lookup_shallow(module->own_scope, name);
        if (!entry) {
          context_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Undefined_Variable,
            .source_range = symbol->source_range,
            .Undefined_Variable = {.name = name},
          });
          return *context->result;
        }

        Value_View expr = value_view_single(symbol_pointer);
        scope_define_lazy_compile_time_expression(&import_context, module->export.scope, name, expr);
      }
      break;
    }
  }
  return *context->result;
}

static Mass_Result
program_load_file_module_into_root_scope(
  Execution_Context *context,
  Slice file_path
) {
  Module *module = program_module_from_file(context, file_path, context->compilation->root_scope);
  return program_import_module(context, module);
}

