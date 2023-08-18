#include "prelude.h"
#include "source.h"
#include "function.h"

#include "generated_exports.c"
#include "tokenizer.c"

static inline bool
mass_value_ensure_static(
  Mass_Context *context,
  Value *value
) {
  if (!mass_value_is_static(value)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Expected_Static,
      .source_range = value->source_range,
    });
    return false;
  }
  return true;
}

static inline bool
mass_value_ensure_static_of(
  Mass_Context *context,
  Value *value,
  const Descriptor *expected_descriptor
) {
  if (!mass_value_ensure_static(context, value)) return false;
  if (!same_type(value->descriptor, expected_descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = value->source_range,
      .Type_Mismatch = { .expected = expected_descriptor, .actual = value->descriptor },
    });
    return false;
  }
  return true;
}

static inline Value *
mass_value_from_expected_result(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Source_Range source_range
) {
  const Descriptor *descriptor = mass_expected_result_descriptor(expected_result);
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Storage storage = expected_result->Exact.storage;
      return value_make(context, descriptor, storage, source_range);
    } break;
    case Expected_Result_Tag_Flexible: {
      Storage storage;
      if (descriptor->bit_size.as_u64) {
        if (descriptor->bit_size.as_u64 <= 64) {
          Register reg = register_acquire_temp(builder);
          storage = storage_register(reg, descriptor->bit_size);
        } else {
          storage = reserve_stack_storage(builder, descriptor->bit_size);
        }
      } else {
        storage = imm0;
      }
      storage.flags |= Storage_Flags_Temporary;
      return value_make(context, descriptor, storage, source_range);
    } break;
  }
  panic("UNREACHABLE");
  return 0;
}

static Value *
expected_result_validate(
  const Expected_Result *expected_result,
  Value *actual_value
) {
  if (!actual_value) return 0;
  assert(actual_value->tag == Value_Tag_Forced);
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      assert(same_type(expected_result->Exact.descriptor, actual_value->descriptor));
      assert(storage_equal(&expected_result->Exact.storage, &actual_value->Forced.storage));
    } break;
    case Expected_Result_Tag_Flexible: {
      const Expected_Result_Flexible *flexible = &expected_result->Flexible;
      if (flexible->descriptor) {
        assert(same_type(flexible->descriptor, actual_value->descriptor));
      }
    } break;
  }
  return actual_value;
}

static inline Scope *
scope_make_declarative(
  const Allocator *allocator,
  const Scope *parent
) {
  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .tag = Scope_Tag_Declarative,
    .allocator = allocator,
    .parent = parent,
    .Declarative = {
      .map = 0,
    }
  };
  return scope;
}

static inline Scope *
scope_make_imperative(
  const Allocator *allocator,
  const Scope *parent,
  const Scope_Entry *scope_entry
) {
  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .tag = Scope_Tag_Imperative,
    .allocator = allocator,
    .parent = parent,
    .Imperative = {
      .entry = *scope_entry,
    }
  };
  return scope;
}

static void
mass_copy_scope_exports(
  Scope *to,
  const Scope *from
) {
  assert(to->tag == Scope_Tag_Declarative);
  assert(from->tag == Scope_Tag_Declarative);
  for (u64 i = 0; i < from->Declarative.map->capacity; ++i) {
    Scope_Map__Entry *map_entry = &from->Declarative.map->entries[i];
    if (!map_entry->occupied) continue;
    Scope_Entry *entry = map_entry->value;
    const Symbol *symbol = map_entry->key;
    scope_define_value(to, entry->epoch, entry->source_range, symbol, entry->value);
  }
}

static void
scope_print_names(
  const Scope *scope,
  Scope_Print_Flags flags
) {
  for (; scope; scope = scope->parent) {
    switch (scope->tag) {
      case Scope_Tag_Imperative: {
        const Scope_Entry *entry = &scope->Imperative.entry;
        slice_print(entry->name);
        printf(" ; ");
      } break;
      case Scope_Tag_Declarative: {
        Scope_Map *map = scope->Declarative.map;
        if (map) {
          for (u64 i = 0; i < map->capacity; ++i) {
            Scope_Map__Entry *entry = &map->entries[i];
            if (entry->occupied) {
              slice_print(entry->value->name);
              printf(" ; ");
            }
          }
        }
        if (flags & Scope_Print_Flags_Stop_At_First_Declarative) goto end;
      } break;
    }
  }
  end:
  printf("\n");
}

static inline Scope_Entry *
scope_lookup_shallow(
  const Scope *scope,
  const Symbol *symbol
) {
  switch (scope->tag) {
    case Scope_Tag_Imperative: {
      // TODO avoid cast
      Scope_Entry *entry = (Scope_Entry *)&scope->Imperative.entry;
      // TODO use symbols here?
      if (slice_equal(entry->name, symbol->name)) return entry;
    } break;
    case Scope_Tag_Declarative: {
      Scope_Map *map = scope->Declarative.map;
      if (!map) return 0;
      Scope_Entry **entry_pointer = hash_map_get(map, symbol);
      if (entry_pointer) return *entry_pointer;
    } break;
  }
  return 0;
}

static inline Scope_Entry *
scope_lookup(
  const Scope *scope,
  const Symbol *symbol
) {
  for (; scope; scope = scope->parent) {
    Scope_Entry *entry = scope_lookup_shallow(scope, symbol);
    if (entry) return entry;
  }
  return 0;
}

static Value *
token_value_force_immediate_integer(
  Mass_Context *context,
  Value *value,
  const Descriptor *target_descriptor,
  const Source_Range *source_range
) {
  assert(descriptor_is_integer(target_descriptor));
  if (value_is_i64(value)) {
    u64 bits = 0xCCccCCccCCccCCcc;
    u64 bit_size = 0xCCccCCccCCccCCcc;
    Literal_Cast_Result cast_result =
      value_i64_cast_to(value, target_descriptor, &bits, &bit_size);
    switch(cast_result) {
      case Literal_Cast_Result_Success: {
        Storage imm = storage_immediate_with_bit_size(&bits, (Bits){bit_size});
        return value_make(context, target_descriptor, imm, *source_range);
      }
      case Literal_Cast_Result_Target_Not_An_Integer: {
        panic("We already checked that target is an integer");
        return 0;
      }
      case Literal_Cast_Result_Target_Too_Small: {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Integer_Range,
          .source_range = *source_range,
          .Integer_Range = { .descriptor = target_descriptor },
          .detailed_message = slice_literal("Literal value does not fit into the target integer size"),
        });
        return 0;
      }
      case Literal_Cast_Result_Target_Too_Big: {
        panic("Integers larger than 64 bits are not supported");
        return 0;
      }
    }
    panic("Unexpected literal cast result");
  } else {
    panic("Trying to force non-literal immediate");
  }

  return value;
}

static inline Storage
storage_adjusted_for_lea(
  Storage source
) {
  assert(source.tag == Storage_Tag_Memory);
  // `LEA` is a weird instruction in that the size of the operands affects
  // what the instruction *does*, instead of describing the operands.
  // For the purposes of this compiler we always want it to generate 64-bit
  // effective address and then store that full address in the target register.
  // This is why here we are forcing the source memory operand to be 8 bytes.
  Storage adjusted_source = source;
  adjusted_source.bit_size.as_u64 = 64;
  return adjusted_source;
}

static void
mass_storage_load_address(
  Function_Builder *builder,
  const Source_Range *source_range,
  const Scope *scope,
  const Storage *target,
  const Storage *source
) {
  bool can_reuse_result_as_temp = target->tag == Storage_Tag_Register;
  Storage register_storage = can_reuse_result_as_temp
    ? *target
    : storage_register_temp(builder, target->bit_size);

  assert(register_storage.bit_size.as_u64 == 64);
  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range, scope,
    &(Instruction_Assembly){x64_lea, {register_storage, storage_adjusted_for_lea(*source)}}
  );

  if (!can_reuse_result_as_temp) {
    assert(register_storage.tag == Storage_Tag_Register);
    move_value(builder, scope, source_range, target, &register_storage);
    register_release(builder, register_storage.Register.index);
  }
}

static bool
assign_from_static(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Scope *scope,
  const Source_Range *source_range
) {
  const Storage *source_storage = &value_as_forced(source)->storage;
  const Storage *target_storage = &value_as_forced(target)->storage;
  if (
    !context_is_compile_time_eval(context) &&
    !mass_value_is_static(target) &&
    source->descriptor->tag == Descriptor_Tag_Pointer_To
  ) {
    void *source_memory = *(void **)storage_static_memory_with_bit_size(source_storage, (Bits){64});
    if (!source_memory) {
      Storage null_pointer = imm64(0);
      move_value(builder, scope, source_range, target_storage, &null_pointer);
      return true;
    }
    // If a `static_pointer_length_map` contains the pointer, it is actually a C-like array
    // and the length (item count) is the value from the map.
    u64 *maybe_custom_array_length =
      hash_map_get(context->compilation->static_pointer_length_map, source_memory);
    const Descriptor *pointee_descriptor = source->descriptor->Pointer_To.descriptor;
    if (maybe_custom_array_length) {
      pointee_descriptor = descriptor_array_of(
        context->allocator, pointee_descriptor, *maybe_custom_array_length
      );
    }
    Bits bit_size = pointee_descriptor->bit_size;

    Section *section = (source->flags & Value_Flags_Constant)
      ? &context->program->memory.ro_data
      : &context->program->memory.rw_data;
    u64 byte_size = descriptor_byte_size(pointee_descriptor);
    u64 alignment = descriptor_byte_alignment(pointee_descriptor);

    // TODO This can probably be deduped for `ro_data` items
    Label *label = allocate_section_memory(
      context->allocator, context->program, section, byte_size, alignment
    );
    Storage target_program_storage = data_label32(label, bit_size);

    Value static_source_value = {
      .tag = Value_Tag_Forced,
      .descriptor = pointee_descriptor,
      .source_range = *source_range,
      .Forced = {
         .storage = storage_static_heap(source_memory, bit_size),
      },
    };

    // It is important to call assign here to make sure we recursively handle
    // any complex types such as structs and arrays
    {
      Value target_program_value;
      value_init(&target_program_value, pointee_descriptor, target_program_storage, *source_range);
      mass_assign_helper(context, builder, &target_program_value, &static_source_value, scope, source_range);
    }
    if (mass_has_error(context)) return true;

    assert(storage_is_label(&target_program_storage));
    if (storage_is_label(target_storage)) {
      dyn_array_push(context->program->relocations, (Relocation) {
        .patch_at = *target_storage,
        .address_of = target_program_storage,
      });
    } else {
      mass_storage_load_address(builder, source_range, scope, target_storage, &target_program_storage);
    }
    return true;
  } else if (storage_is_label(target_storage)) {
    void *section_memory = rip_value_pointer_from_storage(target_storage);
    const void *source_memory =
      storage_static_memory_with_bit_size(source_storage, source_storage->bit_size);
    // TODO the actual copying probably should be deferred till we are ready to write out code
    memcpy(section_memory, source_memory, source_storage->bit_size.as_u64 / 8);
    return true;
  }
  return false;
}

static Value *
value_indirect_from_pointer(
  Mass_Context *context,
  Function_Builder *builder,
  Value *source,
  const Scope *scope,
  const Source_Range *source_range
) {
  const Descriptor *referenced_descriptor;
  if (source->descriptor->tag == Descriptor_Tag_Pointer_To) {
    referenced_descriptor = source->descriptor->Pointer_To.descriptor;
  } else {
    panic("Unexpected descriptor tag for an indirect value");
    return 0;
  }
  if (source->tag == Value_Tag_Lazy) {
    Expected_Result expected_result = expected_result_any(source->descriptor);
    source = value_force(context, builder, scope, &expected_result, source);
    if (mass_has_error(context)) return 0;
  }
  const Storage *source_storage = &value_as_forced(source)->storage;

  switch(source_storage->tag) {
    case Storage_Tag_Register: {
      Register reg = source_storage->Register.index;
      Storage referenced_storage = storage_indirect(referenced_descriptor->bit_size, reg);
      referenced_storage.flags |= source_storage->flags & Storage_Flags_Temporary;
      Value *value = value_make(context, referenced_descriptor, referenced_storage, *source_range);
      return value;
    }
    case Storage_Tag_Memory: {
      Register reg = register_acquire_temp(builder);
      Storage reg_storage = storage_register(reg, source->descriptor->bit_size);
      move_value(builder, scope, source_range, &reg_storage, source_storage);
      storage_release_if_temporary(builder, source_storage);
      Storage referenced_storage = storage_indirect(referenced_descriptor->bit_size, reg);
      referenced_storage.flags |= Storage_Flags_Temporary;
      return value_make(context, referenced_descriptor, referenced_storage, *source_range);
    }
    default:
    case Storage_Tag_Disjoint:
    case Storage_Tag_Immediate:
    case Storage_Tag_Static:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm:{
      panic("Unexpected storage for a reference");
      return 0;
    }
  }
}

typedef struct {
  u64 bit_size;
  u64 bit_alignment;
} C_Struct_Aligner;

static u64
c_struct_aligner_next_byte_offset(
  C_Struct_Aligner *aligner,
  const Descriptor *descriptor
) {
  u64 field_bit_alignment = descriptor->bit_alignment.as_u64;
  if (field_bit_alignment) {
    aligner->bit_size = u64_align(aligner->bit_size, field_bit_alignment);
  }
  u64 field_bit_offset = aligner->bit_size;
  aligner->bit_size += descriptor->bit_size.as_u64;
  aligner->bit_alignment = u64_max(aligner->bit_alignment, field_bit_alignment);

  u64 field_byte_offset = u64_align(field_bit_offset, CHAR_BIT) / CHAR_BIT;
  if (field_byte_offset * CHAR_BIT != field_bit_offset) {
    panic("TODO support non-byte aligned sizes");
  }
  return field_byte_offset;
}

static void
c_struct_aligner_end(
  C_Struct_Aligner *aligner
) {
  if (aligner->bit_size) {
    aligner->bit_size = u64_align(aligner->bit_size, aligner->bit_alignment);
  }
}

static inline bool
struct_find_field_by_name(
  const Descriptor *descriptor,
  Slice field_name,
  const Struct_Field **out_field,
  u64 *out_index
) {
  assert(descriptor->tag == Descriptor_Tag_Struct);
  for(u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
    const Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
    if (slice_equal(field->name, field_name)) {
      *out_field = field;
      *out_index = i;
      return true;
    }
  }
  return false;
}

static inline u64
mass_tuple_length(
  const Tuple *tuple
) {
  return dyn_array_length(tuple->items);
}

static inline Value *
mass_tuple_get(
  const Tuple *tuple,
  u64 index
) {
  return *dyn_array_get(tuple->items, index);
}

typedef enum {
  Tuple_Eval_Mode_Value,
  Tuple_Eval_Mode_Type,
} Tuple_Eval_Mode;

static Descriptor *
anonymous_struct_descriptor_from_tuple(
  Mass_Context *context,
  const Tuple *tuple,
  Tuple_Eval_Mode tuple_eval_mode
) {
  Descriptor *tuple_descriptor = 0;
  Temp_Mark temp_mark = context_temp_mark(context);

  C_Struct_Aligner struct_aligner = {0};
  Array_Struct_Field fields = dyn_array_make(
    Array_Struct_Field,
    .allocator = context->allocator,
    .capacity = dyn_array_length(tuple->items),
  );

  Slice_Set *field_name_set = hash_map_make(
    Slice_Set,
    .initial_capacity = mass_tuple_length(tuple) * 2,
    .allocator = context->temp_allocator,
  );

  for (u64 i = 0; i < mass_tuple_length(tuple); ++i) {
    Value *item = mass_tuple_get(tuple, i);
    Slice name = {0};

    const Descriptor *field_descriptor;
    switch(tuple_eval_mode) {
      case Tuple_Eval_Mode_Value: {
        if (value_is_assignment(item)) {
          const Assignment *assignment = value_as_assignment(item);
          if (!mass_value_ensure_static_of(context, assignment->target, &descriptor_named_accessor)) {
            goto err;
          }
          const Named_Accessor *accessor = value_as_named_accessor(assignment->target);
          name = accessor->symbol->name;
          field_descriptor = deduce_runtime_descriptor_for_value(context, assignment->source, 0);
        } else {
          field_descriptor = deduce_runtime_descriptor_for_value(context, item, 0);
        }
      } break;
      case Tuple_Eval_Mode_Type: {
        if (value_is_typed_symbol(item)) {
          const Typed_Symbol *typed_symbol = value_as_typed_symbol(item);
          name = typed_symbol->symbol->name;
          field_descriptor = typed_symbol->descriptor;
        } else {
          field_descriptor = value_ensure_type(
            context, tuple->scope_where_it_was_created, item, item->source_range
          );
        }
      } break;
      default: {
        panic("UNREACHABLE");
        return 0;
      } break;
    }
    if (!field_descriptor) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_No_Runtime_Use,
        .source_range = item->source_range,
        .detailed_message = slice_literal("Could not deduce a runtime type for the tuple element"),
      });
      return 0;
    }
    u64 field_byte_offset = c_struct_aligner_next_byte_offset(&struct_aligner, field_descriptor);
    if (name.length) {
      u64 *previous_index = hash_map_get(field_name_set, name);
      if (previous_index) {
        const Source_Range* previous_range = &mass_tuple_get(tuple, *previous_index)->source_range;
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Redefinition,
          .source_range = item->source_range,
          .other_source_range = *previous_range,
          .Redefinition = { .name = name },
        });
        goto err;
      } else {
        hash_map_set(field_name_set, name, i);
      }
    }

    dyn_array_push(fields, (Struct_Field) {
      .name = name,
      .descriptor = field_descriptor,
      .offset = field_byte_offset,
    });
  }
  c_struct_aligner_end(&struct_aligner);

  tuple_descriptor = mass_allocate(context, Descriptor);
  *tuple_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .bit_size = {struct_aligner.bit_size},
    .bit_alignment = {struct_aligner.bit_alignment},
    .Struct = {
      .fields = fields,
    },
  };

  err:
  context_temp_reset_to_mark(context, temp_mark);

  return tuple_descriptor;
}

typedef void(*Mass_Report_Tuple_Error_Proc)(
  Mass_Context *,
  Mass_Error
);
typedef bool(*Mass_Process_Tuple_Item_Proc)(
  Mass_Context *,
  const Descriptor *,
  u64 offset,
  Value *tuple_item,
  const Source_Range *source_range,
  void *payload
);

static bool
mass_process_tuple_as_descriptor(
  Mass_Context *context,
  const Tuple *tuple,
  const Descriptor *descriptor,
  const Source_Range *source_range,
  Mass_Report_Tuple_Error_Proc report_error_proc,
  Mass_Process_Tuple_Item_Proc process_item_at_offset_proc,
  void *payload
) {
  Temp_Mark temp_mark = context_temp_mark(context);
  if (descriptor->tag == Descriptor_Tag_Struct) {
    assert(descriptor->tag == Descriptor_Tag_Struct);
    Array_Struct_Field fields = descriptor->Struct.fields;
    Struct_Field_Set *assigned_set = hash_map_make(
      Struct_Field_Set,
      .initial_capacity = dyn_array_length(fields) * 2,
      .allocator = context->temp_allocator,
    );

    u64 field_index = 0;
    for (u64 tuple_index = 0; tuple_index < dyn_array_length(tuple->items); ++tuple_index) {
      Value *tuple_item = *dyn_array_get(tuple->items, tuple_index);
      const Struct_Field *field;
      Value *field_source;
      if (value_is_named_accessor(tuple_item)) {
        const Symbol *symbol = value_as_named_accessor(tuple_item)->symbol;
        Scope_Entry *entry = scope_lookup(tuple->scope_where_it_was_created, symbol);
        if (!entry) {
          // This is a hard error instead of a callback because it is just a syntax sugar expansion
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Undefined_Variable,
            .Undefined_Variable = { .name = symbol->name },
            .source_range = tuple_item->source_range,
          });
          goto err;
        }
        // This is a hard error instead of a callback because it is just a syntax sugar expansion
        if (entry->epoch.as_u64 != tuple->epoch.as_u64) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Epoch_Mismatch,
            .source_range = tuple_item->source_range,
          });
          goto err;
        }
        field_source = scope_entry_force_value(context, entry);
        if (!struct_find_field_by_name(descriptor, symbol->name, &field, &field_index)) {
          report_error_proc(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Unknown_Field,
            .source_range = tuple_item->source_range,
            .Unknown_Field = { .name = symbol->name, .type = descriptor },
          });
          goto err;
        }
      } else if (value_is_assignment(tuple_item)) {
        const Assignment *assignment = value_as_assignment(tuple_item);
        field_source = assignment->source;
        if (!mass_value_ensure_static_of(context, assignment->target, &descriptor_named_accessor)) {
          goto err;
        }
        const Named_Accessor *accessor = value_as_named_accessor(assignment->target);
        if (!struct_find_field_by_name(descriptor, accessor->symbol->name, &field, &field_index)) {
          report_error_proc(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Unknown_Field,
            .source_range = tuple_item->source_range,
            .Unknown_Field = { .name = accessor->symbol->name, .type = descriptor },
          });
          goto err;
        }
      } else {
        field_source = tuple_item;
        if (field_index >= dyn_array_length(fields)) {
          Slice message = slice_literal("Tuple has too many items for the struct it is assigned to");
          report_error_proc(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Type_Mismatch,
            .source_range = tuple_item->source_range,
            .Type_Mismatch = { .expected = descriptor, .actual = &descriptor_tuple },
            .detailed_message = message,
          });
          goto err;
        }
        field = dyn_array_get(fields, field_index);
      }
      field_index += 1;
      const u64* previous_index = hash_map_get(assigned_set, field);
      if (previous_index) {
        const Source_Range* previous_range = &mass_tuple_get(tuple, *previous_index)->source_range;
        report_error_proc(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Redefinition,
          .source_range = tuple_item->source_range,
          .other_source_range = *previous_range,
          .Redefinition = { .name = field->name },
        });
        goto err;
      }

      bool success = process_item_at_offset_proc(
        context, field->descriptor, field->offset, field_source, source_range, payload
      );
      if (!success) goto err;

      Range_u64 field_overlap_range = {
        .from = field->offset,
        .to = field->offset + descriptor_byte_size(field->descriptor),
      };
      // Skip overlapped fields for unions
      // TODO @Speed if sorting is guaranteed can look only forward and back
      for (u64 i = 0; i < dyn_array_length(fields); ++i) {
        const Struct_Field *a_field = dyn_array_get(fields, i);
        if (range_contains(field_overlap_range, a_field->offset)) {
          hash_map_set(assigned_set, a_field, i);
        }
      }
      if (mass_has_error(context)) goto err;
    }

    if ((dyn_array_length(fields) != assigned_set->occupied)) {
      Slice message = slice_literal(
        "Tuple does not have enough items to match the struct it is assigned to"
      );
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .Type_Mismatch = { .expected = descriptor, .actual = &descriptor_tuple },
        .detailed_message = message,
      });
      goto err;
    }
  } else if (descriptor->tag == Descriptor_Tag_Fixed_Array) {
    // TODO support spreading in the middle of the tuple
    Spread *spread = 0;

    u64 array_length = descriptor->Fixed_Array.length;
    u64 tuple_length = dyn_array_length(tuple->items);
    if (tuple_length) {
      Value *last_item = *dyn_array_last(tuple->items);
      if (value_is_spread(last_item)) {
        tuple_length -= 1;
        spread = value_as_spread(last_item);
        // TODO support runtime values - we need to make sure that if it is a lazy value it is only forced once
        if (!mass_value_ensure_static(context, spread->value)) return 0;
      }
    }

    if (tuple_length > array_length || (!spread && tuple_length < array_length)) {
      Slice message = array_length > tuple_length
        ? slice_literal("Tuple does not have enough items to match the array it is assigned to")
        : slice_literal("Tuple has too many items for the array it is assigned to");
      report_error_proc(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .Type_Mismatch = { .expected = descriptor, .actual = &descriptor_tuple },
        .detailed_message = message,
      });
      goto err;
    }
    const Descriptor *item_descriptor = descriptor->Fixed_Array.item;
    u64 item_byte_size = descriptor_byte_size(item_descriptor);

    for (u64 index = 0; index < array_length; ++index) {
      Value *tuple_item;
      if (!spread || index < tuple_length) {
        tuple_item = *dyn_array_get(tuple->items, index);
      } else {
        tuple_item = spread->value;
      }
      bool success = process_item_at_offset_proc(
        context, item_descriptor, item_byte_size * index, tuple_item, source_range, payload
      );
      if (!success) goto err;
    }
  } else {
    report_error_proc(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      // FIXME :TupleAssignError need a better error here?
      .Type_Mismatch = { .expected = descriptor, .actual = &descriptor_tuple },
    });
    goto err;
  }

  context_temp_reset_to_mark(context, temp_mark);
  return true;

  err:
  context_temp_reset_to_mark(context, temp_mark);
  return false;
}

static inline bool
mass_tuple_is_static(
  Mass_Context *context,
  const Tuple *tuple
) {
  // TODO consider creating a tuple iterator to avoid copy-pasting code like this
  for (u64 tuple_index = 0; tuple_index < dyn_array_length(tuple->items); ++tuple_index) {
    Value *tuple_item = *dyn_array_get(tuple->items, tuple_index);
    const Value *item_value;
    if (value_is_named_accessor(tuple_item)) {
      const Symbol *symbol = value_as_named_accessor(tuple_item)->symbol;
      Scope_Entry *entry = scope_lookup(tuple->scope_where_it_was_created, symbol);
      if (!entry) {
        // This is a hard error instead of a callback because it is just a syntax sugar expansion
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Undefined_Variable,
          .Undefined_Variable = { .name = symbol->name },
          .source_range = tuple_item->source_range,
        });
        return false;
      }
      // This is a hard error instead of a callback because it is just a syntax sugar expansion
      if (entry->epoch.as_u64 != tuple->epoch.as_u64) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Epoch_Mismatch,
          .source_range = tuple_item->source_range,
        });
        return false;
      }
      item_value = scope_entry_force_value(context, entry);
    } else if (value_is_assignment(tuple_item)) {
      const Assignment *assignment = value_as_assignment(tuple_item);
      item_value = assignment->source;
    } else {
      item_value = tuple_item;
    }
    if (!mass_value_is_static(item_value)) return false;
    if (value_is_tuple(item_value)) {
      if (!mass_tuple_is_static(context, const_value_as_tuple(item_value))) return false;
    }
  }
  return true;
}

static inline bool
mass_deduce_tuple_item_proc(
  Mass_Context *context,
  const Descriptor *item_descriptor,
  u64 offset,
  Value *source,
  const Source_Range *source_range,
  void *payload
) {
  return !!deduce_runtime_descriptor_for_value(context, source, item_descriptor);
}

// TODO make this function produce a better error on failure
static const Descriptor *
deduce_runtime_descriptor_for_value(
  Mass_Context *context,
  Value *value,
  const Descriptor *maybe_desired_descriptor
) {
  if (maybe_desired_descriptor) {
    if (maybe_desired_descriptor->tag == Descriptor_Tag_Void) return maybe_desired_descriptor;
    if (same_type(value->descriptor, maybe_desired_descriptor)) return value->descriptor;
  }
  if (value->descriptor->tag == Descriptor_Tag_Never) {
    if (!same_type(value->descriptor, maybe_desired_descriptor)) return 0;
    return value->descriptor;
  }

  const Descriptor *deduced_descriptor = value->descriptor;

  if (mass_value_is_static(value)) {
    if (same_type(value->descriptor, &descriptor_i64)) {
      if (maybe_desired_descriptor && !same_type(maybe_desired_descriptor, &descriptor_i64)) {
        if (maybe_desired_descriptor->tag == Descriptor_Tag_Pointer_To) {
          u64 bits = value_as_i64(value)->bits;
          if (bits != 0) {
            return 0;
          }
        } else {
          Literal_Cast_Result cast_result =
            value_i64_cast_to(value, maybe_desired_descriptor, &(u64){0}, &(u64){0});
          if (cast_result != Literal_Cast_Result_Success) {
            return 0;
          }
        }
        deduced_descriptor = maybe_desired_descriptor;
      }
    } else if (same_type(value->descriptor, &descriptor_tuple)) {
      const Tuple *tuple = value_as_tuple(value);
      if (maybe_desired_descriptor) {
        switch(maybe_desired_descriptor->tag) {
          case Descriptor_Tag_Never:
          case Descriptor_Tag_Raw:
          case Descriptor_Tag_Integer:
          case Descriptor_Tag_Float:
          case Descriptor_Tag_Pointer_To:
          case Descriptor_Tag_Function_Instance: {
            // Can not convert to any of these
            return 0;
          } break;
          case Descriptor_Tag_Void: {
            deduced_descriptor = maybe_desired_descriptor;
          } break;
          case Descriptor_Tag_Fixed_Array:
          case Descriptor_Tag_Struct: {
            bool success = mass_process_tuple_as_descriptor(
              context, tuple, maybe_desired_descriptor, &value->source_range,
              (Mass_Report_Tuple_Error_Proc)mass_error, mass_deduce_tuple_item_proc, 0
            );
            if (success) {
              deduced_descriptor = maybe_desired_descriptor;
            } else {
              return 0;
            }
          } break;
        }
      } else {
        deduced_descriptor = anonymous_struct_descriptor_from_tuple(context, tuple, Tuple_Eval_Mode_Value);
      }
    } else if (same_type(value->descriptor, &descriptor_named_accessor)) {
      const Named_Accessor *accessor = value_as_named_accessor(value);
      if (!maybe_desired_descriptor) {
        return 0;
      }
      Module *module = maybe_desired_descriptor->own_module;
      if (!module) return 0;
      Scope_Entry *entry = scope_lookup_shallow(module->exports.scope, accessor->symbol);
      if (!entry) return 0;
      return maybe_desired_descriptor;
    } else if (
      same_type(value->descriptor, &descriptor_overload) ||
      same_type(value->descriptor, &descriptor_function_literal)
    ) {
      Array_Resolved_Function_Parameter parameters;
      if (maybe_desired_descriptor) {
        assert(maybe_desired_descriptor->tag == Descriptor_Tag_Function_Instance);
        parameters = maybe_desired_descriptor->Function_Instance.info->parameters;
      } else {
        if (value->descriptor == &descriptor_function_literal) {
          const Function_Literal *literal = value_as_function_literal(value);
          if (literal->header.generic_parameter_count > 0) {
            return 0;
          }
          Function_Info info;
          mass_function_info_init_for_header_and_maybe_body(
            context, literal->own_scope, &literal->header, literal->body, &info
          );
          if (mass_has_error(context)) return 0;
          parameters = info.parameters;
        } else {
          return 0;
        }
      }

      Overload_Match match = mass_match_overload(context, value, parameters);
      if (match.tag != Overload_Match_Tag_Found) return 0;
      Function_Call_Setup call_setup =
        context->program->default_calling_convention->call_setup_proc(context->allocator, match.Found.info);
      deduced_descriptor = descriptor_function_instance(
        context->allocator, match.Found.info, call_setup, context->program
      );
    }
  }

  if (maybe_desired_descriptor) {
    if (!deduced_descriptor) return 0;
    if (
      (!deduced_descriptor->brand && maybe_desired_descriptor->brand) ||
      (deduced_descriptor->brand && !maybe_desired_descriptor->brand)
    ) {
      // TODO Figure out why can't just call this version right away.
      //      The only idea I have right now is that some code is doing pointer
      //      compares of the descriptors, and assignment below messes with that.
      if (types_equal(maybe_desired_descriptor, deduced_descriptor, Brand_Comparison_Mode_One_Unbranded)) {
        deduced_descriptor = maybe_desired_descriptor;
      } else {
        return 0;
      }
    } else if (!same_type(maybe_desired_descriptor, deduced_descriptor)) {
      return 0;
    }
  }

  return deduced_descriptor;
}

typedef struct {
  Storage base_storage;
  Function_Builder *builder;
  const Scope *scope;
} Mass_Assign_Tuple_Item_Proc_Payload;

static inline bool
mass_assign_tuple_item_proc(
  Mass_Context *context,
  const Descriptor *item_descriptor,
  u64 offset,
  Value *source,
  const Source_Range *source_range,
  Mass_Assign_Tuple_Item_Proc_Payload *payload
) {
  Storage item_storage = storage_with_offset_and_bit_size(
    &payload->base_storage, u64_to_s32(offset), item_descriptor->bit_size
  );
  Value target_value;
  value_init(&target_value, item_descriptor, item_storage, *source_range);
  mass_assign_helper(context, payload->builder, &target_value, source, payload->scope, source_range);
  return !mass_has_error(context);
}

static void
mass_assign_helper(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Scope *scope,
  const Source_Range *source_range
) {
  if (target->flags & Value_Flags_Constant) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Assignment_To_Constant,
      .source_range = *source_range,
    });
    return;
  }

  if (source->tag == Value_Tag_Lazy) {
    value_force_exact(context, builder, scope, target, source);
    return;
  }
  assert(target->tag != Value_Tag_Lazy);
  const Storage *source_storage = &value_as_forced(source)->storage;
  const Storage *target_storage = &value_as_forced(target)->storage;

  if (mass_descriptor_is_void(target->descriptor)) {
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Raw && !source->descriptor->brand) {
    if (value_is_i64(source)) {
      if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
        const i64 *literal = value_as_i64(source);
        if (literal->bits == 0) {
          Value null_pointer;
          value_init(&null_pointer, target->descriptor, imm64(0), source->source_range);
          mass_assign_helper(context, builder, target, &null_pointer, scope, source_range);
          return;
        } else {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Type_Mismatch,
            .source_range = *source_range,
            .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
            .detailed_message = slice_literal("Trying to assign a non-zero literal number to a pointer"),
          });
          return;
        }
      }
    }
    if (descriptor_is_integer(target->descriptor) || target->descriptor->tag == Descriptor_Tag_Raw) {
      Mass_Cast_Lazy_Payload lazy_payload = {
        .target = target->descriptor,
        .expression = source,
      };
      Expected_Result expected_result = expected_result_any(target->descriptor);
      source = mass_cast_lazy_proc(context, builder, &expected_result, scope, source_range, &lazy_payload);
      if (mass_has_error(context)) return;
      source_storage = &value_as_forced(source)->storage;
    }
  }

  if (value_is_tuple(source)) {
    const Tuple *tuple = value_as_tuple(source);

    Mass_Assign_Tuple_Item_Proc_Payload payload = {
      .base_storage = value_as_forced(target)->storage,
      .builder = builder,
      .scope = scope,
    };
    mass_process_tuple_as_descriptor(
      context, tuple, target->descriptor, source_range,
      (Mass_Report_Tuple_Error_Proc)mass_error,
      (Mass_Process_Tuple_Item_Proc)mass_assign_tuple_item_proc, &payload
    );
    return;
  }

  if (
    target->descriptor->tag == Descriptor_Tag_Function_Instance &&
    source->descriptor->tag != Descriptor_Tag_Function_Instance
  ) {
    const Function_Info *target_info = target->descriptor->Function_Instance.info;
    if (
      source->descriptor == &descriptor_function_literal ||
      source->descriptor == &descriptor_overload
    ) {
      Overload_Match_Found match_found;
      if (!mass_match_overload_or_error(context, source, target_info->parameters, &match_found,&target->source_range)) {
        return;
      }

      source = match_found.value;
      if (source->descriptor->tag != Descriptor_Tag_Function_Instance) {
        Function_Literal *literal = value_as_function_literal(match_found.value);
        source = mass_function_literal_instance_for_info(context, literal, match_found.info);
      }
      if (mass_has_error(context)) return;
      assert(source->descriptor->tag == Descriptor_Tag_Function_Instance);
      source_storage = &value_as_forced(source)->storage;
    }

    if (same_type(target->descriptor, source->descriptor)) {
      if (source_storage->tag == Storage_Tag_Memory) {
        mass_storage_load_address(builder, source_range, scope, target_storage, source_storage);
      } else {
        move_value(builder, scope, source_range, target_storage, source_storage);
      }
    } else {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
      });
      return;
    }
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Fixed_Array) {
    if (!same_type(target->descriptor, source->descriptor)) goto err;
    const Descriptor *item_descriptor = source->descriptor->Fixed_Array.item;

    for (u64 i = 0; i < source->descriptor->Fixed_Array.length; ++i) {
      s32 index_number = (u64_to_s32(i));
      s32 offset = index_number * u64_to_s32(descriptor_byte_size(item_descriptor));
      Bits item_bit_size = item_descriptor->bit_size;

      Value source_field = {
        .tag = Value_Tag_Forced,
        .descriptor = item_descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(source_storage, offset, item_bit_size),
        .source_range = source->source_range,
      };
      Value target_field = {
        .tag = Value_Tag_Forced,
        .descriptor = item_descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(target_storage, offset, item_bit_size),
        .source_range = target->source_range,
      };
      mass_assign_helper(context, builder, &target_field, &source_field, scope, source_range);
      if (mass_has_error(context)) return;
    }
    return;
  }

  if (mass_value_is_static(source)) {
    if (value_is_named_accessor(source)) {
      const Named_Accessor *accessor = value_as_named_accessor(source);
      Module *module = target->descriptor->own_module;
      Value *adjusted_source = mass_module_get_impl(context, module, accessor->symbol, source_range);
      if (mass_has_error(context)) return;
      mass_assign_helper(context, builder, target, adjusted_source, scope, source_range);
      return;
    }
    if (assign_from_static(context, builder, target, source, scope, source_range)) {
      return;
    }
  }

  if (source->descriptor->tag == Descriptor_Tag_Struct) {
    if (!types_equal(target->descriptor, source->descriptor, Brand_Comparison_Mode_One_Unbranded)) goto err;

    DYN_ARRAY_FOREACH(Struct_Field, field, source->descriptor->Struct.fields) {
      Value source_field = {
        .tag = Value_Tag_Forced,
        .descriptor = field->descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(
          source_storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = source->source_range,
      };
      Value target_field = {
        .tag = Value_Tag_Forced,
        .descriptor = field->descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(
          target_storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign_helper(context, builder, &target_field, &source_field, scope, source_range);
      if (mass_has_error(context)) return;
    }
    return;
  }

  if (mass_has_error(context)) return;
  if (same_type(target->descriptor, source->descriptor)) {
    if (mass_value_is_static(target)) {
      assert(mass_value_is_static(source));
      assert(same_type(target->descriptor, source->descriptor));
      void *source_memory = (void *)storage_static_memory_with_bit_size(
        &value_as_forced(source)->storage, source->descriptor->bit_size
      );
      void *target_memory = (void *)storage_static_memory_with_bit_size(
        &value_as_forced(target)->storage, target->descriptor->bit_size
      );
      memcpy(target_memory, source_memory, descriptor_byte_size(target->descriptor));
    } else {
      move_value(builder, scope, source_range, target_storage, source_storage);
    }
    return;
  }

  err:
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Type_Mismatch,
    .source_range = *source_range,
    .Type_Mismatch = {
      .expected = target->descriptor,
      .actual = source->descriptor,
    },
  });
}

static inline Value *
value_force_lazy_static(
  Value *value,
  Slice name
) {
  Lazy_Static_Value *lazy = value_as_lazy_static_value(value);
  Mass_Context *context = &lazy->context;
  if (lazy->resolving) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Circular_Dependency,
      .source_range = value->source_range,
      .Circular_Dependency = { .name = name },
    });
    return 0;
  }
  lazy->resolving = true;
  Value *result = token_parse_expression(context, &lazy->parser, lazy->expression, &(u32){0}, 0);
  lazy->resolving = false;
  if (mass_has_error(context)) return 0;
  if (result) {
    *value = *result;
    bool is_function = (
      value->descriptor->tag == Descriptor_Tag_Function_Instance &&
      storage_is_label(&value_as_forced(value)->storage)
    );
    if (!is_function && !mass_value_ensure_static(context, value)) return 0;
  } else {
    memset(value, 0, sizeof(*value));
  }
  return value;
}

static void
scope_maybe_force_overload(
  Mass_Context *context,
  Value *value,
  Slice name
) {
  if (value_is_overload(value)) {
    const Overload *overload = value_as_overload(value);
    scope_maybe_force_overload(context, overload->value, name);
    scope_maybe_force_overload(context, overload->next, name);
    return;
  }
  if (mass_has_error(context)) return;
  if (value_is_lazy_static_value(value)) {
    value_force_lazy_static(value, name);
  }
  if (mass_has_error(context)) return;
  if (
    value->descriptor->tag != Descriptor_Tag_Function_Instance &&
    value->descriptor != &descriptor_function_literal &&
    value->descriptor != &descriptor_overload
  ) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Non_Function_Overload,
      .source_range = value->source_range,
    });
    return;
  }
}

static Value *
scope_entry_force_value(
  Mass_Context *context,
  Scope_Entry *entry
) {
  // Because of overloads we might end up updating the value that is stored
  // in a scope entry. I'm not in love with this design, but it is still possible
  // to cache "forcing" step by remembering which exact value was forced.
  if (entry->latest_forced_value == entry->value) {
    return entry->value;
  }

  if (entry->value->descriptor == &descriptor_lazy_static_value) {
    entry->value = value_force_lazy_static(entry->value, entry->name);
  }

  entry->latest_forced_value = entry->value;

  if (!entry->value) return 0;

  if (value_is_overload(entry->value)) {
    scope_maybe_force_overload(context, entry->value, entry->name);
  }

  return entry->value;
}

static inline Value *
mass_context_force_lookup(
  Mass_Context *context,
  const Parser *parser,
  const Scope *scope,
  const Symbol *symbol,
  const Source_Range *lookup_range
) {
  Scope_Entry *entry = scope_lookup(scope, symbol);

  if (!entry) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Undefined_Variable,
      .Undefined_Variable = { .name = symbol->name },
      .source_range = *lookup_range,
    });
    return 0;
  }
  if (!(parser->flags & Parser_Flags_Type_Only)) {
    if (entry->epoch.as_u64 != VALUE_STATIC_EPOCH.as_u64 && entry->epoch.as_u64 != parser->epoch.as_u64) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Epoch_Mismatch,
        .source_range = *lookup_range,
      });
      return 0;
    }
  }
  return scope_entry_force_value(context, entry);
}

static inline Scope *
scope_find_nearest_declarative(
  Scope *scope
) {
  // TODO avoid const cast here
  for (; scope && scope->tag != Scope_Tag_Declarative; scope = (Scope *) scope->parent);
  assert(scope);
  return scope;
}

// TODO rename to `scope_define_declarative`
static inline void
scope_define_value(
  Scope *scope,
  Epoch epoch,
  Source_Range source_range,
  const Symbol *symbol,
  Value *value
) {
  scope = scope_find_nearest_declarative(scope);
  if (!scope->Declarative.map) {
    scope->Declarative.map = hash_map_make(Scope_Map, scope->allocator);
  }
  Scope_Entry *it = scope_lookup_shallow(scope, symbol);
  if (it) {
    Overload *overload = allocator_allocate(scope->allocator, Overload);
    *overload = (Overload) { .value = value, .next = it->value };
    it->value = value_init(
      allocator_allocate(scope->allocator, Value),
      &descriptor_overload, storage_static(overload), source_range
    );
  } else {
    Scope_Entry *allocated = allocator_allocate(scope->allocator, Scope_Entry);
    *allocated = (Scope_Entry) {
      .value = value,
      .name = symbol->name,
      .epoch = epoch,
      .source_range = source_range,
    };
    hash_map_set(scope->Declarative.map, symbol, allocated);
  }
}

static inline void
scope_define_operator(
  Mass_Context *context,
  Scope *scope,
  Source_Range source_range,
  const Symbol *base_symbol,
  Operator *operator
) {
  Operator_Symbol_Map *map = operator->fixity == Operator_Fixity_Prefix
    ? context->compilation->prefix_operator_symbol_map
    : context->compilation->infix_or_suffix_operator_symbol_map;
  const Symbol *operator_symbol;
  {
    const Symbol **maybe_operator_symbol = hash_map_get(map, base_symbol);
    if (maybe_operator_symbol) {
      operator_symbol = *maybe_operator_symbol;
    } else {
      Symbol *new_symbol = mass_allocate(context, Symbol);
      *new_symbol = *base_symbol;
      hash_map_set(map, base_symbol, new_symbol);
      operator_symbol = new_symbol;
    }
  }

  if (scope_lookup_shallow(scope, operator_symbol)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Operator_Fixity_Conflict,
      .source_range = source_range,
      .Operator_Fixity_Conflict = {
        .fixity = operator->fixity,
        .symbol = operator_symbol->name,
      },
    });
    return;
  }

  Value *operator_value = value_make(context, &descriptor_operator, storage_static(operator), source_range);
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, operator_symbol, operator_value);
}

static inline const Operator *
scope_lookup_operator(
  Mass_Context *context,
  const Scope *scope,
  const Symbol *base_symbol,
  Operator_Fixity fixity
) {
  Operator_Symbol_Map *map = fixity == Operator_Fixity_Prefix
    ? context->compilation->prefix_operator_symbol_map
    : context->compilation->infix_or_suffix_operator_symbol_map;

  const Symbol **operator_symbol_pointer = hash_map_get(map, base_symbol);
  if (!operator_symbol_pointer) return 0;
  const Symbol *operator_symbol = *operator_symbol_pointer;
  Scope_Entry *maybe_operator_entry = scope_lookup(scope, operator_symbol);
  if (!maybe_operator_entry) return 0;
  if (maybe_operator_entry->value->descriptor != &descriptor_operator) return 0;
  return value_as_operator(maybe_operator_entry->value);
}

static inline const Descriptor *
value_ensure_type(
  Mass_Context *context,
  const Scope *scope,
  Value *value,
  Source_Range source_range
) {
  if (!value) return 0;
  if (value_is_tuple(value)) {
    const Tuple *tuple = value_as_tuple(value);
    return anonymous_struct_descriptor_from_tuple(context, tuple, Tuple_Eval_Mode_Type);
  }
  if (value_is_function_header(value)) {
    const Function_Header *header = value_as_function_header(value);
    Function_Info *info = mass_allocate(context, Function_Info);
    mass_function_info_init_for_header_and_maybe_body(context, scope, header, 0, info);
    if (mass_has_error(context)) return 0;
    const Calling_Convention *calling_convention = context->program->default_calling_convention;
    Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, info);
    return descriptor_function_instance(context->allocator, info, call_setup, context->program);
  }
  if (!mass_value_ensure_static(context, value)) {
    return 0;
  }
  if (value_is_type(value)) {
    return value_as_type(value)->descriptor;
  }
  // TODO this should not be necessary but is helpful during transition to strong Type alias
  if (value_is_descriptor_pointer(value)) {
    return *value_as_descriptor_pointer(value);
  }

  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Type_Mismatch,
    .source_range = source_range,
    .Type_Mismatch = { .expected = &descriptor_type, .actual = value->descriptor },
  });
  return 0;
}

static inline Value_View
value_view_match_till_symbol(
  Value_View view,
  u32 *peek_index,
  const Symbol *symbol
) {
  u32 start_index = *peek_index;
  for (; *peek_index < view.length; *peek_index += 1) {
    Value *token = value_view_get(&view, *peek_index);
    if (value_is_symbol(token) && value_as_symbol(token) == symbol) {
      *peek_index += 1;
      return value_view_slice(&view, start_index, *peek_index - 1);
    }
  }
  return value_view_slice(&view, start_index, *peek_index);
}

static inline Value *
value_view_maybe_match_cached_symbol(
  Value_View view,
  u32 *peek_index,
  const Symbol *cached_symbol
) {
  Value *value = value_view_peek(&view, *peek_index);
  if (!value) return 0;
  if (value->descriptor != &descriptor_symbol) return 0;
  if (value_as_symbol(value) != cached_symbol) return 0;
  *peek_index += 1;
  return value;
}

static inline void
context_parse_error(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 peek_index
) {
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = value_view_slice(&view, peek_index, peek_index).source_range,
  });
}

static inline const Descriptor *
token_match_type(
  Mass_Context *context,
  Parser *parser,
  Value_View view
);

static Value *
token_parse_single(
  Mass_Context *context,
  Parser *parser,
  Value *value
);

static Array_Value_Ptr
mass_parse_maybe_list_into_value_array(
  Mass_Context *context,
  Parser *parser,
  Value *list
) {
  Array_Value_Ptr result;
  if (value_is_list_node(list)) {
    const List_Node *node = value_as_list_node(list);
    u64 length = 0;
    for (const List_Node *it = node; it; it = it->maybe_previous) {
      length++;
    }
    result = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator, .capacity = length);
    result.data->length = length;
    u64 index = 0;
    for (const List_Node *it = node; it; it = it->maybe_previous) {
      index++;
      *dyn_array_get(result, length - index) = token_parse_single(context, parser, it->value);
      if (mass_has_error(context)) goto err;
    }
  } else {
    result = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator, .capacity = 1);
    dyn_array_push(result, list);
  }
  err:
  return result;
}

static Value *
token_parse_tuple(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  Value *result_value = 0;

  Array_Value_Ptr items;
  if (view.length) {
    Value *list = token_parse_expression(context, parser, view, &(u32){0}, 0);
    if (mass_has_error(context)) return 0;
    items = mass_parse_maybe_list_into_value_array(context, parser, list);
    if (mass_has_error(context)) return 0;
  } else {
    items = dyn_array_static_empty(Array_Value_Ptr);
  }

  allocator_allocate_bulk(context->allocator, combined, {
    Tuple tuple;
    Value value;
  });
  Tuple *tuple = &combined->tuple;
  *tuple = (Tuple) {
    .epoch = parser->epoch,
    .items = items,
    .scope_where_it_was_created = parser->scope,
  };
  Storage result_storage = storage_static_heap(tuple, descriptor_tuple.bit_size);
  result_value = value_init(&combined->value, &descriptor_tuple, result_storage, view.source_range);

  return result_value;
}

static Value *
token_parse_single(
  Mass_Context *context,
  Parser *parser,
  Value *value
) {
  if (value->descriptor == &descriptor_group_paren) {
    Value_View children = value_as_group_paren(value)->children;
    // Support resolving parenthesized operator (+) as a static value
    if (children.length == 1) {
      Value *only_child = value_view_get(&children, 0);
      if (value_is_symbol(only_child)) {
        const Symbol *symbol = value_as_symbol(only_child);
        const Operator *maybe_operator = scope_lookup_operator(
          context, parser->scope, symbol, Operator_Fixity_Prefix
        );
        if (!maybe_operator) {
          maybe_operator = scope_lookup_operator(
            context, parser->scope, symbol, Operator_Fixity_Infix | Operator_Fixity_Postfix
          );
        }
        if (maybe_operator) {
          switch(maybe_operator->tag) {
            case Operator_Tag_Alias: {
              return mass_context_force_lookup(
                context, parser, parser->scope, maybe_operator->Alias.symbol, &children.source_range
              );
            } break;
            case Operator_Tag_Intrinsic: {
              mass_error(context, (Mass_Error) {
                .tag = Mass_Error_Tag_Parse,
                .source_range = children.source_range,
                .detailed_message = slice_literal("Intrinsic operators can not be used as values"),
              });
              return 0;
            } break;
          }
        }
      }
    } else if (children.length == 0) {
      return mass_make_void(context, value->source_range);
    }
    return token_parse_expression(context, parser, children, &(u32){0}, 0);
  } else if (value->descriptor == &descriptor_ast_block) {
    return token_parse_block(context, parser, value_as_ast_block(value), &value->source_range);
  } else if (value->descriptor == &descriptor_group_square) {
    return token_parse_tuple(context, parser, value_as_group_square(value)->children);
  } else if (value->descriptor == &descriptor_quoted) {
    const Quoted *quoted = value_as_quoted(value);
    return quoted->value;
  } else if (value_is_symbol(value)) {
    return mass_context_force_lookup(context, parser, parser->scope, value_as_symbol(value), &value->source_range);
  } else {
    return value;
  }
}

static bool
mass_is_slice_like(
  Value *value
) {
  // We accept anything that has the same shape as the slice (so ignore brands)
  return types_equal(value->descriptor, &descriptor_slice, Brand_Comparison_Mode_Ignore);
}

static Slice
mass_slice_from_slice_like(
  Value *value
) {
  if (!mass_is_slice_like(value)) panic("Expected a slice-like value");
  Value slice_value = *value;
  slice_value.descriptor = &descriptor_slice;
  return *value_as_slice(&slice_value);
}

static Value *
mass_named_accessor(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *symbol_value = value_view_get(&args, 0);
  const Symbol *symbol = 0;
  if (same_type(symbol_value->descriptor, &descriptor_symbol)) {
    if (!mass_value_ensure_static_of(context, symbol_value, &descriptor_symbol)) {
      return 0;
    }
    symbol = value_as_symbol(symbol_value);
  } else if (mass_is_slice_like(symbol_value)) {
    Slice name = mass_slice_from_slice_like(symbol_value);
    symbol = mass_ensure_symbol(context->compilation, name);
  }

  Named_Accessor named_accessor = {.symbol = symbol };
  Value *result = value_init(
    mass_allocate(context, Value),
    &descriptor_named_accessor, storage_immediate(&named_accessor), args.source_range
  );
  return result;
}

static Value *
mass_spread(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);

  Spread spread = {.value = value };
  Value *result = value_init(
    mass_allocate(context, Value),
    &descriptor_spread, storage_immediate(&spread), args.source_range
  );
  return result;
}


static inline const Descriptor *
token_match_type(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  Value *type_value = token_parse_expression(context, parser, view, &(u32){0}, 0);
  return value_ensure_type(context, parser->scope, type_value, view.source_range);
}

static fn_type_opaque
mass_ensure_jit_function_for_value(
  Mass_Context *context,
  Value *value,
  const Function_Info *fn_info
) {
  Jit *jit = &context->compilation->jit;

  Mass_Context jit_context = *context;
  jit_context.program = jit->program;

  Value *instance = value;
  if (instance->descriptor->tag == Descriptor_Tag_Function_Instance) {
    #ifndef NDEBUG
    const Function_Call_Setup *call_setup = &instance->descriptor->Function_Instance.call_setup;
    Descriptor fake_descriptor = {
      .tag = Descriptor_Tag_Function_Instance,
      .bit_size = {sizeof(void *) * CHAR_BIT},
      .bit_alignment = sizeof(void *) * CHAR_BIT,
      .Function_Instance = { .info = fn_info, .call_setup = *call_setup, .program = context->program, },
    };
    (void)fake_descriptor; // avoid warnings in release build
    assert(same_type(&fake_descriptor, instance->descriptor));
    #endif
  } else {
    Function_Literal *literal = value_as_function_literal(value);
    instance = mass_function_literal_instance_for_info(&jit_context, literal, fn_info);
  }
  if (mass_has_error(context)) return 0;
  const Storage *storage = &value_as_forced(instance)->storage;

  if (storage_is_label(storage)) {
    Label *label = storage->Memory.location.Instruction_Pointer_Relative.label;
    assert(label->program == jit->program);
    if (!label->resolved) {
      program_jit(context, jit);
      if (mass_has_error(context)) return 0;
    }
    if (!label->resolved) {
      // Not sure if there are other reasons for the label to not be resolved but that 100%
      // happens when there is a recursive call to intrinsics
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Recursive_Intrinsic_Use,
        .source_range = value->source_range,
      });
      return 0;
    }
    return (fn_type_opaque)rip_value_pointer_from_storage(storage);
  } else {
    void const * const *address_memory = storage_static_memory_with_bit_size(storage, (Bits){64});
    return (fn_type_opaque)*address_memory;
  }
}

static Function_Parameter
mass_parse_single_function_parameter(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  Function_Parameter arg = {0};
  if (!view.length) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = view.source_range,
      .detailed_message = slice_literal("Expected a parameter definition"),
    });
    goto err;
  }

  u32 peek_index = 0;

  Value *at_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator_at
  );

  Value *name_token = value_view_next(&view, &peek_index);
  if (!mass_value_ensure_static_of(context, name_token, &descriptor_symbol)) goto err;
  const Symbol *symbol = value_as_symbol(name_token);
  // foo(x) or foo(@x)
  if (peek_index == view.length) {
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Generic,
      .symbol = symbol,
      .descriptor = 0,
      .source_range = view.source_range,
      .Generic = { .is_static = !!at_token },
    };
  }

  Value *operator_token = value_view_next(&view, &peek_index);
  if (!mass_value_ensure_static_of(context, operator_token, &descriptor_symbol)) goto err;
  if (peek_index == view.length) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = view.source_range,
      .detailed_message = slice_literal("Expected an expression"),
    });
    goto err;
  }

  const Symbol *operator_symbol = value_as_symbol(operator_token);
  // foo(@x :: 42) or foo(x :: 42) both have the same semantics.
  // TODO Maybe one form should be disallowed
  if (operator_symbol == context->compilation->common_symbols.operator_double_colon) {
    Value_View static_expression = value_view_rest(&view, peek_index);
    Value *static_value = token_parse_expression(context, parser, static_expression, &(u32){0}, 0);
    if (mass_has_error(context)) goto err;
    if (!mass_value_ensure_static(context, static_value)) goto err;
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Exact_Static,
      .Exact_Static = { .storage = value_as_forced(static_value)->storage },
      .symbol = value_as_symbol(name_token),
      .descriptor = static_value->descriptor,
      .source_range = name_token->source_range,
    };
  }

  // foo(@x ~ some_constraint) or foo(x ~ some_constraint)
  if (operator_symbol == context->compilation->common_symbols.operator_tilde) {
    Value_View constraint_expression = value_view_rest(&view, peek_index);
    Value *constraint = token_parse_expression(context, parser, constraint_expression, &(u32){0}, 0);
    if (mass_has_error(context)) goto err;
    if (!mass_value_ensure_static(context, constraint)) goto err;
    const Descriptor *constraint_descriptor =
      deduce_runtime_descriptor_for_value(context, constraint, &descriptor_mass_type_constraint_proc);
    if (mass_has_error(context)) goto err;
    if (!constraint_descriptor) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .Type_Mismatch = {
          .expected = &descriptor_mass_type_constraint_proc,
          .actual = constraint->descriptor,
        },
        .source_range = constraint_expression.source_range,
        .detailed_message = slice_literal("Invalid function signature for a type constraint"),
      });
      goto err;
    }
    const Function_Info *constraint_info = descriptor_as_function_instance(constraint_descriptor)->info;
    Mass_Type_Constraint_Proc maybe_type_constraint =
      (Mass_Type_Constraint_Proc)mass_ensure_jit_function_for_value(context, constraint, constraint_info);
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Generic,
      .symbol = symbol,
      .descriptor = 0,
      .source_range = view.source_range,
      .Generic = { .is_static = !!at_token, .maybe_type_constraint = maybe_type_constraint },
    };
  }

  // foo(@x := 42) or foo(x := 42)
  if (operator_symbol == context->compilation->common_symbols.operator_colon_equal) {
    if (at_token) {
      panic("TODO test and maybe fix static values with a default value");
    }

    Value_View default_expression = value_view_rest(&view, peek_index);
    Value *default_value = token_parse_expression(context, parser, default_expression, &(u32){0}, 0);
    if (mass_has_error(context)) goto err;
    if (!mass_value_ensure_static(context, default_value)) goto err;
    const Descriptor *descriptor = deduce_runtime_descriptor_for_value(context, default_value, 0);
    if (mass_has_error(context)) goto err;
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Runtime,
      .symbol = symbol,
      .descriptor = descriptor,
      .source_range = name_token->source_range,
      .maybe_default_value = default_value,
    };
  }

  if (operator_symbol != context->compilation->common_symbols.operator_colon) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
      .detailed_message = slice_literal("Expected a `:` in parameter definition"),
    });
    goto err;
  }

  const Symbol *equal = context->compilation->common_symbols.operator_equal;
  Value_View type_expression = value_view_match_till_symbol(view, &peek_index, equal);

  if (peek_index == view.length) {
    if (at_token) { // foo(@x : i64)
      return (Function_Parameter) {
        .tag = Function_Parameter_Tag_Generic,
        .symbol = symbol,
        .descriptor = 0,
        .source_range = view.source_range,
        .maybe_type_expression = type_expression,
        .Generic = { .is_static = !!at_token },
      };
    } else { // foo(x : i64)
      return (Function_Parameter) {
        .tag = Function_Parameter_Tag_Runtime,
        .symbol = symbol,
        .descriptor = 0,
        .source_range = name_token->source_range,
        .maybe_type_expression = type_expression,
      };
    }
  }

  Value_View default_expression = value_view_rest(&view, peek_index);
  if (!default_expression.length) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = default_expression.source_range,
      .detailed_message = slice_literal("Expected an expression after `=`"),
    });
    goto err;
  }

  // foo(@x : i64 = 42) or foo(x : i64 = 42)
  if (at_token) {
    panic("TODO test and maybe fix static values with a type and a default value");
  }
  Value *maybe_default_value = token_parse_expression(context, parser, default_expression, &(u32){0}, 0);
  if (mass_has_error(context)) goto err;
  if (!mass_value_ensure_static(context, maybe_default_value)) goto err;

  arg = (Function_Parameter) {
    .tag = Function_Parameter_Tag_Runtime,
    .maybe_default_value = maybe_default_value,
    .maybe_type_expression = type_expression,
    .symbol = value_as_symbol(name_token),
    .descriptor = 0,
    .source_range = name_token->source_range,
  };

  err:
  return arg;
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

static Value *
mass_expected_result_ensure_value_or_temp(
  Mass_Context *context,
  Function_Builder *builder,
  const Scope *scope,
  const Expected_Result *expected_result,
  Value *value
) {
  if (mass_has_error(context)) return 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Value *result_value = value_make(
        context, expected_result->Exact.descriptor, expected_result->Exact.storage, value->source_range
      );
      mass_assign_helper(context, builder, result_value, value, scope, &value->source_range);
      if (mass_has_error(context)) return 0;
      const Storage *actual_storage = &value_as_forced(value)->storage;
      // @Hack there should be a better and more robust way to do this
      if (
        !mass_value_is_static(value) &&
        !storage_occupies_same_memory(&expected_result->Exact.storage, actual_storage)
      ) {
        storage_release_if_temporary(builder, actual_storage);
      }
      return result_value;
    }
    case Expected_Result_Tag_Flexible: {
      if (!value) return 0;
      const Expected_Result_Flexible *flexible = &expected_result->Flexible;
      const Descriptor *expected_descriptor =
        flexible->descriptor ? flexible->descriptor : value->descriptor;
      if (!same_type(expected_descriptor, value->descriptor)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = value->source_range,
          .Type_Mismatch = { .expected = expected_descriptor, .actual = value->descriptor },
        });
        return 0;
      }
      return value;
    }
    default: {
      panic("Unknown Expected_Result tag");
      return 0;
    }
  }
}

static Value *
value_force(
  Mass_Context *context,
  Function_Builder *builder,
  const Scope *scope,
  const Expected_Result *expected_result,
  Value *value
) {
  if (!value) return 0;

  if (value->tag == Value_Tag_Lazy) {
    const Value_Lazy *lazy = value_as_lazy(value);

    if (lazy->epoch.as_u64 != VALUE_STATIC_EPOCH.as_u64 && lazy->epoch.as_u64 != builder->epoch.as_u64) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Epoch_Mismatch,
        .source_range = value->source_range,
      });
      return 0;
    }
    Value *result = lazy->proc(context, builder, expected_result, lazy->scope, &value->source_range, lazy->payload);
    if (mass_has_error(context)) return 0;
    if (!lazy->is_factory) {
      *value = *result; // cache the result
      result = value;
    }
    return mass_expected_result_ensure_value_or_temp(context, builder, lazy->scope, expected_result, result);
  }

  return mass_expected_result_ensure_value_or_temp(context, builder, scope, expected_result, value);
}

static Value *
mass_implicit_function_parameter_factory_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Function_Call_Parameter *param
) {
  Register temp_register = register_acquire_temp(builder);
  Storage register_storage = storage_register(temp_register, (Bits){64});
  Storage param_storage = param->storage;
  { // :ParamStackAreaAdjust
    assert(storage_is_stack(&param_storage));
    param_storage.Memory.location.Stack.area = Stack_Area_Received_Argument;
  }
  move_value(builder, scope, source_range, &register_storage, &param_storage);
  Storage indirect_storage = storage_indirect(param->descriptor->bit_size, temp_register);
  indirect_storage.flags |= Storage_Flags_Temporary;
  Value *result = value_make(context, param->descriptor, indirect_storage, *source_range);
  return expected_result_validate(expected_result, result);
}

static inline void
value_force_exact(
  Mass_Context *context,
  Function_Builder *builder,
  const Scope *scope,
  Value *target,
  Value *source
) {
  if (target->flags & Value_Flags_Constant) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Assignment_To_Constant,
      .source_range = target->source_range,
    });
    return;
  }
  const Storage *target_storage = &value_as_forced(target)->storage;
  Expected_Result expected_result = mass_expected_result_exact(target->descriptor, *target_storage);
  Value *forced = value_force(context, builder, scope, &expected_result, source);
  if (!forced) return;
  assert(forced->descriptor == target->descriptor);
  assert(storage_equal(&value_as_forced(forced)->storage, target_storage));
}

static inline Value *
mass_forward_call_to_alias(
  Mass_Context *context,
  Parser *parser,
  Value_View args,
  const Symbol *alias
) {
  Source_Range source_range = args.source_range;

  // TODO can we cache this somehow?
  Scope_Entry *entry = scope_lookup(parser->scope, alias);
  if (!entry) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Undefined_Variable,
      .source_range = args.source_range,
      .Undefined_Variable = {.name = alias->name},
      .detailed_message = slice_literal("Could not find target when forwarding a call"),
    });
    return 0;
  }

  Value *target = scope_entry_force_value(context, entry);
  if (mass_has_error(context)) return 0;
  return token_handle_function_call(context, parser, target, args, source_range);
}

static inline Value *
mass_make_lazy_value(
  Mass_Context *context,
  Parser *parser,
  Source_Range source_range,
  const void *payload,
  const Descriptor *descriptor,
  Lazy_Value_Proc proc
) {
  Value *lazy = mass_allocate(context, Value);
  *lazy = (Value) {
    .tag = Value_Tag_Lazy,
    .descriptor = descriptor,
    .source_range = source_range,
    .Lazy = {
      .epoch = parser->epoch,
      .proc = proc,
      .scope = parser->scope,
      .payload = payload,
    },
  };

  return lazy;
}

static inline void
scope_define_lazy_compile_time_expression(
  Mass_Context *context,
  Parser *parser,
  Scope *scope,
  const Symbol *symbol,
  Value_View view
) {
  allocator_allocate_bulk(context->allocator, combined, {
    Lazy_Static_Value lazy_static_value;
    Value value;
  });
  Lazy_Static_Value *lazy_static_value = &combined->lazy_static_value;
  *lazy_static_value = (Lazy_Static_Value){
    .context = *context,
    .parser = *parser,
    .expression = view,
  };
  Value *value = value_init(
    &combined->value,
    &descriptor_lazy_static_value,
    storage_static(lazy_static_value),
    view.source_range
  );

  scope_define_value(scope, VALUE_STATIC_EPOCH, view.source_range, symbol, value);
}

static Value *
mass_exports(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *tuple_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (mass_has_error(context)) return 0;
  if (!mass_value_ensure_static_of(context, tuple_value, &descriptor_tuple)) {
    return 0;
  }
  const Tuple *tuple = value_as_tuple(tuple_value);

  Module_Exports *export = mass_allocate(context, Module_Exports);
  *export = (Module_Exports) {
    .tag = Module_Exports_Tag_Selective,
    .Selective = { .tuple = tuple },
    .source_range = args.source_range,
  };

  Value *result = value_make(
    context, &descriptor_module_exports, storage_static(export), args.source_range
  );
  return result;
}

static Value *
mass_import(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  if (args.length != 1) goto parse_err;
  Value *file_path_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (!types_equal(&descriptor_slice, file_path_value->descriptor, Brand_Comparison_Mode_One_Unbranded)) {
    goto parse_err;
  }
  Slice file_path = mass_slice_from_slice_like(file_path_value);

  Module *module;
  if (slice_equal(file_path, slice_literal("mass"))) {
    module = &context->compilation->compiler_module;
  } else {
    file_path = slice_normalize_path(context->allocator, file_path);
    Module **module_pointer = hash_map_get(context->compilation->module_map, file_path);
    if (module_pointer) {
      module = *module_pointer;
    } else {
      const Scope *root_scope = context->compilation->root_scope;
      Scope *module_scope = scope_make_declarative(context->allocator, root_scope);
      module = program_module_from_file(context, file_path, module_scope);
      program_import_module(context, module);
      if (mass_has_error(context)) return 0;
      hash_map_set(context->compilation->module_map, file_path, module);
    }
  }

  return value_make(context, &descriptor_module, storage_static(module), args.source_range);

  parse_err:
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = args.source_range,
    .detailed_message ="import() expects a single string argument"
  });
  return 0;
}

// TODO move this to user land (again)
static Value *
mass_handle_while_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_While *payload
) {
  Program *program = context->program;
  Label *continue_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("continue"));

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .scope = scope,
    .Label.pointer = continue_label,
  });

  Expected_Result expected_condition = expected_result_any(&descriptor__bool);
  Value *condition = value_force(context, builder, scope, &expected_condition, payload->condition);
  if (mass_has_error(context)) return 0;

  Label *break_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("break"));

  encode_inverted_conditional_jump(builder, break_label, scope, &condition->source_range, condition);
  storage_release_if_temporary(builder, &value_as_forced(condition)->storage);

  Value *void_value = mass_make_void(context, *source_range);
  value_force_exact(context, builder, scope, void_value, payload->body);
  if (mass_has_error(context)) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, payload->body->source_range, scope,
    &(Instruction_Assembly){x64_jmp, {code_label32(continue_label)}}
  );

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .scope = scope,
    .Label.pointer = break_label,
  });

  return void_value;
}

static Value *
token_parse_while(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *matched_length,
  const Symbol *end_symbol
) {
  u32 peek_index = 0;
  Value *keyword_token = value_view_next(&view, &peek_index);
  assert(value_as_symbol(keyword_token) == context->compilation->common_symbols._while);

  u32 condition_start_index = peek_index;
  for (; peek_index < view.length; peek_index += 1) {
    Value *token = value_view_get(&view, peek_index);
    if (value_is_ast_block(token)) {
      break;
    }
  }
  Value_View condition_view = value_view_slice(&view, condition_start_index, peek_index);
  if (peek_index == view.length) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
    });
    return 0;
  }

  Value *body_token = value_view_next(&view, &peek_index);
  assert(value_is_ast_block(body_token));
  if (view.length != peek_index) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
    });
    return 0;
  }

  Value *condition = token_parse_expression(context, parser, condition_view, &(u32){0}, 0);
  if (mass_has_error(context)) return 0;
  Value *body = token_parse_single(context, parser, body_token);
  if (mass_has_error(context)) return 0;

  *matched_length = peek_index;
  Mass_While *lazy_payload = mass_allocate(context, Mass_While);
  *lazy_payload = (Mass_While) { .condition = condition, .body = body };

  return value_make(context, &descriptor_mass_while, storage_static(lazy_payload), keyword_token->source_range);
}

static Value *
mass_c_struct(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);

  Value *tuple_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (mass_has_error(context)) return 0;
  if (!mass_value_ensure_static_of(context, tuple_value, &descriptor_tuple)) return 0;
  const Tuple *tuple = value_as_tuple(tuple_value);

  Descriptor *descriptor = anonymous_struct_descriptor_from_tuple(
    context, tuple, Tuple_Eval_Mode_Type
  );
  if (mass_has_error(context)) return 0;
  assert(descriptor->tag == Descriptor_Tag_Struct);
  descriptor->brand = mass_allocate(context, Symbol);

  Value *result = mass_allocate(context, Value);
  *result = MASS_TYPE_VALUE(descriptor);

  return result;
}

// TODO this can probably now be implemented in terms of a mass_trampoline_call
//      with a fake fn that has an inferred return type. But it might be too slow.
static Value *
compile_time_eval(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  const Source_Range *source_range = &view.source_range;

  Jit *jit = &context->compilation->jit;
  Mass_Context eval_context = *context;
  eval_context.program = jit->program;

  Parser eval_parser = {
    .flags = Parser_Flags_None,
    .epoch = get_new_epoch(),
    .scope = scope_make_declarative(context->allocator, parser->scope),
    .module = parser->module,
  };
  Value *expression_result_value = token_parse_expression(&eval_context, &eval_parser, view, &(u32){0}, 0);
  if(mass_has_error(&eval_context)) {
    context->result = eval_context.result;
    return 0;
  }
  const Descriptor *result_descriptor = expression_result_value->descriptor;

  static Slice eval_name = slice_literal_fields("$compile_time_eval$");
  Function_Info fn_info;
  function_info_init(&fn_info, result_descriptor);

  const Calling_Convention *calling_convention = jit->program->default_calling_convention;
  Section *section = &jit->program->memory.code;
  Label *eval_label = make_label(
    context->allocator, jit->program, section, slice_literal("compile_time_eval")
  );
  Function_Builder eval_builder = {
    .epoch = eval_parser.epoch,
    .function = &fn_info,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .code_block = {
      .allocator = context->allocator,
      .start_label = eval_label,
      .end_label = make_label(
        context->allocator, jit->program, section, slice_literal("compile_time_eval_end")
      ),
    },
    .source = source_from_source_range(context->compilation, source_range),
  };

  Expected_Result expected_result = expected_result_any(result_descriptor);
  Value *forced_value = value_force(
    &eval_context, &eval_builder, parser->scope, &expected_result, expression_result_value
  );
  if (mass_has_error(context)) return 0;

  // If we didn't generate any instructions there is no point
  // actually running the code, we can just take the resulting value
  if (!eval_builder.code_block.first_bucket) {
    return forced_value;
  }

  void *result = mass_allocate_bytes_from_descriptor(context, result_descriptor);

  // Load the address of the result
  Register out_register = register_acquire_temp(&eval_builder);
  Value out_value_register;
  value_init(&out_value_register, &descriptor_i64, storage_register(out_register, (Bits){64}), *source_range);
  Value result_address;
  value_init(&result_address, &descriptor_i64, imm64((u64)result), *source_range);

  // Use memory-indirect addressing to copy
  Storage out_storage = storage_indirect(result_descriptor->bit_size, out_register);
  Value *out_value = value_make(&eval_context, result_descriptor, out_storage, *source_range);

  mass_assign_helper(&eval_context, &eval_builder, &out_value_register, &result_address, parser->scope, source_range);
  if (mass_has_error(context)) return 0;

  mass_assign_helper(&eval_context, &eval_builder, out_value, forced_value, parser->scope, source_range);
  if (mass_has_error(context)) return 0;

  calling_convention_x86_64_common_end_proc(jit->program, &eval_builder);
  dyn_array_push(jit->program->functions, eval_builder);

  program_jit(context, jit);
  if (mass_has_error(context)) return 0;

  fn_type_opaque jitted_code = (fn_type_opaque)rip_value_pointer_from_label(eval_label);
  jitted_code();

  Storage result_storage = storage_immediate_with_bit_size(result, result_descriptor->bit_size);
  return value_make(context, out_value->descriptor, result_storage, *source_range);
}

typedef struct {
  Source_Range source_range;
  const Operator *operator;
} Operator_Stack_Entry;
typedef dyn_array_type(Operator_Stack_Entry) Array_Operator_Stack_Entry;

static i64 mass_i64_add(i64 a, i64 b) { return (i64){a.bits + b.bits}; }
static i64 mass_i64_subtract(i64 a, i64 b) { return (i64){a.bits - b.bits}; }
static i64 mass_i64_signed_multiply(i64 a, i64 b) { return (i64){(u64)((s64)a.bits * (s64)b.bits)}; }
static i64 mass_i64_unsigned_multiply(i64 a, i64 b) { return (i64){a.bits * b.bits}; }
static i64 mass_i64_signed_divide(i64 a, i64 b) { return (i64){(u64)((s64)a.bits / (s64)b.bits)}; }
static i64 mass_i64_unsigned_divide(i64 a, i64 b) { return (i64){a.bits / b.bits}; }
static i64 mass_i64_signed_remainder(i64 a, i64 b) { return (i64){(u64)((s64)a.bits % (s64)b.bits)}; }
static i64 mass_i64_unsigned_remainder(i64 a, i64 b) { return (i64){a.bits % b.bits}; }

static i64 mass_i64_logical_shift_left(i64 num, i64 shift) { return (i64){num.bits << shift.bits};}
static i64 mass_i64_logical_shift_right(i64 num, i64 shift) { return (i64){num.bits >> shift.bits};}

static i64 mass_i64_bitwise_and(i64 a, i64 b) { return (i64){a.bits & b.bits}; }
static i64 mass_i64_bitwise_or(i64 a, i64 b) { return (i64){a.bits | b.bits}; }

static bool mass_i64_signed_less(i64 a, i64 b) { return (s64)a.bits < (s64)b.bits; }
static bool mass_i64_unsigned_less(i64 a, i64 b) { return a.bits < b.bits; }
static bool mass_i64_signed_less_equal(i64 a, i64 b) { return (s64)a.bits <= (s64)b.bits; }
static bool mass_i64_unsigned_less_equal(i64 a, i64 b) { return a.bits <= b.bits; }
static bool mass_i64_signed_greater(i64 a, i64 b) { return (s64)a.bits > (s64)b.bits; }
static bool mass_i64_unsigned_greater(i64 a, i64 b) { return a.bits > b.bits; }
static bool mass_i64_signed_greater_equal(i64 a, i64 b) { return (s64)a.bits >= (s64)b.bits; }
static bool mass_i64_unsigned_greater_equal(i64 a, i64 b) { return a.bits >= b.bits; }

static Value *
mass_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *target_descriptor = payload->target;
  Value *expression = payload->expression;
  const Descriptor *source_descriptor = expression->descriptor;

  Expected_Result expected_source = expected_result_any(source_descriptor);
  Value *value = value_force(context, builder, scope, &expected_source, expression);
  if (mass_has_error(context)) return 0;

  Bits cast_to_bit_size = target_descriptor->bit_size;
  Bits original_bit_size = source_descriptor->bit_size;

  Value *result_value = value;
  if (value_is_i64(expression) && descriptor_is_integer(target_descriptor)) {
    result_value = token_value_force_immediate_integer(
      context, value, target_descriptor, source_range
    );
  } else {
    if (cast_to_bit_size.as_u64 > original_bit_size.as_u64) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .detailed_message = "Target type must be smaller or equal to the source type",
        .Type_Mismatch = { .expected = target_descriptor, .actual = source_descriptor },
      });
      return 0;
    } else {
      const Storage *original_storage = &value_as_forced(value)->storage;
      Storage result_storage = value_as_forced(value)->storage;
      if (cast_to_bit_size.as_u64 < original_bit_size.as_u64) {
        if (result_storage.tag == Storage_Tag_Static) {
          const void *memory = get_static_storage_with_bit_size(&result_storage, original_bit_size);
          result_storage = storage_static_heap(memory, cast_to_bit_size);
        } else {
          result_storage.bit_size = cast_to_bit_size;
        }
      }
      // TODO This is awkward and there might be a better way.
      //      It is also might be necessary to somehow mark the original value as invalid maybe?
      result_storage.flags |= (original_storage->flags & Storage_Flags_Temporary);
      result_value = value_make(context, target_descriptor, result_storage, *source_range);
    }
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, result_value
  );
}

static Value *
mass_tuple_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);
  assert(expected_descriptor == payload->target);
  assert(payload->target->tag == Descriptor_Tag_Struct || payload->target->tag == Descriptor_Tag_Fixed_Array);
  Value *result = mass_value_from_expected_result(context, builder, expected_result, *source_range);
  mass_assign_helper(context, builder, result, payload->expression, scope, source_range);
  return result;
}

static Value *
mass_cast_helper(
  Mass_Context *context,
  Parser *parser,
  const Descriptor *target_descriptor,
  Value *expression,
  Source_Range source_range
) {
  Mass_Cast_Lazy_Payload lazy_payload = {
    .target = target_descriptor,
    .expression = expression,
  };

  if (mass_value_is_static(expression)) {
    if (value_is_tuple(expression)) {
      const Tuple *tuple = value_as_tuple(expression);
      if (mass_tuple_is_static(context, tuple)) {
        void *memory = mass_allocate_bytes_from_descriptor(context, target_descriptor);
        Storage storage = storage_static_heap(memory, target_descriptor->bit_size);
        Expected_Result expected_result = mass_expected_result_exact(target_descriptor, storage);
        return mass_tuple_cast_lazy_proc(context, 0, &expected_result, parser->scope, &source_range, &lazy_payload);
      } else {
        Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
        *heap_payload = lazy_payload;

        return mass_make_lazy_value(
          context, parser, source_range, heap_payload, target_descriptor, (Lazy_Value_Proc)mass_tuple_cast_lazy_proc
        );
      }
    } else {
      Expected_Result expected_result = expected_result_any(target_descriptor);
      return mass_cast_lazy_proc(context, 0, &expected_result, parser->scope, &source_range, &lazy_payload);
    }
  }

  Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
  *heap_payload = lazy_payload;

  return mass_make_lazy_value(
    context, parser, source_range, heap_payload, target_descriptor, (Lazy_Value_Proc)mass_cast_lazy_proc
  );
}

static Value *
mass_zero_extend_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *target_descriptor = payload->target;
  Value *expression = payload->expression;
  const Descriptor *source_descriptor = expression->descriptor;

  Bits cast_to_bit_size = target_descriptor->bit_size;
  Bits original_bit_size = source_descriptor->bit_size;

  if (cast_to_bit_size.as_u64 < original_bit_size.as_u64) {
    panic("TODO user error");
    return 0;
  }

  Value *result = mass_value_from_expected_result(context, builder, expected_result, *source_range);
  const Storage *result_storage = &value_as_forced(result)->storage;
  if (cast_to_bit_size.as_u64 > original_bit_size.as_u64) {
    if (cast_to_bit_size.as_u64 > 64) panic("TODO support large zero-extension");
    Storage zero = imm64(0);
    zero.bit_size = cast_to_bit_size;
    move_value(builder, scope, source_range, result_storage, &zero);
  }
  Storage adjusted_storage = *result_storage;
  adjusted_storage.bit_size = original_bit_size;
  Expected_Result adjusted_expected_result =
    mass_expected_result_exact(source_descriptor, adjusted_storage);
  (void)value_force(context, builder, scope, &adjusted_expected_result, expression);

  return mass_expected_result_ensure_value_or_temp(context, builder, scope, expected_result, result);
}

static Value *
mass_zero_extend(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  const Descriptor *target_descriptor = value_ensure_type(
    context, parser->scope, value_view_get(&args_view, 0), args_view.source_range
  );
  if (mass_has_error(context)) return 0;
  if (target_descriptor->bit_size.as_u64 > 64) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = args_view.source_range,
      .detailed_message = "Target type must be smaller or equal to 64 bits",
      .Type_Mismatch = { .expected = &descriptor_void, .actual = target_descriptor },
    });
    return 0;
  }

  Value *expression = value_view_get(&args_view, 1);

  Mass_Cast_Lazy_Payload lazy_payload = {
    .target = target_descriptor,
    .expression = expression,
  };

  if (mass_value_is_static(expression)) {
    u64 result = 0;
    const void *source_memory = storage_static_memory(&value_as_forced(expression)->storage);
    memcpy(&result, source_memory, descriptor_byte_size(target_descriptor));
    Storage storage = storage_immediate_with_bit_size(&result, target_descriptor->bit_size);
    return value_make(context, target_descriptor, storage, args_view.source_range);
  }

  Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
  *heap_payload = lazy_payload;

  return mass_make_lazy_value(
    context, parser, args_view.source_range, heap_payload, target_descriptor, (Lazy_Value_Proc)mass_zero_extend_lazy_proc
  );
}

static Value *
mass_cast(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  const Descriptor *target_descriptor = value_ensure_type(
    context, parser->scope, value_view_get(&args_view, 0), args_view.source_range
  );
  if (mass_has_error(context)) return 0;
  Value *expression = value_view_get(&args_view, 1);
  return mass_cast_helper(context, parser, target_descriptor, expression, args_view.source_range);
}

static bool
token_parse_constant_definitions(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  if (view.length < 2) return false;
  u32 peek_index = 0;
  Value *name = value_view_next(&view, &peek_index);
  Value *double_colon = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator_double_colon
  );
  if (!double_colon) return false;

  if (value_is_group_paren(name)) {
    name = token_parse_single(context, parser, name);
    if (mass_has_error(context)) goto err;
  }
  if (!mass_value_ensure_static_of(context, name, &descriptor_symbol)) goto err;
  const Symbol *symbol = value_as_symbol(name);

  Value_View rest = value_view_rest(&view, peek_index);
  scope_define_lazy_compile_time_expression(context, parser, parser->scope, symbol, rest);

  err:
  return true;
}

typedef struct {
  Value *value;
} Mass_Value_Lazy_Payload;

typedef struct {
  Storage reg;
  Storage stack;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

static void
mass_assert_storage_is_valid_in_context(
  const Storage *storage,
  const Mass_Context *context
) {
  switch(storage->tag) {
    case Storage_Tag_Static: {
      assert(context->program == context->compilation->jit.program);
    } break;
    case Storage_Tag_Immediate:
    case Storage_Tag_Xmm:
    case Storage_Tag_Eflags:
    case Storage_Tag_Register: {
      // Assume valid?
    } break;
    case Storage_Tag_Disjoint: {
      for (u64 i = 0; i < dyn_array_length(storage->Disjoint.pieces); ++i) {
        const Storage *piece = *dyn_array_get(storage->Disjoint.pieces, i);
        mass_assert_storage_is_valid_in_context(piece, context);
      }
    } break;
    case Storage_Tag_Memory: {
      switch(storage->Memory.location.tag) {
        case Memory_Location_Tag_Instruction_Pointer_Relative: {
          Label *label = storage->Memory.location.Instruction_Pointer_Relative.label;
          assert(label->program == context->program);
        } break;
        case Memory_Location_Tag_Indirect:
        case Memory_Location_Tag_Stack: {
          // Assume valid?
        } break;
      }
    } break;
  }
}

static Value *
mass_function_runtime_value_for_call(
  Mass_Context *context,
  Function_Builder *builder,
  const Scope * scope,
  const Function_Info *info,
  Value *overload
) {
  Expected_Result instance_expected_result = expected_result_any(0);
  if (value_is_function_literal(overload)) {
    Function_Literal *literal = value_as_function_literal(overload);
    return mass_function_literal_instance_for_info(context, literal, info);
  } else {
    return value_force(context, builder, scope, &instance_expected_result, overload);
  }
}

static void
mass_x86_64_call_encode_proc(
  Function_Builder *builder,
  Storage address_storage,
  const Source_Range *source_range,
  const Scope *scope
) {
  if (address_storage.tag == Storage_Tag_Immediate) {
    Register temp_reg = register_acquire_temp(builder);
    Storage reg = storage_register(temp_reg, (Bits){64});
    move_value(builder, scope, source_range, &reg, &address_storage);
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range, scope,
      &(Instruction_Assembly){x64_call, {reg}}
    );
    register_release(builder, temp_reg);
  } else {
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range, scope,
      &(Instruction_Assembly){x64_call, {address_storage}}
    );
  }
}

static Value *
call_function_overload(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Function_Call_Lazy_Payload *payload
) {
  Value_View args_view = payload->args;
  Value *runtime_value = mass_function_runtime_value_for_call(
    context, builder, scope, payload->info, payload->overload
  );
  if (mass_has_error(context)) return 0;

  const Storage *runtime_storage = &value_as_forced(runtime_value)->storage;
  mass_assert_storage_is_valid_in_context(runtime_storage, context);

  const Descriptor *runtime_descriptor = runtime_value->descriptor;
  assert(runtime_descriptor->tag == Descriptor_Tag_Function_Instance);

  const Descriptor_Function_Instance *instance_descriptor = &runtime_descriptor->Function_Instance;
  const Function_Info *fn_info = instance_descriptor->info;

  Storage return_storage = instance_descriptor->call_setup.caller_return;
  return_storage.flags |= Storage_Flags_Temporary;

  const Function_Call_Setup *call_setup = &instance_descriptor->call_setup;

  // :TemporaryCallTarget
  // When call target is temporary it might reside in a register that is used for argument passing.
  // To make sure the target is not overwritten with the argument we move it to an empty register.
  // :InstructionQuality The move described above is not always necessary
  Storage call_target_storage = *runtime_storage;
  if (runtime_storage->tag == Storage_Tag_Register) {
    call_target_storage = reserve_stack_storage(builder, (Bits){64});
    call_target_storage.flags |= Storage_Flags_Temporary;
    assert(runtime_storage->bit_size.as_u64 == 64);
    move_value(builder, scope, source_range, &call_target_storage, runtime_storage);
    storage_release_if_temporary(builder, runtime_storage);
  }

  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_Ptr temp_arguments = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = dyn_array_length(call_setup->parameters),
  );
  Array_Value target_params = dyn_array_make(
    Array_Value,
    .allocator = context->temp_allocator,
    .capacity = dyn_array_length(call_setup->parameters),
  );
  Array_Saved_Register stack_saved_registers = dyn_array_make(
    Array_Saved_Register,
    .allocator = context->temp_allocator,
    .capacity = 32,
  );

  u64 all_used_arguments_register_bitset = call_setup->parameter_registers_bitset.bits;
  u64 copied_straight_to_param_bitset = 0;
  u64 temp_register_argument_bitset = 0;
  for (u64 i = 0; i < dyn_array_length(call_setup->parameters); ++i) {
    Function_Call_Parameter *call_param = dyn_array_get(call_setup->parameters, i);
    Value *target_param = dyn_array_push_uninitialized(target_params);

    bool is_zero_sized = call_param->descriptor->bit_size.as_u64 == 0;
    if (call_param->flags & Function_Call_Parameter_Flags_Implicit_Pointer) {
      value_init(target_param, &descriptor_void_pointer, call_param->storage, *source_range);

      // :ZeroSizeImplicitPointer
      // On windows, zero-sized values end up as an implicit pointer because
      // they are not exactly 1, 2, 4 or 8 bytes as the calling convention says:
      //   https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#calling-convention-defaults
      if (is_zero_sized) {
        Storage null_storage = storage_immediate_with_bit_size(&(u64){0}, (Bits) { 64 });
        Value* arg_value = value_make(context, target_param->descriptor, null_storage, *source_range);
        dyn_array_push(temp_arguments, arg_value);
        continue;
      }
    } else {
      value_init(target_param, call_param->descriptor, call_param->storage, *source_range);
      // If argument is not an implicit pointer and zero size, we don't really need a register for it
      if (is_zero_sized) {
        Storage void_storage = storage_immediate_with_bit_size(0, (Bits) { 0 });
        Value* arg_value = value_make(context, target_param->descriptor, void_storage, *source_range);
        dyn_array_push(temp_arguments, arg_value);
        continue;
      }
    }

    if (call_param->flags & Function_Call_Parameter_Flags_Uninitialized) {
      Storage source_storage = reserve_stack_storage(builder, target_param->descriptor->bit_size);
      Value *arg_value = value_make(context, target_param->descriptor, source_storage, *source_range);
      dyn_array_push(temp_arguments, arg_value);
      continue;
    }
    // :ParameterOriginalIndex
    Value *source_arg = value_view_get(&args_view, call_param->original_index);
    const Storage *maybe_source_storage = 0;
    if (source_arg->tag == Value_Tag_Forced) {
      maybe_source_storage = &value_as_forced(source_arg)->storage;
    }
    const Descriptor *stack_descriptor = call_param->descriptor;

    u64 target_param_register_bitset = register_bitset_from_storage(&call_param->storage);
    if (target_param_register_bitset >> 16) {
      panic("Found XMM usage");
    }

    // Generally we can not optimize a copy to stack if the target is implicit pointer
    // or an indirect storage because they need a memory address.
    bool needs_memory_address = (
      (call_param->flags & Function_Call_Parameter_Flags_Implicit_Pointer) ||
      storage_is_indirect(&call_param->storage)
    );

    bool can_use_source_arg_as_is;
    // If source args are on the stack or rip-relative we don't need to worry about their registers,
    // but it is not true for all Storage_Tag_Memory, because indirect access uses registers
    if (
      maybe_source_storage &&
      (storage_is_stack(maybe_source_storage) || storage_is_label(maybe_source_storage))) {
      can_use_source_arg_as_is = true;
    } else if (
      // Compile time-known args also don't need registers so they can be used as-is
      mass_value_is_static(source_arg) &&
      // TODO Consider if it is ok to just pass the address as an immediate here
      !needs_memory_address
    ) {
      can_use_source_arg_as_is = true;
    } else {
      can_use_source_arg_as_is = false;
    }

    bool target_param_registers_are_free =
      !(builder->register_occupied_bitset.bits & target_param_register_bitset);
    bool can_assign_straight_to_target =
      target_param_registers_are_free && !needs_memory_address;

    static const bool SHOULD_OPTIMIZE = true;

    Value *arg_value;
    bool should_assign = true;
    // If target parameter storage is stack, then copying directly is *always*
    // the right thing to do, not just when the optimization is on.
    if (storage_is_stack(&call_param->storage)) {
      arg_value = value_init(
        mass_allocate(context, Value),
        stack_descriptor, call_param->storage, *source_range
      );
    } else if (SHOULD_OPTIMIZE && can_use_source_arg_as_is) {
      arg_value = source_arg;
      should_assign = false;
    } else if (SHOULD_OPTIMIZE && can_assign_straight_to_target) {
      arg_value = target_param;
      copied_straight_to_param_bitset |= target_param_register_bitset;
      register_acquire_bitset(builder, target_param_register_bitset);
    } else {
      u64 prohibited_registers
        = temp_register_argument_bitset
        | all_used_arguments_register_bitset
        | builder->register_occupied_bitset.bits;
      u64 allowed_temp_registers = registers_that_can_be_temp & ~prohibited_registers;
      u64 required_register_count = register_bitset_occupied_count(target_param_register_bitset);
      if (
        SHOULD_OPTIMIZE &&
        // TODO it should be possible to do this for unpacked structs as well,
        //      but it will be quite gnarly
        required_register_count == 1 &&
        !needs_memory_address &&
        register_bitset_occupied_count(allowed_temp_registers) > 1
      ) {
        Register temp_register = register_find_available(builder, prohibited_registers);
        register_acquire(builder, temp_register);
        register_bitset_set(&temp_register_argument_bitset, temp_register);
        Storage arg_storage = storage_register(temp_register, call_param->descriptor->bit_size);
        arg_storage.flags |= Storage_Flags_Temporary;
        arg_value = value_make(context, call_param->descriptor, arg_storage, source_arg->source_range);
      } else {
        // The code below is useful to check how many spills to stack happen
        //static int stack_counter = 0;
        //printf(" > stack %i\n", stack_counter++);
        Storage stack_storage = reserve_stack_storage(builder, stack_descriptor->bit_size);
        stack_storage.flags |= Storage_Flags_Temporary;
        arg_value = value_make(context, stack_descriptor, stack_storage, *source_range);
      }
    }
    if (should_assign) {
      mass_assign_helper(context, builder, arg_value, source_arg, scope, source_range);
      if (mass_has_error(context)) return 0;
    }
    dyn_array_push(temp_arguments, arg_value);
  }

  u64 target_volatile_registers_bitset = call_setup->calling_convention->register_volatile_bitset.bits;
  u64 expected_result_bitset = 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      const Storage *expected = &expected_result->Exact.storage;
      if (expected->tag == Storage_Tag_Register || expected->tag == Storage_Tag_Disjoint) {
        expected_result_bitset = register_bitset_from_storage(expected);
      }
      break;
    }
    case Expected_Result_Tag_Flexible: break;
  }

  u64 saved_registers_from_arguments_bitset = (
    // Need to save registers that are volatile in the callee and are actually used in the caller,
    (target_volatile_registers_bitset & builder->register_occupied_bitset.bits)
    &
    // but only if we are not using them for optimized arguments assignment.
    (~(copied_straight_to_param_bitset | temp_register_argument_bitset))
  );
  // We must not save the register(s) that will be used for the result
  // otherwise we will overwrite it with the restored value
  u64 saved_registers_bitset = saved_registers_from_arguments_bitset & ~expected_result_bitset;
  if (saved_registers_bitset) {
    // TODO can use bit scan to skip to the used register
    for (Register reg_index = 0; reg_index <= Register_R15; ++reg_index) {
      if (!register_bitset_get(saved_registers_bitset, reg_index)) continue;

      Saved_Register *saved = dyn_array_push(stack_saved_registers, (Saved_Register) {
        .reg = storage_register(reg_index, (Bits){64}),
        .stack = reserve_stack_storage(builder, (Bits){64}),
      });

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range, scope,
        &(Instruction_Assembly){x64_mov, {saved->stack, saved->reg}}
      );
    }
  }

  register_release_bitset(builder, saved_registers_from_arguments_bitset);
  u64 spilled_param_register_bitset = all_used_arguments_register_bitset & ~copied_straight_to_param_bitset;
  register_acquire_bitset(builder, spilled_param_register_bitset);

  for (u64 i = 0; i < dyn_array_length(target_params); ++i) {
    Value *param = dyn_array_get(target_params, i);
    Value *source_arg = *dyn_array_get(temp_arguments, i);
    Function_Call_Parameter *call_param = dyn_array_get(call_setup->parameters, i);

    const Storage *param_storage = &value_as_forced(param)->storage;
    const Storage *source_storage = &value_as_forced(source_arg)->storage;
    bool is_implicit_pointer = !!(call_param->flags & Function_Call_Parameter_Flags_Implicit_Pointer);
    bool is_zero_sized = call_param->descriptor->bit_size.as_u64 == 0;
    if (storage_equal(param_storage, source_storage) && !is_implicit_pointer) continue;

    if (is_implicit_pointer) {
      // :ZeroSizeImplicitPointer
      if (is_zero_sized) {
        move_value(builder, scope, source_range, param_storage, source_storage);
      } else {
        mass_storage_load_address(builder, source_range, scope, param_storage, source_storage);
      }
    } else if (storage_is_indirect(param_storage)) {
      Register base_register = param_storage->Memory.location.Indirect.base_register;
      Storage target_storage = storage_register(base_register, (Bits){64});
      mass_storage_load_address(builder, source_range, scope, &target_storage, source_storage);
    } else {
      mass_assign_helper(context, builder, param, source_arg, scope, source_range);
      if (mass_has_error(context)) return 0;
    }
  }

  builder->max_call_parameters_stack_size = u32_max(
    builder->max_call_parameters_stack_size,
    call_setup->parameters_stack_size
  );

  call_setup->call_encode_proc(builder, call_target_storage, source_range, scope);
  storage_release_if_temporary(builder, &call_target_storage);

  register_release_bitset(builder, all_used_arguments_register_bitset | temp_register_argument_bitset);

  u64 return_value_bitset = register_bitset_from_storage(&return_storage);

  // If return value would be overwritten by restored registers we are saving it to the stack.
  // This is not the most efficient way though. @InstructionQuality
  if (return_value_bitset & saved_registers_from_arguments_bitset) {
    Storage temp_return_storage = reserve_stack_storage(builder, return_storage.bit_size);
    move_value(builder, scope, source_range, &temp_return_storage, &return_storage);
    temp_return_storage.flags |= Storage_Flags_Temporary;
    return_storage = temp_return_storage;
  } else {
    register_acquire_bitset(builder, (return_value_bitset & ~expected_result_bitset));
  }

  DYN_ARRAY_FOREACH(Saved_Register, saved, stack_saved_registers) {
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range, scope,
      &(Instruction_Assembly){x64_mov, {saved->reg, saved->stack}}
    );
  }

  register_acquire_bitset(builder, saved_registers_from_arguments_bitset);

  Value *fn_return_value = value_make(context, fn_info->return_descriptor, return_storage, *source_range);

  Value *expected_value = mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, fn_return_value
  );

  context_temp_reset_to_mark(context, temp_mark);

  return expected_value;
}

static void
mass_function_info_init_for_header_and_maybe_body(
  Mass_Context *context,
  const Scope *arguments_scope,
  const Function_Header *header,
  Value *maybe_body,
  Function_Info *out_info
) {
  Mass_Context temp_context = mass_context_from_compilation(context->compilation);
  Temp_Mark temp_mark = context_temp_mark(&temp_context);
  Parser args_parser = {
    .flags = Parser_Flags_None,
    .scope = scope_make_declarative(temp_context.temp_allocator, arguments_scope),
  };

  *out_info = (Function_Info) {
    .flags = Function_Info_Flags_None,
    .parameters = dyn_array_make(
      Array_Resolved_Function_Parameter,
      .allocator = context->allocator,
      .capacity = dyn_array_length(header->parameters),
    ),
  };

  DYN_ARRAY_FOREACH(Function_Parameter, param, header->parameters) {
    Source_Range source_range = param->source_range;
    if (param->maybe_type_expression.length) {
      scope_define_lazy_compile_time_expression(
        &temp_context, &args_parser, args_parser.scope, param->symbol, param->maybe_type_expression
      );
    } else {
      const Descriptor *descriptor = param->descriptor;
      if (!descriptor) {
        assert(param->maybe_default_value);
        descriptor = deduce_runtime_descriptor_for_value(context, param->maybe_default_value, 0);
      }
      Storage storage = storage_immediate(&descriptor);
      Value *param_value = value_make(&temp_context, &descriptor_type, storage, source_range);
      scope_define_value(args_parser.scope, VALUE_STATIC_EPOCH, source_range, param->symbol, param_value);
    }
  }

  DYN_ARRAY_FOREACH(Function_Parameter, param, header->parameters) {
    switch(param->tag) {
      case Function_Parameter_Tag_Runtime:
      case Function_Parameter_Tag_Generic: {
        const Descriptor *descriptor = param->descriptor;
        if (!param->descriptor) {
          const Symbol *symbol = param->symbol;
          Source_Range source_range = param->source_range;
          Value *type_value =
            mass_context_force_lookup(&temp_context, &args_parser, args_parser.scope, symbol, &source_range);
          if (mass_has_error(&temp_context)) goto err;
          descriptor = value_ensure_type(&temp_context, args_parser.scope, type_value, source_range);
          if (mass_has_error(&temp_context)) goto err;
        }

        bool was_generic = false;
        if (param->tag == Function_Parameter_Tag_Generic) {
          if (!param->Generic.maybe_type_constraint) {
            was_generic = true;
          }
        }

        // Default value might need to be cast to the actual type of the parameter
        Value *maybe_default_value = param->maybe_default_value;
        if (maybe_default_value && !same_type(descriptor, maybe_default_value->descriptor)) {
          const Descriptor *runtime_descriptor =
            deduce_runtime_descriptor_for_value(context, maybe_default_value, descriptor);
          if (!runtime_descriptor) {
            mass_error(context, (Mass_Error) {
              .tag = Mass_Error_Tag_Type_Mismatch,
              .source_range = maybe_default_value->source_range,
              .Type_Mismatch = { .expected = descriptor, .actual = maybe_default_value->descriptor },
            });
            goto err;
          }
          maybe_default_value = mass_cast_helper(
            context, &args_parser, descriptor, maybe_default_value, param->source_range
          );
          if (mass_has_error(context)) goto err;
        }

        dyn_array_push(out_info->parameters, (Resolved_Function_Parameter) {
          .tag = Resolved_Function_Parameter_Tag_Unknown,
          .descriptor = descriptor,
          .symbol = param->symbol,
          .source_range = param->source_range,
          .was_generic = was_generic,
          .maybe_default_value = maybe_default_value,
        });
      } break;
      case Function_Parameter_Tag_Exact_Static: {
        assert(param->descriptor);
        dyn_array_push(out_info->parameters, (Resolved_Function_Parameter) {
          .tag = Resolved_Function_Parameter_Tag_Known,
          .Known = { .storage = function_parameter_as_exact_static(param)->storage },
          .descriptor = param->descriptor,
          .symbol = param->symbol,
          .source_range = param->source_range,
          .maybe_default_value = param->maybe_default_value,
        });
      } break;
    }
  }

  switch(header->returns.tag) {
    case Function_Return_Tag_Inferred: {
      if (header->flags & Function_Header_Flags_Intrinsic) {
        // Handled in :IntrinsicReturnType
        out_info->return_descriptor = 0;
      } else {
        // TODO figure out how to provide a better error message here :RecursiveInferredType
        out_info->return_descriptor =
          mass_infer_function_return_type(context, out_info, arguments_scope, maybe_body);
        if (mass_has_error(context)) return;
      }
    } break;
    case Function_Return_Tag_Exact: {
      // Nothing to do, we already know the type
      out_info->return_descriptor = header->returns.Exact.descriptor;
    } break;
    case Function_Return_Tag_Generic: {
      const Descriptor *descriptor =
        token_match_type(&temp_context, &args_parser, header->returns.Generic.type_expression);
      out_info->return_descriptor = descriptor;
    } break;
  }

  err:
  context_temp_reset_to_mark(&temp_context, temp_mark);
}

typedef struct {
  Array_Resolved_Function_Parameter parameters;
  bool all_arguments_are_compile_time_known;
} Mass_Overload_Match_Args;

typedef enum {
  Mass_Argument_Scoring_Flags_None = 0 << 0,
  Mass_Argument_Scoring_Flags_Prefer_Compile_Time = 1 << 0,
} Mass_Argument_Scoring_Flags;

static Overload_Match_Summary
calculate_arguments_match_score(
  Mass_Context *context,
  const Function_Info *info,
  Array_Resolved_Function_Parameter source_params,
  Mass_Argument_Scoring_Flags scoring_flags
) {
  assert(dyn_array_length(source_params) < 65535);
  if (dyn_array_length(source_params) > dyn_array_length(info->parameters)) {
    return (Overload_Match_Summary){0};
  }
  Overload_Match_Summary result = {
    .compile_time = !!(scoring_flags & Mass_Argument_Scoring_Flags_Prefer_Compile_Time),
    .matched = true,
    .inverted_generic_count = UINT16_MAX,
    .inverted_cast_count = UINT16_MAX,
  };
  for (u64 arg_index = 0; arg_index < dyn_array_length(info->parameters); ++arg_index) {
    Resolved_Function_Parameter *target_param = dyn_array_get(info->parameters, arg_index);
    if (arg_index >= dyn_array_length(source_params)) {
      continue; // Default params always match
    }
    const Resolved_Function_Parameter *source_param = dyn_array_get(source_params, arg_index);
    const Descriptor *target_descriptor = target_param->descriptor;
    if (target_param->was_generic) {
      result.inverted_generic_count -= 1;
    }
    const Descriptor *source_descriptor = source_param->descriptor;
    switch(target_param->tag) {
      case Resolved_Function_Parameter_Tag_Unknown: {
        if (same_type(target_descriptor, source_descriptor)) {
          continue;
        }
        if (source_param->tag == Resolved_Function_Parameter_Tag_Known) {
          Value fake_source_value;
          Storage storage = source_param->Known.storage;
          value_init(&fake_source_value, source_param->descriptor, storage, source_param->source_range);
          source_descriptor = deduce_runtime_descriptor_for_value(
            context, &fake_source_value, target_descriptor
          );
          if (!source_descriptor) {
            return (Overload_Match_Summary){0};
          }
        } else {
          return (Overload_Match_Summary){0};
        }
      } break;
      case Resolved_Function_Parameter_Tag_Known: {
        result.exact_count += 1;
        if (source_param->tag != Resolved_Function_Parameter_Tag_Known) {
          return (Overload_Match_Summary){0};
        }
        if (!storage_static_equal(
          target_descriptor, &target_param->Known.storage,
          source_descriptor, &source_param->Known.storage
        )) {
          return (Overload_Match_Summary){0};
        }
      } break;
    }
  }
  return result;
}

static inline bool
match_overload_argument_count(
  Array_Function_Parameter params,
  u64 actual_count
) {
  if (actual_count > dyn_array_length(params)) return false;
  if (actual_count == dyn_array_length(params)) return true;
  // FIXME @Speed cache this as a range
  for (u64 arg_index = 0; arg_index < dyn_array_length(params); ++arg_index) {
    Function_Parameter *param = dyn_array_get(params, arg_index);
    if (arg_index < actual_count) continue;
    if (!param->maybe_default_value) return false;
  }
  return true;
}

static void
mass_match_overload_candidate(
  Mass_Context *context,
  Value *candidate,
  const Mass_Overload_Match_Args *args,
  Array_Overload_Match_State matches
) {
  if (mass_has_error(context)) return;
  if (value_is_overload(candidate)) {
    const Overload *overload = value_as_overload(candidate);
    mass_match_overload_candidate(context, overload->value, args, matches);
    mass_match_overload_candidate(context, overload->next, args, matches);
  } else {
    Mass_Argument_Scoring_Flags scoring_flags = 0;
    const Function_Info *overload_info;
    if (value_is_function_literal(candidate)) {
      const Function_Literal *literal = value_as_function_literal(candidate);
      // :OverloadLock Disallow matching this literal if it was locked for some reason
      if (*literal->overload_lock_count) {
        return;
      }
      if (literal->header.flags & Function_Header_Flags_Compile_Time) {
        if (!args->all_arguments_are_compile_time_known) return;
        scoring_flags |= Mass_Argument_Scoring_Flags_Prefer_Compile_Time;
      }
      if (!match_overload_argument_count(literal->header.parameters, dyn_array_length(args->parameters))) {
        return;
      }
      Function_Info *specialized_info;
      {
        // :OverloadLock
        // A literal must be locked here to allow using its overload to be used
        // inside the function signature. Here's an example:
        //    pointer_to :: fn(type : Type) => (Type) MASS.pointer_to_type
        //    pointer_to :: fn(x) -> (pointer_to(x)) MASS.pointer_to
        // Without the lock the second literal will infinitely recurse
        *literal->overload_lock_count += 1;
        specialized_info = function_literal_info_for_parameters(context, literal, args->parameters);
        *literal->overload_lock_count -= 1;
      }
      overload_info = specialized_info;
      if (!overload_info) return;
    } else {
      const Descriptor *descriptor = candidate->descriptor;
      const Descriptor_Function_Instance *instance = descriptor_as_function_instance(descriptor);
      overload_info = instance->info;
      if (overload_info->flags & Function_Info_Flags_Compile_Time) {
        assert(instance->program == context->compilation->jit.program);
        if (!args->all_arguments_are_compile_time_known) return;
        scoring_flags |= Mass_Argument_Scoring_Flags_Prefer_Compile_Time;
      } else {
        if (instance->program && instance->program != context->program) return;
      }
    }

    Overload_Match_Summary summary = calculate_arguments_match_score(
      context, overload_info, args->parameters, scoring_flags
    );
    if (summary.matched) {
      dyn_array_push(matches, (Overload_Match_State) {
        .info = overload_info,
        .value = candidate,
        .summary = summary,
      });
    }
  }
}

static Overload_Match
mass_match_overload(
  Mass_Context *context,
  Value *value,
  Array_Resolved_Function_Parameter parameters
) {
  Mass_Overload_Match_Args args = {
    .parameters = parameters,
    .all_arguments_are_compile_time_known = true,
  };
  for (u64 i = 0; i < dyn_array_length(parameters); ++i) {
    const Resolved_Function_Parameter *param = dyn_array_get(parameters, i);
    if (param->tag != Resolved_Function_Parameter_Tag_Known) {
      args.all_arguments_are_compile_time_known = false;
      break;
    }
  }
  // TODO Store overload count on the Overload to know how much to reserve here
  Array_Overload_Match_State matches = dyn_array_make(
    Array_Overload_Match_State,
    .capacity = 16,
    .allocator = context->temp_allocator,
  );
  mass_match_overload_candidate(context, value, &args, matches);
  if (!dyn_array_length(matches)) {
    return (Overload_Match){.tag = Overload_Match_Tag_No_Match};
  }
  const Overload_Match_State *best_match = dyn_array_get(matches, 0);
  // The code below is extremely performance-sensitive for programs with a lot of overloads.
  // It is also really hard to guess which version optimized by Clang performs best
  // so make sure to check performance numbers even when making small changes!
  if (dyn_array_length(matches) > 1) {
    const Overload_Match_State *conflict_match = best_match;
    DYN_ARRAY_FOREACH(Overload_Match_State, match, matches) {
      if (memcmp(&match->summary, &best_match->summary, sizeof(best_match->summary)) > 0) {
        best_match = match;
      }
      if (memcmp(&match->summary, &best_match->summary, sizeof(best_match->summary)) == 0) {
        conflict_match = match;
      }
    }
    if (
      conflict_match != best_match &&
      memcmp(&conflict_match->summary, &best_match->summary, sizeof(best_match->summary)) == 0
    ) {
      Array_Undecidable_Match undecidable_overloads = dyn_array_make(
        Array_Undecidable_Match,
        .capacity = dyn_array_length(matches),
        .allocator = context->allocator,
      );
      DYN_ARRAY_FOREACH(Overload_Match_State, match, matches) {
        if (memcmp(&match->summary, &best_match->summary, sizeof(best_match->summary)) == 0) {
          dyn_array_push(undecidable_overloads, (Undecidable_Match) {
            .info = match->info,
            .value = match->value,
          });
        }
      }
      return (Overload_Match){
        .tag = Overload_Match_Tag_Undecidable,
        .Undecidable = { undecidable_overloads },
      };
    }
  }
  return (Overload_Match){
    .tag = Overload_Match_Tag_Found,
    .Found = { .value = best_match->value, .info = best_match->info },
  };

}

static bool
mass_match_overload_or_error(
  Mass_Context *context,
  Value *target,
  Array_Resolved_Function_Parameter arg_parameters,
  Overload_Match_Found *match_found,
  const Source_Range *source_range
) {
  Overload_Match match = mass_match_overload(context, target, arg_parameters);
  if (mass_has_error(context)) return 0;
  switch(match.tag) {
    case Overload_Match_Tag_No_Match: {
      Array_Resolved_Function_Parameter arguments;
      dyn_array_copy_from_temp(Array_Resolved_Function_Parameter, context, &arguments, arg_parameters);
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_No_Matching_Overload,
        .source_range = *source_range,
        .No_Matching_Overload = {
          .target = target,
          .arguments = arguments
        },
      });
      return false;
    }
    case Overload_Match_Tag_Undecidable: {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Undecidable_Overload,
        .Undecidable_Overload = { .matches = match.Undecidable.matches },
        .source_range = *source_range,
      });
      return false;
    }
    case Overload_Match_Tag_Found: {
      *match_found = match.Found;
      return true;
    }
    default: {
      panic("Unexpected Overload_Match_Tag");
      break;
    }
  }
  return false;
}

static inline Value *
mass_intrinsic_call(
  Mass_Context *context,
  Parser *parser,
  Value *overload,
  const Descriptor *expected_descriptor,
  Value_View args_view
) {
  Mass_Intrinsic_Proc proc;
  Mass_Intrinsic_Proc *maybe_cached_proc =
    hash_map_get(context->compilation->intrinsic_proc_cache_map, overload);
  if (maybe_cached_proc) {
    proc = *maybe_cached_proc;
  } else {
    const Descriptor *intrinsic_descriptor =
    deduce_runtime_descriptor_for_value(context, overload, &descriptor_mass_intrinsic_proc);
    assert(!mass_has_error(context));
    assert(intrinsic_descriptor);
    const Function_Info *intrinsic_info = descriptor_as_function_instance(intrinsic_descriptor)->info;
    proc = (Mass_Intrinsic_Proc)mass_ensure_jit_function_for_value(context, overload, intrinsic_info);
    if (mass_has_error(context)) return 0;
    hash_map_set(context->compilation->intrinsic_proc_cache_map, overload, proc);
  }

  Value *result = proc(context, parser, args_view);
  if (mass_has_error(context)) return 0;

  // :IntrinsicReturnType
  if (expected_descriptor && !same_type(expected_descriptor, result->descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = args_view.source_range,
      .Type_Mismatch = { .expected = expected_descriptor, .actual = result->descriptor },
    });
    return 0;
  }
  return result;
}

static inline bool
value_is_intrinsic(
  Value *value
) {
  if (value_is_function_literal(value)) {
    return !!(value_as_function_literal(value)->header.flags & Function_Header_Flags_Intrinsic);
  }
  if (value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    return !!(value->descriptor->Function_Instance.info->flags & Function_Info_Flags_Intrinsic);
  }

  return false;
}

static const Mass_Trampoline *
mass_ensure_trampoline(
  Mass_Context *context,
  Parser *parser,
  Value *original,
  const Function_Info *original_info,
  Value_View args_view
) {
  const Mass_Trampoline **maybe_trampoline_pointer =
    hash_map_get(context->compilation->trampoline_map, original_info);
  if (maybe_trampoline_pointer) {
    return *maybe_trampoline_pointer;
  }

  // Trampoline needs to be compiled in the jit context, so we substitute it here
  Mass_Context jit_context = *context;
  jit_context.program = context->compilation->jit.program;
  context = &jit_context;
  Program *program = context->program;

  // Whenever a trampoline is generated it needs an instance compiled for JIT execution.
  // The `Function_Info` of that instance must not be marked `Compile_Time` otherwise
  // it will not be matched during compilation of the trampoline body as the arguments
  // to the trampoline are not compile-time known.
  //
  // To solve this here we create a clone of the instanced with a swapped out
  // `Function_Info` that is not marked `Compile_Time`.
  //
  // TODO would be nice to structure the types in such a way that this would be easier to do
  Value *proxy_value;
  {
    Function_Info *proxy_info = mass_allocate(context, Function_Info);
    *proxy_info = *original_info;
    proxy_info->flags &= ~Function_Info_Flags_Compile_Time;

    const Calling_Convention *calling_convention = program->default_calling_convention;
    Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, proxy_info);
    Descriptor *proxy_descriptor = descriptor_function_instance(
      context->allocator, proxy_info, call_setup, program
    );
    proxy_descriptor->Function_Instance.info = proxy_info;
    Value *runtime_instance = original;
    if (value_is_function_literal(runtime_instance)) {
      Function_Literal *literal = value_as_function_literal(runtime_instance);
      runtime_instance = mass_function_literal_instance_for_info(context, literal, proxy_info);
    }
    if (mass_has_error(context)) return 0;
    const Storage *runtime_storage = &value_as_forced(runtime_instance)->storage;
    proxy_value = value_make(context, proxy_descriptor, *runtime_storage, original->source_range);
  }

  C_Struct_Aligner struct_aligner = {0};
  Array_Struct_Field fields = dyn_array_make(
    Array_Struct_Field,
    .allocator = context->allocator,
    .capacity = args_view.length,
  );
  for (u64 i = 0; i < args_view.length; ++i) {
    Value *item = value_view_get(&args_view, i);
    const Descriptor *field_descriptor = item->descriptor;
    assert(item->tag == Value_Tag_Forced);
    u64 field_byte_offset = c_struct_aligner_next_byte_offset(&struct_aligner, field_descriptor);

    dyn_array_push(fields, (Struct_Field) {
      .name = {0},
      .descriptor = field_descriptor,
      .offset = field_byte_offset,
    });
  }

  c_struct_aligner_end(&struct_aligner);

  Descriptor *args_struct_descriptor = allocator_allocate(context->allocator, Descriptor);
  *args_struct_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .bit_size = {struct_aligner.bit_size},
    .bit_alignment = {struct_aligner.bit_alignment},
    .Struct = { .fields = fields, },
  };

  // The code below is a specialized version of `mass_function_literal_instance_for_info`.
  // This avoids generating a literal with a text version of the body.
  Scope *trampoline_scope = scope_make_declarative(context->allocator, context->compilation->root_scope);

  Source_Range return_range;
  INIT_LITERAL_SOURCE_RANGE(&return_range, "()");

  const Calling_Convention *calling_convention = program->default_calling_convention;

  Parser body_parser = {
    .flags = Parser_Flags_None,
    .scope = trampoline_scope,
    .epoch = get_new_epoch(),
    .module = parser->module,
  };
  const Function_Info *proxy_info =
    descriptor_as_function_instance(&descriptor_mass_trampoline_proc)->info;

  Label *start_label = make_label(context->allocator, program, &program->memory.code, slice_literal(":start"));
  Label *end_label = make_label(context->allocator, program, &program->memory.code, slice_literal(":end"));
  Function_Builder *builder = &(Function_Builder){
    .epoch = body_parser.epoch,
    .function = proxy_info,
    .register_volatile_bitset = calling_convention->register_volatile_bitset,
    .return_value = {0},
    .code_block = {
      .allocator = context->allocator,
      .start_label = start_label,
      .end_label = end_label,
    },
  };
  value_init(&builder->return_value, &descriptor_void, imm0, return_range);

  Function_Call_Setup call_setup = calling_convention->call_setup_proc(context->allocator, proxy_info);
  register_acquire_bitset(builder, call_setup.parameter_registers_bitset.bits);

  assert(dyn_array_length(call_setup.parameters) == 2);
  const Storage *return_pointer_storage = &dyn_array_get(call_setup.parameters, 0)->storage;
  const Storage *args_struct_pointer_storage = &dyn_array_get(call_setup.parameters, 1)->storage;

  assert(args_struct_pointer_storage->tag == Storage_Tag_Register);
  // TODO Moving this value to a volatile register that is *not* used
  //      in target parameter passing can result in way less spills
  //      given current weak optimization state.
  Storage args_struct_indirect_storage = storage_indirect(
    args_struct_descriptor->bit_size, args_struct_pointer_storage->Register.index
  );

  Array_Value_Ptr destructured_arg_array = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->allocator,
    .capacity = dyn_array_length(fields),
  );
  for (u64 i = 0; i < dyn_array_length(fields); ++i) {
    const Struct_Field *field = dyn_array_get(fields, i);
    Storage storage;
    Resolved_Function_Parameter *param = dyn_array_get(original_info->parameters, i);
    if (param->tag == Resolved_Function_Parameter_Tag_Known) {
      storage = param->Known.storage;
      assert(same_type(field->descriptor, param->descriptor));
    } else {
      storage = storage_with_offset_and_bit_size(
        &args_struct_indirect_storage, u64_to_s32(field->offset), field->descriptor->bit_size
      );
    }
    Value *arg_value = value_make(context, field->descriptor, storage, args_view.source_range);
    dyn_array_push(destructured_arg_array, arg_value);
  }
  Value_View destructured_arg_view = value_view_from_value_array(destructured_arg_array, &return_range);
  Value *call_result = token_handle_function_call(
    context, &body_parser, proxy_value, destructured_arg_view, return_range
  );
  if (mass_has_error(context)) return 0;

  const Descriptor *return_descriptor = original_info->return_descriptor;
  assert(return_pointer_storage->tag == Storage_Tag_Register);
  Storage return_indirect_storage = storage_indirect(
    return_descriptor->bit_size, return_pointer_storage->Register.index
  );

  Value *indirect_return = value_make(context, return_descriptor, return_indirect_storage, return_range);

  mass_assign_helper(context, builder, indirect_return, call_result, parser->scope, &return_range);
  if (mass_has_error(context)) return 0;

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .scope = parser->scope,
    .Label.pointer = builder->code_block.end_label,
  });

  calling_convention_x86_64_common_end_proc(program, builder);

  // Only push the builder at the end to avoid problems in nested JIT compiles
  dyn_array_push(program->functions, *builder);

  const Descriptor *instance_descriptor =
    descriptor_function_instance(context->allocator, proxy_info, call_setup, program);
  Value *proxy_instance = value_make(
    context, instance_descriptor, code_label32(start_label), original->source_range
  );

  Mass_Trampoline *trampoline = allocator_allocate(context->allocator, Mass_Trampoline);
  *trampoline = (Mass_Trampoline) {
    .args_descriptor = args_struct_descriptor,
    .proc = (Mass_Trampoline_Proc)mass_ensure_jit_function_for_value(
      context, proxy_instance, proxy_info
    ),
    .original_info = original_info,
  };
  hash_map_set(context->compilation->trampoline_map, original_info, trampoline);
  return trampoline;
}

static Value *
mass_trampoline_call(
  Mass_Context *context,
  Parser *parser,
  Value *original,
  const Function_Info *original_info,
  Value_View args_view
) {
  const Mass_Trampoline *trampoline =
    mass_ensure_trampoline(context, parser, original, original_info, args_view);
  if (mass_has_error(context)) return 0;

  Temp_Mark temp_mark = context_temp_mark(context);
  u64 bit_size = trampoline->args_descriptor->bit_size.as_u64;
  u8* args_struct_memory = 0;

  if (bit_size) {
    args_struct_memory = allocator_allocate_bytes(
      context->temp_allocator,
      descriptor_byte_size(trampoline->args_descriptor),
      descriptor_byte_alignment(trampoline->args_descriptor)
    );

    Array_Struct_Field fields = trampoline->args_descriptor->Struct.fields;
    assert(trampoline->args_descriptor->tag == Descriptor_Tag_Struct);
    for (u64 i = 0; i < args_view.length; ++i) {
      Value* item = value_view_get(&args_view, i);
      assert(mass_value_is_static(item));
      const Struct_Field* field = dyn_array_get(fields, i);
      u64 offset = field->offset;
      void* arg_memory = args_struct_memory + offset;
      const void* source_memory = storage_static_memory_with_bit_size(
        &value_as_forced(item)->storage, item->descriptor->bit_size
      );
      memcpy(arg_memory, source_memory, descriptor_byte_size(item->descriptor));
    }
  }

  const Descriptor *return_descriptor = trampoline->original_info->return_descriptor;
  void *return_memory = mass_allocate_bytes_from_descriptor(context, return_descriptor);
  Storage return_storage = storage_static_heap(return_memory, return_descriptor->bit_size);
  Value *result = value_make(context, return_descriptor, return_storage, args_view.source_range);

  trampoline->proc(return_memory, args_struct_memory);
  context_temp_reset_to_mark(context, temp_mark);

  return result;
}

static Value *
token_handle_function_call(
  Mass_Context *context,
  Parser *parser,
  Value *target_expression,
  Value_View args_view,
  Source_Range source_range
) {
  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Resolved_Function_Parameter arg_parameters = dyn_array_make(
    Array_Resolved_Function_Parameter,
    .capacity = args_view.length,
    .allocator = context->temp_allocator,
  );
  for (u64 i = 0; i < args_view.length; ++i) {
    Value *arg = value_view_get(&args_view, i);
    const Descriptor *descriptor = arg->descriptor;
    if (mass_value_is_static(arg)) {
      dyn_array_push(arg_parameters, (Resolved_Function_Parameter) {
        .tag = Resolved_Function_Parameter_Tag_Known,
        .Known = {.storage = value_as_forced(arg)->storage},
        .descriptor = descriptor,
        .source_range = arg->source_range,
      });
    } else {
      dyn_array_push(arg_parameters, (Resolved_Function_Parameter) {
        .tag = Resolved_Function_Parameter_Tag_Unknown,
        .descriptor = descriptor,
        .source_range = arg->source_range,
      });
    }
  }
  Overload_Match_Found match_found;
  if (!mass_match_overload_or_error(context, target_expression, arg_parameters, &match_found, &args_view.source_range)) {
    return 0;
  }
  context_temp_reset_to_mark(context, temp_mark);

  Value *overload = match_found.value;
  const Function_Info *info = match_found.info;

  // :ArgumentNormalization
  // This is required for a few reasons:
  //   1. `args_view` might point to temp memory
  //   2. Default args are substituted. Required for trampolines and intrinsics to work.
  //   3. Implicits casts for static args are done at compile time. Required for trampolines, macros
  //      and intrinsics to work. Also provides a nice optimization for runtime.
  u64 normalized_arg_count = dyn_array_length(info->parameters);
  Value_View normalized_args = {
    .values = 0,
    .length = u64_to_u32(normalized_arg_count),
    .source_range = args_view.source_range,
  };

  bool normalization_failed_to_cast_a_static_value = false;
  if (normalized_arg_count) {
    assert(args_view.length <= normalized_arg_count);
    normalized_args.values = allocator_allocate_array(context->allocator, Value *, normalized_arg_count);
    for (u64 i = 0; i < dyn_array_length(info->parameters); ++i) {
      const Resolved_Function_Parameter *param = dyn_array_get(info->parameters, i);
      if (i >= args_view.length) {
        normalized_args.values[i] = param->maybe_default_value;
        continue;
      }
      Value *source = value_view_get(&args_view, i);
      assert(source);
      // TODO @Speed might avoid this check if all args are exact match in Overload_Match_Found
      if (!same_type(param->descriptor, source->descriptor)) {
        // TODO instead of this code maybe it would be more robust (and performant?)
        //      to create compile-time casting functions. This would also allow to have
        //      user-defined casts. The only tricky part is that these casting functions
        //      must match arguments exactly (no implicit casts).
        const Descriptor *runtime_source_descriptor =
          deduce_runtime_descriptor_for_value(context, source, param->descriptor);
        if (!runtime_source_descriptor) {
          panic("We should not have matched an overload if we couldn't deduce the type");
        }
        bool can_static_cast = mass_value_is_static(source);
        if (param->descriptor->tag == Descriptor_Tag_Function_Instance) {
          can_static_cast = false;
        } else if (value_is_tuple(source)) {
          const Tuple *tuple = value_as_tuple(source);
          can_static_cast = mass_tuple_is_static(context, tuple);
        }
        if (can_static_cast) {
          void *memory = mass_allocate_bytes_from_descriptor(context, param->descriptor);
          Storage storage = storage_static_heap(memory, param->descriptor->bit_size);
          Value *adjusted_source = value_make(context, param->descriptor, storage, source->source_range);
          if (same_type(source->descriptor, &descriptor_i64)) {
            Mass_Cast_Lazy_Payload lazy_payload = { .target = param->descriptor, .expression = source };
            Expected_Result cast_result = mass_expected_result_exact(param->descriptor, storage);
            mass_cast_lazy_proc(context, 0/*no builder */, &cast_result, parser->scope, &source_range, &lazy_payload);
          } else {
            mass_assign_helper(context, 0/*no builder */, adjusted_source, source, parser->scope, &args_view.source_range);
          }
          if (mass_has_error(context)) return 0;
          source = adjusted_source;
        } else {
          normalization_failed_to_cast_a_static_value = true;
        }
      }
      normalized_args.values[i] = source;
    }
  }

  if (value_is_function_literal(overload)) {
    const Function_Literal *literal = value_as_function_literal(overload);
    if (value_is_intrinsic(literal->body)) {
      assert(!normalization_failed_to_cast_a_static_value);
      return mass_intrinsic_call(context, parser, literal->body, info->return_descriptor, normalized_args);
    }
  }

  if (info->flags & Function_Info_Flags_Compile_Time) {
    assert(!normalization_failed_to_cast_a_static_value);
    Value *result = mass_trampoline_call(context, parser, overload, info, normalized_args);
    if (mass_has_error(context)) return 0;
    return result;
  }

  Mass_Function_Call_Lazy_Payload *call_payload =
    allocator_allocate(context->allocator, Mass_Function_Call_Lazy_Payload);
  *call_payload = (Mass_Function_Call_Lazy_Payload){
    .overload = overload,
    .args = normalized_args,
    .info = info,
  };

  const Descriptor *lazy_descriptor = info->return_descriptor;
  Value *result = mass_make_lazy_value(
    context, parser, source_range, call_payload, lazy_descriptor, (Lazy_Value_Proc)call_function_overload
  );
  return result;
}

static Value *
token_handle_parsed_function_call(
  Mass_Context *context,
  Parser *parser,
  Value *target_token,
  Value *args_token,
  Source_Range source_range
) {
  Value *call_return_value = 0;
  Temp_Mark temp_mark = context_temp_mark(context);

  Value *target_expression = token_parse_single(context, parser, target_token);
  if (mass_has_error(context)) goto defer;

  Value_View args_view;
  if(value_is_group_paren(args_token)) {
    Value_View children = value_as_group_paren(args_token)->children;
    if (children.length) {
      Value *list = token_parse_expression(context, parser, children, &(u32){0}, 0);
      if (mass_has_error(context)) return 0;
      Array_Value_Ptr args = mass_parse_maybe_list_into_value_array(context, parser, list);
      if (mass_has_error(context)) return 0;
      args_view = value_view_from_value_array(args, &source_range);
    } else {
      args_view = (Value_View){.source_range = source_range};
    }
  } else if (args_token->descriptor == &descriptor_value_view) {
    if (!mass_value_ensure_static(context, args_token)) goto defer;
    args_view = *value_as_value_view(args_token);
  } else {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = source_range,
      .detailed_message = slice_literal("Expected a list of arguments in ()"),
    });
    goto defer;
  }

  call_return_value = token_handle_function_call(context, parser, target_expression, args_view, source_range);

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return call_return_value;
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
  Source_Range source_range;
} Mass_Arithmetic_Operator_Lazy_Payload;

static Value *
mass_handle_arithmetic_operation_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Arithmetic_Operator_Lazy_Payload *payload
) {
  const Descriptor *descriptor = mass_expected_result_descriptor(expected_result);

  const Source_Range result_range = payload->source_range;

  switch(payload->operator) {
    case Mass_Arithmetic_Operator_Add:
    case Mass_Arithmetic_Operator_Subtract: {
      // Try to reuse result_value if we can
      // TODO should be able to reuse memory and register operands
      Storage temp_lhs_storage = storage_register_temp(builder, descriptor->bit_size);
      Expected_Result expected_a = mass_expected_result_exact(descriptor, temp_lhs_storage);
      Value *temp_lhs = value_force(context, builder, scope, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_rhs_storage = storage_register_temp(builder, descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_rhs_storage);
      (void)value_force(context, builder, scope, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      const X64_Mnemonic *mnemonic = payload->operator == Mass_Arithmetic_Operator_Add ? x64_add : x64_sub;

      push_eagerly_encoded_assembly(
        &builder->code_block, result_range, scope,
        &(Instruction_Assembly){mnemonic, {temp_lhs_storage, temp_rhs_storage}}
      );
      storage_release_if_temporary(builder, &temp_rhs_storage);

      // temp_a is used as a result, so it is intentionally not released
      return mass_expected_result_ensure_value_or_temp(
        context, builder, scope, expected_result, temp_lhs
      );
    }
    case Mass_Arithmetic_Operator_Multiply: {
      // Save RDX as it will be used for the result overflow,
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Storage maybe_saved_rdx = {0};
      Storage reg_d = storage_register(Register_D, (Bits){64});
      if (
        expected_result->tag != Expected_Result_Tag_Exact ||
        !storage_is_register_index(&expected_result->Exact.storage, Register_D)
      ) {
        // We need both D and A for this operation so using either as temp will not work
        u64 disallowed_temp_registers = 0;
        register_bitset_set(&disallowed_temp_registers, Register_D);
        register_bitset_set(&disallowed_temp_registers, Register_A);

        Register temp_register = register_find_available(builder, disallowed_temp_registers);
        register_acquire(builder, temp_register);
        maybe_saved_rdx = storage_register(temp_register, (Bits){64});
        move_value(builder, scope, &result_range, &maybe_saved_rdx, &reg_d);
      }

      Storage temp_a_storage =
        storage_register(register_acquire(builder, Register_A), descriptor->bit_size);
      temp_a_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_a = mass_expected_result_exact(descriptor, temp_a_storage);
      Value *temp_a = value_force(context, builder, scope, &expected_a, payload->lhs);

      // TODO we do not acquire here because it is done by maybe_saved_rdx,
      //      but it is awkward that it is disconnected so need to think about
      Storage temp_b_storage = storage_register(Register_D, descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_b_storage);
      (void)value_force(context, builder, scope, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_instruction(&builder->code_block, (Instruction) {
        .tag = Instruction_Tag_Location,
        .scope = scope,
        .Location = { .source_range = result_range },
      });

      const X64_Mnemonic *mnemonic = descriptor_is_signed_integer(descriptor) ? x64_imul : x64_mul;
      push_eagerly_encoded_assembly(
        &builder->code_block, result_range, scope,
        &(Instruction_Assembly){mnemonic, {temp_b_storage}}
      );
      if (maybe_saved_rdx.tag == Storage_Tag_Register) {
        move_value(builder, scope, &result_range, &reg_d, &maybe_saved_rdx);
        register_release(builder, maybe_saved_rdx.Register.index);
      }

      // temp_a is used as a result, so it is intentionally not released
      return mass_expected_result_ensure_value_or_temp(
        context, builder, scope, expected_result, temp_a
      );
    }
    case Mass_Arithmetic_Operator_Divide:
    case Mass_Arithmetic_Operator_Remainder: {
      u64 bit_size = descriptor->bit_size.as_u64;

      // We need both D and A for this operation so using either as temp will not work
      u64 disallowed_temp_registers = 0;
      register_bitset_set(&disallowed_temp_registers, Register_D);
      register_bitset_set(&disallowed_temp_registers, Register_A);

      Storage temp_dividend_storage = storage_register(Register_A, descriptor->bit_size);
      register_acquire(builder, Register_A);
      temp_dividend_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_dividend = mass_expected_result_exact(descriptor, temp_dividend_storage);
      Value *temp_dividend = value_force(context, builder, scope, &expected_dividend, payload->lhs);

      Register temp_divisor_register = register_find_available(builder, disallowed_temp_registers);
      register_acquire(builder, temp_divisor_register);
      Storage temp_divisor_storage = storage_register(temp_divisor_register, descriptor->bit_size);
      temp_divisor_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_divisor = mass_expected_result_exact(descriptor, temp_divisor_storage);
      (void)value_force(context, builder, scope, &expected_divisor, payload->rhs);

      // Save RDX as it will be used for the remainder,
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Storage maybe_saved_rdx = {0};
      Storage reg_d = storage_register(Register_D, (Bits){64});
      if (register_bitset_get(builder->register_occupied_bitset.bits, Register_D)) {
        if (
          expected_result->tag != Expected_Result_Tag_Exact ||
          !storage_is_register_index(&expected_result->Exact.storage, Register_D)
        ) {
          Register temp_register = register_find_available(builder, disallowed_temp_registers);
          register_acquire(builder, temp_register);
          maybe_saved_rdx = storage_register(temp_register, (Bits){64});
          move_value(builder, scope, &result_range, &maybe_saved_rdx, &reg_d);
        }
      }

      push_instruction(&builder->code_block, (Instruction) {
        .tag = Instruction_Tag_Location,
        .scope = scope,
        .Location = { .source_range = result_range },
      });

      if (mass_has_error(context)) return 0;

      if (descriptor_is_signed_integer(descriptor)){
        const X64_Mnemonic *widen = 0;
        switch (bit_size) {
          case 64: widen = x64_cqo; break;
          case 32: widen = x64_cdq; break;
          case 16: widen = x64_cwd; break;
          case 8: widen = x64_cbw; break;
          default: assert(false); break;
        }
        assert(widen);
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, scope, &(Instruction_Assembly){widen}
        );
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, scope, &(Instruction_Assembly){x64_idiv, {temp_divisor_storage}}
        );
      } else {
        if (bit_size == 8) {
          Storage reg_ax = storage_register(Register_A, (Bits){16});
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, scope, &(Instruction_Assembly){x64_movzx, {reg_ax, temp_dividend_storage}}
          );
        } else {
          // We need to zero-extend A to D which means just clearing D register
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, scope, &(Instruction_Assembly){x64_xor, {reg_d, reg_d}}
          );
        }
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, scope, &(Instruction_Assembly){x64_asm_div, {temp_divisor_storage}}
        );
      }

      if (payload->operator == Mass_Arithmetic_Operator_Remainder) {
        if (bit_size == 8) {
          // :64bitMode8BitOperations
          // The encoder does not support access to AH, so we hard code byte of `mov AL, AH`
          // This is not optimal, but it should do for now.
          push_instruction(&builder->code_block, (Instruction) {
            .tag = Instruction_Tag_Bytes,
            .scope = scope,
            .Bytes = {.memory = {0x88, 0xe0}, .length = 2},
          });
        } else {
          Storage reg_d = storage_register(Register_D, descriptor->bit_size);
          move_value(builder, scope, &result_range, &temp_dividend_storage, &reg_d);
        }
      }

      storage_release_if_temporary(builder, &temp_divisor_storage);
      if (maybe_saved_rdx.tag == Storage_Tag_Register) {
        move_value(builder, scope, &result_range, &reg_d, &maybe_saved_rdx);
        register_release(builder, maybe_saved_rdx.Register.index);
      }

      return mass_expected_result_ensure_value_or_temp(
        context, builder, scope, expected_result, temp_dividend
      );
    }
    default: {
      panic("Internal error: Unexpected operator");
      return 0;
    }
  }
}

static Value *
mass_handle_arithmetic_operation(
  Mass_Context *context,
  Parser *parser,
  Value_View arguments,
  Mass_Arithmetic_Operator operator
) {
  Value *lhs = value_view_get(&arguments, 0);
  Value *rhs = value_view_get(&arguments, 1);

  if (mass_has_error(context)) return 0;

  const Descriptor *result_descriptor = lhs->descriptor;
  if (!descriptor_is_integer(lhs->descriptor)) {
    result_descriptor = rhs->descriptor;
    lhs = mass_cast_helper(context, parser, result_descriptor, lhs, lhs->source_range);
  }
  assert(descriptor_is_integer(result_descriptor));
  assert(same_type(lhs->descriptor, rhs->descriptor));

  Mass_Arithmetic_Operator_Lazy_Payload stack_lazy_payload =
    { .lhs = lhs, .rhs = rhs, .operator = operator, .source_range = arguments.source_range };
  Mass_Arithmetic_Operator_Lazy_Payload *lazy_payload =
    allocator_allocate(context->allocator, Mass_Arithmetic_Operator_Lazy_Payload);
  *lazy_payload = stack_lazy_payload;
  return mass_make_lazy_value(
    context, parser, arguments.source_range, lazy_payload, result_descriptor, (Lazy_Value_Proc)mass_handle_arithmetic_operation_lazy_proc
  );
}

static inline Value *mass_integer_add(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, parser, arguments, Mass_Arithmetic_Operator_Add);
}
static inline Value *mass_integer_subtract(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, parser, arguments, Mass_Arithmetic_Operator_Subtract);
}
static inline Value *mass_integer_multiply(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, parser, arguments, Mass_Arithmetic_Operator_Multiply);
}
static inline Value *mass_integer_divide(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, parser, arguments, Mass_Arithmetic_Operator_Divide);
}
static inline Value *mass_integer_remainder(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_arithmetic_operation(context, parser, arguments, Mass_Arithmetic_Operator_Remainder);
}

typedef struct {
  Compare_Type compare_type;
  Value *lhs;
  Value *rhs;
} Mass_Comparison_Operator_Lazy_Payload;

static Value *
mass_handle_integer_comparison_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Comparison_Operator_Lazy_Payload *payload
) {
  Compare_Type compare_type = payload->compare_type;
  const Descriptor *lhs_descriptor = payload->lhs->descriptor;
  const Descriptor *rhs_descriptor = payload->rhs->descriptor;
  assert(same_type(lhs_descriptor, rhs_descriptor));

  const Descriptor *descriptor = lhs_descriptor;
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

  // Try to reuse result_value if we can
  // TODO should also be able to reuse memory operands
  Storage temp_a_storage = storage_register_temp(builder, descriptor->bit_size);
  Expected_Result expected_a = mass_expected_result_exact(descriptor, temp_a_storage);
  (void)value_force(context, builder, scope, &expected_a, payload->lhs);

  // TODO This can be optimized in cases where one of the operands is an immediate
  Storage temp_b_storage = storage_register_temp(builder, descriptor->bit_size);
  Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_b_storage);
  (void)value_force(context, builder, scope, &expected_b, payload->rhs);

  if (mass_has_error(context)) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range, scope,
    &(Instruction_Assembly){x64_cmp, {temp_a_storage, temp_b_storage}}
  );

  Value *comparison_value = value_make(
    context, &descriptor__bool, storage_eflags(compare_type), *source_range
  );

  storage_release_if_temporary(builder, &temp_a_storage);
  storage_release_if_temporary(builder, &temp_b_storage);

  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, comparison_value
  );
}

static Value *
mass_handle_generic_comparison_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Comparison_Operator_Lazy_Payload *payload
) {
  Compare_Type compare_type = payload->compare_type;

  Value *lhs = payload->lhs;
  Value *rhs = payload->rhs;

  if (!same_type(lhs->descriptor, rhs->descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = lhs->descriptor, .actual = rhs->descriptor },
    });
    return 0;
  }

  bool negated;
  switch(compare_type) {
    case Compare_Type_Equal: {
      negated = false;
    } break;
    case Compare_Type_Not_Equal: {
      negated = true;
    } break;

    case Compare_Type_Unsigned_Below:
    case Compare_Type_Unsigned_Below_Equal:
    case Compare_Type_Unsigned_Above:
    case Compare_Type_Unsigned_Above_Equal:
    case Compare_Type_Signed_Less:
    case Compare_Type_Signed_Less_Equal:
    case Compare_Type_Signed_Greater:
    case Compare_Type_Signed_Greater_Equal:
    default: {
      assert(!"Unsupported comparison");
      negated = false;
    } break;
  }

  if (mass_value_is_static(lhs) && mass_value_is_static(rhs)) {
    const Storage *lhs_storage = &value_as_forced(lhs)->storage;
    const Storage *rhs_storage = &value_as_forced(rhs)->storage;
    bool equal = storage_static_equal(lhs->descriptor, lhs_storage, rhs->descriptor, rhs_storage);
    if (negated) equal = !equal;
    return value_make(context, &descriptor__bool, storage_immediate(&equal), *source_range);
  }

  Value *result = 0;
  switch(lhs->descriptor->tag) {
    // Two void or never values are always equal to each other
    case Descriptor_Tag_Void:
    case Descriptor_Tag_Never: {
      bool equal = true;
      result = value_make(context, &descriptor__bool, storage_immediate(&equal), *source_range);
    } break;
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Float:
    case Descriptor_Tag_Integer:
    case Descriptor_Tag_Function_Instance: {
      if (lhs->descriptor->bit_size.as_u64 > 64) {
        panic("TODO support larger than register compares");
      }

      // @CopyPaste from integer compares

      // Try to reuse result_value if we can
      // TODO should also be able to reuse memory operands
      Storage temp_a_storage = storage_register_temp(builder, lhs->descriptor->bit_size);
      Expected_Result expected_a = mass_expected_result_exact(lhs->descriptor, temp_a_storage);
      (void)value_force(context, builder, scope, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_b_storage = storage_register_temp(builder, rhs->descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(rhs->descriptor, temp_b_storage);
      (void)value_force(context, builder, scope, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range, scope,
        &(Instruction_Assembly){x64_cmp, {temp_a_storage, temp_b_storage}}
      );

      result = value_make(context, &descriptor__bool, storage_eflags(compare_type), *source_range);

      storage_release_if_temporary(builder, &temp_a_storage);
      storage_release_if_temporary(builder, &temp_b_storage);
    } break;
    case Descriptor_Tag_Fixed_Array: {
      panic("TODO figure out semantics and support comparing fixed size");
    } break;
    case Descriptor_Tag_Struct: {
      panic("TODO figure out semantics and support comparing structs");
    } break;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, result
  );
}

static inline Value *
mass_handle_comparison(
  Mass_Context *context,
  Parser *parser,
  Value_View arguments,
  Lazy_Value_Proc lazy_value_proc,
  Compare_Type compare_type
) {
  Value *lhs = value_view_get(&arguments, 0);
  Value *rhs = value_view_get(&arguments, 1);
  if (mass_has_error(context)) return 0;

  Mass_Comparison_Operator_Lazy_Payload stack_lazy_payload =
    { .lhs = lhs, .rhs = rhs, .compare_type = compare_type };
  Mass_Comparison_Operator_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_Comparison_Operator_Lazy_Payload);
  *payload = stack_lazy_payload;
  return mass_make_lazy_value(
    context, parser, arguments.source_range, payload, &descriptor__bool, lazy_value_proc
  );
}

static inline Value *mass_integer_less(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Less
  );
}
static inline Value *mass_integer_greater(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Greater
  );
}
static inline Value *mass_integer_less_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Less_Equal
  );
}
static inline Value *mass_integer_greater_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Greater_Equal
  );
}
static inline Value *mass_integer_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Equal
  );
}
static inline Value *mass_integer_not_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_integer_comparison_lazy_proc, Compare_Type_Not_Equal
  );
}

static inline Value *mass_generic_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_generic_comparison_lazy_proc, Compare_Type_Equal
  );
}
static inline Value *mass_generic_not_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, (Lazy_Value_Proc)mass_handle_generic_comparison_lazy_proc, Compare_Type_Not_Equal
  );
}

static Value *
mass_parse_type(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
  value = token_parse_single(context, parser, value);
  if (mass_has_error(context)) return 0;
  const Descriptor *descriptor = value_ensure_type(context, parser->scope, value, args.source_range);
  Type type = { descriptor };
  return value_make(context, &descriptor_type, storage_immediate(&type), args.source_range);
}

static Value *
mass_type_only_token_parse_expression(
  Mass_Context *context,
  Parser *parser,
  Value *value
) {
  Parser_Flags saved_flags = parser->flags;
  parser->flags |= Parser_Flags_Type_Only;
  value = token_parse_single(context, parser, value);
  parser->flags = saved_flags;
  if (mass_has_error(context)) return 0;

  return value;
}

static Value *
mass_type_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
  const Descriptor *descriptor = mass_type_only_token_parse_expression(context, parser, value)->descriptor;
  Type type = { descriptor };
  if (mass_has_error(context)) return 0;
  return value_make(context, &descriptor_type, storage_immediate(&type), args.source_range);
}

static Value *
mass_size_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
  const Descriptor *descriptor;
  Value *parsed_value = mass_type_only_token_parse_expression(context, parser, value);
  // When you are doing `size_of(u32)` you are most likely not interested in the size
  // of the `Type` type, but rather want to know the size of the actual type.
  // This makes it inconsistent with `type_of` but it is probably an OK tradeoff.
  if (value_is_type(parsed_value)) {
    descriptor = value_as_type(parsed_value)->descriptor;
  } else {
    descriptor = parsed_value->descriptor;
  }
  if (mass_has_error(context)) return 0;
  i64 literal = { .bits = descriptor_byte_size(descriptor) };
  return value_make(context, &descriptor_i64, storage_immediate(&literal), args.source_range);
}

static Value *
mass_pointer_to_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Value_Lazy_Payload *payload
) {
  Value *pointee = payload->value;
  const Descriptor *descriptor = pointee->descriptor;
  Expected_Result expected_pointee = expected_result_any(descriptor);
  Value *forced = value_force(context, builder, scope, &expected_pointee, pointee);
  if (mass_has_error(context)) return 0;
  Value *pointer_value = mass_value_from_expected_result(context, builder, expected_result, *source_range);
  const Storage *pointer_storage = &value_as_forced(pointer_value)->storage;
  const Storage *forced_storage = &value_as_forced(forced)->storage;
  mass_storage_load_address(builder, source_range, scope, pointer_storage, forced_storage);
  storage_release_if_temporary(builder, forced_storage);
  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, pointer_value
  );
}

// Static assert is implemented in the core as it is really handy to have
// when implementing meta features. Moving it to userland constantly
// creates weird circular dependencies of static_assert on itself.
static Value *
mass_static_assert(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  // TODO Resolve optional arguments before calling the intrinsic
  assert(args.length == 1 || args.length == 2);
  Value *condition_value = value_view_get(&args, 0);

  bool condition = *value_as__bool(condition_value);
  if (!condition) {
    Slice detailed_message;
    if (args.length == 2) {
      detailed_message = mass_slice_from_slice_like(value_view_get(&args, 1));
    } else {
      detailed_message = (Slice){0};
    }

    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_User_Defined,
      .source_range = args.source_range,
      .detailed_message = detailed_message,
      .User_Defined = {
        .name = slice_literal("Static Assert Failed"),
      },
    });
  }
  return mass_make_void(context, args.source_range);
}

static Value *
mass_pointer_to(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *pointee = value_view_get(&args, 0);
  const Descriptor *descriptor = descriptor_pointer_to(context->compilation, pointee->descriptor);
  if (mass_value_is_static(pointee)) {
    const void *source_memory = storage_static_memory_with_bit_size(
      &value_as_forced(pointee)->storage, pointee->descriptor->bit_size
    );
    Value *result = value_make(
      context, descriptor, storage_immediate(&source_memory), args.source_range
    );
    return result;
  }

  Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
  *payload = (Mass_Value_Lazy_Payload) {.value = pointee};
  return mass_make_lazy_value(
    context, parser, args.source_range, payload, descriptor, (Lazy_Value_Proc)mass_pointer_to_lazy_proc
  );
}

static Value *
mass_pointer_to_type(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 1);
  Value *type_value = value_view_get(&args_view, 0);
  const Descriptor *descriptor =
    value_ensure_type(context, parser->scope, type_value, args_view.source_range);
  if (mass_has_error(context)) return 0;
  Type pointer_type = { descriptor_pointer_to(context->compilation, descriptor) };
  Storage storage = storage_immediate(&pointer_type);
  return value_make(context, &descriptor_type, storage, args_view.source_range);
}

static Value *
mass_call(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs_value = value_view_get(&args_view, 0);
  Value *rhs_value = value_view_get(&args_view, 1);
  return token_handle_parsed_function_call(
    context, parser, lhs_value, rhs_value, args_view.source_range
  );
}

static inline Value *
mass_apply(
  Mass_Context *context,
  Parser *parser,
  Value_View operands
) {
  assert(operands.length == 2);
  Value *lhs_token = value_view_get(&operands, 0);
  Value *rhs_token = value_view_get(&operands, 1);
  Value *lhs_value = operands.values[0] = token_parse_single(context, parser, lhs_token);
  if (mass_has_error(context)) return 0;
  if (value_is_type(lhs_value)) {
    const Descriptor *descriptor = value_as_type(lhs_value)->descriptor;
    Value *rhs_value = token_parse_single(context, parser, rhs_token);
    return mass_cast_helper(context, parser, descriptor, rhs_value, operands.source_range);
  } else if (value_is_function_header(lhs_value)) {
    return mass_function_literal(context, parser, operands);
  } else if (value_is_group_paren(rhs_token)) {
    return mass_call(context, parser, operands);
  } else if (value_is_ast_block(rhs_token)) {
    const Symbol *alias = context->compilation->common_symbols.postfix_block;
    return mass_forward_call_to_alias(context, parser, operands, alias);
  }
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = operands.source_range,
    .detailed_message = "Unexpected token sequence"
  });
  return 0;
}

static Value *
mass_typed_symbol(
  Mass_Context *context,
  Parser *parser,
  Value_View operands
) {
  Value *lhs_value = value_view_get(&operands, 0);
  Value *rhs_value = token_parse_single(context, parser, value_view_get(&operands, 1));
  Source_Range source_range = operands.source_range;

  if (!value_is_symbol(lhs_value)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = source_range,
      .detailed_message = "operator : expects a symbol on the left hand side"
    });
    return 0;
  }

  const Descriptor *descriptor = value_ensure_type(context, parser->scope, rhs_value, source_range);
  Typed_Symbol *typed_symbol = allocator_allocate(context->allocator, Typed_Symbol);
  *typed_symbol = (Typed_Symbol) {
    .symbol = value_as_symbol(lhs_value),
    .descriptor = descriptor,
  };

  return value_make(context, &descriptor_typed_symbol, storage_static(typed_symbol), source_range);
}

typedef struct {
  const Descriptor *descriptor;
  Stack_Init_Mode init_mode;
} Mass_Variable_Definition_Lazy_Payload;

static Value *
mass_handle_variable_definition_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Variable_Definition_Lazy_Payload *payload
) {
  Storage storage = payload->descriptor->bit_size.as_u64
    ? reserve_stack_storage(builder, payload->descriptor->bit_size)
    : (Storage){.tag = Storage_Tag_Immediate, .bit_size = payload->descriptor->bit_size};
  Value *result = value_make(context, payload->descriptor, storage, *source_range);
  switch (payload->init_mode) {
    case Stack_Init_Mode_Zero: {
      mass_zero_storage(builder, scope, &storage, source_range);
    } break;
    case Stack_Init_Mode_Uninitialized: {
      // Nothing to do as we are OK with the garbage values
    } break;
  }
  return result;
}

static Value *
mass_handle_assignment_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Assignment *payload
) {
  const Descriptor *target_descriptor = payload->target->descriptor;
  Expected_Result expected_target = expected_result_any(target_descriptor);
  Value *target = value_force(context, builder, scope, &expected_target, payload->target);
  if (mass_has_error(context)) return 0;

  if (target->flags & Value_Flags_Constant) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Assignment_To_Constant,
      .source_range = *source_range,
    });
    return 0;
  }

  Value *source = payload->source;
  const Descriptor *deduced_source_descriptor =
    deduce_runtime_descriptor_for_value(context, source, target_descriptor);
  if (mass_has_error(context)) return 0;
  if (!deduced_source_descriptor) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = target_descriptor, .actual = source->descriptor },
    });
    return 0;
  }
  if (!types_equal(deduced_source_descriptor, source->descriptor, Brand_Comparison_Mode_One_Unbranded)) {
    if (!mass_value_is_static(source)) {
      panic("Give current deduction rules only static values should be able to implicitly cast");
    }
    if (same_type(source->descriptor, &descriptor_i64)) {
      Mass_Cast_Lazy_Payload lazy_payload = { .target = target_descriptor, .expression = source };
      Expected_Result cast_result =
        mass_expected_result_exact(target_descriptor, value_as_forced(target)->storage);
      mass_cast_lazy_proc(context, builder, &cast_result, scope, source_range, &lazy_payload);
    } else if (
      value_is_tuple(source) ||
      value_is_named_accessor(source) ||
      deduced_source_descriptor->tag == Descriptor_Tag_Function_Instance
    ) {
      // Casting handled in `mass_assign_helper`
      value_force_exact(context, builder, scope, target, payload->source);
    } else {
      panic("TODO");
    }
  } else {
    value_force_exact(context, builder, scope, target, payload->source);
  }
  storage_release_if_temporary(builder, &value_as_forced(target)->storage);
  if (mass_has_error(context)) return 0;

  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);
  if (!mass_descriptor_is_void(expected_descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = expected_descriptor, .actual = &descriptor_void },
    });
    return 0;
  }

  Value *void_value = mass_make_void(context, *source_range);
  return expected_result_validate(expected_result, void_value);
}

static Value *
mass_define_stack_value_from_typed_symbol(
  Mass_Context *context,
  Parser *parser,
  const Typed_Symbol *typed_symbol,
  Stack_Init_Mode init_mode,
  Source_Range source_range
) {
  Mass_Variable_Definition_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_Variable_Definition_Lazy_Payload);
  *payload = (Mass_Variable_Definition_Lazy_Payload){
    .descriptor = typed_symbol->descriptor,
    .init_mode = init_mode,
  };
  Value *defined = mass_make_lazy_value(
    context, parser, source_range, payload,
    typed_symbol->descriptor, (Lazy_Value_Proc)mass_handle_variable_definition_lazy_proc
  );

  // :ScopeForEachDeclaration
  // Every time a new value is defined we create a new scope.
  // This should help with a few of things:
  //   1. This should be slightly safer as it makes sure that statements above can not be using
  //      variables from anywhere below.
  //   2. Since we do not change the storage of a variable currently after it is created
  //      we can probably use this info for debugging.
  //   3. Allows for redeclaration of variables similar to Rust. It is unclear if that is good
  //      long term, but it is good for experimentation at the moment.
  parser->scope = scope_make_imperative(context->allocator, parser->scope, &(const Scope_Entry) {
    .value = defined,
    .name = typed_symbol->symbol->name,
    .epoch = parser->epoch,
    .source_range = source_range
  });

  return defined;
}

static Value *
mass_operator_assignment(
  Mass_Context *context,
  Parser *parser,
  Value_View operands
) {
  Value *target = token_parse_single(context, parser, value_view_get(&operands, 0));
  if (mass_has_error(context)) return 0;

  // It is important to check this before parsing the expression because we want to allow
  // redeclaration with the same name to happen while referring to the old variable
  if (value_is_typed_symbol(target)) {
    const Typed_Symbol *typed_symbol = value_as_typed_symbol(target);
    target = mass_define_stack_value_from_typed_symbol(
      context, parser, typed_symbol, Stack_Init_Mode_Uninitialized, target->source_range
    );
  }

  Value *source = token_parse_single(context, parser, value_view_get(&operands, 1));
  if (mass_has_error(context)) return 0;

  Assignment *assignment = allocator_allocate(context->allocator, Assignment);
  *assignment = (Assignment) {
    .target = target,
    .source = source,
  };
  return value_make(
    context, &descriptor_assignment, storage_static(assignment), operands.source_range
  );
}

static Value *
mass_eval(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 1);
  Value *body = value_view_get(&args_view, 0);
  if (value_is_group_paren(body) || value_is_ast_block(body)) {
    return compile_time_eval(context, parser, args_view);
  } else {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = args_view.source_range,
      .detailed_message = slice_literal("@ operator must be followed by an expression in () or {}"),
    });
    return 0;
  }
}

typedef struct {
  Value *struct_;
  const Struct_Field *field;
} Mass_Field_Access_Lazy_Payload;

static Value *
mass_handle_field_access_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Field_Access_Lazy_Payload *payload
) {
  const Struct_Field *field = payload->field;

  Expected_Result expected_struct = expected_result_any(payload->struct_->descriptor);
  Value *struct_ = value_force(context, builder, scope, &expected_struct, payload->struct_);
  if (mass_has_error(context)) return 0;
  bool source_is_constant = struct_->flags & Value_Flags_Constant;

  Storage struct_storage = value_as_forced(struct_)->storage;

  // Since storage_field_access reuses indirect memory storage of the struct
  // the release of memory will be based on the field value release and we need
  // to propagate the temporary flag correctly
  // TODO should this always copy the flags? Or maybe it should be mutating?
  Storage field_storage = storage_with_offset_and_bit_size(
    &struct_storage, u64_to_s32(field->offset), field->descriptor->bit_size
  );
  field_storage.flags = struct_storage.flags;

  Value *field_value = value_make(context, field->descriptor, field_storage, *source_range);

  if (source_is_constant) {
    field_value->flags |= Value_Flags_Constant;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, field_value
  );
}

typedef struct {
  Value *array;
  Value *index;
} Mass_Array_Access_Lazy_Payload;

static Value *
mass_handle_array_access_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Array_Access_Lazy_Payload *payload
) {
  Expected_Result expected_array = expected_result_any(payload->array->descriptor);
  Value *array = value_force(context, builder, scope, &expected_array, payload->array);
  Expected_Result expected_index = expected_result_any(payload->index->descriptor);
  Value *index = value_force(context, builder, scope, &expected_index, payload->index);

  if (mass_has_error(context)) return 0;

  Value *array_element_value;

  const Descriptor *item_descriptor;
  Storage array_storage;
  if(array->descriptor->tag == Descriptor_Tag_Fixed_Array) {
    item_descriptor = array->descriptor->Fixed_Array.item;
    array_storage = value_as_forced(array)->storage;
  } else {
    assert(array->descriptor->tag == Descriptor_Tag_Pointer_To);
    item_descriptor = array->descriptor->Pointer_To.descriptor;
    Value *dereferenced = value_indirect_from_pointer(context, builder, array, scope, source_range);
    array_storage = value_as_forced(dereferenced)->storage;
  }

  u64 item_byte_size = descriptor_byte_size(item_descriptor);

  Storage element_storage;
  if (value_is_i64(index)) {
    u64 index_bits = value_as_i64(index)->bits;
    s32 offset = u64_to_s32(index_bits * item_byte_size);
    element_storage = storage_with_offset_and_bit_size(&array_storage, offset, item_descriptor->bit_size);
    element_storage.flags = array_storage.flags;
  } else {
    Register base_register = register_acquire_temp(builder);
    Storage base_storage = storage_register(base_register, (Bits){64});

    // Move the index into the register
    move_value(builder, scope, source_range, &base_storage, &value_as_forced(index)->storage);

    // Multiplication by 1 byte is useless so checking it here
    if (item_descriptor->bit_size.as_u64 != 8) {
      u32 item_byte_size = u64_to_u32(descriptor_byte_size(item_descriptor));
      Register byte_size_register = register_acquire_temp(builder);
      Storage byte_size_storage = storage_register(byte_size_register, (Bits){64});

      // Multiply index by the item byte size
      Storage item_byte_size_storage = imm32(item_byte_size);
      move_value(builder, scope, source_range, &byte_size_storage, &item_byte_size_storage);

      // TODO @InstructionQuality this should use shifts for power-of-2 item byte sizes
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range, scope,
        &(Instruction_Assembly){x64_imul, {base_storage, byte_size_storage}}
      );
      register_release(builder, byte_size_register);
    }

    {
      // @InstructionQuality
      // TODO If the source does not have index, on X64 it should be possible to avoid
      //      using an extra register and put the index into SIB

      // Load previous address into a temp register
      Register address_register = register_acquire_temp(builder);
      Storage address_storage = storage_register(address_register, (Bits){64});
      mass_storage_load_address(builder, source_range, scope, &address_storage, &array_storage);

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range, scope,
        &(Instruction_Assembly){x64_add, {base_storage, address_storage}}
      );
      register_release(builder, address_register);
    }

    element_storage = storage_indirect(item_descriptor->bit_size, base_register);
    element_storage.flags |= Storage_Flags_Temporary;

    storage_release_if_temporary(builder, &array_storage);
  }

  array_element_value = value_make(context, item_descriptor, element_storage, array->source_range);

  if (array->flags & Value_Flags_Constant) {
    array_element_value->flags |= Value_Flags_Constant;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, scope, expected_result, array_element_value
  );
}

static Value *
mass_struct_field_access(
  Mass_Context *context,
  Parser *parser,
  Value *struct_,
  const Struct_Field *field,
  const Source_Range *source_range
) {
  Mass_Field_Access_Lazy_Payload stack_lazy_payload = {
    .struct_ = struct_,
    .field = field,
  };

  if (mass_value_is_static(struct_)) {
    Expected_Result expected_result = expected_result_any(field->descriptor);
    return mass_handle_field_access_lazy_proc(
      context, 0, &expected_result, parser->scope, source_range, &stack_lazy_payload
    );
  } else {
    Mass_Field_Access_Lazy_Payload *lazy_payload =
      allocator_allocate(context->allocator, Mass_Field_Access_Lazy_Payload);
    *lazy_payload = stack_lazy_payload;

    return mass_make_lazy_value(
      context, parser,
      *source_range,
      lazy_payload,
      field->descriptor,
      (Lazy_Value_Proc)mass_handle_field_access_lazy_proc
    );
  }
}
static Value *
mass_handle_dereference_operator_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_Value_Lazy_Payload *payload
) {
  Value *pointer = payload->value;
  // TODO value_indirect_from_pointer should probably take an expected_result
  Value *value = value_indirect_from_pointer(context, builder, pointer, scope, source_range);
  return mass_expected_result_ensure_value_or_temp(context, builder, scope, expected_result, value);
}

static Value *
mass_dereference(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *pointer = token_parse_single(context, parser, value_view_get(&args_view, 0));
  if (mass_has_error(context)) return 0;
  const Descriptor *descriptor = pointer->descriptor;
  if (descriptor->tag != Descriptor_Tag_Pointer_To) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = args_view.source_range,
      .Type_Mismatch = {
        .expected = descriptor_pointer_to(context->compilation, descriptor),
        .actual = descriptor,
      },
    });
    return 0;
  }
  if (mass_value_is_static(pointer)) {
    const Descriptor *pointee_descriptor = descriptor_as_pointer_to(descriptor)->descriptor;
    void *pointee_memory = *(void **)storage_static_memory_with_bit_size(
      &value_as_forced(pointer)->storage, descriptor->bit_size
    );
    Storage pointee_storage = storage_static_heap(pointee_memory, pointee_descriptor->bit_size);
    return value_make(context, pointee_descriptor, pointee_storage, args_view.source_range);
  }
  Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
  *payload = (Mass_Value_Lazy_Payload) {.value = pointer};
  return mass_make_lazy_value(
    context, parser,
    args_view.source_range,
    payload,
    descriptor->Pointer_To.descriptor,
    (Lazy_Value_Proc)mass_handle_dereference_operator_lazy_proc
  );
}

static bool
mass_constraint_pointer_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Pointer_To;
}

static bool
mass_constraint_integer_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Integer;
}

static bool
mass_constraint_float_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Float;
}

static bool
mass_constraint_fixed_array_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Fixed_Array;
}

static bool
mass_constraint_struct_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Struct;
}

static bool
mass_constraint_function_instance_type(
  Type type
) {
  return type.descriptor->tag == Descriptor_Tag_Function_Instance;
}

static Value *
mass_module_get_impl(
  Mass_Context *context,
  const Module *module,
  const Symbol *symbol,
  const Source_Range *source_range
) {
  Scope_Entry *entry = scope_lookup_shallow(module->exports.scope, symbol);
  if (!entry) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unknown_Field,
      .source_range = *source_range,
      .Unknown_Field = { .name = symbol->name, .type = &descriptor_module, },
    });
    return 0;
  }
  // FIXME when looking up values from a different file, need to either adjust source
  //       range or have some other mechanism to track it.
  return scope_entry_force_value(context, entry);
}

static Value *
mass_module_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(&args_view, 0);
  Value *rhs = value_view_get(&args_view, 1);

  if (!mass_value_ensure_static_of(context, lhs, &descriptor_module)) return 0;
  if (!mass_value_ensure_static_of(context, rhs, &descriptor_symbol)) return 0;

  const Symbol *symbol = value_as_symbol(rhs);
  const Module *module = value_as_module(lhs);
  return mass_module_get_impl(context, module, symbol, &args_view.source_range);
}

static Value *
mass_struct_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(&args_view, 0);
  Value *rhs = value_view_get(&args_view, 1);

  const Descriptor *unwrapped_lhs_descriptor = maybe_unwrap_pointer_descriptor(lhs->descriptor);
  assert(unwrapped_lhs_descriptor->tag == Descriptor_Tag_Struct);
  Array_Struct_Field fields = unwrapped_lhs_descriptor->Struct.fields;

  const Struct_Field *field;
  if (value_is_i64(rhs)) {
    u64 index = value_as_i64(rhs)->bits;
    field = 0;
    DYN_ARRAY_FOREACH(Struct_Field, it, fields) {
      // Only allow access to unnamed fields via numbers. This is a purely an empirical decision.
      // Allowing arbitrary fields just by index was very confusing in practice.
      // A user of the language can always redefine this choice or use a specialized fn.
      if (it->name.length) continue;
      if (index == 0) {
        field = it;
        break;
      }
      index -= 1;
    }
    if (!field) {
      Slice field_name = source_from_source_range(context->compilation, &rhs->source_range);
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Unknown_Field,
        .source_range = rhs->source_range,
        .Unknown_Field = { .name = field_name, .type = unwrapped_lhs_descriptor },
      });
      return 0;
    }
  } else {
    if (!mass_value_ensure_static_of(context, rhs, &descriptor_symbol)) return 0;
    const Symbol *symbol = value_as_symbol(rhs);

    if (!struct_find_field_by_name(unwrapped_lhs_descriptor, symbol->name, &field, &(u64){0})) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Unknown_Field,
        .source_range = rhs->source_range,
        .Unknown_Field = { .name = symbol->name, .type = unwrapped_lhs_descriptor },
      });
      return 0;
    }
  }
  return mass_struct_field_access(context, parser, lhs, field, &args_view.source_range);
}

static Value *
mass_array_like_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(&args_view, 0);
  Value *rhs = token_parse_single(context, parser, value_view_get(&args_view, 1));
  if (mass_has_error(context)) return 0;

  const Descriptor *item_descriptor;
  if (lhs->descriptor->tag == Descriptor_Tag_Fixed_Array) {
    item_descriptor = lhs->descriptor->Fixed_Array.item;
  } else if (lhs->descriptor->tag == Descriptor_Tag_Pointer_To) {
    item_descriptor = lhs->descriptor->Pointer_To.descriptor;
  } else {
    panic("UNREACHABLE");
    return 0;
  }

  if (rhs->descriptor != &descriptor_i64) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = args_view.source_range,
      .Type_Mismatch = { .expected = &descriptor_i64, .actual = rhs->descriptor },
    });
    return 0;
  }

  Mass_Array_Access_Lazy_Payload lazy_payload = { .array = lhs, .index = rhs };
  if (mass_value_is_static(lhs) && mass_value_is_static(rhs)) {
    Expected_Result expected_result = expected_result_any(item_descriptor);
    return mass_handle_array_access_lazy_proc(
      context, 0, &expected_result, parser->scope, &args_view.source_range, &lazy_payload
    );
  }
  Mass_Array_Access_Lazy_Payload *heap_payload =
    mass_allocate(context, Mass_Array_Access_Lazy_Payload);
  *heap_payload = lazy_payload;
  return mass_make_lazy_value(
    context, parser, args_view.source_range, heap_payload,
    item_descriptor, (Lazy_Value_Proc)mass_handle_array_access_lazy_proc
  );
}

static Value *
mass_get_from_descriptor_module(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *lhs = value_view_get(&args_view, 0);
  Value *rhs = value_view_get(&args_view, 1);
  const Descriptor *descriptor = value_as_type(lhs)->descriptor;
  if (!descriptor->own_module) {
    Slice field_name = source_from_source_range(context->compilation, &rhs->source_range);
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unknown_Field,
      .source_range = rhs->source_range,
      .Unknown_Field = { .name = field_name, .type = descriptor },
    });
    return 0;
  }
  if (!mass_value_ensure_static_of(context, rhs, &descriptor_symbol)) return 0;
  const Symbol *symbol = value_as_symbol(rhs);
  return mass_module_get_impl(context, descriptor->own_module, symbol, &args_view.source_range);
}

static Value *
mass_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *lhs = token_parse_single(context, parser, value_view_get(&args_view, 0));
  Value *rhs = value_view_get(&args_view, 1);

  Value_View parsed_args = {
    .values = (Value*[]){lhs, rhs},
    .length = 2,
    .source_range = args_view.source_range,
  };

  if (mass_has_error(context)) return 0;


  if (lhs->descriptor == &descriptor_module) {
    return mass_module_get(context, parser, parsed_args);
  }
  if (value_is_type(lhs)) {
    return mass_get_from_descriptor_module(context, parser, parsed_args);
  }

  const Symbol *symbol = context->compilation->common_symbols.get;
  return mass_forward_call_to_alias(context, parser, parsed_args, symbol);
}

static Value *
mass_comma(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *lhs = value_view_get(&args_view, 0);
  if (args_view.length == 1) {
    return lhs;
  }
  assert(args_view.length == 2);

  Value *rhs = value_view_get(&args_view, 1);
  const List_Node *previous;
  if (value_is_list_node(lhs)) {
    previous = value_as_list_node(lhs);
  } else {
    List_Node *node = mass_allocate(context, List_Node);
    *node = (List_Node) {
      .value = lhs,
      .maybe_previous = 0,
    };
    previous = node;
  }
  List_Node *result = mass_allocate(context, List_Node);
  *result = (List_Node){
    .value = rhs,
    .maybe_previous = previous,
  };

  return value_make(context, &descriptor_list_node, storage_static(result), args_view.source_range);
}

static void
token_dispatch_operator(
  Mass_Context *context,
  Parser *parser,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *stack_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  const Operator *operator = stack_entry->operator;

  u32 argument_count = operator->fixity == Operator_Fixity_Infix ? 2 : 1;

  if (dyn_array_length(*stack) < argument_count) {
    if (operator->flags & Operator_Flags_Optional_Rhs) {
      argument_count -= 1;
    }
    if (dyn_array_length(*stack) < argument_count) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = stack_entry->source_range,
        .detailed_message = slice_literal("Not enough arguments for operator"),
      });
      return;
    }
  }
  Value_View args_view;
  Source_Range source_range = stack_entry->source_range;
  u64 start_index = dyn_array_length(*stack) - argument_count;
  if (argument_count) {
    args_view = (Value_View) {
      .values = dyn_array_get(*stack, start_index),
      .length = argument_count,
      .source_range = source_range,
    };
  } else {
    args_view = (Value_View){.source_range = source_range};
  }
  Value *result_value = 0;
  switch(operator->tag) {
    case Operator_Tag_Alias: {
      if (argument_count) {
        Value **first_arg_pointer = dyn_array_get(*stack, start_index);
        *first_arg_pointer = token_parse_single(context, parser, *first_arg_pointer);

        if (argument_count == 2) {
          Value **second_arg_pointer = dyn_array_get(*stack, start_index + 1);
          *second_arg_pointer = token_parse_single(context, parser, *second_arg_pointer);
        }
      }

      result_value = mass_forward_call_to_alias(context, parser, args_view, operator->Alias.symbol);
    } break;
    case Operator_Tag_Intrinsic: {
      Value *target = operator->Intrinsic.body;
      const Descriptor *expected_descriptor = 0; // inferred
      result_value = mass_intrinsic_call(context, parser, target, expected_descriptor, args_view);
    } break;
  }

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
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Mass_If_Expression_Lazy_Payload *payload
) {
  Expected_Result expected_condition = expected_result_any(&descriptor__bool);
  Value *condition = value_force(context, builder, scope, &expected_condition, payload->condition);
  if (mass_has_error(context)) return 0;

  Program *program = context->program;
  Label *else_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("else"));

  encode_inverted_conditional_jump(builder, else_label, scope, &condition->source_range, condition);
  storage_release_if_temporary(builder, &value_as_forced(condition)->storage);

  Label *after_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("endif"));

  Value *result_value = value_force(context, builder, scope, expected_result, payload->then);
  if (mass_has_error(context)) return 0;

  bool empty_else_branch =
    payload->else_->descriptor == &descriptor_void && mass_value_is_static(payload->else_);
  if (empty_else_branch) {
    push_instruction(&builder->code_block, (Instruction) {
      .tag = Instruction_Tag_Label,
      .scope = scope,
      .Label.pointer = else_label,
    });
  } else {
    Source_Range after_then_body_source_range = payload->then->source_range;
    after_then_body_source_range.offsets.from = after_then_body_source_range.offsets.to;
    push_eagerly_encoded_assembly(
      &builder->code_block, after_then_body_source_range, scope,
      &(Instruction_Assembly){x64_jmp, {code_label32(after_label)}}
    );
    push_instruction(&builder->code_block, (Instruction) {
      .tag = Instruction_Tag_Label,
      .scope = scope,
      .Label.pointer = else_label,
    });
    if (payload->else_->descriptor->tag == Descriptor_Tag_Never) {
      Expected_Result never_result = expected_result_any(&descriptor_never);
      (void)value_force(context, builder, scope, &never_result, payload->else_);
    } else {
      value_force_exact(context, builder, scope, result_value, payload->else_);
    }
  }

  if (mass_has_error(context)) return 0;

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .scope = scope,
    .Label.pointer = after_label,
  });

  return result_value;
}

static Value *
token_parse_if_expression(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *matched_length,
  const Symbol *end_symbol
) {
  u32 peek_index = 0;
  Value *keyword = value_view_next(&view, &peek_index);
  assert(value_as_symbol(keyword) == context->compilation->common_symbols._if);

  Value *value_condition;
  {
    Value_View condition_view = value_view_slice(&view, peek_index, view.length);
    u32 condition_length = 0;
    value_condition = token_parse_expression(
      context, parser, condition_view, &condition_length, context->compilation->common_symbols.then
    );
    peek_index += condition_length;
    if (mass_has_error(context)) return 0;
  }

  Value *value_then;
  {
    Value_View then_view = value_view_slice(&view, peek_index, view.length);
    u32 then_length = 0;
    value_then = token_parse_expression(
      context, parser, then_view, &then_length, context->compilation->common_symbols._else
    );
    peek_index += then_length;
    if (mass_has_error(context)) return 0;
  }

  Value *value_else;
  {
    Value_View else_view = value_view_slice(&view, peek_index, view.length);
    u32 else_length = else_view.length;
    if (else_length) {
      value_else = token_parse_expression(context, parser, else_view, &else_length, end_symbol);
    } else {
      value_else = mass_make_void(context, else_view.source_range);
    }
    peek_index += else_length;
    if (mass_has_error(context)) return 0;
  }

  *matched_length = peek_index;

  if (mass_value_is_static(value_condition)) {
    if (!mass_value_ensure_static_of(context, value_condition, &descriptor__bool)) {
      return 0;
    }
    bool condition = *value_as__bool(value_condition);
    return condition ? value_then : value_else;
  }

  const Descriptor *result_descriptor = value_then->descriptor->tag == Descriptor_Tag_Never
    ? value_else->descriptor
    : value_then->descriptor;

  Mass_If_Expression_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_If_Expression_Lazy_Payload);
  *payload = (Mass_If_Expression_Lazy_Payload){
    .condition = value_condition,
    .then = value_then,
    .else_ = value_else,
  };

  return mass_make_lazy_value(
    context, parser, keyword->source_range, payload, result_descriptor, (Lazy_Value_Proc)mass_handle_if_expression_lazy_proc
  );
}

static Function_Literal *
mass_make_fake_function_literal(
  Mass_Context *context,
  Parser *parser,
  Value *body,
  const Descriptor *returns,
  const Source_Range *source_range
) {
  Scope *function_scope = scope_make_declarative(context->allocator, parser->scope);

  Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
  *literal = (Function_Literal){
    .header = {
      .flags = Function_Header_Flags_None,
      .parameters = (Array_Function_Parameter){&dyn_array_zero_items},
      .returns = function_return_exact(returns, *source_range),
    },
    .overload_lock_count = allocator_make(context->allocator, u64, 0),
    .body = body,
    .own_scope = function_scope,
  };
  return literal;
}

static Value *
mass_intrinsic(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 1);

  Value *body = value_view_get(&args_view, 0);
  if (!mass_value_ensure_static_of(context, body, &descriptor_ast_block)) return 0;
  const Source_Range *source_range = &args_view.source_range;

  Function_Literal *literal = mass_make_fake_function_literal(
    context, parser, body, &descriptor_value_pointer, source_range
  );

  // @Volatile :IntrinsicFunctionSignature
  // These arguments must match how we call it.
  literal->header.parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = 2
  );
  dyn_array_push(literal->header.parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("context")),
    .descriptor = &descriptor_mass_context_pointer,
    .source_range = *source_range,
  });
  dyn_array_push(literal->header.parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("parser")),
    .descriptor = &descriptor_parser_pointer,
    .source_range = *source_range,
  });
  dyn_array_push(literal->header.parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("arguments")),
    .descriptor = &descriptor_value_view,
    .source_range = *source_range,
  });
  literal->header.flags |= Function_Header_Flags_Intrinsic;

  return value_make(context, &descriptor_function_literal, storage_static(literal), *source_range);
}

static Array_Function_Parameter
mass_parse_function_parameters(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Parser arg_parser = *parser;
  arg_parser.scope = scope_make_declarative(context->allocator, parser->scope);
  arg_parser.epoch = get_new_epoch();

  Array_Function_Parameter result = (Array_Function_Parameter){&dyn_array_zero_items};
  if (args_view.length == 0) return result;

  Temp_Mark temp_mark = context_temp_mark(context);

  Array_Function_Parameter temp_params = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->temp_allocator,
    .capacity = 32,
  );

  const Symbol *comma = context->compilation->common_symbols.operator_comma;

  u32 match_length;
  bool previous_argument_has_default_value = false;
  for (Value_View rest = args_view; rest.length; rest = value_view_rest(&rest, match_length)) {
    match_length = 0;
    Value_View param_view = value_view_match_till_symbol(rest, &match_length, comma);
    assert(match_length);
    Function_Parameter param = mass_parse_single_function_parameter(context, &arg_parser, param_view);
    if (mass_has_error(context)) goto defer;
    dyn_array_push(temp_params, param);
    if (previous_argument_has_default_value) {
      if (!param.maybe_default_value) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Non_Trailing_Default_Argument,
          .source_range = param.source_range,
        });
        goto defer;
      }
    } else {
      previous_argument_has_default_value = !!param.maybe_default_value;
    }
  }
  dyn_array_copy_from_temp(Array_Function_Parameter, context, &result, temp_params);

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return result;
}

static inline Value *
mass_make_function_literal(
  Mass_Context *context,
  Parser *parser,
  const Function_Header *header,
  Value *body_value,
  Source_Range source_range
) {
  Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
  *literal = (Function_Literal){
    .header = *header,
    .body = body_value,
    .own_scope = parser->scope,
    .overload_lock_count = allocator_make(context->allocator, u64, 0),
  };
  if (value_is_intrinsic(body_value)) literal->header.flags |= Function_Header_Flags_Intrinsic;
  return value_make(context, &descriptor_function_literal, storage_static(literal), source_range);
}

static Value *
mass_function_literal(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *header_value = token_parse_single(context, parser, value_view_get(&args_view, 0));
  if (mass_has_error(context)) return 0;
  Value *body_value = value_view_get(&args_view, 1);
  if (!value_is_ast_block(body_value)) {
    body_value = token_parse_single(context, parser, body_value);
  }
  if (!mass_value_ensure_static_of(context, header_value, &descriptor_function_header)) return 0;
  const Function_Header *header = value_as_function_header(header_value);
  return mass_make_function_literal(context, parser, header, body_value, args_view.source_range);
}

static Value *
token_parse_function_literal(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *matched_length,
  const Symbol *end_symbol
) {
  u32 peek_index = 0;

  Value *keyword = value_view_next(&view, &peek_index);
  const Symbol *keyword_symbol = value_as_symbol(keyword);
  assert(keyword_symbol == context->compilation->common_symbols.fn);

  Value *args = value_view_next(&view, &peek_index);
  if (!value_is_group_paren(args)) {
    context_parse_error(context, parser, view, peek_index);
    return 0;
  }

  Value *arrow = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator_arrow
  );
  bool is_compile_time = false;
  if (!arrow) {
    arrow = value_view_maybe_match_cached_symbol(
      view, &peek_index, context->compilation->common_symbols.operator_fat_arrow
    );
    if (arrow) is_compile_time = true;
  }

  Function_Return returns;
  if (!arrow) {
    context_parse_error(context, parser, view, peek_index);
    return 0;
  }

  Value *token = value_view_next(&view, &peek_index);
  if (!token) {
    context_parse_error(context, parser, view, peek_index);
    return 0;
  }
  Source_Range return_range = token->source_range;
  if (value_is_group_paren(token)) {
    Value_View return_types_view = value_as_group_paren(token)->children;
    if (return_types_view.length == 0) {
      returns = function_return_exact(&descriptor_void, return_range);
    } else {
      returns = function_return_generic(return_types_view, return_range);
    }
  } else if (
    value_is_symbol(token) && value_as_symbol(token) == context->compilation->common_symbols._
  ) {
    Source_Range return_range = value_view_slice(&view, peek_index, peek_index).source_range;
    returns = function_return_inferred(return_range);
  } else {
    context_parse_error(context, parser, view, peek_index);
    return 0;
  }

  Value_View args_view = value_as_group_paren(args)->children;
  Array_Function_Parameter parameters = mass_parse_function_parameters(context, parser, args_view);
  if (mass_has_error(context)) return 0;

  Value *body_value = value_view_peek(&view, peek_index);
  if (body_value) {
    if (value_is_ast_block(body_value)) {
      peek_index += 1;
    } else {
      body_value = 0;
    }
  }
  if (!body_value) {
    Value_View rest = value_view_match_till_symbol(view, &peek_index, end_symbol);
    if (rest.length) body_value = token_parse_expression(context, parser, rest, &(u32){0}, 0);
  }
  if (mass_has_error(context)) return 0;

  Function_Header header = {
    .flags = Function_Header_Flags_None,
    .parameters = parameters,
    .returns = returns,
  };
  DYN_ARRAY_FOREACH(Function_Parameter, param, parameters) {
    if (param->tag == Function_Parameter_Tag_Generic) {
      header.generic_parameter_count += 1;
      break;
    }
  }

  if (is_compile_time) header.flags |= Function_Header_Flags_Compile_Time;

  *matched_length = peek_index;

  if (!body_value) {
    Function_Header *returned_header = mass_allocate(context, Function_Header);
    *returned_header = header;
    Storage fn_storage = storage_static(returned_header);
    return value_make(context, &descriptor_function_header, fn_storage, view.source_range);
  }

  if (!mass_value_ensure_static(context, body_value)) return 0;
  return mass_make_function_literal(context, parser, &header, body_value, view.source_range);
}

static inline void
token_handle_operator(
  Mass_Context *context,
  Parser *parser,
  Array_Value_Ptr *stack,
  Array_Operator_Stack_Entry *operator_stack,
  const Operator *operator,
  Source_Range source_range
) {
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
    token_dispatch_operator(context, parser, stack, last_operator);
  }
  dyn_array_push(*operator_stack, (Operator_Stack_Entry) {
    .source_range = source_range,
    .operator = operator,
  });
}

static Value *
token_parse_expression(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *out_match_length,
  const Symbol *end_symbol
) {
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

  bool is_value_expected = false;
  u32 matched_length = view.length;

  for (u32 i = 0; ; ++i) {
    if (i >= view.length) break;

    Value *value = value_view_get(&view, i);

    if (value_is_symbol(value)) {
      const Symbol *symbol = value_as_symbol(value);
      if (symbol == end_symbol) {
        matched_length = i + 1;
        goto drain;
      }

      Operator_Fixity fixity_mask = is_value_expected
        ? Operator_Fixity_Infix | Operator_Fixity_Postfix
        : Operator_Fixity_Prefix;

      if (
        symbol == context->compilation->common_symbols.fn ||
        symbol == context->compilation->common_symbols._while ||
        symbol == context->compilation->common_symbols._if
      ) {
        Value_View rest = value_view_rest(&view, i);
        u32 match_length = 0;
        if (symbol == context->compilation->common_symbols._if) {
          value = token_parse_if_expression(context, parser, rest, &match_length, end_symbol);
        } else if (symbol == context->compilation->common_symbols._while) {
          value = token_parse_while(context, parser, rest, &match_length, end_symbol);
        } else {
          value = token_parse_function_literal(context, parser, rest, &match_length, end_symbol);
        }
        if (mass_has_error(context)) goto defer;
        if (match_length) {
          i += match_length; // Skip over the matched slice
          goto maybe_apply;
        }
      }

      const Operator *maybe_operator = scope_lookup_operator(context, parser->scope, symbol, fixity_mask);
      if (maybe_operator) {
        token_handle_operator(context, parser, &value_stack, &operator_stack, maybe_operator, value->source_range);
        if (mass_has_error(context)) goto defer;
        is_value_expected = (maybe_operator->fixity == Operator_Fixity_Postfix);
        continue;
      }
    }

    maybe_apply:
    if (is_value_expected) {
      const Operator *empty_space_operator = &context->compilation->apply_operator;
      token_handle_operator(context, parser, &value_stack, &operator_stack, empty_space_operator, value->source_range);
      if (mass_has_error(context)) goto defer;
    }
    dyn_array_push(value_stack, value);
    is_value_expected = true;
  }

  drain:
  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_operator(context, parser, &value_stack, entry);
  }

  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(value_stack) == 1) {
      result = *dyn_array_last(value_stack);
      result = token_parse_single(context, parser, result);
    } else {
      mass_error(context, (Mass_Error) {
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

typedef struct {
  Array_Value_Ptr statements;
} Mass_Block_Lazy_Payload;

static Value *
mass_handle_block_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_block_result,
  const Scope *scope,
  const Source_Range *block_source_range,
  const Mass_Block_Lazy_Payload *payload
) {
  Array_Value_Ptr lazy_statements = payload->statements;
  u64 statement_count = dyn_array_length(lazy_statements);
  assert(statement_count);
  Value *result_value = 0;
  Expected_Result expected_void = mass_expected_result_exact(&descriptor_void, imm0);
  for (u64 i = 0; i < statement_count; ++i) {
    if (mass_has_error(context)) return 0;
    Register_Bitset registers_before = (Register_Bitset){0};
    if (builder) registers_before = builder->register_occupied_bitset;
    Value *lazy_statement = *dyn_array_get(lazy_statements, i);
    Source_Range debug_source_range = lazy_statement->source_range;
    Slice debug_source = source_from_source_range(context->compilation, &debug_source_range);
    // This is an easy way to break on the statement based on source text
    if (slice_starts_with(debug_source, slice_literal("")) && false) {
      printf("%"PRIslice"\n", SLICE_EXPAND_PRINTF(debug_source));
    }
    bool is_last_statement = i == statement_count - 1;
    const Expected_Result *expected_result =
      is_last_statement ? expected_block_result : &expected_void;
    result_value = value_force(context, builder, scope, expected_result, lazy_statement);
    if (mass_has_error(context)) return 0;
    // We do not do cross-statement register allocation so can check that there
    // are no stray registers retained across statement boundaries except when a block
    // returns a flexible result from a function call in the last statement.
    if (!builder) continue;
    Register_Bitset registers_after = builder->register_occupied_bitset;
    if (is_last_statement && expected_result->tag == Expected_Result_Tag_Flexible) {
      const Storage *result_storage = &value_as_forced(result_value)->storage;
      if (result_storage->tag == Storage_Tag_Register) {
        Register result_register = result_storage->Register.index;
        register_bitset_unset(&registers_after.bits, result_register);
      }
    }
    if(registers_before.bits == registers_after.bits) continue;
    for (s32 reg_index = Register_R15; reg_index >= Register_A; --reg_index) {
      bool before = register_bitset_get(registers_before.bits, reg_index);
      bool after = register_bitset_get(registers_after.bits, reg_index);
      if (before != after) {
        if (after) {
          printf("Unreleased %s\n", register_name(reg_index));
        } else {
          printf("Falsely released %s\n", register_name(reg_index));
        }
      }
    }
    panic("Found unreleased registers");
  }
  return result_value;
}

static Value *
mass_handle_explicit_return_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Scope *scope,
  const Source_Range *source_range,
  const Ast_Return *ast_return
) {
  Value *parse_result = ast_return->value;
  mass_assign_helper(context, builder, &builder->return_value, parse_result, scope, source_range);
  if (mass_has_error(context)) return 0;
  Storage return_label = code_label32(builder->code_block.end_label);

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range, scope,
    &(Instruction_Assembly) {x64_jmp, {return_label}}
  );

  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);
  Value *result_value;
  if (same_type(expected_descriptor, &descriptor_void)) {
    result_value = mass_make_void(context, *source_range);
  } else {
    result_value = mass_make_never(context, *source_range);
  }
  return expected_result_validate(expected_result, result_value);
}

static Value *
mass_return(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  Ast_Return *ast_return = mass_allocate(context, Ast_Return);
  if (args.length == 0) {
    *ast_return = (Ast_Return) {.value = mass_make_void(context, args.source_range)};
  } else if (args.length == 1) {
    *ast_return = (Ast_Return) {.value = token_parse_single(context, parser, value_view_get(&args, 0))};
  } else {
    panic("UNREACHABLE");
    return 0;
  }
  if (mass_has_error(context)) return 0;
  if (!*parser->return_descriptor_pointer) {
    *parser->return_descriptor_pointer = ast_return->value->descriptor;
  }

  return value_make(context, &descriptor_ast_return, storage_static(ast_return), args.source_range);
}

static Value *
token_parse_block_statements(
  Mass_Context *context,
  Parser *parser,
  const Ast_Statement *statement,
  const Source_Range *source_range
) {
  if (!statement) {
    return mass_make_void(context, *source_range);
  }
  Value *block_result = 0;
  Scope *block_declarative_scope = parser->scope;
  assert(block_declarative_scope->tag == Scope_Tag_Declarative);

  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_Ptr temp_lazy_statements = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = 128,
  );

  bool last_statement_was_return = false;
  Value *last_parse_result = 0;
  for (; statement; statement = statement->next) {
    if (mass_has_error(context)) goto defer;
    // :IgnoreStaticValueStatements
    if (last_parse_result) {
      if (mass_value_is_static(last_parse_result)) {
        // Static values that are result of the statements are useless and ignored.
        // It might make sense to warn here, but might be tough to distinguish
        // cases where this was generated as part of optimization (constant folding,
        // if branch pruning) vs when it is a user error.
      } else {
        dyn_array_push(temp_lazy_statements, last_parse_result);
      }
    }
    Source_Range statement_range = statement->children.source_range;

    if (last_statement_was_return) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Unreachable_Statement,
        .source_range = statement_range,
      });
      return 0;
    }

    if (token_parse_constant_definitions(context, parser, statement->children)) continue;
    if (mass_has_error(context)) goto defer;

    u32 match_length = 0;
    Value *parse_result = token_parse_expression(context, parser, statement->children, &match_length, 0);
    if (mass_has_error(context)) goto defer;

    if (match_length != statement->children.length) {
      Value_View remainder = value_view_rest(&statement->children, match_length);
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = remainder.source_range,
      });
      goto defer;
    }

    if (mass_value_is_static(parse_result)) {
      if (parse_result->descriptor == &descriptor_ast_using) {
        const Module *module = value_as_ast_using(parse_result)->module;
        mass_copy_scope_exports(block_declarative_scope, module->exports.scope);
        continue;
      } else if (parse_result->descriptor == &descriptor_ast_return) {
        last_statement_was_return = true;
        const Ast_Return *ast_return = value_as_ast_return(parse_result);
        parse_result = mass_make_lazy_value(
          context, parser, parse_result->source_range, ast_return,
          &descriptor_never, (Lazy_Value_Proc)mass_handle_explicit_return_lazy_proc
        );
      } else if (parse_result->descriptor == &descriptor_assignment) {
        parse_result = mass_make_lazy_value(
          context, parser, parse_result->source_range, value_as_assignment(parse_result),
          &descriptor_void, (Lazy_Value_Proc)mass_handle_assignment_lazy_proc
        );
      } else if (parse_result->descriptor == &descriptor_typed_symbol) {
        parse_result = mass_define_stack_value_from_typed_symbol(
          context, parser, value_as_typed_symbol(parse_result), Stack_Init_Mode_Zero, parse_result->source_range
        );
      } else if (parse_result->descriptor == &descriptor_mass_while) {
        parse_result = mass_make_lazy_value(
          context, parser, parse_result->source_range, value_as_mass_while(parse_result),
          &descriptor_void, (Lazy_Value_Proc)mass_handle_while_lazy_proc
        );
      } else if (parse_result->descriptor == &descriptor_module_exports) {
        if (!(parser->flags & Parser_Flags_Global)) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Parse,
            .source_range = statement_range,
            .detailed_message = slice_literal("Export declarations are only supported at top level"),
          });
          goto defer;
        }
        if (parser->module->exports.tag != Module_Exports_Tag_Not_Specified) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Parse,
            .source_range = statement_range,
            .detailed_message = slice_literal("A module can not have multiple exports statements. Original declaration at:"),
            .other_source_range = parser->module->exports.source_range,
          });
          goto defer;
        }
        const Module_Exports *exports = value_as_module_exports(parse_result);
        parser->module->exports = *exports;
        continue;
      }
    }

    // :IgnoreStaticValueStatements
    last_parse_result = parse_result;
  }

  // :IgnoreStaticValueStatements
  if (last_parse_result) {
    dyn_array_push(temp_lazy_statements, last_parse_result);
  }

  if (mass_has_error(context)) goto defer;

  u64 statement_count = dyn_array_length(temp_lazy_statements);
  if (statement_count) {
    Value *last_result = *dyn_array_last(temp_lazy_statements);
    const Descriptor *last_descriptor = last_result->descriptor;
    if (statement_count == 1) {
      block_result = last_result;
    } else {
      Mass_Block_Lazy_Payload *payload = mass_allocate(context, Mass_Block_Lazy_Payload);
      dyn_array_copy_from_temp(Array_Value_Ptr, context, &payload->statements, temp_lazy_statements);

      block_result = mass_make_lazy_value(
        context, parser, last_result->source_range, payload, last_descriptor, (Lazy_Value_Proc)mass_handle_block_lazy_proc
      );
    }
  } else {
    block_result = mass_make_void(context, *source_range);
  }

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return block_result;
}

static inline Value *
token_parse_block(
  Mass_Context *context,
  Parser *parser,
  const Ast_Block *group,
  const Source_Range *source_range
) {
  Parser block_parser = *parser;
  block_parser.scope = scope_make_declarative(context->allocator, parser->scope);
  return token_parse_block_statements(context, &block_parser, group->first_statement, source_range);
}

static Value *
mass_using(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *module_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (mass_has_error(context)) return 0;
  if (!mass_value_ensure_static_of(context, module_value, &descriptor_module)) return 0;
  const Module *module = value_as_module(module_value);
  Ast_Using *using = mass_allocate(context, Ast_Using);
  *using = (Ast_Using) { .module = module };
  return value_make(context, &descriptor_ast_using, storage_immediate(using), args.source_range);
}

static Value *
token_define_global_variable(
  Mass_Context *context,
  Parser *parser,
  Value *symbol,
  Value *value
) {
  const Descriptor *descriptor = deduce_runtime_descriptor_for_value(context, value, 0);
  if (!descriptor) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_No_Runtime_Use,
      .source_range = value->source_range,
    });
    return 0;
  }
  Value *global_value;
  if (storage_is_label(&value_as_forced(value)->storage)) {
    global_value = value;
  } else {
    Section *section = &context->program->memory.rw_data;
    u64 byte_size = descriptor_byte_size(descriptor);
    u64 alignment = descriptor_byte_alignment(descriptor);

    Label *label = allocate_section_memory(
      context->allocator, context->program, section, byte_size, alignment
    );
    Storage global_storage = data_label32(label, descriptor->bit_size);
    global_value = value_make(context, descriptor, global_storage, value->source_range);

    if (mass_value_ensure_static(context, value)) {
      mass_assign_helper(context, 0, global_value, value, parser->scope, &value->source_range);
    }
    if (mass_has_error(context)) return 0;
  }

  scope_define_value(parser->scope, VALUE_STATIC_EPOCH, symbol->source_range, value_as_symbol(symbol), global_value);
  return mass_make_void(context, symbol->source_range);
}

static Value *
token_define_local_variable(
  Mass_Context *context,
  Parser *parser,
  Value *symbol,
  Value *value
) {
  const Descriptor *variable_descriptor = deduce_runtime_descriptor_for_value(context, value, 0);
  if (mass_has_error(context)) return 0;
  if (!variable_descriptor) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_No_Runtime_Use,
      .source_range = value->source_range,
    });
    return 0;
  }

  Mass_Variable_Definition_Lazy_Payload *variable_payload =
    mass_allocate(context, Mass_Variable_Definition_Lazy_Payload);
  *variable_payload = (Mass_Variable_Definition_Lazy_Payload){
    .descriptor = variable_descriptor,
    .init_mode = Stack_Init_Mode_Uninitialized,
  };

  Value *variable_value = mass_make_lazy_value(
    context, parser, symbol->source_range, variable_payload, variable_descriptor,
    (Lazy_Value_Proc)mass_handle_variable_definition_lazy_proc
  );

  const Source_Range *source_range = &symbol->source_range;

  // :ScopeForEachDeclaration
  parser->scope = scope_make_imperative(context->allocator, parser->scope, &(const Scope_Entry) {
    .value = variable_value,
    .name = value_as_symbol(symbol)->name,
    .epoch = parser->epoch,
    .source_range = *source_range
  });

  Assignment *assignment = mass_allocate(context, Assignment);
  *assignment = (Assignment) {
    .target = variable_value,
    .source = value,
  };
  return value_make(context, &descriptor_assignment, storage_static(assignment), *source_range);
}

static Value *
mass_define_inferred(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 2);
  Value *lhs = value_view_get(&args, 0);
  if (!mass_value_ensure_static_of(context, lhs, &descriptor_symbol)) return 0;

  Value *rhs = token_parse_single(context, parser, value_view_get(&args, 1));
  if (mass_has_error(context)) return 0;

  if (parser->flags & Parser_Flags_Global) {
    return token_define_global_variable(context, parser, lhs, rhs);
  } else {
    return token_define_local_variable(context, parser, lhs, rhs);
  }
}

static void
scope_define_enum(
  Compilation *compilation,
  Scope *scope,
  Source_Range source_range,
  Slice enum_name,
  Value *enum_type_value,
  C_Enum_Item *items,
  u64 item_count
) {
  const Allocator *allocator = compilation->allocator;
  Scope *enum_scope = scope_make_declarative(allocator, 0);
  const Descriptor *enum_descriptor = value_as_type(enum_type_value)->descriptor;
  for (u64 i = 0; i < item_count; ++i) {
    C_Enum_Item *it = &items[i];
    Value *item_value = value_init(
      allocator_allocate(allocator, Value),
      enum_descriptor, storage_immediate(&it->value), source_range
    );
    const Symbol *it_symbol = mass_ensure_symbol(compilation, it->name);
    scope_define_value(enum_scope, VALUE_STATIC_EPOCH, source_range, it_symbol, item_value);
  }

  Module *enum_module = allocator_allocate(allocator, Module);
  *enum_module = (Module) {
    .source_range = source_range,
    .own_scope = enum_scope,
    .exports = {
      .tag = Module_Exports_Tag_All,
      .scope = enum_scope,
    },
  };
  ((Descriptor *)enum_descriptor)->own_module = enum_module;

  const Symbol *enum_symbol = mass_ensure_symbol(compilation, enum_name);
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, enum_symbol, enum_type_value);
}

static void
mass_alias_operator(
  Mass_Context *context,
  const Source_Range *source_range,
  Slice symbol_slice,
  Slice alias,
  Operator_Flags flags,
  Operator_Fixity fixity,
  u32 precedence
) {
  Operator *op = mass_allocate(context, Operator);
  *op = (Operator){
    .tag = Operator_Tag_Alias,
    .flags = flags,
    .fixity = fixity,
    .precedence = precedence,
    .associativity = Operator_Associativity_Left,
    .Alias = { .symbol = mass_ensure_symbol(context->compilation, alias) },
  };
  const Symbol *symbol = mass_ensure_symbol(context->compilation, symbol_slice);
  scope_define_operator(context, context->compilation->root_scope, *source_range, symbol, op);
}

static void
mass_intrinsic_operator(
  Mass_Context *context,
  const Source_Range *source_range,
  Slice symbol_slice,
  Slice intrinsic,
  Operator_Flags flags,
  Operator_Fixity fixity,
  u32 precedence
) {
  Operator *op = mass_allocate(context, Operator);
  *op = (Operator) {
    .tag = Operator_Tag_Intrinsic,
      .flags = flags,
      .fixity = fixity,
      .precedence = precedence,
      .associativity = Operator_Associativity_Left,
      .Intrinsic = {
        .body = scope_lookup(
          context->compilation->compiler_module.own_scope,
          mass_ensure_symbol(context->compilation, intrinsic)
        )->value
      },
  };
  const Symbol *symbol = mass_ensure_symbol(context->compilation, symbol_slice);
  scope_define_operator(context, context->compilation->root_scope, *source_range, symbol, op);
}

static void
mass_compilation_init_scopes(
  Compilation *compilation
) {
  Mass_Context context = mass_context_from_compilation(compilation);
  const Allocator *allocator = compilation->allocator;
  Scope *root_scope =  scope_make_declarative(compilation->allocator, 0);
  compilation->root_scope = root_scope;
  Scope *module_scope = scope_make_declarative(allocator, root_scope);
  compilation->compiler_module = (Module) {
    .source_range = {0},
    .own_scope = module_scope,
    .exports = {
      .tag = Module_Exports_Tag_All,
      .scope = module_scope,
    },
  };
  INIT_LITERAL_SOURCE_RANGE(&compilation->compiler_module.source_range, "MASS");
  compiler_scope_define_exports(compilation, module_scope);

  compilation->apply_operator = (Operator){
    .precedence = 20,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .tag = Operator_Tag_Intrinsic,
    .Intrinsic.body = scope_lookup(module_scope, mass_ensure_symbol(compilation, slice_literal("apply")))->value,
  };

  global_scope_define_exports(compilation, root_scope);
  #define MASS_INTRINSIC_OPERATOR(_SYMBOL_, _FLAGS_, _FIXITY_, _PRECEDENCE_, _INTRINSIC_)\
    do {\
      Source_Range source_range;\
      INIT_LITERAL_SOURCE_RANGE(&source_range, (_SYMBOL_));\
      mass_intrinsic_operator(\
        &context, &source_range, slice_literal(_SYMBOL_), slice_literal(_INTRINSIC_),\
        (_FLAGS_), Operator_Fixity_##_FIXITY_, (_PRECEDENCE_)\
      );\
    } while(false)

  #define MASS_ALIAS_OPERATOR(_SYMBOL_, _FLAGS_, _FIXITY_, _PRECEDENCE_, _ALIAS_)\
    do {\
      Source_Range source_range;\
      INIT_LITERAL_SOURCE_RANGE(&source_range, (_SYMBOL_));\
      mass_alias_operator(\
        &context, &source_range, slice_literal(_SYMBOL_), slice_literal(_ALIAS_),\
        (_FLAGS_), Operator_Fixity_##_FIXITY_, (_PRECEDENCE_)\
      );\
    } while(false)

  MASS_INTRINSIC_OPERATOR(".", Operator_Flags_None, Infix, 20, "get");

  // These keyword-like operators have extremely high precedence to make sure that
  // they really get the next token, unless the user has something really specific
  // in mind and creates an operator with an even higher precedence.
  MASS_INTRINSIC_OPERATOR("module", Operator_Flags_None, Prefix, 100, "inline_module");
  MASS_INTRINSIC_OPERATOR("intrinsic", Operator_Flags_None, Prefix, 100, "intrinsic");
  MASS_INTRINSIC_OPERATOR("c_struct", Operator_Flags_None, Prefix, 100, "c_struct");
  MASS_INTRINSIC_OPERATOR("exports", Operator_Flags_None, Prefix, 100, "exports");

  // `using` and `return` want an arbitrary expression so have the lowest precedence
  MASS_INTRINSIC_OPERATOR("using", Operator_Flags_None, Prefix, 0, "using");
  MASS_INTRINSIC_OPERATOR("return", Operator_Flags_Optional_Rhs, Prefix, 0, "return");

  // `size_of` and `type_of` are essentially function calls with special parsing
  // rules and evaluation behavior, so they get the same precedence as `apply`
  MASS_INTRINSIC_OPERATOR("import", Operator_Flags_None, Prefix, 20, "import");
  MASS_INTRINSIC_OPERATOR("type_of", Operator_Flags_None, Prefix, 20, "type_of");
  MASS_INTRINSIC_OPERATOR("parse_type", Operator_Flags_None, Prefix, 20, "parse_type");
  MASS_INTRINSIC_OPERATOR("size_of", Operator_Flags_None, Prefix, 20, "size_of");

  MASS_INTRINSIC_OPERATOR(",", Operator_Flags_Optional_Rhs, Infix, 0, "comma");

  MASS_INTRINSIC_OPERATOR("=", Operator_Flags_None, Infix, 1, "operator_assignment");
  MASS_INTRINSIC_OPERATOR(":=", Operator_Flags_None, Infix, 0, "define_inferred");
  MASS_INTRINSIC_OPERATOR(".*", Operator_Flags_None, Postfix, 20, "dereference");

  MASS_INTRINSIC_OPERATOR(":", Operator_Flags_None, Infix, 2, "typed_symbol");

  MASS_INTRINSIC_OPERATOR("@", Operator_Flags_None, Prefix, 20, "eval");
  MASS_INTRINSIC_OPERATOR(".", Operator_Flags_None, Prefix, 30, "named_accessor");
  MASS_INTRINSIC_OPERATOR("...", Operator_Flags_None, Prefix, 1, "spread");

  MASS_ALIAS_OPERATOR("==", Operator_Flags_None, Infix, 7, "equal");
  MASS_ALIAS_OPERATOR("!=", Operator_Flags_None, Infix, 7, "not_equal");
  MASS_ALIAS_OPERATOR("<", Operator_Flags_None, Infix, 8, "less");
  MASS_ALIAS_OPERATOR(">", Operator_Flags_None, Infix, 8, "greater");
  MASS_ALIAS_OPERATOR("<=", Operator_Flags_None, Infix, 8, "less_equal");
  MASS_ALIAS_OPERATOR(">=", Operator_Flags_None, Infix, 8, "greater_equal");
  MASS_ALIAS_OPERATOR("+", Operator_Flags_None, Infix, 10, "add");
  MASS_ALIAS_OPERATOR("-", Operator_Flags_None, Infix, 10, "subtract");
  MASS_ALIAS_OPERATOR("*", Operator_Flags_None, Infix, 15, "multiply");
  MASS_ALIAS_OPERATOR("/", Operator_Flags_None, Infix, 15, "divide");
  MASS_ALIAS_OPERATOR("%", Operator_Flags_None, Infix, 15, "remainder");
  MASS_ALIAS_OPERATOR("<<", Operator_Flags_None, Infix, 15, "logical_shift_left");
  MASS_ALIAS_OPERATOR(">>", Operator_Flags_None, Infix, 15, "logical_shift_right");
  MASS_ALIAS_OPERATOR("|", Operator_Flags_None, Infix, 15, "bitwise_or");
  MASS_ALIAS_OPERATOR("&", Operator_Flags_None, Infix, 15, "bitwise_and");
  MASS_ALIAS_OPERATOR("-", Operator_Flags_None, Prefix, 16, "negate");
  MASS_ALIAS_OPERATOR("&", Operator_Flags_None, Prefix, 16, "pointer_to");

  #undef MASS_INTRINSIC_OPERATOR
  #undef MASS_ALIAS_OPERATOR
}

static void
module_process_exports(
  Mass_Context *context,
  Parser *parser
) {
  Module *module = parser->module;
  switch(module->exports.tag) {
    case Module_Exports_Tag_Not_Specified: // Export everything when no explicit exports specified
    case Module_Exports_Tag_All: {
      module->exports.scope = module->own_scope;
    } break;
    case Module_Exports_Tag_Selective: {
      module->exports.scope = scope_make_declarative(context->allocator, module->own_scope->parent);

      const Tuple *tuple = module->exports.Selective.tuple;
      for (u64 tuple_index = 0; tuple_index < dyn_array_length(tuple->items); ++tuple_index) {
        Value *tuple_item = *dyn_array_get(tuple->items, tuple_index);
        if (!mass_value_ensure_static_of(context, tuple_item, &descriptor_named_accessor)) {
          return;
        }
        // TODO support renaming and wildcard exports
        const Symbol *symbol = value_as_named_accessor(tuple_item)->symbol;
        Scope_Entry *entry = scope_lookup_shallow(module->own_scope, symbol);
        if (!entry) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Undefined_Variable,
            .source_range = tuple_item->source_range,
            .Undefined_Variable = {.name = symbol->name},
          });
        }
        Value *symbol_value = value_make(
          context, &descriptor_symbol, storage_static(symbol), tuple_item->source_range
        );
        Value **expr_values = mass_allocate(context, Value *);
        *expr_values = symbol_value;
        Value_View expr = value_view_single(expr_values);
        scope_define_lazy_compile_time_expression(context, parser, module->exports.scope, symbol, expr);
      }
    } break;
  }
}

static Value *
mass_inline_module(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *arg_value = value_view_get(&args, 0);
  if (!mass_value_ensure_static_of(context, arg_value, &descriptor_ast_block)) return 0;
  const Ast_Block *curly = value_as_ast_block(arg_value);

  Module *module = mass_allocate(context, Module);
  *module = (Module) {
    .source_range = args.source_range,
    .own_scope = scope_make_declarative(context->allocator, parser->scope),
  };
  Parser module_parser = {
    .flags = Parser_Flags_Global,
    .scope = module->own_scope,
    .module = module,
    .epoch = VALUE_STATIC_EPOCH,
  };
  // Need a new context here to ensure we are using the runtime program for new modules
  Mass_Context module_context = mass_context_from_compilation(context->compilation);
  Value *block_result = token_parse_block_statements(
    &module_context, &module_parser, curly->first_statement, &args.source_range
  );
  Value *void_value = mass_make_void(context, args.source_range);
  value_force_exact(&module_context, 0, module_parser.scope, void_value, block_result);
  module_process_exports(&module_context, &module_parser);

  return value_make(&module_context, &descriptor_module, storage_static(module), args.source_range);
}

static inline void
program_parse(
  Mass_Context *context,
  Parser *parser
) {
  assert(parser->module);
  Performance_Counter perf = system_performance_counter_start();
  Ast_Block block;
  const Source_Range *program_range = &parser->module->source_range;
  *context->result = tokenize(context, *program_range, &block);
  if (mass_has_error(context)) return;
  if (0) {
    u64 usec = system_performance_counter_end(&perf);
    printf("Tokenizer took %"PRIu64" µs\n", usec);
  }

  Value *block_result = token_parse_block_statements(
    context, parser, block.first_statement, program_range
  );
  if (mass_has_error(context)) return;
  compile_time_eval(context, parser, value_view_single(&block_result));
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
    DWORD current_dir_size = GetCurrentDirectoryW(0, 0) * sizeof(wchar_t);
    sys_buffer->occupied =
      GetCurrentDirectoryW(current_dir_size, (wchar_t *)sys_buffer->memory) * sizeof(wchar_t);
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

static Module *
program_module_from_file(
  Mass_Context *context,
  Slice file_path,
  Scope *scope
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = program_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Source_File *file = mass_allocate(context, Source_File);
  *file = (Source_File) {
    .path = file_path,
  };
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path);
  if (!buffer) {
    Source_Range error_source_range = { .file = file, .offsets = {0} };
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_File_Open,
      .source_range = error_source_range,
      .detailed_message = slice_literal("Unable to open the file"),
      .File_Open = {.path = file_path},
    });
    return 0;
  }
  file->text = fixed_buffer_as_slice(buffer);

  if (buffer->occupied > UINT32_MAX) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_File_Too_Large,
      .File_Too_Large = { .path = file_path },
      .source_range = {
        .file = file,
        .offsets = {.from = UINT32_MAX, .to = UINT32_MAX}
      },
    });
    return 0;
  }

  Module *module = mass_allocate(context, Module);
  *module = (Module) {
    .source_range = {
      .file = file,
      .offsets = {.from = 0, .to = u64_to_u32(buffer->occupied)}
    },
    .own_scope = scope,
  };
  return module;
}

static void
program_import_module(
  Mass_Context *context,
  Module *module
) {
  Parser import_parser = {
    .flags = Parser_Flags_Global,
    .scope = module->own_scope,
    .module = module,
    .epoch = VALUE_STATIC_EPOCH,
  };

  program_parse(context, &import_parser);
  if (mass_has_error(context)) return;
  module_process_exports(context, &import_parser);
}

static void
program_load_file_module_into_root_scope(
  Mass_Context *context,
  Slice file_path
) {
  Module *module = program_module_from_file(context, file_path, context->compilation->root_scope);
  if (mass_has_error(context)) return;
  program_import_module(context, module);
}

static void
mass_print_error(
  Compilation *compilation,
  Mass_Error *error
) {
  Fixed_Buffer *error_buffer = mass_error_to_string(compilation, error);
  slice_print(fixed_buffer_as_slice(error_buffer));
  fixed_buffer_destroy(error_buffer);
  printf("\n  at ");
  source_range_print_start_position(compilation, &error->source_range);
}

static void
mass_maybe_trim_shebang(
  Source_Range *source_range
) {
  Range_u32 *offsets = &source_range->offsets;
  for (; offsets->from < offsets->to; offsets->from += 1) {
    char ch = source_range->file->text.bytes[offsets->from];
    if (isspace(ch)) continue;
    if (ch == '#') {
      // Skip till the end of line
      for (; offsets->from < offsets->to; offsets->from += 1) {
        if (source_range->file->text.bytes[offsets->from] == '\n') {
          offsets->from += 1;
          goto next;
        }
      }
    }
    next:
    break;
  }
}

static void
mass_run_script(
  Mass_Context *context,
  Slice file_path
) {
  Compilation *compilation = context->compilation;
  context->program = compilation->jit.program;
  Module *root_module = program_module_from_file(context, file_path, compilation->root_scope);
  if (mass_has_error(context)) return;

  // Trim leading whitespace and possible a shebang
  Source_Range source_range = root_module->source_range;
  mass_maybe_trim_shebang(&source_range);

  Ast_Block body;
  *context->result = tokenize(context, source_range, &body);
  if (mass_has_error(context)) return;

  Value *fake_function_body = value_make(
    context, &descriptor_ast_block, storage_static(&body), source_range
  );

  Parser parser = {
    .flags = Parser_Flags_None,
    .scope = compilation->root_scope,
    .module = root_module,
    .epoch = get_new_epoch(),
  };

  Function_Literal *literal = mass_make_fake_function_literal(
    context, &parser, fake_function_body, &descriptor_void, &source_range
  );
  Array_Resolved_Function_Parameter params = dyn_array_static_empty(Array_Resolved_Function_Parameter);
  const Function_Info *entry_info = function_literal_info_for_parameters(context, literal, params);
  context->program->entry_point = mass_function_literal_instance_for_info(context, literal, entry_info);

  if (mass_has_error(context)) return;

  Jit *jit = &compilation->jit;
  program_jit(context, jit);
  if (mass_has_error(context)) return;
  fn_type_opaque script = value_as_function(jit->program, jit->program->entry_point);
  script();
}