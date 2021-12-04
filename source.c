#include "prelude.h"
#include "source.h"
#include "function.h"

#include "generated_exports.c"

static inline bool
mass_value_ensure_static_of(
  Mass_Context *context,
  Value *value,
  const Descriptor *expected_descriptor
) {
  if (!mass_value_is_compile_time_known(value)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Expected_Static,
      .source_range = value->source_range,
    });
    return false;
  }
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
  const Allocator *allocator,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  Source_Range source_range
) {
  const Descriptor *descriptor = mass_expected_result_descriptor(expected_result);
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Storage storage = expected_result->Exact.storage;
      return value_make(allocator, descriptor, storage, source_range);
    } break;
    case Expected_Result_Tag_Flexible: {
      if (mass_descriptor_is_void(descriptor)) {
        return mass_make_void(allocator, source_range);
      }
      Storage storage = storage_none;
      if (descriptor->bit_size.as_u64 <= 64) {
        Register reg = register_acquire_temp(builder);
        storage = storage_register(reg, descriptor->bit_size);
      } else if (descriptor->bit_size.as_u64) {
        storage = reserve_stack_storage(builder, descriptor->bit_size);
      }
      storage.flags |= Storage_Flags_Temporary;
      return value_make(allocator, descriptor, storage, source_range);
    } break;
  }
  panic("UNREACHABLE");
  return 0;
}


static inline const Descriptor *
mass_expected_result_descriptor(
  const Expected_Result *expected_result
) {
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: return expected_result->Exact.descriptor;
    case Expected_Result_Tag_Flexible: return expected_result->Flexible.descriptor;
  }
  panic("Unknown Expected_Result tag");
  return 0;
}

static inline Expected_Result
mass_expected_result_exact(
  const Descriptor *descriptor,
  Storage storage
) {
  return (Expected_Result) {
    .tag = Expected_Result_Tag_Exact,
    .Exact = { .descriptor = descriptor, .storage = storage },
  };
}

static inline Expected_Result
expected_result_any(
  const Descriptor *descriptor
) {
  return (Expected_Result){
    .tag = Expected_Result_Tag_Flexible,
    .Flexible = { .descriptor = descriptor },
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
      assert(same_type(expected_result->Exact.descriptor, actual_value->descriptor));
      assert(storage_equal(&expected_result->Exact.storage, &actual_value->storage));
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
scope_make(
  const Allocator *allocator,
  const Scope *parent
) {
  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .allocator = allocator,
    .parent = parent,
    .map = 0,
  };
  return scope;
}

static inline u32
scope_statement_matcher_shallow(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  const Scope *scope
) {
  const Token_Statement_Matcher *matcher = scope->statement_matcher;
  for (; matcher; matcher = matcher->previous) {
    u32 match_length = matcher->proc(context, parser, view, out_lazy_value, matcher->payload);

    if (mass_has_error(context)) return 0;
    if (match_length) return match_length;
  }
  return 0;
}

static inline u32
token_statement_matcher_in_scopes(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  const Scope *scope
) {
  for (; scope; scope = scope->parent) {
    // Do a reverse iteration because we want statements that are defined later
    // to have higher precedence when parsing
    u32 match_length = scope_statement_matcher_shallow(context, parser, view, out_lazy_value, scope);
    if (match_length) return match_length;
  }
  return 0;
}

static void
mass_copy_scope_exports(
  Scope *to,
  const Scope *from
) {
  // TODO also deal with statement matchers here
  for (u64 i = 0; i < from->map->capacity; ++i) {
    Scope_Map__Entry *map_entry = &from->map->entries[i];
    if (!map_entry->occupied) continue;
    Scope_Entry *entry = map_entry->value;
    const Symbol *symbol = map_entry->key;
    scope_define_value(to, entry->epoch, entry->source_range, symbol, entry->value);
  }
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
        slice_print(entry->value->name);
        printf(" ; ");
      }
    }
  }
  printf("\n");
}

static inline Scope_Entry *
scope_lookup_shallow(
  const Scope *scope,
  const Symbol *symbol
) {
  if (!scope->map) return 0;
  Scope_Entry **entry_pointer = hash_map_get(scope->map, symbol);
  if (!entry_pointer) return 0;
  return *entry_pointer;
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
        return value_make(context->allocator, target_descriptor, imm, *source_range);
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
      case Literal_Cast_Result_Unsigned_Target_For_Negative_Literal: {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Integer_Range,
          .source_range = *source_range,
          .Integer_Range = { .descriptor = target_descriptor },
          .detailed_message = slice_literal("Can not convert a negative literal to an unsigned number"),
        });
        return 0;
      }
    }
    panic("Unexpected literal cast result");
  } else {
    panic("Trying to force non-literal immediate");
  }

  return value;
}

static bool
assign_from_static(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Source_Range *source_range
) {
  if (
    !context_is_compile_time_eval(context) &&
    source->descriptor->tag == Descriptor_Tag_Pointer_To
  ) {
    // If a static value contains a pointer, we expect an entry in a special map used to track
    // whether the target memory is also already copied to the compiled binary.
    // This is done to only include static values actually used at runtime.
    void *source_memory = *(void **)storage_static_memory_with_bit_size(&source->storage, (Bits){64});
    Value *static_pointer = *hash_map_get(context->compilation->static_pointer_map, source_memory);
    if (static_pointer->storage.tag == Storage_Tag_None) {
      Section *section = (static_pointer->flags & Value_Flags_Constant)
       ? &context->program->memory.ro_data
       : &context->program->memory.rw_data;
      u64 byte_size = descriptor_byte_size(static_pointer->descriptor);
      u64 alignment = descriptor_byte_alignment(static_pointer->descriptor);

      // TODO this should also be deduped
      Label *label = allocate_section_memory(
        context->allocator, context->program, section, byte_size, alignment
      );
      static_pointer->storage = data_label32(label, static_pointer->descriptor->bit_size);

      Value static_source_value = {
        .descriptor = static_pointer->descriptor,
        .storage = storage_static_heap(source_memory, static_pointer->descriptor->bit_size),
        .source_range = *source_range,
      };

      // It is important to call assign here to make sure we recursively handle
      // any complex types such as structs and arrays
      mass_assign(context, builder, static_pointer, &static_source_value, source_range);
      if (mass_has_error(context)) return true;
    }
    assert(storage_is_label(&static_pointer->storage));
    if (storage_is_label(&target->storage)) {
      dyn_array_push(context->program->relocations, (Relocation) {
        .patch_at = target->storage,
        .address_of = static_pointer->storage,
      });
    } else {
      load_address(builder, source_range, target, static_pointer->storage);
    }
    return true;
  } else if (storage_is_label(&target->storage)) {
    void *section_memory = rip_value_pointer_from_label(
      target->storage.Memory.location.Instruction_Pointer_Relative.label
    );
    const void *source_memory =
      storage_static_memory_with_bit_size(&source->storage, source->storage.bit_size);
    memcpy(section_memory, source_memory, source->storage.bit_size.as_u64 / 8);
    return true;
  }
  return false;
}

static Value *
value_indirect_from_pointer(
  Mass_Context *context,
  Function_Builder *builder,
  Value *source
) {
  const Descriptor *referenced_descriptor;
  const Descriptor *source_descriptor = value_or_lazy_value_descriptor(source);
  if (source_descriptor->tag == Descriptor_Tag_Pointer_To) {
    referenced_descriptor = source_descriptor->Pointer_To.descriptor;
  } else {
    panic("Unexpected descriptor tag for an indirect value");
    return 0;
  }
  if (source->descriptor == &descriptor_lazy_value) {
    Expected_Result expected_result = expected_result_any(source_descriptor);
    source = value_force(context, builder, &expected_result, source);
    if (mass_has_error(context)) return 0;
  }

  switch(source->storage.tag) {
    case Storage_Tag_Register: {
      Register reg = source->storage.Register.index;
      Storage referenced_storage = storage_indirect(referenced_descriptor->bit_size, reg);
      referenced_storage.flags |= source->storage.flags & Storage_Flags_Temporary;
      Value *value = value_make(
        context->allocator, referenced_descriptor, referenced_storage, source->source_range
      );
      return value;
    }
    case Storage_Tag_Memory: {
      Register reg = register_acquire_temp(builder);
      Storage reg_storage = storage_register(reg, source_descriptor->bit_size);
      move_value(builder, &source->source_range, &reg_storage, &source->storage);
      storage_release_if_temporary(builder, &source->storage);
      Storage referenced_storage = storage_indirect(referenced_descriptor->bit_size, reg);
      referenced_storage.flags |= Storage_Flags_Temporary;
      Value *temp = value_make(
        context->allocator, referenced_descriptor, referenced_storage, source->source_range
      );
      return temp;
    }
    default:
    case Storage_Tag_Unpacked:
    case Storage_Tag_Immediate:
    case Storage_Tag_Static:
    case Storage_Tag_None:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm:{
      panic("Unexpected storage for a reference");
      return 0;
    }
  }
}

static const Descriptor *
deduce_runtime_descriptor_for_value(
  Mass_Context *context,
  Value *value,
  const Descriptor *maybe_desired_descriptor
);

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

typedef enum {
  Tuple_Eval_Mode_Value,
  Tuple_Eval_Mode_Type,
} Tuple_Eval_Mode;

static inline const Descriptor *
value_ensure_type(
  Mass_Context *context,
  Value *value,
  Source_Range source_range
);

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
    .initial_capacity = dyn_array_length(tuple->items) * 2,
    .allocator = context->temp_allocator,
  );

  for (u64 i = 0; i < dyn_array_length(tuple->items); ++i) {
    Value *item = *dyn_array_get(tuple->items, i);
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
          field_descriptor = value_ensure_type(context, item, item->source_range);
        }
      } break;
      default: {
        panic("UNREACHABLE");
        return 0;
      } break;
    }
    u64 field_byte_offset = c_struct_aligner_next_byte_offset(&struct_aligner, field_descriptor);
    if (name.length) {
      if (hash_map_has(field_name_set, name)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Redefinition,
          .source_range = item->source_range,
          .Redefinition = { .name = name },
        });
        goto err;
      } else {
        hash_map_set(field_name_set, name, 1);
      }
    }

    dyn_array_push(fields, (Struct_Field) {
      .name = name,
      .descriptor = field_descriptor,
      .source_range = item->source_range,
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

// TODO make this function produce a better error on failure
static const Descriptor *
deduce_runtime_descriptor_for_value(
  Mass_Context *context,
  Value *value,
  const Descriptor *maybe_desired_descriptor
) {
  if (!mass_value_is_compile_time_known(value)) {
    return value_or_lazy_value_descriptor(value);
  }

  if (value->descriptor == &descriptor_i64) {
    if (maybe_desired_descriptor) {
      Literal_Cast_Result cast_result =
        value_i64_cast_to(value, maybe_desired_descriptor, &(u64){0}, &(u64){0});
      if (cast_result == Literal_Cast_Result_Success) {
        return maybe_desired_descriptor;
      } else {
        return 0;
      }
    } else {
      return &descriptor_s64;
    }
  }

  if (value->descriptor == &descriptor_tuple) {
    const Tuple *tuple = value_as_tuple(value);
    return anonymous_struct_descriptor_from_tuple(context, tuple, Tuple_Eval_Mode_Value);
  }

  if (value->descriptor == &descriptor_overload || value->descriptor == &descriptor_function_literal) {
    const Function_Info *target_info;
    if (value->descriptor == &descriptor_function_literal) {
      const Function_Literal *literal = value_as_function_literal(value);
      if (literal->flags & Function_Literal_Flags_Macro) {
        return 0;
      }
      if (literal->flags & Function_Literal_Flags_Generic) {
        return 0;
      }
      target_info = literal->info;
    } else {
      if (!maybe_desired_descriptor || maybe_desired_descriptor->tag != Descriptor_Tag_Function_Instance) {
        return 0;
      }
      target_info = maybe_desired_descriptor->Function_Instance.info;
    }
    // @CopyPaste in `mass_assign`
    // TODO @Speed use temp allocator here
    Array_Value_Ptr fake_args = dyn_array_make(
      Array_Value_Ptr,
      .allocator = context->allocator,
      .capacity = dyn_array_length(target_info->parameters),
    );
    DYN_ARRAY_FOREACH(Function_Parameter, param, target_info->parameters) {
      assert(param->tag == Function_Parameter_Tag_Runtime);
      assert(param->descriptor);
      Value *fake_value = value_make(
        context->allocator,
        param->descriptor,
        storage_none,
        param->source_range
      );
      dyn_array_push(fake_args, fake_value);
    }
    Value_View args_view = value_view_from_value_array(fake_args, &value->source_range);

    Overload_Match_Found match_found;
    if (!mass_match_overload_or_error(context, value, args_view, &match_found)) {
      return 0;
    }
    // @Speed it is probably wasteful to ask for an instance every time here
    Value *instance = ensure_function_instance(context, match_found.value, args_view);
    if (mass_has_error(context)) panic("UNREACHABLE");
    assert(instance->descriptor->tag == Descriptor_Tag_Function_Instance);
    return instance->descriptor;
  }

  return value_or_lazy_value_descriptor(value);
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

// TODO :AssignCleanup Merge this with `same_type_or_can_implicitly_move_cast`
static void
assign_tuple(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Source_Range *source_range
) {
  const Tuple *tuple = value_as_tuple(source);
  Temp_Mark temp_mark = context_temp_mark(context);
  if (target->descriptor->tag == Descriptor_Tag_Struct) {
    Array_Struct_Field fields = target->descriptor->Struct.fields;
    if ((dyn_array_length(fields) != dyn_array_length(tuple->items))) {
      Slice message = dyn_array_length(fields) > dyn_array_length(tuple->items)
        ? slice_literal("Tuple does not have enough items to match the struct it is assigned to")
        : slice_literal("Tuple has too many items for the struct it is assigned to");
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
        .detailed_message = message,
      });
      goto err;
    }

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
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Undefined_Variable,
            .Undefined_Variable = { .name = symbol->name },
            .source_range = tuple_item->source_range,
          });
          goto err;
        }
        if (entry->epoch.as_u64 != tuple->epoch.as_u64) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Epoch_Mismatch,
            .source_range = source->source_range,
          });
          goto err;
        }
        field_source = scope_entry_force_value(context, entry);
        if (!struct_find_field_by_name(target->descriptor, symbol->name, &field, &field_index)) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Unknown_Field,
            .source_range = tuple_item->source_range,
            .Unknown_Field = { .name = symbol->name, .type = target->descriptor },
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
        if (!struct_find_field_by_name(target->descriptor, accessor->symbol->name, &field, &field_index)) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Unknown_Field,
            .source_range = tuple_item->source_range,
            .Unknown_Field = { .name = accessor->symbol->name, .type = target->descriptor },
          });
          goto err;
        }
      } else {
        field_source = tuple_item;
        field = dyn_array_get(fields, field_index);
      }
      field_index += 1;
      if (hash_map_has(assigned_set, field)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Redefinition,
          .source_range = tuple_item->source_range,
          .Redefinition = { .name = field->name },
        });
        goto err;
      }
      hash_map_set(assigned_set, field, 1);

      Value target_field = {
        .descriptor = field->descriptor,
        .storage = storage_with_offset_and_bit_size(
          &target->storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign(context, builder, &target_field, field_source, source_range);
      if (mass_has_error(context)) goto err;
    }
  } else if (target->descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
    u64 length = target->descriptor->Fixed_Size_Array.length;
    if ((length != dyn_array_length(tuple->items))) {
      Slice message = length > dyn_array_length(tuple->items)
        ? slice_literal("Tuple does not have enough items to match the array it is assigned to")
        : slice_literal("Tuple has too many items for the struct it is assigned to");
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Type_Mismatch,
        .source_range = *source_range,
        .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
        .detailed_message = message,
      });
      goto err;
    }

    const Descriptor *item_descriptor = target->descriptor->Fixed_Size_Array.item;
    u64 item_byte_size = descriptor_byte_size(item_descriptor);
    for (u64 index = 0; index < length; ++index) {
      Value *tuple_item = *dyn_array_get(tuple->items, index);
      s32 byte_offset = u64_to_s32(item_byte_size * index);
      Value target_field = {
        .descriptor = item_descriptor,
        .storage = storage_with_offset_and_bit_size(
          &target->storage, byte_offset, item_descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign(context, builder, &target_field, tuple_item, source_range);
      if (mass_has_error(context)) goto err;
    }
  } else {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = target->descriptor, .actual = source->descriptor },
    });
  }

  err:
  context_temp_reset_to_mark(context, temp_mark);
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
  const Storage *target,
  const Storage *source
) {
  bool can_reuse_result_as_temp = target->tag == Storage_Tag_Register;
  Storage register_storage = can_reuse_result_as_temp
    ? *target
    : storage_register_temp(builder, target->bit_size);

  assert(register_storage.bit_size.as_u64 == 64);
  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){lea, {register_storage, storage_adjusted_for_lea(*source)}}
  );

  if (!can_reuse_result_as_temp) {
    assert(register_storage.tag == Storage_Tag_Register);
    move_value(builder, source_range, target, &register_storage);
    register_release(builder, register_storage.Register.index);
  }
}

static inline void
load_address(
  Function_Builder *builder,
  const Source_Range *source_range,
  Value *result_value,
  Storage source
) {
  assert(
    result_value->descriptor->tag == Descriptor_Tag_Pointer_To ||
    result_value->descriptor->tag == Descriptor_Tag_Function_Instance
  );
  mass_storage_load_address(builder, source_range, &result_value->storage, &source);
}

static void
mass_assign(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Source_Range *source_range
) {
  if (target->flags & Value_Flags_Constant) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Assignment_To_Constant,
      .source_range = *source_range,
    });
    return;
  }

  if (source->descriptor == &descriptor_lazy_value) {
    value_force_exact(context, builder, target, source);
    return;
  }
  assert(target->descriptor != &descriptor_lazy_value);

  if (mass_descriptor_is_void(target->descriptor)) {
    return;
  }

  if (descriptor_is_implicit_pointer(source->descriptor)) {
    Value *ref_source = value_indirect_from_pointer(context, builder, source);
    mass_assign(context, builder, target, ref_source, source_range);
    storage_release_if_temporary(builder, &ref_source->storage);
    return;
  }

  if (descriptor_is_implicit_pointer(target->descriptor)) {
    if (
      (
        mass_value_is_compile_time_known(source) &&
        !assign_from_static(context, builder, target, source, source_range)
      ) ||
      source->descriptor == &descriptor_tuple
    ) {
      Value *referenced_target = value_indirect_from_pointer(context, builder, target);
      mass_assign(context, builder, referenced_target, source, source_range);
      storage_release_if_temporary(builder, &referenced_target->storage);
      return;
    } else {
      const Descriptor *original_descriptor = target->descriptor->Pointer_To.descriptor;
      if (!same_type(original_descriptor, source->descriptor)) goto err;
      load_address(builder, source_range, target, source->storage);
    }
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Raw && !source->descriptor->brand) {
    if (value_is_i64(source)) {
      if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
        const i64 *literal = value_as_i64(source);
        if (literal->bits == 0) {
          Storage zero = imm64(0);
          move_value(builder, source_range, &target->storage, &zero);
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
    if (descriptor_is_integer(target->descriptor)) {
       Mass_Cast_Lazy_Payload lazy_payload = {
        .target = target->descriptor,
        .expression = source,
      };
      Expected_Result expected_result = expected_result_any(target->descriptor);
      source = mass_handle_cast_lazy_proc(context, builder, &expected_result, source_range, &lazy_payload);
      if (mass_has_error(context)) return;
    }
  }

  if (source->descriptor == &descriptor_tuple) {
    assign_tuple(context, builder, target, source, source_range);
    return;
  }

  if (target->descriptor->tag == Descriptor_Tag_Function_Instance) {
    const Function_Info *target_info = target->descriptor->Function_Instance.info;
    if (
      source->descriptor == &descriptor_function_literal ||
      source->descriptor == &descriptor_overload
    ) {
      // TODO @Speed use temp allocator here
      Array_Value_Ptr fake_args = dyn_array_make(
        Array_Value_Ptr,
        .allocator = context->allocator,
        .capacity = dyn_array_length(target_info->parameters),
      );
      DYN_ARRAY_FOREACH(Function_Parameter, param, target_info->parameters) {
        assert(param->tag == Function_Parameter_Tag_Runtime);
        assert(param->descriptor);
        Value *fake_value = value_make(
          context->allocator,
          param->descriptor,
          storage_none,
          param->source_range
        );
        dyn_array_push(fake_args, fake_value);
      }
      Value_View args_view = value_view_from_value_array(fake_args, &source->source_range);

      Overload_Match_Found match_found;
      if (!mass_match_overload_or_error(context, source, args_view, &match_found)) {
        return;
      }

      source = ensure_function_instance(context, match_found.value, args_view);
      if (mass_has_error(context)) return;
      assert(source->descriptor->tag == Descriptor_Tag_Function_Instance);
    }

    if (same_type(target->descriptor, source->descriptor)) {
      if (source->storage.tag == Storage_Tag_Memory) {
        load_address(builder, source_range, target, source->storage);
      } else {
        move_value(builder, source_range, &target->storage, &source->storage);
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

  if (source->descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
    if (!same_type(target->descriptor, source->descriptor)) goto err;
    const Descriptor *item_descriptor = source->descriptor->Fixed_Size_Array.item;

    Storage source_array_storage = value_maybe_dereference(context, builder, source);
    Storage target_array_storage = value_maybe_dereference(context, builder, target);

    for (u64 i = 0; i < source->descriptor->Fixed_Size_Array.length; ++i) {
      s32 index_number = (u64_to_s32(i));
      s32 offset = index_number * u64_to_s32(descriptor_byte_size(item_descriptor));

      Value source_field = {
        .descriptor = item_descriptor,
        .storage = storage_with_offset_and_bit_size(
          &source_array_storage, offset, item_descriptor->bit_size
        ),
        .source_range = source->source_range,
      };
      Value target_field = {
        .descriptor = item_descriptor,
        .storage = storage_with_offset_and_bit_size(
          &target_array_storage, offset, item_descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign(context, builder, &target_field, &source_field, source_range);
      if (mass_has_error(context)) return;
    }
    storage_release_if_temporary(builder, &source_array_storage);
    storage_release_if_temporary(builder, &target_array_storage);
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Struct) {
    if (!same_type_or_can_implicitly_move_cast(target->descriptor, source->descriptor)) goto err;

    DYN_ARRAY_FOREACH(Struct_Field, field, source->descriptor->Struct.fields) {
      Value source_field = {
        .descriptor = field->descriptor,
        .storage = storage_with_offset_and_bit_size(
          &source->storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = source->source_range,
      };
      Value target_field = {
        .descriptor = field->descriptor,
        .storage = storage_with_offset_and_bit_size(
          &target->storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign(context, builder, &target_field, &source_field, source_range);
      if (mass_has_error(context)) return;
    }
    return;
  }

  if (mass_value_is_compile_time_known(source)) {
    if (assign_from_static(context, builder, target, source, source_range)) {
      return;
    }
  }

  if (mass_has_error(context)) return;
  if (same_type_or_can_implicitly_move_cast(target->descriptor, source->descriptor)) {
    move_value(builder, source_range, &target->storage, &source->storage);
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
  // TODO figure out how to get rid of this cast
  Lazy_Static_Value *lazy = (Lazy_Static_Value *)value_as_lazy_static_value(value);
  if (lazy->resolving) {
    mass_error(&lazy->context, (Mass_Error) {
      .tag = Mass_Error_Tag_Circular_Dependency,
      .source_range = value->source_range,
      .Circular_Dependency = { .name = name },
    });
    return 0;
  }
  lazy->resolving = true;
  Value *result = compile_time_eval(&lazy->context, &lazy->parser, lazy->expression);
  if (!result) return 0;
  lazy->resolving = false;
  *value = *result;
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

static inline void
scope_define_value(
  Scope *scope,
  Epoch epoch,
  Source_Range source_range,
  const Symbol *symbol,
  Value *value
) {
  if (!scope->map) {
    scope->map = hash_map_make(Scope_Map, scope->allocator);
  }
  Scope_Entry *it = scope_lookup_shallow(scope, symbol);
  if (it) {
    Overload *overload = allocator_allocate(scope->allocator, Overload);
    *overload = (Overload) { .value = value, .next = it->value };
    it->value = value_make(
      scope->allocator, &descriptor_overload, storage_static(overload), source_range
    );
  } else {
    Scope_Entry *allocated = allocator_allocate(scope->allocator, Scope_Entry);
    *allocated = (Scope_Entry) {
      .value = value,
      .name = symbol->name,
      .epoch = epoch,
      .source_range = source_range,
    };
    hash_map_set(scope->map, symbol, allocated);
  }
}

static inline void
scope_define_operator(
  Mass_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice name,
  Operator *operator
) {
  Symbol_Map *map = operator->fixity == Operator_Fixity_Prefix
    ? context->compilation->prefix_operator_symbol_map
    : context->compilation->infix_or_suffix_operator_symbol_map;
  const Symbol *symbol = mass_ensure_symbol_for_map(context->allocator, map, name);

  if (scope_lookup_shallow(scope, symbol)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Operator_Fixity_Conflict,
      .source_range = source_range,
      .Operator_Fixity_Conflict = {
        .fixity = operator->fixity,
        .symbol = symbol->name,
      },
    });
    return;
  }

  Value *operator_value = value_make(
    context->allocator, &descriptor_operator, storage_static(operator), source_range
  );
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, symbol, operator_value);
}

static inline const Operator *
scope_lookup_operator(
  Mass_Context *context,
  const Scope *scope,
  Slice name,
  Operator_Fixity fixity
) {
  Symbol_Map *map = fixity == Operator_Fixity_Prefix
    ? context->compilation->prefix_operator_symbol_map
    : context->compilation->infix_or_suffix_operator_symbol_map;

  // TODO use symbol lookups instead of string-based ones
  Symbol *const *operator_symbol_pointer = hash_map_get(map, name);
  if (!operator_symbol_pointer) return 0;
  const Symbol *operator_symbol = *operator_symbol_pointer;
  Scope_Entry *maybe_operator_entry = scope_lookup(scope, operator_symbol);
  if (!maybe_operator_entry) return 0;
  if (maybe_operator_entry->value->descriptor != &descriptor_operator) return 0;
  return value_as_operator(maybe_operator_entry->value);
}

static inline Value *
token_make_symbol_value(
  Mass_Context *context,
  Slice name,
  Source_Range source_range
) {
  const Symbol *symbol = mass_ensure_symbol(context->compilation, name);

  return value_make(
    context->allocator, &descriptor_symbol, storage_static(symbol), source_range
  );
}

static inline bool
value_match_symbol(
  const Value *token,
  Slice name
) {
  if (!value_is_symbol(token)) return false;
  if (!name.length) return true;
  return slice_equal(value_as_symbol(token)->name, name);
}

static inline Value_View
temp_token_array_into_value_view(
  const Allocator *allocator,
  Value **children,
  u32 child_count,
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
  const Descriptor *descriptor;
  u64 index;
} Tokenizer_Parent;
typedef dyn_array_type(Tokenizer_Parent) Array_Tokenizer_Parent;

static inline Value_View
tokenizer_make_group_children_view(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Tokenizer_Parent *parent,
  Value *parent_value,
  u64 offset
) {
  Value **children_values = dyn_array_raw(*stack) + parent->index + 1;
  u64 child_count = dyn_array_length(*stack) - parent->index - 1;
  stack->data->length = parent->index + 1; // pop the children

  Source_Range children_range = {
    .file = parent_value->source_range.file,
    .offsets = {.from = u64_to_u32(offset), .to = u64_to_u32(offset)},
  };
  if (child_count) {
    Value *first_child = children_values[0];
    children_range.offsets.from = first_child->source_range.offsets.from;
  }

  parent_value->source_range.offsets.to = u64_to_u32(offset);

  return temp_token_array_into_value_view(
    allocator, children_values, u64_to_u32(child_count), children_range
  );
}

static inline bool
tokenizer_maybe_push_statement(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  u64 offset
) {
  assert(dyn_array_length(*parent_stack));
  Tokenizer_Parent *parent = dyn_array_last(*parent_stack);
  if(parent->descriptor != &descriptor_ast_block) return false;
  bool has_children = parent->index + 1 != dyn_array_length(*stack);
  // Do not treat leading newlines as semicolons
  if (!has_children) return true;

  Value *parent_value = *dyn_array_get(*stack, parent->index);

  Ast_Block *group = (Ast_Block *)value_as_ast_block(parent_value);

  assert(offset);
  Value_View statement = tokenizer_make_group_children_view(
    context->allocator, stack, parent, parent_value, offset
  );
  dyn_array_push(group->statements, statement);
  return true;
}

static inline Value *
tokenizer_group_start(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  const Descriptor *group_descriptor,
  Source_Range source_range
) {
  Value *value = value_init(
    allocator_allocate(allocator, Value),
    group_descriptor, storage_none, source_range
  );
  dyn_array_push(*parent_stack, (Tokenizer_Parent){
    .descriptor = group_descriptor,
    .index = dyn_array_length(*stack)
  });
  dyn_array_push(*stack, value);
  return value;
}

static inline void
tokenizer_group_start_curly(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  const Descriptor *group_descriptor,
  Source_Range source_range
) {
  Value *value = tokenizer_group_start(allocator, stack, parent_stack, group_descriptor, source_range);
  assert(value->storage.tag == Storage_Tag_None);
  Ast_Block *group = allocator_allocate(allocator, Ast_Block);
  // TODO use temp allocator first?
  *group = (Ast_Block){.statements = dyn_array_make(Array_Value_View, .allocator = allocator)};
  value->storage = storage_immediate(group);
}

static inline bool
tokenizer_group_end_paren(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  u64 offset
) {
  if (!dyn_array_length(*parent_stack)) return false;
  Tokenizer_Parent *parent = dyn_array_pop(*parent_stack);
  Value *parent_value = *dyn_array_get(*stack, parent->index);
  if (parent_value->descriptor != &descriptor_group_paren) return false;

  Value_View children = tokenizer_make_group_children_view(
    context->allocator, stack, parent, parent_value, offset
  );
  Group_Paren *group = mass_allocate(context, Group_Paren);
  *group = (Group_Paren){.children = children};
  parent_value->storage = storage_static(group);

  return true;
}

static inline bool
tokenizer_group_end_square(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  u64 offset
) {
  if (!dyn_array_length(*parent_stack)) return false;
  Tokenizer_Parent *parent = dyn_array_pop(*parent_stack);
  Value *parent_value = *dyn_array_get(*stack, parent->index);
  if (parent_value->descriptor != &descriptor_group_square) return false;

  Value_View children = tokenizer_make_group_children_view(
    context->allocator, stack, parent, parent_value, offset
  );
  Group_Square *group = mass_allocate(context, Group_Square);
  *group = (Group_Square){.children = children};
  parent_value->storage = storage_static(group);

  return true;
}

static inline bool
tokenizer_group_end_curly(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  u64 offset
) {
  if (!tokenizer_maybe_push_statement(context, stack, parent_stack, offset)) return false;
  dyn_array_pop(*parent_stack);
  return true;
}

static inline void
tokenizer_push_string_literal(
  Mass_Context *context,
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
  char *bytes = allocator_allocate_bytes(context->allocator, length, 1);
  memcpy(bytes, (*string_buffer)->memory, length);

  allocator_allocate_bulk(context->allocator, combined, {
    Descriptor bits_descriptor;
    Slice slice;
    Value string_value;
    Value static_pointer_value;
  });
  {
    // TODO This should be an array of `i8`
    Descriptor *bits_descriptor = &combined->bits_descriptor;
    *bits_descriptor = (Descriptor) {
      .tag = Descriptor_Tag_Raw,
      .bit_size = {length * CHAR_BIT},
      .bit_alignment = { CHAR_BIT },
    };
    hash_map_set(context->compilation->static_pointer_map, bytes, &combined->static_pointer_value);
    value_init(&combined->static_pointer_value, bits_descriptor, storage_none, source_range);
  }

  Slice *string = &combined->slice;
  *string = (Slice){bytes, length};
  Value *string_value = value_init(
    &combined->string_value,
    &descriptor_slice, storage_static(string), source_range
  );
  dyn_array_push(*stack, string_value);
}

#include "generated_tokenizer.c"

typedef struct {
  Value_View view;
  u32 index;
  bool done;
} Value_View_Split_Iterator;

static Value_View
token_split_next(
  Value_View_Split_Iterator *it,
  const Symbol *separator
) {
  if (it->done) return (Value_View){0};
  u32 start_index = it->index;
  for (
    ;
    it->index < it->view.length;
    it->index++
  ) {
    Value *token = value_view_get(it->view, it->index);
    if (value_is_symbol(token) && value_as_symbol(token) == separator) {
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
  Mass_Context *context,
  Value *value,
  Source_Range source_range
) {
  if (!value) return 0;
  if (value_is_tuple(value)) {
    const Tuple *tuple = value_as_tuple(value);
    return anonymous_struct_descriptor_from_tuple(context, tuple, Tuple_Eval_Mode_Type);
  }
  if (!mass_value_ensure_static_of(context, value, &descriptor_descriptor_pointer)) {
    return 0;
  }
  // Can't use `value_as_descriptor_pointer` because it might a user-generated version
  // of the type that does not pointer compare unless we memoize
  return *(Descriptor const **)storage_static_memory_with_bit_size(
    &value->storage, value->descriptor->bit_size
  );
}

static inline Value_View
value_view_match_till_symbol(
  Value_View view,
  u32 *peek_index,
  const Symbol *symbol
) {
  u32 start_index = *peek_index;
  for (; *peek_index < view.length; *peek_index += 1) {
    Value *token = value_view_get(view, *peek_index);
    if (value_is_symbol(token) && value_as_symbol(token) == symbol) {
      *peek_index += 1;
      return value_view_slice(&view, start_index, *peek_index - 1);
    }
  }
  return value_view_slice(&view, start_index, *peek_index);
}

static inline Value_View
value_view_match_till_end_of_statement(
  Mass_Context *context,
  Value_View view,
  u32 *peek_index
) {
  const Symbol *semicolon = context->compilation->common_symbols.operator_semicolon;
  return value_view_match_till_symbol(view, peek_index, semicolon);
}

static inline Value *
value_view_maybe_match_cached_symbol(
  Value_View view,
  u32 *peek_index,
  const Symbol *cached_symbol
) {
  Value *value = value_view_peek(view, *peek_index);
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

typedef struct {
  Slice name;
  Value *value;
} Token_Match_Arg;

static inline const Descriptor *
token_match_type(
  Mass_Context *context,
  Parser *parser,
  Value_View view
);

static Value *
token_parse_tuple(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  Value_View remaining = view;
  u32 match_length = 0;

  Value *result_value = 0;

  // Use temp allocator first for the parse
  Array_Value_Ptr items = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator);
  for (; remaining.length; remaining = value_view_rest(&remaining, match_length)) {
    if (mass_has_error(context)) goto err;
    const Symbol *end_symbol = context->compilation->common_symbols.operator_comma;
    Value *item = token_parse_expression(context, parser, remaining, &match_length, end_symbol);
    dyn_array_push(items, item);
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
  err:
  return result_value;
}

static Value *
token_parse_single(
  Mass_Context *context,
  Parser *parser,
  Value *value
) {
  if (value->descriptor == &descriptor_group_paren) {
    return token_parse_expression(context, parser, value_as_group_paren(value)->children, &(u32){0}, 0);
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

static Value *
mass_named_accessor(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *symbol_value = value_view_get(args, 0);
  if (!mass_value_ensure_static_of(context, symbol_value, &descriptor_symbol)) {
    return 0;
  }

  Named_Accessor named_accessor = {.symbol = value_as_symbol(symbol_value)};
  Value *result = value_init(
    mass_allocate(context, Value),
    &descriptor_named_accessor, storage_immediate(&named_accessor), args.source_range
  );
  return result;
}

static Value *
mass_quote(
  Mass_Context *context,
  Parser *parser,
  Value_View args,
  const void *payload
) {
  assert(args.length == 1);
  Value *value = value_view_get(args, 0);
  Quoted quoted = {.value = value};
  Value *result = value_init(
    mass_allocate(context, Value),
    &descriptor_quoted, storage_immediate(&quoted), args.source_range
  );
  return result;
}

static Value *
mass_unquote(
  Mass_Context *context,
  Parser *parser,
  Value_View args,
  const void *payload
) {
  assert(args.length == 1);
  return token_parse_single(context, parser, value_view_get(args, 0));
}

static Value *
mass_handle_statement_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *lazy_value
) {
  assert(mass_descriptor_is_void(mass_expected_result_descriptor(expected_result)));
  return value_force(context, builder, expected_result, lazy_value);
}

static inline const Descriptor *
token_match_type(
  Mass_Context *context,
  Parser *parser,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value *type_value = compile_time_eval(context, parser, view);
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
  u32 lhs_end = 0;
  u32 rhs_start = 0;
  bool found = false;
  for (u32 i = 0; i < view.length; ++i) {
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

static fn_type_opaque
mass_ensure_jit_function_for_value(
  Mass_Context *context,
  Value *value,
  const Source_Range *source_range
) {
  Jit *jit = &context->compilation->jit;

  Mass_Context jit_context = *context;
  jit_context.program = jit->program;

  // TODO check function signature
  Value *instance = ensure_function_instance(&jit_context, value, (Value_View){0});
  if (mass_has_error(context)) return 0;

  if (storage_is_label(&instance->storage)) {
    Label *label = instance->storage.Memory.location.Instruction_Pointer_Relative.label;
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
        .source_range = *source_range,
      });
      return 0;
    }
    return (fn_type_opaque)rip_value_pointer_from_label(label);
  } else {
    void const * const *address_memory = storage_static_memory_with_bit_size(&instance->storage, (Bits){64});
    return (fn_type_opaque)*address_memory;
  }
}

static Function_Parameter
token_match_argument(
  Mass_Context *context,
  Parser *parser,
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
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = slice_literal("Expected an expression after `::`"),
      });
      goto err;
    }
    // TODO @CopyPaste
    if (definition.length != 1 || !value_is_symbol(value_view_get(definition, 0))) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = slice_literal("Expected an argument name"),
      });
      goto err;
    }
    Value *name_token = value_view_get(definition, 0);
    Value *static_value = compile_time_eval(context, parser, static_expression);
    if (mass_has_error(context)) goto err;
    return (Function_Parameter) {
      .tag = Function_Parameter_Tag_Exact_Static,
      .Exact_Static = {
        .storage = static_value->storage,
      },
      .symbol = value_as_symbol(name_token),
      .descriptor = static_value->descriptor,
      .source_range = definition.source_range,
    };
  }

  if (token_maybe_split_on_operator(
    view, slice_literal("="), &definition, &default_expression, &equals
  )) {
    if (default_expression.length == 0) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = slice_literal("Expected an expression after `=`"),
      });
      goto err;
    }
  } else if (token_maybe_split_on_operator(
    view, slice_literal(":="), &definition, &default_expression, &equals
  )) {
    is_inferred_type = true;
    if (default_expression.length == 0) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = equals->source_range,
        .detailed_message = slice_literal("Expected an expression after `:=`"),
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
  Mass_Type_Constraint_Proc maybe_type_constraint = 0;
  if (is_inferred_type) {
    if (definition.length != 1 || !value_is_symbol(value_view_get(definition, 0))) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = slice_literal("Expected an argument name"),
      });
      goto err;
    }
    name_token = value_view_get(definition, 0);
  } else {
    Value_View name_tokens;
    Value *operator;
    generic = !token_maybe_split_on_operator(
      definition, slice_literal(":"), &name_tokens, &maybe_type_expression, &operator
    );
    if (generic) {
      if (token_maybe_split_on_operator(
        definition, slice_literal("~"), &name_tokens, &maybe_type_expression, &operator
      )) {
        Value *constraint = compile_time_eval(context, parser, maybe_type_expression);
        if (mass_has_error(context)) goto err;
        maybe_type_constraint = (Mass_Type_Constraint_Proc)mass_ensure_jit_function_for_value(
          context, constraint, &maybe_type_expression.source_range
        );
        if (mass_has_error(context)) goto err;
      } else {
        name_tokens = definition;
      }
    }
    if (name_tokens.length == 0) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = definition.source_range,
        .detailed_message = slice_literal("':' operator expects an identifier on the left hand side"),
      });
      goto err;
    }
    name_token = value_view_get(name_tokens, 0);
    if (name_tokens.length > 1 || !value_is_symbol(name_token)) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = name_tokens.source_range,
      });
      goto err;
    }
  }

  Value *maybe_default_value = 0;
  if (default_expression.length) {
    maybe_default_value = compile_time_eval(context, parser, default_expression);
    if (is_inferred_type) {
      descriptor = deduce_runtime_descriptor_for_value(context, maybe_default_value, 0);
      //if (!descriptor) {
        //mass_error(context, (Mass_Error) {
          //.tag = Mass_Error_Tag_No_Runtime_Use,
          //.source_range = maybe_default_value->source_range,
        //});
        //goto err;
      //}
    }
  }

  arg = (Function_Parameter) {
    .tag = generic ? Function_Parameter_Tag_Generic : Function_Parameter_Tag_Runtime,
    .maybe_default_value = maybe_default_value,
    .maybe_type_expression = maybe_type_expression,
    .maybe_type_constraint = maybe_type_constraint,
    .symbol = value_as_symbol(name_token),
    .descriptor = descriptor,
    .source_range = definition.source_range,
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
  const Expected_Result *expected_result,
  Value *value
) {
  if (mass_has_error(context)) return 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Value *result_value = value_init(
        mass_allocate(context, Value),
        expected_result->Exact.descriptor, expected_result->Exact.storage, value->source_range
      );
      mass_assign(context, builder, result_value, value, &value->source_range);
      if (mass_has_error(context)) return 0;
      // @Hack there should be a better and more robust way to do this
      if (
        !mass_value_is_compile_time_known(value) &&
        !storage_occupies_same_memory(&result_value->storage, &value->storage)
      ) {
        storage_release_if_temporary(builder, &value->storage);
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
  const Expected_Result *expected_result,
  Value *value
) {
  if (!value) return 0;

  if (value_is_lazy_value(value)) {
    const Lazy_Value *lazy = value_as_lazy_value(value);

    if (lazy->epoch.as_u64 != VALUE_STATIC_EPOCH.as_u64 && lazy->epoch.as_u64 != builder->epoch.as_u64) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Epoch_Mismatch,
        .source_range = value->source_range,
      });
      return 0;
    }
    Value *result = lazy->proc(context, builder, expected_result, &value->source_range, lazy->payload);
    if (mass_has_error(context)) return 0;
    // TODO is there a better way to cache the result?
    *value = *result;
    return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, value);
  }

  return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, value);
}

static inline void
value_force_exact(
  Mass_Context *context,
  Function_Builder *builder,
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
  Expected_Result expected_result = mass_expected_result_exact(target->descriptor, target->storage);
  Value *forced = value_force(context, builder, &expected_result, source);
  if (!forced) return;
  assert(forced->descriptor == target->descriptor);
  if (mass_descriptor_is_void(target->descriptor)) {
    assert(forced->descriptor == target->descriptor);
    assert(storage_equal(&forced->storage, &target->storage));
  }
}

static void
token_match_call_arguments(
  Mass_Context *context,
  Parser *parser,
  const Group_Paren *args_group,
  Array_Value_Ptr *out_args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  dyn_array_clear(*out_args);

  if (args_group->children.length == 0) return;

  Value_View_Split_Iterator it = { .view = args_group->children };

  while (!it.done) {
    if (context->result->tag != Mass_Result_Tag_Success) return;
    Value_View view = token_split_next(&it, context->compilation->common_symbols.operator_comma);
    Value *parse_result = token_parse_expression(context, parser, view, &(u32){0}, 0);
    dyn_array_push(*out_args, parse_result);
  }
}

static Value *
token_handle_user_defined_operator_proc(
  Mass_Context *context,
  Parser *parser,
  Value_View args,
  const Operator *operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  assert(operator->tag == Operator_Tag_Alias);
  u32 argument_count = operator->fixity == Operator_Fixity_Infix ? 2 : 1;
  assert(argument_count == args.length);

  Value *fn = mass_context_force_lookup(
    context, parser, parser->scope, operator->Alias.symbol, &args.source_range
  );
  if (mass_has_error(context)) return 0;

  Value *parsed_values[2];
  parsed_values[0] = token_parse_single(context, parser, value_view_get(args, 0));
  if (argument_count == 2) {
    parsed_values[1] = token_parse_single(context, parser, value_view_get(args, 1));
  }

  Value_View parsed_args = (Value_View){
    .values = parsed_values,
    .length = argument_count,
    .source_range = args.source_range,
  };

  return token_handle_function_call(context, parser, fn, parsed_args, parsed_args.source_range);
}

static inline Value *
mass_make_lazy_value(
  Mass_Context *context,
  Parser *parser,
  Source_Range source_range,
  void *payload,
  const Descriptor *descriptor,
  Lazy_Value_Proc proc
) {
  allocator_allocate_bulk(context->allocator, combined, {
    Lazy_Value lazy_value;
    Value value;
  });

  Lazy_Value *lazy = &combined->lazy_value;
  *lazy = (Lazy_Value) {
    .epoch = parser->epoch,
    .descriptor = descriptor,
    .proc = proc,
    .payload = payload,
  };
  return value_init(
    &combined->value,
    &descriptor_lazy_value, storage_static(lazy), source_range
  );
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
  assert(args.length == 2);
  assert(value_match_symbol(value_view_get(args, 0), slice_literal("exports")));
  Value *tuple_value = token_parse_single(context, parser, value_view_get(args, 1));
  if (mass_has_error(context)) return 0;
  if (!mass_value_ensure_static_of(context, tuple_value, &descriptor_tuple)) {
    return 0;
  }
  const Tuple *tuple = value_as_tuple(tuple_value);

  Module_Exports *export = mass_allocate(context, Module_Exports);
  *export = (Module_Exports) {
    .tag = Module_Exports_Tag_Selective,
    .Selective = {
      .symbols = dyn_array_make(Array_Value_Ptr, .capacity = dyn_array_length(tuple->items)),
    },
    .source_range = args.source_range,
  };

  for (u64 tuple_index = 0; tuple_index < dyn_array_length(tuple->items); ++tuple_index) {
    Value *tuple_item = *dyn_array_get(tuple->items, tuple_index);
    if (!mass_value_ensure_static_of(context, tuple_item, &descriptor_named_accessor)) {
      return 0;
    }
    // TODO support renaming and wildcard exports
    const Symbol *symbol = value_as_named_accessor(tuple_item)->symbol;
    Value *symbol_value = value_make(
      context->allocator, &descriptor_symbol_pointer, storage_immediate(&symbol), tuple_item->source_range
    );
    dyn_array_push(export->Selective.symbols, symbol_value);
  }

  Value *result = value_make(
    context->allocator, &descriptor_module_exports, storage_static(export), args.source_range
  );
  return result;
}

static u32
token_parse_operator_definition(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_data
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u32 peek_index = 0;
  Value *keyword_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator
  );
  if (!keyword_token) return 0;
  Value *precedence_token = value_view_next(view, &peek_index);
  if (!precedence_token) { context_parse_error(context, parser, view, peek_index); goto err; }
  Value *pattern_token = value_view_next(view, &peek_index);
  if (!value_is_group_paren(pattern_token)) { context_parse_error(context, parser, view, peek_index); goto err; }

  Value_View rest = value_view_rest(&view, peek_index);
  u32 body_length;
  Value *body = token_parse_expression(
    context, parser, rest, &body_length, context->compilation->common_symbols.operator_semicolon
  );
  if (mass_has_error(context)) goto err;

  if (!body_length) { context_parse_error(context, parser, view, peek_index); goto err; }
  peek_index += body_length;

  Value *precedence_value = token_parse_single(context, parser, precedence_token);
  if (!value_is_i64(precedence_value)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = precedence_token->source_range,
      .detailed_message ="Expected a precedence literal number",
    });
    goto err;
  }
  u32 precendence = u64_to_u32(value_as_i64(precedence_value)->bits);

  Value_View definition = value_as_group_paren(pattern_token)->children;

  Value *operator_token;
  Value *arguments[2] = {0};
  Operator_Fixity fixity = Operator_Fixity_Prefix;
  u16 argument_count = 1;

  // prefix and postfix
  if (definition.length == 2) {
    Value *first =  value_view_get(definition, 0);
    bool is_first_operator_like = false;
    if (value_is_symbol(first)) {
      Slice operator_string = value_as_symbol(first)->name;
      is_first_operator_like = operator_string.length && !isalpha(operator_string.bytes[0]);
    }
    fixity = is_first_operator_like ? Operator_Fixity_Prefix : Operator_Fixity_Postfix;
    if (fixity == Operator_Fixity_Prefix) {
      operator_token = value_view_get(definition, 0);
      arguments[0] = value_view_get(definition, 1);
    } else {
      operator_token = value_view_get(definition, 1);
      arguments[0] = value_view_get(definition, 0);
    }
  } else if (definition.length == 3) { // infix
    argument_count = 2;
    fixity = Operator_Fixity_Infix;
    operator_token = value_view_get(definition, 1);
    arguments[0] = value_view_get(definition, 0);
    arguments[1] = value_view_get(definition, 2);
  } else {
    operator_token = 0;
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = pattern_token->source_range,
      .detailed_message ="Expected the pattern to have two (for prefix / postfix) or three tokens"
    });
    goto err;
  }

  for (u8 i = 0; i < argument_count; ++i) {
    if (!value_is_symbol(arguments[i])) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Invalid_Identifier,
        .source_range = arguments[i]->source_range,
      });
      goto err;
    }
  }

  Operator *operator = mass_allocate(context, Operator);

  if (value_is_symbol(body)) {
    const Symbol *alias = value_as_symbol(body);
    *operator = (Operator){
      .tag = Operator_Tag_Alias,
      .fixity = fixity,
      .precedence = precendence,
      .Alias = {
        .symbol = alias,
        .handler = token_handle_user_defined_operator_proc,
      },
    };
  } else if (value_is_intrinsic(body)) {
    *operator = (Operator){
      .tag = Operator_Tag_Intrinsic,
      .fixity = fixity,
      .precedence = precendence,
      .Intrinsic = { .body = body },
    };
  } else {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = body->source_range,
      .detailed_message = slice_literal("Expected a valid operator body"),
    });
    goto err;
  }

  scope_define_operator(
    context,
    parser->scope,
    keyword_token->source_range,
    value_as_symbol(operator_token)->name,
    operator
  );

  err:
  return peek_index;
}

static Value *
mass_import(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  if (args.length != 1) goto parse_err;
  Value *file_path_value = value_view_get(args, 0);
  if (file_path_value->descriptor != &descriptor_slice) goto parse_err;
  Slice file_path = *value_as_slice(file_path_value);

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
      Scope *module_scope = scope_make(context->allocator, root_scope);
      module = program_module_from_file(context, file_path, module_scope);
      program_import_module(context, module);
      if (mass_has_error(context)) return 0;
      hash_map_set(context->compilation->module_map, file_path, module);
    }
  }

  return value_make(context->allocator, &descriptor_module, storage_static(module), args.source_range);

  parse_err:
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = args.source_range,
    .detailed_message ="import() expects a single string argument"
  });
  return 0;
}

static void
mass_push_token_matcher(
  Mass_Context *context,
  Parser *parser,
  Token_Statement_Matcher_Proc proc,
  void *payload
) {
  Token_Statement_Matcher *matcher =
    mass_allocate(context, Token_Statement_Matcher);
  *matcher = (Token_Statement_Matcher){
    .previous = parser->scope->statement_matcher,
    .proc = proc,
    .payload = payload,
  };
  parser->scope->statement_matcher = matcher;
}

typedef struct {
  Value *condition;
  Value *body;
} Mass_While;

// TODO move this to user land (again)
static Value *
mass_handle_while_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_While *payload
) {
  Program *program = context->program;
  Label *continue_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("continue"));

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .Label.pointer = continue_label,
  });

  Expected_Result expected_condition = expected_result_any(&descriptor__bool);
  Value *condition = value_force(context, builder, &expected_condition, payload->condition);
  if (mass_has_error(context)) return 0;

  Label *break_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("break"));

  encode_inverted_conditional_jump(builder, break_label, &condition->source_range, condition);
  storage_release_if_temporary(builder, &condition->storage);

  Value *void_value = mass_make_void(context->allocator, *source_range);
  value_force_exact(context, builder, void_value, payload->body);
  if (mass_has_error(context)) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, payload->body->source_range,
    &(Instruction_Assembly){jmp, {code_label32(continue_label)}}
  );

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .Label.pointer = break_label,
  });

  return void_value;
}

static u32
token_parse_while(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *payload
) {
  if (mass_has_error(context)) return 0;

  u32 peek_index = 0;
  Value *keyword_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols._while
  );
  if (!keyword_token) return 0;

  u32 condition_start_index = peek_index;
  for (; peek_index < view.length; peek_index += 1) {
    Value *token = value_view_get(view, peek_index);
    if (value_is_ast_block(token)) {
      break;
    }
  }
  Value_View condition_view = value_view_slice(&view, condition_start_index, peek_index);
  if (peek_index == view.length) {
    return mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
    });
  }

  Value *body_token = value_view_next(view, &peek_index);
  assert(value_is_ast_block(body_token));
  if (view.length != peek_index) {
    Value *semicolon = value_view_next(view, &peek_index);
    const Symbol* symbol = context->compilation->common_symbols.operator_semicolon;
    if (!value_is_symbol(semicolon) || value_as_symbol(semicolon) != symbol) {
      return mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = semicolon->source_range,
      });
    }
  }

  Mass_While *lazy_payload = mass_allocate(context, Mass_While);
  *lazy_payload = (Mass_While) {
    .condition = token_parse_expression(context, parser, condition_view, &(u32){0}, 0),
    .body = token_parse_single(context, parser, body_token),
  };

  out_lazy_value->proc = mass_handle_while_lazy_proc;
  out_lazy_value->payload = lazy_payload;

  return peek_index;
}

static Value *
mass_c_struct(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 2);
  assert(value_match_symbol(value_view_get(args, 0), slice_literal("c_struct")));

  Value *tuple_value = token_parse_single(context, parser, value_view_get(args, 1));
  const Tuple *tuple = value_as_tuple(tuple_value);

  Descriptor *descriptor = anonymous_struct_descriptor_from_tuple(
    context, tuple, Tuple_Eval_Mode_Type
  );
  assert(descriptor->tag == Descriptor_Tag_Struct);
  descriptor->brand = mass_allocate(context, Symbol);

  Value *result = mass_allocate(context, Value);
  *result = MASS_TYPE_VALUE(descriptor);

  return result;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

static inline const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
) {
  if (value->descriptor == &descriptor_lazy_value && value->storage.tag == Storage_Tag_Static) {
    const Lazy_Value *lazy = value_as_lazy_value(value);
    return lazy->descriptor;
  }
  return value->descriptor;
}

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
    .scope = scope_make(context->allocator, parser->scope),
    .module = parser->module,
  };
  Value *expression_result_value = token_parse_expression(&eval_context, &eval_parser, view, &(u32){0}, 0);
  if(mass_has_error(&eval_context)) {
    context->result = eval_context.result;
    return 0;
  }
  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(expression_result_value);

  static Slice eval_name = slice_literal_fields("$compile_time_eval$");
  Function_Info fn_info;
  function_info_init(&fn_info, function_return_exact(result_descriptor, *source_range));

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
    &eval_context, &eval_builder, &expected_result, expression_result_value
  );
  if (mass_has_error(context)) return 0;

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
    .storage = storage_register(out_register, (Bits){64}),
  };
  Value result_address = {
    .descriptor = &descriptor_s64,
    .storage = imm64((u64)result),
  };

  // Use memory-indirect addressing to copy
  Storage out_storage = storage_indirect(result_descriptor->bit_size, out_register);
  Value *out_value = value_make(eval_context.allocator, result_descriptor, out_storage, *source_range);

  mass_assign(&eval_context, &eval_builder, &out_value_register, &result_address, source_range);
  if (mass_has_error(context)) return 0;

  mass_assign(&eval_context, &eval_builder, out_value, forced_value, source_range);
  if (mass_has_error(context)) return 0;

  calling_convention_x86_64_common_end_proc(jit->program, &eval_builder);
  dyn_array_push(jit->program->functions, eval_builder);

  program_jit(context, jit);
  if (mass_has_error(context)) return 0;

  fn_type_opaque jitted_code = (fn_type_opaque)rip_value_pointer_from_label(eval_label);
  jitted_code();

  Storage result_storage = mass_descriptor_is_void(result_descriptor)
    ? storage_none
    : storage_immediate_with_bit_size(result, result_descriptor->bit_size);
  return value_make(context->allocator, out_value->descriptor, result_storage, *source_range);
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
mass_handle_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *target_descriptor = payload->target;
  Value *expression = payload->expression;
  const Descriptor *source_descriptor = value_or_lazy_value_descriptor(expression);

  Expected_Result expected_source = expected_result_any(source_descriptor);
  Value *value = value_force(context, builder, &expected_source, expression);
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
      panic("TODO user error or trying to cast to a larger type");
      return 0;
    } else {
      Storage result_storage = value->storage;
      if (cast_to_bit_size.as_u64 < original_bit_size.as_u64) {
        if (result_storage.tag == Storage_Tag_Static) {
          const void *memory = get_static_storage_with_bit_size(&value->storage, original_bit_size);
          result_storage = storage_static_heap(memory, cast_to_bit_size);
        } else {
          result_storage.bit_size = cast_to_bit_size;
        }
      } else {
        result_storage = value->storage;
      }
      result_value = value_init(
        mass_allocate(context, Value),
        target_descriptor, result_storage, *source_range
      );
      // TODO This is awkward and there might be a better way.
      //      It is also might be necessary to somehow mark the original value as invalid maybe?
      result_value->storage.flags |= (value->storage.flags & Storage_Flags_Temporary);
    }
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, result_value
  );
}

static Value *
mass_handle_tuple_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Cast_Lazy_Payload *payload
) {
  const Allocator *allocator = context->allocator;
  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);
  assert(expected_descriptor == payload->target);
  assert(payload->target->tag == Descriptor_Tag_Struct);
  Value *result = mass_value_from_expected_result(allocator, builder, expected_result, *source_range);
  mass_assign(context, builder, result, payload->expression, source_range);
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

  if (
    expression->descriptor == &descriptor_tuple &&
    mass_value_is_compile_time_known(expression)
  ) {
    Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
    *heap_payload = lazy_payload;

    return mass_make_lazy_value(
      context, parser, source_range, heap_payload, target_descriptor, mass_handle_tuple_cast_lazy_proc
    );
  }

  if (mass_value_is_compile_time_known(expression)) {
    Expected_Result expected_result = expected_result_any(target_descriptor);
    return mass_handle_cast_lazy_proc(context, 0, &expected_result, &source_range, &lazy_payload);
  }

  Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
  *heap_payload = lazy_payload;

  return mass_make_lazy_value(
    context, parser, source_range, heap_payload, target_descriptor, mass_handle_cast_lazy_proc
  );
}

static Value *
mass_cast(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  if (mass_has_error(context)) return 0;
  assert(args_view.length == 2);
  const Descriptor *target_descriptor = value_ensure_type(
    context, value_view_get(args_view, 0), args_view.source_range
  );
  Value *expression = value_view_get(args_view, 1);
  return mass_cast_helper(context, parser, target_descriptor, expression, args_view.source_range);
}

static void
token_dispatch_operator(
  Mass_Context *context,
  Parser *parser,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *operator_entry
);

static bool
token_handle_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Array_Value_Ptr *stack,
  Array_Operator_Stack_Entry *operator_stack,
  const Operator *operator,
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
    token_dispatch_operator(context, parser, stack, last_operator);
  }
  dyn_array_push(*operator_stack, (Operator_Stack_Entry) {
    .source_range = source_range,
    .operator = operator,
  });
  return true;
}

static u32
token_parse_constant_definitions(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Value_View lhs;
  Value_View rhs;
  Value *operator;

  u32 statement_length = 0;
  view = value_view_match_till_end_of_statement(context, view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal("::"), &lhs, &rhs, &operator)) {
    return 0;
  }
  if (lhs.length > 1) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unimplemented,
      .source_range = lhs.source_range,
      .detailed_message = slice_literal("Multiple assignment are not supported at the moment")
    });
    goto err;
  }
  Value *symbol = value_view_get(view, 0);
  if (value_is_group_paren(symbol)) {
    symbol = compile_time_eval(context, parser, value_as_group_paren(symbol)->children);
    if (mass_has_error(context)) goto err;
  }

  if (!value_is_symbol(symbol)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Invalid_Identifier,
      .source_range = symbol->source_range,
    });
    goto err;
  }

  scope_define_lazy_compile_time_expression(context, parser, parser->scope, value_as_symbol(symbol), rhs);

  err:
  return statement_length;
}

static Value *
mass_macro_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *body_value
) {
  Value *result_value =
    mass_value_from_expected_result(context->allocator, builder, expected_result, *source_range);

  Label *saved_return_label = builder->code_block.end_label;
  Value saved_return_value = builder->return_value;
  {
    builder->code_block.end_label = make_label(
      context->allocator,
      context->program,
      &context->program->memory.code,
      slice_literal("macro return")
    );
    builder->return_value = *result_value;
    value_force_exact(context, builder, result_value, body_value);

    if (mass_has_error(context)) return 0;

    push_instruction( &builder->code_block, (Instruction) {
      .tag = Instruction_Tag_Label,
      .Label.pointer = builder->code_block.end_label,
    });
  }
  builder->code_block.end_label = saved_return_label;
  builder->return_value = saved_return_value;

  return result_value;
}

static Value *
mass_macro_temp_param_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *arg
) {
  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);

  // @InstructionQuality
  // Would be nice to get rid of the stack allocation, but we would need to somehow
  // track which registers were allocated for macro arguments so that they are properly
  // released upon returning.
  Storage stack_storage = reserve_stack_storage(builder, expected_descriptor->bit_size);
  Value *forced = value_init(
    mass_allocate(context, Value),
    expected_descriptor, stack_storage, *source_range
  );
  value_force_exact(context, builder, forced, arg);
  return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, forced);
}

static inline Value *
mass_handle_macro_call(
  Mass_Context *context,
  Parser *parser,
  Value *overload,
  Value_View args_view,
  Source_Range source_range
) {
  const Function_Literal *literal = value_as_function_literal(overload);
  assert(literal->flags & Function_Literal_Flags_Macro);

  // We make a nested scope based on function's original scope
  // instead of current scope for hygiene reasons. I.e. function body
  // should not have access to locals inside the call scope.
  Scope *body_scope = scope_make(context->allocator, literal->own_scope);

  for(u64 i = 0; i < dyn_array_length(literal->info->parameters); ++i) {
    if (mass_has_error(context)) goto err;
    Function_Parameter *param = dyn_array_get(literal->info->parameters, i);
    if (param->symbol) {
      Value *arg_value;
      if (i >= args_view.length) {
        arg_value = param->maybe_default_value;
      } else {
        arg_value = value_view_get(args_view, i);
      }

      Epoch arg_epoch =
        mass_value_is_compile_time_known(arg_value) ? VALUE_STATIC_EPOCH : parser->epoch;

      bool needs_casting = (
        // FIXME pass in resolved Function_Info from call and remove a guard on the next line
        param->descriptor &&
        !same_type(param->descriptor, arg_value->descriptor)
      );

      // Macro parameters, like function ones are expected to be non-lazy values
      // in the compiler and should not have side effects when accessed multiple times
      // in the body of the macro.
      //
      // This assumption holds if the argument is exactly the right type and is statically known
      Value *param_value;
      if (mass_value_is_compile_time_known(arg_value) && !needs_casting) {
        param_value = arg_value;
      } else {
        // Otherwise we will create a temp copy
        // TODO should this be forced or is first access ok?
        param_value = mass_make_lazy_value(
          context, parser, param->source_range,
          arg_value, param->descriptor,
          mass_macro_temp_param_lazy_proc
        );
      }

      scope_define_value(
        body_scope, arg_epoch, param->source_range, param->symbol, param_value
      );
    }
  }

  Parser body_parser = *parser;
  body_parser.scope = body_scope;

  Array_Value_View statements = value_as_ast_block(literal->body)->statements;
  Value *body_value = token_parse_block_statements(context, &body_parser, statements, &source_range);
  if (mass_has_error(context)) goto err;

  const Descriptor *actual_return_descriptor = value_or_lazy_value_descriptor(body_value);
  switch(literal->info->returns.tag) {
    case Function_Return_Tag_Inferred: {
      // Accept whatever the actual return type is
    } break;
    case Function_Return_Tag_Exact: {
      const Descriptor *expected = literal->info->returns.Exact.descriptor;
      // TODO should this actually perform a cast?
      if (!same_type_or_can_implicitly_move_cast(expected, actual_return_descriptor)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = body_value->source_range,
          .Type_Mismatch = { .expected = expected, .actual = actual_return_descriptor },
        });
        return 0;
      }
    } break;
    case Function_Return_Tag_Generic: {
      panic("TODO");
    } break;
  }

  if (mass_value_is_compile_time_known(body_value)) {
    return body_value;
  }

  Source_Range return_range = literal->info->returns.source_range;
  return mass_make_lazy_value(
    context, parser, return_range, body_value, actual_return_descriptor, mass_macro_lazy_proc
  );

  err:
  return 0;
}

typedef struct {
  Storage reg;
  Storage stack;
} Saved_Register;
typedef dyn_array_type(Saved_Register) Array_Saved_Register;

typedef struct {
  Array_Value_Ptr args;
  Value *overload;
} Mass_Function_Call_Lazy_Payload;

static void
mass_assert_storage_is_valid_in_context(
  const Storage *storage,
  const Mass_Context *context
) {
  switch(storage->tag) {
    case Storage_Tag_Immediate:
    case Storage_Tag_Static: {
      assert(context->program == context->compilation->jit.program);
    } break;
    case Storage_Tag_Xmm:
    case Storage_Tag_None:
    case Storage_Tag_Eflags:
    case Storage_Tag_Unpacked:
    case Storage_Tag_Register: {
      // Assume valid?
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
call_function_overload(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Function_Call_Lazy_Payload *payload
) {
  Value *to_call = payload->overload;
  Array_Value_Ptr arguments = payload->args;

  Value_View args_view = value_view_from_value_array(arguments, source_range);
  Value *instance = ensure_function_instance(context, to_call, args_view);
  if (mass_has_error(context)) return 0;
  assert(instance->descriptor->tag == Descriptor_Tag_Function_Instance);
  const Descriptor_Function_Instance *instance_descriptor = &instance->descriptor->Function_Instance;
  const Function_Info *fn_info = instance_descriptor->info;

  mass_assert_storage_is_valid_in_context(&instance->storage, context);

  const Descriptor *return_descriptor = function_return_as_exact(&fn_info->returns)->descriptor;
  Value *fn_return_value;
  Storage return_storage = storage_none;
  if (!mass_descriptor_is_void(return_descriptor)) {
    return_storage = instance_descriptor->call_setup.caller_return;
    return_storage.flags |= Storage_Flags_Temporary;
  }
  fn_return_value = value_make(context->allocator, return_descriptor, return_storage, *source_range);

  const Function_Call_Setup *call_setup = &instance_descriptor->call_setup;

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
  u64 argument_register_bitset = 0;
  u64 copied_straight_to_param_bitset = 0;
  u64 temp_register_argument_bitset = 0;
  for (u64 i = 0; i < dyn_array_length(call_setup->parameters); ++i) {
    Function_Call_Parameter *target_item = dyn_array_get(call_setup->parameters, i);
    Value *source_arg;
    Value *target_arg = dyn_array_push_uninitialized(target_params);
    value_init(target_arg, target_item->descriptor, target_item->storage, *source_range);
    if (i >= dyn_array_length(arguments)) {
      if (target_item->flags & Function_Call_Parameter_Flags_Uninitialized) {
        Storage source_storage = reserve_stack_storage(builder, target_arg->descriptor->bit_size);
        source_arg = value_init(
          mass_allocate(context, Value),
          target_arg->descriptor, source_storage, *source_range
        );
      } else {
        Function_Parameter *declared_argument = dyn_array_get(fn_info->parameters, i);
        source_arg = declared_argument->maybe_default_value;
      }
    } else {
      source_arg = *dyn_array_get(arguments, i);
    }
    const Descriptor *stack_descriptor = target_item->descriptor;
    if (descriptor_is_implicit_pointer(stack_descriptor)) {
      stack_descriptor = stack_descriptor->Pointer_To.descriptor;
    }
    bool source_is_stack = (
      storage_is_stack(&source_arg->storage) &&
      descriptor_is_implicit_pointer(source_arg->descriptor)
    );
    bool should_assign = !(target_item->flags & Function_Call_Parameter_Flags_Uninitialized);

    u64 target_arg_register_bitset = register_bitset_from_storage(&target_arg->storage);
    if (target_arg_register_bitset >> 16) {
      panic("Found XMM usage");
    }

    argument_register_bitset |= target_arg_register_bitset;
    bool target_arg_registers_are_free =
      !(builder->register_occupied_bitset.bits & target_arg_register_bitset);
    bool can_assign_straight_to_target = (
      target_arg_registers_are_free &&
      !descriptor_is_implicit_pointer(target_item->descriptor)
    );

    bool can_use_source_registers = false;
    u64 source_registers_bitset = 0;
    if (
      source_arg->descriptor != &descriptor_lazy_value &&
      !mass_descriptor_is_void(source_arg->descriptor) &&
      source_arg->storage.tag != Storage_Tag_Static &&
      source_arg->storage.tag != Storage_Tag_Immediate &&
      !descriptor_is_implicit_pointer(target_item->descriptor)
    ) {
      source_registers_bitset = register_bitset_from_storage(&source_arg->storage);
      if (!(all_used_arguments_register_bitset & source_registers_bitset)) {
        // Check that we haven't accidentally acquired this register as temp for something else
        assert(!(temp_register_argument_bitset & source_registers_bitset));
        can_use_source_registers = true;
      }
      // Just a sanity check that if a source value is in a register,
      // that register is tracked by the builder.
      if (source_registers_bitset) {
        assert(builder->register_occupied_bitset.bits & source_registers_bitset);
      }
    }

    Value *arg_value;
    if (storage_is_stack(&target_arg->storage)) {
      arg_value = value_init(
        mass_allocate(context, Value),
        stack_descriptor, target_arg->storage, *source_range
      );
    } else if (source_is_stack) {
      arg_value = source_arg;
      should_assign = false;
    } else if (can_use_source_registers) {
      arg_value = source_arg;
      should_assign = false;
    } else if (
      mass_value_is_compile_time_known(source_arg) &&
      !descriptor_is_implicit_pointer(target_item->descriptor)
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
        | builder->register_occupied_bitset.bits;
      u64 allowed_temp_registers = registers_that_can_be_temp & ~prohibited_registers;
      u64 required_register_count = register_bitset_occupied_count(target_arg_register_bitset);
      if (
        // TODO it should be possible to do this for unpacked structs as well,
        //      but it will be quite gnarly
        !descriptor_is_implicit_pointer(target_item->descriptor)&&
        required_register_count == 1 &&
        register_bitset_occupied_count(allowed_temp_registers) > 1
      ) {
        Register temp_register = register_find_available(builder, prohibited_registers);
        register_acquire(builder, temp_register);
        register_bitset_set(&temp_register_argument_bitset, temp_register);
        arg_value = value_init(
          mass_allocate(context, Value),
          target_item->descriptor,
          storage_register(temp_register, target_item->descriptor->bit_size),
          source_arg->source_range
        );
        arg_value->storage.flags |= Storage_Flags_Temporary;
      } else {
        // The code below is useful to check how many spills to stack happen
        //static int stack_counter = 0;
        //printf(" > stack %i\n", stack_counter++);
        Storage stack_storage = reserve_stack_storage(builder, stack_descriptor->bit_size);
        arg_value = value_init(
          mass_allocate(context, Value),
          stack_descriptor, stack_storage, *source_range
        );
        arg_value->storage.flags |= Storage_Flags_Temporary;
      }
    };
    if (should_assign) {
      mass_assign(context, builder, arg_value, source_arg, source_range);
      if (mass_has_error(context)) return 0;
    }
    dyn_array_push(temp_arguments, arg_value);
  }

  u64 target_volatile_registers_bitset = call_setup->calling_convention->register_volatile_bitset.bits;
  u64 expected_result_bitset = 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      const Storage *expected = &expected_result->Exact.storage;
      if (expected->tag == Storage_Tag_Register || expected->tag == Storage_Tag_Unpacked) {
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
        &builder->code_block, *source_range,
        &(Instruction_Assembly){mov, {saved->stack, saved->reg}}
      );
    }
  }

  register_release_bitset(builder, saved_registers_from_arguments_bitset);
  u64 spilled_param_register_bitset = argument_register_bitset & ~copied_straight_to_param_bitset;
  register_acquire_bitset(builder, spilled_param_register_bitset);

  for (u64 i = 0; i < dyn_array_length(target_params); ++i) {
    Value *param = dyn_array_get(target_params, i);

    if (storage_is_stack(&param->storage)) continue;

    Value *source_arg = *dyn_array_get(temp_arguments, i);
    if (storage_is_indirect(&param->storage)) {
      Register base_register = param->storage.Memory.location.Indirect.base_register;
      Storage target_storage = storage_register(base_register, (Bits){64});
      Storage source_storage = storage_adjusted_for_lea(source_arg->storage);

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){lea, {target_storage, source_storage}}
      );
    } else {
      mass_assign(context, builder, param, source_arg, source_range);
      if (mass_has_error(context)) return 0;
    }
  }

  builder->max_call_parameters_stack_size = u32_max(
    builder->max_call_parameters_stack_size,
    call_setup->parameters_stack_size
  );

  switch(call_setup->jump.tag) {
    case Function_Call_Jump_Tag_Call: {
      if (instance->storage.tag == Storage_Tag_Static || instance->storage.tag == Storage_Tag_Immediate) {
        Register temp_reg = register_acquire_temp(builder);
        Storage reg = storage_register(temp_reg, (Bits){64});
        move_value(builder, source_range, &reg, &instance->storage);
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
      Storage syscal_number_storage = storage_register(Register_A, (Bits){64});
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

  assert(!(return_value_bitset & saved_registers_from_arguments_bitset));
  DYN_ARRAY_FOREACH(Saved_Register, saved, stack_saved_registers) {
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mov, {saved->reg, saved->stack}}
    );
  }

  register_acquire_bitset(builder, saved_registers_from_arguments_bitset);
  register_acquire_bitset(builder, (return_value_bitset & ~expected_result_bitset));

  Value *expected_value = mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, fn_return_value
  );

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
  Mass_Context *context,
  Function_Info *info,
  Scope *arguments_scope
) {
  Mass_Context temp_context = mass_context_from_compilation(context->compilation);
  Parser args_parser = {
    .flags = Parser_Flags_None,
    .scope = scope_make(temp_context.temp_allocator, arguments_scope),
  };

  Temp_Mark temp_mark = context_temp_mark(&temp_context);

  DYN_ARRAY_FOREACH(Function_Parameter, param, info->parameters) {
    Source_Range source_range = param->source_range;
    if (param->descriptor) {
      Storage storage = storage_immediate(&param->descriptor);
      Value *param_value = value_make(
        temp_context.temp_allocator, &descriptor_descriptor_pointer, storage, source_range
      );
      scope_define_value(
        args_parser.scope, VALUE_STATIC_EPOCH, source_range, param->symbol, param_value
      );
    } else {
      scope_define_lazy_compile_time_expression(
        &temp_context, &args_parser, args_parser.scope, param->symbol, param->maybe_type_expression
      );
    }
  }

  DYN_ARRAY_FOREACH(Function_Parameter, param, info->parameters) {
    if (param->descriptor) continue;
    const Symbol *symbol = param->symbol;
    Source_Range source_range = param->source_range;
    Value *type_value =
      mass_context_force_lookup(&temp_context, &args_parser, args_parser.scope, symbol, &source_range);
    if (mass_has_error(&temp_context)) goto err;
    param->descriptor = value_ensure_type(
      &temp_context, type_value, source_range
    );
    if (mass_has_error(&temp_context)) goto err;
    assert(param->descriptor);
  }

  switch(info->returns.tag) {
    case Function_Return_Tag_Inferred: {
      if (info->flags & Function_Info_Flags_Intrinsic) {
        // :IntrinsicReturnType
        // there is no fn body to infer this from so will be handled elsewhere
      } else {
        panic("TODO");
      }
    } break;
    case Function_Return_Tag_Exact: {
      // Nothing to do, we already know the type
    } break;
    case Function_Return_Tag_Generic: {
      const Descriptor *descriptor =
        token_match_type(&temp_context, &args_parser, info->returns.Generic.type_expression);
      info->returns = (Function_Return) {
        .tag = Function_Return_Tag_Exact,
        .source_range = info->returns.source_range,
        .Exact = { .descriptor = descriptor },
      };
    } break;
  }

  err:
  context_temp_reset_to_mark(&temp_context, temp_mark);
}

typedef struct {
  Value_View view;
  bool all_arguments_are_compile_time_known;
} Mass_Overload_Match_Args;

typedef enum {
  Mass_Argument_Scoring_Flags_None = 0 << 0,
  Mass_Argument_Scoring_Flags_Prefer_Compile_Time = 1 << 0,
} Mass_Argument_Scoring_Flags;

static s64
calculate_arguments_match_score(
  Mass_Context *context,
  const Function_Info *descriptor,
  Value_View args_view,
  Mass_Argument_Scoring_Flags scoring_flags
) {
  const s64 MAX_ARG_COUNT = 255;
  const s64 Score_Cast = 1;
  const s64 Score_Generic = Score_Cast << 8;
  const s64 Score_Generic_Constrained = Score_Generic << 8;
  const s64 Score_Same_Type = Score_Generic_Constrained << 8;
  const s64 Score_Same_Type_Static = Score_Same_Type << 8;
  const s64 Score_Exact_Static = Score_Same_Type_Static << 8;

  assert(args_view.length < MAX_ARG_COUNT);
  s64 score = 0;
  if (args_view.length > dyn_array_length(descriptor->parameters)) return -1;
  for (u64 arg_index = 0; arg_index < dyn_array_length(descriptor->parameters); ++arg_index) {
    Function_Parameter *param = dyn_array_get(descriptor->parameters, arg_index);
    const Descriptor *target_descriptor = param->descriptor;
    Value *source_arg = 0;
    const Descriptor *source_descriptor;
    if (arg_index >= args_view.length) {
      if (!param->maybe_default_value) return -1;
      source_arg = param->maybe_default_value;
    } else {
      source_arg = value_view_get(args_view, arg_index);
    }
    switch(param->tag) {
      case Function_Parameter_Tag_Runtime: {
        source_descriptor = value_or_lazy_value_descriptor(source_arg);
        if (same_type(target_descriptor, source_descriptor)) {
          if (scoring_flags & Mass_Argument_Scoring_Flags_Prefer_Compile_Time) {
            score += Score_Same_Type_Static;
          } else {
            score += Score_Same_Type;
          }
        } else {
          if (mass_value_is_compile_time_known(source_arg)) {
            source_descriptor = deduce_runtime_descriptor_for_value(
              context, source_arg, target_descriptor
            );
            if (!source_descriptor) return -1;
          }
          if (same_type_or_can_implicitly_move_cast(target_descriptor, source_descriptor)) {
            score += Score_Cast;
          } else {
            return -1;
          }
        }
      } break;
      case Function_Parameter_Tag_Exact_Static: {
        if (!mass_value_is_compile_time_known(source_arg)) return -1;
        if (!storage_static_equal(
          target_descriptor, &param->Exact_Static.storage,
          source_arg->descriptor, &source_arg->storage
        )) return -1;
        score += Score_Exact_Static;
      } break;
      case Function_Parameter_Tag_Generic: {
        if (param->maybe_type_constraint) {
          score += Score_Generic_Constrained;
        } else {
          score += Score_Generic;
        }
      } break;
    }
  }
  return score;
}

static inline bool
match_overload_argument_count(
  const Function_Info *info,
  u64 actual_count
) {
  if (actual_count > dyn_array_length(info->parameters)) return false;
  if (actual_count == dyn_array_length(info->parameters)) return true;
  // FIXME @Speed cache this as a range
  for (u64 arg_index = 0; arg_index < dyn_array_length(info->parameters); ++arg_index) {
    Function_Parameter *param = dyn_array_get(info->parameters, arg_index);
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
  struct Overload_Match_State *match,
  struct Overload_Match_State *best_conflict_match
) {
  if (value_is_overload(candidate)) {
    const Overload *overload = value_as_overload(candidate);
    mass_match_overload_candidate(context, overload->value, args, match, best_conflict_match);
    mass_match_overload_candidate(context, overload->next, args, match, best_conflict_match);
  } else {
    const Function_Info *overload_info;
    if (value_is_function_literal(candidate)) {
      const Function_Literal *literal = value_as_function_literal(candidate);
      if (literal->flags & Function_Literal_Flags_Generic) {
        if (!match_overload_argument_count(literal->info, args->view.length)) return;
        if (literal->info->flags & Function_Info_Flags_Compile_Time) {
          if (!args->all_arguments_are_compile_time_known) return;
        }
        overload_info = function_literal_info_for_args(context, literal, args->view);
        if (!overload_info) return;
      } else {
        overload_info = literal->info;
      }
    } else {
      const Descriptor *descriptor = value_or_lazy_value_descriptor(candidate);
      assert(descriptor->tag == Descriptor_Tag_Function_Instance);
      overload_info = descriptor->Function_Instance.info;
    }
    Mass_Argument_Scoring_Flags scoring_flags = 0;

    if (overload_info->flags & Function_Info_Flags_Compile_Time) {
      scoring_flags |= Mass_Argument_Scoring_Flags_Prefer_Compile_Time;
      // :CompileTimeFnInJitMode
      // When we are JIT-compiling a body of a function it is OK to allow
      // to call a function that requires compile-time known args since
      // they will be known at a point when this code runs which will
      // still be at compile time (JIT).
      if (!context_is_compile_time_eval(context)) {
        if (!args->all_arguments_are_compile_time_known) return;
      }
    }

    s64 score = calculate_arguments_match_score(
      context, overload_info, args->view, scoring_flags
    );
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
  Mass_Context *context,
  Value *value,
  Value_View args_view
) {
  struct Overload_Match_State match = { .score = -1 };
  struct Overload_Match_State best_conflict_match = match;
  Mass_Overload_Match_Args args = {
    .view = args_view,
    .all_arguments_are_compile_time_known = true,
  };
  for (u64 i = 0; i < args_view.length; ++i) {
    if (!mass_value_is_compile_time_known(value_view_get(args_view, i))) {
      args.all_arguments_are_compile_time_known = false;
      break;
    }
  }
  mass_match_overload_candidate(context, value, &args, &match, &best_conflict_match);

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

static bool
mass_match_overload_or_error(
  Mass_Context *context,
  Value *target,
  Value_View args_view,
  Overload_Match_Found *match_found
) {
  Overload_Match match = mass_match_overload(context, target, args_view);
  if (mass_has_error(context)) return 0;
  switch(match.tag) {
    case Overload_Match_Tag_No_Match: {
      Array_Value_Ptr error_args = value_view_to_value_array(context->allocator, args_view);
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_No_Matching_Overload,
        .source_range = args_view.source_range,
        .No_Matching_Overload = { .target = target, .arguments = error_args },
      });
      return false;
    }
    case Overload_Match_Tag_Undecidable: {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Undecidable_Overload,
        .Undecidable_Overload = { match.Undecidable.a, match.Undecidable.b },
        .source_range = args_view.source_range,
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
  const Function_Info *info,
  Value_View args_view
) {
  // @Volatile :IntrinsicFunctionSignature
  Mass_Intrinsic_Proc jitted_code = (Mass_Intrinsic_Proc)mass_ensure_jit_function_for_value(
    context, overload, &args_view.source_range
  );
  if (mass_has_error(context)) return 0;

  Value *result = jitted_code(context, parser, args_view);
  if (!result) return 0;
  // :IntrinsicReturnType
  switch(info->returns.tag) {
    case Function_Return_Tag_Inferred: {
      // Accept whatever was returned
      return result;
    } break;
    case Function_Return_Tag_Exact: {
      const Descriptor *expected = info->returns.Exact.descriptor;
      if (mass_descriptor_is_void(expected)) {
        return value_make(context->allocator, &descriptor_void, storage_none, info->returns.source_range);
      }
      const Descriptor *actual = value_or_lazy_value_descriptor(result);
      if (!same_type(expected, actual)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = info->returns.source_range,
          .Type_Mismatch = { .expected = expected, .actual = actual },
        });
        return 0;
      }
    } break;
    case Function_Return_Tag_Generic: {
      panic("Generic returns should have been resolved in `ensure_function_instance`");
    } break;
  }
  return result;
}

static inline bool
value_is_intrinsic(
  Value *value
) {
  const Function_Info *info;
  if (value_is_function_literal(value)) {
    info = value_as_function_literal(value)->info;
  } else if (value->descriptor->tag == Descriptor_Tag_Function_Instance) {
    info = value->descriptor->Function_Instance.info;
  } else {
    return false;
  }
  return !!(info->flags & Function_Info_Flags_Intrinsic);
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

  Mass_Context jit_context = *context;
  jit_context.program = context->compilation->jit.program;

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
    Value *runtime_instance = ensure_function_instance(&jit_context, original, args_view);
    if (mass_has_error(context)) return 0;
    assert(runtime_instance->descriptor->tag == Descriptor_Tag_Function_Instance);
    Function_Info *proxy_info = mass_allocate(context, Function_Info);
    *proxy_info = *original_info;
    proxy_info->flags &= ~Function_Info_Flags_Compile_Time;

    Descriptor *proxy_descriptor = allocator_allocate(context->allocator, Descriptor);
    *proxy_descriptor = *runtime_instance->descriptor;
    proxy_descriptor->Function_Instance.info = proxy_info;
    proxy_value = value_make(
      context->allocator,
      proxy_descriptor,
      runtime_instance->storage,
      runtime_instance->source_range
    );
  }

  C_Struct_Aligner struct_aligner = {0};
  Array_Struct_Field fields = dyn_array_make(
    Array_Struct_Field,
    .allocator = context->allocator,
    .capacity = args_view.length,
  );
  for (u64 i = 0; i < args_view.length; ++i) {
    Value *item = value_view_get(args_view, i);
    const Descriptor *field_descriptor = item->descriptor;
    assert(item->descriptor != &descriptor_lazy_value);
    u64 field_byte_offset = c_struct_aligner_next_byte_offset(&struct_aligner, field_descriptor);

    dyn_array_push(fields, (Struct_Field) {
      .name = {0},
      .descriptor = field_descriptor,
      .source_range = item->source_range,
      .offset = field_byte_offset,
    });
  }

  assert(original_info->returns.tag == Function_Return_Tag_Exact);
  const Descriptor *return_descriptor = original_info->returns.Exact.descriptor;
  u64 return_byte_offset = c_struct_aligner_next_byte_offset(&struct_aligner, return_descriptor);
  {
    dyn_array_push(fields, (Struct_Field) {
      .name = slice_literal("returns"),
      .descriptor = return_descriptor,
      .source_range = original_info->returns.source_range,
      .offset = return_byte_offset,
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

  Scope *trampoline_scope = scope_make(context->allocator, context->compilation->root_scope);

  Source_Range return_range;
  INIT_LITERAL_SOURCE_RANGE(&return_range, "()");
  Function_Info *trampoline_info = allocator_allocate(context->allocator, Function_Info);
  trampoline_info->flags = Function_Info_Flags_Compile_Time;
  trampoline_info->returns = (Function_Return) {
    .tag = Function_Return_Tag_Exact,
    .source_range = return_range,
    .Exact = { .descriptor = &descriptor_void },
  };
  trampoline_info->parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = 1
  );
  Source_Range args_source_range;
  INIT_LITERAL_SOURCE_RANGE(&args_source_range, "args");
  dyn_array_push(trampoline_info->parameters, (Function_Parameter) {
    .tag = Function_Parameter_Tag_Runtime,
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("args")),
    .descriptor = descriptor_pointer_to(context->compilation, args_struct_descriptor),
    .source_range = args_source_range,
  });

  Source_Range proxy_source_range;
  INIT_LITERAL_SOURCE_RANGE(&proxy_source_range, "proxy");
  const Symbol *proxy_symbol = mass_ensure_symbol(context->compilation, slice_literal("proxy"));
  scope_define_value(trampoline_scope, VALUE_STATIC_EPOCH, proxy_source_range, proxy_symbol, proxy_value);
  Fixed_Buffer *buffer =
    fixed_buffer_make(.allocator = context->allocator, .capacity = 1024);
  fixed_buffer_append_slice(buffer, slice_literal("{args.returns = proxy("));
  assert(args_view.length <= 10);
  for (u64 i = 0; i < args_view.length; ++i) {
    if (i != 0) {
      fixed_buffer_append_slice(buffer, slice_literal(", "));
    }
    fixed_buffer_append_slice(buffer, slice_literal("args."));
    fixed_buffer_append_u8(buffer, u64_to_u8(i) + '0');
  }
  fixed_buffer_append_slice(buffer, slice_literal(");}"));

  Source_Range body_range = {
    .file = allocator_make(context->allocator, Source_File, .text = fixed_buffer_as_slice(buffer)),
    .offsets = {.from = 0, .to = u64_to_u32(buffer->occupied), },
  };
  Array_Value_View statements;
  Mass_Result result = tokenize(context, body_range, &statements);
  if (mass_result_is_error(&result)) {
    panic("This body should always be tokenizable since we constructed it to be");
  }
  Ast_Block body = {.statements = statements};
  Value *body_value = value_make(context->allocator, &descriptor_ast_block, storage_immediate(&body), body_range);

  Function_Literal *trampoline_literal = allocator_allocate(context->allocator, Function_Literal);
  *trampoline_literal = (Function_Literal) {
    .info = trampoline_info,
    .body = body_value,
    .own_scope = trampoline_scope,
  };
  Value *literal_value = value_make(
    context->allocator, &descriptor_function_literal, storage_static(trampoline_literal), body_range
  );

  Mass_Trampoline *trampoline = allocator_allocate(context->allocator, Mass_Trampoline);
  *trampoline = (Mass_Trampoline) {
    .args_descriptor = args_struct_descriptor,
    .proc = (Mass_Trampoline_Proc)mass_ensure_jit_function_for_value(
      &jit_context, literal_value, &args_view.source_range
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
  u8 *args_struct_memory = trampoline->args_descriptor->bit_size.as_u64
    ? allocator_allocate_bytes(
      context->temp_allocator,
      descriptor_byte_size(trampoline->args_descriptor),
      descriptor_byte_alignment(trampoline->args_descriptor)
    )
    : 0;

  Array_Struct_Field fields = trampoline->args_descriptor->Struct.fields;
  assert(trampoline->args_descriptor->tag == Descriptor_Tag_Struct);
  for (u64 i = 0; i < args_view.length; ++i) {
    Value *item = value_view_get(args_view, i);
    assert(mass_value_is_compile_time_known(item));
    const Struct_Field *field = dyn_array_get(fields, i);
    u64 offset = field->offset;
    void *arg_memory = args_struct_memory + offset;
    const void *source_memory = storage_static_memory_with_bit_size(
      &item->storage, item->descriptor->bit_size
    );
    memcpy(arg_memory, source_memory, descriptor_byte_size(item->descriptor));
  }

  trampoline->proc(args_struct_memory);
  const Descriptor *return_descriptor =
    function_return_as_exact(&trampoline->original_info->returns)->descriptor;
  Value *result;
  if (mass_descriptor_is_void(return_descriptor)) {
    result = mass_make_void(context->allocator, args_view.source_range);
  } else {
    const Struct_Field *return_field = dyn_array_last(fields);
    const void *temp_return_memory = args_struct_memory + return_field->offset;
    u64 return_byte_size = descriptor_byte_size(return_field->descriptor);
    void *return_memory = allocator_allocate_bytes(
      context->allocator,
      return_byte_size,
      descriptor_byte_alignment(return_field->descriptor)
    );
    memcpy(return_memory, temp_return_memory, return_byte_size);

    Storage return_storage = storage_static_heap(return_memory, return_field->descriptor->bit_size);

    result = value_make(context->allocator, return_field->descriptor, return_storage, args_view.source_range);
  }
  context_temp_reset_to_mark(context, temp_mark);

  return result;
}

static bool
mass_can_trampoline_call(
  const Function_Info *info,
  Value_View args_view
) {
  for (u64 i = 0; i < args_view.length; ++i) {
    Value *arg = value_view_get(args_view, i);

    if (!mass_value_is_compile_time_known(arg)) return false;

    // The check here is required because casts may generate extra instructions.
    // This can be removed if the casts are not a thing for fn calls or if
    // there will be a way to compile-time cast without generating instructions
    // maybe also with trampolines or something similar.
    const Function_Parameter *param = dyn_array_get(info->parameters, i);
    if (!same_type(param->descriptor, arg->descriptor)) return false;
  }
  return true;
}

static Value *
token_handle_function_call(
  Mass_Context *context,
  Parser *parser,
  Value *target_expression,
  Value_View args_view,
  Source_Range source_range
) {
  Overload_Match_Found match_found;
  if (!mass_match_overload_or_error(context, target_expression, args_view, &match_found)) {
    return 0;
  }

  Value *overload = match_found.value;
  const Function_Info *info = match_found.info;

  if (value_is_function_literal(overload)) {
    const Function_Literal *literal = value_as_function_literal(overload);
    if (value_is_intrinsic(literal->body)) {
      return mass_intrinsic_call(context, parser, literal->body, info, args_view);
    }
    if (literal->flags & Function_Literal_Flags_Macro) {
      return mass_handle_macro_call(context, parser, overload, args_view, source_range);
    }
  }

  if (info->flags & Function_Info_Flags_Compile_Time) {
    if (mass_can_trampoline_call(info, args_view)) {
      Value *result = mass_trampoline_call(context, parser, overload, info, args_view);
      if (mass_has_error(context)) return 0;
      return result;
    } else {
      // :CompileTimeFnInJitMode
      if (context_is_compile_time_eval(context)) {
        // Fall through and do a regular call
      } else {
        panic("A compile-time overload should not have been matched if we can't call it");
      }
    }
  }

  Mass_Function_Call_Lazy_Payload *call_payload =
    allocator_allocate(context->allocator, Mass_Function_Call_Lazy_Payload);
  *call_payload = (Mass_Function_Call_Lazy_Payload){
    .overload = overload,
    .args = value_view_to_value_array(context->allocator, args_view),
  };

  const Descriptor *lazy_descriptor = function_return_as_exact(&info->returns)->descriptor;
  Value *result = mass_make_lazy_value(
    context, parser, source_range, call_payload, lazy_descriptor, call_function_overload
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
    Array_Value_Ptr temp_args = dyn_array_make(
      Array_Value_Ptr,
      .allocator = context->temp_allocator,
      .capacity = 32,
    );
    token_match_call_arguments(context, parser, value_as_group_paren(args_token), &temp_args);
    if (mass_has_error(context)) goto defer;
    args_view = value_view_from_value_array(temp_args, &source_range);
  } else if (args_token->descriptor == &descriptor_value_view) {
    if (!mass_value_is_compile_time_known(args_token)) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Expected_Static,
        .source_range = source_range,
        .detailed_message = slice_literal("Expected a static Value_View"),
      });
      goto defer;
    }
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
  const Source_Range *source_range,
  Mass_Arithmetic_Operator_Lazy_Payload *payload
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
      Value *temp_lhs = value_force(context, builder, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_rhs_storage = storage_register_temp(builder, descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_rhs_storage);
      Value *temp_rhs = value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      const X64_Mnemonic *mnemonic = payload->operator == Mass_Arithmetic_Operator_Add ? add : sub;

      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_lhs->storage, temp_rhs->storage}}
      );
      storage_release_if_temporary(builder, &temp_rhs_storage);

      // temp_a is used as a result so it is intentionnaly not released
      return mass_expected_result_ensure_value_or_temp(
        context, builder, expected_result, temp_lhs
      );
    }
    case Mass_Arithmetic_Operator_Multiply: {
      // Save RDX as it will be used for the result overflow
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Storage maybe_saved_rdx = storage_none;
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
        move_value(builder, &result_range, &maybe_saved_rdx, &reg_d);
      }

      Storage temp_a_storage =
        storage_register(register_acquire(builder, Register_A), descriptor->bit_size);
      temp_a_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_a = mass_expected_result_exact(descriptor, temp_a_storage);
      Value *temp_a = value_force(context, builder, &expected_a, payload->lhs);

      // TODO we do not acquire here because it is done by maybe_saved_rdx,
      //      but it is awkward that it is disconnected so need to think about
      Storage temp_b_storage = storage_register(Register_D, descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_b_storage);
      Value *temp_b = value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_instruction(&builder->code_block, (Instruction) {
        .tag = Instruction_Tag_Location,
        .Location = { .source_range = result_range },
      });

      const X64_Mnemonic *mnemonic = descriptor_is_signed_integer(descriptor) ? imul : mul;
      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_b->storage}}
      );
      if (maybe_saved_rdx.tag != Storage_Tag_None) {
        assert(maybe_saved_rdx.tag == Storage_Tag_Register);
        move_value(builder, &result_range, &reg_d, &maybe_saved_rdx);
        register_release(builder, maybe_saved_rdx.Register.index);
      }

      // temp_a is used as a result so it is intentionnaly not released
      return mass_expected_result_ensure_value_or_temp(
        context, builder, expected_result, temp_a
      );
    }
    case Mass_Arithmetic_Operator_Divide:
    case Mass_Arithmetic_Operator_Remainder: {
      u64 bit_size = descriptor->bit_size.as_u64;

      // We need both D and A for this operation so using either as temp will not work
      u64 disallowed_temp_registers = 0;
      register_bitset_set(&disallowed_temp_registers, Register_D);
      register_bitset_set(&disallowed_temp_registers, Register_A);

      Storage register_a_storage = storage_register(Register_A, descriptor->bit_size);
      register_acquire(builder, Register_A);
      register_a_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_dividend = mass_expected_result_exact(descriptor, register_a_storage);
      Value *temp_dividend = value_force(context, builder, &expected_dividend, payload->lhs);

      Register temp_divisor_register = register_find_available(builder, disallowed_temp_registers);
      register_acquire(builder, temp_divisor_register);
      Storage temp_divisor_storage = storage_register(temp_divisor_register, descriptor->bit_size);
      temp_divisor_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_divisor = mass_expected_result_exact(descriptor, temp_divisor_storage);
      Value *temp_divisor = value_force(context, builder, &expected_divisor, payload->rhs);

      // Save RDX as it will be used for the remainder
      // but we should not save or restore it if it is the result
      // @CopyPaste :SaveRDX
      Storage maybe_saved_rdx = storage_none;
      Storage reg_d = storage_register(Register_D, (Bits){64});
      if (register_bitset_get(builder->register_occupied_bitset.bits, Register_D)) {
        if (
          expected_result->tag != Expected_Result_Tag_Exact ||
          !storage_is_register_index(&expected_result->Exact.storage, Register_D)
        ) {
          Register temp_register = register_find_available(builder, disallowed_temp_registers);
          register_acquire(builder, temp_register);
          maybe_saved_rdx = storage_register(temp_register, (Bits){64});
          move_value(builder, &result_range, &maybe_saved_rdx, &reg_d);
        }
      }

      push_instruction(&builder->code_block, (Instruction) {
        .tag = Instruction_Tag_Location,
        .Location = { .source_range = result_range },
      });

      if (mass_has_error(context)) return 0;

      if (descriptor_is_signed_integer(descriptor)){
        const X64_Mnemonic *widen = 0;
        switch (bit_size) {
          case 64: widen = cqo; break;
          case 32: widen = cdq; break;
          case 16: widen = cwd; break;
          case 8: widen = cbw; break;
        }
        assert(widen);
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, result_range, &(Instruction_Assembly){widen}
        );
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, result_range, &(Instruction_Assembly){idiv, {temp_divisor->storage}}
        );
      } else {
        if (bit_size == 8) {
          Storage reg_ax = storage_register(Register_A, (Bits){16});
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, result_range, &(Instruction_Assembly){movzx, {reg_ax, temp_dividend->storage}}
          );
        } else {
          // We need to zero-extend A to D which means just clearing D register
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, result_range, &(Instruction_Assembly){xor, {reg_d, reg_d}}
          );
        }
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, result_range, &(Instruction_Assembly){asm_div, {temp_divisor->storage}}
        );
      }

      if (payload->operator == Mass_Arithmetic_Operator_Remainder) {
        if (bit_size == 8) {
          // :64bitMode8BitOperations
          // The encoder does not support access to AH so we hardcode byte of `mov AL, AH`
          // This is not optimal, but it should do for now.
          push_instruction(&builder->code_block, (Instruction) {
            .tag = Instruction_Tag_Bytes,
            .Bytes = {.memory = {0x88, 0xe0}, .length = 2},
          });
        } else {
          Storage reg_d = storage_register(Register_D, descriptor->bit_size);
          move_value(builder, &result_range, &temp_dividend->storage, &reg_d);
        }
      }

      storage_release_if_temporary(builder, &temp_divisor->storage);
      if (maybe_saved_rdx.tag != Storage_Tag_None) {
        assert(maybe_saved_rdx.tag == Storage_Tag_Register);
        move_value(builder, &result_range, &reg_d, &maybe_saved_rdx);
        register_release(builder, maybe_saved_rdx.Register.index);
      }

      return mass_expected_result_ensure_value_or_temp(
        context, builder, expected_result, temp_dividend
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
  Value *lhs = token_parse_single(context, parser, value_view_get(arguments, 0));
  Value *rhs = token_parse_single(context, parser, value_view_get(arguments, 1));

  if (mass_has_error(context)) return 0;

  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(lhs);
  if (value_is_i64(rhs)) {
    rhs = token_value_force_immediate_integer(
      context, rhs, result_descriptor, &arguments.source_range
    );
  } else if (value_is_i64(lhs)) {
    result_descriptor = value_or_lazy_value_descriptor(rhs);
    lhs = token_value_force_immediate_integer(
      context, lhs, result_descriptor, &arguments.source_range
    );
  }
  assert(same_type(value_or_lazy_value_descriptor(lhs), value_or_lazy_value_descriptor(rhs)));

  Mass_Arithmetic_Operator_Lazy_Payload stack_lazy_payload =
    { .lhs = lhs, .rhs = rhs, .operator = operator, .source_range = arguments.source_range };
  Mass_Arithmetic_Operator_Lazy_Payload *lazy_payload =
    allocator_allocate(context->allocator, Mass_Arithmetic_Operator_Lazy_Payload);
  *lazy_payload = stack_lazy_payload;
  return mass_make_lazy_value(
    context, parser, arguments.source_range, lazy_payload, result_descriptor, mass_handle_arithmetic_operation_lazy_proc
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
  const Source_Range *source_range,
  Mass_Comparison_Operator_Lazy_Payload *payload
) {
  Compare_Type compare_type = payload->compare_type;
  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(payload->lhs);
  const Descriptor *rhs_descriptor = value_or_lazy_value_descriptor(payload->rhs);
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
  Value *temp_a = value_force(context, builder, &expected_a, payload->lhs);

  // TODO This can be optimized in cases where one of the operands is an immediate
  Storage temp_b_storage = storage_register_temp(builder, descriptor->bit_size);
  Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_b_storage);
  Value *temp_b = value_force(context, builder, &expected_b, payload->rhs);

  if (mass_has_error(context)) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){cmp, {temp_a->storage, temp_b->storage}}
  );

  Value *comparison_value = value_make(
    context->allocator, &descriptor__bool, storage_eflags(compare_type), *source_range
  );

  storage_release_if_temporary(builder, &temp_a_storage);
  storage_release_if_temporary(builder, &temp_b_storage);

  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, comparison_value
  );
}

static Value *
mass_handle_generic_comparison_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Comparison_Operator_Lazy_Payload *payload
) {
  Compare_Type compare_type = payload->compare_type;

  Value *lhs = payload->lhs;
  Value *rhs = payload->rhs;
  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(lhs);
  const Descriptor *rhs_descriptor = value_or_lazy_value_descriptor(rhs);

  if (!same_type(lhs_descriptor, rhs_descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = *source_range,
      .Type_Mismatch = { .expected = lhs_descriptor, .actual = rhs_descriptor },
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

  if (mass_value_is_compile_time_known(lhs) && mass_value_is_compile_time_known(rhs)) {
    bool equal = storage_static_equal(lhs->descriptor, &lhs->storage, rhs->descriptor, &rhs->storage);
    if (negated) equal = !equal;
    return value_make(
      context->allocator, &descriptor__bool, storage_immediate(&equal), *source_range
    );
  }

  Value *result = 0;
  switch(lhs_descriptor->tag) {
    // Two void values are always equal to each other
    case Descriptor_Tag_Void: {
      bool equal = true;
      result = value_make(
        context->allocator, &descriptor__bool, storage_immediate(&equal), *source_range
      );
    } break;
    case Descriptor_Tag_Pointer_To:
    case Descriptor_Tag_Raw:
    case Descriptor_Tag_Float:
    case Descriptor_Tag_Integer:
    case Descriptor_Tag_Function_Instance: {
      if (lhs_descriptor->bit_size.as_u64 > 64) {
        panic("TODO support larger than register compares");
      }

      // @CopyPaste from integer compares

      // Try to reuse result_value if we can
      // TODO should also be able to reuse memory operands
      Storage temp_a_storage = storage_register_temp(builder, lhs_descriptor->bit_size);
      Expected_Result expected_a = mass_expected_result_exact(lhs_descriptor, temp_a_storage);
      Value *temp_a = value_force(context, builder, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_b_storage = storage_register_temp(builder, rhs_descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(rhs_descriptor, temp_b_storage);
      Value *temp_b = value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){cmp, {temp_a->storage, temp_b->storage}}
      );

      result = value_make(
        context->allocator, &descriptor__bool, storage_eflags(compare_type), *source_range
      );

      storage_release_if_temporary(builder, &temp_a_storage);
      storage_release_if_temporary(builder, &temp_b_storage);
    } break;
    case Descriptor_Tag_Fixed_Size_Array: {
      panic("TODO figure out semantics and support comparing fixed size");
    } break;
    case Descriptor_Tag_Struct: {
      panic("TODO figure out semantics and support comparing structs");
    } break;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, result
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
  Value *lhs = token_parse_single(context, parser, value_view_get(arguments, 0));
  Value *rhs = token_parse_single(context, parser, value_view_get(arguments, 1));
  if (mass_has_error(context)) return 0;

  if (value_is_i64(rhs) && value_or_lazy_value_descriptor(lhs) != &descriptor_i64) {
    rhs = token_value_force_immediate_integer(
      context, rhs, value_or_lazy_value_descriptor(lhs), &arguments.source_range
    );
  } else if (value_is_i64(lhs) && value_or_lazy_value_descriptor(rhs) != &descriptor_i64) {
    lhs = token_value_force_immediate_integer(
      context, lhs, value_or_lazy_value_descriptor(rhs), &arguments.source_range
    );
  }

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
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Less
  );
}
static inline Value *mass_integer_greater(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Greater
  );
}
static inline Value *mass_integer_less_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Less_Equal
  );
}
static inline Value *mass_integer_greater_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Signed_Greater_Equal
  );
}
static inline Value *mass_integer_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Equal
  );
}
static inline Value *mass_integer_not_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_integer_comparison_lazy_proc, Compare_Type_Not_Equal
  );
}

static inline Value *mass_generic_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_generic_comparison_lazy_proc, Compare_Type_Equal
  );
}
static inline Value *mass_generic_not_equal(Mass_Context *context, Parser *parser, Value_View arguments) {
  return mass_handle_comparison(
    context, parser, arguments, mass_handle_generic_comparison_lazy_proc, Compare_Type_Not_Equal
  );
}

static Value *
mass_handle_startup_call_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *startup_function
) {
  if(startup_function->descriptor != &descriptor_function_literal) goto err;
  const Function_Literal *literal = value_as_function_literal(startup_function);
  if (dyn_array_length(literal->info->parameters)) goto err;
  if (literal->info->returns.tag != Function_Return_Tag_Exact) goto err;
  const Descriptor *descriptor = function_return_as_exact(&literal->info->returns)->descriptor;
  if (!mass_descriptor_is_void(descriptor)) goto err;

  // This call is executed at compile time, but the actual startup function
  // will be run only at runtime so we need to make sure to use the right Program
  Mass_Context runtime_context = mass_context_from_compilation(context->compilation);
  ensure_function_instance(&runtime_context, startup_function, (Value_View){0});

  dyn_array_push(runtime_context.program->startup_functions, startup_function);
  Value *result = mass_make_void(context->allocator, *source_range);
  return expected_result_validate(expected_result, result);

  err:
  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = *source_range,
    .detailed_message = slice_literal("`startup` expects a () -> () {...} function as an argument"),
  });
  return 0;
}

static Value *
mass_startup(
  Mass_Context *context,
  Parser *parser,
  Value_View arguments
) {
  assert(arguments.length == 1);
  Value *startup_function = value_view_get(arguments, 0);

  return mass_make_lazy_value(
    context, parser,
    arguments.source_range,
    startup_function,
    &descriptor_void,
    mass_handle_startup_call_lazy_proc
  );
}

static const Descriptor *
user_presentable_descriptor_for(
  Value *expression
) {
  const Descriptor *descriptor = value_or_lazy_value_descriptor(expression);
  if (descriptor_is_implicit_pointer(descriptor)) {
    descriptor = descriptor->Pointer_To.descriptor;
  }
  return descriptor;
}

static Value *
mass_type_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(value_match_symbol(value_view_get(args, 0), slice_literal("type_of")));
  assert(args.length == 2);
  Parser_Flags saved_flags = parser->flags;
  parser->flags |= Parser_Flags_Type_Only;
  Value *expression = token_parse_single(context, parser, value_view_last(args));
  parser->flags = saved_flags;

  const Descriptor *descriptor = user_presentable_descriptor_for(expression);

  return value_make(
    context->allocator,
    &descriptor_descriptor_pointer,
    storage_immediate(&descriptor),
    args.source_range
  );
}

static Value *
mass_size_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(value_match_symbol(value_view_get(args, 0), slice_literal("size_of")));
  assert(args.length == 2);
  Value *expression = token_parse_single(context, parser, value_view_last(args));
  const Descriptor *descriptor = user_presentable_descriptor_for(expression);
  u64 byte_size = descriptor_byte_size(descriptor);

  allocator_allocate_bulk(context->allocator, combined, {
    Value value;
  });

  i64 literal = { .bits = byte_size };
  return value_init(
    &combined->value, &descriptor_i64, storage_immediate(&literal), args.source_range
  );
}

static Value *
mass_pointer_to_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *pointee
) {
  const Descriptor *descriptor = value_or_lazy_value_descriptor(pointee);
  Expected_Result expected_pointee = expected_result_any(descriptor);
  Value *forced = value_force(context, builder, &expected_pointee, pointee);
  if (mass_has_error(context)) return 0;
  Value *pointer_value =
    mass_value_from_expected_result(context->allocator, builder, expected_result, *source_range);
  load_address(builder, source_range, pointer_value, forced->storage);
  storage_release_if_temporary(builder, &forced->storage);
  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, pointer_value
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
  Value *condition_value = value_view_get(args, 0);

  bool condition = *value_as__bool(condition_value);
  if (!condition) {
    Slice detailed_message;
    if (args.length == 2) {
      detailed_message = *value_as_slice(value_view_get(args, 1));
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
  return mass_make_void(context->allocator, args.source_range);
}

static Value *
mass_pointer_to(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *pointee = value_view_get(args, 0);
  const Descriptor *pointee_descriptor = value_or_lazy_value_descriptor(pointee);
  const Descriptor *descriptor = descriptor_pointer_to(context->compilation, pointee_descriptor);
  if (mass_value_is_compile_time_known(pointee)) {
    if (context_is_compile_time_eval(context)) {
      const void *source_memory =
        get_static_storage_with_bit_size(&pointee->storage, pointee_descriptor->bit_size);
      Value *result = value_make(
        context->allocator, descriptor, storage_immediate(&source_memory), args.source_range
      );
      return result;
    }
  }

  return mass_make_lazy_value(
    context, parser, args.source_range, pointee, descriptor, mass_pointer_to_lazy_proc
  );
}

static Value *
mass_pointer_to_type(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 1);
  Value *type_value = value_view_get(args_view, 0);
  const Descriptor *descriptor = value_ensure_type(context, type_value, args_view.source_range);
  if (mass_has_error(context)) return 0;
  const Descriptor *pointer_descriptor = descriptor_pointer_to(context->compilation, descriptor);
  Storage storage = storage_immediate(&pointer_descriptor);
  return value_make(
    context->allocator, &descriptor_descriptor_pointer, storage, args_view.source_range
  );
}

static Value *
mass_call(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs_value = value_view_get(args_view, 0);
  Value *rhs_value = value_view_get(args_view, 1);
  return token_handle_parsed_function_call(
    context, parser, lhs_value, rhs_value, args_view.source_range
  );
}

static Value *
mass_handle_apply_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View operands_view,
  const Operator *operator
) {
  Source_Range source_range = operands_view.source_range;

  // TODO can we cache this somehow
  Scope_Entry *apply_entry = scope_lookup(parser->scope, context->compilation->common_symbols.apply);
  if (!apply_entry) {
    Value *rhs_value = value_view_get(operands_view, 1);
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = rhs_value->source_range,
      .detailed_message = slice_literal("Expected an operator"),
    });
    return 0;
  }

  Value *apply_function = scope_entry_force_value(context, apply_entry);
  return token_handle_function_call(context, parser, apply_function, operands_view, source_range);
}

static Value *
mass_handle_typed_symbol_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View operands,
  const Operator *operator
) {
  Value *lhs_value = value_view_get(operands, 0);
  Value *rhs_value = token_parse_single(context, parser, value_view_get(operands, 1));
  Source_Range source_range = operands.source_range;

  if (!value_is_symbol(lhs_value)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = source_range,
      .detailed_message = "operator : expects a symbol on the left hand side"
    });
    return 0;
  }

  const Descriptor *descriptor = value_ensure_type(context, rhs_value, source_range);
  Typed_Symbol *typed_symbol = allocator_allocate(context->allocator, Typed_Symbol);
  *typed_symbol = (Typed_Symbol) {
    .symbol = value_as_symbol(lhs_value),
    .descriptor = descriptor,
  };

  return value_make(context->allocator, &descriptor_typed_symbol, storage_static(typed_symbol), source_range);
}

typedef struct {
  const Descriptor *descriptor;
} Mass_Variable_Definition_Lazy_Payload;

static Value *
mass_handle_variable_definition_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Variable_Definition_Lazy_Payload *payload
) {
  Storage storage = mass_descriptor_is_void(payload->descriptor)
    ? storage_none
    : reserve_stack_storage(builder, payload->descriptor->bit_size);
  return value_make(
    context->allocator, payload->descriptor, storage, *source_range
  );
}

static Value *
mass_handle_assignment_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Assignment *payload
) {
  const Descriptor *target_descriptor = value_or_lazy_value_descriptor(payload->target);
  Expected_Result expected_target = expected_result_any(target_descriptor);
  Value *target = value_force(context, builder, &expected_target, payload->target);
  if (mass_has_error(context)) return 0;

  value_force_exact(context, builder, target, payload->source);
  storage_release_if_temporary(builder, &target->storage);
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

  Value *void_value = mass_make_void(context->allocator, *source_range);
  return expected_result_validate(expected_result, void_value);
}

static Value *
mass_define_stack_value_from_typed_symbol(
  Mass_Context *context,
  Parser *parser,
  const Typed_Symbol *typed_symbol,
  Source_Range source_range
) {
  Mass_Variable_Definition_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_Variable_Definition_Lazy_Payload);
  *payload = (Mass_Variable_Definition_Lazy_Payload){
    .descriptor = typed_symbol->descriptor,
  };
  Value *defined = mass_make_lazy_value(
    context, parser, source_range, payload,
    typed_symbol->descriptor, mass_handle_variable_definition_lazy_proc
  );
  scope_define_value(parser->scope, parser->epoch, source_range, typed_symbol->symbol, defined);
  return defined;
}

static Value *
mass_handle_assignment_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View operands,
  const Operator *operator
) {
  Value *target = token_parse_single(context, parser, value_view_get(operands, 0));
  Value *source = token_parse_single(context, parser, value_view_get(operands, 1));


  if (mass_has_error(context)) return 0;

  if (value_is_typed_symbol(target)) {
    const Typed_Symbol *typed_symbol = value_as_typed_symbol(target);
    target = mass_define_stack_value_from_typed_symbol(context, parser, typed_symbol, target->source_range);
  }

  Assignment *assignment = allocator_allocate(context->allocator, Assignment);
  *assignment = (Assignment) {
    .target = target,
    .source = source,
  };
  return value_make(
    context->allocator, &descriptor_assignment, storage_static(assignment), operands.source_range
  );
}

static Value *
mass_eval(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 1);
  Value *body = value_view_get(args_view, 0);
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

static Storage
value_maybe_dereference(
  Mass_Context *context,
  Function_Builder *builder,
  Value *value
) {
  const Descriptor *descriptor = value_or_lazy_value_descriptor(value);
  const Descriptor *unwrapped_descriptor = maybe_unwrap_pointer_descriptor(descriptor);
  // Auto dereference pointers to structs
  bool is_pointer = descriptor != unwrapped_descriptor;
  if (!is_pointer) {
    return value->storage;
  } else {
    if (mass_value_is_compile_time_known(value)) {
      const void *pointed_memory =
        *(void **)storage_static_memory_with_bit_size(&value->storage, (Bits){64});
      return storage_static_heap(pointed_memory, unwrapped_descriptor->bit_size);
    } else if (value->storage.tag == Storage_Tag_Register) {
      Register reg = value->storage.Register.index;
      return storage_indirect(unwrapped_descriptor->bit_size, reg);
    } else {
      Register reg = register_acquire_temp(builder);
      Storage base_storage = storage_register(reg, descriptor->bit_size);
      Storage storage = storage_indirect(unwrapped_descriptor->bit_size, reg);
      storage.flags |= Storage_Flags_Temporary;
      move_value(builder, &value->source_range, &base_storage, &value->storage);
      storage_release_if_temporary(builder, &value->storage);
      return storage;
    }
  }
}

static Value *
mass_handle_field_access_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Mass_Field_Access_Lazy_Payload *payload
) {
  const Struct_Field *field = payload->field;

  Expected_Result expected_struct =
    expected_result_any(value_or_lazy_value_descriptor(payload->struct_));
  Value *struct_ = value_force(context, builder, &expected_struct, payload->struct_);
  if (mass_has_error(context)) return 0;

  const Descriptor *struct_descriptor = value_or_lazy_value_descriptor(struct_);
  const Descriptor *unwrapped_descriptor = maybe_unwrap_pointer_descriptor(struct_descriptor);
  assert(unwrapped_descriptor->tag == Descriptor_Tag_Struct);

  Storage struct_storage = value_maybe_dereference(context, builder, struct_);

  // Since storage_field_access reuses indirect memory storage of the struct
  // the release of memory will be based on the field value release and we need
  // to propagate the temporary flag correctly
  // TODO should this always copy the flags? Or maybe it should be mutating?
  Storage field_storage = storage_with_offset_and_bit_size(
    &struct_storage, u64_to_s32(field->offset), field->descriptor->bit_size
  );
  field_storage.flags = struct_storage.flags;

  Value *field_value = value_make(
    context->allocator, field->descriptor, field_storage, *source_range
  );

  if (struct_descriptor->tag != Descriptor_Tag_Pointer_To && (struct_->flags & Value_Flags_Constant)) {
    field_value->flags |= Value_Flags_Constant;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, field_value
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
  const Source_Range *source_range,
  void *raw_payload
) {
  Mass_Array_Access_Lazy_Payload *payload = raw_payload;
  Expected_Result expected_array =
    expected_result_any(value_or_lazy_value_descriptor(payload->array));
  Value *array = value_force(context, builder, &expected_array, payload->array);
  Expected_Result expected_index =
    expected_result_any(value_or_lazy_value_descriptor(payload->index));
  Value *index = value_force(context, builder, &expected_index, payload->index);

  if (mass_has_error(context)) return 0;

  Value *array_element_value;

  const Descriptor *array_descriptor = value_or_lazy_value_descriptor(array);
  const Descriptor *unwrapped_descriptor = maybe_unwrap_pointer_descriptor(array_descriptor);

  const Descriptor *item_descriptor;
  if(unwrapped_descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
    item_descriptor = unwrapped_descriptor->Fixed_Size_Array.item;
  } else {
    assert(array_descriptor->tag == Descriptor_Tag_Pointer_To);
    item_descriptor = unwrapped_descriptor;
  }

  u64 item_byte_size = descriptor_byte_size(item_descriptor);

  Storage element_storage;
  if (value_is_i64(index)) {
    u64 index_bits = value_as_i64(index)->bits;
    s32 offset = u64_to_s32(index_bits * item_byte_size);
    Storage array_storage = value_maybe_dereference(context, builder, array);
    element_storage = storage_with_offset_and_bit_size(&array_storage, offset, item_descriptor->bit_size);
    element_storage.flags = array_storage.flags;
  } else {
    Storage array_storage = value_maybe_dereference(context, builder, array);

    Register base_register = register_acquire_temp(builder);
    Storage base_storage = storage_register(base_register, (Bits){64});

    // Move the index into the register
    move_value(builder, source_range, &base_storage, &index->storage);

    // Multiplication by 1 byte is useless so checking it here
    if (item_descriptor->bit_size.as_u64 != 8) {
      u32 item_byte_size = u64_to_u32(descriptor_byte_size(item_descriptor));
      Register byte_size_register = register_acquire_temp(builder);
      Storage byte_size_storage = storage_register(byte_size_register, (Bits){64});

      // Multiply index by the item byte size
      Storage item_byte_size_storage = imm32(item_byte_size);
      move_value(builder, source_range, &byte_size_storage, &item_byte_size_storage);

      // TODO @InstructionQuality this should use shifts for power-of-2 item byte sizes
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){imul, {base_storage, byte_size_storage}}
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
      mass_storage_load_address(builder, source_range, &address_storage, &array_storage);

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){add, {base_storage, address_storage}}
      );
      register_release(builder, address_register);
    }

    element_storage = storage_indirect(item_descriptor->bit_size, base_register);
    element_storage.flags |= Storage_Flags_Temporary;

    storage_release_if_temporary(builder, &array_storage);
  }

  array_element_value = value_make(
    context->allocator, item_descriptor, element_storage, array->source_range
  );

  if (array->flags & Value_Flags_Constant) {
    array_element_value->flags |= Value_Flags_Constant;
  }

  return mass_expected_result_ensure_value_or_temp(
    context, builder, expected_result, array_element_value
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

  if (mass_value_is_compile_time_known(struct_)) {
    Expected_Result expected_result = expected_result_any(field->descriptor);
    return mass_handle_field_access_lazy_proc(
      context, 0, &expected_result, source_range, &stack_lazy_payload
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
      mass_handle_field_access_lazy_proc
    );
  }
}
static Value *
mass_handle_dereference_operator_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value* pointer
) {
  // TODO value_indirect_from_pointer should probably take an expected_result
  Value *value = value_indirect_from_pointer(context, builder, pointer);
  return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, value);
}

static Value *
mass_handle_dereference_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view,
  const void *payload
) {
  Value *pointer = token_parse_single(context, parser, value_view_get(args_view, 0));
  if (mass_has_error(context)) return 0;
  const Descriptor *descriptor = value_or_lazy_value_descriptor(pointer);
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
  // FIXME support this for static values
  return mass_make_lazy_value(
    context, parser,
    args_view.source_range,
    pointer,
    descriptor->Pointer_To.descriptor,
    mass_handle_dereference_operator_lazy_proc
  );
}

static Value *
mass_module_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(args_view, 0);
  Value *rhs = value_view_get(args_view, 1);

  if (!mass_value_ensure_static_of(context, lhs, &descriptor_module)) return 0;
  if (!mass_value_ensure_static_of(context, rhs, &descriptor_symbol)) return 0;

  const Symbol *symbol = value_as_symbol(rhs);
  const Module *module = value_as_module(lhs);
  Scope_Entry *entry = scope_lookup_shallow(module->exports.scope, symbol);
  if (!entry) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unknown_Field,
      .source_range = args_view.source_range,
      .Unknown_Field = { .name = symbol->name, .type = lhs->descriptor, },
    });
    return 0;
  }
  // FIXME when looking up values from a different file, need to either adjust source
  //       range or have some other mechanism to track it.
  return scope_entry_force_value(context, entry);
}

static Value *
mass_struct_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(args_view, 0);
  Value *rhs = value_view_get(args_view, 1);

  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(lhs);
  const Descriptor *unwrapped_lhs_descriptor = maybe_unwrap_pointer_descriptor(lhs_descriptor);
  assert(unwrapped_lhs_descriptor->tag == Descriptor_Tag_Struct);
  Array_Struct_Field fields = unwrapped_lhs_descriptor->Struct.fields;

  const Struct_Field *field;
  if (value_is_i64(rhs)) {
    u64 index = value_as_i64(rhs)->bits;
    field = 0;
    DYN_ARRAY_FOREACH(Struct_Field, it, fields) {
      // Only allow access to unnamed fields via numbers. This is a purely an empirical decision.
      // Allowing arbitrary fields just by index was very confusing in practice.
      // A user of the language can always redifine this choice or use a specialized fn.
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
  Value *lhs = value_view_get(args_view, 0);
  Value *rhs = token_parse_single(context, parser, value_view_get(args_view, 1));

  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(lhs);
  const Descriptor *item_descriptor;
  if (lhs_descriptor->tag == Descriptor_Tag_Fixed_Size_Array) {
    item_descriptor = lhs_descriptor->Fixed_Size_Array.item;
  } else if (lhs_descriptor->tag == Descriptor_Tag_Pointer_To) {
    item_descriptor = lhs_descriptor->Pointer_To.descriptor;
  } else {
    panic("UNREACHABLE");
    return 0;
  }

  const Descriptor *rhs_descriptor = value_or_lazy_value_descriptor(rhs);
  // TODO this should this only accept i64?
  if (rhs_descriptor != &descriptor_i64 && !descriptor_is_integer(rhs_descriptor)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Type_Mismatch,
      .source_range = args_view.source_range,
      .Type_Mismatch = { .expected = &descriptor_i64, .actual = rhs_descriptor },
    });
    return 0;
  }

  Mass_Array_Access_Lazy_Payload lazy_payload = { .array = lhs, .index = rhs };
  if (mass_value_is_compile_time_known(lhs) && mass_value_is_compile_time_known(rhs)) {
    Expected_Result expected_result = expected_result_any(item_descriptor);
    return mass_handle_array_access_lazy_proc(
      context, 0, &expected_result, &args_view.source_range, &lazy_payload
    );
  }
  Mass_Array_Access_Lazy_Payload *heap_payload =
    mass_allocate(context, Mass_Array_Access_Lazy_Payload);
  *heap_payload = lazy_payload;
  return mass_make_lazy_value(
    context, parser, args_view.source_range, heap_payload,
    item_descriptor, mass_handle_array_access_lazy_proc
  );
}

static Value *
mass_get(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *lhs = token_parse_single(context, parser, value_view_get(args_view, 0));
  Value *rhs = value_view_get(args_view, 1);

  Value_View parsed_args = {
    .values = (Value*[]){lhs, rhs},
    .length = 2,
    .source_range = args_view.source_range,
  };

  if (mass_has_error(context)) return 0;

  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(lhs);
  if (lhs_descriptor == &descriptor_module) {
    return mass_module_get(context, parser, parsed_args);
  }

  const Descriptor *unwrapped_lhs_descriptor = maybe_unwrap_pointer_descriptor(lhs_descriptor);

  if (unwrapped_lhs_descriptor->tag == Descriptor_Tag_Struct) {
    return mass_struct_get(context, parser, parsed_args);
  }

  if (
    lhs_descriptor->tag == Descriptor_Tag_Fixed_Size_Array ||
    lhs_descriptor->tag == Descriptor_Tag_Pointer_To
  ) {
    return mass_array_like_get(context, parser, parsed_args);
  }

  mass_error(context, (Mass_Error) {
    .tag = Mass_Error_Tag_Parse,
    .source_range = lhs->source_range,
    .detailed_message = slice_literal("Left hand side of the . operator must be a struct or an array"),
  });
  return 0;
}

static Value *
mass_handle_dot_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view,
  const Operator *operator
) {
  return mass_get(context, parser, args_view);
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
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = stack_entry->source_range,
      .detailed_message = slice_literal("Not enough arguments for operator"),
    });
    return;
  }
  u64 start_index = dyn_array_length(*stack) - argument_count;
  Value *first_arg = *dyn_array_get(*stack, start_index);
  Value *last_arg = *dyn_array_last(*stack);
  Source_Range source_range;
  if(first_arg->source_range.file == last_arg->source_range.file) {
    source_range = last_arg->source_range;
    source_range.offsets.from = first_arg->source_range.offsets.from;
  } else {
    source_range = stack_entry->source_range;
  }
  Value_View args_view = {
    .values = dyn_array_get(*stack, start_index),
    .length = argument_count,
    .source_range = source_range,
  };
  Value *result_value = 0;
  switch(operator->tag) {
    case Operator_Tag_Alias: {
      result_value = operator->Alias.handler(context, parser, args_view, operator);
    } break;
    case Operator_Tag_Intrinsic: {
      Mass_Intrinsic_Proc proc = (Mass_Intrinsic_Proc)mass_ensure_jit_function_for_value(
        context, operator->Intrinsic.body, &source_range
      );
      if (mass_has_error(context)) return;
      result_value = proc(context, parser, args_view);
    } break;
  }
  if (mass_has_error(context)) return;

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
  const Source_Range *source_range,
  Mass_If_Expression_Lazy_Payload *payload
) {
  Expected_Result expected_condition = expected_result_any(&descriptor__bool);
  Value *condition = value_force(context, builder, &expected_condition, payload->condition);
  if (mass_has_error(context)) return 0;

  Program *program = context->program;
  Label *else_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("else"));

  encode_inverted_conditional_jump(builder, else_label, &condition->source_range, condition);
  storage_release_if_temporary(builder, &condition->storage);

  Label *after_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("endif"));

  Value *result_value = value_force(context, builder, expected_result, payload->then);
  if (mass_has_error(context)) return 0;

  Source_Range after_then_body_source_range = payload->then->source_range;
  after_then_body_source_range.offsets.from = after_then_body_source_range.offsets.to;
  push_eagerly_encoded_assembly(
    &builder->code_block, after_then_body_source_range,
    &(Instruction_Assembly){jmp, {code_label32(after_label)}}
  );

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
    .Label.pointer = else_label,
  });

  value_force_exact(context, builder, result_value, payload->else_);
  if (mass_has_error(context)) return 0;

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
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
  Value *keyword = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols._if
  );
  if (!keyword) return 0;

  Value *value_condition;
  {
    Value_View condition_view = value_view_slice(&view, peek_index, view.length);
    u32 condition_length;
    value_condition = token_parse_expression(
      context, parser, condition_view, &condition_length, context->compilation->common_symbols.then
    );
    peek_index += condition_length;
    if (mass_has_error(context)) return 0;
  }

  Value *value_then;
  {
    Value_View then_view = value_view_slice(&view, peek_index, view.length);
    u32 then_length;
    value_then = token_parse_expression(
      context, parser, then_view, &then_length, context->compilation->common_symbols._else
    );
    peek_index += then_length;
    if (mass_has_error(context)) return 0;
  }

  Value *value_else;
  {
    Value_View else_view = value_view_slice(&view, peek_index, view.length);
    u32 else_length;
    value_else = token_parse_expression(context, parser, else_view, &else_length, end_symbol);
    peek_index += else_length;
    if (mass_has_error(context)) return 0;
  }

  *matched_length = peek_index;

  if (mass_value_is_compile_time_known(value_condition)) {
    if (!mass_value_ensure_static_of(context, value_condition, &descriptor__bool)) {
      return 0;
    }
    bool condition = *value_as__bool(value_condition);
    return condition ? value_then : value_else;
  }

  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(value_then);
  if (context_is_compile_time_eval(context)) {
    result_descriptor = deduce_runtime_descriptor_for_value(
      context, value_else, result_descriptor
    );
  }

  Mass_If_Expression_Lazy_Payload *payload =
    allocator_allocate(context->allocator, Mass_If_Expression_Lazy_Payload);
  *payload = (Mass_If_Expression_Lazy_Payload){
    .condition = value_condition,
    .then = value_then,
    .else_ = value_else,
  };

  return mass_make_lazy_value(
    context, parser, keyword->source_range, payload, result_descriptor, mass_handle_if_expression_lazy_proc
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
  Scope *function_scope = scope_make(context->allocator, parser->scope);

  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info, function_return_exact(returns, *source_range));

  Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
  *literal = (Function_Literal){
    .info = fn_info,
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
  assert(args_view.length == 2);
  assert(value_match_symbol(value_view_get(args_view, 0), slice_literal("intrinsic")));

  Value *body = value_view_get(args_view, 1);
  if (!value_is_ast_block(body)) {
    context_parse_error(context, parser, args_view, 1);
    return 0;
  }
  const Source_Range *source_range = &args_view.source_range;

  Function_Literal *literal = mass_make_fake_function_literal(
    context, parser, body, &descriptor_value_pointer, source_range
  );

  // @Volatile :IntrinsicFunctionSignature
  // These arguments must match how we call it.
  literal->info->parameters = dyn_array_make(
    Array_Function_Parameter,
    .allocator = context->allocator,
    .capacity = 2
  );
  dyn_array_push(literal->info->parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("context")),
    .descriptor = &descriptor_mass_context_pointer,
    .source_range = *source_range,
  });
  dyn_array_push(literal->info->parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("parser")),
    .descriptor = &descriptor_parser_pointer,
    .source_range = *source_range,
  });
  dyn_array_push(literal->info->parameters, (Function_Parameter) {
    // TODO make a common symbol for this
    .symbol = mass_ensure_symbol(context->compilation, slice_literal("arguments")),
    .descriptor = &descriptor_value_view,
    .source_range = *source_range,
  });
  literal->info->flags |= Function_Info_Flags_Intrinsic;

  return value_make(context->allocator, &descriptor_function_literal, storage_static(literal), *source_range);
}

static Function_Info *
function_info_from_parameters_and_return(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view,
  Function_Return returns
) {
  Parser arg_parser = *parser;
  arg_parser.scope = scope_make(context->allocator, parser->scope);
  arg_parser.epoch = get_new_epoch();

  Function_Info *fn_info = allocator_allocate(context->allocator, Function_Info);
  function_info_init(fn_info, returns);

  Temp_Mark temp_mark = context_temp_mark(context);

  if (args_view.length != 0) {
    bool previous_argument_has_default_value = false;

    Array_Function_Parameter temp_params = dyn_array_make(
      Array_Function_Parameter,
      .allocator = context->temp_allocator,
      .capacity = 32,
    );

    for (Value_View_Split_Iterator it = { .view = args_view }; !it.done;) {
      Value_View param_view = token_split_next(&it, context->compilation->common_symbols.operator_comma);
      Function_Parameter param = token_match_argument(context, &arg_parser, param_view, fn_info);
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
    dyn_array_copy_from_temp(Array_Function_Parameter, context, &fn_info->parameters, temp_params);
  }

  if (mass_has_error(context)) return 0;

  defer:
  context_temp_reset_to_mark(context, temp_mark);
  return fn_info;
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
  bool is_macro = false;
  Value *at = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator_at
  );
  Value *keyword = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.fn
  );
  if (!keyword) {
    keyword = value_view_maybe_match_cached_symbol(
      view, &peek_index, context->compilation->common_symbols.macro
    );
    is_macro = true;
  }
  if (!keyword) return 0;

  Value *args = value_view_next(view, &peek_index);
  if (!value_is_group_paren(args)) {
    context_parse_error(context, parser, view, peek_index);
    return 0;
  }

  Value *arrow = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator_arrow
  );
  bool is_compile_time = !!at;
  if (!arrow) {
    arrow = value_view_maybe_match_cached_symbol(
      view, &peek_index, context->compilation->common_symbols.operator_fat_arrow
    );
    if (arrow) is_compile_time = true;
  }

  if (is_macro && is_compile_time) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = keyword->source_range,
      .detailed_message = slice_literal("Function-like macro can not be marked compile time"),
    });
    return 0;
  }

  Function_Return returns;
  if (arrow) {
    Value *token = value_view_next(view, &peek_index);
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
      Value_View return_types_view = value_view_make_single(context->allocator, token);
      returns = function_return_generic(return_types_view, return_range);
    }
  } else {
    Source_Range return_range = value_view_slice(&view, peek_index, peek_index).source_range;
    returns = function_return_exact(&descriptor_void, return_range);
  }

  Value_View args_view = value_as_group_paren(args)->children;
  Function_Info *fn_info =
    function_info_from_parameters_and_return(context, parser, args_view, returns);
  if (mass_has_error(context)) return 0;


  Value *body_value = value_view_peek(view, peek_index);
  if (body_value) {
    if (value_is_ast_block(body_value)) {
      peek_index += 1;
    } else {
      body_value = 0;
    }
  }
  if (!body_value) {
    Value_View rest = value_view_match_till_symbol(view, &peek_index, end_symbol);
    if (is_macro) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = rest.source_range,
        .detailed_message = slice_literal("Function-like macro must have a literal body in {}"),
      });
      return 0;
    }
    if (rest.length) body_value = compile_time_eval(context, parser, rest);
  }
  if (mass_has_error(context)) return 0;

  *matched_length = peek_index;

  if (is_compile_time) {
    fn_info->flags |= Function_Info_Flags_Compile_Time;
  }
  bool is_syscall = body_value && body_value->descriptor == &descriptor_syscall;

  // TODO should be extracted from the :: if available or maybe stored separately from Descriptor
  Slice name = {0};

  // TODO support this on non-Linux systems
  if (is_syscall) {
    ensure_parameter_descriptors(context, fn_info, parser->scope);
    Function_Call_Setup call_setup =
      calling_convention_x86_64_system_v_syscall.call_setup_proc(context->allocator, fn_info);
    // TODO this patching after the fact feels awkward and brittle
    i64 syscall_number = value_as_syscall(body_value)->number;
    assert(call_setup.jump.tag == Function_Call_Jump_Tag_Syscall);
    call_setup.jump.Syscall.number = u64_to_s64(syscall_number.bits);

    Descriptor *fn_descriptor =
      descriptor_function_instance(context->allocator, name, fn_info, call_setup);

    return value_make(
      context->allocator, fn_descriptor, storage_none, view.source_range
    );
  } else if (body_value) {
    Function_Literal_Flags flags = Function_Literal_Flags_None;
    if (value_is_intrinsic(body_value)) {
      fn_info->flags |= Function_Info_Flags_Intrinsic;
    }

    DYN_ARRAY_FOREACH(Function_Parameter, param, fn_info->parameters) {
      if (param->tag == Function_Parameter_Tag_Generic) {
        flags |= Function_Literal_Flags_Generic;
        break;
      }
    }
    if (is_macro) flags |= Function_Literal_Flags_Macro;
    if (!(flags & Function_Literal_Flags_Generic)) {
      ensure_parameter_descriptors(context, fn_info, parser->scope);
      if (mass_has_error(context)) return 0;
    }
    Function_Literal *literal = allocator_allocate(context->allocator, Function_Literal);
    *literal = (Function_Literal){
      .flags = flags,
      .info = fn_info,
      .body = body_value,
      .own_scope = parser->scope,
    };
    return value_make(context->allocator, &descriptor_function_literal, storage_static(literal), view.source_range);
  } else {
    ensure_parameter_descriptors(context, fn_info, parser->scope);
    if (mass_has_error(context)) return 0;

    const Calling_Convention *calling_convention =
      context->compilation->runtime_program->default_calling_convention;
    Function_Call_Setup call_setup =
      calling_convention->call_setup_proc(context->allocator, fn_info);
    Descriptor *fn_descriptor =
      descriptor_function_instance(context->allocator, name, fn_info, call_setup);
    Storage fn_storage = storage_immediate(&fn_descriptor);
    return value_make(context->allocator, &descriptor_descriptor_pointer, fn_storage, view.source_range);
  }
}

typedef Value *(*Expression_Matcher_Proc)(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *out_match_length,
  const Symbol *end_symbol
);

typedef struct {
  Expression_Matcher_Proc proc;
  bool matches_end_of_expression;
} Expression_Matcher;

static Value *
token_parse_expression(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  u32 *out_match_length,
  const Symbol *end_symbol
) {
  if(view.length == 0) {
    // TODO this probably should be special cased in the `()` handling
    //      and not allow generally empty expressions
    return mass_make_void(context->allocator, view.source_range);
  }
  if(view.length == 1) {
    *out_match_length = 1;
    return token_parse_single(context, parser, value_view_get(view, 0));
  }

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
  u32 matched_length = view.length;

  for (u32 i = 0; ; ++i) {
    repeat:
    if (i >= view.length) break;

    Value_View rest = value_view_rest(&view, i);

    { // if expression
      u32 match_length = 0;
      Value *match_result = token_parse_if_expression(context, parser, rest, &match_length, end_symbol);
      if (mass_has_error(context)) goto defer;
      if (match_length) {
        dyn_array_push(value_stack, match_result);
        i += match_length; // Skip over the matched slice
        matched_length = i;
        goto drain;
      }
    }

    { // function literal
      u32 match_length = 0;
      Value *match_result = token_parse_function_literal(context, parser, rest, &match_length, end_symbol);
      if (mass_has_error(context)) goto defer;
      if (match_length) {
        dyn_array_push(value_stack, match_result);
        i += match_length; // Skip over the matched slice
        goto repeat;
      }
    }

    Operator_Fixity fixity_mask = is_previous_an_operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Infix | Operator_Fixity_Postfix;

    Value *value = value_view_get(view, i);

    if (end_symbol && value_is_symbol(value) && value_as_symbol(value) == end_symbol) {
      matched_length = i + 1;
      goto drain;
    }

    if (value_is_symbol(value)) {
      const Operator *maybe_operator = scope_lookup_operator(
        context, parser->scope, value_as_symbol(value)->name, fixity_mask
      );
      if (maybe_operator) {
        if (!token_handle_operator(
          context, parser, view, &value_stack, &operator_stack, maybe_operator, value->source_range
        )) goto defer;
        is_previous_an_operator = (maybe_operator->fixity != Operator_Fixity_Postfix);
        continue;
      }
    }

    if (!is_previous_an_operator) {
      const Operator *empty_space_operator = &context->compilation->apply_operator;
      if (!token_handle_operator(
        context, parser, view, &value_stack, &operator_stack, empty_space_operator, value->source_range
      )) goto defer;
    }
    dyn_array_push(value_stack, value);
    is_previous_an_operator = false;
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

static Value *
mass_handle_block_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_block_result,
  const Source_Range *block_source_range,
  void *raw_payload
) {
  Array_Value_Ptr lazy_statements;
  UNPACK_FROM_VOID_POINTER(lazy_statements, raw_payload);
  u64 statement_count = dyn_array_length(lazy_statements);
  assert(statement_count);
  Value *result_value = 0;
  Expected_Result expected_void = mass_expected_result_exact(&descriptor_void, storage_none);
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
    result_value = value_force(context, builder, expected_result, lazy_statement);
    if (mass_has_error(context)) return 0;
    // We do not do cross-statement register allocation so can check that there
    // are no stray registers retained across statement boundaries except when a block
    // returns a flexible result from a function call in the last statement.
    if (!builder) continue;
    Register_Bitset registers_after = builder->register_occupied_bitset;
    if (is_last_statement && expected_result->tag == Expected_Result_Tag_Flexible) {
      if (result_value->storage.tag == Storage_Tag_Register) {
        Register result_register = result_value->storage.Register.index;
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
token_parse_block_statements(
  Mass_Context *context,
  Parser *parser,
  Array_Value_View statements,
  const Source_Range *source_range
) {
  u64 max_statement_count = dyn_array_length(statements);

  if (!max_statement_count) {
    return mass_make_void(context->allocator, *source_range);
  }
  Value *block_result = 0;

  Temp_Mark temp_mark = context_temp_mark(context);
  Array_Value_Ptr temp_lazy_statements = dyn_array_make(
    Array_Value_Ptr,
    .allocator = context->temp_allocator,
    .capacity = max_statement_count,
  );

  DYN_ARRAY_FOREACH(Value_View, statement, statements) {
    if (mass_has_error(context)) goto defer;

    Lazy_Value lazy_value = {
      .epoch = parser->epoch,
      .descriptor = &descriptor_void,
    };

    u32 match_length = token_statement_matcher_in_scopes(context, parser, *statement, &lazy_value, parser->scope);
    if (mass_has_error(context)) goto defer;
    if (match_length) {
      if (match_length != statement->length) {
        Value_View remainder = value_view_rest(statement, match_length);
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = remainder.source_range,
        });
        goto defer;
      }
      // If the statement did not assign a proc that means that it does not need
      // to output any instructions and there is nothing to force.
      if (lazy_value.proc) {
        assert(lazy_value.descriptor);
        allocator_allocate_bulk(context->allocator, combined, {
          Lazy_Value lazy_value;
          Value value;
        });

        Lazy_Value *lazy_value_storage = &combined->lazy_value;
        *lazy_value_storage = lazy_value;
        Storage storage = storage_static(lazy_value_storage);
        value_init(&combined->value, &descriptor_lazy_value, storage, statement->source_range);
        dyn_array_push(temp_lazy_statements, &combined->value);
      }
    } else {
      Value *parse_result = token_parse_expression(context, parser, *statement, &match_length, 0);
      if (mass_has_error(context)) goto defer;

      if (match_length != statement->length) {
        Value_View remainder = value_view_rest(statement, match_length);
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Parse,
          .source_range = remainder.source_range,
        });
        goto defer;
      }

      if (mass_value_is_compile_time_known(parse_result)) {
        if (value_is_assignment(parse_result)) {
          // TODO make lazy value accept const payload
          Assignment *assignment = (Assignment *)value_as_assignment(parse_result);
          parse_result = mass_make_lazy_value(
            context, parser, parse_result->source_range, assignment,
            &descriptor_void, mass_handle_assignment_lazy_proc
          );
        } else if (parse_result->descriptor == &descriptor_typed_symbol) {
          parse_result = mass_define_stack_value_from_typed_symbol(
            context, parser, value_as_typed_symbol(parse_result), parse_result->source_range
          );
        } else if (value_is_module_exports(parse_result)) {
          if (!(parser->flags & Parser_Flags_Global)) {
            mass_error(context, (Mass_Error) {
              .tag = Mass_Error_Tag_Parse,
              .source_range = statement->source_range,
              .detailed_message = slice_literal("Export declarations are only supported at top level"),
            });
            goto defer;
          }
          if (parser->module->exports.tag != Module_Exports_Tag_Not_Specified) {
            mass_error(context, (Mass_Error) {
              .tag = Mass_Error_Tag_Parse,
              .source_range = statement->source_range,
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
      dyn_array_push(temp_lazy_statements, parse_result);
    }
  }

  if (mass_has_error(context)) goto defer;

  u64 statement_count = dyn_array_length(temp_lazy_statements);
  if (statement_count) {
    Value *last_result = *dyn_array_last(temp_lazy_statements);
    const Descriptor *last_descriptor = value_or_lazy_value_descriptor(last_result);
    if (statement_count == 1) {
      block_result = last_result;
    } else {
      Array_Value_Ptr lazy_statements;
      dyn_array_copy_from_temp(Array_Value_Ptr, context, &lazy_statements, temp_lazy_statements);

      void *payload;
      PACK_AS_VOID_POINTER(payload, lazy_statements);

      block_result = mass_make_lazy_value(
        context, parser, last_result->source_range, payload, last_descriptor, mass_handle_block_lazy_proc
      );
    }
  } else {
    block_result = mass_make_void(context->allocator, *source_range);
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
  block_parser.scope = scope_make(context->allocator, parser->scope);
  return token_parse_block_statements(context, &block_parser, group->statements, source_range);
}

static u32
token_parse_statement_using(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  u32 peek_index = 0;
  Value *keyword = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.using
  );
  if (!keyword) return 0;
  Value_View rest = value_view_match_till_end_of_statement(context, view, &peek_index);

  Value *result = compile_time_eval(context, parser, rest);
  if (mass_has_error(context)) return 0;
  if (!mass_value_ensure_static_of(context, result, &descriptor_module)) {
    goto err;
  }

  const Module *module = value_as_module(result);
  mass_copy_scope_exports(parser->scope, module->exports.scope);

  err:
  return peek_index;
}

static Value *
mass_handle_explicit_return_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  Value *parse_result
) {
  mass_assign(context, builder, &builder->return_value, parse_result, source_range);
  if (mass_has_error(context)) return 0;
  Storage return_label = code_label32(builder->code_block.end_label);

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly) {jmp, {return_label}}
  );

  Value *void_value = mass_make_void(context->allocator, *source_range);
  return expected_result_validate(expected_result, void_value);
}

static Value *
mass_handle_return_operator(
  Mass_Context *context,
  Parser *parser,
  Value_View args,
  const Operator *operator
) {
  assert(args.length == 1);
  Value *return_value = token_parse_single(context, parser, value_view_get(args, 0));

  return mass_make_lazy_value(
    context, parser, args.source_range, return_value, &descriptor_void,
    mass_handle_explicit_return_lazy_proc
  );
}

static void
token_define_global_variable(
  Mass_Context *context,
  Parser *parser,
  Value *symbol,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = token_parse_expression(context, parser, expression, &(u32){0}, 0);
  if (mass_has_error(context)) return;

  const Descriptor *descriptor = deduce_runtime_descriptor_for_value(context, value, 0);
  if (!descriptor) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_No_Runtime_Use,
      .source_range = expression.source_range,
    });
    return;
  }
  Value *global_value;
  if (storage_is_label(&value->storage)) {
    global_value = value;
  } else {
    Section *section = &context->program->memory.rw_data;
    u64 byte_size = descriptor_byte_size(descriptor);
    u64 alignment = descriptor_byte_alignment(descriptor);

    Label *label = allocate_section_memory(
      context->allocator, context->program, section, byte_size, alignment
    );
    Storage global_storage = data_label32(label, descriptor->bit_size);
    global_value = value_make(context->allocator, descriptor, global_storage, expression.source_range);

    if (mass_value_is_compile_time_known(value)) {
      mass_assign(context, 0, global_value, value, &expression.source_range);
      if (mass_has_error(context)) return;
    } else {
      Assignment *assignment_payload = mass_allocate(context, Assignment);
      *assignment_payload = (Assignment) {
        .target = global_value,
        .source = value,
      };

      Value *body_value = mass_make_lazy_value(
        context, parser, symbol->source_range, assignment_payload, &descriptor_void,
        mass_handle_assignment_lazy_proc
      );

      Function_Literal *startup_literal = mass_make_fake_function_literal(
        context, parser, body_value, &descriptor_void, &expression.source_range
      );
      Value *startup_function = value_make(
        context->allocator, &descriptor_function_literal, storage_static(startup_literal), value->source_range
      );
      Compilation *compilation = context->compilation;
      Program *program = compilation->runtime_program;
      ensure_function_instance(context, startup_function, (Value_View){0});
      dyn_array_push(program->startup_functions, startup_function);
    }
  }

  scope_define_value(parser->scope, VALUE_STATIC_EPOCH, symbol->source_range, value_as_symbol(symbol), global_value);
}

static void
token_define_local_variable(
  Mass_Context *context,
  Parser *parser,
  Value *symbol,
  Lazy_Value *out_lazy_value,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = token_parse_expression(context, parser, expression, &(u32){0}, 0);
  if (mass_has_error(context)) return;
  const Descriptor *variable_descriptor = deduce_runtime_descriptor_for_value(context, value, 0);
  if (mass_has_error(context)) return;
  if (!variable_descriptor) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_No_Runtime_Use,
      .source_range = expression.source_range,
    });
    return;
  }

  Mass_Variable_Definition_Lazy_Payload *variable_payload =
    mass_allocate(context, Mass_Variable_Definition_Lazy_Payload);
  *variable_payload = (Mass_Variable_Definition_Lazy_Payload){
    .descriptor = variable_descriptor,
  };

  Value *variable_value = mass_make_lazy_value(
    context, parser, symbol->source_range, variable_payload, variable_descriptor,
    mass_handle_variable_definition_lazy_proc
  );

  const Source_Range *source_range = &symbol->source_range;

  scope_define_value(parser->scope, parser->epoch, *source_range, value_as_symbol(symbol), variable_value);

  Assignment *assignment_payload = mass_allocate(context, Assignment);
  *assignment_payload = (Assignment) {
    .target = variable_value,
    .source = value,
  };

  out_lazy_value->proc = mass_handle_assignment_lazy_proc;
  out_lazy_value->payload = assignment_payload;
}

static u32
token_parse_definition_and_assignment_statements(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Lazy_Value *out_lazy_value,
  void *unused_payload
) {
  Value_View lhs;
  Value_View rhs;
  Value *operator;

  u32 statement_length = 0;
  view = value_view_match_till_end_of_statement(context, view, &statement_length);
  if (!token_maybe_split_on_operator(view, slice_literal(":="), &lhs, &rhs, &operator)) {
    return 0;
  }
  if (lhs.length > 1) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Unimplemented,
      .source_range = lhs.source_range,
      .detailed_message = slice_literal("multiple assignments"),
    });
    goto err;
  }
  Value *name_token = value_view_get(view, 0);

  if (!value_is_symbol(name_token)) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Invalid_Identifier,
      .source_range = name_token->source_range,
      .Invalid_Identifier = {.id = name_token},
    });
    goto err;
  }

  if (parser->flags & Parser_Flags_Global) {
    token_define_global_variable(context, parser, name_token, rhs);
  } else {
    token_define_local_variable(context, parser, name_token, out_lazy_value, rhs);
  }

  err:
  return statement_length;
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
  Scope *enum_scope = scope_make(allocator, 0);
  for (u64 i = 0; i < item_count; ++i) {
    C_Enum_Item *it = &items[i];
    const Descriptor *enum_descriptor = *value_as_descriptor_pointer(enum_type_value);
    Value *item_value = value_make(
      allocator, enum_descriptor, storage_immediate(&it->value), source_range
    );
    const Symbol *it_symbol = mass_ensure_symbol(compilation, it->name);
    scope_define_value(enum_scope, VALUE_STATIC_EPOCH, source_range, it_symbol, item_value);
  }

  const Symbol *type_symbol = mass_ensure_symbol(compilation, slice_literal("_Type"));
  scope_define_value(enum_scope, VALUE_STATIC_EPOCH, source_range, type_symbol, enum_type_value);

  Module *enum_module = allocator_allocate(allocator, Module);
  *enum_module = (Module) {
    .source_range = source_range,
    .own_scope = enum_scope,
    .exports = {
      .tag = Module_Exports_Tag_All,
      .scope = enum_scope,
    },
  };

  Value *enum_value = value_make(
    allocator, &descriptor_module, storage_static(enum_module), source_range
  );
  const Symbol *enum_symbol = mass_ensure_symbol(compilation, enum_name);
  scope_define_value(scope, VALUE_STATIC_EPOCH, source_range, enum_symbol, enum_value);
}

static void
module_compiler_init(
  Compilation *compilation,
  Module *out_module
) {
  const Allocator *allocator = compilation->allocator;
  Scope *scope = scope_make(allocator, compilation->root_scope);
  *out_module = (Module) {
    .source_range = {0},
    .own_scope = scope,
    .exports = {
      .tag = Module_Exports_Tag_All,
      .scope = scope,
    },
  };
  INIT_LITERAL_SOURCE_RANGE(&out_module->source_range, "MASS");
  compiler_scope_define_exports(compilation, scope);
}

static void
scope_define_builtins(
  Compilation *compilation,
  Scope *scope
) {
  const Allocator *allocator = compilation->allocator;
  Mass_Context context = mass_context_from_compilation(compilation);

  global_scope_define_exports(compilation, scope);
  Source_Range type_source_range;
  INIT_LITERAL_SOURCE_RANGE(&type_source_range, "Type");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, type_source_range,
    mass_ensure_symbol(compilation, slice_literal("Type")), type_descriptor_pointer_value
  );

  Value *compiler_module_value = value_make(
    allocator, &descriptor_module, storage_static(&compilation->compiler_module), (Source_Range){0}
  );
  INIT_LITERAL_SOURCE_RANGE(&compiler_module_value->source_range, "MASS");
  scope_define_value(
    scope, VALUE_STATIC_EPOCH, compiler_module_value->source_range,
    mass_ensure_symbol(compilation, slice_literal("MASS")), compiler_module_value
  );

  Source_Range dot_source_range;
  INIT_LITERAL_SOURCE_RANGE(&dot_source_range, ".");
  scope_define_operator(&context, scope, dot_source_range, slice_literal("."), allocator_make(allocator, Operator,
    .precedence = 20,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .tag = Operator_Tag_Alias,
    .Alias.handler = mass_handle_dot_operator,
  ));
  {
    Source_Range quote_source_range;
    INIT_LITERAL_SOURCE_RANGE(&quote_source_range, "'");
    scope_define_operator(&context, scope, quote_source_range, slice_literal("'"), allocator_make(allocator, Operator,
      .precedence = 30,
      .fixity = Operator_Fixity_Prefix,
      .associativity = Operator_Associativity_Right,
      .tag = Operator_Tag_Alias,
      .Alias.handler = mass_quote,
    ));
    scope_define_operator(&context, scope, quote_source_range, slice_literal("'"), allocator_make(allocator, Operator,
      .precedence = 30,
      .fixity = Operator_Fixity_Postfix,
      .associativity = Operator_Associativity_Right,
      .tag = Operator_Tag_Alias,
      .Alias.handler = mass_unquote,
    ));
  }
  Source_Range dot_star_source_range;
  INIT_LITERAL_SOURCE_RANGE(&dot_star_source_range, ".*");
  scope_define_operator(&context, scope, dot_star_source_range, slice_literal(".*"), allocator_make(allocator, Operator,
    .precedence = 20,
    .fixity = Operator_Fixity_Postfix,
    .associativity = Operator_Associativity_Left,
    .tag = Operator_Tag_Alias,
    .Alias.handler = mass_handle_dereference_operator,
  ));
  Source_Range colon_source_range;
  INIT_LITERAL_SOURCE_RANGE(&colon_source_range, ":");
  scope_define_operator(&context, scope, colon_source_range, slice_literal(":"), allocator_make(allocator, Operator,
    .precedence = 2,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .tag = Operator_Tag_Alias,
    .Alias.handler = mass_handle_typed_symbol_operator,
  ));
  Source_Range equal_source_range;
  INIT_LITERAL_SOURCE_RANGE(&equal_source_range, "=");
  scope_define_operator(&context, scope, equal_source_range, slice_literal("="), allocator_make(allocator, Operator,
    .precedence = 1,
    .fixity = Operator_Fixity_Infix,
    .associativity = Operator_Associativity_Left,
    .tag = Operator_Tag_Alias,
    .Alias.handler = mass_handle_assignment_operator,
  ));
  Source_Range return_source_range;
  INIT_LITERAL_SOURCE_RANGE(&return_source_range, "return");
  scope_define_operator(&context, scope, return_source_range, slice_literal("return"), allocator_make(allocator, Operator,
    .precedence = 0,
    .fixity = Operator_Fixity_Prefix,
    .associativity = Operator_Associativity_Right,
    .tag = Operator_Tag_Alias,
    .Alias.handler = mass_handle_return_operator,
  ));

  {
    static Token_Statement_Matcher default_statement_matchers[] = {
      {.proc = token_parse_constant_definitions},
      {.proc = token_parse_definition_and_assignment_statements},
      {.proc = token_parse_statement_using},
      {.proc = token_parse_while},
      {.proc = token_parse_operator_definition},
    };
    for (u64 i = 0; i < countof(default_statement_matchers) - 1; ++i) {
      default_statement_matchers[i + 1].previous = &default_statement_matchers[i];
    }

    scope->statement_matcher = &default_statement_matchers[countof(default_statement_matchers) - 1];
  }
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
      break;
    }
    case Module_Exports_Tag_Selective: {
      module->exports.scope = scope_make(context->allocator, module->own_scope->parent);
      Array_Value_Ptr symbols = module->exports.Selective.symbols;
      for(u64 i = 0; i < dyn_array_length(symbols); i += 1) {
        Value **symbol_pointer = dyn_array_get(symbols, i);
        const Symbol *symbol = value_as_symbol(*symbol_pointer);
        Scope_Entry *entry = scope_lookup_shallow(module->own_scope, symbol);
        if (!entry) {
          mass_error(context, (Mass_Error) {
            .tag = Mass_Error_Tag_Undefined_Variable,
            .source_range = (*symbol_pointer)->source_range,
            .Undefined_Variable = {.name = symbol->name},
          });
        }

        Value_View expr = value_view_single(symbol_pointer);
        scope_define_lazy_compile_time_expression(context, parser, module->exports.scope, symbol, expr);
      }
      break;
    }
  }
}

static Value *
mass_inline_module(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 2);
  assert(value_match_symbol(value_view_get(args, 0), slice_literal("module")));
  const Ast_Block *curly = value_as_ast_block(value_view_get(args, 1));

  Module *module = mass_allocate(context, Module);
  *module = (Module) {
    .source_range = args.source_range,
    .own_scope = scope_make(context->allocator, parser->scope),
  };
  Parser module_parser = {
    .flags = Parser_Flags_Global,
    .scope = module->own_scope,
    .module = module,
    .epoch = VALUE_STATIC_EPOCH,
  };
  // Need a new context here to ensure we are using the runtime program for new modules
  Mass_Context module_context = mass_context_from_compilation(context->compilation);
  Value *block_result =
    token_parse_block_statements(&module_context, &module_parser, curly->statements, &args.source_range);
  Value *void_value = mass_make_void(context->allocator, args.source_range);
  value_force_exact(&module_context, 0, void_value, block_result);
  module_process_exports(&module_context, &module_parser);

  return value_make(module_context.allocator, &descriptor_module, storage_static(module), args.source_range);
}

static inline void
program_parse(
  Mass_Context *context,
  Parser *parser
) {
  assert(parser->module);
  Performance_Counter perf = system_performance_counter_start();
  Array_Value_View statements;
  const Source_Range *program_range = &parser->module->source_range;
  *context->result = tokenize(context, *program_range, &statements);
  if (mass_has_error(context)) return;
  if (0) {
    u64 usec = system_performance_counter_end(&perf);
    printf("Tokenizer took %"PRIu64" µs\n", usec);
  }

  Value *block_result = token_parse_block_statements(context, parser, statements, program_range);
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
        char ch = source_range->file->text.bytes[offsets->from];
        if (ch == '\n') {
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
  Module *root_module = program_module_from_file(context, file_path, compilation->root_scope);
  if (mass_has_error(context)) return;

  // Trim leading whitespace and possible a shebang
  Source_Range source_range = root_module->source_range;
  mass_maybe_trim_shebang(&source_range);

  Array_Value_View statements;
  *context->result = tokenize(context, source_range, &statements);
  if (mass_has_error(context)) return;

  Ast_Block body = {.statements = statements};
  Value *fake_function_body = value_make(
    context->allocator, &descriptor_ast_block, storage_immediate(&body), source_range
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
  Value *entry =
    value_make(context->allocator, &descriptor_function_literal, storage_static(literal), source_range);
  entry = ensure_function_instance(context, entry, (Value_View){0});

  context->program->entry_point = entry;
  if (mass_has_error(context)) return;

  Jit jit;
  jit_init(&jit, context->program);
  program_jit(context, &jit);
  if (mass_has_error(context)) return;
  fn_type_opaque script = value_as_function(jit.program, jit.program->entry_point);
  script();
}