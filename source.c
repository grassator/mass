#include "prelude.h"
#include "source.h"
#include "function.h"

#include "generated_exports.c"

static inline bool
mass_value_ensure_static(
  Mass_Context *context,
  Value *value
) {
  if (!mass_value_is_compile_time_known(value)) {
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

static inline bool
scope_statement_matcher_shallow(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Value_Lazy *out_lazy_value,
  const Scope *scope
) {
  const Token_Statement_Matcher *matcher = scope->statement_matcher;
  // Do a reverse iteration because we want statements that are defined later
  // to have higher precedence when parsing
  for (; matcher; matcher = matcher->previous) {
    if (matcher->proc(context, parser, view, out_lazy_value)) return true;
    if (mass_has_error(context)) return true;
  }
  return false;
}

static inline bool
token_statement_matcher_in_scopes(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Value_Lazy *out_lazy_value,
  const Scope *scope
) {
  for (; scope; scope = scope->parent) {
    if (scope_statement_matcher_shallow(context, parser, view, out_lazy_value, scope)) {
      return true;
    }
  }
  return false;
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

static bool
assign_from_static(
  Mass_Context *context,
  Function_Builder *builder,
  Value *target,
  Value *source,
  const Source_Range *source_range
) {
  const Storage *source_storage = &value_as_forced(source)->storage;
  const Storage *target_storage = &value_as_forced(target)->storage;
  if (
    !context_is_compile_time_eval(context) &&
    !mass_value_is_compile_time_known(target) &&
    source->descriptor->tag == Descriptor_Tag_Pointer_To
  ) {
    void *source_memory = *(void **)storage_static_memory_with_bit_size(source_storage, (Bits){64});
    if (!source_memory) {
      Storage null_pointer = imm64(0);
      move_value(builder, source_range, target_storage, &null_pointer);
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
      mass_assign_helper(context, builder, &target_program_value, &static_source_value, source_range);
    }
    if (mass_has_error(context)) return true;

    assert(storage_is_label(&target_program_storage));
    if (storage_is_label(target_storage)) {
      dyn_array_push(context->program->relocations, (Relocation) {
        .patch_at = *target_storage,
        .address_of = target_program_storage,
      });
    } else {
      mass_storage_load_address(builder, source_range, target_storage, &target_program_storage);
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
  const Source_Range *source_range
) {
  const Descriptor *referenced_descriptor;
  const Descriptor *source_descriptor = value_or_lazy_value_descriptor(source);
  if (source_descriptor->tag == Descriptor_Tag_Pointer_To) {
    referenced_descriptor = source_descriptor->Pointer_To.descriptor;
  } else {
    panic("Unexpected descriptor tag for an indirect value");
    return 0;
  }
  if (source->tag == Value_Tag_Lazy) {
    Expected_Result expected_result = expected_result_any(source_descriptor);
    source = value_force(context, builder, &expected_result, source);
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
      Storage reg_storage = storage_register(reg, source_descriptor->bit_size);
      move_value(builder, source_range, &reg_storage, source_storage);
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
  const Descriptor *original_descriptor = value_or_lazy_value_descriptor(value);
  if (descriptor_is_implicit_pointer(original_descriptor)) {
    return descriptor_as_pointer_to(original_descriptor)->descriptor;
  }

  if (!mass_value_is_compile_time_known(value)) {
    return original_descriptor;
  }

  if (value->descriptor == &descriptor_i64) {
    if (maybe_desired_descriptor && maybe_desired_descriptor != &descriptor_i64) {
      Literal_Cast_Result cast_result =
        value_i64_cast_to(value, maybe_desired_descriptor, &(u64){0}, &(u64){0});
      if (cast_result == Literal_Cast_Result_Success) {
        return maybe_desired_descriptor;
      } else {
        return 0;
      }
    }
    return original_descriptor;
  }

  if (value->descriptor == &descriptor_tuple) {
    const Tuple *tuple = value_as_tuple(value);
    return anonymous_struct_descriptor_from_tuple(context, tuple, Tuple_Eval_Mode_Value);
  }

  if (value->descriptor == &descriptor_overload || value->descriptor == &descriptor_function_literal) {
    Array_Function_Parameter parameters;
    if (maybe_desired_descriptor) {
      assert(maybe_desired_descriptor->tag == Descriptor_Tag_Function_Instance);
      parameters = maybe_desired_descriptor->Function_Instance.info->parameters;
    } else {
      if (value->descriptor == &descriptor_function_literal) {
        const Function_Literal *literal = value_as_function_literal(value);
        if (literal->header.flags & Function_Header_Flags_Macro) {
          return 0;
        }
        if (literal->header.flags & Function_Header_Flags_Generic) {
          return 0;
        }
        Function_Info info;
        mass_function_info_init_for_header_and_maybe_body(
          context, literal->own_scope, &literal->header, literal->body, &info
        );
        parameters = info.parameters;
      } else {
        return 0;
      }
    }
    if (mass_has_error(context)) return 0;
    Array_Value_Ptr fake_args =
      mass_fake_argument_array_from_parameters(context->temp_allocator, parameters);
    Value_View args_view = value_view_from_value_array(fake_args, &value->source_range);

    Overload_Match_Found match_found;
    if (!mass_match_overload_or_error(context, value, args_view, &match_found)) {
      return 0;
    }
    Function_Call_Setup call_setup =
      context->program->default_calling_convention->call_setup_proc(context->allocator, match_found.info);
    return descriptor_function_instance(
      context->allocator, match_found.info, call_setup, context->program
    );
  }

  return original_descriptor;
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

      const Storage *target_storage = &value_as_forced(target)->storage;
      Value target_field = {
        .tag = Value_Tag_Forced,
        .descriptor = field->descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(
          target_storage, u64_to_s32(field->offset), field->descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign_helper(context, builder, &target_field, field_source, source_range);
      if (mass_has_error(context)) goto err;
    }
  } else if (target->descriptor->tag == Descriptor_Tag_Fixed_Array) {
    const Storage *target_storage = &value_as_forced(target)->storage;
    u64 length = target->descriptor->Fixed_Array.length;
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

    const Descriptor *item_descriptor = target->descriptor->Fixed_Array.item;
    u64 item_byte_size = descriptor_byte_size(item_descriptor);
    for (u64 index = 0; index < length; ++index) {
      Value *tuple_item = *dyn_array_get(tuple->items, index);
      s32 byte_offset = u64_to_s32(item_byte_size * index);
      Value target_field = {
        .tag = Value_Tag_Forced,
        .descriptor = item_descriptor,
        .Forced.storage = storage_with_offset_and_bit_size(
          target_storage, byte_offset, item_descriptor->bit_size
        ),
        .source_range = target->source_range,
      };
      mass_assign_helper(context, builder, &target_field, tuple_item, source_range);
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

static void
mass_assign_helper(
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

  if (source->tag == Value_Tag_Lazy) {
    value_force_exact(context, builder, target, source);
    return;
  }
  assert(target->tag != Value_Tag_Lazy);
  const Storage *source_storage = &value_as_forced(source)->storage;
  const Storage *target_storage = &value_as_forced(target)->storage;

  if (mass_descriptor_is_void(target->descriptor)) {
    return;
  }

  if (descriptor_is_implicit_pointer(source->descriptor)) {
    Value *ref_source = value_indirect_from_pointer(context, builder, source, source_range);
    mass_assign_helper(context, builder, target, ref_source, source_range);
    storage_release_if_temporary(builder, &value_as_forced(ref_source)->storage);
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
      Value *referenced_target = value_indirect_from_pointer(context, builder, target, source_range);
      mass_assign_helper(context, builder, referenced_target, source, source_range);
      storage_release_if_temporary(builder, &value_as_forced(referenced_target)->storage);
      return;
    } else {
      const Descriptor *original_descriptor = target->descriptor->Pointer_To.descriptor;
      if (!same_type(original_descriptor, source->descriptor)) goto err;
      mass_storage_load_address(builder, source_range, target_storage, source_storage);
    }
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Raw && !source->descriptor->brand) {
    if (value_is_i64(source)) {
      if (target->descriptor->tag == Descriptor_Tag_Pointer_To) {
        const i64 *literal = value_as_i64(source);
        if (literal->bits == 0) {
          Storage zero = imm64(0);
          move_value(builder, source_range, target_storage, &zero);
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
      source_storage = &value_as_forced(source)->storage;
    }
  }

  if (value_is_tuple(source)) {
    assign_tuple(context, builder, target, source, source_range);
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
      Array_Value_Ptr fake_args =
        mass_fake_argument_array_from_parameters(context->temp_allocator, target_info->parameters);
      Value_View args_view = value_view_from_value_array(fake_args, &target->source_range);

      Overload_Match_Found match_found;
      if (!mass_match_overload_or_error(context, source, args_view, &match_found)) {
        return;
      }

      source = match_found.value;
      if (source->descriptor->tag != Descriptor_Tag_Function_Instance) {
        const Function_Literal *literal = value_as_function_literal(match_found.value);
        source = mass_function_literal_instance_for_info(context, literal, match_found.info);
      }
      if (mass_has_error(context)) return;
      assert(source->descriptor->tag == Descriptor_Tag_Function_Instance);
      source_storage = &value_as_forced(source)->storage;
    }

    if (same_type(target->descriptor, source->descriptor)) {
      if (source_storage->tag == Storage_Tag_Memory) {
        mass_storage_load_address(builder, source_range, target_storage, source_storage);
      } else {
        move_value(builder, source_range, target_storage, source_storage);
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
      mass_assign_helper(context, builder, &target_field, &source_field, source_range);
      if (mass_has_error(context)) return;
    }
    return;
  }

  if (source->descriptor->tag == Descriptor_Tag_Struct) {
    if (!same_type_or_can_implicitly_move_cast(target->descriptor, source->descriptor)) goto err;

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
      mass_assign_helper(context, builder, &target_field, &source_field, source_range);
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
    if (mass_value_is_compile_time_known(target)) {
      assert(mass_value_is_compile_time_known(source));
      assert(same_type(target->descriptor, source->descriptor));
      void *source_memory = (void *)storage_static_memory_with_bit_size(
        &value_as_forced(source)->storage, source->descriptor->bit_size
      );
      void *target_memory = (void *)storage_static_memory_with_bit_size(
        &value_as_forced(target)->storage, target->descriptor->bit_size
      );
      memcpy(target_memory, source_memory, descriptor_byte_size(target->descriptor));
    } else {
      move_value(builder, source_range, target_storage, source_storage);
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
  // TODO figure out how to get rid of this cast
  Lazy_Static_Value *lazy = (Lazy_Static_Value *)value_as_lazy_static_value(value);
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
    hash_map_set(scope->map, symbol, allocated);
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

static inline Value *
token_make_symbol_value(
  Mass_Context *context,
  Slice name,
  Source_Range source_range
) {
  const Symbol *symbol = mass_ensure_symbol(context->compilation, name);

  return value_make(context, &descriptor_symbol, storage_static(symbol), source_range);
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
  Value *value;
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
  assert(offset);

  Source_Range children_range = {
    .file = parent_value->source_range.file,
    .offsets = {.from = u64_to_u32(offset - 1), .to = u64_to_u32(offset - 1)},
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
  if(parent->value->descriptor != &descriptor_ast_block) return false;
  bool has_children = parent->index + 1 != dyn_array_length(*stack);
  // Do not treat leading newlines as semicolons
  if (!has_children) return true;

  Ast_Block *group = (Ast_Block *)value_as_ast_block(parent->value);

  assert(offset);
  Value_View statement = tokenizer_make_group_children_view(
    context->allocator, stack, parent, parent->value, offset
  );
  dyn_array_push(group->statements, statement);
  return true;
}

static inline void
tokenizer_group_push(
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Value *value
) {
  dyn_array_push(*parent_stack, (Tokenizer_Parent){
    .value = value,
    .index = dyn_array_length(*stack)
  });
  dyn_array_push(*stack, value);
}

static inline void
tokenizer_group_start_curly(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Source_Range source_range
) {
  Ast_Block *group = mass_allocate(context, Ast_Block);
  // TODO use temp allocator first?
  *group = (Ast_Block){.statements = dyn_array_make(Array_Value_View, .allocator = context->allocator)};
  Value *value = value_make(context, &descriptor_ast_block, storage_immediate(group), source_range);
  tokenizer_group_push(stack, parent_stack, value);
}

static inline void
tokenizer_group_start_paren(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Source_Range source_range
) {
  Group_Paren *group = mass_allocate(context, Group_Paren);
  Value *value = value_make(context, &descriptor_group_paren, storage_static(group), source_range);
  tokenizer_group_push(stack, parent_stack, value);
}

static inline void
tokenizer_group_start_square(
  Mass_Context *context,
  Array_Value_Ptr *stack,
  Array_Tokenizer_Parent *parent_stack,
  Source_Range source_range
) {
  Group_Square *group = mass_allocate(context, Group_Square);
  Value *value = value_make(context, &descriptor_group_square, storage_static(group), source_range);
  tokenizer_group_push(stack, parent_stack, value);
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
  assert(parent_value->tag == Value_Tag_Forced);
  parent_value->Forced.storage = storage_static(group);

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
  assert(parent_value->tag == Value_Tag_Forced);
  parent_value->Forced.storage = storage_static(group);

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
    Mass_Byte_Slice byte_slice;
    Value string_value;
  });

  hash_map_set(context->compilation->static_pointer_length_map, bytes, length);

  Mass_Byte_Slice *byte_slice = &combined->byte_slice;
  *byte_slice = (Mass_Byte_Slice){bytes, length};
  Value *byte_slice_value = value_init(
    &combined->string_value,
    &descriptor_byte_slice, storage_static(byte_slice), source_range
  );
  dyn_array_push(*stack, byte_slice_value);
}

#include "tokenizer.c"
//#include "generated_tokenizer.c"

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
  if (!mass_value_ensure_static_of(context, value, &descriptor_descriptor_pointer)) {
    return 0;
  }
  // Can't use `value_as_descriptor_pointer` because it might a user-generated version
  // of the type that does not pointer compare unless we memoize
  return *(Descriptor const **)storage_static_memory_with_bit_size(
    &value_as_forced(value)->storage, value->descriptor->bit_size
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
  Value_View remaining = view;

  Value *result_value = 0;

  Value *list = token_parse_expression(context, parser, remaining, &(u32){0}, 0);
  if (mass_has_error(context)) return 0;
  Array_Value_Ptr items = mass_parse_maybe_list_into_value_array(context, parser, list);
  if (mass_has_error(context)) return 0;

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
  Value *symbol_value = value_view_get(&args, 0);
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
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
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
  Value_View args
) {
  assert(args.length == 1);
  return token_parse_single(context, parser, value_view_get(&args, 0));
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
    const Function_Literal *literal = value_as_function_literal(value);
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
  const Expected_Result *expected_result,
  Value *value
) {
  if (mass_has_error(context)) return 0;
  switch(expected_result->tag) {
    case Expected_Result_Tag_Exact: {
      Value *result_value = value_make(
        context, expected_result->Exact.descriptor, expected_result->Exact.storage, value->source_range
      );
      mass_assign_helper(context, builder, result_value, value, &value->source_range);
      if (mass_has_error(context)) return 0;
      const Storage *actual_storage = &value_as_forced(value)->storage;
      // @Hack there should be a better and more robust way to do this
      if (
        !mass_value_is_compile_time_known(value) &&
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
  const Storage *target_storage = &value_as_forced(target)->storage;
  Expected_Result expected_result = mass_expected_result_exact(target->descriptor, *target_storage);
  Value *forced = value_force(context, builder, &expected_result, source);
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

static bool
token_parse_operator_definition(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Value_Lazy *out_lazy_value
) {
  u32 peek_index = 0;
  Value *keyword_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols.operator
  );
  if (!keyword_token) return 0;

  Value *pattern_token = value_view_next(&view, &peek_index);
  if (!value_is_group_paren(pattern_token)) { context_parse_error(context, parser, view, peek_index); goto err; }

  Value *separator_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, mass_ensure_symbol(context->compilation, slice_literal("::"))
  );
  if (!separator_token) { context_parse_error(context, parser, view, peek_index); goto err; }

  Value *precedence_or_tuple_token = value_view_next(&view, &peek_index);
  if (!precedence_or_tuple_token) { context_parse_error(context, parser, view, peek_index); goto err; }

  Value *precedence_or_tuple_value = token_parse_single(context, parser, precedence_or_tuple_token);
  if (mass_has_error(context)) goto err;

  Value *precedence_value;
  Value *body;
  if (value_is_tuple(precedence_or_tuple_value)) {
    const Tuple *tuple = value_as_tuple(precedence_or_tuple_value);
    if (dyn_array_length(tuple->items) != 2) {
      panic("TODO better matching here");
      return false;
    }
    precedence_value = *dyn_array_get(tuple->items, 0);
    body = *dyn_array_get(tuple->items, 1);
  } else {
    precedence_value = precedence_or_tuple_value;
    Value_View rest = value_view_rest(&view, peek_index);
    u32 body_length;
    body = token_parse_expression(context, parser, rest, &body_length, 0);
    if (mass_has_error(context)) goto err;

    if (!body_length) { context_parse_error(context, parser, view, peek_index); goto err; }
    peek_index += body_length;
  }

  if (!mass_value_ensure_static_of(context, precedence_value, &descriptor_i64)) goto err;
  u32 precendence = u64_to_u32(value_as_i64(precedence_value)->bits);

  Value_View definition = value_as_group_paren(pattern_token)->children;

  Value *operator_token;
  Operator_Fixity fixity = Operator_Fixity_Prefix;
  u16 argument_count = 1;

  // prefix and postfix
  if (definition.length == 2) {
    Value *first =  value_view_get(&definition, 0);
    fixity = Operator_Fixity_Prefix;
    if (value_match_symbol(first, slice_literal("_"))) {
      fixity = Operator_Fixity_Postfix;
    }
    if (fixity == Operator_Fixity_Prefix) {
      if (!value_match_symbol(value_view_get(&definition, 1), slice_literal("_"))) {
        context_parse_error(context, parser, view, peek_index);
        goto err;
      }
      operator_token = value_view_get(&definition, 0);
    } else {
      operator_token = value_view_get(&definition, 1);
    }
  } else if (definition.length == 1) { // infix
    argument_count = 2;
    fixity = Operator_Fixity_Infix;
    operator_token = value_view_get(&definition, 0);
  } else {
    operator_token = 0;
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = pattern_token->source_range,
      .detailed_message ="Expected the pattern to have 2 tokens for prefix / postfix or 1 for infix operators"
    });
    goto err;
  }

  Operator *operator = mass_allocate(context, Operator);

  if (value_is_symbol(body)) {
    const Symbol *alias = value_as_symbol(body);
    *operator = (Operator){
      .tag = Operator_Tag_Alias,
      .fixity = fixity,
      .precedence = precendence,
      .Alias = { .symbol = alias },
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
    value_as_symbol(operator_token),
    operator
  );

  err:
  return true;
}

static Slice
mass_slice_from_slice_like(
  Value *value
) {
  if (same_type(value->descriptor, &descriptor_slice)) {
    return *value_as_slice(value);
  } else if (same_type(value->descriptor, &descriptor_byte_slice)) {
    const Mass_Byte_Slice *byte_slice = value_as_byte_slice(value);
    return (Slice){.bytes = byte_slice->bytes, .length = byte_slice->length};
  } else {
    panic("TODO accept any right-shaped struct, not just generic view");
    return (Slice){0};
  }
}

static Value *
mass_import(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  if (args.length != 1) goto parse_err;
  Value *file_path_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (!same_type_or_can_implicitly_move_cast(&descriptor_slice, file_path_value->descriptor)) {
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
      Scope *module_scope = scope_make(context->allocator, root_scope);
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
  const Mass_While *payload
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
  storage_release_if_temporary(builder, &value_as_forced(condition)->storage);

  Value *void_value = mass_make_void(context, *source_range);
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

static bool
token_parse_while(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Value_Lazy *out_lazy_value
) {
  u32 peek_index = 0;
  Value *keyword_token = value_view_maybe_match_cached_symbol(
    view, &peek_index, context->compilation->common_symbols._while
  );
  if (!keyword_token) return 0;

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
    return false;
  }

  Value *body_token = value_view_next(&view, &peek_index);
  assert(value_is_ast_block(body_token));
  if (view.length != peek_index) {
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = value_view_rest(&view, peek_index).source_range,
    });
    return false;
  }

  Mass_While *lazy_payload = mass_allocate(context, Mass_While);
  *lazy_payload = (Mass_While) {
    .condition = token_parse_expression(context, parser, condition_view, &(u32){0}, 0),
    .body = token_parse_single(context, parser, body_token),
  };

  out_lazy_value->proc = mass_handle_while_lazy_proc;
  out_lazy_value->payload = lazy_payload;

  return true;
}

static Value *
mass_c_struct(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);

  Value *tuple_value = token_parse_single(context, parser, value_view_get(&args, 0));
  if (!mass_value_ensure_static_of(context, tuple_value, &descriptor_tuple)) return 0;
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

// TODO just inline this
static inline const Descriptor *
value_or_lazy_value_descriptor(
  const Value *value
) {
  return value->descriptor;
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
    &eval_context, &eval_builder, &expected_result, expression_result_value
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

  mass_assign_helper(&eval_context, &eval_builder, &out_value_register, &result_address, source_range);
  if (mass_has_error(context)) return 0;

  mass_assign_helper(&eval_context, &eval_builder, out_value, forced_value, source_range);
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
mass_handle_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
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
      panic("TODO user error");
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
    context, builder, expected_result, result_value
  );
}

static Value *
mass_handle_tuple_cast_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *expected_descriptor = mass_expected_result_descriptor(expected_result);
  assert(expected_descriptor == payload->target);
  assert(payload->target->tag == Descriptor_Tag_Struct);
  Value *result = mass_value_from_expected_result(context, builder, expected_result, *source_range);
  mass_assign_helper(context, builder, result, payload->expression, source_range);
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
mass_zero_extend_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Cast_Lazy_Payload *payload
) {
  const Descriptor *target_descriptor = payload->target;
  Value *expression = payload->expression;
  const Descriptor *source_descriptor = value_or_lazy_value_descriptor(expression);

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
    move_value(builder, source_range, result_storage, &zero);
  }
  Storage adjusted_storage = *result_storage;
  adjusted_storage.bit_size = original_bit_size;
  Expected_Result adjusted_expected_result =
    mass_expected_result_exact(source_descriptor, adjusted_storage);
  (void)value_force(context, builder, &adjusted_expected_result, expression);

  return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, result);
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
  Value *expression = value_view_get(&args_view, 1);

  Mass_Cast_Lazy_Payload lazy_payload = {
    .target = target_descriptor,
    .expression = expression,
  };

  if (mass_value_is_compile_time_known(expression)) {
    Expected_Result expected_result = expected_result_any(target_descriptor);
    return mass_zero_extend_lazy_proc(context, 0, &expected_result, &args_view.source_range, &lazy_payload);
  }

  Mass_Cast_Lazy_Payload *heap_payload = mass_allocate(context, Mass_Cast_Lazy_Payload);
  *heap_payload = lazy_payload;

  return mass_make_lazy_value(
    context, parser, args_view.source_range, heap_payload, target_descriptor, mass_zero_extend_lazy_proc
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

static bool
token_parse_constant_definitions(
  Mass_Context *context,
  Parser *parser,
  Value_View view,
  Value_Lazy *out_lazy_value
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

static Value *
mass_macro_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Value_Lazy_Payload *payload
) {
  Value *result_value = mass_value_from_expected_result(context, builder, expected_result, *source_range);

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
    value_force_exact(context, builder, result_value, payload->value);

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
  const Mass_Value_Lazy_Payload *payload
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
  value_force_exact(context, builder, forced, payload->value);
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
  assert(literal->header.flags & Function_Header_Flags_Macro);

  // We make a nested scope based on function's original scope
  // instead of current scope for hygiene reasons. I.e. function body
  // should not have access to locals inside the call scope.
  Scope *body_scope = scope_make(context->allocator, literal->own_scope);

  for(u64 i = 0; i < dyn_array_length(literal->header.parameters); ++i) {
    if (mass_has_error(context)) goto err;
    Function_Parameter *param = dyn_array_get(literal->header.parameters, i);
    if (param->symbol) {
      Value *arg_value;
      if (i >= args_view.length) {
        arg_value = param->maybe_default_value;
      } else {
        arg_value = value_view_get(&args_view, i);
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
        Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
        *payload = (Mass_Value_Lazy_Payload) {.value = arg_value};
        param_value = mass_make_lazy_value(
          context, parser, param->source_range,
          payload, value_or_lazy_value_descriptor(arg_value),
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
  switch(literal->header.returns.tag) {
    case Function_Return_Tag_Inferred:
    case Function_Return_Tag_Generic: {
      // Accept whatever the actual return type is
    } break;
    case Function_Return_Tag_Exact: {
      const Descriptor *expected = literal->header.returns.Exact.descriptor;
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
  }

  if (mass_value_is_compile_time_known(body_value)) {
    return body_value;
  }

  Source_Range return_range = literal->header.returns.source_range;
  Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
  *payload = (Mass_Value_Lazy_Payload) {.value = body_value};
  return mass_make_lazy_value(
    context, parser, return_range, payload, actual_return_descriptor, mass_macro_lazy_proc
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
  Value_View args;
  Value *overload;
} Mass_Function_Call_Lazy_Payload;

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
call_function_overload(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Function_Call_Lazy_Payload *payload
) {
  Value_View args_view = payload->args;

  Expected_Result instance_expected_result = expected_result_any(0);
  Value *runtime_value = value_force(context, builder, &instance_expected_result, payload->overload);
  if (mass_has_error(context)) return 0;

  runtime_value = ensure_function_instance(context, runtime_value, args_view);
  if (mass_has_error(context)) return 0;

  const Storage *runtime_storage = &value_as_forced(runtime_value)->storage;
  mass_assert_storage_is_valid_in_context(runtime_storage, context);

  const Descriptor *runtime_descriptor = value_or_lazy_value_descriptor(runtime_value);
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
  if (call_setup->jump == Function_Call_Jump_Call) {
    if (runtime_storage->tag == Storage_Tag_Register) {
      call_target_storage = reserve_stack_storage(builder, (Bits){64});
      call_target_storage.flags |= Storage_Flags_Temporary;
      assert(runtime_storage->bit_size.as_u64 == 64);
      move_value(builder, source_range, &call_target_storage, runtime_storage);
      storage_release_if_temporary(builder, runtime_storage);
      runtime_value = 0;
    }
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
    value_init(target_param, call_param->descriptor, call_param->storage, *source_range);
    if (i >= args_view.length) {
      assert(call_param->flags & Function_Call_Parameter_Flags_Uninitialized);
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
    if (descriptor_is_implicit_pointer(stack_descriptor)) {
      stack_descriptor = stack_descriptor->Pointer_To.descriptor;
    }

    u64 target_param_register_bitset = register_bitset_from_storage(&call_param->storage);
    if (target_param_register_bitset >> 16) {
      panic("Found XMM usage");
    }

    bool can_use_source_arg_as_is;
    // If source args are on the stack or rip-relative we don't need to worry about their registers
    // but it is not true for all Storage_Tag_Memory, because indirect access uses registers
    if (
      maybe_source_storage &&
      (storage_is_stack(maybe_source_storage) || storage_is_label(maybe_source_storage))) {
      can_use_source_arg_as_is = true;
    } else if (
      // Compile time-known args also don't need registers so they can be used as-is
      mass_value_is_compile_time_known(source_arg) &&
      // TODO figure out why this is required and explain but since it just disallows
      //      some potential optizations it is not critical to do right now.
      !descriptor_is_implicit_pointer(call_param->descriptor)
    ) {
      can_use_source_arg_as_is = true;
    } else {
      can_use_source_arg_as_is = false;
    }

    bool target_param_registers_are_free =
      !(builder->register_occupied_bitset.bits & target_param_register_bitset);
    bool can_assign_straight_to_target = (
      target_param_registers_are_free &&
      !descriptor_is_implicit_pointer(call_param->descriptor)
    );

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
        !descriptor_is_implicit_pointer(call_param->descriptor) &&
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
    };
    if (should_assign) {
      mass_assign_helper(context, builder, arg_value, source_arg, source_range);
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
        &builder->code_block, *source_range,
        &(Instruction_Assembly){mov, {saved->stack, saved->reg}}
      );
    }
  }

  register_release_bitset(builder, saved_registers_from_arguments_bitset);
  u64 spilled_param_register_bitset = all_used_arguments_register_bitset & ~copied_straight_to_param_bitset;
  register_acquire_bitset(builder, spilled_param_register_bitset);

  for (u64 i = 0; i < dyn_array_length(target_params); ++i) {
    Value *param = dyn_array_get(target_params, i);

    const Storage *param_storage = &value_as_forced(param)->storage;
    if (storage_is_stack(param_storage)) continue;

    Value *source_arg = *dyn_array_get(temp_arguments, i);
    // :IndirectReturnArgument
    if (storage_is_indirect(param_storage)) {
      Register base_register = param_storage->Memory.location.Indirect.base_register;
      Storage target_storage = storage_register(base_register, (Bits){64});
      Storage source_storage = storage_adjusted_for_lea(value_as_forced(source_arg)->storage);

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){lea, {target_storage, source_storage}}
      );
    } else {
      mass_assign_helper(context, builder, param, source_arg, source_range);
      if (mass_has_error(context)) return 0;
    }
  }

  builder->max_call_parameters_stack_size = u32_max(
    builder->max_call_parameters_stack_size,
    call_setup->parameters_stack_size
  );

  switch(call_setup->jump) {
    case Function_Call_Jump_Call: {
      if (storage_is_label(&call_target_storage)) {
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){call, {call_target_storage}}
        );
      } else {
        Register temp_reg = register_acquire_temp(builder);
        Storage reg = storage_register(temp_reg, (Bits){64});
        move_value(builder, source_range, &reg, &call_target_storage);
        push_eagerly_encoded_assembly(
          &builder->code_block, *source_range,
          &(Instruction_Assembly){call, {reg}}
        );
        register_release(builder, temp_reg);
      }
    } break;
    case Function_Call_Jump_Syscall: {
      Storage syscal_number_storage = storage_register(Register_A, (Bits){64});
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){mov, {syscal_number_storage, call_target_storage}}
      );
      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){asm_syscall}
      );
    } break;
  }
  storage_release_if_temporary(builder, &call_target_storage);

  register_release_bitset(builder, all_used_arguments_register_bitset | temp_register_argument_bitset);

  u64 return_value_bitset = register_bitset_from_storage(&return_storage);

  assert(!(return_value_bitset & saved_registers_from_arguments_bitset));
  DYN_ARRAY_FOREACH(Saved_Register, saved, stack_saved_registers) {
    push_eagerly_encoded_assembly(
      &builder->code_block, *source_range,
      &(Instruction_Assembly){mov, {saved->reg, saved->stack}}
    );
  }

  register_acquire_bitset(builder, saved_registers_from_arguments_bitset);
  register_acquire_bitset(builder, (return_value_bitset & ~expected_result_bitset));

  Value *fn_return_value = value_make(context, fn_info->return_descriptor, return_storage, *source_range);

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
mass_function_info_init_for_header_and_maybe_body(
  Mass_Context *context,
  const Scope *arguments_scope,
  const Function_Header *header,
  Value *maybe_body,
  Function_Info *out_info
) {
  Mass_Context temp_context = mass_context_from_compilation(context->compilation);
  Parser args_parser = {
    .flags = Parser_Flags_None,
    .scope = scope_make(temp_context.temp_allocator, arguments_scope),
  };

  *out_info = (Function_Info) {
    .flags = Function_Info_Flags_None,
    .parameters = dyn_array_make(
      Array_Function_Parameter,
      .allocator = context->allocator,
      .capacity = dyn_array_length(header->parameters),
    ),
  };

  Temp_Mark temp_mark = context_temp_mark(&temp_context);

  DYN_ARRAY_FOREACH(Function_Parameter, param, header->parameters) {
    Source_Range source_range = param->source_range;
    if (param->descriptor || param->maybe_default_value) {
      const Descriptor *descriptor = param->descriptor;
      if (!descriptor) {
        descriptor = deduce_runtime_descriptor_for_value(context, param->maybe_default_value, 0);
      }
      Storage storage = storage_immediate(&descriptor);
      Value *param_value = value_make(&temp_context, &descriptor_descriptor_pointer, storage, source_range);
      scope_define_value(args_parser.scope, VALUE_STATIC_EPOCH, source_range, param->symbol, param_value);
    } else {
      scope_define_lazy_compile_time_expression(
        &temp_context, &args_parser, args_parser.scope, param->symbol, param->maybe_type_expression
      );
    }
  }

  DYN_ARRAY_FOREACH(Function_Parameter, param, header->parameters) {
    Function_Parameter *info_param = dyn_array_push(out_info->parameters, *param);
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
        info_param->descriptor = descriptor;
      } break;
      case Function_Parameter_Tag_Exact_Static: {
        // Nothing to do
      } break;
    }
  }

  switch(header->returns.tag) {
    case Function_Return_Tag_Inferred: {
      if (header->flags & Function_Header_Flags_Intrinsic) {
        // Handled in :IntrinsicReturnType
        out_info->return_descriptor = 0;
      } else {
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
      source_arg = value_view_get(&args_view, arg_index);
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
          source_arg->descriptor, &value_as_forced(source_arg)->storage
        )) return -1;
        score += Score_Exact_Static;
      } break;
      case Function_Parameter_Tag_Generic: {
        if (param->Generic.maybe_type_constraint) {
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
  struct Overload_Match_State *match,
  struct Overload_Match_State *best_conflict_match
) {
  if (mass_has_error(context)) return;
  if (value_is_overload(candidate)) {
    const Overload *overload = value_as_overload(candidate);
    mass_match_overload_candidate(context, overload->value, args, match, best_conflict_match);
    mass_match_overload_candidate(context, overload->next, args, match, best_conflict_match);
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
      if (!match_overload_argument_count(literal->header.parameters, args->view.length)) return;
      Function_Info *specialized_info;
      {
        // :OverloadLock
        // A literal must be locked here to allow using its overload to be used
        // inside the function signature. Here's an example:
        //    pointer_to :: fn(type : Type) => (Type) MASS.pointer_to_type
        //    pointer_to :: fn(x) -> (pointer_to(x)) MASS.pointer_to
        // Without the lock the second literal will infinitely recurse
        *literal->overload_lock_count += 1;
        specialized_info = function_literal_info_for_args(context, literal, args->view);
        *literal->overload_lock_count -= 1;
      }
      overload_info = specialized_info;
      if (!overload_info) return;
    } else {
      const Descriptor *descriptor = value_or_lazy_value_descriptor(candidate);
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
    if (!mass_value_is_compile_time_known(value_view_get(&args_view, i))) {
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
  const Descriptor *expected_descriptor,
  Value_View args_view
) {
  // TODO only do this check in debug but that requires `descriptor_mass_intrinsic_proc`
  //      to have proper symbols in the parameter definitions.
  const Descriptor *intrinsic_descriptor =
    deduce_runtime_descriptor_for_value(context, overload, &descriptor_mass_intrinsic_proc);
  assert(!mass_has_error(context));
  assert(intrinsic_descriptor);
  const Function_Info *intrinsic_info = descriptor_as_function_instance(intrinsic_descriptor)->info;
  Mass_Intrinsic_Proc jitted_code =
    (Mass_Intrinsic_Proc)mass_ensure_jit_function_for_value(context, overload, intrinsic_info);
  if (mass_has_error(context)) return 0;

  Value *result = jitted_code(context, parser, args_view);
  if (!result) return 0;

  // :IntrinsicReturnType
  if (expected_descriptor) {
    const Descriptor *actual = value_or_lazy_value_descriptor(result);
    if (!same_type(expected_descriptor, actual)) {
      const Descriptor *runtime = deduce_runtime_descriptor_for_value(context, result, expected_descriptor);
      if (!runtime) return 0;
      if (!same_type(expected_descriptor, runtime)) {
        mass_error(context, (Mass_Error) {
          .tag = Mass_Error_Tag_Type_Mismatch,
          .source_range = args_view.source_range,
          .Type_Mismatch = { .expected = expected_descriptor, .actual = actual },
        });
        return 0;
      }
    }
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
    const Storage *runtime_storage = &value_as_forced(runtime_instance)->storage;
    proxy_value = value_make(context, proxy_descriptor, *runtime_storage, runtime_instance->source_range);
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
  Scope *trampoline_scope = scope_make(context->allocator, context->compilation->root_scope);

  Source_Range return_range;
  INIT_LITERAL_SOURCE_RANGE(&return_range, "()");

  context = &jit_context;
  Program *program = jit_context.program;
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
    Storage storage = storage_with_offset_and_bit_size(
      &args_struct_indirect_storage, u64_to_s32(field->offset), field->descriptor->bit_size
    );
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

  mass_assign_helper(context, builder, indirect_return, call_result, &return_range);
  if (mass_has_error(context)) return 0;

  push_instruction(&builder->code_block, (Instruction) {
    .tag = Instruction_Tag_Label,
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
      &jit_context, proxy_instance, proxy_info
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
    Value *item = value_view_get(&args_view, i);
    assert(mass_value_is_compile_time_known(item));
    const Struct_Field *field = dyn_array_get(fields, i);
    u64 offset = field->offset;
    void *arg_memory = args_struct_memory + offset;
    const void *source_memory = storage_static_memory_with_bit_size(
      &value_as_forced(item)->storage, item->descriptor->bit_size
    );
    memcpy(arg_memory, source_memory, descriptor_byte_size(item->descriptor));
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
  Overload_Match_Found match_found;
  if (!mass_match_overload_or_error(context, target_expression, args_view, &match_found)) {
    return 0;
  }

  Value *overload = match_found.value;
  const Function_Info *info = match_found.info;

  // This normalization is required for a few reasons:
  //   1. `args_view` might point to temp memory
  //   2. Default args are substituted. Required for trampolines and intrinsics to work.
  //   3. Implicits casts for static args are done at compile time. Required for trampolines
  //      and intrinsics to work. Also provides a nice optimization for runtime.
  u64 normalized_arg_count = dyn_array_length(info->parameters);
  Value_View normalized_args = {
    .values = 0,
    .length = u64_to_u32(normalized_arg_count),
    .source_range = args_view.source_range,
  };
  if (normalized_arg_count) {
    assert(args_view.length <= normalized_arg_count);
    normalized_args.values =
      allocator_allocate_array(context->allocator, Value *, normalized_arg_count);
    for (u64 i = 0; i < dyn_array_length(info->parameters); ++i) {
      const Function_Parameter *param = dyn_array_get(info->parameters, i);
      Value *source = i >= args_view.length
        ? param->maybe_default_value
        : value_view_get(&args_view, i);
      assert(source);
      // TODO @Speed it should be possible to save if all args are exact match in Overload_Match_Found
      if (!same_type(param->descriptor, source->descriptor)) {
        // TODO instead of this code maybe it would be more robust (and performant?)
        //      to create compile-time casting functions. This would also allow to have
        //      user-defined casts. The only tricky part is that these casting functions
        //      must match arguments exactly (no implicit casts).
        const Descriptor *runtime_source_descriptor =
          deduce_runtime_descriptor_for_value(context, source, param->descriptor);
        if (!same_type_or_can_implicitly_move_cast(param->descriptor, runtime_source_descriptor)) {
          panic("We should not have matched an overload if the value is not assignable");
        }
        bool can_static_cast = mass_value_is_compile_time_known(source);
        if (param->descriptor->tag == Descriptor_Tag_Function_Instance) {
          // FIXME for runtime calls a function instance might be a relocation, so we can't hard-code
          //       a static value.
          // FIXME And for trampolines intrinsics this is completeley broken because we need to
          //       force a jit function instance.
          can_static_cast = false;
        } else if (value_is_tuple(source)) {
          const Tuple *tuple = value_as_tuple(source);
          for (u64 tuple_index = 0; tuple_index < dyn_array_length(tuple->items); ++tuple_index) {
            Value *tuple_item = *dyn_array_get(tuple->items, tuple_index);
            if (!mass_value_is_compile_time_known(tuple_item)) {
              can_static_cast = false;
              break;
            }
          }
        }
        if (can_static_cast) {
          void *memory = mass_allocate_bytes_from_descriptor(context, param->descriptor);
          Storage storage = storage_static_heap(memory, param->descriptor->bit_size);
          Value *adjusted_source = value_make(context, param->descriptor, storage, source->source_range);
          mass_assign_helper(context, 0/*no builder */, adjusted_source, source, &args_view.source_range);
          if (mass_has_error(context)) return 0;
          source = adjusted_source;
        }
      }
      normalized_args.values[i] = source;
    }
  }

  if (value_is_function_literal(overload)) {
    const Function_Literal *literal = value_as_function_literal(overload);
    if (value_is_intrinsic(literal->body)) {
      return mass_intrinsic_call(context, parser, literal->body, info->return_descriptor, normalized_args);
    }
    if (literal->header.flags & Function_Header_Flags_Macro) {
      return mass_handle_macro_call(context, parser, overload, normalized_args, source_range);
    }
  }

  if (info->flags & Function_Info_Flags_Compile_Time) {
    for (u64 i = 0; i < normalized_args.length; ++i) {
      Value *arg = value_view_get(&normalized_args, i);
      const Function_Parameter *param = dyn_array_get(info->parameters, i);
      assert(mass_value_is_compile_time_known(arg));
      assert(same_type(param->descriptor, arg->descriptor));
    }
    Value *result = mass_trampoline_call(context, parser, overload, info, normalized_args);
    if (mass_has_error(context)) return 0;
    return result;
  }

  Mass_Function_Call_Lazy_Payload *call_payload =
    allocator_allocate(context->allocator, Mass_Function_Call_Lazy_Payload);
  *call_payload = (Mass_Function_Call_Lazy_Payload){
    .overload = overload,
    .args = normalized_args,
  };

  const Descriptor *lazy_descriptor = info->return_descriptor;
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
      Value *temp_lhs = value_force(context, builder, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_rhs_storage = storage_register_temp(builder, descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_rhs_storage);
      (void)value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      const X64_Mnemonic *mnemonic = payload->operator == Mass_Arithmetic_Operator_Add ? add : sub;

      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_lhs_storage, temp_rhs_storage}}
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
      (void)value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_instruction(&builder->code_block, (Instruction) {
        .tag = Instruction_Tag_Location,
        .Location = { .source_range = result_range },
      });

      const X64_Mnemonic *mnemonic = descriptor_is_signed_integer(descriptor) ? imul : mul;
      push_eagerly_encoded_assembly(
        &builder->code_block, result_range,
        &(Instruction_Assembly){mnemonic, {temp_b_storage}}
      );
      if (maybe_saved_rdx.tag == Storage_Tag_Register) {
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

      Storage temp_dividend_storage = storage_register(Register_A, descriptor->bit_size);
      register_acquire(builder, Register_A);
      temp_dividend_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_dividend = mass_expected_result_exact(descriptor, temp_dividend_storage);
      Value *temp_dividend = value_force(context, builder, &expected_dividend, payload->lhs);

      Register temp_divisor_register = register_find_available(builder, disallowed_temp_registers);
      register_acquire(builder, temp_divisor_register);
      Storage temp_divisor_storage = storage_register(temp_divisor_register, descriptor->bit_size);
      temp_divisor_storage.flags |= Storage_Flags_Temporary;
      Expected_Result expected_divisor = mass_expected_result_exact(descriptor, temp_divisor_storage);
      (void)value_force(context, builder, &expected_divisor, payload->rhs);

      // Save RDX as it will be used for the remainder
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
          &builder->code_block, result_range,
          &(Instruction_Assembly){idiv, {temp_divisor_storage}}
        );
      } else {
        if (bit_size == 8) {
          Storage reg_ax = storage_register(Register_A, (Bits){16});
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, result_range,
            &(Instruction_Assembly){movzx, {reg_ax, temp_dividend_storage}}
          );
        } else {
          // We need to zero-extend A to D which means just clearing D register
          push_eagerly_encoded_assembly_no_source_range(
            &builder->code_block, result_range, &(Instruction_Assembly){xor, {reg_d, reg_d}}
          );
        }
        push_eagerly_encoded_assembly_no_source_range(
          &builder->code_block, result_range, &(Instruction_Assembly){asm_div, {temp_divisor_storage}}
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
          move_value(builder, &result_range, &temp_dividend_storage, &reg_d);
        }
      }

      storage_release_if_temporary(builder, &temp_divisor_storage);
      if (maybe_saved_rdx.tag == Storage_Tag_Register) {
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
  Value *lhs = value_view_get(&arguments, 0);
  Value *rhs = value_view_get(&arguments, 1);

  if (mass_has_error(context)) return 0;

  const Descriptor *result_descriptor = value_or_lazy_value_descriptor(lhs);
  // FIXME this is required for default params with a type specified
  //       i.e. fn(x : s64 = 20) and should be fixed there
  if (!descriptor_is_integer(value_or_lazy_value_descriptor(rhs))) {
    rhs = mass_cast_helper(context, parser, result_descriptor, rhs, rhs->source_range);
  } else if (!descriptor_is_integer(value_or_lazy_value_descriptor(lhs))) {
    result_descriptor = value_or_lazy_value_descriptor(rhs);
    lhs = mass_cast_helper(context, parser, result_descriptor, lhs, lhs->source_range);
  }
  assert(descriptor_is_integer(result_descriptor));
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
  const Mass_Comparison_Operator_Lazy_Payload *payload
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
  (void)value_force(context, builder, &expected_a, payload->lhs);

  // TODO This can be optimized in cases where one of the operands is an immediate
  Storage temp_b_storage = storage_register_temp(builder, descriptor->bit_size);
  Expected_Result expected_b = mass_expected_result_exact(descriptor, temp_b_storage);
  (void)value_force(context, builder, &expected_b, payload->rhs);

  if (mass_has_error(context)) return 0;

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly){cmp, {temp_a_storage, temp_b_storage}}
  );

  Value *comparison_value = value_make(
    context, &descriptor__bool, storage_eflags(compare_type), *source_range
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
  const Mass_Comparison_Operator_Lazy_Payload *payload
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
    const Storage *lhs_storage = &value_as_forced(lhs)->storage;
    const Storage *rhs_storage = &value_as_forced(rhs)->storage;
    bool equal = storage_static_equal(lhs->descriptor, lhs_storage, rhs->descriptor, rhs_storage);
    if (negated) equal = !equal;
    return value_make(context, &descriptor__bool, storage_immediate(&equal), *source_range);
  }

  Value *result = 0;
  switch(lhs_descriptor->tag) {
    // Two void values are always equal to each other
    case Descriptor_Tag_Void: {
      bool equal = true;
      result = value_make(context, &descriptor__bool, storage_immediate(&equal), *source_range);
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
      (void)value_force(context, builder, &expected_a, payload->lhs);

      // TODO This can be optimized in cases where one of the operands is an immediate
      Storage temp_b_storage = storage_register_temp(builder, rhs_descriptor->bit_size);
      Expected_Result expected_b = mass_expected_result_exact(rhs_descriptor, temp_b_storage);
      (void)value_force(context, builder, &expected_b, payload->rhs);

      if (mass_has_error(context)) return 0;

      push_eagerly_encoded_assembly(
        &builder->code_block, *source_range,
        &(Instruction_Assembly){cmp, {temp_a_storage, temp_b_storage}}
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

static const Descriptor *
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

  const Descriptor *user_presentable_descriptor = value_or_lazy_value_descriptor(value);
  if (descriptor_is_implicit_pointer(user_presentable_descriptor)) {
    user_presentable_descriptor = user_presentable_descriptor->Pointer_To.descriptor;
  }
  return user_presentable_descriptor;
}

static Value *
mass_type_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
  const Descriptor *descriptor = mass_type_only_token_parse_expression(context, parser, value);
  if (mass_has_error(context)) return 0;
  return value_make(context, &descriptor_descriptor_pointer, storage_immediate(&descriptor), args.source_range);
}

static Value *
mass_size_of(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *value = value_view_get(&args, 0);
  const Descriptor *descriptor = mass_type_only_token_parse_expression(context, parser, value);
  if (mass_has_error(context)) return 0;
  i64 literal = { .bits = descriptor_byte_size(descriptor) };
  return value_make(context, &descriptor_i64, storage_immediate(&literal), args.source_range);
}

static Value *
mass_pointer_to_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Value_Lazy_Payload *payload
) {
  Value *pointee = payload->value;
  const Descriptor *descriptor = value_or_lazy_value_descriptor(pointee);
  Expected_Result expected_pointee = expected_result_any(descriptor);
  Value *forced = value_force(context, builder, &expected_pointee, pointee);
  if (mass_has_error(context)) return 0;
  Value *pointer_value = mass_value_from_expected_result(context, builder, expected_result, *source_range);
  const Storage *pointer_storage = &value_as_forced(pointer_value)->storage;
  const Storage *forced_storage = &value_as_forced(forced)->storage;
  if (
    forced->descriptor->tag == Descriptor_Tag_Pointer_To &&
    forced->descriptor->Pointer_To.is_implicit
  ) {
    move_value(builder, source_range, pointer_storage, forced_storage);
  } else {
    mass_storage_load_address(builder, source_range, pointer_storage, forced_storage);
  }
  storage_release_if_temporary(builder, forced_storage);
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
  const Descriptor *pointee_descriptor = value_or_lazy_value_descriptor(pointee);
  const Descriptor *descriptor = descriptor_pointer_to(context->compilation, pointee_descriptor);
  if (mass_value_is_compile_time_known(pointee)) {
    const void *source_memory = get_static_storage_with_bit_size(
      &value_as_forced(pointee)->storage, pointee_descriptor->bit_size
    );
    Value *result = value_make(
      context, descriptor, storage_immediate(&source_memory), args.source_range
    );
    return result;
  }

  Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
  *payload = (Mass_Value_Lazy_Payload) {.value = pointee};
  return mass_make_lazy_value(
    context, parser, args.source_range, payload, descriptor, mass_pointer_to_lazy_proc
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
  const Descriptor *pointer_descriptor = descriptor_pointer_to(context->compilation, descriptor);
  Storage storage = storage_immediate(&pointer_descriptor);
  return value_make(
    context, &descriptor_descriptor_pointer, storage, args_view.source_range
  );
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
  return mass_forward_call_to_alias(context, parser, operands, context->compilation->common_symbols.apply);
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
} Mass_Variable_Definition_Lazy_Payload;

static Value *
mass_handle_variable_definition_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Variable_Definition_Lazy_Payload *payload
) {
  Storage storage = payload->descriptor->bit_size.as_u64
    ? reserve_stack_storage(builder, payload->descriptor->bit_size)
    : (Storage){.tag = Storage_Tag_Immediate, .bit_size = payload->descriptor->bit_size};
  return value_make(
    context, payload->descriptor, storage, *source_range
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
mass_assign(
  Mass_Context *context,
  Parser *parser,
  Value_View operands
) {
  Value *target = token_parse_single(context, parser, value_view_get(&operands, 0));
  Value *source = token_parse_single(context, parser, value_view_get(&operands, 1));

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
  const Source_Range *source_range,
  const Mass_Field_Access_Lazy_Payload *payload
) {
  const Struct_Field *field = payload->field;

  Expected_Result expected_struct =
    expected_result_any(value_or_lazy_value_descriptor(payload->struct_));
  Value *struct_ = value_force(context, builder, &expected_struct, payload->struct_);
  if (mass_has_error(context)) return 0;
  bool source_is_contant = struct_->flags & Value_Flags_Constant;

  if (descriptor_is_implicit_pointer(struct_->descriptor)) {
    struct_ = value_indirect_from_pointer(context, builder, struct_, source_range);
  }

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

  if (source_is_contant) {
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
  const Mass_Array_Access_Lazy_Payload *payload
) {
  Expected_Result expected_array =
    expected_result_any(value_or_lazy_value_descriptor(payload->array));
  Value *array = value_force(context, builder, &expected_array, payload->array);
  Expected_Result expected_index =
    expected_result_any(value_or_lazy_value_descriptor(payload->index));
  Value *index = value_force(context, builder, &expected_index, payload->index);

  if (mass_has_error(context)) return 0;

  Value *array_element_value;

  const Descriptor *array_descriptor = value_or_lazy_value_descriptor(array);

  const Descriptor *item_descriptor;
  Storage array_storage;
  if(array_descriptor->tag == Descriptor_Tag_Fixed_Array) {
    item_descriptor = array_descriptor->Fixed_Array.item;
    array_storage = value_as_forced(array)->storage;
  } else {
    assert(array_descriptor->tag == Descriptor_Tag_Pointer_To);
    item_descriptor = array_descriptor->Pointer_To.descriptor;
    Value *dereferenced = value_indirect_from_pointer(context, builder, array, source_range);
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
    move_value(builder, source_range, &base_storage, &value_as_forced(index)->storage);

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

  array_element_value = value_make(context, item_descriptor, element_storage, array->source_range);

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
  const Mass_Value_Lazy_Payload *payload
) {
  Value *pointer = payload->value;
  // TODO value_indirect_from_pointer should probably take an expected_result
  Value *value = value_indirect_from_pointer(context, builder, pointer, source_range);
  return mass_expected_result_ensure_value_or_temp(context, builder, expected_result, value);
}

static Value *
mass_dereference(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  Value *pointer = token_parse_single(context, parser, value_view_get(&args_view, 0));
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
  if (mass_value_is_compile_time_known(pointer)) {
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
    mass_handle_dereference_operator_lazy_proc
  );
}

static const Descriptor *
mass_constraint_pointer_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Pointer_To ? descriptor : 0;
}

static const Descriptor *
mass_constraint_integer_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Integer ? descriptor : 0;
}

static const Descriptor *
mass_constraint_float_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Float ? descriptor : 0;
}

static const Descriptor *
mass_constraint_fixed_array_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Fixed_Array ? descriptor : 0;
}

static const Descriptor *
mass_constraint_struct_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Struct ? descriptor : 0;
}

static const Descriptor *
mass_constraint_function_instance_type(
  const Descriptor *descriptor
) {
  return descriptor->tag == Descriptor_Tag_Function_Instance ? descriptor : 0;
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
  Value *lhs = value_view_get(&args_view, 0);
  Value *rhs = token_parse_single(context, parser, value_view_get(&args_view, 1));
  if (mass_has_error(context)) return 0;

  const Descriptor *lhs_descriptor = value_or_lazy_value_descriptor(lhs);
  const Descriptor *item_descriptor;
  if (lhs_descriptor->tag == Descriptor_Tag_Fixed_Array) {
    item_descriptor = lhs_descriptor->Fixed_Array.item;
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
  Value *lhs = token_parse_single(context, parser, value_view_get(&args_view, 0));
  Value *rhs = value_view_get(&args_view, 1);

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
  if (value_is_descriptor_pointer(lhs)) {
    const Descriptor *descriptor = *value_as_descriptor_pointer(lhs);
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

  const Symbol *symbol = context->compilation->common_symbols.get;
  return mass_forward_call_to_alias(context, parser, parsed_args, symbol);
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

static Value *
mass_comma(
  Mass_Context *context,
  Parser *parser,
  Value_View args_view
) {
  assert(args_view.length == 2);
  Value *lhs = value_view_get(&args_view, 0);
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
    mass_error(context, (Mass_Error) {
      .tag = Mass_Error_Tag_Parse,
      .source_range = stack_entry->source_range,
      .detailed_message = slice_literal("Not enough arguments for operator"),
    });
    return;
  }
  u64 start_index = dyn_array_length(*stack) - argument_count;
  Source_Range source_range = stack_entry->source_range;
  Value_View args_view = {
    .values = dyn_array_get(*stack, start_index),
    .length = argument_count,
    .source_range = source_range,
  };
  Value *result_value = 0;
  switch(operator->tag) {
    case Operator_Tag_Alias: {
      Value **first_arg_pointer = dyn_array_get(*stack, start_index);
      *first_arg_pointer = token_parse_single(context, parser, *first_arg_pointer);

      if (argument_count == 2) {
        Value **second_arg_pointer = dyn_array_get(*stack, start_index + 1);
        *second_arg_pointer = token_parse_single(context, parser, *second_arg_pointer);
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
  const Source_Range *source_range,
  const Mass_If_Expression_Lazy_Payload *payload
) {
  Expected_Result expected_condition = expected_result_any(&descriptor__bool);
  Value *condition = value_force(context, builder, &expected_condition, payload->condition);
  if (mass_has_error(context)) return 0;

  Program *program = context->program;
  Label *else_label =
    make_label(context->allocator, program, &program->memory.code, slice_literal("else"));

  encode_inverted_conditional_jump(builder, else_label, &condition->source_range, condition);
  storage_release_if_temporary(builder, &value_as_forced(condition)->storage);

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
  arg_parser.scope = scope_make(context->allocator, parser->scope);
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
  bool is_macro = false;
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
  } else {
    Source_Range return_range = value_view_slice(&view, peek_index, peek_index).source_range;
    returns = function_return_exact(&descriptor_void, return_range);
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
    if (is_macro) {
      mass_error(context, (Mass_Error) {
        .tag = Mass_Error_Tag_Parse,
        .source_range = rest.source_range,
        .detailed_message = slice_literal("Function-like macro must have a literal body in {}"),
      });
      return 0;
    }
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
      header.flags |= Function_Header_Flags_Generic;
      break;
    }
  }

  if (is_macro) header.flags |= Function_Header_Flags_Macro;
  if (is_compile_time) header.flags |= Function_Header_Flags_Compile_Time;

  *matched_length = peek_index;

  if (!body_value) {
    Function_Header *returned_header = mass_allocate(context, Function_Header);
    *returned_header = header;
    Storage fn_storage = storage_static(returned_header);
    return value_make(context, &descriptor_function_header, fn_storage, view.source_range);
  }

  if (!mass_value_ensure_static(context, body_value)) return 0;

  // TODO Move to userland
  if (value_is_syscall(body_value)) {
    Function_Info *fn_info = mass_allocate(context, Function_Info);
    mass_function_info_init_for_header_and_maybe_body(context, parser->scope, &header, 0, fn_info);
    if (mass_has_error(context)) return 0;
    assert(!is_compile_time);
    Function_Call_Setup call_setup =
      calling_convention_x86_64_system_v_syscall.call_setup_proc(context->allocator, fn_info);

    Descriptor *fn_descriptor = descriptor_function_instance(context->allocator, fn_info, call_setup, 0);

    i64 syscall_number = value_as_syscall(body_value)->number;
    return value_make(context, fn_descriptor, imm64(syscall_number.bits), view.source_range);
  }
  return mass_make_function_literal(context, parser, &header, body_value, view.source_range);
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
    return mass_make_void(context, view.source_range);
  }
  if(view.length == 1) {
    *out_match_length = 1;
    return token_parse_single(context, parser, value_view_get(&view, 0));
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

  bool is_value_expected = false;
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

    Operator_Fixity fixity_mask = is_value_expected
      ? Operator_Fixity_Infix | Operator_Fixity_Postfix
      : Operator_Fixity_Prefix;

    Value *value = value_view_get(&view, i);

    if (value_is_symbol(value)) {
      const Symbol *symbol = value_as_symbol(value);
      if (symbol == end_symbol) {
        matched_length = i + 1;
        goto drain;
      }

      const Operator *maybe_operator = scope_lookup_operator(context, parser->scope, symbol, fixity_mask);
      if (maybe_operator) {
        if (!token_handle_operator(
          context, parser, view, &value_stack, &operator_stack, maybe_operator, value->source_range
        )) goto defer;
        is_value_expected = (maybe_operator->fixity == Operator_Fixity_Postfix);
        continue;
      }
    }

    if (is_value_expected) {
      const Operator *empty_space_operator = &context->compilation->apply_operator;
      if (!token_handle_operator(
        context, parser, view, &value_stack, &operator_stack, empty_space_operator, value->source_range
      )) goto defer;
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
    result_value = value_force(context, builder, expected_result, lazy_statement);
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
token_parse_block_statements(
  Mass_Context *context,
  Parser *parser,
  Array_Value_View statements,
  const Source_Range *source_range
) {
  u64 max_statement_count = dyn_array_length(statements);

  if (!max_statement_count) {
    return mass_make_void(context, *source_range);
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

    // FIXME token_statement_matcher_in_scopes should instead return a Value *
    Value temp_lazy_value = {
      .tag = Value_Tag_Lazy,
      .descriptor = &descriptor_void,
      .Lazy = {
        .epoch = parser->epoch,
      },
    };

    bool matched = token_statement_matcher_in_scopes(context, parser, *statement, &temp_lazy_value.Lazy, parser->scope);
    if (mass_has_error(context)) goto defer;
    if (matched) {
      // If the statement did not assign a proc that means that it does not need
      // to output any instructions and there is nothing to force.
      if (temp_lazy_value.Lazy.proc) {
        assert(temp_lazy_value.descriptor);
        Value *lazy_value = mass_allocate(context, Value);
        *lazy_value = temp_lazy_value;
        lazy_value->source_range = statement->source_range;
        dyn_array_push(temp_lazy_statements, lazy_value);
      }
    } else {
      u32 match_length = 0;
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
        if (parse_result->descriptor == &descriptor_ast_using) {
          const Module *module = value_as_ast_using(parse_result)->module;
          mass_copy_scope_exports(parser->scope, module->exports.scope);
          continue;
        } else if (parse_result->descriptor == &descriptor_assignment) {
          parse_result = mass_make_lazy_value(
            context, parser, parse_result->source_range, value_as_assignment(parse_result),
            &descriptor_void, mass_handle_assignment_lazy_proc
          );
        } else if (parse_result->descriptor == &descriptor_typed_symbol) {
          parse_result = mass_define_stack_value_from_typed_symbol(
            context, parser, value_as_typed_symbol(parse_result), parse_result->source_range
          );
        } else if (parse_result->descriptor == &descriptor_module_exports) {
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
      Mass_Block_Lazy_Payload *payload = mass_allocate(context, Mass_Block_Lazy_Payload);
      dyn_array_copy_from_temp(Array_Value_Ptr, context, &payload->statements, temp_lazy_statements);

      block_result = mass_make_lazy_value(
        context, parser, last_result->source_range, payload, last_descriptor, mass_handle_block_lazy_proc
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
  block_parser.scope = scope_make(context->allocator, parser->scope);
  return token_parse_block_statements(context, &block_parser, group->statements, source_range);
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
mass_handle_explicit_return_lazy_proc(
  Mass_Context *context,
  Function_Builder *builder,
  const Expected_Result *expected_result,
  const Source_Range *source_range,
  const Mass_Value_Lazy_Payload *payload
) {
  Value *parse_result = payload->value;
  mass_assign_helper(context, builder, &builder->return_value, parse_result, source_range);
  if (mass_has_error(context)) return 0;
  Storage return_label = code_label32(builder->code_block.end_label);

  push_eagerly_encoded_assembly(
    &builder->code_block, *source_range,
    &(Instruction_Assembly) {jmp, {return_label}}
  );

  Value *void_value = mass_make_void(context, *source_range);
  return expected_result_validate(expected_result, void_value);
}

static Value *
mass_return(
  Mass_Context *context,
  Parser *parser,
  Value_View args
) {
  assert(args.length == 1);
  Value *return_value = token_parse_single(context, parser, value_view_get(&args, 0));

  Mass_Value_Lazy_Payload *payload = mass_allocate(context, Mass_Value_Lazy_Payload);
  *payload = (Mass_Value_Lazy_Payload) {.value = return_value};
  return mass_make_lazy_value(
    context, parser, args.source_range, payload, &descriptor_void,
    mass_handle_explicit_return_lazy_proc
  );
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
      mass_assign_helper(context, 0, global_value, value, &value->source_range);
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
  };

  Value *variable_value = mass_make_lazy_value(
    context, parser, symbol->source_range, variable_payload, variable_descriptor,
    mass_handle_variable_definition_lazy_proc
  );

  const Source_Range *source_range = &symbol->source_range;

  scope_define_value(parser->scope, parser->epoch, *source_range, value_as_symbol(symbol), variable_value);

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
  Scope *enum_scope = scope_make(allocator, 0);
  const Descriptor *enum_descriptor = *value_as_descriptor_pointer(enum_type_value);
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
mass_compilation_init_scopes(
  Compilation *compilation
) {
  const Allocator *allocator = compilation->allocator;
  Scope *root_scope =  scope_make(compilation->allocator, 0);
  compilation->root_scope = root_scope;
  Scope *module_scope = scope_make(allocator, root_scope);
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
  Source_Range type_source_range;
  INIT_LITERAL_SOURCE_RANGE(&type_source_range, "Type");
  scope_define_value(
    root_scope, VALUE_STATIC_EPOCH, type_source_range,
    mass_ensure_symbol(compilation, slice_literal("Type")), type_descriptor_pointer_value
  );

  {
    static Token_Statement_Matcher default_statement_matchers[] = {
      {.proc = token_parse_constant_definitions},
      {.proc = token_parse_while},
      {.proc = token_parse_operator_definition},
    };
    for (u64 i = 0; i < countof(default_statement_matchers) - 1; ++i) {
      default_statement_matchers[i + 1].previous = &default_statement_matchers[i];
    }

    root_scope->statement_matcher = &default_statement_matchers[countof(default_statement_matchers) - 1];
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
    } break;
    case Module_Exports_Tag_Selective: {
      module->exports.scope = scope_make(context->allocator, module->own_scope->parent);

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
  Value *void_value = mass_make_void(context, args.source_range);
  value_force_exact(&module_context, 0, void_value, block_result);
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
  context->program = compilation->jit.program;
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
    context, &descriptor_ast_block, storage_immediate(&body), source_range
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
  const Function_Info *entry_info = function_literal_info_for_args(context, literal, (Value_View){0});
  context->program->entry_point = mass_function_literal_instance_for_info(context, literal, entry_info);

  if (mass_has_error(context)) return;

  Jit *jit = &compilation->jit;
  program_jit(context, jit);
  if (mass_has_error(context)) return;
  fn_type_opaque script = value_as_function(jit->program, jit->program->entry_point);
  script();
}