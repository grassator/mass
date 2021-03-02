#include "prelude.h"
#include "source.h"
#include "function.h"

static inline Value *
value_view_peek(
  Value_View view,
  u64 index
) {
  return index < view.length ? view.values[index] : 0;
}

static inline Value *
value_view_get(
  Value_View view,
  u64 index
) {
  Value *result = value_view_peek(view, index);
  assert(result);
  return result;
}

static inline Value *
value_view_last(
  Value_View view
) {
  assert(view.length);
  return value_view_get(view, view.length - 1);
}

static Value_View
value_view_slice(
  const Value_View *view,
  u64 start_index,
  u64 end_index
) {
  assert(end_index <= view->length);
  assert(start_index <= end_index);

  Source_Range source_range = view->source_range;
  source_range.offsets.to = end_index == view->length
    ? view->source_range.offsets.to
    : view->values[end_index]->source_range.offsets.from;
  source_range.offsets.from = start_index == end_index
    ? source_range.offsets.to
    : view->values[start_index]->source_range.offsets.from;

  return (Value_View) {
    .values = view->values + start_index,
    .length = end_index - start_index,
    .source_range = source_range,
  };
}

static inline Value_View
value_view_rest(
  const Value_View *view,
  u64 index
) {
  return value_view_slice(view, index, view->length);
}

static inline Value_View
value_view_from_value_array(
  Array_Value_Ptr value_array,
  const Source_Range *source_range
) {
  return (Value_View) {
    .values = dyn_array_raw(value_array),
    .length = dyn_array_length(value_array),
    .source_range = *source_range
  };
}

Scope *
scope_make(
  const Allocator *allocator,
  const Scope *parent
) {
  // TODO use _Atomic when supported
  static u64 id = 0;

  Scope *scope = allocator_allocate(allocator, Scope);
  *scope = (Scope) {
    .id = ++id,
    .allocator = allocator,
    .parent = parent,
    .map = 0,
  };
  return scope;
}

const Scope *
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

static inline Scope *
scope_flatten_till_internal(
  const Allocator *allocator,
  const Scope *scope,
  const Scope *till,
  u64 macro_count,
  u64 statement_matcher_count
) {
  // On the way up we calculate how many things all levels combined contain to avoid resizes
  // TODO allow to pre-size a hashmap
  if (scope != till) {
    if (dyn_array_is_initialized(scope->macros)) {
      macro_count += dyn_array_length(scope->macros);
    }
    if (dyn_array_is_initialized(scope->statement_matchers)) {
      statement_matcher_count += dyn_array_length(scope->statement_matchers);
    }
  } else {
    Scope *result = scope_make(allocator, 0);
    result->map = Scope_Map__make(result->allocator);
    if (macro_count) {
      result->macros = dyn_array_make(
        Array_Macro_Ptr, .capacity = macro_count, .allocator = allocator
      );
    }
    if (statement_matcher_count) {
      result->statement_matchers = dyn_array_make(
        Array_Token_Statement_Matcher, .capacity = statement_matcher_count, .allocator = allocator
      );
    }
    return result;
  }

  Scope *result = scope_flatten_till_internal(
    allocator, scope->parent, till, macro_count, statement_matcher_count
  );
  if (dyn_array_is_initialized(scope->macros)) {
    for (u64 i = 0; i < dyn_array_length(scope->macros); ++i) {
      dyn_array_push(result->macros, *dyn_array_get(scope->macros, i));
    }
  }
  if (dyn_array_is_initialized(scope->statement_matchers)) {
    for (u64 i = 0; i < dyn_array_length(scope->statement_matchers); ++i) {
      dyn_array_push(result->statement_matchers, *dyn_array_get(scope->statement_matchers, i));
    }
  }

  if (scope->map) {
    for (u64 i = 0; i < scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &scope->map->entries[i];
      if (entry->occupied) {
        hash_map_set_by_hash(result->map, entry->bookkeeping.hash, entry->key, entry->value);
      }
    }
  }
  return result;
}

static inline Scope *
scope_flatten_till(
  const Allocator *allocator,
  const Scope *scope,
  const Scope *till
) {
  return scope_flatten_till_internal(allocator, scope, till, 0, 0);
}

void
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

Scope_Entry *
scope_lookup(
  const Scope *scope,
  Slice name
) {
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    Scope_Entry *entry = *entry_pointer;
    if (entry) {
      return entry;
    }
  }
  return 0;
}

Value *
token_value_force_immediate_integer(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *value,
  const Descriptor *target_descriptor
) {
  MASS_ON_ERROR(*context->result) return 0;

  assert(descriptor_is_integer(target_descriptor));
  if (value->descriptor == &descriptor_number_literal) {
    u64 bits = 0xCCccCCccCCccCCcc;
    u64 bit_size = 0xCCccCCccCCccCCcc;
    Literal_Cast_Result cast_result =
      value_number_literal_cast_to(value, target_descriptor, &bits, &bit_size);
    switch(cast_result) {
      case Literal_Cast_Result_Success: {
        // Always copy full value. Truncation is handled by the byte_size of the immediate
        u64 byte_size = u64_to_u32(bit_size / 8);
        return value_make(
          context,
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
        context_error_snprintf(
          context, *source_range,
          // TODO maybe provide the range instead
          "Literal value does not fit into the target integer size %u",
          u64_to_u8(bit_size / 8)
        );
        return 0;
      }
      case Literal_Cast_Result_Target_Too_Big: {
        context_error_snprintf(
          context, *source_range, "Integers larger than 64 bits are not supported"
        );
        return 0;
      }
      case Literal_Cast_Result_Unsigned_Target_For_Negative_Literal: {
        context_error_snprintf(
          context, *source_range, "Can not convert a negative literal to an unsigned number"
        );
        return 0;
      }
    }
    panic("Unexpected literal cast result");
  }

  if (!descriptor_is_integer(value->descriptor)) {
    context_error_snprintf(
      context, *source_range,
      "Expected an integer"
    );
    return 0;
  }

  if (value->storage.tag != Storage_Tag_Static) {
    context_error_snprintf(
      context, *source_range,
      "Value is not an immediate"
    );
    return 0;
  }

  u64 target_byte_size = descriptor_byte_size(target_descriptor);
  if (target_byte_size > descriptor_byte_size(value->descriptor)) {
    context_error_snprintf(
      context, *source_range,
      "Static value does not fit into the target integer size %"PRIu64,
      target_byte_size
    );
  }

  // FIXME resize the value?

  return value;
}

Value *
maybe_coerce_number_literal_to_integer(
  Execution_Context *context,
  Value *value,
  const Descriptor *target_descriptor
) {
  if (!descriptor_is_integer(target_descriptor)) return value;
  if (value->descriptor != &descriptor_number_literal) return value;
  return token_value_force_immediate_integer(context, &value->source_range, value, target_descriptor);
}

PRELUDE_NO_DISCARD Mass_Result
assign(
  Execution_Context *context,
  Value *target,
  Value *source
) {
  MASS_TRY(*context->result);

  if (target->storage.tag == Storage_Tag_Eflags) {
    panic("Internal Error: Trying to move into Eflags");
  }
  if (target->descriptor->tag == Descriptor_Tag_Void) {
    return *context->result;
  }
  if (target->descriptor->tag == Descriptor_Tag_Any) {
    assert(target->storage.tag == Storage_Tag_Any);
    target->descriptor = source->descriptor;
    target->storage = source->storage;
    target->next_overload = source->next_overload;
    return *context->result;
  }
  Source_Range source_range = target->source_range;

  if (source->descriptor == &descriptor_number_literal) {
    if (target->descriptor->tag == Descriptor_Tag_Pointer) {
      const Number_Literal *literal = storage_static_as_c_type(&source->storage, Number_Literal);
      if (literal->bits == 0) {
        source = token_value_force_immediate_integer(
          context, &source_range, source, &descriptor_u64
        );
        source->descriptor = target->descriptor;
      } else {
        context_error_snprintf(
          context, source_range, "Trying to assign a non-zero literal number to a pointer"
        );
        return *context->result;
      }
    } else if (descriptor_is_integer(target->descriptor)) {
      source = token_value_force_immediate_integer(
        context, &source_range, source, target->descriptor
      );
    } else {
      context_error_snprintf(
        context, source_range, "Trying to assign a literal number to a non-integer value"
      );
      return *context->result;
    }
  } else if (
    source->descriptor->tag == Descriptor_Tag_Pointer &&
    source->storage.tag == Storage_Tag_Static
  ) {
    // If a static value contains a pointer, we expect an entry
    // in a special map that allows us to track whether the target memory
    // is also available in the compiled binary
    // TODO this probably needs to be recursive for structs.
    //      This might require support for relocations.
    const void *source_memory =
      storage_static_as_c_type_internal(&source->storage, source->storage.byte_size);
    Value *static_pointer = hash_map_get(context->compilation->static_pointer_map, source_memory);
    assert(static_pointer);
    if (static_pointer->storage.tag == Storage_Tag_None) {
      // TODO should depend on constness of the static value I guess?
      Section *section = &context->program->memory.sections.ro_data;
      u64 byte_size = descriptor_byte_size(static_pointer->descriptor);
      u64 alignment = descriptor_alignment(static_pointer->descriptor);

      // TODO this should also be deduped
      Label_Index label_index = allocate_section_memory(context, section, byte_size, alignment);
      static_pointer->storage = data_label32(label_index, byte_size);

      void *section_memory = rip_value_pointer_from_label_index(context->program, label_index);
      memcpy(section_memory, source_memory, byte_size);
    }
    assert(storage_is_label(&static_pointer->storage));
    load_address(context, &source_range, target, static_pointer);
    return *context->result;
  }

  assert(source->descriptor->tag != Descriptor_Tag_Function);
  if (same_value_type_or_can_implicitly_move_cast(target, source)) {
    move_value(context->allocator, context->builder, &source_range, &target->storage, &source->storage);
    return *context->result;
  }
  context_error_snprintf(
    context, source_range,
    "Incompatible type: expected %"PRIslice", got %"PRIslice,
    SLICE_EXPAND_PRINTF(target->descriptor->name), SLICE_EXPAND_PRINTF(source->descriptor->name)
  );
  return *context->result;
}

PRELUDE_NO_DISCARD Mass_Result
token_force_value(
  Execution_Context *context,
  Value *token,
  Value *result_value
);

Value *
scope_entry_force(
  Execution_Context *context,
  Scope_Entry *entry
) {
  switch(entry->tag) {
    case Scope_Entry_Tag_Operator: {
      panic("Internal Error: Operators are not allowed in this context");
      return 0;
    }
    case Scope_Entry_Tag_Lazy_Expression: {
      Scope_Entry_Lazy_Expression *expr = &entry->Lazy_Expression;
      Execution_Context lazy_context = *context;
      lazy_context.scope = expr->scope;
      Value *result = value_any(context, expr->tokens.source_range);
      compile_time_eval(&lazy_context, expr->tokens, result);
      *entry = (Scope_Entry) {
        .tag = Scope_Entry_Tag_Value,
        .Value.value = result,
        .next_overload = entry->next_overload,
        .source_range = entry->source_range,
      };
      return result;
    }
    case Scope_Entry_Tag_Value: {
      return entry->Value.value;
    }
  }
  panic("Internal Error: Unexpected scope entry type");
  return 0;
}

Value *
scope_lookup_force(
  Execution_Context *context,
  const Scope *scope,
  Slice name
) {
  Scope_Entry *entry = 0;
  for (; scope; scope = scope->parent) {
    if (!scope->map) continue;
    Scope_Entry **entry_pointer = hash_map_get(scope->map, name);
    if (!entry_pointer) continue;
    if (*entry_pointer) {
      entry = *entry_pointer;
      break;
    }
  }
  if (!entry) {
    return 0;
  }

  // Force lazy entries
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    if (it->tag == Scope_Entry_Tag_Lazy_Expression) {
      scope_entry_force(context, it);
    }
  }

  Value *result = 0;
  for (Scope_Entry *it = entry; it; it = it->next_overload) {
    assert(it->tag == Scope_Entry_Tag_Value);

    if (!result) {
      result = it->Value.value;
    } else {
      if (it->Value.value->descriptor->tag != Descriptor_Tag_Function) {
        panic("Only functions support overloading");
      }
      Value *overload = it->Value.value;
      overload->next_overload = result;
      result = overload;
    }
  }

  // For functions we need to gather up overloads from all parent scopes
  if (result && result->descriptor->tag == Descriptor_Tag_Function) {
    Value *last = result;
    const Scope *parent = scope;
    for (;;) {
      parent = parent->parent;
      if (!parent) break;
      if (!parent->map) continue;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(context, parent, name);
      if (!overload) panic("Just checked that hash map has the name so lookup must succeed");
      if (overload->descriptor->tag != Descriptor_Tag_Function) {
        panic("There should only be function overloads");
      }
      while (last->next_overload) {
        last = last->next_overload;
      }
      last->next_overload = overload;
    };
  }
  return result;
}

static inline void
scope_define(
  Scope *scope,
  Slice name,
  Scope_Entry entry
) {
  if (!scope->map) {
    scope->map = Scope_Map__make(scope->allocator);
  }
  Scope_Entry *allocated = allocator_allocate(scope->allocator, Scope_Entry);
  *allocated = entry;
  if (hash_map_has(scope->map, name)) {
    // We just checked that the map has the entry so it safe to deref right away
    Scope_Entry *it = *hash_map_get(scope->map, name);
    // TODO Consider using a hash map that allows multiple values instead
    while (it->next_overload) {
      it = it->next_overload;
    }
    it->next_overload = allocated;
  } else {
    hash_map_set(scope->map, name, allocated);
  }
}

bool
code_point_is_operator(
  s32 code_point
) {
  switch(code_point) {
    case '+':
    case '-':
    case '=':
    case '!':
    case '@':
    case '%':
    case '^':
    case '&':
    case '$':
    case '*':
    case '/':
    case ':':
    case ';':
    case ',':
    case '?':
    case '|':
    case '.':
    case '~':
    case '>':
    case '<':
      return true;
    default:
      return false;
  }
}

static inline Value *
token_make_symbol(
  const Allocator *allocator,
  Slice name,
  Symbol_Type type,
  Source_Range source_range
) {
  Symbol *symbol = allocator_allocate(allocator, Symbol);
  *symbol = (Symbol){
    .type = type,
    .name = name,
  };
  return value_init(
    allocator_allocate(allocator, Value),
    VALUE_STATIC_EPOCH, &descriptor_symbol, storage_static(symbol), source_range
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

bool
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
    case Token_Pattern_Tag_String: {
      if (!value_is_slice(value)) return false;
      if (pattern->String.slice.length) {
        const Slice *slice = value_as_slice(value);
        if (!slice_equal(pattern->String.slice, *slice)) return false;
      }
      return true;
    }
    case Token_Pattern_Tag_Group: {
      if (!value_is_group(value)) return false;
      return value_as_group(value)->tag == pattern->Group.tag;
    }
  }
  return true;
}

static inline bool
value_match_symbol(
  Value *token,
  Slice name
) {
  return value_match(token, &(Token_Pattern){.tag = Token_Pattern_Tag_Symbol, .Symbol.name = name});
}

static inline bool
value_match_group(
  Value *token,
  Group_Tag tag
) {
  return value_match(token, &(Token_Pattern){.tag = Token_Pattern_Tag_Group, .Group.tag = tag});
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

void
tokenizer_maybe_push_fake_semicolon(
  const Allocator *allocator,
  Array_Value_Ptr *stack,
  Array_u64 *parent_index_stack,
  Source_Range source_range
) {
  bool has_children = dyn_array_length(*stack) != 0;
  if (dyn_array_length(*parent_index_stack)) {
    u64 parent_index = *dyn_array_last(*parent_index_stack);
    Value *parent_value = *dyn_array_get(*stack, parent_index);
    if(value_as_group(parent_value)->tag != Group_Tag_Curly) return;
    has_children = parent_index + 1 != dyn_array_length(*stack);
  }
  // Do not treat leading newlines as semicolons
  if (!has_children) return;
  dyn_array_push(*stack, token_make_symbol(
    allocator, slice_literal(";"), Symbol_Type_Operator_Like, source_range
  ));
}

PRELUDE_NO_DISCARD Mass_Result
tokenize(
  Compilation *compilation,
  Source_File *file,
  Value_View *out_tokens
) {
  const Allocator *allocator = compilation->allocator;
  assert(!dyn_array_is_initialized(file->line_ranges));
  file->line_ranges = dyn_array_make(Array_Range_u64);

  enum Tokenizer_State {
    Tokenizer_State_Default,
    Tokenizer_State_Decimal_Integer,
    Tokenizer_State_Binary_Integer,
    Tokenizer_State_Hex_Integer,
    Tokenizer_State_Operator,
    Tokenizer_State_Symbol,
    Tokenizer_State_String,
    Tokenizer_State_String_Escape,
    Tokenizer_State_Single_Line_Comment,
  };

  Range_u64 current_line = {0};
  Source_Range current_token_range = {.file = file};
  enum Tokenizer_State state = Tokenizer_State_Default;
  Array_Value_Ptr stack = dyn_array_make(Array_Value_Ptr, .capacity = 100);
  Array_u64 parent_index_stack = dyn_array_make(Array_u64);

  Fixed_Buffer *string_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = 4096,
  );

  Mass_Result result = {.tag = Mass_Result_Tag_Success};

#define current_token_source()\
   slice_sub(file->text, current_token_range.offsets.from, i)

#define push(_VALUE_)\
  do {\
    dyn_array_push(stack, (_VALUE_));\
    state = Tokenizer_State_Default;\
  } while(0)

#define TOKENIZER_HANDLE_ERROR(_MESSAGE_)\
  do {\
    result = (const Mass_Result) {\
      .tag = Mass_Result_Tag_Error,\
      .Error.details = {\
        .message = slice_literal(_MESSAGE_),\
        .source_range = {\
          .file = file,\
          .offsets = {.from = i, .to = i},\
        }\
      }\
    };\
    goto err;\
  } while (0)
#define push_line()\
  do {\
    current_line.to = i + 1;\
    dyn_array_push(file->line_ranges, current_line);\
    current_line.from = current_line.to;\
    current_token_range.offsets = (Range_u64){i + 1, i + 1};\
    tokenizer_maybe_push_fake_semicolon(\
      allocator, &stack, &parent_index_stack, current_token_range\
    );\
  } while(0)

  u64 i = 0;
  for (; i < file->text.length; ++i) {
    u8 ch = file->text.bytes[i];
    u8 peek = i + 1 < file->text.length ? file->text.bytes[i + 1] : 0;

    retry: switch(state) {
      case Tokenizer_State_Default: {
        current_token_range.offsets = (Range_u64){.from = i, .to = i};
        if (ch == '\n') {
          push_line();
        } else if (ch == '\r') {
          if (peek == '\n') i++;
          push_line();
        } else if (isspace(ch)) {
          continue;
        } else if (ch == '0' && peek == 'x') {
          i++;
          state = Tokenizer_State_Hex_Integer;
        } else if (ch == '0' && peek == 'b') {
          i++;
          state = Tokenizer_State_Binary_Integer;
        } else if (isdigit(ch)) {
          state = Tokenizer_State_Decimal_Integer;
        } else if (isalpha(ch) || ch == '_') {
          state = Tokenizer_State_Symbol;
        } else if(ch == '/' && peek == '/') {
          state = Tokenizer_State_Single_Line_Comment;
        } else if (code_point_is_operator(ch)) {
          state = Tokenizer_State_Operator;
        } else if (ch == '"') {
          string_buffer->occupied = 0;
          state = Tokenizer_State_String;
        } else if (ch == '(' || ch == '{' || ch == '[') {
          Group *group = allocator_allocate(allocator, Group);
          group->tag =
            ch == '(' ? Group_Tag_Paren :
            ch == '{' ? Group_Tag_Curly :
            Group_Tag_Square;
          Value *value = value_init(
            allocator_allocate(allocator, Value),
            VALUE_STATIC_EPOCH, &descriptor_group, storage_static(group), current_token_range
          );
          dyn_array_push(parent_index_stack, dyn_array_length(stack));
          push(value);
        } else if (ch == ')' || ch == '}' || ch == ']') {
          Value *parent_value = 0;
          u64 parent_index = 0;
          if (dyn_array_length(parent_index_stack)) {
            parent_index = *dyn_array_last(parent_index_stack);
            parent_value = *dyn_array_get(stack, parent_index);
          }
          if (!parent_value || !value_is_group(parent_value)) {
            panic("Tokenizer: unexpected closing char for group");
          }
          // FIXME This const cast works at the moment, but could easily break
          //       should put the group on the stack instead.
          Group *group = (Group *)value_as_group(parent_value);
          s8 expected_paren = 0;
          switch (group->tag) {
            case Group_Tag_Paren: {
              expected_paren = ')';
              break;
            }
            case Group_Tag_Curly: {
              // Newlines at the end of the block do not count as semicolons otherwise this:
              // { 42
              // }
              // is being interpreted as:
              // { 42 ; }
              while (parent_index + 1 < dyn_array_length(stack)) {
                Value *last = *dyn_array_last(stack);
                bool is_last_token_a_fake_semicolon = (
                  range_length(last->source_range.offsets) == 0 &&
                  value_is_symbol(last) &&
                  slice_equal(value_as_symbol(last)->name, slice_literal(";"))
                );
                if (!is_last_token_a_fake_semicolon) break;
                dyn_array_pop(stack);
              }

              expected_paren = '}';
              break;
            }
            case Group_Tag_Square: {
              expected_paren = ']';
              break;
            }
          }
          if (ch != expected_paren) {
            TOKENIZER_HANDLE_ERROR("Mismatched closing brace");
          }
          parent_value->source_range.offsets.to = i + 1;
          Source_Range children_range = parent_value->source_range;
          children_range.offsets.to -= 1;
          children_range.offsets.from += 1;
          Value **children_values = dyn_array_raw(stack) + parent_index + 1;
          u64 child_count = dyn_array_length(stack) - parent_index - 1;
          group->children = temp_token_array_into_value_view(
            allocator, children_values, child_count, children_range
          );
          stack.data->length = parent_index + 1; // pop the children
          dyn_array_pop(parent_index_stack);
        } else {
          TOKENIZER_HANDLE_ERROR("Unpexpected input");
        }
        break;
      }
      case Tokenizer_State_Decimal_Integer: {
        if (!isdigit(ch)) {
          Slice digits = current_token_source();
          current_token_range.offsets.to = i;
          push(value_number_literal(allocator, digits, Number_Base_10, current_token_range));
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Hex_Integer: {
        if (!code_point_is_hex_digit(ch)) {
          Slice digits = current_token_source();
          current_token_range.offsets.to = i;
          // Cut off `0x` prefix
          digits = slice_sub(digits, 2, digits.length);
          push(value_number_literal(allocator, digits, Number_Base_16, current_token_range));
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Binary_Integer: {
        if (ch != '0' && ch != '1') {
          Slice digits = current_token_source();
          current_token_range.offsets.to = i;
          // Cut off `0b` prefix
          digits = slice_sub(digits, 2, digits.length);
          push(value_number_literal(allocator, digits, Number_Base_2, current_token_range));
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Symbol: {
        if (!(isalpha(ch) || isdigit(ch) || ch == '_')) {
          Slice name = current_token_source();
          current_token_range.offsets.to = i;
          push(token_make_symbol(allocator, name, Symbol_Type_Id_Like, current_token_range));
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Operator: {
        if (!code_point_is_operator(ch)) {
          Slice name = current_token_source();
          current_token_range.offsets.to = i;
          push(token_make_symbol(allocator, name, Symbol_Type_Operator_Like, current_token_range));
          goto retry;
        }
        break;
      }
      case Tokenizer_State_String: {
        if (ch == '\\') {
          state = Tokenizer_State_String_Escape;
        } else if (ch == '"') {
          current_token_range.offsets.to = i + 1;
          u64 length = string_buffer->occupied;
          char *bytes = allocator_allocate_bytes(allocator, length, 1);
          memcpy(bytes, string_buffer->memory, length);
          {
            Descriptor *bytes_descriptor = allocator_allocate(allocator, Descriptor);
            *bytes_descriptor = (Descriptor) {
              .tag = Descriptor_Tag_Fixed_Size_Array,
              .Fixed_Size_Array = { .item = &descriptor_u8, .length = length },
            };
            value_init(
              hash_map_set(compilation->static_pointer_map, bytes, (Value){0}),
              VALUE_STATIC_EPOCH,
              bytes_descriptor,
              (Storage){.tag = Storage_Tag_None},
              current_token_range
            );
          }

          Slice *string = allocator_allocate(allocator, Slice);
          *string = (Slice){bytes, string_buffer->occupied};
          Value *string_value = value_init(
            allocator_allocate(allocator, Value),
            VALUE_STATIC_EPOCH, &descriptor_slice, storage_static(string), current_token_range
          );
          push(string_value);
        } else {
          fixed_buffer_resizing_append_u8(&string_buffer, ch);
        }
        break;
      }
      case Tokenizer_State_String_Escape: {
        s8 escaped_character;
        switch (ch) {
          case 'n': escaped_character = '\n'; break;
          case 'r': escaped_character = '\r'; break;
          case 't': escaped_character = '\t'; break;
          case 'v': escaped_character = '\v'; break;
          case '0': escaped_character = '\0'; break;
          default: escaped_character = ch; break;
        }
        fixed_buffer_resizing_append_s8(&string_buffer, escaped_character);
        state = Tokenizer_State_String;
        break;
      }
      case Tokenizer_State_Single_Line_Comment: {
        if (ch == '\n') {
          state = Tokenizer_State_Default;
        }
        break;
      }
    }
  }

  // Handle end of file
  current_token_range.offsets.to = i;
  switch(state) {
    case Tokenizer_State_Operator: {
      Slice name = current_token_source();
      push(token_make_symbol(allocator, name, Symbol_Type_Operator_Like, current_token_range));
      break;
    }
    case Tokenizer_State_Symbol: {
      Slice name = current_token_source();
      push(token_make_symbol(allocator, name, Symbol_Type_Id_Like, current_token_range));
      break;
    }
    case Tokenizer_State_Default:
    case Tokenizer_State_Single_Line_Comment: {
      // Nothing to do
      break;
    }

    case Tokenizer_State_Decimal_Integer: {
      Slice digits = current_token_source();
      push(value_number_literal(allocator, digits, Number_Base_10, current_token_range));
      break;
    }
    case Tokenizer_State_Hex_Integer: {
      Slice digits = current_token_source();
      digits = slice_sub(digits, 2, digits.length); // Cut off `0x` prefix
      push(value_number_literal(allocator, digits, Number_Base_16, current_token_range));
      break;
    }
    case Tokenizer_State_Binary_Integer: {
      Slice digits = current_token_source();
      digits = slice_sub(digits, 2, digits.length); // Cut off `0b` prefix
      push(value_number_literal(allocator, digits, Number_Base_2, current_token_range));
      break;
    }
    case Tokenizer_State_String:
    case Tokenizer_State_String_Escape: {
      TOKENIZER_HANDLE_ERROR("String without closing quote");
      break;
    }
  }

  current_line.to = file->text.length;
  dyn_array_push(file->line_ranges, current_line);

  if (dyn_array_length(parent_index_stack)) {
    TOKENIZER_HANDLE_ERROR("Unexpected end of file. Expected a closing brace.");
  }

  err:
#undef TOKENIZER_HANDLE_ERROR
  if (result.tag == Mass_Result_Tag_Success) {
    Source_Range children_range = { .file = file, .offsets = {.from = 0, .to = file->text.length} };
    *out_tokens = temp_token_array_into_value_view(
      allocator, dyn_array_raw(stack), dyn_array_length(stack), children_range
    );
  }
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(stack);
  dyn_array_destroy(parent_index_stack);
  return result;
}

Value *
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

Value_View
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
  if (value->descriptor != &descriptor_type) {
    context_error_snprintf(context, source_range, "Expected a type");
    return 0;
  }
  const Descriptor *descriptor = storage_static_as_c_type(&value->storage, Descriptor);
  return descriptor;
}

const Descriptor *
scope_lookup_type(
  Execution_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice type_name
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  Scope_Entry *scope_entry = scope_lookup(scope, type_name);
  if (!scope_entry) {
    context_error_snprintf(
      context, source_range, "Could not find type %"PRIslice,
      SLICE_EXPAND_PRINTF(type_name)
    );
    return 0;
  }
  Value *value = scope_entry_force(context, scope_entry);
  return value_ensure_type(context, value, source_range);
}

static inline Value_View
value_view_match_till_end_of_statement(
  Value_View view,
  u64 *peek_index
) {
  u64 start_index = *peek_index;
  for (; *peek_index < view.length; *peek_index += 1) {
    Value *token = value_view_get(view, *peek_index);
    if (value_match_symbol(token, slice_literal(";"))) {
      *peek_index += 1;
      return value_view_slice(&view, start_index, *peek_index - 1);
    }
  }
  return value_view_slice(&view, start_index, *peek_index);
}

#define Token_Maybe_Match(_id_, ...)\
  Value *(_id_) = token_peek_match(view, peek_index, &(Token_Pattern) { __VA_ARGS__ });\
  if (_id_) (++peek_index)

#define Token_Match(_id_, ...)\
  Token_Maybe_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal(_op_))

typedef struct {
  Slice name;
  Value *value;
} Token_Match_Arg;

const Descriptor *
token_force_type(
  Execution_Context *context,
  Scope *scope,
  Value *value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  if (!value) return 0;

  const Descriptor *descriptor = 0;
  Source_Range source_range = value->source_range;
  if (value_is_group(value)) {
    const Group *group = value_as_group(value);
    if (group->tag != Group_Tag_Square) {
      panic("TODO");
    }
    if (group->children.length != 1) {
      context_error_snprintf(
        context, source_range, "Pointer type must have a single type inside"
      );
      return 0;
    }
    Value *child = value_view_get(group->children, 0);
    if (!value_is_symbol(child)) {
      panic("TODO: should be recursive");
    }
    Slice name = value_as_symbol(child)->name;
    Descriptor *temp = allocator_allocate(context->allocator, Descriptor);
    *temp = (Descriptor) {
      .tag = Descriptor_Tag_Pointer,
      .name = name,
      .Pointer.to = scope_lookup_type(context, scope, child->source_range, name),
    };
    descriptor = temp;
  } else if (value_is_symbol(value)) {
    const Symbol *symbol = value_as_symbol(value);
    descriptor = scope_lookup_type(context, scope, source_range, symbol->name);
    if (!descriptor) {
      MASS_ON_ERROR(*context->result) return 0;
      context_error_snprintf(
        context, source_range, "Could not find type %"PRIslice,
        SLICE_EXPAND_PRINTF(symbol->name)
      );
    }
  } else {
    descriptor = value_ensure_type(context, value, source_range);
  }
  return descriptor;
}

typedef enum {
  Macro_Match_Mode_Expression,
  Macro_Match_Mode_Statement
} Macro_Match_Mode;

u64
token_match_pattern(
  Value_View view,
  Macro *macro,
  Array_Value_View *out_match,
  Macro_Match_Mode mode
) {
  u64 pattern_length = dyn_array_length(macro->pattern);
  if (!pattern_length) panic("Zero-length pattern does not make sense");

  u64 pattern_index = 0;
  u64 view_index = 0;

  for (; pattern_index < pattern_length && view_index < view.length; pattern_index++) {
    Macro_Pattern *pattern = dyn_array_get(macro->pattern, pattern_index);
    switch(pattern->tag) {
      case Macro_Pattern_Tag_Single_Token: {
        Value *token = token_peek_match(view, view_index, &pattern->Single_Token.token_pattern);
        if (!token) {
          return 0;
        }
        if (out_match) {
          dyn_array_push(*out_match, value_view_slice(&view, view_index, view_index + 1));
        }
        view_index++;
        break;
      }
      case Macro_Pattern_Tag_Any_Token_Sequence: {
        u64 any_token_start_view_index = view_index;
        Macro_Pattern *peek = pattern_index + 1 < pattern_length
          ? dyn_array_get(macro->pattern, pattern_index + 1)
          : 0;
        assert(!peek || peek->tag == Macro_Pattern_Tag_Single_Token);
        for (; view_index < view.length; ++view_index) {
          Value *token = value_view_get(view, view_index);
          if (
            !peek &&
            mode == Macro_Match_Mode_Statement &&
            value_match(token, &token_pattern_semicolon)) {
            break;
          }
          if (peek && value_match(token, &peek->Single_Token.token_pattern)) {
            break;
          }
        }
        if (out_match) {
          dyn_array_push(*out_match, value_view_slice(&view, any_token_start_view_index, view_index));
        }
        break;
      }
    }
  }

  // Did not match full pattern
  if (
    pattern_index != pattern_length &&
    !(
      pattern_index == pattern_length - 1 &&
      dyn_array_last(macro->pattern)->tag == Macro_Pattern_Tag_Any_Token_Sequence
    )
  ) {
    return 0;
  }

  return view_index;
}

Value *
token_make_fake_body(
  Execution_Context *context,
  Value_View children
) {
  Group *group = allocator_allocate(context->allocator, Group);
  *group = (Group){
    .tag = Group_Tag_Curly,
    .children = children,
  };

  return value_make(context, &descriptor_group, storage_static(group), children.source_range);
}

Value *
token_make_macro_capture_function(
  Execution_Context *context,
  Value *body,
  Scope *captured_scope,
  Array_Function_Argument arguments,
  Descriptor *return_descriptor,
  Slice capture_name
) {
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Function,
    .name = capture_name,
    .Function.info = {
      .arguments = arguments,
      .scope = captured_scope,
      .body = body,
      .flags
        = Descriptor_Function_Flags_Macro
        | Descriptor_Function_Flags_No_Own_Scope
        | Descriptor_Function_Flags_No_Own_Return,
      .returns = {
        .name = {0},
        .descriptor = return_descriptor,
      }
    },
  };

  return value_make(context, descriptor, (Storage){.tag = Storage_Tag_None}, body->source_range);
}

void
token_apply_macro_syntax(
  Execution_Context *context,
  Array_Value_View match,
  Macro *macro,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

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
    Value *fake_body = token_make_fake_body(context, capture_view);
    Descriptor *return_descriptor = macro->replacement.length ? &descriptor_any : &descriptor_void;

    Array_Function_Argument empty_arguments = {&dyn_array_zero_items};
    Value *result = token_make_macro_capture_function(
      context, fake_body, captured_scope, empty_arguments, return_descriptor, capture_name
    );
    // This overload allows the macro implementation to pass in a scope into a captured that
    // will be expanded with `using` to bring in values from into a local scope. It is used
    // to expose `break` and `continue` statements to the body of the loop while avoiding their
    // definition in the captured scope of an expansion
    // TODO @Speed figure out a better way to do this
    {
      Array_Function_Argument overload_arguments = dyn_array_make(
        Array_Function_Argument, .capacity = 1, .allocator = context->allocator
      );
      Source_Range source_range = capture_view.source_range;
      Value *argument_value = value_make(context, &descriptor_scope, storage_none, source_range);
      dyn_array_push(overload_arguments, (Function_Argument) {
        .name = slice_literal("@spliced_scope"),
        .value = argument_value,
      });

      Value *using = token_make_symbol(
        context->allocator, slice_literal("using"), Symbol_Type_Id_Like, source_range
      );
      Value *scope_symbol = token_make_symbol(
        context->allocator, slice_literal("@spliced_scope"), Symbol_Type_Id_Like, source_range
      );
      Value *semicolon = token_make_symbol(
        context->allocator, slice_literal(";"), Symbol_Type_Operator_Like, source_range
      );

      Value **scope_body_tokens = allocator_allocate_array(context->allocator, Value *, 4);
      scope_body_tokens[0] = using;
      scope_body_tokens[1] = scope_symbol;
      scope_body_tokens[2] = semicolon;
      scope_body_tokens[3] = fake_body;

      Value_View scope_value_view = (Value_View) {
        .values = scope_body_tokens,
        .length = 4,
        .source_range = capture_view.source_range,
      };
      Value *overload_body = token_make_fake_body(context, scope_value_view);
      Value *scope_overload = token_make_macro_capture_function(
        context, overload_body, captured_scope, overload_arguments, return_descriptor, capture_name
      );
      result->next_overload = scope_overload;
    }

    scope_define(expansion_scope, capture_name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = result,
      .source_range = capture_view.source_range,
    });
  }

  Execution_Context body_context = *context;
  body_context.scope = expansion_scope;

  token_parse_expression(&body_context, macro->replacement, result_value, 0);
}

u64
token_parse_macro_statement(
  Execution_Context *context,
  Value_View value_view,
  Value *result_value,
  void *payload
) {
  assert(payload);
  if (!value_view.length) return 0;
  Macro *macro = payload;
  u64 match_length = token_match_pattern(value_view, macro, 0, Macro_Match_Mode_Statement);
  if (!match_length) return 0;

  Value_View rest = value_view_rest(&value_view, match_length);
  if (rest.length) {
    if (value_match(value_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length += 1;
    } else {
      return 0;
    }
  }

  Array_Value_View match = dyn_array_make(Array_Value_View);
  token_match_pattern(value_view, macro, &match, Macro_Match_Mode_Statement);

  token_apply_macro_syntax(context, match, macro, result_value);
  dyn_array_destroy(match);
  return match_length;
}

hash_map_slice_template(Raw_Macro_Map, Value_View)

void
token_parse_block_view(
  Execution_Context *context,
  Value_View children_view,
  Value *block_result_value
);

u64
token_parse_macro_rewrite(
  Execution_Context *context,
  Value_View value_view,
  Value *result_value,
  void *payload
) {
  assert(payload);
  if (!value_view.length) return 0;
  Macro *macro = payload;
  u64 match_length = token_match_pattern(value_view, macro, 0, Macro_Match_Mode_Statement);
  if (!match_length) return 0;

  Value_View rest = value_view_rest(&value_view, match_length);
  if (rest.length) {
    if (value_match(value_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length += 1;
    } else {
      return 0;
    }
  }
  Array_Value_View match = dyn_array_make(Array_Value_View);
  token_match_pattern(value_view, macro, &match, Macro_Match_Mode_Statement);

  // TODO precalculate required capacity
  Array_Value_Ptr result_tokens = dyn_array_make(Array_Value_Ptr, .allocator = context->allocator);

  for (u64 i = 0; i < macro->replacement.length; ++i) {
    Value *value = value_view_get(macro->replacement, i);
    if (value_is_symbol(value)) {
      Slice name = value_as_symbol(value)->name;
      Value_View *maybe_replacement = 0;
      for (u64 i = 0; i < dyn_array_length(macro->pattern); ++i) {
        Macro_Pattern *item = dyn_array_get(macro->pattern, i);
        if (slice_equal(item->capture_name, name)) {
          maybe_replacement = dyn_array_get(match, i);
          break;
        }
      }
      if (maybe_replacement) {
        for (u64 splice_index = 0; splice_index < maybe_replacement->length; ++splice_index) {
          dyn_array_push(result_tokens, value_view_get(*maybe_replacement, splice_index));
        }
        continue;
      }
    }
    dyn_array_push(result_tokens, value);
  }

  Value_View block_tokens =
    value_view_from_value_array(result_tokens, &macro->replacement.source_range);
  token_parse_block_view(context, block_tokens, result_value);

  dyn_array_destroy(match);
  return match_length;
}

Value *
token_parse_macros(
  Execution_Context *context,
  Value_View value_view,
  u64 *match_length
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  const Scope *scope = context->scope;
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);

      *match_length = token_match_pattern(value_view, macro, 0, Macro_Match_Mode_Expression);
      if (!*match_length) continue;
      Array_Value_View match = dyn_array_make(Array_Value_View);
      token_match_pattern(value_view, macro, &match, Macro_Match_Mode_Expression);

      Value *replacement = value_any(context, value_view.source_range);
      token_apply_macro_syntax(context, match, macro, replacement);
      dyn_array_destroy(match);
      return replacement;
    }
  }
  return 0;
}

Descriptor *
token_match_fixed_array_type(
  Execution_Context *context,
  Value_View view
);

const Descriptor *
token_match_type(
  Execution_Context *context,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Descriptor *descriptor = token_match_fixed_array_type(context, view);
  MASS_ON_ERROR(*context->result) return 0;
  if (descriptor) return descriptor;
  if (!view.length) panic("Caller must not call token_match_type with empty token list");
  Value *token = value_view_get(view, 0);
  if (view.length > 1) {
    context_error_snprintf(
      context, token->source_range, "Can not resolve type"
    );
    return 0;
  }
  return token_force_type(context, context->scope, token);
}

bool
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

Function_Argument
token_match_argument(
  Execution_Context *context,
  Value_View view,
  Function_Info *function
) {
  Function_Argument arg = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return arg;

  Value_View default_expression;
  Value_View definition;
  Value *equals;

  if (token_maybe_split_on_operator(
    view, slice_literal("="), &definition, &default_expression, &equals
  )) {
    if (default_expression.length == 0) {
      context_error_snprintf(context, equals->source_range, "Expected an expression after `=`");
      goto err;
    }
  } else {
    definition = view;
    default_expression = (Value_View){0};
  }

  Value_View type_expression;
  Value_View name_tokens;
  Value *operator;
  if (token_maybe_split_on_operator(
    definition, slice_literal(":"), &name_tokens, &type_expression, &operator
  )) {
    if (name_tokens.length == 0) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects an identifier on the left hand side"
      );
      goto err;
    }
    Value *name_token = value_view_get(name_tokens, 0);
    if (name_tokens.length > 1 || !value_is_symbol(name_token)) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects only a single identifier on the left hand side"
      );
      goto err;
    }

    const Descriptor *descriptor = token_match_type(context, type_expression);
    arg = (Function_Argument) {
      .name = value_as_symbol(name_token)->name,
      .value = value_make(context, descriptor, storage_none, definition.source_range),
      .maybe_default_expression = default_expression,
    };
  } else {
    Value *value = value_any(context, definition.source_range);
    compile_time_eval(context, view, value);
    arg = (Function_Argument) { .value = value };
  }

  //arg.value->epoch = VALUE_STATIC_EPOCH;

  err:
  return arg;
}

Function_Return
token_match_return_type(
  Execution_Context *context,
  Value_View view
) {
  Function_Return returns = {0};
  if (context->result->tag != Mass_Result_Tag_Success) return returns;

  Value_View lhs;
  Value_View rhs;
  Value *operator;
  if (token_maybe_split_on_operator(view, slice_literal(":"), &lhs, &rhs, &operator)) {
    if (lhs.length == 0) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects an identifier on the left hand side"
      );
      goto err;
    }
    if (lhs.length > 1 || !value_is_symbol(value_view_get(lhs, 0))) {
      context_error_snprintf(
        context, operator->source_range,
        "':' operator expects only a single identifier on the left hand side"
      );
      goto err;
    }
    returns.descriptor = token_match_type(context, rhs);
    returns.name = value_as_symbol(value_view_get(lhs, 0))->name;
  } else {
    returns.descriptor = token_match_type(context, view);
  }

  err:
  return returns;
}

PRELUDE_NO_DISCARD Mass_Result
token_force_value(
  Execution_Context *context,
  Value *token,
  Value *result_value
) {
  MASS_TRY(*context->result);

  if (token) {
    Value *value = token;
    if (value_is_group(token)) {
      const Group *group = value_as_group(token);
      switch(group->tag) {
        case Group_Tag_Paren: {
          token_parse_expression(context, group->children, result_value, 0);
          return *context->result;
        }
        case Group_Tag_Curly: {
          token_parse_block(context, token, result_value);
          return *context->result;
        }
        case Group_Tag_Square: {
          panic("TODO");
          return *context->result;
        }
      }
    } else if(value_is_symbol(token)) {
      Slice name = value_as_symbol(token)->name;
      value = scope_lookup_force(context, context->scope, name);
      MASS_TRY(*context->result);
      if (!value) {
        scope_print_names(context->scope);
        context_error_snprintf(
          context, value->source_range,
          "Undefined variable %"PRIslice,
          SLICE_EXPAND_PRINTF(name)
        );

        return *context->result;
      } else if (
        value->storage.tag != Storage_Tag_Static &&
        value->storage.tag != Storage_Tag_None
      ) {
        if (value->epoch != context->epoch) {
          context_error_snprintf(
            context, value->source_range,
            "Trying to access a runtime variable %"PRIslice" from a different epoch. "
            "This happens when you access value from runtime in compile-time execution "
            "or a runtime value of one compile time execution in a diffrent one.",
            SLICE_EXPAND_PRINTF(name)
          );
          return *context->result;
        }
      }
    }
    return assign(context, result_value, value);
  } else {
    // TODO consider what should happen here
  }
  return *context->result;
}


// FIXME pass in the function definition
Array_Value_Ptr
token_match_call_arguments(
  Execution_Context *context,
  Value *token
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (context->result->tag != Mass_Result_Tag_Success) return result;
  const Group *group = value_as_group(token);

  if (group->children.length != 0) {
    Value_View_Split_Iterator it = { .view = group->children };

    while (!it.done) {
      if (context->result->tag != Mass_Result_Tag_Success) return result;
      Value_View view = token_split_next(&it, &token_pattern_comma_operator);
      // TODO :TargetValue
      // There is an interesting conundrum here that we need to know the types of the
      // arguments for overload resolution, but then we need the exact function definition
      // to know the result_value definition to do the evaluation. Proper solution would
      // be to introduce :TypeOnlyEvalulation, but for now we will just create a special
      // target value that can be anything that will behave like type inference and is
      // needed regardless for something like x := (...)
      Value *result_value = value_any(context, view.source_range);
      token_parse_expression(context, view, result_value, 0);
      dyn_array_push(result, result_value);
    }
  }
  return result;
}

void
scope_add_macro(
  Scope *scope,
  Macro *macro
) {
  if (!dyn_array_is_initialized(scope->macros)) {
    scope->macros = dyn_array_make(Array_Macro_Ptr);
  }
  dyn_array_push(scope->macros, macro);
}

void
token_handle_user_defined_operator(
  Execution_Context *context,
  Value_View args,
  Value *result_value,
  User_Defined_Operator *operator
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  // We make a nested scope based on the original scope
  // instead of current scope for hygiene reasons.
  Scope *body_scope = scope_make(context->allocator, operator->scope);
  assert(operator->argument_count == args.length);

  for (u8 i = 0; i < operator->argument_count; ++i) {
    Slice arg_name = operator->argument_names[i];
    Value *arg = value_view_get(args, i);
    Source_Range source_range = arg->source_range;
    Value *arg_value = value_any(context, source_range);
    MASS_ON_ERROR(token_force_value(context, arg, arg_value)) return;
    scope_define(body_scope, arg_name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = arg_value,
      .source_range = source_range,
    });
  }

  Execution_Context body_context = *context;
  body_context.scope = body_scope;
  token_parse_block(&body_context, operator->body, result_value);
}

static inline void
token_handle_user_defined_operator_proc(
  Execution_Context *context,
  Value_View args,
  Value *result_value,
  void *payload
) {
  token_handle_user_defined_operator(context, args, result_value, payload);
}

u64
token_parse_exports(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_data
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;
  u64 peek_index = 0;
  Token_Match(keyword_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("exports"));
  Token_Maybe_Match(block, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Curly);

  if (!block) {
    context_error_snprintf(
      context, keyword_token->source_range,
      "exports keyword must be followed {}"
    );
    goto err;
  }

  if (context->module->flags & Module_Flags_Has_Exports) {
    // TODO track original exports
    context_error_snprintf(
      context, keyword_token->source_range,
      "A module can not have multiple exports statements"
    );
    goto err;
  }

  Value_View children = value_as_group(block)->children;
  if (children.length == 1) {
    if (value_match_symbol(value_view_get(children, 0), slice_literal(".."))) {
      context->module->export_scope = context->module->own_scope;
      return peek_index;
    }
  }
  context->module->export_scope =
    scope_make(context->allocator, context->module->own_scope->parent);

  if (children.length != 0) {
    Value_View_Split_Iterator it = { .view = children };

    while (!it.done) {
      if (context->result->tag != Mass_Result_Tag_Success) goto err;
      Value_View item = token_split_next(&it, &token_pattern_comma_operator);
      if (item.length != 1 || !value_is_symbol(value_view_get(item, 0))) {
        context_error_snprintf(
          context, item.source_range,
          "Exports {} block must contain a comma-separated identifier list"
        );
        goto err;
      }
      Value *symbol_token = value_view_get(item, 0);
      Slice name = value_as_symbol(symbol_token)->name;
      scope_define(context->module->export_scope, name, (Scope_Entry) {
        .tag = Scope_Entry_Tag_Lazy_Expression,
        .Lazy_Expression = {
          .name = name,
          .tokens = item,
          .scope = context->module->own_scope,
        },
        .source_range = symbol_token->source_range,
      });
    }
  }

  err:
  return peek_index;
}

static inline Slice
operator_fixity_to_lowercase_slice(
  Operator_Fixity fixity
) {
  switch(fixity) {
    case Operator_Fixity_Infix: return slice_literal("an infix");
    case Operator_Fixity_Prefix: return slice_literal("a prefix");
    case Operator_Fixity_Postfix: return slice_literal("a postfix");
  }
  panic("Unexpected fixity");
  return slice_literal("");
}

u64
token_parse_operator_definition(
  Execution_Context *context,
  Value_View view,
  Value *unused_value,
  void *unused_data
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  User_Defined_Operator *operator = 0;

  u64 peek_index = 0;
  Token_Match(keyword_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("operator"));

  Token_Maybe_Match(precedence_token, .tag = Token_Pattern_Tag_Any);

  if (!precedence_token) {
    context_error_snprintf(
      context, keyword_token->source_range,
      "'operator' keyword must be followed by a precedence number"
    );
    goto err;
  }

  Source_Range precedence_source_range = precedence_token->source_range;
  Value *precedence_value = value_any(context, precedence_source_range);
  MASS_ON_ERROR(token_force_value(context, precedence_token, precedence_value)) goto err;
  precedence_value = token_value_force_immediate_integer(
    context, &precedence_source_range, precedence_value, &descriptor_u64
  );
  MASS_ON_ERROR(*context->result) goto err;

  assert(precedence_value->storage.tag == Storage_Tag_Static);
  u64 precendence = storage_static_value_up_to_u64(&precedence_value->storage);

  Token_Maybe_Match(pattern_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);

  if (!pattern_token) {
    context_error_snprintf(
      context, precedence_source_range,
      "Operator definition have a pattern in () following the precedence"
    );
    goto err;
  }

  Token_Maybe_Match(body_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Curly);

  if (!body_token) {
    context_error_snprintf(
      context, precedence_source_range,
      "Operator definition have a macro body in {} following the pattern"
    );
    goto err;
  }

  Value_View definition = value_as_group(pattern_token)->children;

  operator = allocator_allocate(context->allocator, User_Defined_Operator);
  *operator = (User_Defined_Operator) {
    .body = body_token,
    .scope = context->scope,
  };

  Value *operator_token;
  Value *arguments[2] = {0};

  // prefix and postfix
  if (definition.length == 2) {
    Value *first =  value_view_get(definition, 0);
    bool is_first_operator_like =
      value_is_symbol(first) && value_as_symbol(first)->type == Symbol_Type_Operator_Like;
    operator->fixity = is_first_operator_like ? Operator_Fixity_Prefix : Operator_Fixity_Postfix;
    operator->argument_count = 1;
    if (operator->fixity == Operator_Fixity_Prefix) {
      operator_token = value_view_get(definition, 0);
      arguments[0] = value_view_get(definition, 1);
    } else {
      operator_token = value_view_get(definition, 1);
      arguments[0] = value_view_get(definition, 0);
    }
  } else if (definition.length == 3) { // infix
    operator->argument_count = 2;
    operator->fixity = Operator_Fixity_Infix;
    operator_token = value_view_get(definition, 1);
    arguments[0] = value_view_get(definition, 0);
    arguments[1] = value_view_get(definition, 2);
  } else {
    operator_token = 0;
    context_error_snprintf(
      context, pattern_token->source_range,
      "Expected the pattern to have two (for prefix / postfix) or three tokens"
    );
    goto err;
  }

  for (u8 i = 0; i < operator->argument_count; ++i) {
    if (!value_is_symbol(arguments[i])) {
      context_error_snprintf(
        context, arguments[i]->source_range,
        "Operator argument must be an identifier"
      );
      goto err;
    }
    operator->argument_names[i] = value_as_symbol(arguments[i])->name;
  }

  Slice operator_name = value_as_symbol(operator_token)->name;
  Scope_Entry *existing_scope_entry = scope_lookup(context->scope, operator_name);
  while (existing_scope_entry) {
    if (existing_scope_entry->tag != Scope_Entry_Tag_Operator) {
      panic("Internal Error: Found an operator-like scope entry that is not an operator");
    }
    Scope_Entry_Operator *operator_entry = &existing_scope_entry->Operator;
    if ((
      operator->fixity == operator_entry->fixity
    ) || (
      operator->fixity == Operator_Fixity_Infix &&
      operator_entry->fixity == Operator_Fixity_Postfix
    ) || (
      operator->fixity == Operator_Fixity_Postfix &&
      operator_entry->fixity == Operator_Fixity_Infix
    )) {
      Slice existing = operator_fixity_to_lowercase_slice(operator_entry->fixity);
      context_error_snprintf(
        context, keyword_token->source_range,
        "There is already %"PRIslice" operator %"PRIslice
        ". You can only have one definition for prefix and one for infix or suffix.",
        SLICE_EXPAND_PRINTF(existing), SLICE_EXPAND_PRINTF(operator_name)
      );
      goto err;
    }
    existing_scope_entry = existing_scope_entry->next_overload;
  }

  scope_define(context->scope, operator_name, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .source_range = operator_token->source_range,
    .Operator = {
      .precedence = precendence,
      .argument_count = operator->argument_count,
      .fixity = operator->fixity,
      .handler = token_handle_user_defined_operator_proc,
      .handler_payload = operator,
    }
  });

  return peek_index;

  err:
  if (operator) allocator_deallocate(context->allocator, operator, sizeof(*operator));
  return peek_index;
}

Slice
mass_normalize_import_path(
  const Allocator *allocator,
  Slice raw
) {
  // @Speed
  u8 *bytes = allocator_allocate_bytes(allocator, raw.length, _Alignof(u8));
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

Scope
mass_import(
  Execution_Context context,
  Slice file_path
) {
  Module *module;
  if (slice_equal(file_path, slice_literal("mass"))) {
    return *context.compilation->compiler_module.export_scope;
  }

  file_path = mass_normalize_import_path(context.allocator, file_path);
  Module **module_pointer = hash_map_get(context.compilation->module_map, file_path);
  if (module_pointer) {
    module = *module_pointer;
  } else {
    const Scope *root_scope = context.scope;
    while (root_scope->parent) root_scope = root_scope->parent;
    Scope *module_scope = scope_make(context.allocator, root_scope);
    module = program_module_from_file(&context, file_path, module_scope);
    program_import_module(&context, module);
    hash_map_set(context.compilation->module_map, file_path, module);
  }

  if (!module->export_scope) {
    return (Scope){0};
  }

  return *module->export_scope;
}

u64
token_parse_syntax_definition(
  Execution_Context *context,
  Value_View view,
  Value *result_value,
  void *payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("syntax"));
  Token_Maybe_Match(statement, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("statement"));
  Token_Maybe_Match(rewrite, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("rewrite"));

  Token_Maybe_Match(pattern_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);

  if (!pattern_token) {
    context_error_snprintf(
      context, name->source_range,
      "Syntax definition requires a parenthesized pattern definitions"
    );
    goto err;
  }

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
        context_error_snprintf(
          context, value->source_range,
          "Nested group matches are not supported in syntax declarations (yet)"
        );
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
        context_error_snprintf(
          context, value->source_range,
          "@ operator in a syntax definition requires an id after it"
        );
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
        context_error_snprintf(
          context, value->source_range,
          "@ requires a valid pattern before it"
        );
        goto err;
      }
      last_pattern->capture_name = value_as_symbol(symbol_token)->name;
    } else {
      context_error_snprintf(
        context, value->source_range,
        "Only compile time strings are allowed as values in the pattern"
      );
      goto err;
    }
  }
  Macro *macro = allocator_allocate(context->allocator, Macro);
  *macro = (Macro){
    .pattern = pattern,
    .replacement = replacement,
    .scope = context->scope
  };
  if (statement) {
    if (!dyn_array_is_initialized(context->scope->statement_matchers)) {
      context->scope->statement_matchers =
        dyn_array_make(Array_Token_Statement_Matcher, .allocator = context->allocator);
    }
    if (rewrite) {
      dyn_array_push(context->scope->statement_matchers, (Token_Statement_Matcher){
        .proc = token_parse_macro_rewrite,
        .payload = macro,
      });
    } else {
      dyn_array_push(context->scope->statement_matchers, (Token_Statement_Matcher){
        .proc = token_parse_macro_statement,
        .payload = macro,
      });
    }
  } else {
    scope_add_macro(context->scope, macro);
  }
  MASS_ON_ERROR(assign(context, result_value, &void_value));
  return peek_index;

  err:
  dyn_array_destroy(pattern);
  return peek_index;
}

bool
token_match_struct_field(
  Execution_Context *context,
  Descriptor *struct_descriptor,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(symbol, .tag = Token_Pattern_Tag_Symbol);
  Token_Match_Operator(define, ":");

  Value_View rest = value_view_rest(&view, peek_index);
  const Descriptor *descriptor = token_match_type(context, rest);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, value_as_symbol(symbol)->name);
  return true;
}

Descriptor
mass_bit_type(
  u64 bit_size
) {
  return (Descriptor) {
    .tag = Descriptor_Tag_Opaque,
    .Opaque = { .bit_size = bit_size },
  };
}

Value *
token_process_c_struct_definition(
  Execution_Context *context,
  Value *args
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if (!value_match_group(args, Group_Tag_Paren)) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct must be followed by ()"
    );
    goto err;
  }
  const Group *args_group = value_as_group(args);
  if (args_group->children.length != 1) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct expects 1 argument, got %"PRIu64,
      args_group->children.length
    );
    goto err;
  }
  Value *layout_block = value_view_get(args_group->children, 0);
  if (!value_match_group(layout_block, Group_Tag_Curly)) {
    context_error_snprintf(
      context, args->source_range,
      "c_struct expects a {} block as the argument"
    );
    goto err;
  }

  Value *result = allocator_allocate(context->allocator, Value);
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);

  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Struct,
    .Struct = {
      .fields = dyn_array_make(Array_Descriptor_Struct_Field),
    },
  };

  const Group *layout_group = value_as_group(layout_block);
  if (layout_group->children.length != 0) {
    Value_View_Split_Iterator it = { .view = layout_group->children };
    while (!it.done) {
      Value_View field_view = token_split_next(&it, &token_pattern_semicolon);
      token_match_struct_field(context, descriptor, field_view);
    }
  }

  *result = type_value_for_descriptor(descriptor);
  return result;

  err:
  return 0;
}

Value *
token_process_function_literal(
  Execution_Context *context,
  Value *args,
  Value *return_types,
  Value *body
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Scope *function_scope = scope_make(context->allocator, context->scope);

  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Function,
    .Function.info = {
      .arguments = (Array_Function_Argument){&dyn_array_zero_items},
      .body = body,
      .scope = function_scope,
      .returns = 0,
    },
  };

  Value_View return_types_view = value_as_group(return_types)->children;
  if (return_types_view.length == 0) {
    descriptor->Function.info.returns = (Function_Return) { .descriptor = &descriptor_void, };
  } else {
    Value_View_Split_Iterator it = { .view = return_types_view };

    for (u64 i = 0; !it.done; ++i) {
      if (i > 0) {
        context_error_snprintf(
          context, return_types->source_range,
          "Multiple return types are not supported at the moment"
        );
        return 0;
      }
      Value_View arg_view = token_split_next(&it, &token_pattern_comma_operator);

      Execution_Context arg_context = *context;
      arg_context.scope = function_scope;
      arg_context.builder = 0;
      descriptor->Function.info.returns = token_match_return_type(&arg_context, arg_view);
    }
  }

  bool previous_argument_has_default_value = false;
  Value_View args_view = value_as_group(args)->children;
  if (args_view.length != 0) {
    descriptor->Function.info.arguments = dyn_array_make(
      Array_Function_Argument,
      .allocator = context->allocator,
      .capacity = 4,
    );

    Value_View_Split_Iterator it = { .view = args_view };
    while (!it.done) {
      Value_View arg_view = token_split_next(&it, &token_pattern_comma_operator);
      Execution_Context arg_context = *context;
      arg_context.scope = function_scope;
      arg_context.builder = 0;
      Function_Argument arg = token_match_argument(&arg_context, arg_view, &descriptor->Function.info);
      dyn_array_push(descriptor->Function.info.arguments, arg);
      MASS_ON_ERROR(*context->result) return 0;
      if (previous_argument_has_default_value) {
        if (function_argument_is_exact(&arg) || !arg.maybe_default_expression.length ) {
          context_error_snprintf(
            context, arg_view.source_range,
            "Non-default argument can not come after a default one"
          );
          return 0;
        }
      } else {
        previous_argument_has_default_value = !!arg.maybe_default_expression.length;
      }
    }
  }

  return value_make(context, descriptor, (Storage){0}, args->source_range);
}

typedef void (*Compile_Time_Eval_Proc)(void *);

static u64 get_new_epoch() {
  // FIXME make atomic
  static u64 epoch = 1;
  return epoch++;
}

void
compile_time_eval(
  Execution_Context *context,
  Value_View view,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  const Source_Range *source_range = &view.source_range;

  Jit *jit = &context->compilation->jit;
  Execution_Context eval_context = *context;
  eval_context.epoch = get_new_epoch();
  eval_context.program = jit->program;
  // TODO consider if compile-time eval should create a nested scope
  //eval_context.scope = scope_make(context->allocator, context->scope);
  Descriptor *descriptor = allocator_allocate(context->allocator, Descriptor);
  *descriptor = (Descriptor){
    .tag = Descriptor_Tag_Function,
    .name = slice_literal("$compile_time_eval$"),
    .Function.info = {
      .returns = {
        .descriptor = &descriptor_void,
      },
    },
  };
  Label_Index eval_label_index = make_label(jit->program, &jit->program->memory.sections.code, slice_literal("compile_time_eval"));
  Value *eval_value = value_make(context, descriptor, code_label32(eval_label_index), view.source_range);
  Function_Builder eval_builder = {
    .function = &descriptor->Function.info,
    .label_index = eval_label_index,
    .code_block = {
      .end_label = make_label(jit->program, &jit->program->memory.sections.code, slice_literal("compile_time_eval_end")),
      .instructions = dyn_array_make(Array_Instruction, .allocator = context->allocator),
    },
  };
  eval_context.builder = &eval_builder;
  eval_context.builder->source = source_from_source_range(source_range);

  // FIXME We have to call token_parse_expression here before we figure out
  //       what is the return value because we need to figure out the return type.
  //       Symboleally there would be a type-only eval available instead
  Value *expression_result_value = value_any(context, view.source_range);
  token_parse_expression(&eval_context, view, expression_result_value, 0);
  MASS_ON_ERROR(*eval_context.result) {
    context->result = eval_context.result;
    return;
  }

  // If we didn't generate any instructions there is no point
  // actually running the code, we can just take the resulting value
  if (!dyn_array_length(eval_builder.code_block.instructions)) {
    if (expression_result_value->descriptor->tag == Descriptor_Tag_Function) {
      // It is only allowed to to pass through funciton definitions not compiled ones
      assert(expression_result_value->storage.tag == Storage_Tag_None);
    }
    MASS_ON_ERROR(assign(context, result_value, expression_result_value));
    return;
  }

  u64 result_byte_size = expression_result_value->storage.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u64 alignment = 16;
  void *result = allocator_allocate_bytes(context->allocator, result_byte_size, alignment);

  // Load the address of the result
  Register out_register = register_acquire_temp(&eval_builder);
  Value out_value_register = {
    .descriptor = &descriptor_s64,
    .storage = {
      .tag = Storage_Tag_Register,
      .byte_size = 8,
      .Register.index = out_register
    }
  };
  Value result_address = {
    .descriptor = &descriptor_s64,
    .storage = imm64((u64)result),
  };

  MASS_ON_ERROR(assign(&eval_context, &out_value_register, &result_address)) {
    context->result = eval_context.result;
    return;
  }

  // Use memory-indirect addressing to copy
  Value *out_value = value_make(&eval_context, expression_result_value->descriptor, (Storage){
    .tag = Storage_Tag_Memory,
    .byte_size = expression_result_value->storage.byte_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Indirect,
      .Indirect = {
        .base_register = out_register
      },
    },
  }, view.source_range);

  MASS_ON_ERROR(assign(&eval_context, out_value, expression_result_value)) {
    context->result = eval_context.result;
    return;
  }
  fn_end(jit->program, &eval_builder);
  dyn_array_push(jit->program->functions, eval_builder);

  program_jit(jit);

  fn_type_opaque jitted_code = value_as_function(jit, eval_value);
  jitted_code();

  Value *temp_result = value_make(context, out_value->descriptor, (Storage){0}, view.source_range);
  switch(out_value->descriptor->tag) {
    case Descriptor_Tag_Void: {
      temp_result->storage = (Storage){0};
      break;
    }
    case Descriptor_Tag_Any: {
      panic("Internal Error: We should never get Any type from comp time eval");
      break;
    }
    case Descriptor_Tag_Pointer: {
      panic("TODO move to data section or maybe we should allocate from there right away above?");
      break;
    };
    case Descriptor_Tag_Struct:
    case Descriptor_Tag_Fixed_Size_Array:
    case Descriptor_Tag_Opaque: {
      temp_result->storage = storage_static_internal(result, result_byte_size);
      break;
    }
    case Descriptor_Tag_Function: {
      assert(out_value->storage.tag == Storage_Tag_None);
      break;
    }
  }
  MASS_ON_ERROR(assign(context, result_value, temp_result));
}

typedef struct {
  Slice source;
  Source_Range source_range;
  Scope_Entry_Operator scope_entry;
} Operator_Stack_Entry;
typedef dyn_array_type(Operator_Stack_Entry) Array_Operator_Stack_Entry;

void
token_handle_storage_variant_of(
  Execution_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  if (dyn_array_length(args) != 1) {
    context_error_snprintf(
      context, *source_range,
      "storage_variant_of expects a single argument"
    );
    return;
  }

  Value *value = *dyn_array_get(args, 0);

  Value *storage_value;
  switch(value->storage.tag) {
    default:
    case Storage_Tag_None:
    case Storage_Tag_Any:
    case Storage_Tag_Static:
    case Storage_Tag_Eflags:
    case Storage_Tag_Xmm:
    case Storage_Tag_Memory: {
      panic("TODO implement operand reflection for more types");
      storage_value = 0;
      break;
    }
    case Storage_Tag_Register: {
      Descriptor *result_descriptor = 0;
      switch(value->storage.byte_size) {
        case 1: result_descriptor = &descriptor_register_8; break;
        case 2: result_descriptor = &descriptor_register_16; break;
        case 4: result_descriptor = &descriptor_register_32; break;
        case 8: result_descriptor = &descriptor_register_64; break;
        default: {
          panic("Internal Error: Unsupported register size");
          break;
        }
      }
      storage_value = value_make(
        context,
        result_descriptor,
        imm8(value->storage.Register.index),
        *source_range
      );
    }
  }
  MASS_ON_ERROR(assign(context, result_value, storage_value));
}

void
token_handle_c_string(
  Execution_Context *context,
  Value *args_token,
  Value *result_value
) {
  Array_Value_Ptr args = token_match_call_arguments(context, args_token);
  if (dyn_array_length(args) != 1) goto err;
  Value *arg_value = *dyn_array_get(args, 0);
  const Slice *c_string = value_as_slice(arg_value);
  if (!c_string) goto err;

  const Value *c_string_bytes =
    value_global_c_string_from_slice(context, *c_string, arg_value->source_range);
  Value *c_string_pointer = value_any(context, arg_value->source_range);
  load_address(context, &arg_value->source_range, c_string_pointer, c_string_bytes);
  MASS_ON_ERROR(assign(context, result_value, c_string_pointer));

  goto defer;

  err:
  context_error_snprintf(
    context, args_token->source_range,
    "c_string expects a single compile-time known string"
  );

  defer:
  dyn_array_destroy(args);
}

External_Symbol
mass_compiler_external(
  Slice library_name,
  Slice symbol_name
) {
  return (External_Symbol) {
    .library_name = library_name,
    .symbol_name = symbol_name,
  };
}

#define MASS_PROCESS_BUILT_IN_TYPE(_TYPE_, _BIT_SIZE)\
  _TYPE_ mass_##_TYPE_##_logical_shift_left(\
    _TYPE_ input,\
    u64 shift\
  ) {\
    return input << shift;\
  }\
  _TYPE_ mass_##_TYPE_##_bitwise_and(\
    _TYPE_ a,\
    _TYPE_ b\
  ) {\
    return a & b;\
  }\
  _TYPE_ mass_##_TYPE_##_bitwise_or(\
    _TYPE_ a,\
    _TYPE_ b\
  ) {\
    return a | b;\
  }
MASS_ENUMERATE_INTEGER_TYPES
#undef MASS_PROCESS_BUILT_IN_TYPE

void
token_handle_cast(
  Execution_Context *context,
  const Source_Range *source_range,
  Array_Value_Ptr args,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  const Descriptor *cast_to_descriptor = value_ensure_type(context, type, *source_range);

  assert(descriptor_is_integer(cast_to_descriptor));
  Value *after_cast_value = value;
  u64 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u64 original_byte_size = descriptor_byte_size(value->descriptor);
  if (value->descriptor == &descriptor_number_literal) {
    after_cast_value = token_value_force_immediate_integer(
      context, source_range, value, cast_to_descriptor
    );
  } else if (cast_to_byte_size < original_byte_size) {
    after_cast_value = value_make(context, cast_to_descriptor, value->storage, value->source_range);
    after_cast_value->storage.byte_size = cast_to_byte_size;
  }
  MASS_ON_ERROR(assign(context, result_value, after_cast_value));
}

void
token_handle_negation(
  Execution_Context *context,
  Value_View args,
  Value *result_value,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;
  assert(args.length == 1);
  Value *token = value_view_get(args, 0);

  // FIXME use result_value here
  Source_Range source_range = token->source_range;
  Value *value = value_any(context, source_range);
  MASS_ON_ERROR(token_force_value(context, token, value)) return;
  Value *negated_value;
  if (value->descriptor == &descriptor_number_literal) {
    const Number_Literal *original = storage_static_as_c_type(&value->storage, Number_Literal);
    Number_Literal *negated = allocator_allocate(context->allocator, Number_Literal);
    *negated = *original;
    negated->negative = !negated->negative;
    negated_value = value_make(
      context, &descriptor_number_literal, storage_static(negated), source_range
    );
  } else {
    panic("TODO support general negation");
    negated_value = 0;
  }
  MASS_ON_ERROR(assign(context, result_value, negated_value));
}

void
token_dispatch_operator(
  Execution_Context *context,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *operator_entry
);

bool
token_handle_operator(
  Execution_Context *context,
  Value_View view,
  Array_Value_Ptr *stack,
  Array_Operator_Stack_Entry *operator_stack,
  Slice new_operator,
  Source_Range source_range,
  Operator_Fixity fixity_mask
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  Scope_Entry *scope_entry = scope_lookup(context->scope, new_operator);

  Scope_Entry_Operator *operator_entry = 0;
  for (; scope_entry; scope_entry = scope_entry->next_overload) {
    if (scope_entry->tag != Scope_Entry_Tag_Operator) {
      context_error_snprintf(
        context, source_range,
        "%"PRIslice" is not an operator",
        SLICE_EXPAND_PRINTF(new_operator)
      );
      return false;
    }
    if (scope_entry->Operator.fixity & fixity_mask) {
      operator_entry = &scope_entry->Operator;
    }
  }

  if (!operator_entry) {
    context_error_snprintf(
      context, source_range,
      "Unknown operator %"PRIslice,
      SLICE_EXPAND_PRINTF(new_operator)
    );
    return false;
  }


  while (dyn_array_length(*operator_stack)) {
    Operator_Stack_Entry *last_operator = dyn_array_last(*operator_stack);

    if (last_operator->scope_entry.precedence < operator_entry->precedence) break;
    if (last_operator->scope_entry.precedence == operator_entry->precedence) {
      if (last_operator->scope_entry.associativity != Operator_Associativity_Left) {
        break;
      }
    }

    dyn_array_pop(*operator_stack);

    // apply the operator on the stack
    token_dispatch_operator(context, stack, last_operator);
  }
  dyn_array_push(*operator_stack, (Operator_Stack_Entry) {
    .source = new_operator,
    .source_range = source_range,
    .scope_entry = *operator_entry,
  });
  return true;
}

u64
token_parse_constant_definitions(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
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
  // For now we support only single ID on the left
  if (lhs.length > 1) {
    panic("TODO user error");
    goto err;
  }
  Value *symbol = value_view_get(lhs, 0);
  if (!value_is_symbol(symbol)) {
    panic("TODO user error");
    goto err;
  }

  Slice name = value_as_symbol(symbol)->name;
  scope_define(context->scope, name, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Lazy_Expression,
    .Lazy_Expression = {
      .name = name,
      .tokens = rhs,
      .scope = context->scope,
    },
    .source_range = lhs.source_range,
  });

  err:
  return statement_length;
}

void
token_handle_function_call(
  Execution_Context *context,
  Value *target_token,
  Value *args_token,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Source_Range source_range = target_token->source_range; // TODO add args as well
  Value *target = value_any(context, source_range);
  MASS_ON_ERROR(token_force_value(context, target_token, target)) return;
  assert(value_match_group(args_token, Group_Tag_Paren));

  if (
    target->descriptor->tag == Descriptor_Tag_Function &&
    (target->descriptor->Function.info.flags & Descriptor_Function_Flags_Compile_Time)
  ) {
    Descriptor *non_compile_time_descriptor = allocator_allocate(context->allocator, Descriptor);
    *non_compile_time_descriptor = *target->descriptor;
    // Need to remove Compile_Time flag otherwise we will go into an infinite loop
    non_compile_time_descriptor->Function.info.flags &= ~Descriptor_Function_Flags_Compile_Time;
    Value *fake_target_value =
      value_make(context, non_compile_time_descriptor, target->storage, source_range);
    Value_View fake_eval_view = {
      .values = (Value *[]){fake_target_value, args_token},
      .length = 2,
      .source_range = source_range,
    };
    compile_time_eval(context, fake_eval_view, result_value);
    return;
  }

  Array_Value_Ptr args;

  // FIXME this is a bit of hack to make sure we don't use argument registers for
  //       computing arguments as it might end up in the wrong one and overwrite.
  //       The right fix for that is to add type-only evaluation to figure out the
  //       correct target register for each argument and use it during mathcing.
  //       We will need type-only eval anyway for things like `typeof(some expression)`.
  {
    Register arg_registers[] = {Register_C, Register_D, Register_R8, Register_R9};
    bool acquired_registers[countof(arg_registers)] = {0};
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      Register reg_index = arg_registers[i];
      if (!register_bitset_get(context->builder->code_block.register_occupied_bitset, reg_index)) {
        register_acquire(context->builder, reg_index);
        acquired_registers[i] = true;
      }
    }

    args = token_match_call_arguments(context, args_token);
    MASS_ON_ERROR(*context->result) return;

    // Release any registers that we fake acquired to make sure that the actual call
    // does not unnecessarily store them to stack
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      if (acquired_registers[i]) {
        Register reg_index = arg_registers[i];
        register_release(context->builder, reg_index);
      }
    }
  }

  const Descriptor *target_descriptor = maybe_unwrap_pointer_descriptor(target->descriptor);

  if (target_descriptor->tag != Descriptor_Tag_Function) {
    Slice source = source_from_source_range(&source_range);
    context_error_snprintf(
      context, source_range,
      "%"PRIslice" is not a function",
      SLICE_EXPAND_PRINTF(source)
    );
    return;
  }

  struct Overload_Match { Value *value; s64 score; } match = { .score = -1 };
  for (Value *to_call = target; to_call; to_call = to_call->next_overload) {
    const Descriptor *to_call_descriptor = maybe_unwrap_pointer_descriptor(to_call->descriptor);
    assert(to_call_descriptor->tag == Descriptor_Tag_Function);
    const Function_Info *descriptor = &to_call_descriptor->Function.info;
    s64 score = calculate_arguments_match_score(descriptor, args);
    if (score == -1) continue; // no match
    if (score == match.score) {
      Slice previous_name = match.value->descriptor->name;
      Slice current_name = to_call_descriptor->name;
      // TODO provide names of matched overloads
      context_error_snprintf(
        context, source_range,
        "Could not decide which overload to pick."
        "Candidates are %"PRIslice" and %"PRIslice,
        SLICE_EXPAND_PRINTF(previous_name), SLICE_EXPAND_PRINTF(current_name)
      );
      return;
    } else if (score > match.score) {
      match.value = to_call;
      match.score = score;
    } else {
      // Skip a worse match
    }
  }

  if (match.score == -1) {
    Slice source = source_from_source_range(&source_range);
    // TODO provide types of actual arguments
    context_error_snprintf(
      context, source_range,
      "Could not find matching overload for call %"PRIslice,
      SLICE_EXPAND_PRINTF(source)
    );
    return;
  }


  Value *overload = match.value;
  if (overload) {
    const Function_Info *function = &overload->descriptor->Function.info;

    if (function->flags & Descriptor_Function_Flags_Macro) {
      assert(function->scope->parent);
      // We make a nested scope based on function's original parent scope
      // instead of current scope for hygiene reasons. I.e. function body
      // should not have access to locals inside the call scope.
      Scope *body_scope = (function->flags & Descriptor_Function_Flags_No_Own_Scope)
        ? function->scope
        : scope_make(context->allocator, function->scope);

      for (u64 i = 0; i < dyn_array_length(function->arguments); ++i) {
        Function_Argument *arg = dyn_array_get(function->arguments, i);
        if (arg->name.length) {
          assert(!function_argument_is_exact(arg));
          Value *arg_value;
          const Descriptor *arg_descriptor = arg->value->descriptor;
          if (i >= dyn_array_length(args)) {
            // We should catch the missing default expression in the matcher
            Value_View default_expression = arg->maybe_default_expression;
            assert(default_expression.length);
            // FIXME avoid using a stack value
            arg_value = reserve_stack(
              context, context->builder, arg_descriptor, default_expression.source_range
            );
            {
              Execution_Context arg_context = *context;
              arg_context.scope = body_scope;
              token_parse_expression(&arg_context, default_expression, arg_value, 0);
            }
            scope_define(body_scope, arg->name, (Scope_Entry) {
              .tag = Scope_Entry_Tag_Value,
              .Value.value = arg_value,
              .source_range = arg_value->source_range,
            });
          } else {
            arg_value = *dyn_array_get(args, i);
          }

          arg_value = maybe_coerce_number_literal_to_integer(context, arg_value, arg_descriptor);
          scope_define(body_scope, arg->name, (Scope_Entry) {
            .tag = Scope_Entry_Tag_Value,
            .Value.value = arg_value,
            .source_range = arg_value->source_range,
          });
        }
      }

      // Define a new return target label and value so that explicit return statements
      // jump to correct location and put value in the right place
      Program *program = context->program;
      Label_Index fake_return_label_index =
        make_label(program, &program->memory.sections.code, MASS_RETURN_LABEL_NAME);

      if (!(function->flags & Descriptor_Function_Flags_No_Own_Return)) {
        Value return_label = {
          .descriptor = &descriptor_void,
          .storage = code_label32(fake_return_label_index),
          .compiler_source_location = COMPILER_SOURCE_LOCATION_FIELDS,
        };
        scope_define(body_scope, MASS_RETURN_LABEL_NAME, (Scope_Entry) {
          .tag = Scope_Entry_Tag_Value,
          .Value.value = &return_label,
        });
        scope_define(body_scope, MASS_RETURN_VALUE_NAME, (Scope_Entry) {
          .tag = Scope_Entry_Tag_Value,
          .Value.value = result_value,
        });
      }

      Value *body = function->body;
      {
        Execution_Context body_context = *context;
        body_context.scope = body_scope;
        token_parse_block_no_scope(&body_context, body, result_value);
      }

      if (!(function->flags & Descriptor_Function_Flags_No_Own_Return)) {
        // @Hack if there are no instructions generated so far there definitely was no jumps
        //       to return so we can avoid generating this instructions which also can enable
        //       optimizations in the compile_time_eval that check for the instruction count.
        if (dyn_array_length(context->builder->code_block.instructions)) {
          push_instruction(
            &context->builder->code_block.instructions,
            source_range,
            (Instruction) {
              .type = Instruction_Type_Label,
              .label = fake_return_label_index
            }
          );
        }
      }
    } else {
      call_function_overload(context, &source_range, overload, args, result_value);
    }
  } else {
    // TODO add better error message
    context_error_snprintf(
      context, source_range,
      "Could not find matching overload"
    );
  }
  dyn_array_destroy(args);
}

static inline Value *
extend_integer_value(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *value,
  const Descriptor *target_descriptor
) {
  assert(descriptor_is_integer(value->descriptor));
  assert(descriptor_is_integer(target_descriptor));
  assert(descriptor_byte_size(target_descriptor) > descriptor_byte_size(value->descriptor));
  Value *result = reserve_stack(context, context->builder, target_descriptor, *source_range);
  move_value(context->allocator, context->builder, source_range, &result->storage, &value->storage);
  return result;
}

static inline Value *
extend_signed_integer_value_to_next_size(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *value
) {
  assert(descriptor_is_signed_integer(value->descriptor));
  assert(value->descriptor->tag == Descriptor_Tag_Opaque);
  Descriptor *one_size_larger;
  switch(value->descriptor->Opaque.bit_size) {
    case 8: {
      one_size_larger = &descriptor_s16;
      break;
    }
    case 16: {
      one_size_larger = &descriptor_s32;
      break;
    }
    case 32: {
      one_size_larger = &descriptor_s64;
      break;
    }
    default: {
      context_error_snprintf(
        context, *source_range,
        "Could not find large enough signed integer type to fit both operands"
      );
      return 0;
    }
  }
  return extend_integer_value(context, source_range, value, one_size_larger);
}

void
maybe_resize_values_for_integer_math_operation(
  Execution_Context *context,
  const Source_Range *source_range,
  Value **lhs_pointer,
  Value **rhs_pointer
) {
  *lhs_pointer = maybe_coerce_number_literal_to_integer(
    context, *lhs_pointer, (*rhs_pointer)->descriptor
  );
  *rhs_pointer = maybe_coerce_number_literal_to_integer(
    context, *rhs_pointer, (*lhs_pointer)->descriptor
  );

  const Descriptor *ld = (*lhs_pointer)->descriptor;
  const Descriptor *rd = (*rhs_pointer)->descriptor;

  bool ld_signed = descriptor_is_signed_integer(ld);
  bool rd_signed = descriptor_is_signed_integer(rd);

  u64 ld_size = descriptor_byte_size(ld);
  u64 rd_size = descriptor_byte_size(rd);

  if (ld_signed == rd_signed) {
    if (ld_size == rd_size) return;
    const Descriptor *larger_descriptor = ld_size > rd_size ? ld : rd;
    if (ld == larger_descriptor) {
      *rhs_pointer = extend_integer_value(context, source_range, *rhs_pointer, larger_descriptor);
    } else {
      *lhs_pointer = extend_integer_value(context, source_range, *lhs_pointer, larger_descriptor);
    }
    return;
  } else {
    // If the signed and unsigned have the same size need to
    // increase the size of the signed one so it fits the unsigned
    if (ld_size == rd_size) {
      if (ld_size == 8) {
        context_error_snprintf(
          context, *source_range,
          "Could not find large enough signed integer type to fit both operands"
        );
        return;
      }
      if (ld_signed) {
        *lhs_pointer =
          extend_signed_integer_value_to_next_size(context, source_range, *lhs_pointer);
        MASS_ON_ERROR(*context->result) return;
      } else {
        assert(rd_signed);
        *rhs_pointer =
          extend_signed_integer_value_to_next_size(context, source_range, *rhs_pointer);
        MASS_ON_ERROR(*context->result) return;
      }
    }

    // Now we know that the signed operand is larger so we move
    if (ld_signed) {
      *rhs_pointer =
        extend_integer_value(context, source_range, *rhs_pointer, (*lhs_pointer)->descriptor);
    } else {
      assert(rd_signed);
      *lhs_pointer =
        extend_integer_value(context, source_range, *lhs_pointer, (*rhs_pointer)->descriptor);
    }
  }
}

void
struct_get_field(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *struct_value,
  Slice name,
  Value *result_value
) {
  const Descriptor *descriptor = struct_value->descriptor;
  assert(descriptor->tag == Descriptor_Tag_Struct);
  for (u64 i = 0; i < dyn_array_length(descriptor->Struct.fields); ++i) {
    Descriptor_Struct_Field *field = dyn_array_get(descriptor->Struct.fields, i);
    if (slice_equal(name, field->name)) {
      Value *field_value;
      Storage *storage = &struct_value->storage;
      switch(storage->tag) {
        default:
        case Storage_Tag_Any:
        case Storage_Tag_Eflags:
        case Storage_Tag_Register:
        case Storage_Tag_Xmm:
        case Storage_Tag_None: {
          panic("Internal Error: Unexpected storage type for structs");
          field_value = 0;
          break;
        }
        case Storage_Tag_Static: {
          Storage field_storage = storage_static_internal(
            (s8 *)storage_static_as_c_type_internal(storage, storage->byte_size) + field->offset,
            descriptor_byte_size(field->descriptor)
          );
          field_value = value_make(context, field->descriptor, field_storage, *source_range);
          break;
        }
        case Storage_Tag_Memory: {
          Storage field_storage = *storage;
          assert(field_storage.Memory.location.tag == Memory_Location_Tag_Indirect);
          field_storage.byte_size = descriptor_byte_size(field->descriptor);
          field_storage.Memory.location.Indirect.offset += field->offset;
          field_value = value_make(context, field->descriptor, field_storage, *source_range);
          break;
        }
      }

      MASS_ON_ERROR(assign(context, result_value, field_value));
      return;
    }
  }

  assert(!"Could not find a field with specified name");
  return;
}

Storage
storage_load_index_address(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *target,
  const Descriptor *item_descriptor,
  Value *index_value
) {
  // @InstructionQuality
  // This code is very general in terms of the operands where the base
  // or the index are stored, but it is

  u64 item_byte_size = descriptor_byte_size(item_descriptor);
  Register reg = register_acquire_temp(context->builder);
  // FIXME this acquires but never releases a register
  Value *new_base_register =
    value_register_for_descriptor(context, reg, &descriptor_s64, index_value->source_range);

  // Move the index into the register
  move_value(
    context->allocator,
    context->builder,
    source_range,
    &new_base_register->storage,
    &index_value->storage
  );

  Value *byte_size_value = value_from_s64(context, item_byte_size, index_value->source_range);
  // Multiply index by the item byte size
  multiply(context, source_range, new_base_register, new_base_register, byte_size_value);

  {
    // @InstructionQuality
    // TODO If the source does not have index, on X64 it should be possible to avoid
    //      using an extra register and put the index into SIB

    // Load previous address into a temp register
    Register temp_register = register_acquire_temp(context->builder);
    Value temp_value = {
      .descriptor = &descriptor_s64,
      .storage = storage_register_for_descriptor(temp_register, &descriptor_s64)
    };

    if (target->descriptor->tag == Descriptor_Tag_Pointer) {
      move_value(
        context->allocator,
        context->builder,
        source_range,
        &temp_value.storage,
        &target->storage
      );
    } else {
      assert(target->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
      load_address(context, source_range, &temp_value, target);
    }
    plus(context, source_range, new_base_register, new_base_register, &temp_value);
    register_release(context->builder, temp_register);
  }

  return (Storage) {
    .tag = Storage_Tag_Memory,
    .byte_size = item_byte_size,
    .Memory.location = {
      .tag = Memory_Location_Tag_Indirect,
      .Indirect = {
        .base_register = new_base_register->storage.Register.index,
      }
    }
  };
}

void
token_handle_array_access(
  Execution_Context *context,
  const Source_Range *source_range,
  Value *array_value,
  Value *index_value,
  Value *result_value
) {
  index_value = maybe_coerce_number_literal_to_integer(context, index_value, &descriptor_u64);
  Value *array_element_value;
  if (array_value->descriptor->tag == Descriptor_Tag_Pointer) {
    const Descriptor *item_descriptor = array_value->descriptor->Pointer.to;
    Storage storage = storage_load_index_address(
      context, source_range, array_value, item_descriptor, index_value
    );
    array_element_value = value_make(context, item_descriptor, storage, array_value->source_range);
  } else {
    assert(array_value->descriptor->tag == Descriptor_Tag_Fixed_Size_Array);
    assert(array_value->storage.tag == Storage_Tag_Memory);
    assert(array_value->storage.Memory.location.tag == Memory_Location_Tag_Indirect);
    assert(!array_value->storage.Memory.location.Indirect.maybe_index_register.has_value);

    const Descriptor *item_descriptor = array_value->descriptor->Fixed_Size_Array.item;

    u64 item_byte_size = descriptor_byte_size(item_descriptor);

    array_element_value =
      value_make(context, item_descriptor, array_value->storage, array_value->source_range);
    array_element_value->storage.byte_size = item_byte_size;
    if (index_value->storage.tag == Storage_Tag_Static) {
      s32 index = s64_to_s32(storage_static_value_up_to_s64(&index_value->storage));
      array_element_value->storage.Memory.location.Indirect.offset = index * item_byte_size;
    } else {
      array_element_value->storage = storage_load_index_address(
        context, source_range, array_value, item_descriptor, index_value
      );
    }
  }
  // FIXME this might actually cause problems in assigning to an array element
  MASS_ON_ERROR(assign(context, result_value, array_element_value)) return;
}

void
token_eval_operator(
  Execution_Context *context,
  Value_View args_view,
  Operator_Stack_Entry *operator_entry,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;

  if (operator_entry->scope_entry.handler) {
    operator_entry->scope_entry.handler(
      context, args_view, result_value, operator_entry->scope_entry.handler_payload
    );
  } else if (slice_equal(operator, slice_literal("()"))) {
    Value *target = value_view_get(args_view, 0);
    Value *args_token = value_view_get(args_view, 1);
    Source_Range args_range = args_token->source_range;
    // TODO turn `cast` into a compile-time function call / macro
    if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("cast"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      token_handle_cast(context, &args_range, args, result_value);
      dyn_array_destroy(args);
    } else if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("c_string"))
    ) {
      token_handle_c_string(context, args_token, result_value);
    } else if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("c_struct"))
    ) {
      Value *result_token = token_process_c_struct_definition(context, args_token);
      MASS_ON_ERROR(token_force_value(context, result_token, result_value)) return;
    } else if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("storage_variant_of"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      token_handle_storage_variant_of(context, &args_range, args, result_value);
      dyn_array_destroy(args);
    } else if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("startup"))
    ) {
      Value *startup_function = value_any(context, args_token->source_range);
      token_parse_expression(
        context, value_as_group(args_token)->children, startup_function, 0
      );
      if (
        !startup_function ||
        startup_function->descriptor->tag != Descriptor_Tag_Function ||
        dyn_array_length(startup_function->descriptor->Function.info.arguments) ||
        startup_function->descriptor->Function.info.returns.descriptor != &descriptor_void
      ) {
        context_error_snprintf(
          context, args_range, "`startup` expects a () -> () {...} function as an argument"
        );
        return;
      }
      ensure_compiled_function_body(context, startup_function);
      dyn_array_push(context->program->startup_functions, startup_function);
      MASS_ON_ERROR(assign(context, result_value, &void_value)) return;
    } else if (
      value_is_symbol(target) &&
      slice_equal(value_as_symbol(target)->name, slice_literal("address_of"))
    ) {
      Array_Value_Ptr args = token_match_call_arguments(context, args_token);
      if (dyn_array_length(args) != 1) {
        context_error_snprintf(
          context, args_range, "address_of expects a single argument"
        );
        return;
      }
      load_address(context, &args_range, result_value, *dyn_array_get(args, 0));
      dyn_array_destroy(args);
    } else {
      token_handle_function_call(context, target, args_token, result_value);
    }
  } else if (slice_equal(operator, slice_literal("@"))) {
    Value *body = value_view_get(args_view, 0);
    Source_Range body_range = body->source_range;
    if (value_match_symbol(body, slice_literal("scope"))) {
      Value *scope_value =
        value_make(context, &descriptor_scope, storage_static(context->scope), body_range);
      MASS_ON_ERROR(assign(context, result_value, scope_value)) return;
    } else if (value_match_symbol(body, slice_literal("context"))) {
      Value *scope_value =
        value_make(context, &descriptor_execution_context, storage_static(context), body_range);
      MASS_ON_ERROR(assign(context, result_value, scope_value)) return;
    } else if (value_match_group(body, Group_Tag_Paren)) {
      compile_time_eval(context, value_as_group(body)->children, result_value);
    } else {
      context_error_snprintf(
        context, body_range,
        "@ operator must be followed by a parenthesized expression"
      );
      return;
    }
  } else if (slice_equal(operator, slice_literal("."))) {
    Value *lhs = value_view_get(args_view, 0);
    Value *rhs = value_view_get(args_view, 1);

    Source_Range rhs_range = rhs->source_range;
    Source_Range lhs_range = lhs->source_range;
    Value *lhs_value = value_any(context, lhs_range);
    MASS_ON_ERROR(token_force_value(context, lhs, lhs_value)) return;
    if (
      lhs_value->descriptor->tag == Descriptor_Tag_Struct ||
      lhs_value->descriptor == &descriptor_scope
    ) {
      if (value_is_symbol(rhs)) {
        if (lhs_value->descriptor->tag == Descriptor_Tag_Struct) {
          struct_get_field(context, &rhs_range, lhs_value, value_as_symbol(rhs)->name, result_value);
        } else {
          assert(lhs_value->descriptor == &descriptor_scope);
          const Scope *module_scope = storage_static_as_c_type(&lhs_value->storage, Scope);
          // TODO this is quite wasteful and also might not be correct
          //      to to create a nested scope when we try to force a value.
          //      A better option is probably to have a special case here instead
          //      of using `token_force_value`
          Scope *force_scope = scope_make(context->allocator, module_scope);
          Execution_Context module_context = *context;
          module_context.scope = force_scope;
          module_context.builder = 0;
          MASS_ON_ERROR(token_force_value(&module_context, rhs, result_value)) return;
        }
      } else {
        context_error_snprintf(
          context, rhs_range,
          "Right hand side of the . operator on structs must be an identifier"
        );
        return;
      }
    } else if (
      lhs_value->descriptor->tag == Descriptor_Tag_Fixed_Size_Array ||
      lhs_value->descriptor->tag == Descriptor_Tag_Pointer
    ) {
      if (
        value_match_group(rhs, Group_Tag_Paren) ||
        value_is_number_literal(rhs)
      ) {
        Value *index = value_any(context, rhs_range);
        token_force_value(context, rhs, index);
        token_handle_array_access(context, &lhs_range, lhs_value, index, result_value);
      } else {
        context_error_snprintf(
          context, rhs_range,
          "Right hand side of the . operator for an array must be a (expr) or a literal number"
        );
        return;
      }
    } else {
      context_error_snprintf(
        context, rhs_range,
        "Left hand side of the . operator must be a struct"
      );
      return;
    }
  } else if (
    slice_equal(operator, slice_literal("+")) ||
    slice_equal(operator, slice_literal("-")) ||
    slice_equal(operator, slice_literal("*")) ||
    slice_equal(operator, slice_literal("/")) ||
    slice_equal(operator, slice_literal("%"))
  ) {
    Value *lhs = value_view_get(args_view, 0);
    Value *rhs = value_view_get(args_view, 1);
    Source_Range rhs_range = rhs->source_range;
    Source_Range lhs_range = lhs->source_range;

    Value *lhs_value = value_any_init(&(Value){0}, context, lhs_range);
    MASS_ON_ERROR(token_force_value(context, lhs, lhs_value)) return;
    Value *rhs_value = value_any_init(&(Value){0}, context, rhs_range);
    MASS_ON_ERROR(token_force_value(context, rhs, rhs_value)) return;

    bool lhs_is_literal = lhs_value->descriptor == &descriptor_number_literal;
    bool rhs_is_literal = rhs_value->descriptor == &descriptor_number_literal;
    if (lhs_is_literal && rhs_is_literal) {
      // FIXME support large unsigned numbers
      lhs_value = token_value_force_immediate_integer(
        context, &lhs_range, lhs_value, &descriptor_s64
      );
      rhs_value = token_value_force_immediate_integer(
        context, &rhs_range, rhs_value, &descriptor_s64
      );
      MASS_ON_ERROR(*context->result) return;
    }

    if (!descriptor_is_integer(lhs_value->descriptor) && !lhs_is_literal) {
      context_error_snprintf(
        context, lhs_range,
        "Left hand side of the %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }
    if (!descriptor_is_integer(rhs_value->descriptor) && !rhs_is_literal) {
      context_error_snprintf(
        context, rhs_range,
        "Right hand side of the %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }

    maybe_resize_values_for_integer_math_operation(context, &lhs_range, &lhs_value, &rhs_value);
    MASS_ON_ERROR(*context->result) return;

    Function_Builder *builder = context->builder;

    Value *any_result = value_any_init(&(Value){0}, context, lhs_range);
    if (slice_equal(operator, slice_literal("+"))) {
      plus(context, &lhs_range, any_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("-"))) {
      minus(context, &lhs_range, any_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("*"))) {
      multiply(context, &lhs_range, any_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("/"))) {
      divide(context, &lhs_range, any_result, lhs_value, rhs_value);
    } else if (slice_equal(operator, slice_literal("%"))) {
      value_remainder(context, &lhs_range, any_result, lhs_value, rhs_value);
    } else {
      panic("Internal error: Unexpected operator");
    }
    if (any_result->storage.tag != Storage_Tag_Static) {
      // FIXME do proper register allocation
      Value *stack_result = reserve_stack(context, builder, lhs_value->descriptor, lhs_range);
      MASS_ON_ERROR(assign(context, stack_result, any_result)) return;
      if (any_result->storage.tag == Storage_Tag_Register) {
        ensure_register_released(context->builder, any_result->storage.Register.index);
      }
      MASS_ON_ERROR(assign(context, result_value, stack_result)) return;
    } else {
      MASS_ON_ERROR(assign(context, result_value, any_result)) return;
    }
  } else if (
    slice_equal(operator, slice_literal(">")) ||
    slice_equal(operator, slice_literal("<")) ||
    slice_equal(operator, slice_literal(">=")) ||
    slice_equal(operator, slice_literal("<=")) ||
    slice_equal(operator, slice_literal("==")) ||
    slice_equal(operator, slice_literal("!="))
  ) {
    Value *lhs = value_view_get(args_view, 0);
    Value *rhs = value_view_get(args_view, 1);
    Source_Range rhs_range = rhs->source_range;
    Source_Range lhs_range = lhs->source_range;

    Value *lhs_value = value_any(context, lhs_range);
    MASS_ON_ERROR(token_force_value(context, lhs, lhs_value)) return;
    Value *rhs_value = value_any(context, rhs_range);
    MASS_ON_ERROR(token_force_value(context, rhs, rhs_value)) return;

    bool lhs_is_literal = lhs_value->descriptor == &descriptor_number_literal;
    bool rhs_is_literal = rhs_value->descriptor == &descriptor_number_literal;
    if (lhs_is_literal && rhs_is_literal) {
      // FIXME support large unsigned numbers
      lhs_value = token_value_force_immediate_integer(
        context, &lhs_range, lhs_value, &descriptor_s64
      );
      rhs_value = token_value_force_immediate_integer(
        context, &rhs_range, rhs_value, &descriptor_s64
      );
      MASS_ON_ERROR(*context->result) return;
    }

    if (!descriptor_is_integer(lhs_value->descriptor) && !lhs_is_literal) {
      context_error_snprintf(
        context, lhs_range,
        "Left hand side of the %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }
    if (!descriptor_is_integer(rhs_value->descriptor) && !rhs_is_literal) {
      context_error_snprintf(
        context, rhs_range,
        "Right hand side of the %"PRIslice" is not an integer",
        SLICE_EXPAND_PRINTF(operator)
      );
      return;
    }
    maybe_resize_values_for_integer_math_operation(context, &lhs_range, &lhs_value, &rhs_value);
    MASS_ON_ERROR(*context->result) return;

    Compare_Type compare_type = 0;

    if (slice_equal(operator, slice_literal(">"))) compare_type = Compare_Type_Signed_Greater;
    else if (slice_equal(operator, slice_literal("<"))) compare_type = Compare_Type_Signed_Less;
    else if (slice_equal(operator, slice_literal(">="))) compare_type = Compare_Type_Signed_Greater_Equal;
    else if (slice_equal(operator, slice_literal("<="))) compare_type = Compare_Type_Signed_Less_Equal;
    else if (slice_equal(operator, slice_literal("=="))) compare_type = Compare_Type_Equal;
    else if (slice_equal(operator, slice_literal("!="))) compare_type = Compare_Type_Not_Equal;

    if (descriptor_is_unsigned_integer(lhs_value->descriptor)) {
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
        }
      }
    }

    compare(context, compare_type, &lhs_range, result_value, lhs_value, rhs_value);
  } else if (slice_equal(operator, slice_literal("->"))) {
    Value *arguments = value_view_get(args_view, 0);
    Value *return_types = value_view_get(args_view, 1);
    Value *body = value_view_get(args_view, 2);
    Value *function_value = token_process_function_literal(context, arguments, return_types, body);
    MASS_ON_ERROR(assign(context, result_value, function_value)) return;
  } else if (slice_equal(operator, slice_literal("macro"))) {
    Value *function = value_view_get(args_view, 0);
    Source_Range source_range = function->source_range;
    Value *function_value = value_any(context, source_range);
    MASS_ON_ERROR(token_force_value(context, function, function_value)) return;
    if (function_value) {
      if (
        function_value->descriptor->tag == Descriptor_Tag_Function &&
        !value_is_external_symbol(function_value->descriptor->Function.info.body)
      ) {
        Descriptor *macro_descriptor = allocator_allocate(context->allocator, Descriptor);
        *macro_descriptor = *function_value->descriptor;
        macro_descriptor->Function.info.flags |= Descriptor_Function_Flags_Macro;
        function_value = value_make(
          context, macro_descriptor, function_value->storage, function_value->source_range
        );
      } else {
        context_error_snprintf(
          context, source_range,
          "Only literal functions (with a body) can be marked as macro"
        );
      }
    }
    MASS_ON_ERROR(assign(context, result_value, function_value)) return;
  } else {
    panic("TODO: Unknown operator");
  }
}

void
token_dispatch_operator(
  Execution_Context *context,
  Array_Value_Ptr *stack,
  Operator_Stack_Entry *operator_entry
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Slice operator = operator_entry->source;

  u64 argument_count = operator_entry->scope_entry.argument_count;

  if (dyn_array_length(*stack) < argument_count) {
    // FIXME provide source range
    context_error_snprintf(
      context, operator_entry->source_range,
      "Operator %"PRIslice" required %"PRIu64", got %"PRIu64,
      SLICE_EXPAND_PRINTF(operator), argument_count, dyn_array_length(*stack)
    );
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
  Value *result_value = value_any(context, args_view.source_range);
  token_eval_operator(context, args_view, operator_entry, result_value);
  MASS_ON_ERROR(*context->result) return;

  // Pop off current arguments and push a new one
  dyn_array_splice_raw(*stack, start_index, argument_count, &result_value, 1);
}

Value *
token_parse_if_expression(
  Execution_Context *context,
  Value_View view,
  u64 *matched_length
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("if"));

  Value_View condition = {0};
  Value_View then_branch = {0};
  Value_View else_branch = {0};

  enum {
    Parse_State_Condition,
    Parse_State_Then,
    Parse_State_Else
  } parse_state = Parse_State_Condition;

  for (u64 i = peek_index; i < view.length; ++i) {
    Value *token = value_view_get(view, i);
    if (value_match_symbol(token, slice_literal("then"))) {
      Value_View till_here = value_view_slice(&view, peek_index, i);
      if (parse_state != Parse_State_Condition) {
        context_error_snprintf(
          context, till_here.source_range,
          "Expected `else`, encountered `then` inside an `if` expression"
        );
        goto err;
      }
      parse_state = Parse_State_Then;
      condition = till_here;
      peek_index = i + 1;
    } else if (value_match_symbol(token, slice_literal("else"))) {
      Value_View till_here = value_view_slice(&view, peek_index, i);
      if (parse_state != Parse_State_Then) {
        context_error_snprintf(
          context, till_here.source_range,
          "Expected `then`, encountered `else` inside an `if` expression"
        );
        goto err;
      }
      then_branch = till_here;
      peek_index = i + 1;
      else_branch = value_view_rest(&view, peek_index);
      parse_state = Parse_State_Else;
      break;
    } else if (value_match(token, &token_pattern_semicolon)) {
      break;
    }
  }
  if (!condition.length) {
    context_error_snprintf(
      context, view.source_range,
      "`if` keyword must be followed by an expression"
    );
    goto err;
  }
  if (!then_branch.length) {
    context_error_snprintf(
      context, view.source_range,
      "`then` branch of an if expression must not be empty"
    );
    goto err;
  }
  if (!else_branch.length) {
    context_error_snprintf(
      context, view.source_range,
      "`else` branch of an if expression must not be empty"
    );
    goto err;
  }

  Value *condition_value = value_any(context, condition.source_range);
  token_parse_expression(context, condition, condition_value, 0);
  MASS_ON_ERROR(*context->result) goto err;

  Source_Range keyword_range = keyword->source_range;
  Label_Index else_label = make_if(
    context, &context->builder->code_block.instructions, &keyword_range, condition_value
  );

  Value *if_value = value_any(context, then_branch.source_range);
  token_parse_expression(context, then_branch, if_value, 0);
  MASS_ON_ERROR(*context->result) goto err;

  if (if_value->storage.tag == Storage_Tag_Static) {
    const Descriptor *stack_descriptor = if_value->descriptor;
    if (stack_descriptor == &descriptor_number_literal) {
      stack_descriptor = &descriptor_s64;
    }
    Value *on_stack =
      reserve_stack(context, context->builder, stack_descriptor, if_value->source_range);
    MASS_ON_ERROR(assign(context, on_stack, if_value)) {
      goto err;
    }
    if_value = on_stack;
  }

  Label_Index after_label =
    make_label(context->program, &context->program->memory.sections.code, slice_literal("if end"));
  push_instruction(
    &context->builder->code_block.instructions, keyword_range,
    (Instruction) {.assembly = {jmp, {code_label32(after_label), 0, 0}}}
  );

  push_instruction(
    &context->builder->code_block.instructions, keyword_range,
    (Instruction) {.type = Instruction_Type_Label, .label = else_label}
  );

  u64 else_length = token_parse_expression(context, else_branch, if_value, 0);
  *matched_length = peek_index + else_length;

  push_instruction(
    &context->builder->code_block.instructions, keyword_range,
    (Instruction) {.type = Instruction_Type_Label, .label = after_label}
  );
  return if_value;

  err:
  return 0;
}

PRELUDE_NO_DISCARD u64
token_parse_expression(
  Execution_Context *context,
  Value_View view,
  Value *result_value,
  const Token_Pattern *end_pattern
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  if(!view.length) {
    MASS_ON_ERROR(assign(context, result_value, &void_value)) return true;
    return true;
  }

  Array_Value_Ptr value_stack = dyn_array_make(Array_Value_Ptr);
  Array_Operator_Stack_Entry operator_stack = dyn_array_make(Array_Operator_Stack_Entry);

  bool is_previous_an_operator = true;
  u64 matched_length = view.length;
  for (u64 i = 0; i < view.length; ++i) {
    Value_View rest = value_view_rest(&view, i);
    {
      // Try to match macros at the current position
      u64 macro_match_length = 0;
      Value *macro_result = token_parse_macros(context, rest, &macro_match_length);
      MASS_ON_ERROR(*context->result) goto err;
      if (macro_match_length) {
        assert(macro_result);
        dyn_array_push(value_stack, macro_result);
        // Skip over the matched slice
        i += macro_match_length - 1;
        continue;
      }
    }

    {
      u64 if_match_length = 0;
      Value *if_expression = token_parse_if_expression(context, rest, &if_match_length);
      MASS_ON_ERROR(*context->result) goto err;
      if (if_match_length) {
        assert(if_expression);
        dyn_array_push(value_stack, if_expression);
        // Skip over the matched slice
        i += if_match_length - 1;
        continue;
      }
    }

    Operator_Fixity fixity_mask = is_previous_an_operator
      ? Operator_Fixity_Prefix
      : Operator_Fixity_Infix | Operator_Fixity_Postfix;

    Value *value = value_view_get(view, i);
    if (value_is_group(value)) {
      dyn_array_push(value_stack, value);
      const Group *group = value_as_group(value);
      switch (group->tag) {
        case Group_Tag_Paren: {
          if (!is_previous_an_operator) {
            if (!token_handle_operator(
              context, view, &value_stack, &operator_stack, slice_literal("()"),
              value->source_range, Operator_Fixity_Postfix
            )) goto err;
          }
          break;
        }
        case Group_Tag_Curly: {
          // Nothing special to do for now?
          break;
        }
        case Group_Tag_Square: {
          context_error_snprintf(
            context, value->source_range,
            "Unexpected [] in an expression"
          );
          break;
        }
      }
      is_previous_an_operator = false;
    } else if (value_is_symbol(value)) {
      Slice symbol_name = value_as_symbol(value)->name;
      if (end_pattern && value_match(value, end_pattern)) {
        matched_length = i + 1;
        goto drain;
      }

      Scope_Entry *scope_entry = scope_lookup(context->scope, symbol_name);
      if (scope_entry && scope_entry->tag == Scope_Entry_Tag_Operator) {
        if (!token_handle_operator(
          context, view, &value_stack, &operator_stack,
          symbol_name, value->source_range, fixity_mask
        )) goto err;
        is_previous_an_operator = true;
      } else {
        is_previous_an_operator = false;
        dyn_array_push(value_stack, value);
      }
    } else {
      dyn_array_push(value_stack, value);
      is_previous_an_operator = false;
    }
  }

  drain:
  while (dyn_array_length(operator_stack)) {
    Operator_Stack_Entry *entry = dyn_array_pop(operator_stack);
    token_dispatch_operator(context, &value_stack, entry);
  }
  if (context->result->tag == Mass_Result_Tag_Success) {
    if (dyn_array_length(value_stack) == 1) {
      Value *value = *dyn_array_last(value_stack);
      MASS_ON_ERROR(token_force_value(context, value, result_value)) goto err;
    } else {
      context_error_snprintf(
        context, view.source_range,
        "Could not parse the expression"
      );
    }
  }

  err:

  dyn_array_destroy(value_stack);
  dyn_array_destroy(operator_stack);

  return matched_length;
}

void
token_parse_block_view(
  Execution_Context *context,
  Value_View children_view,
  Value *block_result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  if (!children_view.length) {
    MASS_ON_ERROR(assign(context, block_result_value, &void_value));
    return;
  }

  u64 match_length = 0;
  Value last_result;
  // FIXME deal with terminated vs unterminated statements
  for(u64 start_index = 0; start_index < children_view.length; start_index += match_length) {
    MASS_ON_ERROR(*context->result) return;
    Value_View rest = value_view_rest(&children_view, start_index);
    // TODO provide a better source range here
    value_init(&last_result, context->epoch, &descriptor_any, (Storage){.tag = Storage_Tag_Any}, rest.source_range);
    // Skipping over empty statements
    if (value_match(value_view_get(rest, 0), &token_pattern_semicolon)) {
      match_length = 1;
      continue;
    }
    for (
      const Scope *statement_matcher_scope = context->scope;
      statement_matcher_scope;
      statement_matcher_scope = statement_matcher_scope->parent
    ) {
      if (!dyn_array_is_initialized(statement_matcher_scope->statement_matchers)) {
        continue;
      }
      const Array_Token_Statement_Matcher *matchers = &statement_matcher_scope->statement_matchers;
      // Do a reverse iteration because we want statements that are defined later
      // to have higher precedence when parsing
      for (u64 i = dyn_array_length(*matchers) ; i > 0; --i) {
        Token_Statement_Matcher *matcher = dyn_array_get(*matchers, i - 1);
        match_length = matcher->proc(context, rest, &last_result, matcher->payload);
        MASS_ON_ERROR(*context->result) {
            return;
        }
        if (match_length) {
          if (last_result.descriptor->tag == Descriptor_Tag_Any) {
            MASS_ON_ERROR(assign(context, &last_result, &void_value));
          }
          goto check_match;
        }
      }
    }
    match_length = token_parse_expression(context, rest, &last_result, &token_pattern_semicolon);

    check_match:
    if (!match_length) {
      Value *token = value_view_get(rest, 0);
      Slice source = source_from_source_range(&token->source_range);
      context_error_snprintf(
        context, token->source_range,
        "Can not parse statement. Unexpected token %"PRIslice".",
        SLICE_EXPAND_PRINTF(source)
      );
      return;
    }
  }

  // TODO This is not optimal as we might generate extra temp values
  MASS_ON_ERROR(assign(context, block_result_value, &last_result));
}

void
token_parse_block_no_scope(
  Execution_Context *context,
  Value *block,
  Value *block_result_value
) {
  const Group *group = value_as_group(block);
  assert(group->tag == Group_Tag_Curly);

  if (!group->children.length) {
    MASS_ON_ERROR(assign(context, block_result_value, &void_value));
    return;
  }

  token_parse_block_view(context, group->children, block_result_value);
}

void
token_parse_block(
  Execution_Context *context,
  Value *block,
  Value *block_result_value
) {
  Execution_Context body_context = *context;
  Scope *block_scope = scope_make(context->allocator, context->scope);
  body_context.scope = block_scope;
  token_parse_block_no_scope(&body_context, block, block_result_value);
}

u64
token_parse_statement_using(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("using"));
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);

  Value *result = value_any(context, rest.source_range);
  token_parse_expression(context, rest, result, 0);

  if (result->descriptor != &descriptor_scope) {
    context_error_snprintf(context, rest.source_range, "Expected a scope");
    goto err;
  }

  if (result->storage.tag != Storage_Tag_Static) {
    context_error_snprintf(context, rest.source_range, "Scope for `using` must be compile-time known");
    goto err;
  }

  // This code injects a proxy scope that just uses the same data as the other
  Scope *current_scope = context->scope;
  const Scope *using_scope = storage_static_as_c_type(&result->storage, Scope);
  const Scope *common_ancestor = scope_maybe_find_common_ancestor(current_scope, using_scope);
  assert(common_ancestor);
  // TODO @Speed This is quite inefficient but I can't really think of something faster
  Scope *proxy = scope_flatten_till(context->allocator, using_scope, common_ancestor);
  proxy->parent = current_scope;
  Scope *new_scope = scope_make(context->allocator, proxy);

  // FIXME introduce a more generic mechanism for the statements to introduce a new scope
  context->scope = new_scope;

  err:
  return peek_index;
}

u64
token_parse_statement_label(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("label"));
  Token_Maybe_Match(placeholder, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("placeholder"));

  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);

  if (rest.length != 1 || !value_is_symbol(value_view_get(rest, 0))) {
    context_error_snprintf(context, rest.source_range, "Expected a label identifier identifier");
    goto err;
  }

  Value *symbol = value_view_get(rest, 0);
  Slice name = value_as_symbol(symbol)->name;

  // :ForwardLabelRef
  // First try to lookup a label that might have been declared by `goto`
  Scope_Entry *scope_entry = scope_lookup(context->scope, name);
  Value *value;
  if (scope_entry) {
    value = scope_entry_force(context, scope_entry);
  } else {
    Scope *label_scope = context->scope;

    Source_Range source_range = symbol->source_range;
    Label_Index label = make_label(context->program, &context->program->memory.sections.code, name);
    value = value_make(context, &descriptor_void, code_label32(label), source_range);
    scope_define(label_scope, name, (Scope_Entry) {
      .tag = Scope_Entry_Tag_Value,
      .Value.value = value,
      .source_range = source_range,
    });
    if (placeholder) {
      return peek_index;
    }
  }

  Source_Range keyword_range = keyword->source_range;
  if (
    value->descriptor != &descriptor_void ||
    value->storage.tag != Storage_Tag_Memory ||
    value->storage.Memory.location.tag != Memory_Location_Tag_Instruction_Pointer_Relative
  ) {
    Slice source = source_from_source_range(&keyword_range);
    context_error_snprintf(
      context, keyword_range,
      "Trying to redefine variable %"PRIslice" as a label",
      SLICE_EXPAND_PRINTF(source)
    );
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, keyword_range,
    (Instruction) {
      .type = Instruction_Type_Label,
      .label = value->storage.Memory.location.Instruction_Pointer_Relative.label_index
    }
  );

  err:
  return peek_index;
}

u64
token_parse_goto(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("goto"));
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);

  if (rest.length == 0) {
    context_error_snprintf(
      context, keyword->source_range,
      "`goto` keyword must be followed by an identifier"
    );
    goto err;
  }

  if (rest.length > 1) {
    context_error_snprintf(
      context, value_view_get(rest, 1)->source_range,
      "Unexpected token"
    );
    goto err;
  }
  Value *symbol = value_view_get(rest, 0);
  if (!value_is_symbol(symbol)) {
    context_error_snprintf(
      context, symbol->source_range,
      "`goto` keyword must be followed by an identifier"
    );
    goto err;
  }

  Slice name = value_as_symbol(symbol)->name;
  Scope_Entry *scope_entry = scope_lookup(context->scope, name);
  Value *value = scope_entry_force(context, scope_entry);

  if (
    !value ||
    value->descriptor != &descriptor_void ||
    value->storage.tag != Storage_Tag_Memory ||
    value->storage.Memory.location.tag != Memory_Location_Tag_Instruction_Pointer_Relative
  ) {
    context_error_snprintf(
      context, keyword->source_range,
      "%"PRIslice" is not a label",
      SLICE_EXPAND_PRINTF(name)
    );
    goto err;
  }

  push_instruction(
    &context->builder->code_block.instructions, keyword->source_range,
    (Instruction) {.assembly = {jmp, {value->storage, 0, 0}}}
  );

  err:
  return peek_index;
}

u64
token_parse_explicit_return(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(keyword, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("return"));
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  bool has_return_expression = rest.length > 0;

  Scope_Entry *scope_value_entry = scope_lookup(context->scope, MASS_RETURN_VALUE_NAME);
  assert(scope_value_entry);
  Value *fn_return = scope_entry_force(context, scope_value_entry);
  assert(fn_return);

  bool is_any_return = fn_return->descriptor->tag == Descriptor_Tag_Any;
  token_parse_expression(context, rest, fn_return, 0);

  // FIXME with inline functions and explicit returns we can end up with multiple immediate
  //       values that are trying to be moved in the same return value
  if (is_any_return) {
    Descriptor *stack_descriptor = fn_return->descriptor == &descriptor_number_literal
      ? &descriptor_s64
      : fn_return->descriptor;
    Value *stack_return =
      reserve_stack(context, context->builder, stack_descriptor, fn_return->source_range);
    MASS_ON_ERROR(assign(context, stack_return, fn_return)) return true;
    *fn_return = *stack_return;
  }

  bool is_void = fn_return->descriptor->tag == Descriptor_Tag_Void;
  if (!is_void && !has_return_expression) {
    context_error_snprintf(
      context, fn_return->source_range,
      "Explicit return from a non-void function requires a value"
    );
  }

  Scope_Entry *scope_label_entry = scope_lookup(context->scope, MASS_RETURN_LABEL_NAME);
  assert(scope_label_entry);
  Value *return_label = scope_entry_force(context, scope_label_entry);
  assert(return_label);
  assert(return_label->descriptor == &descriptor_void);
  assert(storage_is_label(&return_label->storage));

  push_instruction(
    &context->builder->code_block.instructions,
    fn_return->source_range,
    (Instruction) {.assembly = {jmp, {return_label->storage, 0, 0}}}
  );

  return peek_index;
}

Descriptor *
token_match_fixed_array_type(
  Execution_Context *context,
  Value_View view
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(type, .tag = Token_Pattern_Tag_Symbol);
  Token_Match(square_brace, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Square);
  const Descriptor *descriptor = scope_lookup_type(
    context, context->scope, type->source_range, value_as_symbol(type)->name
  );

  Value_View size_view = value_as_group(square_brace)->children;
  Value *size_value = value_any(context, size_view.source_range);
  compile_time_eval(context, size_view, size_value);
  size_value = token_value_force_immediate_integer(
    context, &size_view.source_range, size_value, &descriptor_u32
  );
  MASS_ON_ERROR(*context->result) return 0;
  u32 length = u64_to_u32(storage_static_value_up_to_u64(&size_value->storage));

  // TODO extract into a helper
  Descriptor *array_descriptor = allocator_allocate(context->allocator, Descriptor);
  *array_descriptor = (Descriptor) {
    .tag = Descriptor_Tag_Fixed_Size_Array,
    .name = source_from_source_range(&view.source_range),
    .Fixed_Size_Array = {
      .item = descriptor,
      .length = length,
    },
  };
  return array_descriptor;
}

u64
token_parse_inline_machine_code_bytes(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  u64 peek_index = 0;
  Token_Match(id_token, .tag = Token_Pattern_Tag_Symbol, .Symbol.name = slice_literal("inline_machine_code_bytes"));
  // TODO improve error reporting and / or transition to compile time functions when available
  Token_Match(args_token, .tag = Token_Pattern_Tag_Group, .Group.tag = Group_Tag_Paren);
  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  if (rest.length) {
    context_error_snprintf(
      context, rest.source_range,
      "Expected the end of the statement"
    );
    goto err;
  }

  Array_Value_Ptr args = token_match_call_arguments(context, args_token);

  Instruction_Bytes bytes = {
    .label_offset_in_instruction = INSTRUCTION_BYTES_NO_LABEL,
  };

  for (u64 i = 0; i < dyn_array_length(args); ++i) {
    if (bytes.length >= 15) {
      context_error_snprintf(
        context, args_token->source_range,
        "Expected a maximum of 15 bytes"
      );
      goto err;
    }
    Value *value = *dyn_array_get(args, i);
    if (!value) continue;
    if (storage_is_label(&value->storage)) {
      if (bytes.label_offset_in_instruction != INSTRUCTION_BYTES_NO_LABEL) {
        context_error_snprintf(
          context, value->source_range,
          "inline_machine_code_bytes only supports one label"
        );
        goto err;
      }
      bytes.label_index = value->storage.Memory.location.Instruction_Pointer_Relative.label_index;
      bytes.label_offset_in_instruction = u64_to_u8(i);
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
      bytes.memory[bytes.length++] = 0;
    } else {
      value = token_value_force_immediate_integer(
        context, &value->source_range, value, &descriptor_u8
      );
      u8 byte = u64_to_u8(storage_static_value_up_to_u64(&value->storage));
      bytes.memory[bytes.length++] = s64_to_u8(byte);
    }
  }

  push_instruction(
    &context->builder->code_block.instructions, id_token->source_range,
    (Instruction) {
      .type = Instruction_Type_Bytes,
      .Bytes = bytes,
     }
  );

  err:
  return peek_index;
}

u64
token_parse_definition(
  Execution_Context *context,
  Value_View view,
  Value *result_value
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .tag = Token_Pattern_Tag_Symbol);
  Token_Match_Operator(define, ":");

  Value_View rest = value_view_match_till_end_of_statement(view, &peek_index);
  const Descriptor *descriptor = token_match_type(context, rest);
  MASS_ON_ERROR(*context->result) goto err;
  Source_Range name_range = name->source_range;
  Value *value = reserve_stack(context, context->builder, descriptor, name_range);
  scope_define(context->scope, value_as_symbol(name)->name, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = value,
    .source_range = name_range,
  });
  MASS_ON_ERROR(assign(context, result_value, value));

  err:
  return peek_index;
}

u64
token_parse_definitions(
  Execution_Context *context,
  Value_View state,
  Value *value_result,
  void *unused_payload
) {
  if (context->result->tag != Mass_Result_Tag_Success) return 0;

  return token_parse_definition(context, state, value_result);
}

void
token_define_global_variable(
  Execution_Context *context,
  Value *symbol,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = value_any(context, symbol->source_range);
  compile_time_eval(context, expression, value);
  MASS_ON_ERROR(*context->result) return;

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (value->descriptor == &descriptor_number_literal) {
    value = token_value_force_immediate_integer(
      context, &expression.source_range, value, &descriptor_s64
    );
  }

  Value *global_value;
  if (storage_is_label(&value->storage)) {
    global_value = value;
  } else {
    assert(value->storage.tag == Storage_Tag_Static);

    if (value->descriptor->tag == Descriptor_Tag_Function) {
      global_value = 0;
      panic("TODO implement when relocations are available");
      //Descriptor *fn_pointer = descriptor_pointer_to(context->allocator, value->descriptor);
      //ensure_compiled_function_body(context, value);
      //global_value = value_global(context, fn_pointer, rhs.source_range);
      //load_address(context, &view.source_range, on_stack, value);
    } else {
      Section *section = &context->program->memory.sections.rw_data;
      u64 byte_size = descriptor_byte_size(value->descriptor);
      u64 alignment = descriptor_alignment(value->descriptor);

      // TODO this should also be deduped
      Label_Index label_index = allocate_section_memory(context, section, byte_size, alignment);
      global_value = value_make(
        context, value->descriptor, data_label32(label_index, byte_size), value->source_range
      );

      void *section_memory = rip_value_pointer_from_label_index(context->program, label_index);
      memcpy(section_memory, storage_static_as_c_type_internal(&value->storage, byte_size), byte_size);
    }
  }

  scope_define(context->scope, value_as_symbol(symbol)->name, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = global_value,
    .source_range = symbol->source_range,
  });
}

void
token_define_local_variable(
  Execution_Context *context,
  Value *symbol,
  Value_View expression
) {
  if (context->result->tag != Mass_Result_Tag_Success) return;

  Value *value = value_any(context, symbol->source_range);
  token_parse_expression(context, expression, value, 0);
  MASS_ON_ERROR(*context->result) return;

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (value->descriptor == &descriptor_number_literal) {
    value = token_value_force_immediate_integer(
      context, &expression.source_range, value, &descriptor_s64
    );
  }
  Value *on_stack;
  if (value->descriptor->tag == Descriptor_Tag_Function) {
    Descriptor *fn_pointer = descriptor_pointer_to(context->allocator, value->descriptor);
    ensure_compiled_function_body(context, value);
    on_stack = reserve_stack(context, context->builder, fn_pointer, symbol->source_range);
    load_address(context, &symbol->source_range, on_stack, value);
  } else {
    on_stack = reserve_stack(context, context->builder, value->descriptor, symbol->source_range);
    MASS_ON_ERROR(assign(context, on_stack, value)) return;
  }

  scope_define(context->scope, value_as_symbol(symbol)->name, (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = on_stack,
    .source_range = symbol->source_range,
  });
}

u64
token_parse_definition_and_assignment_statements(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
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
  // For now we support only single ID on the left
  if (lhs.length > 1) {
    panic("TODO user error");
    goto err;
  }
  Value *name_token = value_view_get(view, 0);

  if (!value_is_symbol(name_token)) {
    panic("TODO user error");
    goto err;
  }

  if (context->builder) {
    token_define_local_variable(context, name_token, rhs);
  } else {
    token_define_global_variable(context, name_token, rhs);
  }

  err:
  return statement_length;
}

u64
token_parse_assignment(
  Execution_Context *context,
  Value_View view,
  Value *unused_result,
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

  Value *target = value_any(context, lhs.source_range);
  if (!token_parse_definition(context, lhs, target)) {
    token_parse_expression(context, lhs, target, 0);
  }
  token_parse_expression(context, rhs, target, 0);

  return statement_length;
}

PRELUDE_NO_DISCARD Mass_Result
token_parse(
  Execution_Context *context,
  Value_View view
) {
  token_parse_block_view(context, view, &void_value);
  return *context->result;
}

void
scope_define_builtins(
  const Allocator *allocator,
  Scope *scope
) {
  scope_define(scope, slice_literal("()"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 20, .fixity = Operator_Fixity_Postfix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("@"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = 20,
      .fixity = Operator_Fixity_Prefix,
      .associativity = Operator_Associativity_Right,
      .argument_count = 1
     }
  });
  scope_define(scope, slice_literal("."), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 19, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("->"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = 19,
      .fixity = Operator_Fixity_Infix,
      .associativity = Operator_Associativity_Right,
      .argument_count = 3
    }
  });
  scope_define(scope, slice_literal("macro"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = 19,
      .associativity = Operator_Associativity_Right,
      .fixity = Operator_Fixity_Prefix,
      .argument_count = 1
    }
  });

  scope_define(scope, slice_literal("-"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = {
      .precedence = 17,
      .handler = token_handle_negation,
      .argument_count = 1,
      .fixity = Operator_Fixity_Prefix
    }
  });

  scope_define(scope, slice_literal("*"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("/"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("%"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 15, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });

  scope_define(scope, slice_literal("+"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 10, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("-"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 10, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("<"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal(">"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("<="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal(">="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 8, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });

  scope_define(scope, slice_literal("=="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 7, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("!="), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 7, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("&&"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 5, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });
  scope_define(scope, slice_literal("||"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Operator,
    .Operator = { .precedence = 4, .fixity = Operator_Fixity_Infix, .argument_count = 2 }
  });


  scope_define(scope, slice_literal("any"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_any_value
  });

  scope_define(scope, slice_literal("Register_8"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_8_value
  });
  scope_define(scope, slice_literal("Register_16"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_16_value
  });
  scope_define(scope, slice_literal("Register_32"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_32_value
  });
  scope_define(scope, slice_literal("Register_64"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_register_64_value
  });

  scope_define(scope, slice_literal("External_Symbol"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_external_symbol_value
  });

  scope_define(scope, slice_literal("String"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_slice_value
  });

  scope_define(scope, slice_literal("Scope"), (Scope_Entry) {
    .tag = Scope_Entry_Tag_Value,
    .Value.value = type_scope_value
  });

  #define MASS_PROCESS_BUILT_IN_TYPE(_NAME_, _BIT_SIZE_)\
    scope_define(scope, slice_literal(#_NAME_), (Scope_Entry) {\
      .tag = Scope_Entry_Tag_Value,\
      .Value.value = type_##_NAME_##_value\
    });
  MASS_ENUMERATE_BUILT_IN_TYPES
  #undef MASS_PROCESS_BUILT_IN_TYPE

  #define MASS_FN_ARG_ANY_OF_TYPE(_NAME_, _DESCRIPTOR_)\
    {\
      .name = slice_literal_fields(_NAME_),\
      .value = value_init(\
        allocator_allocate(allocator, Value),\
        VALUE_STATIC_EPOCH, (_DESCRIPTOR_), storage_none, (Source_Range){0}\
      )\
    }

  #define MASS_DEFINE_COMPILE_TIME_FUNCTION(_FN_, _NAME_, _RETURN_DESCRIPTOR_, ...)\
  {\
    Function_Argument raw_arguments[] = {__VA_ARGS__};\
    u64 arg_length = countof(raw_arguments);\
    Array_Function_Argument arguments =\
      dyn_array_make(Array_Function_Argument, .allocator = allocator, .capacity = arg_length);\
    for (u64 i = 0; i < arg_length; ++i) {\
      dyn_array_push(arguments, raw_arguments[i]);\
    }\
    Descriptor *descriptor = allocator_allocate(allocator, Descriptor);\
    *descriptor = (Descriptor) {\
      .tag = Descriptor_Tag_Function,\
      .name = slice_literal(_NAME_),\
      .Function.info = {\
        .flags = Descriptor_Function_Flags_Compile_Time,\
        .arguments = arguments,\
        .returns = {\
          .descriptor = (_RETURN_DESCRIPTOR_),\
        }\
      },\
    };\
    Value *value = value_init(\
      allocator_allocate(allocator, Value),\
      VALUE_STATIC_EPOCH, descriptor, imm64((u64)_FN_), (Source_Range){0}\
    );\
    scope_define(scope, slice_literal(_NAME_), (Scope_Entry) {\
      .tag = Scope_Entry_Tag_Value,\
      .Value.value = value,\
    });\
  }

  #define MASS_PROCESS_BUILT_IN_TYPE(_TYPE_, _BIT_SIZE)\
    MASS_DEFINE_COMPILE_TIME_FUNCTION(\
      mass_##_TYPE_##_logical_shift_left, "logical_shift_left", &descriptor_##_TYPE_,\
      MASS_FN_ARG_ANY_OF_TYPE("number", &descriptor_##_TYPE_),\
      MASS_FN_ARG_ANY_OF_TYPE("shift", &descriptor_u64)\
    )
  MASS_ENUMERATE_INTEGER_TYPES
  #undef MASS_PROCESS_BUILT_IN_TYPE

  #define MASS_PROCESS_BUILT_IN_TYPE(_TYPE_, _BIT_SIZE)\
    MASS_DEFINE_COMPILE_TIME_FUNCTION(\
      mass_##_TYPE_##_bitwise_and, "bitwise_and", &descriptor_##_TYPE_,\
      MASS_FN_ARG_ANY_OF_TYPE("a", &descriptor_##_TYPE_),\
      MASS_FN_ARG_ANY_OF_TYPE("b", &descriptor_##_TYPE_)\
    )
  MASS_ENUMERATE_INTEGER_TYPES
  #undef MASS_PROCESS_BUILT_IN_TYPE

  #define MASS_PROCESS_BUILT_IN_TYPE(_TYPE_, _BIT_SIZE)\
    MASS_DEFINE_COMPILE_TIME_FUNCTION(\
      mass_##_TYPE_##_bitwise_or, "bitwise_or", &descriptor_##_TYPE_,\
      MASS_FN_ARG_ANY_OF_TYPE("a", &descriptor_##_TYPE_),\
      MASS_FN_ARG_ANY_OF_TYPE("b", &descriptor_##_TYPE_)\
    )
  MASS_ENUMERATE_INTEGER_TYPES
  #undef MASS_PROCESS_BUILT_IN_TYPE

  MASS_DEFINE_COMPILE_TIME_FUNCTION(
    mass_compiler_external, "external", &descriptor_external_symbol,
    MASS_FN_ARG_ANY_OF_TYPE("library_name", &descriptor_slice),
    MASS_FN_ARG_ANY_OF_TYPE("symbol_name", &descriptor_slice)
  );

  MASS_DEFINE_COMPILE_TIME_FUNCTION(
    mass_import, "mass_import", &descriptor_scope,
    MASS_FN_ARG_ANY_OF_TYPE("context", &descriptor_execution_context),
    MASS_FN_ARG_ANY_OF_TYPE("module_path", &descriptor_slice)
  );

  MASS_DEFINE_COMPILE_TIME_FUNCTION(
    mass_bit_type, "bit_type", &descriptor_type,
    MASS_FN_ARG_ANY_OF_TYPE("bit_size", &descriptor_u64),
  );

  {
    Array_Token_Statement_Matcher matchers =
      dyn_array_make(Array_Token_Statement_Matcher, .allocator = allocator);

    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_constant_definitions});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_goto});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_explicit_return});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_definitions});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_definition_and_assignment_statements});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_assignment});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_inline_machine_code_bytes});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_statement_label});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_statement_using});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_syntax_definition});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_operator_definition});
    dyn_array_push(matchers, (Token_Statement_Matcher){token_parse_exports});

    scope->statement_matchers = matchers;
  }
}

Mass_Result
program_parse(
  Execution_Context *context
) {
  assert(context->module);
  Value_View tokens;

  MASS_TRY(tokenize(context->compilation, &context->module->source_file, &tokens));
  Value_View program_value_view = tokens;
  MASS_TRY(token_parse(context, program_value_view));
  return *context->result;
}

Fixed_Buffer *
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

void
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

Module *
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
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) {
    context_error_snprintf(
      context, (Source_Range){0}, "Unable to open the file %"PRIslice, SLICE_EXPAND_PRINTF(file_path)
    );
    return 0;
  }

  Module *module = allocator_allocate(context->allocator, Module);
  program_module_init(module, file_path, fixed_buffer_as_slice(buffer), scope);
  return module;
}

Mass_Result
program_import_module(
  Execution_Context *context,
  Module *module
) {
  MASS_TRY(*context->result);
  Execution_Context import_context = *context;
  import_context.module = module;
  import_context.scope = module->own_scope;
  Mass_Result parse_result = program_parse(&import_context);
  MASS_TRY(parse_result);
  if (module->export_scope && module->export_scope->map) {
    for (u64 i = 0; i < module->export_scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &module->export_scope->map->entries[i];
      if (!entry->occupied) continue;
      if (!module->own_scope->map || !hash_map_has(module->own_scope->map, entry->key)) {
        context_error_snprintf(
          context, entry->value->source_range,
          "Trying to export a missing declaration %"PRIslice, SLICE_EXPAND_PRINTF(entry->key)
        );
        break;
      }
    }
  }
  return *context->result;
}

