#include "prelude.h"
#include "source.h"
#include "function.h"

Source_Range
source_range_from_token_matcher_state(
  const Token_Matcher_State *state,
  u64 delta
) {
  Token *first = *dyn_array_get(state->tokens, state->start_index);
  Token *last = *dyn_array_get(state->tokens, state->start_index + delta - 1);
  assert(first->source_range.file == last->source_range.file);
  return (Source_Range) {
    .file = first->source_range.file,
    .offsets = {
      .from = first->source_range.offsets.from,
      .to = last->source_range.offsets.to,
    },
  };
}

Scope *
scope_make(
  Scope *parent
) {
  Scope *scope = temp_allocate(Scope);
  *scope = (Scope) {
    .parent = parent,
    .map = hash_map_make(Scope_Map),
  };
  return scope;
}

void
scope_print_names(
  Scope *scope
) {
  while (scope) {
    for (u64 i = 0; i < scope->map->capacity; ++i) {
      Scope_Map__Entry *entry = &scope->map->entries[i];
      if (entry->occupied) {
        slice_print(entry->key);
        printf(" ; ");
      }
    }
    scope = scope->parent;
  }
  printf("\n");
}

Array_Scope_Entry *
scope_lookup(
  Scope *scope,
  Slice name
) {
  while (scope) {
    Array_Scope_Entry *result = hash_map_get(scope->map, name);
    if (result) return result;
    scope = scope->parent;
  }
  return 0;
}

Token *
token_rewrite_constant_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
);

void
token_force_value(
  Compilation_Context *context,
  Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
);

Value *
scope_lookup_force(
  Compilation_Context *context,
  Scope *scope,
  Slice name,
  Function_Builder *builder
) {
  Array_Scope_Entry *entries = 0;
  while (scope) {
    entries = hash_map_get(scope->map, name);
    if (entries) break;
    scope = scope->parent;
  }
  if (!entries) {
    return 0;
  }
  assert(dyn_array_length(*entries));

  // Force lazy entries
  for (u64 i = 0; i < dyn_array_length(*entries); ++i) {
    Scope_Entry *entry = dyn_array_get(*entries, i);
    if (entry->type == Scope_Entry_Type_Lazy_Constant_Expression) {
      Token_Matcher_State state = {entry->lazy_constant_expression};
      Token *token = token_rewrite_constant_expression(context, &state, scope, builder);
      Value *result = 0;
      if (token) {
        result = value_any();
        token_force_value(context, token, scope, builder, result);
      }
      *entry = (Scope_Entry) {
        .type = Scope_Entry_Type_Value,
        .value = result,
      };
    }
  }



  Value *result = 0;
  if (dyn_array_length(*entries) == 1) {
    Scope_Entry *entry = dyn_array_get(*entries, 0);
    assert(entry->type == Scope_Entry_Type_Value);
    result = entry->value;
  } else {
    // Must be a function overload
    for (u64 i = 0; i < dyn_array_length(*entries); ++i) {
      Scope_Entry *entry = dyn_array_get(*entries, i);
      assert(entry->type == Scope_Entry_Type_Value);
      if (!result) {
        result = entry->value;
      } else {
        Value *overload = entry->value;
        overload->descriptor->function.next_overload = result;
        result = overload;
      }
      if (result->descriptor->type != Descriptor_Type_Function) {
        panic("Only functions should be lazy values");
      }
    }
  }
  // For functions we need to gather up overloads from all parent scopes
  if (result && result->descriptor->type == Descriptor_Type_Function) {
    Value *last = result;
    Scope *parent = scope;
    for (;;) {
      parent = parent->parent;
      if (!parent) break;
      if (!hash_map_has(parent->map, name)) continue;
      Value *overload = scope_lookup_force(context, parent, name, builder);
      if (!overload) panic("Just checked that hash map has the name so lookup must succeed");
      if (overload->descriptor->type != Descriptor_Type_Function) {
        panic("There should only be function overloads");
      }
      while (last->descriptor->function.next_overload) {
        last = last->descriptor->function.next_overload;
      }
      last->descriptor->function.next_overload = overload;
    };
  }
  return result;
}

static inline void
scope_define_internal(
  Scope *scope,
  Slice name,
  Scope_Entry entry
) {
  if (!hash_map_has(scope->map, name)) {
    hash_map_set(scope->map, name, dyn_array_make(Array_Scope_Entry));
  }
  Array_Scope_Entry *entries = hash_map_get(scope->map, name);
  dyn_array_push(*entries, entry);
}

void
scope_define_value(
  Scope *scope,
  Slice name,
  Value *value
) {
  scope_define_internal(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Value,
    .value = value,
  });
}

void
scope_define_lazy(
  Scope *scope,
  Slice name,
  Array_Token_Ptr tokens
) {
  scope_define_internal(scope, name, (Scope_Entry) {
    .type = Scope_Entry_Type_Lazy_Constant_Expression,
    .lazy_constant_expression = tokens,
  });
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

Tokenizer_Result
tokenize(
  Source_File *file
) {
  Array_Token_Ptr parent_stack = dyn_array_make(Array_Token_Ptr);
  Token *root = temp_allocate(Token);
  root->type = Token_Type_Module;
  root->children = dyn_array_make(Array_Token_Ptr);
  root->source_range = (Source_Range){
    .file = file,
    .offsets = {.from = 0, .to = file->text.length},
  };
  root->source = slice_sub_range(file->text, root->source_range.offsets);

  assert(!dyn_array_is_initialized(file->lines));
  file->lines = dyn_array_make(Array_Range_u64);

  Range_u64 current_line = {0};
  Tokenizer_State state = Tokenizer_State_Default;
  Token *current_token = 0;
  Token *parent = root;
  Fixed_Buffer *string_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = 4096,
  );

  Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);

#define start_token(_type_)\
  do {\
    current_token = temp_allocate(Token);\
    *current_token = (Token) {\
      .type = (_type_),\
      .source_range = {\
        .file = file,\
        .offsets = {.from = i, .to = i},\
      }\
    };\
  } while(0)

#define do_push\
  do {\
    current_token->source = \
      slice_sub_range(file->text, current_token->source_range.offsets);\
    dyn_array_push(parent->children, current_token);\
    current_token = 0;\
    state = Tokenizer_State_Default;\
  } while(0)

#define reject_and_push\
  do {\
    current_token->source_range.offsets.to = i;\
    do_push;\
  } while(0)

#define accept_and_push\
  do {\
    current_token->source_range.offsets.to = i + 1;\
    do_push;\
  } while(0)

#define push_error(_MESSAGE_)\
  dyn_array_push(errors, (Parse_Error) {\
    .message = slice_literal(_MESSAGE_),\
    .source_range = {\
      .file = file,\
      .offsets = {.from = i, .to = i},\
    }\
  })
#define push_line()\
  do {\
    current_line.to = i + 1;\
    dyn_array_push(file->lines, current_line);\
    current_line.from = current_line.to;\
  } while(0)

  u64 i = 0;
  for (; i < file->text.length; ++i) {
    s8 ch = file->text.bytes[i];
    s8 peek = i + 1 < file->text.length ? file->text.bytes[i + 1] : 0;

    retry: switch(state) {
      case Tokenizer_State_Default: {
        if (ch == '\n') {
          start_token(Token_Type_Newline);
          push_line();
          accept_and_push;
        } else if (ch == '\r') {
          start_token(Token_Type_Newline);
          if (peek == '\n') i++;
          push_line();
          accept_and_push;
        } else if (isspace(ch)) {
          continue;
        } else if (ch == '0' && peek == 'x') {
          start_token(Token_Type_Hex_Integer);
          i++;
          state = Tokenizer_State_Hex_Integer;
        } else if (isdigit(ch)) {
          start_token(Token_Type_Integer);
          state = Tokenizer_State_Integer;
        } else if (isalpha(ch) || ch == '_') {
          start_token(Token_Type_Id);
          state = Tokenizer_State_Id;
        } else if(ch == '/' && peek == '/') {
          state = Tokenizer_State_Single_Line_Comment;
        } else if (code_point_is_operator(ch)) {
          start_token(Token_Type_Operator);
          state = Tokenizer_State_Operator;
        } else if (ch == '"') {
          string_buffer->occupied = 0;
          start_token(Token_Type_String);
          state = Tokenizer_State_String;
        } else if (ch == '(' || ch == '{' || ch == '[') {
          Token_Type type =
            ch == '(' ? Token_Type_Paren :
            ch == '{' ? Token_Type_Curly :
            Token_Type_Square;
          start_token(type);
          current_token->children = dyn_array_make(Array_Token_Ptr, 4);
          dyn_array_push(parent->children, current_token);
          dyn_array_push(parent_stack, parent);
          parent = current_token;
        } else if (ch == ')' || ch == '}' || ch == ']') {
          s8 expected_paren = 0;

          switch (parent->type) {
            case Token_Type_Paren: {
              expected_paren = ')';
              break;
            }
            case Token_Type_Curly: {
              expected_paren = '}';
              break;
            }
            case Token_Type_Square: {
              expected_paren = ']';
              break;
            }
            case Token_Type_Newline:
            case Token_Type_Value:
            case Token_Type_Id:
            case Token_Type_Integer:
            case Token_Type_Hex_Integer:
            case Token_Type_Operator:
            case Token_Type_String:
            case Token_Type_Module: {
              panic("Tokenizer: unexpected closing char for group");
              break;
            }
          }
          if (ch != expected_paren) {
            push_error("Mismatched closing brace");
            goto end;
          }
          parent->source_range.offsets.to = i + 1;
          parent->source = slice_sub_range(file->text, parent->source_range.offsets);
          if (!dyn_array_length(parent_stack)) {
            push_error("Encountered a closing brace without a matching open one");
            goto end;
          }
          parent = *dyn_array_pop(parent_stack);
          current_token = 0;
        } else {
          push_error("Unpexpected input");
          goto end;
        }
        break;
      }
      case Tokenizer_State_Integer: {
        if (!isdigit(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Hex_Integer: {
        if (!code_point_is_hex_digit(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Id: {
        if (!(isalpha(ch) || isdigit(ch) || ch == '_')) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_Operator: {
        if (!code_point_is_operator(ch)) {
          reject_and_push;
          goto retry;
        }
        break;
      }
      case Tokenizer_State_String: {
        if (ch == '\\') {
          state = Tokenizer_State_String_Escape;
        } else if (ch == '"') {
          u8 *string = allocator_allocate_bytes(temp_allocator, string_buffer->occupied, 1);
          memcpy(string, string_buffer->memory, string_buffer->occupied);
          current_token->string = (Slice){string, string_buffer->occupied};
          accept_and_push;
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

  current_line.to = file->text.length;
  dyn_array_push(file->lines, current_line);

  if (parent != root) {
    push_error("Unexpected end of file. Expected a closing brace.");
  }
  // current_token can be null in case of an empty input
  if (current_token) {
    // Strings need to be terminated with a '"'
    if (state == Tokenizer_State_String) {
      push_error("Unexpected end of file. Expected a \".");
    } else {
      accept_and_push;
    }
  }
  end:
#undef push_error
#undef start_token
#undef push_and_retry
  fixed_buffer_destroy(string_buffer);
  dyn_array_destroy(parent_stack);
  if (dyn_array_length(errors)) {
    return (Tokenizer_Result){.type = Tokenizer_Result_Type_Error, .errors = errors};
  }
  return (Tokenizer_Result){.type = Tokenizer_Result_Type_Success, .root = root};
}

Token *
token_peek(
  Token_Matcher_State *state,
  u64 delta
) {
  u64 index = state->start_index + delta;
  Token **peek = dyn_array_peek(state->tokens, index);
  if (!peek) return 0;
  return *peek;
}

bool
token_match(
  const Token *token,
  const Token_Pattern *pattern
) {
  if (pattern->type && pattern->type != token->type) return false;
  if (pattern->source.length) {
    return slice_equal(token->source, pattern->source);
  }
  return true;
}

Token *
token_peek_match(
  Token_Matcher_State *state,
  u64 delta,
  const Token_Pattern *pattern
) {
  Token *token = token_peek(state, delta);
  if (!token) return 0;
  if (!token_match(token, pattern)) return 0;
  return token;
}

Array_Token_Matcher_State
token_split(
  Array_Token_Ptr tokens,
  const Token_Pattern *separator
) {
  Array_Token_Matcher_State result = dyn_array_make(Array_Token_Matcher_State);

  Array_Token_Ptr sequence = dyn_array_make(Array_Token_Ptr);
  for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
    Token *token = *dyn_array_get(tokens, i);
    if (token_match(token, separator)) {
      dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
      sequence = dyn_array_make(Array_Token_Ptr);
    } else {
      dyn_array_push(sequence, token);
    }
  }
  dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
  return result;
}

Array_Token_Matcher_State
token_split_by_newlines_and_semicolons(
  Array_Token_Ptr tokens
) {
  Array_Token_Matcher_State result = dyn_array_make(Array_Token_Matcher_State);

  Array_Token_Ptr sequence = dyn_array_make(Array_Token_Ptr);
  for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
    Token *token = *dyn_array_get(tokens, i);
    if (
      token->type == Token_Type_Newline ||
      (token->type == Token_Type_Operator && slice_equal(token->source, slice_literal(";")))
    ) {
      if (dyn_array_length(sequence)) {
        dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
        sequence = dyn_array_make(Array_Token_Ptr);
      }
    } else {
      dyn_array_push(sequence, token);
    }
  }
  dyn_array_push(result, (Token_Matcher_State) { .tokens = sequence });
  return result;
}

Descriptor *
scope_lookup_type(
  Compilation_Context *context,
  Scope *scope,
  Source_Range source_range,
  Slice type_name,
  Function_Builder *builder
) {
  Value *value = scope_lookup_force(context, scope, type_name, builder);
  if (!value) return 0;
  if (value->descriptor->type != Descriptor_Type_Type) {
    program_error_builder(context->program, source_range) {
      program_error_append_slice(type_name);
      program_error_append_literal(" is not a type");
    }
    return 0;
  }
  Descriptor *descriptor = value->descriptor->type_descriptor;
  return descriptor;
}

#define Token_Maybe_Match(_id_, ...)\
  Token *(_id_) = token_peek_match(state, peek_index, &(Token_Pattern) { __VA_ARGS__ });\
  if (_id_) (++peek_index)

#define Token_Match(_id_, ...)\
  Token_Maybe_Match(_id_, __VA_ARGS__);\
  if (!(_id_)) return 0

#define Token_Match_Operator(_id_, _op_)\
  Token_Match(_id_, .type = Token_Type_Operator, .source = slice_literal(_op_))

#define Token_Match_End()\
  if(peek_index != dyn_array_length(state->tokens)) return 0

#define TOKEN_MATCHED_SOURCE()\
  source_range_from_token_matcher_state(state, peek_index)

typedef struct {
  Slice arg_name;
  Descriptor *type_descriptor;
} Token_Match_Arg;

Descriptor *
token_force_type(
  Compilation_Context *context,
  Scope *scope,
  Token *token,
  Function_Builder *builder
) {
  Descriptor *descriptor = 0;
  switch (token->type) {
    case Token_Type_Id: {
      descriptor =
        scope_lookup_type(context, scope, token->source_range, token->source, builder);
      if (!descriptor) {
        program_error_builder(context->program, token->source_range) {
          program_error_append_literal("Could not find type ");
          program_error_append_slice(token->source);
        }
        return 0;
      }
      break;
    }
    case Token_Type_Square: {
      if (dyn_array_length(token->children) != 1) {
        program_push_error_from_slice(
          context->program,
          token->source_range,
          slice_literal("Pointer type must have a single type inside")
        );
        return 0;
      }
      Token *child = *dyn_array_get(token->children, 0);
      if (child->type != Token_Type_Id) {
        panic("TODO: should be recursive");
      }
      descriptor = temp_allocate(Descriptor);
      *descriptor = (Descriptor) {
        .type = Descriptor_Type_Pointer,
        .pointer_to =
          scope_lookup_type(context, scope, child->source_range, child->source, builder),
      };
      break;
    }
    case Token_Type_Hex_Integer:
    case Token_Type_Integer: {
      program_error_builder(context->program, token->source_range) {
        program_error_append_slice(token->source);
        program_error_append_literal(" is not a type");
      }
      return 0;
    }
    case Token_Type_Newline: {
      program_push_error_from_slice(
        context->program,
        token->source_range,
        slice_literal("Unexpected newline token")
      );
      return 0;
    }
    case Token_Type_Operator:
    case Token_Type_String:
    case Token_Type_Paren:
    case Token_Type_Curly:
    case Token_Type_Module:
    case Token_Type_Value:
    default: {
      panic("TODO");
      break;
    }
  }
  return descriptor;
}

typedef Array_Token_Ptr (*token_pattern_callback)(
  Compilation_Context *context,
  Array_Token_Ptr match,
  Scope *scope,
  Function_Builder *builder_
);

Array_Token_Ptr
token_match_pattern(
  Token_Matcher_State *state,
  Array_Token_Pattern pattern_array
) {
  u64 pattern_length = dyn_array_length(pattern_array);
  if (!pattern_length) panic("Zero-length pattern does not make sense");
  for (u64 i = 0; i < pattern_length; ++i) {
    Token_Pattern *pattern = dyn_array_get(pattern_array, i);
    Token *token = token_peek_match(state, i, pattern);
    if (!token) return (Array_Token_Ptr){0};
  }
  Array_Token_Ptr match = dyn_array_make(Array_Token_Ptr, .capacity = pattern_length);
  for (u64 i = 0; i < pattern_length; ++i) {
    Token *token = token_peek(state, i);
    dyn_array_push(match, token);
  }
  return match;
}

inline void
token_replace_length_with_tokens(
  Token_Matcher_State *state,
  u64 pattern_length,
  Array_Token_Ptr replacement
) {
  dyn_array_splice(state->tokens, state->start_index, pattern_length, replacement);
}

bool
token_rewrite_pattern(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Array_Token_Pattern pattern,
  token_pattern_callback callback
) {
  Array_Token_Ptr match = token_match_pattern(state, pattern);
  if (!dyn_array_is_initialized(match)) return false;
  Array_Token_Ptr replacement = callback(context, match, scope, builder);
  token_replace_length_with_tokens(state, dyn_array_length(pattern), replacement);
  dyn_array_destroy(match);
  return true;
}

Array_Token_Ptr
token_clone_token_array_deep(
  Array_Token_Ptr source
);

Token *
token_clone_deep(
  Token *token
) {
  Token *clone = temp_allocate(Token);
  *clone = *token;
  switch (token->type) {
    case Token_Type_Newline:
    case Token_Type_Integer:
    case Token_Type_Hex_Integer:
    case Token_Type_Operator:
    case Token_Type_String:
    case Token_Type_Id: {
      // Nothing to do
      break;
    }
    case Token_Type_Square:
    case Token_Type_Paren:
    case Token_Type_Curly:
    case Token_Type_Module: {
      clone->children = token_clone_token_array_deep(token->children);
      break;
    }
    case Token_Type_Value: {
      panic("Macro definitions should not contain semi-resolved tokens");
      break;
    }
  }
  return clone;
}

Array_Token_Ptr
token_clone_token_array_deep(
  Array_Token_Ptr source
) {
  Array_Token_Ptr result = dyn_array_make(Array_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    Token *token = *dyn_array_get(source, i);
    Token *clone = token_clone_deep(token);
    dyn_array_push(result, clone);
  }
  return result;
}

Array_Token_Ptr
token_apply_macro_replacements(
  Macro_Replacement_Map *map,
  Array_Token_Ptr source
) {
  Array_Token_Ptr result = dyn_array_make(Array_Token_Ptr, .capacity = dyn_array_length(source));
  for (u64 i = 0; i < dyn_array_length(source); ++i) {
    Token *token = *dyn_array_get(source, i);
    Token *copy = temp_allocate(Token);
    *copy = *token;
    switch (token->type) {
      case Token_Type_Id: {
        Slice name = token->source;
        Token **replacement_ptr = hash_map_get(map, name);
        if (replacement_ptr) {
          *copy = **replacement_ptr;
        }
        break;
      }
      case Token_Type_Newline:
      case Token_Type_Integer:
      case Token_Type_Hex_Integer:
      case Token_Type_Operator:
      case Token_Type_String: {
        // Nothing to do
        break;
      }
      case Token_Type_Square:
      case Token_Type_Paren:
      case Token_Type_Curly:
      case Token_Type_Module: {
        copy->children = token_apply_macro_replacements(map, token->children);
        break;
      }
      case Token_Type_Value: {
        panic("Macro definitions should not contain semi-resolved tokens");
        break;
      }
    }
    dyn_array_push(result, copy);
  }
  return result;
}

void
token_rewrite_macro_match(
  Token_Matcher_State *state,
  Macro *macro,
  Array_Token_Ptr match
) {
  Macro_Replacement_Map *map = hash_map_make(Macro_Replacement_Map);
  if (dyn_array_length(macro->pattern_names) != dyn_array_length(match)) {
    panic("Should not have chosen the macro if pattern length do not match");
  }
  for (u64 i = 0; i < dyn_array_length(macro->pattern_names); ++i) {
    Slice *name = dyn_array_get(macro->pattern_names, i);
    if (name->length) {
      hash_map_set(map, *name, *dyn_array_get(match, i));
    }
  }
  Array_Token_Ptr replacement = token_apply_macro_replacements(map, macro->replacement);

  token_replace_length_with_tokens(state, dyn_array_length(macro->pattern), replacement);
  dyn_array_destroy(match);
  hash_map_destroy(map);
}

void
token_rewrite_macros(
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  for (;scope; scope = scope->parent) {
    if (!dyn_array_is_initialized(scope->macros)) continue;
    for (u64 macro_index = 0; macro_index < dyn_array_length(scope->macros); ++macro_index) {
      Macro *macro = *dyn_array_get(scope->macros, macro_index);
      start: for (;;) {
        for (u64 i = 0; i < dyn_array_length(state->tokens); ++i) {
          Token_Matcher_State sub_state = {state->tokens, i};
          Array_Token_Ptr match = token_match_pattern(&sub_state, macro->pattern);
          if (dyn_array_is_initialized(match)) {
            token_rewrite_macro_match(&sub_state, macro, match);
            goto start;
          }
        }
        break;
      }
    }
  }
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
);

Descriptor *
token_match_type(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Descriptor *descriptor = token_match_fixed_array_type(context, state, scope, builder);
  if (descriptor) return descriptor;
  u64 length = dyn_array_length(state->tokens);
  if (!length) panic("Caller must not call token_match_type with empty token list");
  Token *token = *dyn_array_get(state->tokens, 0);
  if (length > 1) {
    program_push_error_from_slice(
      context->program,
      token->source_range,
      slice_literal("Can not resolve type")
    );
    return 0;
  }
  return token_force_type(context, scope, token, builder);
}

Token_Match_Arg *
token_match_argument(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":");

  u64 start_index = state->start_index + peek_index;
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){start_index, dyn_array_length(state->tokens)}
  );
  Token_Matcher_State rest_state = {.tokens = rest};
  state->tokens.data->length = state->start_index;
  Descriptor *type_descriptor = token_match_type(context, &rest_state, scope, builder);
  if (!type_descriptor) return 0;
  Token_Match_Arg *arg = temp_allocate(Token_Match_Arg);
  *arg = (Token_Match_Arg){name->source, type_descriptor};
  return arg;
}

void
token_match_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *target
);

void
token_force_value(
  Compilation_Context *context,
  Token *token,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  switch(token->type) {
    case Token_Type_Integer: {
      bool ok = false;
      u64 number = slice_parse_u64(token->source, &ok);
      if (!ok) {
        program_error_builder(context->program, token->source_range) {
          program_error_append_literal("Invalid integer literal: ");
          program_error_append_slice(token->source);
        }
        return;
      }
      Value *immediate = value_from_unsigned_immediate(number);
      move_value(builder, &token->source_range, result_value, immediate);
      return;
    }
    case Token_Type_Hex_Integer: {
      bool ok = false;
      Slice digits = slice_sub(token->source, 2, token->source.length);
      u64 number = slice_parse_hex(digits, &ok);
      if (!ok) {
        program_error_builder(context->program, token->source_range) {
          program_error_append_literal("Invalid integer hex literal: ");
          program_error_append_slice(token->source);
        }
        return;
      }
      // TODO should be unsigned
      Value *immediate = value_from_signed_immediate(number);
      move_value(builder, &token->source_range, result_value, immediate);
      return;
    }
    case Token_Type_String: {
      Slice string = token->string;
      Value *string_bytes = value_global_c_string_from_slice(context->program, string);
      Value *c_string_pointer = value_pointer_to(builder, &token->source_range, string_bytes);
      move_value(builder, &token->source_range, result_value, c_string_pointer);
      return;
    }
    case Token_Type_Id: {
      Slice name = token->source;
      Value *value = scope_lookup_force(context, scope, name, builder);
      if (!value) {
        program_error_builder(context->program, token->source_range) {
          program_error_append_literal("Undefined variable ");
          program_error_append_slice(name);
        }
      } else {
        move_value(builder, &token->source_range, result_value, value);
      }
      return;
    }
    case Token_Type_Value: {
      move_value(builder, &token->source_range, result_value, token->value);
      return;
    }
    case Token_Type_Paren: {
      if (!builder) panic("Caller should only force (...) in a builder context");
      Token_Matcher_State state = {.tokens = token->children};
      token_match_expression(context, &state, scope, builder, result_value);
      return;
    }
    case Token_Type_Curly: {
      if (!builder) panic("Caller should only force {...} in a builder context");
      token_parse_block(context, token, scope, builder, result_value);
      return;
    }
    case Token_Type_Module:
    case Token_Type_Square:
    case Token_Type_Operator: {
      panic("TODO");
      return;
    }
    case Token_Type_Newline: {
      program_push_error_from_slice(
        context->program,
        token->source_range,
        slice_literal("Unexpected newline token")
      );
      return;
    }
  }
  panic("Not reached");
  return;
}


// FIXME pass in the function definition
Array_Value_Ptr
token_match_call_arguments(
  Compilation_Context *context,
  Token *token,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Value_Ptr result = dyn_array_make(Array_Value_Ptr);
  if (dyn_array_length(token->children) != 0) {
    Array_Token_Matcher_State argument_states = token_split(token->children, &(Token_Pattern){
      .type = Token_Type_Operator,
      .source = slice_literal(","),
    });
    // TODO :TargetValue
    // There is an interesting conundrum here that we need to know the types of the
    // arguments for overload resolution, but then we need the exact function definition
    // to know the result_value definition to do the evaluation. Proper solution would
    // be to introduce :TypeOnlyEvalulation, but for now we will just create a special
    // target value that can be anything that will behave like type inference and is
    // needed regardless for something like x := (...)
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_Matcher_State *state = dyn_array_get(argument_states, i);
      Value *result_value = value_any();
      token_match_expression(context, state, scope, builder, result_value);
      dyn_array_push(result, result_value);
    }
  }
  return result;
}

static inline Token *
token_value_make(
  Value *result,
  Source_Range source_range
) {
  Token *result_token = temp_allocate(Token);
  *result_token = (Token){
    .type = Token_Type_Value,
    .source_range = source_range,
    .source = slice_sub_range(source_range.file->text, source_range.offsets),
    .value = result,
  };
  return result_token;
}

void
token_replace_tokens_in_state(
  Token_Matcher_State *state,
  u64 length,
  Token *token
) {
  u64 from = state->start_index;
  if (token) {
    *dyn_array_get(state->tokens, from) = token;
    from += 1;
    length -= 1;
  }
  dyn_array_delete_range(state->tokens, (Range_u64){from, from + length});
}

bool
token_state_clear_newlines(
  Token_Matcher_State *state
) {
  for (u64 i = state->start_index; i < dyn_array_length(state->tokens); ++i) {
    Token *token = *dyn_array_get(state->tokens, i);
    if (token->type == Token_Type_Newline) {
      dyn_array_delete(state->tokens, i);
      --i;
    }
  }
  return true;
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

bool
token_rewrite_macro_definitions(
  Token_Matcher_State *state,
  Scope *scope
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("macro"));
  Token_Match(pattern_token, .type = Token_Type_Paren);
  Token_Match(replacement_token, .type = Token_Type_Paren);
  token_replace_tokens_in_state(state, 3, 0);

  Array_Token_Pattern pattern = dyn_array_make(Array_Token_Pattern);
  Array_Slice pattern_names = dyn_array_make(Array_Slice);

  for (u64 i = 0; i < dyn_array_length(pattern_token->children); ++i) {
    Token *token = *dyn_array_get(pattern_token->children, i);
    if (token->type == Token_Type_Id && slice_starts_with(token->source, slice_literal("_"))) {
      Slice pattern_name = slice_sub(token->source, 1, token->source.length);
      dyn_array_push(pattern_names, pattern_name);
      dyn_array_push(pattern, (Token_Pattern) {0});
    } else {
      dyn_array_push(pattern_names, (Slice){0});
      if (token->type == Token_Type_Id) {
        dyn_array_push(pattern, (Token_Pattern) {
          .type = token->type,
          .source = token->source,
        });
      } else {
        dyn_array_push(pattern, (Token_Pattern) {
          .type = token->type,
        });
      }
    }
  }

  Macro *macro = temp_allocate(Macro);
  *macro = (Macro){
    .pattern = pattern,
    .pattern_names = pattern_names,
    .replacement = replacement_token->children,
  };

  scope_add_macro(scope, macro);

  return true;
}

bool
token_match_struct_field(
  Compilation_Context *context,
  Descriptor *struct_descriptor,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":");

  u64 start_index = state->start_index + peek_index;
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){start_index, dyn_array_length(state->tokens)}
  );
  Token_Matcher_State rest_state = {.tokens = rest};
  state->tokens.data->length = state->start_index;

  Descriptor *descriptor = token_match_type(context, &rest_state, scope, builder);
  if (!descriptor) return false;
  descriptor_struct_add_field(struct_descriptor, descriptor, name->source);
  return true;
}

bool
token_rewrite_type_definitions(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("type"));
  Token_Match(layout_token, .type = Token_Type_Paren);
  Token *replacement_token = 0;

  u64 layout_children_count = dyn_array_length(layout_token->children);
  Token *layout_block = 0;
  Value *bit_size_value = 0;
  switch (layout_children_count) {
    case 0: {
      // TODO print error
      goto err;
    }
    case 1: {
      Token *token = *dyn_array_get(layout_token->children, 0);
      // TODO should be a compile time function later on
      if (token->type == Token_Type_Id && slice_equal(token->source, slice_literal("c_struct"))) {
        layout_block =
          token_peek_match(state, peek_index++, &(Token_Pattern) { .type = Token_Type_Curly });
        if (!layout_block) {
          // TODO print error
          goto err;
        }
        break;
      }
      // fallthrough
    }
    default: {
      Token_Matcher_State layout_state = {layout_token->children};
      Token *token = token_rewrite_constant_expression(context, &layout_state, scope, builder);
      if (token) {
        bit_size_value = value_any();
        token_force_value(context, token, scope, builder, bit_size_value);
      }
      if (!bit_size_value) {
        // TODO print error
        goto err;
      }
      break;
    }
  }

  assert(layout_block || bit_size_value);
  Value *result = temp_allocate(Value);

  Descriptor *descriptor = temp_allocate(Descriptor);
  if (bit_size_value) {
    if (bit_size_value->descriptor->type != Descriptor_Type_Integer) {
      // TODO err
      goto err;
    }
    if (!operand_is_immediate(&bit_size_value->operand)) {
      // TODO err
      goto err;
    }
    u64 bit_size = s64_to_u64(operand_immediate_as_s64(&bit_size_value->operand));
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Opaque,
      .opaque = { .bit_size = bit_size },
    };
  } else {
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Struct,
      .struct_ = {
        .fields = dyn_array_make(Array_Descriptor_Struct_Field),
      },
    };

    if (dyn_array_length(layout_block->children) != 0) {
      Array_Token_Matcher_State definitions =
        token_split_by_newlines_and_semicolons(layout_block->children);
      for (u64 i = 0; i < dyn_array_length(definitions); ++i) {
        Token_Matcher_State *field_state = dyn_array_get(definitions, i);
        token_match_struct_field(context, descriptor, field_state, scope, builder);
      }
    }
  }

  Descriptor *value_descriptor = temp_allocate(Descriptor);
  *value_descriptor = (Descriptor) {
    .type = Descriptor_Type_Type,
    .type_descriptor = descriptor,
  };
  *result = (Value) {
    .descriptor = value_descriptor,
    .operand = {.type = Operand_Type_None },
  };
  replacement_token = token_value_make(result, TOKEN_MATCHED_SOURCE());

  err:
  u64 replacement_count = layout_block ? 3 : 2;
  token_replace_tokens_in_state(state, replacement_count, replacement_token);

  return true;
}

bool
token_maybe_split_on_operator(
  Array_Token_Ptr tokens,
  Slice operator,
  Array_Token_Ptr *lhs,
  Array_Token_Ptr *rhs,
  Token **operator_token
) {
  u64 lhs_end = 0;
  u64 rhs_start = 0;
  for (u64 i = 0; i < dyn_array_length(tokens); ++i) {
    Token *token = *dyn_array_get(tokens, i);
    if (token->type == Token_Type_Operator && slice_equal(token->source, operator)) {
      *operator_token = token;
      lhs_end = i;
      rhs_start = i + 1;
      break;
    }
  }

  if (lhs_end == 0) {
    *lhs = *rhs = (Array_Token_Ptr){0};
    return false;
  }

  *lhs = dyn_array_sub(Array_Token_Ptr, tokens, (Range_u64){ 0, lhs_end });
  *rhs = dyn_array_sub(Array_Token_Ptr, tokens, (Range_u64){ rhs_start, dyn_array_length(tokens) });

  return true;
}

Token *
token_import_match_arguments(
  Source_Range source_range,
  Token_Matcher_State *state,
  Compilation_Context *context
) {
  u64 peek_index = 0;
  Token *library_name_token = token_peek_match(state, peek_index++, &(Token_Pattern) {
    .type = Token_Type_String,
  });
  if (!library_name_token) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("First argument to external() must be a literal string")
    );
    return 0;
  }
  Token *comma = token_peek_match(state, peek_index++, &(Token_Pattern) {
    .type = Token_Type_Operator,
    .source = slice_literal(","),
  });
  if (!comma) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("external(\"library_name\", \"symbol_name\") requires two arguments")
    );
    return 0;
  }
  Token *symbol_name_token = token_peek_match(state, peek_index++, &(Token_Pattern) {
    .type = Token_Type_String,
  });
  if (!symbol_name_token) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("Second argument to external() must be a literal string")
    );
    return 0;
  }
  Slice library_name = library_name_token->string;
  Slice symbol_name = symbol_name_token->string;

  Value *result = temp_allocate(Value);
  *result = (const Value) {
    .descriptor = 0,
    .operand = import_symbol(context->program, library_name, symbol_name),
  };
  return token_value_make(result, TOKEN_MATCHED_SOURCE());
}

bool
token_rewrite_external_import(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder_
) {
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id, .source = slice_literal("external"));
  Token_Match(args, .type = Token_Type_Paren);
  Token_Matcher_State *args_state = &(Token_Matcher_State) {.tokens = args->children };
  Token *result_token = token_import_match_arguments(args->source_range, args_state, context);
  if (!result_token) return false;
  token_replace_tokens_in_state(state, 2, result_token);
  return true;
}

typedef bool (*token_rewrite_expression_callback)
(Compilation_Context *context, Token_Matcher_State *, Scope *, Function_Builder *, Value *result_value);

void
token_rewrite_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value,
  token_rewrite_expression_callback callback
) {
  start: for (;;) {
    for (u64 i = state->start_index; i < dyn_array_length(state->tokens); ++i) {
      Token_Matcher_State rewrite_state = {state->tokens, i};
      if (callback(context, &rewrite_state, scope, builder, result_value)) goto start;
    }
    break;
  }
}

typedef bool (*token_rewrite_statement_callback)
(Compilation_Context *context, Token_Matcher_State *, Scope *, Function_Builder *);

void
token_rewrite_statement(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  token_rewrite_statement_callback callback
) {
  start: for (;;) {
    for (u64 i = state->start_index; i < dyn_array_length(state->tokens); ++i) {
      Token_Matcher_State rewrite_state = {state->tokens, i};
      if (callback(context, &rewrite_state, scope, builder)) goto start;
    }
    break;
  }
}

bool
token_rewrite_function_literal(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *outer_builder
) {
  u64 peek_index = 0;
  Token_Maybe_Match(inline_, .type = Token_Type_Id, .source = slice_literal("inline"));
  Token_Match(args, .type = Token_Type_Paren);
  Token_Match_Operator(arrow, "->");
  Token_Match(return_types, .type = Token_Type_Paren);
  Token_Maybe_Match(external_body, .type = Token_Type_Value);
  Token_Maybe_Match(regular_body, .type = Token_Type_Curly);

  Scope *function_scope = scope_make(context->program->global_scope);

  Function_Builder *builder = 0;
  // TODO think about a better way to distinguish imports
  Descriptor *descriptor = 0;

  if (external_body) {
    if (inline_) {
      program_error_builder(context->program, inline_->source_range) {
        program_error_append_literal("External functions can not be inline");
      }
      inline_ = 0;
    }
    if(!external_body->value) return 0;
    descriptor = temp_allocate(Descriptor);
    *descriptor = (Descriptor) {
      .type = Descriptor_Type_Function,
      .function = {
        .arguments = dyn_array_make(Array_Value_Ptr, .allocator = temp_allocator),
        .argument_names = dyn_array_make(Array_Slice, .allocator = temp_allocator),
        .returns = 0,
      },
    };
  } else if (regular_body) {
    builder = fn_begin(context->program);
    descriptor = builder->descriptor;
  } else {
    // FIXME better error reporting or maybe consider this a function type
    return 0;
  }

  switch (dyn_array_length(return_types->children)) {
    case 0: {
      descriptor->function.returns = &void_value;
      break;
    }
    case 1: {
      Token *return_type_token = *dyn_array_get(return_types->children, 0);
      Descriptor *return_descriptor =
        token_force_type(context, function_scope, return_type_token, outer_builder);
      if (!return_descriptor) return 0;
      function_return_descriptor(&descriptor->function, return_descriptor);
      break;
    }
    default: {
      panic("Multiple return types are not supported at the moment");
      break;
    }
  }

  if (dyn_array_length(args->children) != 0) {
    Array_Token_Matcher_State argument_states = token_split(args->children, &(Token_Pattern){
      .type = Token_Type_Operator,
      .source = slice_literal(","),
    });
    for (u64 i = 0; i < dyn_array_length(argument_states); ++i) {
      Token_Matcher_State *args_state = dyn_array_get(argument_states, i);
      token_state_clear_newlines(args_state);
      Token_Match_Arg *arg =
        token_match_argument(context, args_state, function_scope, outer_builder);
      if (!arg) return 0;
      Value *arg_value = function_push_argument(&descriptor->function, arg->type_descriptor);
      if (regular_body && arg_value->operand.type == Operand_Type_Register) {
        register_bitset_set(&builder->code_block.register_occupied_bitset, arg_value->operand.reg);
      }
      dyn_array_push(descriptor->function.argument_names, arg->arg_name);
      scope_define_value(function_scope, arg->arg_name, arg_value);
    }
    assert(
      dyn_array_length(descriptor->function.argument_names) ==
      dyn_array_length(descriptor->function.arguments)
    );
  }

  Value *result = 0;
  if (external_body) {
    external_body->value->descriptor = descriptor;
    result = external_body->value;
  } else {
    Value *return_result_value =
      descriptor->function.returns->descriptor->type == Descriptor_Type_Void
        ? value_any()
        : descriptor->function.returns;
    if (inline_) {
      descriptor->function.parent_scope = scope;
      descriptor->function.inline_body = token_clone_deep(regular_body);
    }
    // TODO might want to do this lazily for inline functions
    token_parse_block(context, regular_body, function_scope, builder, return_result_value);

    fn_end(builder);
    result = builder->value;
  }
  u64 replacement_count = inline_ ? 5 : 4;
  Token *value_token = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, replacement_count, value_token);
  return true;
}

bool
token_rewrite_negative_literal(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  // FIXME Allow unary minus on any expression
  Token_Match_Operator(define, "-");
  Token_Match(integer, .type = Token_Type_Integer);
  Value *result = value_any();
  token_force_value(context, integer, scope, builder, result);
  if (result->operand.type == Operand_Type_Immediate_8) {
    result->operand.s8 = -result->operand.s8;
  } else if (result->operand.type == Operand_Type_Immediate_16) {
    result->operand.s16 = -result->operand.s16;
  } else if (result->operand.type == Operand_Type_Immediate_32) {
    result->operand.s32 = -result->operand.s32;
  } else if (result->operand.type == Operand_Type_Immediate_64) {
    result->operand.s64 = -result->operand.s64;
  } else {
    panic("Internal error, expected an immediate");
  }

  Token *value_token = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 2, value_token);
  return true;
}

bool
token_rewrite_constant_sub_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(paren, .type = Token_Type_Paren);

  Token_Matcher_State sub_state = {paren->children};
  Token *result_token = token_rewrite_constant_expression(context, &sub_state, scope, builder);
  if (!result_token) {
    return false;
  }
  token_replace_tokens_in_state(state, 1, result_token);
  return true;
}

typedef void (*Compile_Time_Eval_Proc)(void *);

Token *
compile_time_eval(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope
) {
  Token *first_token = *dyn_array_get(state->tokens, state->start_index);

  Program eval_program = {
    .import_libraries = dyn_array_copy(Array_Import_Library, context->program->import_libraries),
    // FIXME this is probably broken now because code ones should point to a different section
    .labels = dyn_array_copy(Array_Label, context->program->labels),
    .patch_info_array =
      dyn_array_copy(Array_Label_Location_Diff_Patch_Info, context->program->patch_info_array),
    .functions = dyn_array_copy(Array_Function_Builder, context->program->functions),
    .global_scope = scope_make(context->program->global_scope),
    .errors = dyn_array_make(Array_Parse_Error),
    .data_section = context->program->data_section,
    .code_section = {
      .buffer = bucket_buffer_make(.allocator = allocator_system),
      .permissions = Section_Permissions_Execute,
    }
  };
  Function_Builder *eval_builder = fn_begin(&eval_program);
  function_return_descriptor(&eval_builder->descriptor->function, &descriptor_void);

  Compilation_Context eval_context = *context;
  eval_context.program = &eval_program;

  Value *expression_result_value = value_any();
  token_match_expression(
    &eval_context, state, eval_program.global_scope, eval_builder, expression_result_value
  );

  // We use a something like a C++ reference out parameter for the
  // result to have a consitent function signature on this C side of things.

  // Make it out parameter a pointer to ensure it is passed inside a register according to ABI
  Value *arg_value = function_push_argument(
    &eval_builder->value->descriptor->function,
    descriptor_pointer_to(expression_result_value->descriptor)
  );

  // Create a reference Value
  assert(arg_value->operand.type == Operand_Type_Register);
  Value *out_value = temp_allocate(Value);
  *out_value = (Value) {
    .descriptor = expression_result_value->descriptor,
    .operand = (Operand){
      .type = Operand_Type_Memory_Indirect,
      .byte_size = expression_result_value->operand.byte_size,
      .indirect = { .reg = arg_value->operand.reg },
    },
  };

  move_value(eval_builder, &first_token->source_range, out_value, expression_result_value);
  fn_end(eval_builder);

  program_jit(&eval_program);

  u32 result_byte_size = out_value->operand.byte_size;
  // Need to ensure 16-byte alignment here because result value might be __m128
  // TODO When we support AVX-2 or AVX-512, this might need to increase further
  u32 alignment = 16;
  void *result = allocator_allocate_bytes(temp_allocator, result_byte_size, alignment);

  Compile_Time_Eval_Proc jitted_code =
    (Compile_Time_Eval_Proc)value_as_function(&eval_program, eval_builder->value);

  jitted_code(result);
  Value *token_value = temp_allocate(Value);
  *token_value = (Value) {
    .descriptor = out_value->descriptor,
  };
  switch(out_value->descriptor->type) {
    case Descriptor_Type_Float:
    case Descriptor_Type_Integer: {
      switch (result_byte_size) {
        case 8: {
          token_value->operand = imm64(*(s64 *)result);
          break;
        }
        case 4: {
          token_value->operand = imm32(*(s32 *)result);
          break;
        }
        case 2: {
          token_value->operand = imm16(*(s16 *)result);
          break;
        }
        case 1: {
          token_value->operand = imm8(*(s8 *)result);
          break;
        }
        default: {
          panic("Unsupported immediate size");
          break;
        }
      }
      break;
    }
    case Descriptor_Type_Void: {
      token_value->operand = (Operand){0};
      break;
    }
    case Descriptor_Type_Any: {
      panic("Internal Error: We should never get Any type from comp time eval");
      break;
    }
    case Descriptor_Type_Tagged_Union:
    case Descriptor_Type_Fixed_Size_Array:
    case Descriptor_Type_Pointer:
    case Descriptor_Type_Struct: {
      panic("TODO move to data section or maybe we should allocate from there right away above?");
      break;
    };
    case Descriptor_Type_Opaque:
    case Descriptor_Type_Function:
    case Descriptor_Type_Type: {
      panic("TODO figure out how that works");
      break;
    }
  }
  return token_value_make(
    token_value,
    source_range_from_token_matcher_state(
      state, dyn_array_length(state->tokens) - state->start_index
    )
  );
}

bool
token_rewrite_compile_time_eval(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *unused_builder
) {
  u64 peek_index = 0;
  Token_Match_Operator(at, "@");
  Token_Match(paren, .type = Token_Type_Paren);

  Token_Matcher_State sub_state = {paren->children};

  Token *result_token = compile_time_eval(context, &sub_state, scope);

  if (!result_token) {
    return false;
  }
  token_replace_tokens_in_state(state, 2, result_token);
  return true;
}

bool
token_rewrite_compile_time_function_call(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  // @CopyPaste with the regular function call
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(args_token, .type = Token_Type_Paren);
  if (target_token->type != Token_Type_Id && target_token->type != Token_Type_Paren) return false;

  Range_u64 call_token_range = {
    .from = state->start_index,
    .to = state->start_index + peek_index
  };
  Array_Token_Ptr call_tokens = dyn_array_sub(Array_Token_Ptr, state->tokens, call_token_range);
  Token *result = compile_time_eval(context, &(Token_Matcher_State){call_tokens}, scope);

  token_replace_tokens_in_state(state, 2, result);
  return result;
}

Token *
token_rewrite_constant_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 token_count = dyn_array_length(state->tokens);
  if (!token_count) return 0;
  Source_Range source_range =  source_range_from_token_matcher_state(state, token_count);
  token_state_clear_newlines(state);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_negative_literal);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_external_import);

  token_rewrite_type_definitions(context, state, scope, builder) ||
  token_rewrite_function_literal(context, state, scope, builder) ||
  token_rewrite_compile_time_function_call(context, state, scope, builder) ||
  token_rewrite_compile_time_eval(context, state, scope, builder);

  token_rewrite_statement(context, state, scope, builder, token_rewrite_constant_sub_expression);

  if (dyn_array_length(state->tokens) != 1) {
    program_push_error_from_slice(
      context->program, source_range,
      slice_literal("Invalid constant definition")
    );
    return 0;
  }
  return *dyn_array_get(state->tokens, 0);
}

bool
token_rewrite_constant_definitions(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(state->tokens, slice_literal("::"), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (dyn_array_length(lhs) > 1) return false;
  Token *name = *dyn_array_get(lhs, 0);
  if (name->type != Token_Type_Id) return false;
  scope_define_lazy(scope, name->source, rhs);
  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

void
token_parse_statement(
  Compilation_Context *context,
  Token_Matcher_State *state,
  const Source_Range *source_range,
  Scope *scope,
  Function_Builder *builder,
  Value *statement_result_value
);

bool
token_parse_block(
  Compilation_Context *context,
  Token *block,
  Scope *scope,
  Function_Builder *builder,
  Value *block_result_value
) {
  assert(block->type == Token_Type_Curly);
  Array_Token_Ptr children = block->children;
  if (!dyn_array_length(children)) return false;
  Scope *block_scope = scope_make(scope);

  // Newlines at the end of the block do not count as semicolons otherwise this:
  // { 42
  // }
  // is being interpreted as:
  // { 42 ; }
  while ((*dyn_array_last(children))->type == Token_Type_Newline) dyn_array_pop(children);

  Array_Token_Matcher_State block_statements = token_split_by_newlines_and_semicolons(children);
  for (u64 i = 0; i < dyn_array_length(block_statements); ++i) {
    Token_Matcher_State *state = dyn_array_get(block_statements, i);
    if (!dyn_array_length(state->tokens)) continue;
    bool is_last_statement = i + 1 == dyn_array_length(block_statements);
    Value *result_value = is_last_statement ? block_result_value : value_any();

    // If result is a register we need to make sure it is acquired to avoid it being used
    // as temporary when evaluating last statement. This definitely can happen with
    // the function returns but should be safe to do all the time.
    if (is_last_statement && result_value->operand.type == Operand_Type_Register) {
      if (!register_bitset_get(builder->used_register_bitset, result_value->operand.reg)) {
        register_acquire(builder, result_value->operand.reg);
      }
    }

    token_parse_statement(context, state, &block->source_range, block_scope, builder, result_value);
  }
  return true;
}

bool
token_rewrite_statement_if(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("if"));
  Token_Match(condition, .type = Token_Type_Paren);
  Token_Match(body, .type = Token_Type_Curly);
  Token_Match_End();

  Value *condition_value = value_any();
  token_force_value(context, condition, scope, builder, condition_value);
  Label_Index else_label = make_if(
    context->program, &builder->code_block.instructions, &keyword->source_range, condition_value
  );
  token_parse_block(context, body, scope, builder, value_any());
  push_instruction(
    &builder->code_block.instructions, &keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = else_label}
  );

  token_replace_tokens_in_state(state, 3, 0);
  return true;
}

bool
token_rewrite_goto(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("goto"));
  Token_Match(label_name, .type = Token_Type_Id);
  Token_Match_End();
  Value *value = scope_lookup_force(context, scope, label_name->source, builder);
  if (value) {
    if (
      value->descriptor->type == Descriptor_Type_Void &&
      value->operand.type == Operand_Type_Label_32
    ) {
      push_instruction(
        &builder->code_block.instructions, &keyword->source_range,
        (Instruction) {.assembly = {jmp, {value->operand, 0, 0}}}
      );
    } else {
      program_error_builder(context->program, label_name->source_range) {
        program_error_append_slice(label_name->source);
        program_error_append_literal(" is not a label");
      }
    }
  }

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_explicit_return(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("return"));
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){peek_index, dyn_array_length(state->tokens)}
  );
  bool has_return_expression = dyn_array_length(rest) > 0;
  Value *fn_return = builder->descriptor->function.returns;
  token_match_expression(context, &(Token_Matcher_State){rest}, scope, builder, fn_return);

  bool is_void = fn_return->descriptor->type == Descriptor_Type_Void;
  if (!is_void && !has_return_expression) {
    program_push_error_from_slice(
      context->program, keyword->source_range,
      slice_literal("Explicit return from a non-void function requires a value")
    );
  }

  push_instruction(
    &builder->code_block.instructions,
    &keyword->source_range,
    (Instruction) {.assembly = {jmp, {label32(builder->code_block.end_label), 0, 0}}}
  );

  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_pointer_to(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match_Operator(operator, "&");
  Token_Match(value_token, 0);

  Value *pointee = value_any();
  token_force_value(context, value_token, scope, builder, pointee);
  Value *result = value_pointer_to(builder, &operator->source_range, pointee);
  Token *token_value = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 2, token_value);
  return true;
}

Descriptor *
token_match_fixed_array_type(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(type, .type = Token_Type_Id);
  Token_Match(square_brace, .type = Token_Type_Square);
  Descriptor *descriptor =
    scope_lookup_type(context, scope, type->source_range, type->source, builder);

  Token_Matcher_State size_state = {.tokens = square_brace->children};
  // FIXME :TargetValue Make a convention to have this as a constant / immediate
  Value *size_value = value_any();
  token_match_expression(context, &size_state, scope, builder, size_value);
  if (size_value->descriptor->type != Descriptor_Type_Integer) {
    program_push_error_from_slice(
      context->program,
      square_brace->source_range,
      slice_literal("Fixed size array size is not an integer")
    );
    return 0;
  }
  if (!operand_is_immediate(&size_value->operand)) {
    program_push_error_from_slice(
      context->program,
      square_brace->source_range,
      slice_literal("Fixed size array size must be known at compile time")
    );
    return 0;
  }
  u32 length = s64_to_u32(operand_immediate_as_s64(&size_value->operand));

  // TODO extract into a helper
  Descriptor *array_descriptor = temp_allocate(Descriptor);
  *array_descriptor = (Descriptor) {
    .type = Descriptor_Type_Fixed_Size_Array,
    .array = {
      .item = descriptor,
      .length = length,
    },
  };
  return array_descriptor;
}

bool
token_rewrite_inline_machine_code_bytes(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(id_token, .type = Token_Type_Id, .source = slice_literal("inline_machine_code_bytes"));
  Token_Match(args_token, .type = Token_Type_Paren);
  Token_Match_End();

  Array_Value_Ptr args = token_match_call_arguments(context, args_token, scope, builder);

  u64 byte_count = dyn_array_length(args);
  if (byte_count > 15) {
    program_error_builder(context->program, args_token->source_range) {
      program_error_append_literal("Expected a maximum of 15 arguments, got ");
      program_error_append_number("%lld", byte_count);
    }
    goto end;
  }

  Fixed_Buffer *buffer = fixed_buffer_make(.allocator = temp_allocator, .capacity = byte_count);
  for (u64 i = 0; i < byte_count; ++i) {
    Value *value = *dyn_array_get(args, i);
    if (!value) continue;
    if (value->descriptor->type != Descriptor_Type_Integer) {
      program_error_builder(context->program, args_token->source_range) {
        program_error_append_literal("inline_machine_code_bytes expects arguments to be integers");
      }
      goto end;
    }
    if (!operand_is_immediate(&value->operand)) {
      program_error_builder(context->program, args_token->source_range) {
        program_error_append_literal("inline_machine_code_bytes expects arguments to be compile-time known");
      }
      goto end;
    }
    s64 byte = operand_immediate_as_s64(&value->operand);
    if (!u64_fits_into_u8(byte)) {
      program_error_builder(context->program, args_token->source_range) {
        program_error_append_literal("Expected integer between 0 and 255, got ");
        program_error_append_number("%lld", byte);
      }
      goto end;
    }
    fixed_buffer_append_u8(buffer, s64_to_u8(byte));
  }

  push_instruction(
    &builder->code_block.instructions, &id_token->source_range,
    (Instruction) {
      .type = Instruction_Type_Bytes,
      .bytes = fixed_buffer_as_slice(buffer),
     }
  );

  end:
  token_replace_tokens_in_state(state, 2, 0);
  return true;
}

bool
token_rewrite_cast(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(cast, .type = Token_Type_Id, .source = slice_literal("cast"));
  Token_Match(value_token, .type = Token_Type_Paren);

  Array_Value_Ptr args = token_match_call_arguments(context, value_token, scope, builder);
  assert(dyn_array_length(args) == 2);
  Value *type = *dyn_array_get(args, 0);
  Value *value = *dyn_array_get(args, 1);
  assert(type->descriptor->type == Descriptor_Type_Type);

  Descriptor *cast_to_descriptor = type->descriptor->type_descriptor;
  assert(cast_to_descriptor->type == Descriptor_Type_Integer);
  assert(value->descriptor->type == cast_to_descriptor->type);

  u32 cast_to_byte_size = descriptor_byte_size(cast_to_descriptor);
  u32 original_byte_size = descriptor_byte_size(value->descriptor);
  Value *result = value;
  if (cast_to_byte_size != original_byte_size) {
    result = temp_allocate(Value);
    if (cast_to_byte_size < original_byte_size) {
      *result = (Value) {
        .descriptor = cast_to_descriptor,
        .operand = value->operand,
      };
      result->operand.byte_size = cast_to_byte_size;
    } else if (cast_to_byte_size > original_byte_size) {
      result = reserve_stack(builder, cast_to_descriptor);
      move_value(builder, &cast->source_range, result, value);
    }
  }

  Token *token_value = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 2, token_value);
  return true;
}

Value *
token_match_label(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(keyword, .type = Token_Type_Id, .source = slice_literal("label"));
  Token_Match_End();

  Label_Index label = make_label(context->program, &context->program->code_section);
  push_instruction(
    &builder->code_block.instructions, &keyword->source_range,
    (Instruction) {.type = Instruction_Type_Label, .label = label }
  );
  Value *value = temp_allocate(Value);
  *value = (Value) {
    .descriptor = &descriptor_void,
    .operand = label32(label),
  };

  return value;
}

bool
token_rewrite_definitions(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  // TODO consider merging with argument matching
  u64 peek_index = 0;
  Token_Match(name, .type = Token_Type_Id);
  Token_Match_Operator(define, ":");

  u64 start_index = state->start_index + peek_index;
  Array_Token_Ptr rest = dyn_array_sub(
    Array_Token_Ptr, state->tokens, (Range_u64){start_index, dyn_array_length(state->tokens)}
  );
  Token_Matcher_State rest_state = {.tokens = rest};
  u64 size_to_replace = dyn_array_length(state->tokens) - state->start_index;

  Value *value = token_match_label(context, &rest_state, scope, builder);
  if (!value) {
    Descriptor *descriptor = token_match_type(context, &rest_state, scope, builder);
    if (!descriptor) {
      program_error_builder(context->program, define->source_range) {
        program_error_append_literal("Could not find type");
      }
      return false;
    }
    value = reserve_stack(builder, descriptor);
  }
  scope_define_value(scope, name->source, value);
  Token *token_value = token_value_make(value, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, size_to_replace, token_value);

  return true;
}

bool
token_rewrite_definition_and_assignment_statements(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;

  if (!token_maybe_split_on_operator(state->tokens, slice_literal(":="), &lhs, &rhs, &operator)) {
    return false;
  }
  // For now we support only single ID on the left
  if (dyn_array_length(lhs) > 1) return false;
  Token *name = *dyn_array_get(lhs, 0);

  if (name->type != Token_Type_Id) return false;

  Token_Matcher_State rhs_state = {rhs};
  Value *value = value_any();
  token_match_expression(context, &rhs_state, scope, builder, value);

  // x := 42 should always be initialized to s64 to avoid weird suprises
  if (
    value->descriptor->type == Descriptor_Type_Integer &&
    operand_is_immediate(&value->operand)
  ) {
    value = value_from_s64(operand_immediate_as_s64(&value->operand));
  } else if (
    value->descriptor->type == Descriptor_Type_Float &&
    operand_is_immediate(&value->operand)
  ) {
    panic("TODO decide how to handle floats");
  }
  Value *on_stack = reserve_stack(builder, value->descriptor);
  move_value(builder, &name->source_range, on_stack, value);

  scope_define_value(scope, name->source, on_stack);

  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

bool
token_rewrite_array_index(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *target
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(brackets, .type = Token_Type_Square);

  Value *array = value_any();
  token_force_value(context, target_token, scope, builder, array);
  Token_Matcher_State *index_state = &(Token_Matcher_State) {brackets->children};
  Value *index_value = value_any();
  token_match_expression(context, index_state, scope, builder, index_value);
  assert(array->descriptor->type == Descriptor_Type_Fixed_Size_Array);
  assert(array->operand.type == Operand_Type_Memory_Indirect);

  Descriptor *item_descriptor = array->descriptor->array.item;
  u32 item_byte_size = descriptor_byte_size(item_descriptor);

  Value *result = temp_allocate(Value);
  if (operand_is_immediate(&index_value->operand)) {
    s32 index = s64_to_s32(operand_immediate_as_s64(&index_value->operand));
    *result = (Value){
      .descriptor = item_descriptor,
      .operand = {
        .type = Operand_Type_Memory_Indirect,
        .byte_size = item_byte_size,
        .indirect = (Operand_Memory_Indirect) {
          .reg = array->operand.indirect.reg,
          .displacement = array->operand.indirect.displacement + index * item_byte_size,
        }
      }
    };

  } else if(
    item_byte_size == 1 ||
    item_byte_size == 2 ||
    item_byte_size == 4 ||
    item_byte_size == 8
  ) {
    SIB_Scale scale = SIB_Scale_1;
    if (item_byte_size == 2) {
      scale = SIB_Scale_2;
    } else if (item_byte_size == 4) {
      scale = SIB_Scale_4;
    } else if (item_byte_size == 8) {
      scale = SIB_Scale_8;
    }
    Value *index_value_in_register =
      value_register_for_descriptor(Register_R10, index_value->descriptor);
    move_value(builder, &target_token->source_range, index_value_in_register, index_value);
    *result = (Value){
      .descriptor = item_descriptor,
      .operand = {
        .type = Operand_Type_Sib,
        .byte_size = item_byte_size,
        .sib = (Operand_Sib) {
          .scale = scale,
          .index = index_value_in_register->operand.reg,
          .base = array->operand.indirect.reg,
          .displacement = array->operand.indirect.displacement,
        }
      }
    };
  } else {
    panic("TODO");
  }

  Token *token_value = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 2, token_value);
  return true;
}

bool
token_rewrite_struct_field(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  u64 peek_index = 0;
  Token_Match(struct_token, 0);
  Token_Match_Operator(dot, ".");
  Token_Match(field_name, .type = Token_Type_Id);

  Value *struct_value = value_any();
  token_force_value(context, struct_token, scope, builder, struct_value);
  Value *result = struct_get_field(struct_value, field_name->source);

  Token *token_value = token_value_make(result, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}

bool
token_rewrite_assignment(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder
) {
  Array_Token_Ptr lhs;
  Array_Token_Ptr rhs;
  Token *operator;
  if (!token_maybe_split_on_operator(state->tokens, slice_literal("="), &lhs, &rhs, &operator)) {
    return false;
  }

  Token_Matcher_State lhs_state = {lhs};
  token_rewrite_expression(context, &lhs_state, scope, builder, 0, token_rewrite_array_index);
  token_rewrite_statement(context, &lhs_state, scope, builder, token_rewrite_struct_field);
  token_rewrite_statement(context, &lhs_state, scope, builder, token_rewrite_definitions);
  if (!dyn_array_length(lhs_state.tokens)) {
    panic("Left hand side is checked to be non-empty when matched so something went wrong");
  }
  Value *target = value_any();
  if (dyn_array_length(lhs_state.tokens) == 1) {
    Token *token = *dyn_array_get(lhs_state.tokens, 0);
    token_force_value(context, token, scope, builder, target);
  } else {
    Token *first_token = *dyn_array_get(lhs_state.tokens, 0);
    program_push_error_from_slice(
      context->program,
      first_token->source_range,
      slice_literal("Could not parse the target of the assignment")
    );
  }

  Token_Matcher_State rhs_state = {rhs};
  token_match_expression(context, &rhs_state, scope, builder, target);

  token_replace_tokens_in_state(state, dyn_array_length(state->tokens), 0);
  return true;
}

bool
token_rewrite_function_calls(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(target_token, 0);
  Token_Match(args_token, .type = Token_Type_Paren);
  if (target_token->type != Token_Type_Id && target_token->type != Token_Type_Paren) return false;

  Value *target = value_any();
  token_force_value(context, target_token, scope, builder, target);

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
      if (!register_bitset_get(builder->code_block.register_occupied_bitset, reg_index)) {
        register_acquire(builder, reg_index);
        acquired_registers[i] = true;
      }
    }

    args = token_match_call_arguments(context, args_token, scope, builder);

    // Release any registers that we fake acquired to make sure that the actual call
    // does not unnecessarily store them to stack
    for (uint64_t i = 0; i < countof(arg_registers); ++i) {
      if (acquired_registers[i]) {
        Register reg_index = arg_registers[i];
        register_release(builder, reg_index);
      }
    }
  }

  if (target->descriptor->type != Descriptor_Type_Function) {
    program_error_builder(context->program, target_token->source_range) {
      program_error_append_slice(target_token->source);
      program_error_append_literal(" is not a function");
    }
    return false;
  }
  Source_Range *source_range = &target_token->source_range;
  Value *overload = find_matching_function_overload(builder, target, args);
  if (overload) {
    Value *return_value;
    Descriptor_Function *function = &overload->descriptor->function;
    if (function->inline_body) {
      assert(function->parent_scope);
      // We make a nested scope based on function's original parent scope
      // instead of current scope for hygiene reasons. I.e. function body
      // should not have access to locals inside the call scope.
      Scope *body_scope = scope_make(function->parent_scope);

      for (u64 i = 0; i < dyn_array_length(function->arguments); ++i) {
        Slice arg_name = *dyn_array_get(function->argument_names, i);
        Value *arg_value = *dyn_array_get(args, i);
        scope_define_value(body_scope, arg_name, arg_value);
      }
      return_value = result_value;

      // We need to have a fake builder so that return target label and
      // the return types are correct
      Function_Builder inline_builder = *builder;
      {
        inline_builder.code_block.end_label =
          make_label(context->program, &context->program->code_section);
        inline_builder.descriptor = &(Descriptor) {0};
        *inline_builder.descriptor = *builder->descriptor;
        inline_builder.descriptor->function.returns = result_value;
      }

      token_parse_block(context, function->inline_body, body_scope, &inline_builder, return_value);

      // Because instructions are stored in a dynamic array it might have been
      // reallocated which means we need to copy it. It might be better to
      // switch to a bucket array.
      builder->code_block.instructions = inline_builder.code_block.instructions;

      push_instruction(
        &builder->code_block.instructions,
        &target_token->source_range,
        (Instruction) {
          .type = Instruction_Type_Label,
          .label = inline_builder.code_block.end_label
        }
      );
    } else {
      return_value = call_function_overload(builder, source_range, overload, args);
    }
    Token *token_value = token_value_make(return_value, TOKEN_MATCHED_SOURCE());
    token_replace_tokens_in_state(state, 2, token_value);
  } else {
    program_error_builder(context->program, target_token->source_range) {
      // TODO add better error message
      program_error_append_literal("Could not find matching overload");
    }
    token_replace_tokens_in_state(state, 2, 0);
  }
  dyn_array_destroy(args);
  return true;
}

bool
token_rewrite_plus(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "+");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(context, rhs, scope, builder, rhs_value);
  plus(builder, &op_token->source_range, result_value, lhs_value, rhs_value);
  Token *token_value = token_value_make(result_value, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}

bool
token_rewrite_minus(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "-");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(context, rhs, scope, builder, rhs_value);
  minus(builder, &op_token->source_range, result_value, lhs_value, rhs_value);
  Token *token_value = token_value_make(result_value, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}


bool
token_rewrite_divide(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "/");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(context, rhs, scope, builder, rhs_value);
  divide(builder, &op_token->source_range, result_value, lhs_value, rhs_value);
  Token *token_value = token_value_make(result_value, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}

bool
token_rewrite_remainder(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match_Operator(op_token, "%");
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(context, rhs, scope, builder, rhs_value);
  Value *temp = reserve_stack(builder, lhs_value->descriptor);
  value_remainder(builder, &op_token->source_range, temp, lhs_value, rhs_value);
  Token *token_value = token_value_make(temp, TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}

bool
token_rewrite_compare(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  u64 peek_index = 0;
  Token_Match(lhs, 0);
  Token_Match(operator, .type = Token_Type_Operator);
  Compare_Type compare_type = 0;
  switch(operator->source.length) {
    case 1: {
      s8 byte1 = operator->source.bytes[0];
      if (byte1 == '<') {
        compare_type = Compare_Type_Signed_Less;
      } else if (byte1 == '>') {
        compare_type = Compare_Type_Signed_Greater;
      } else {
        return 0;
      }
      break;
    }
    case 2: {
      s8 byte1 = operator->source.bytes[0];
      s8 byte2 = operator->source.bytes[1];
      if (byte1 == '<' && byte2 == '=') {
        compare_type = Compare_Type_Signed_Less_Equal;
      } else if (byte1 == '>' && byte2 == '=') {
        compare_type = Compare_Type_Signed_Greater_Equal;
      } else if (byte1 == '=' && byte2 == '=') {
        compare_type = Compare_Type_Equal;
      } else if (byte1 == '!' && byte2 == '=') {
        compare_type = Compare_Type_Equal;
      } else {
        return 0;
      }
      break;
    }
    default: {
      return 0;
    }
  }
  Token_Match(rhs, 0);

  Value *lhs_value = value_any();
  token_force_value(context, lhs, scope, builder, lhs_value);
  Value *rhs_value = value_any();
  token_force_value(context, rhs, scope, builder, rhs_value);

  // FIXME add implicit unsigned to signed conversion
  if (
    lhs_value->descriptor->type != Descriptor_Type_Integer ||
    rhs_value->descriptor->type != Descriptor_Type_Integer
  ) {
    panic("FIXME handle errors here");
  }

  if (lhs_value->descriptor->integer.is_signed != rhs_value->descriptor->integer.is_signed) {
    // FIXME solve generally
    if (!rhs_value->descriptor->integer.is_signed && operand_is_immediate(&rhs_value->operand)) {
      switch(lhs_value->operand.byte_size) {
        case 1: {
          if (u8_fits_into_s8(rhs_value->operand.u8)) {
            Value *adjusted = temp_allocate(Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s8;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 2: {
          if (u16_fits_into_s16(rhs_value->operand.u16)) {
            Value *adjusted = temp_allocate(Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s16;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 4: {
          if (u32_fits_into_s32(rhs_value->operand.u32)) {
            Value *adjusted = temp_allocate(Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s32;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        case 8: {
          if (u64_fits_into_s64(rhs_value->operand.u64)) {
            Value *adjusted = temp_allocate(Value);
            *adjusted = *rhs_value;
            adjusted->descriptor = &descriptor_s64;
            rhs_value = adjusted;
          } else {
            panic("FIXME report immediate overflow");
          }
          break;
        }
        default: {
          panic("Internal Error: Unexpected integer size");
          break;
        }
      }
    } else {
      panic("FIXME handle errors here");
    }
  }

  if (!lhs_value->descriptor->integer.is_signed) {
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

  compare(compare_type, builder, &operator->source_range, result_value, lhs_value, rhs_value);
  Token *token_value = token_value_make(result_value,TOKEN_MATCHED_SOURCE());
  token_replace_tokens_in_state(state, 3, token_value);
  return true;
}

void
token_parse_statement(
  Compilation_Context *context,
  Token_Matcher_State *state,
  const Source_Range *source_range,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  // TODO consider how this should work
  token_rewrite_macros(state, scope, builder);

  if (
    token_rewrite_inline_machine_code_bytes(context, state, scope, builder) ||
    token_rewrite_assignment(context, state, scope, builder) ||
    token_rewrite_definition_and_assignment_statements( context, state, scope, builder) ||
    token_rewrite_definitions(context, state, scope, builder) ||
    token_rewrite_explicit_return(context, state, scope, builder) ||
    token_rewrite_goto(context, state, scope, builder) ||
    token_rewrite_constant_definitions(context, state, scope, builder)||
    token_rewrite_statement_if(context, state, scope, builder)
  ) {
    return;
  }
  token_match_expression(context, state, scope, builder, result_value);
}

void
token_match_expression(
  Compilation_Context *context,
  Token_Matcher_State *state,
  Scope *scope,
  Function_Builder *builder,
  Value *result_value
) {
  if (!dyn_array_length(state->tokens)) {
    return;
  }
  token_state_clear_newlines(state);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_cast);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_struct_field);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_function_literal);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_negative_literal);
  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_function_calls);
  token_rewrite_statement(context, state, scope, builder, token_rewrite_pointer_to);

  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_divide);
  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_remainder);
  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_plus);
  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_minus);

  token_rewrite_expression(context, state, scope, builder, result_value, token_rewrite_compare);

  switch(dyn_array_length(state->tokens)) {
    case 0: {
      return;
    }
    case 1: {
      Token *token = *dyn_array_get(state->tokens, 0);
      token_force_value(context, token, scope, builder, result_value);
      return;
    }
    default: {
      Token *token = *dyn_array_get(state->tokens, 0);
      program_error_builder(context->program, token->source_range) {
        program_error_append_literal("Could not parse the expression");
      }
      return;
    }
  }
}

bool
token_parse_module(
  Token *token,
  Compilation_Context *context
) {
  if (token->type != Token_Type_Module) {
    panic("Caller must provide a value known to be a module");
  }
  if (!dyn_array_length(token->children)) return true;

  Function_Builder global_builder = { 0 };

  Array_Token_Matcher_State module_statements =
    token_split_by_newlines_and_semicolons(token->children);
  for (u64 i = 0; i < dyn_array_length(module_statements); ++i) {
    Token_Matcher_State *state = dyn_array_get(module_statements, i);
    if (!dyn_array_length(state->tokens)) continue;
    token_rewrite_macro_definitions(state, context->program->global_scope);
    token_rewrite_statement(
      context, state, context->program->global_scope,
      &global_builder, token_rewrite_constant_definitions
    );
    // Detect unmatched statements
    if (dyn_array_length(state->tokens)) {
      Source_Range source_range =
        source_range_from_token_matcher_state(state, dyn_array_length(state->tokens));
      program_error_builder(context->program, source_range) {
        program_error_append_literal("Could not parse a top level statement");
      }
      return false;
    }
  }

  return true;
}

Parse_Result
program_parse(
  Compilation_Context *context,
  Source_File *file
) {
  Tokenizer_Result tokenizer_result = tokenize(file);
  if (tokenizer_result.type != Tokenizer_Result_Type_Success) {
    return (Parse_Result) {
      .type = Parse_Result_Type_Error,
      .errors = tokenizer_result.errors,
    };
  }
  ;
  return (Parse_Result) {
    .type = token_parse_module(tokenizer_result.root, context)
      ? Parse_Result_Type_Success
      : Parse_Result_Type_Error
  };
}

Fixed_Buffer *
win32_absolute_path(
  Slice raw_path
) {
  Slice result_path = raw_path;
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
  Fixed_Buffer *result_buffer = fixed_buffer_make(
    .allocator = allocator_system,
    .capacity = result_path.length + 1024
  );

  fixed_buffer_append_slice(result_buffer, result_path);

  if (sys_buffer) fixed_buffer_destroy(sys_buffer);
  return result_buffer;
}

#define STRINGIFY(x) #x

Parse_Result
program_import_file(
  Compilation_Context *context,
  Slice file_path
) {
  Slice extension = slice_literal(".mass");
  Fixed_Buffer *absolute_path = win32_absolute_path(file_path);

  if (!slice_ends_with(fixed_buffer_as_slice(absolute_path), extension)) {
    fixed_buffer_append_slice(absolute_path, extension);
    file_path = fixed_buffer_as_slice(absolute_path);
  }
  Source_File *file = temp_allocate(Source_File);
  *file = (Source_File) {
    .path = file_path,
    .text = {0},
  };
  Fixed_Buffer *buffer = fixed_buffer_from_file(file_path, .allocator = allocator_system);
  if (!buffer) {
    Array_Parse_Error errors = dyn_array_make(Array_Parse_Error);
    dyn_array_push(errors, (Parse_Error) {
      .message = slice_literal("Unable to open the file"),
      .source_range = {
        .file = file,
      },
    });
    return (Parse_Result) {
      .type = Parse_Result_Type_Error,
      .errors = errors,
    };
  }
  file->text = fixed_buffer_as_slice(buffer);
  return program_parse(context, file);
}

