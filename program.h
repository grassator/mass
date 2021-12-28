#ifndef RUNTIME_H
#define RUNTIME_H

#include "types.h"

static void
program_init(
  Allocator *allocator,
  Program *program,
  Os os
);

static void
program_deinit(
  Program *program
);

static inline void
program_resolve_label(
  Program *program,
  Virtual_Memory_Buffer *buffer,
  Label *label
);

static void
program_set_label_offset(
  Program *program,
  Label *label,
  u32 offset_in_section
);

static inline u32
program_resolve_label_to_rva(
  const Program *program,
  const Label *label
) {
  return label->section->base_rva + label->offset_in_section;
}

static void
program_patch_labels(
  Program *program
);

static Import_Library *
program_find_import_library(
  const Program *program,
  const Slice library_name
);

static Import_Symbol *
import_library_find_symbol(
  const Import_Library *library,
  const Slice symbol_name
);

static Import_Symbol *
program_find_import(
  const Program *program,
  const Slice library_name,
  const Slice symbol_name
);

static Storage
import_symbol(
  const Allocator *allocator,
  Program *program,
  const Slice library_name,
  const Slice symbol_name
);

static void
program_jit(
  Mass_Context *context,
  Jit *jit
);

typedef struct {
  void *(*load_library)(const char *name);
  void *(*load_symbol)(void *handle, const char *name);
} Native_Library_Load_Callbacks;

static void
program_jit_imports(
  Compilation *compilation,
  Jit *jit,
  Virtual_Memory_Buffer *ro_data_buffer,
  const Native_Library_Load_Callbacks *callbacks
);


static void
program_jit_resolve_relocations(
  Jit *jit
);

#endif // RUNTIME_H
