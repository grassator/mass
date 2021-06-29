#ifndef RUNTIME_H
#define RUNTIME_H

#include "types.h"

static void
program_init(
  Allocator *allocator,
  Program *program,
  const Calling_Convention *default_calling_convention
);

static void
program_deinit(
  Program *program
);

static void
program_set_label_offset(
  Program *program,
  Label_Index label_index,
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
  Execution_Context *context,
  const Slice library_name,
  const Slice symbol_name
);

static void
program_jit(
  Compilation *compilation,
  Jit *jit
);


#endif // RUNTIME_H
