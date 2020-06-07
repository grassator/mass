#include "value.h"

typedef enum {
  Instruction_Extension_Type_None,
  Instruction_Extension_Type_Register,
  Instruction_Extension_Type_Op_Code,
  Instruction_Extension_Type_Plus_Register,
} Instruction_Extension_Type;

typedef enum {
  Operand_Encoding_Type_None,
  Operand_Encoding_Type_Op_Code_Plus_Register,
  Operand_Encoding_Type_Register,
  Operand_Encoding_Type_Register_Memory,
  Operand_Encoding_Type_Immediate_8,
  Operand_Encoding_Type_Immediate_32,
  Operand_Encoding_Type_Immediate_64,
} Operand_Encoding_Type;

typedef struct {
  u8 op_code[2];
  Instruction_Extension_Type extension_type;
  u8 op_code_extension;
  Operand_Encoding_Type operand_encoding_types[3];
} Instruction_Encoding;

typedef struct {
  const char *name;
  const Instruction_Encoding *encoding_list;
  u32 encoding_count;
} X64_Mnemonic;

typedef struct {
  const X64_Mnemonic mnemonic;
  Operand operands[3];
} Instruction;

////////////////////////////////////////////////////////////////////////////////
// mov
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding mov_encoding_list[] = {
  {
    .op_code = { 0x00, 0x89 },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x8B },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0xc7 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 0,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0xb8 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Op_Code_Plus_Register,
      Operand_Encoding_Type_Immediate_64,
      Operand_Encoding_Type_None
    },
  },
};

const X64_Mnemonic mov = {
  .name = "mov",
  .encoding_list = (const Instruction_Encoding *)mov_encoding_list,
  .encoding_count = static_array_size(mov_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// ret
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding ret_encoding_list[] = {
  {
    .op_code = { 0x00, 0xc3 },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic ret = {
  .name = "ret",
  .encoding_list = (const Instruction_Encoding *)ret_encoding_list,
  .encoding_count = static_array_size(ret_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// inc
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding inc_encoding_list[] = {
  {
    .op_code = { 0x00, 0xFF },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 0,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic inc = {
  .name = "inc",
  .encoding_list = (const Instruction_Encoding *)inc_encoding_list,
  .encoding_count = static_array_size(inc_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// add
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding add_encoding_list[] = {
  {
    .op_code = { 0x00, 0x03 },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x83 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 0,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_8,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x81 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 0,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic add = {
  .name = "add",
  .encoding_list = (const Instruction_Encoding *)add_encoding_list,
  .encoding_count = static_array_size(add_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// sub
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding sub_encoding_list[] = {
  {
    .op_code = { 0x00, 0x29 },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x2B },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x83 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 5,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_8,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x81 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 5,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic sub = {
  .name = "sub",
  .encoding_list = (const Instruction_Encoding *)sub_encoding_list,
  .encoding_count = static_array_size(sub_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// imul
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding imul_encoding_list[] = {
  {
    .op_code = { 0x0F, 0xAF },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x69 },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_32
    },
  },
};
const X64_Mnemonic imul = {
  .name = "imul",
  .encoding_list = (const Instruction_Encoding *)imul_encoding_list,
  .encoding_count = static_array_size(imul_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// idiv
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding idiv_encoding_list[] = {
  {
    .op_code = { 0x00, 0xF7 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 7,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic idiv = {
  .name = "idiv",
  .encoding_list = (const Instruction_Encoding *)idiv_encoding_list,
  .encoding_count = static_array_size(idiv_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// cwd/cdq/cqo
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding cqo_encoding_list[] = {
  {
    .op_code = { 0x48, 0x99 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .operand_encoding_types = {
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};

const X64_Mnemonic cqo = {
  .name = "cqo",
  .encoding_list = (const Instruction_Encoding *)cqo_encoding_list,
  .encoding_count = static_array_size(cqo_encoding_list),
};

const Instruction_Encoding cdq_encoding_list[] = {
  {
    .op_code = { 0x00, 0x99 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .operand_encoding_types = {
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic cdq = {
  .name = "cdq",
  .encoding_list = (const Instruction_Encoding *)cdq_encoding_list,
  .encoding_count = static_array_size(cdq_encoding_list),
};

const Instruction_Encoding cwd_encoding_list[] = {
  {
    .op_code = { 0x66, 0x99 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .operand_encoding_types = {
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};

const X64_Mnemonic cwd = {
  .name = "cwd",
  .encoding_list = (const Instruction_Encoding *)cwd_encoding_list,
  .encoding_count = static_array_size(cwd_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// call
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding call_encoding_list[] = {
  {
    .op_code = { 0x00, 0xE8 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0xFF },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 2,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic call = {
  .name = "call",
  .encoding_list = (const Instruction_Encoding *)call_encoding_list,
  .encoding_count = static_array_size(call_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// cmp
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding cmp_encoding_list[] = {
  {
    .op_code = { 0x00, 0x81 },
    .extension_type = Instruction_Extension_Type_Op_Code,
    .op_code_extension = 7,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0x3B },
    .extension_type = Instruction_Extension_Type_Register,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register,
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic cmp = {
  .name = "cmp",
  .encoding_list = (const Instruction_Encoding *)cmp_encoding_list,
  .encoding_count = static_array_size(cmp_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jnz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jnz_encoding_list[] = {
  {
    .op_code = { 0x00, 0x75 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_8,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x0F, 0x85 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic jnz = {
  .name = "jnz",
  .encoding_list = (const Instruction_Encoding *)jnz_encoding_list,
  .encoding_count = static_array_size(jnz_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jz_encoding_list[] = {
  {
    .op_code = { 0x0F, 0x84 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic jz = {
  .name = "jz",
  .encoding_list = (const Instruction_Encoding *)jz_encoding_list,
  .encoding_count = static_array_size(jz_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// setz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setz_encoding_list[] = {
  {
    .op_code = { 0x0F, 0x94 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic setz = {
  .name = "setz",
  .encoding_list = (const Instruction_Encoding *)setz_encoding_list,
  .encoding_count = static_array_size(setz_encoding_list),
};
const X64_Mnemonic sete = {
  .name = "sete",
  .encoding_list = (const Instruction_Encoding *)setz_encoding_list,
  .encoding_count = static_array_size(setz_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// setl
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setl_encoding_list[] = {
  {
    .op_code = { 0x0F, 0x9C },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      // TODO encoding that this only operates on a byte value
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic setl = {
  .name = "setl",
  .encoding_list = (const Instruction_Encoding *)setl_encoding_list,
  .encoding_count = static_array_size(setl_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// setg
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setg_encoding_list[] = {
  {
    .op_code = { 0x0F, 0x9F },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      // TODO encoding that this only operates on a byte value
      Operand_Encoding_Type_Register_Memory,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic setg = {
  .name = "setg",
  .encoding_list = (const Instruction_Encoding *)setg_encoding_list,
  .encoding_count = static_array_size(setg_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jmp
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jmp_encoding_list[] = {
  {
    .op_code = { 0x00, 0xEB },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_8,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
  {
    .op_code = { 0x00, 0xE9 },
    .extension_type = Instruction_Extension_Type_None,
    .operand_encoding_types = {
      Operand_Encoding_Type_Immediate_32,
      Operand_Encoding_Type_None,
      Operand_Encoding_Type_None
    },
  },
};
const X64_Mnemonic jmp = {
  .name = "jmp",
  .encoding_list = (const Instruction_Encoding *)jmp_encoding_list,
  .encoding_count = static_array_size(jmp_encoding_list),
};
