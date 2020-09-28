#include "value.h"

#define none .extension_type = Instruction_Extension_Type_None,
#define _r .extension_type = Instruction_Extension_Type_Register,
#define plus_r .extension_type = Instruction_Extension_Type_Plus_Register,
#define _op_code(_extension_)\
  .extension_type = Instruction_Extension_Type_Op_Code,\
  .op_code_extension = (_extension_ & 0b111),


#define r_al  { Operand_Encoding_Type_Register_A, Operand_Size_8 }
#define r_ax  { Operand_Encoding_Type_Register_A, Operand_Size_16 }
#define r_eax { Operand_Encoding_Type_Register_A, Operand_Size_32 }
#define r_rax { Operand_Encoding_Type_Register_A, Operand_Size_64 }

#define r8  { Operand_Encoding_Type_Register, Operand_Size_8 }
#define r16 { Operand_Encoding_Type_Register, Operand_Size_16 }
#define r32 { Operand_Encoding_Type_Register, Operand_Size_32 }
#define r64 { Operand_Encoding_Type_Register, Operand_Size_64 }

#define r_m8  { Operand_Encoding_Type_Register_Memory, Operand_Size_8 }
#define r_m16 { Operand_Encoding_Type_Register_Memory, Operand_Size_16 }
#define r_m32 { Operand_Encoding_Type_Register_Memory, Operand_Size_32 }
#define r_m64 { Operand_Encoding_Type_Register_Memory, Operand_Size_64 }

#define m   { Operand_Encoding_Type_Memory, Operand_Size_Any }
#define m8  { Operand_Encoding_Type_Memory, Operand_Size_8 }
#define m16 { Operand_Encoding_Type_Memory, Operand_Size_16 }
#define m32 { Operand_Encoding_Type_Memory, Operand_Size_32 }
#define m64 { Operand_Encoding_Type_Memory, Operand_Size_64 }

#define imm8  { Operand_Encoding_Type_Immediate, Operand_Size_8 }
#define imm16 { Operand_Encoding_Type_Immediate, Operand_Size_16 }
#define imm32 { Operand_Encoding_Type_Immediate, Operand_Size_32 }
#define imm64 { Operand_Encoding_Type_Immediate, Operand_Size_64 }

#define xmm32 { Operand_Encoding_Type_Xmm, Operand_Size_32 }
#define xmm_m32 { Operand_Encoding_Type_Xmm_Memory, Operand_Size_32 }
#define xmm64 { Operand_Encoding_Type_Xmm, Operand_Size_64 }
#define xmm_m64 { Operand_Encoding_Type_Xmm_Memory, Operand_Size_64 }

#define encoding_operands(...) __VA_ARGS__

#define encoding(_op_code_u16_, _extension_type_, ...)\
  {\
    .op_code = {\
      ((_op_code_u16_) >> 24) & 0xFFu,\
      ((_op_code_u16_) >> 16) & 0xFFu,\
      ((_op_code_u16_) >> 8) & 0xFFu,\
      (_op_code_u16_) & 0xFFu\
    },\
    _extension_type_\
    .operands = { encoding_operands(__VA_ARGS__) },\
  }

////////////////////////////////////////////////////////////////////////////////
// mov
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding mov_encoding_list[] = {
  encoding(0x88, _r, r_m8, r8),
  encoding(0x89, _r, r_m16, r16),
  encoding(0x89, _r, r_m32, r32),
  encoding(0x89, _r, r_m64, r64),

  encoding(0x8A, _r, r8, r_m8),
  encoding(0x8B, _r, r16, r_m16),
  encoding(0x8B, _r, r32, r_m32),
  encoding(0x8B, _r, r64, r_m64),

  encoding(0xC6, _op_code(0), r_m8, imm8),

  encoding(0xC7, _op_code(0), r_m16, imm16),
  encoding(0xC7, _op_code(0), r_m32, imm32),
  encoding(0xC7, _op_code(0), r_m64, imm32),

  encoding(0xB8, plus_r, r16, imm16),
  encoding(0xB8, plus_r, r32, imm32),
  encoding(0xB8, plus_r, r64, imm64),
};

const X64_Mnemonic mov = {
  .name = "mov",
  .encoding_list = (const Instruction_Encoding *)mov_encoding_list,
  .encoding_count = countof(mov_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// movsx
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding movsx_encoding_list[] = {
  encoding(0x0FBE, _r, r16, r_m8),
  encoding(0x0FBE, _r, r32, r_m8),
  encoding(0x0FBE, _r, r64, r_m8),
  encoding(0x0FBF, _r, r32, r_m16),
  encoding(0x0FBF, _r, r64, r_m16),
};

const X64_Mnemonic movsx = {
  .name = "movsx",
  .encoding_list = (const Instruction_Encoding *)movsx_encoding_list,
  .encoding_count = countof(movsx_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// movss
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding movss_encoding_list[] = {
  encoding(0xF30F10, _r, xmm32, xmm_m32),
  encoding(0xF30F11, _r, xmm_m32, xmm32),
};

const X64_Mnemonic movss = {
  .name = "movss",
  .encoding_list = (const Instruction_Encoding *)movss_encoding_list,
  .encoding_count = countof(movss_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// movsd
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding movsd_encoding_list[] = {
  encoding(0xF20F10, _r, xmm64, xmm_m64),
  encoding(0xF20F11, _r, xmm_m64, xmm64),
};

const X64_Mnemonic movsd = {
  .name = "movss",
  .encoding_list = (const Instruction_Encoding *)movsd_encoding_list,
  .encoding_count = countof(movss_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// lea
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding lea_encoding_list[] = {
  encoding(0x8d, _r, r64, m),
};
const X64_Mnemonic lea = {
  .name = "lea",
  .encoding_list = (const Instruction_Encoding *)lea_encoding_list,
  .encoding_count = countof(lea_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// int8
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding int3_encoding_list[] = {
  encoding(0xCC, none, 0),
};
const X64_Mnemonic int3 = {
  .name = "int3",
  .encoding_list = (const Instruction_Encoding *)int3_encoding_list,
  .encoding_count = countof(int3_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// ret
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding ret_encoding_list[] = {
  encoding(0xC3, none, 0),
};
const X64_Mnemonic ret = {
  .name = "ret",
  .encoding_list = (const Instruction_Encoding *)ret_encoding_list,
  .encoding_count = countof(ret_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// inc
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding inc_encoding_list[] = {
  encoding(0xFF, _op_code(0), r_m16),
  encoding(0xFF, _op_code(0), r_m32),
  encoding(0xFF, _op_code(0), r_m64),
};
const X64_Mnemonic inc = {
  .name = "inc",
  .encoding_list = (const Instruction_Encoding *)inc_encoding_list,
  .encoding_count = countof(inc_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// xor
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding xor_encoding_list[] = {
  encoding(0x34, none, r_al, imm8),
  encoding(0x35, none, r_ax, imm16),
  encoding(0x35, none, r_eax, imm32),
  encoding(0x35, none, r_rax, imm32),

  encoding(0x32, _r, r8, r_m8),
  encoding(0x33, _r, r16, r_m16),
  encoding(0x33, _r, r32, r_m32),
  encoding(0x33, _r, r64, r_m64),
};
const X64_Mnemonic xor = {
  .name = "xor",
  .encoding_list = (const Instruction_Encoding *)xor_encoding_list,
  .encoding_count = countof(xor_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// add
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding add_encoding_list[] = {
  encoding(0x04, none, r_al, imm8),
  encoding(0x05, none, r_ax, imm16),
  encoding(0x05, none, r_eax, imm32),
  encoding(0x05, none, r_rax, imm32),

  encoding(0x02, _r, r8, r_m8),
  encoding(0x03, _r, r16, r_m16),
  encoding(0x03, _r, r32, r_m32),
  encoding(0x03, _r, r64, r_m64),

  encoding(0x80, _op_code(0), r_m8, imm8),
  encoding(0x81, _op_code(0), r_m16, imm16),
  encoding(0x81, _op_code(0), r_m32, imm32),
  encoding(0x81, _op_code(0), r_m64, imm32),

  encoding(0x83, _op_code(0), r_m16, imm8),
  encoding(0x83, _op_code(0), r_m32, imm8),
  encoding(0x83, _op_code(0), r_m64, imm8),
};
const X64_Mnemonic add = {
  .name = "add",
  .encoding_list = (const Instruction_Encoding *)add_encoding_list,
  .encoding_count = countof(add_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// addss
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding addss_encoding_list[] = {
  encoding(0xF30F58, _r, xmm32, xmm_m32),
};

const X64_Mnemonic addss = {
  .name = "addss",
  .encoding_list = (const Instruction_Encoding *)addss_encoding_list,
  .encoding_count = countof(addss_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// sub
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding sub_encoding_list[] = {
  encoding(0x2C, none, r_al, imm8),
  encoding(0x2D, none, r_ax, imm16),
  encoding(0x2D, none, r_eax, imm32),
  encoding(0x2D, none, r_rax, imm32),

  encoding(0x2A, _r, r8, r_m8),
  encoding(0x2B, _r, r16, r_m16),
  encoding(0x2B, _r, r32, r_m32),
  encoding(0x2B, _r, r64, r_m64),

  encoding(0x28, _r, r_m8, r8),
  encoding(0x29, _r, r_m16, r16),
  encoding(0x29, _r, r_m32, r32),
  encoding(0x29, _r, r_m64, r64),

  encoding(0x80, _op_code(5), r_m8, imm8),
  encoding(0x81, _op_code(5), r_m16, imm16),
  encoding(0x81, _op_code(5), r_m32, imm32),
  encoding(0x81, _op_code(5), r_m64, imm32),

  encoding(0x83, _op_code(5), r_m16, imm8),
  encoding(0x83, _op_code(5), r_m32, imm8),
  encoding(0x83, _op_code(5), r_m64, imm8),
};
const X64_Mnemonic sub = {
  .name = "sub",
  .encoding_list = (const Instruction_Encoding *)sub_encoding_list,
  .encoding_count = countof(sub_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// imul
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding imul_encoding_list[] = {
  encoding(0x0FAF, _r, r16, r_m16),
  encoding(0x0FAF, _r, r32, r_m32),
  encoding(0x0FAF, _r, r64, r_m64),

  encoding(0x69, _r, r16, r_m16, imm16),
  encoding(0x69, _r, r32, r_m32, imm32),
  encoding(0x69, _r, r64, r_m64, imm32),
};
const X64_Mnemonic imul = {
  .name = "imul",
  .encoding_list = (const Instruction_Encoding *)imul_encoding_list,
  .encoding_count = countof(imul_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// idiv
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding idiv_encoding_list[] = {
  encoding(0xF6, _op_code(7), r_m8),
  encoding(0xF7, _op_code(7), r_m16),
  encoding(0xF7, _op_code(7), r_m32),
  encoding(0xF7, _op_code(7), r_m64),
};
const X64_Mnemonic idiv = {
  .name = "idiv",
  .encoding_list = (const Instruction_Encoding *)idiv_encoding_list,
  .encoding_count = countof(idiv_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// cwb/cwd/cdq/cqo
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding cqo_encoding_list[] = {
  encoding(0x4899, none, 0),
};

const X64_Mnemonic cqo = {
  .name = "cqo",
  .encoding_list = (const Instruction_Encoding *)cqo_encoding_list,
  .encoding_count = countof(cqo_encoding_list),
};

const Instruction_Encoding cdq_encoding_list[] = {
  encoding(0x99, none, 0),
};
const X64_Mnemonic cdq = {
  .name = "cdq",
  .encoding_list = (const Instruction_Encoding *)cdq_encoding_list,
  .encoding_count = countof(cdq_encoding_list),
};

const Instruction_Encoding cwd_encoding_list[] = {
  encoding(0x66, none, 0),
};

const X64_Mnemonic cwd = {
  .name = "cwd",
  .encoding_list = (const Instruction_Encoding *)cwd_encoding_list,
  .encoding_count = countof(cwd_encoding_list),
};

const Instruction_Encoding cwb_encoding_list[] = {
  encoding(0x98, none, 0),
};

const X64_Mnemonic cwb = {
  .name = "cwb",
  .encoding_list = (const Instruction_Encoding *)cwb_encoding_list,
  .encoding_count = countof(cwb_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// call
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding call_encoding_list[] = {
  encoding(0xE8, none, imm16),
  encoding(0xE8, none, imm32),

  encoding(0xFF, _op_code(2), r_m16),
  encoding(0xFF, _op_code(2), r_m32),
  encoding(0xFF, _op_code(2), r_m64),
};
const X64_Mnemonic call = {
  .name = "call",
  .encoding_list = (const Instruction_Encoding *)call_encoding_list,
  .encoding_count = countof(call_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// cmp
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding cmp_encoding_list[] = {
  encoding(0x3C, none, r_al, imm8),
  encoding(0x3D, none, r_ax, imm16),
  encoding(0x3D, none, r_eax, imm32),
  encoding(0x3D, none, r_rax, imm32),

  encoding(0x80, _op_code(7), r_m8, imm8),
  encoding(0x81, _op_code(7), r_m16, imm16),
  encoding(0x81, _op_code(7), r_m32, imm32),
  encoding(0x81, _op_code(7), r_m64, imm32),

  encoding(0x38, _r, r_m8, r8),
  encoding(0x39, _r, r_m16, r16),
  encoding(0x39, _r, r_m32, r32),
  encoding(0x39, _r, r_m64, r64),

  encoding(0x3A, _r, r8, r_m8),
  encoding(0x3B, _r, r16, r_m16),
  encoding(0x3B, _r, r32, r_m32),
  encoding(0x3B, _r, r64, r_m64),
};
const X64_Mnemonic cmp = {
  .name = "cmp",
  .encoding_list = (const Instruction_Encoding *)cmp_encoding_list,
  .encoding_count = countof(cmp_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jnz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jnz_encoding_list[] = {
  encoding(0x75, none, imm8),
  encoding(0x0F85, none, imm32),
};
const X64_Mnemonic jnz = {
  .name = "jnz",
  .encoding_list = (const Instruction_Encoding *)jnz_encoding_list,
  .encoding_count = countof(jnz_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jz_encoding_list[] = {
  encoding(0x0F84, none, imm32),
};
const X64_Mnemonic jz = {
  .name = "jz",
  .encoding_list = (const Instruction_Encoding *)jz_encoding_list,
  .encoding_count = countof(jz_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// setz
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setz_encoding_list[] = {
  encoding(0x0F94, none, r_m8),
};
const X64_Mnemonic setz = {
  .name = "setz",
  .encoding_list = (const Instruction_Encoding *)setz_encoding_list,
  .encoding_count = countof(setz_encoding_list),
};
const X64_Mnemonic sete = {
  .name = "sete",
  .encoding_list = (const Instruction_Encoding *)setz_encoding_list,
  .encoding_count = countof(setz_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// setne
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setne_encoding_list[] = {
  encoding(0x0F95, none, r_m8),
};
const X64_Mnemonic setne = {
  .name = "setne",
  .encoding_list = (const Instruction_Encoding *)setne_encoding_list,
  .encoding_count = countof(setne_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// setl
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setl_encoding_list[] = {
  encoding(0x0F9C, none, r_m8),
};
const X64_Mnemonic setl = {
  .name = "setl",
  .encoding_list = (const Instruction_Encoding *)setl_encoding_list,
  .encoding_count = countof(setl_encoding_list),
};

////////////////////////////////////////////////////////////////////////////////
// setg
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding setg_encoding_list[] = {
  encoding(0x0F9F, none, r_m8),
};
const X64_Mnemonic setg = {
  .name = "setg",
  .encoding_list = (const Instruction_Encoding *)setg_encoding_list,
  .encoding_count = countof(setg_encoding_list),
};


////////////////////////////////////////////////////////////////////////////////
// jmp
////////////////////////////////////////////////////////////////////////////////
const Instruction_Encoding jmp_encoding_list[] = {
  encoding(0xEB, none, imm8),
  encoding(0xE9, none, imm32),
};
const X64_Mnemonic jmp = {
  .name = "jmp",
  .encoding_list = (const Instruction_Encoding *)jmp_encoding_list,
  .encoding_count = countof(jmp_encoding_list),
};

#undef none
#undef _r
#undef plus_r
#undef _op_code

#undef r_al
#undef r_ax
#undef r_eax
#undef r_rax

#undef r8
#undef r16
#undef r32
#undef r64

#undef r_m8
#undef r_m16
#undef r_m32
#undef r_m64

#undef m8
#undef m16
#undef m32
#undef m64

#undef imm8
#undef imm16
#undef imm32
#undef imm64

#undef xmm32
#undef xmm_m32
#undef xmm64
#undef xmm_m64

#undef encoding_operands

#undef encoding
