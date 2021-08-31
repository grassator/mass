#include "types.h"

#define none .extension_type = Instruction_Extension_Type_None,
#define _r .extension_type = Instruction_Extension_Type_Register,
#define plus_r .extension_type = Instruction_Extension_Type_Plus_Register,
#define _op_code(_extension_)\
  .extension_type = Instruction_Extension_Type_Op_Code,\
  .op_code_extension = (_extension_ & 0b111),


#define eflags  { Operand_Encoding_Type_Eflags, Operand_Size_8 }

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

#define m8  { Operand_Encoding_Type_Memory, Operand_Size_8 }
#define m16 { Operand_Encoding_Type_Memory, Operand_Size_16 }
#define m32 { Operand_Encoding_Type_Memory, Operand_Size_32 }
#define m64 { Operand_Encoding_Type_Memory, Operand_Size_64 }

#define imm8  { Operand_Encoding_Type_Immediate, Operand_Size_8 }
#define imm16 { Operand_Encoding_Type_Immediate, Operand_Size_16 }
#define imm32 { Operand_Encoding_Type_Immediate, Operand_Size_32 }
#define imm64 { Operand_Encoding_Type_Immediate, Operand_Size_64 }

#define xmm32 { Operand_Encoding_Type_Xmm, Operand_Size_32 }
#define xmm64 { Operand_Encoding_Type_Xmm, Operand_Size_64 }

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

#define mnemonic(_name_, ...)\
  const X64_Mnemonic *_name_ = &(const X64_Mnemonic){\
    .name = #_name_,\
    .encoding_list = (const Instruction_Encoding[]){__VA_ARGS__},\
    .encoding_count = countof((const Instruction_Encoding[]){__VA_ARGS__}),\
  }

mnemonic(mov,
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
  encoding(0xB8, plus_r, r64, imm64)
);

mnemonic(movsx,
  encoding(0x0FBE, _r, r16, r_m8),
  encoding(0x0FBE, _r, r32, r_m8),
  encoding(0x0FBE, _r, r64, r_m8),
  encoding(0x0FBF, _r, r32, r_m16),
  encoding(0x0FBF, _r, r64, r_m16),
);

mnemonic(movzx,
  encoding(0x0FB6, _r, r16, r_m8),
  encoding(0x0FB6, _r, r32, r_m8),
  encoding(0x0FB6, _r, r64, r_m8),
  encoding(0x0FB7, _r, r32, r_m16),
  encoding(0x0FB7, _r, r64, r_m16),
);

mnemonic(movss,
  encoding(0xF30F10, _r, xmm32, m32),
  encoding(0xF30F11, _r, m32, xmm32),
);

mnemonic(movsd,
  encoding(0xF20F10, _r, xmm64, m64),
  encoding(0xF20F11, _r, m64, xmm64),
);

// TODO figure out how to better deal with implicit parameters here
mnemonic(rep_movsb,
  encoding(0xF3A4, none, 0),
);

mnemonic(lea,
  encoding(0x8d, _r, r64, m64),
);

mnemonic(int3,
  encoding(0xCC, none, 0),
);

mnemonic(asm_syscall,
  encoding(0x0F05, none, 0),
);

mnemonic(ret,
  encoding(0xC3, none, 0),
);

mnemonic(push,
  encoding(0x50, plus_r, r16),
  encoding(0x50, plus_r, r32),
  encoding(0x50, plus_r, r64),

  encoding(0xFF, _op_code(6), r_m16),
  encoding(0xFF, _op_code(6), r_m32),
  encoding(0xFF, _op_code(6), r_m64),
);

mnemonic(pop,
  encoding(0x58, plus_r, r16),
  encoding(0x58, plus_r, r32),
  encoding(0x58, plus_r, r64),

  encoding(0x8F, _op_code(0), r_m16),
  encoding(0x8F, _op_code(0), r_m32),
  encoding(0x8F, _op_code(0), r_m64),
);

mnemonic(inc,
  encoding(0xFF, _op_code(0), r_m16),
  encoding(0xFF, _op_code(0), r_m32),
  encoding(0xFF, _op_code(0), r_m64),
);

mnemonic(xor,
  encoding(0x34, none, r_al, imm8),
  encoding(0x35, none, r_ax, imm16),
  encoding(0x35, none, r_eax, imm32),
  encoding(0x35, none, r_rax, imm32),

  encoding(0x32, _r, r8, r_m8),
  encoding(0x33, _r, r16, r_m16),
  encoding(0x33, _r, r32, r_m32),
  encoding(0x33, _r, r64, r_m64),
);

mnemonic(add,
  encoding(0x04, none, r_al, imm8),
  encoding(0x05, none, r_ax, imm16),
  encoding(0x05, none, r_eax, imm32),
  encoding(0x05, none, r_rax, imm32),

  encoding(0x00, _r, r_m8, r8),
  encoding(0x01, _r, r_m16, r16),
  encoding(0x01, _r, r_m32, r32),
  encoding(0x01, _r, r_m64, r64),

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
);

mnemonic(addss,
  encoding(0xF30F58, _r, xmm32, m32),
);

mnemonic(sub,
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
);

mnemonic(imul,
  encoding(0xF6, _op_code(5), r_m8),
  encoding(0xF7, _op_code(5), r_m16),
  encoding(0xF7, _op_code(5), r_m32),
  encoding(0xF7, _op_code(5), r_m64),

  encoding(0x0FAF, _r, r16, r_m16),
  encoding(0x0FAF, _r, r32, r_m32),
  encoding(0x0FAF, _r, r64, r_m64),

  encoding(0x69, _r, r16, r_m16, imm16),
  encoding(0x69, _r, r32, r_m32, imm32),
  encoding(0x69, _r, r64, r_m64, imm32),
);

mnemonic(mul,
  encoding(0xF6, _op_code(4), r_m8),
  encoding(0xF7, _op_code(4), r_m16),
  encoding(0xF7, _op_code(4), r_m32),
  encoding(0xF7, _op_code(4), r_m64),
);

mnemonic(idiv,
  encoding(0xF6, _op_code(7), r_m8),
  encoding(0xF7, _op_code(7), r_m16),
  encoding(0xF7, _op_code(7), r_m32),
  encoding(0xF7, _op_code(7), r_m64),
);

mnemonic(asm_div,
  encoding(0xF6, _op_code(6), r_m8),
  encoding(0xF7, _op_code(6), r_m16),
  encoding(0xF7, _op_code(6), r_m32),
  encoding(0xF7, _op_code(6), r_m64),
);

mnemonic(cbw,
  encoding(0x6698, none, 0),
);

mnemonic(cwd,
  encoding(0x6699, none, 0),
);

mnemonic(cdq,
  encoding(0x99, none, 0),
);

mnemonic(cqo,
  encoding(0x4899, none, 0),
);

mnemonic(and,
  encoding(0x80, _op_code(4), r_m8, imm8),
  encoding(0x81, _op_code(4), r_m16, imm16),
  encoding(0x81, _op_code(4), r_m32, imm32),
  encoding(0x81, _op_code(4), r_m64, imm32),
  encoding(0x20, _r, r_m8, r8),
  encoding(0x21, _r, r_m16, r16),
  encoding(0x21, _r, r_m32, r32),
  encoding(0x21, _r, r_m64, r64),
  encoding(0x22, _r, r8, r_m8),
  encoding(0x23, _r, r16, r_m16),
  encoding(0x23, _r, r32, r_m32),
  encoding(0x23, _r, r64, r_m64),
);

mnemonic(or,
  encoding(0x80, _op_code(1), r_m8, imm8),
  encoding(0x81, _op_code(1), r_m16, imm16),
  encoding(0x81, _op_code(1), r_m32, imm32),
  encoding(0x81, _op_code(1), r_m64, imm32),
  encoding(0x08, _r, r_m8, r8),
  encoding(0x09, _r, r_m16, r16),
  encoding(0x09, _r, r_m32, r32),
  encoding(0x09, _r, r_m64, r64),
  encoding(0x0A, _r, r8, r_m8),
  encoding(0x0B, _r, r16, r_m16),
  encoding(0x0B, _r, r32, r_m32),
  encoding(0x0B, _r, r64, r_m64),
);

mnemonic(shr,
  encoding(0xC0, _op_code(5), r_m8, imm8),
  encoding(0xC1, _op_code(5), r_m16, imm8),
  encoding(0xC1, _op_code(5), r_m32, imm8),
  encoding(0xC1, _op_code(5), r_m64, imm8),
);

mnemonic(shl,
  encoding(0xC0, _op_code(4), r_m8, imm8),
  encoding(0xC1, _op_code(4), r_m16, imm8),
  encoding(0xC1, _op_code(4), r_m32, imm8),
  encoding(0xC1, _op_code(4), r_m64, imm8),
);

mnemonic(call,
  encoding(0xE8, none, imm16),
  encoding(0xE8, none, imm32),

  encoding(0xFF, _op_code(2), r_m16),
  encoding(0xFF, _op_code(2), r_m32),
  encoding(0xFF, _op_code(2), r_m64),
);

mnemonic(x64_test,
  encoding(0x84, _r, r_m8, r8),
  encoding(0x85, _r, r_m16, r16),
  encoding(0x85, _r, r_m32, r32),
  encoding(0x85, _r, r_m64, r64),
);

mnemonic(cmp,
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
);

mnemonic(jmp,
  encoding(0xEB, none, imm8),
  encoding(0xE9, none, imm32),
  encoding(0xFF, _op_code(4), r_m64),
);

#define ENUMERATE_CC(process)\
 process(0x0, o)\
 process(0x1, no)\
 process(0x2, b)\
 process(0x2, c)\
 process(0x2, nae)\
 process(0x3, ae)\
 process(0x3, nb)\
 process(0x3, nc)\
 process(0x4, e)\
 process(0x4, z)\
 process(0x5, ne)\
 process(0x5, nz)\
 process(0x6, be)\
 process(0x6, na)\
 process(0x7, a)\
 process(0x7, nbe)\
 process(0x8, s)\
 process(0x9, ns)\
 process(0xA, p)\
 process(0xA, pe)\
 process(0xB, np)\
 process(0xB, po)\
 process(0xC, l)\
 process(0xC, nge)\
 process(0xD, ge)\
 process(0xD, nl)\
 process(0xE, le)\
 process(0xE, ng)\
 process(0xF, g)\
 process(0xF, nle)

#define jcc(_value_, _suffix_)\
  mnemonic(j##_suffix_,\
    encoding(0x70 + (_value_), none, imm8, eflags),\
    encoding(0x0F80 + (_value_), none, imm32, eflags),\
  );
ENUMERATE_CC(jcc)
#undef jcc

#define setcc(_value_, _suffix_)\
  mnemonic(set##_suffix_,\
    encoding(0x0F90 + (_value_), none, r_m8, eflags),\
  );
ENUMERATE_CC(setcc)
#undef setcc

#undef ENUMERATE_CC

#undef none
#undef _r
#undef plus_r
#undef _op_code
#undef eflags

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

#undef mnemonic
#undef encoding
