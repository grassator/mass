#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef void (*fn_type_void_to_void)(void);
typedef fn_type_void_to_void fn_type_opaque;

typedef s32 (*fn_type_void_to_s32)(void);
typedef s64 (*fn_type_void_to_s64)(void);
typedef s32 (*fn_type_voidp_to_s32)(void*);
typedef void (*fn_type_s32p_to_void)(s32*);
typedef s32 (*fn_type_s32_to_s32)(s32);
typedef s32 (*fn_type_s32_s32_to_s32)(s32, s32);
typedef s64 (*fn_type_s64_to_s64)(s64);
typedef s64 (*fn_type_s64_s64_to_s64)(s64, s64);
typedef s64 (*fn_type_s64_s64_s64_to_s64)(s64, s64, s64);
typedef s32 (*fn_type__void_to_s32__to_s32)(fn_type_void_to_s32);

#endif TYPES_H
