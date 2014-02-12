/* -----------------------------------------------------------------*-C-*-
   ffitarget_mini.h - Copyright (c) 2012  Anthony Green
                      Copyright (c) 2009, 2010, 2011, 2012 ARM Ltd.
                      Copyright (c) 1996-2003, 2010  Red Hat, Inc.
                      Copyright (c) 2010  CodeSourcery
                      Copyright (C) 2008  Free Software Foundation, Inc.

   Target configuration macros for x86, x86-64, ARM, and AArch64 on Darwin.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.

   ----------------------------------------------------------------------- */

#if defined(__APPLE__) && defined(__MACH__) && !defined(MACOSX)
# define MACOSX
#endif

#ifndef MACOSX
# error "This file is only supported on OS X or iOS."
#endif

#ifndef LIBFFI_TARGET_MINI_H
#define LIBFFI_TARGET_MINI_H

#ifndef LIBFFI_MINI_H
#error "Please do not include ffitarget.h directly into your source.  Use ffi_mini.h instead."
#endif

#ifndef LIBFFI_ASM

/* ---- Generic type definitions ----------------------------------------- */

# if defined __x86_64__
#  define FFIM_SIZEOF_ARG 8
typedef unsigned long long     ffim_arg;
typedef long long              ffim_sarg;
# else
typedef unsigned long          ffim_arg;
typedef signed long            ffim_sarg;
# endif

typedef enum ffim_abi {
  FFIM_FIRST_ABI = 0,

  FFIM_SYSV,
# if defined (__i386__) || defined (__x86_64__)
  FFIM_UNIX64, // Unix variants all use the same ABI for x86-64
# elif defined (__arm__)
  FFIM_VFP,
# endif
  FFIM_LAST_ABI,
# if defined (__x86_64__)
  FFIM_DEFAULT_ABI = FFIM_UNIX64
# else
  FFIM_DEFAULT_ABI = FFIM_SYSV
# endif
} ffim_abi;

#endif /* !defined(LIBFFI_ASM) */

/* ---- Internal definitions --------------------------------------------- */

/* ---- Internal defines ---- */
#if defined(__i386__) || defined(__x86_64__)

/* For code common to all platforms on x86 and x86_64. */
#define X86_ANY

#if defined (X86_64) && defined (__i386__)
#undef X86_64
#define X86
#endif

#define FFIM_TYPE_SMALL_STRUCT_1B (FFIM_TYPE_LAST + 1)
#define FFIM_TYPE_SMALL_STRUCT_2B (FFIM_TYPE_LAST + 2)
#define FFIM_TYPE_SMALL_STRUCT_4B (FFIM_TYPE_LAST + 3)
#define FFIM_TYPE_MS_STRUCT       (FFIM_TYPE_LAST + 4)

#elif defined(__arm__)

#define FFIM_EXTRA_CIF_FIELDS      \
  int vfp_used;         \
  short vfp_reg_free, vfp_nargs;    \
  signed char vfp_args[16]      \

/* Internally used. */
#define FFIM_TYPE_STRUCT_VFP_FLOAT  (FFIM_TYPE_LAST + 1)
#define FFIM_TYPE_STRUCT_VFP_DOUBLE (FFIM_TYPE_LAST + 2)

#define FFI_TARGET_SPECIFIC_VARIADIC

#elif defined(__arm64__)

/* ---- Internal ---- */

#define FFIM_EXTRA_CIF_FIELDS unsigned aarch64_flags

#define AARCH64_FFI_WITH_V_BIT 0

#define AARCH64_N_XREG 32
#define AARCH64_N_VREG 32
#define AARCH64_CALL_CONTEXT_SIZE (AARCH64_N_XREG * 8 + AARCH64_N_VREG * 16)

#else

#error "Unsupported CPU type"

#endif /* CPU_TYPE */
#endif /* LIBFFI_TARGET_MINI_H */
