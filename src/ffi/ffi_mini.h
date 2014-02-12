/* -----------------------------------------------------------------*-C-*-
   libffi 3.0.14-rc0 - Copyright (c) 2011 Anthony Green
                     - Copyright (c) 1996-2003, 2007, 2008 Red Hat, Inc.

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the ``Software''), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.

   ----------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   The basic API is described in the README file.
   -------------------------------------------------------------------- */

#ifndef LIBFFI_MINI_H
#define LIBFFI_MINI_H

#ifdef __cplusplus
extern "C" {
#endif

#include "fficonfig_mini.h"

/* ---- System configuration information --------------------------------- */

#include "ffitarget_mini.h"

#ifndef LIBFFI_ASM

#include <stddef.h>
#include <limits.h>

/* LONG_LONG_MAX is not always defined (not if STRICT_ANSI, for example).
   But we can find it either under the correct ANSI name, or under GNU
   C's internal name.  */

#define FFI_64_BIT_MAX 9223372036854775807
#define FFI_LONG_LONG_MAX LONG_LONG_MAX

/* The closure code assumes that this works on pointers, i.e. a size_t	*/
/* can hold a pointer.							*/

typedef struct _ffim_type
{
  size_t size;
  unsigned short alignment;
  unsigned short type;
  struct _ffim_type **elements;
} ffim_type;

#ifndef LIBFFI_HIDE_BASIC_TYPES
#if SCHAR_MAX == 127
# define ffim_type_uchar                ffim_type_uint8
# define ffim_type_schar                ffim_type_sint8
#else
 #error "char size not supported"
#endif

#if SHRT_MAX == 32767
# define ffim_type_ushort       ffim_type_uint16
# define ffim_type_sshort       ffim_type_sint16
#elif SHRT_MAX == 2147483647
# define ffim_type_ushort       ffim_type_uint32
# define ffim_type_sshort       ffim_type_sint32
#else
 #error "short size not supported"
#endif

#if INT_MAX == 32767
# define ffim_type_uint         ffim_type_uint16
# define ffim_type_sint         ffim_type_sint16
#elif INT_MAX == 2147483647
# define ffim_type_uint         ffim_type_uint32
# define ffim_type_sint         ffim_type_sint32
#elif INT_MAX == 9223372036854775807
# define ffim_type_uint         ffim_type_uint64
# define ffim_type_sint         ffim_type_sint64
#else
 #error "int size not supported"
#endif

#if LONG_MAX == 2147483647
# if FFI_LONG_LONG_MAX != FFI_64_BIT_MAX
 #error "no 64-bit data type supported"
# endif
#elif LONG_MAX != FFI_64_BIT_MAX
 #error "long size not supported"
#endif

#if LONG_MAX == 2147483647
# define ffim_type_ulong        ffim_type_uint32
# define ffim_type_slong        ffim_type_sint32
#elif LONG_MAX == FFI_64_BIT_MAX
# define ffim_type_ulong        ffim_type_uint64
# define ffim_type_slong        ffim_type_sint64
#else
 #error "long size not supported"
#endif

#define FFI_EXTERN extern

/* These are defined in types.c */
FFI_EXTERN ffim_type ffim_type_void;
FFI_EXTERN ffim_type ffim_type_uint8;
FFI_EXTERN ffim_type ffim_type_sint8;
FFI_EXTERN ffim_type ffim_type_uint16;
FFI_EXTERN ffim_type ffim_type_sint16;
FFI_EXTERN ffim_type ffim_type_uint32;
FFI_EXTERN ffim_type ffim_type_sint32;
FFI_EXTERN ffim_type ffim_type_uint64;
FFI_EXTERN ffim_type ffim_type_sint64;
FFI_EXTERN ffim_type ffim_type_float;
FFI_EXTERN ffim_type ffim_type_double;
FFI_EXTERN ffim_type ffim_type_pointer;

#ifdef HAVE_LONG_DOUBLE
FFI_EXTERN ffim_type ffim_type_longdouble;
#else
#define ffim_type_longdouble ffim_type_double
#endif
#endif /* HAVE_LONG_DOUBLE */

typedef enum {
  FFIM_OK = 0,
  FFIM_BAD_TYPEDEF,
  FFIM_BAD_ABI
} ffim_status;

typedef unsigned FFIM_TYPE;

typedef struct {
  ffim_abi abi;
  unsigned nargs;
  ffim_type **arg_types;
  ffim_type *rtype;
  unsigned bytes;
  unsigned flags;
#ifdef FFIM_EXTRA_CIF_FIELDS
  FFIM_EXTRA_CIF_FIELDS;
#endif
} ffim_cif;

#if HAVE_LONG_DOUBLE_VARIANT
/* Used to adjust size/alignment of ffi types.  */
void ffi_mini_prep_types (ffim_abi abi);
#endif /* HAVE_LONG_DOUBLE_VARIANT */

/* ---- Definitions for the raw API -------------------------------------- */

#ifndef FFIM_SIZEOF_ARG
# if LONG_MAX == 2147483647
#  define FFIM_SIZEOF_ARG        4
# elif LONG_MAX == FFI_64_BIT_MAX
#  define FFIM_SIZEOF_ARG        8
# endif
#endif

/* ---- Public interface definition -------------------------------------- */

ffim_status ffi_mini_prep_cif(ffim_cif *cif,
			ffim_abi abi,
			unsigned int nargs,
			ffim_type *rtype,
			ffim_type **atypes);

ffim_status ffi_mini_prep_cif_var(ffim_cif *cif,
			    ffim_abi abi,
			    unsigned int nfixedargs,
			    unsigned int ntotalargs,
			    ffim_type *rtype,
			    ffim_type **atypes);

void ffi_mini_call(ffim_cif *cif,
	      void (*fn)(void),
	      void *rvalue,
	      void **avalue);

/* Useful for eliminating compiler warnings */
#define FFI_FN(f) ((void (*)(void))f)

/* ---- Definitions shared with assembly code ---------------------------- */

#endif

#define FFIM_TYPE_VOID       0    
#define FFIM_TYPE_INT        1
#define FFIM_TYPE_FLOAT      2    
#define FFIM_TYPE_DOUBLE     3
#if HAVE_LONG_DOUBLE
#define FFIM_TYPE_LONGDOUBLE 4
#else
#define FFIM_TYPE_LONGDOUBLE FFIM_TYPE_DOUBLE
#endif
#define FFIM_TYPE_UINT8      5   
#define FFIM_TYPE_SINT8      6
#define FFIM_TYPE_UINT16     7 
#define FFIM_TYPE_SINT16     8
#define FFIM_TYPE_UINT32     9
#define FFIM_TYPE_SINT32     10
#define FFIM_TYPE_UINT64     11
#define FFIM_TYPE_SINT64     12
#define FFIM_TYPE_STRUCT     13
#define FFIM_TYPE_POINTER    14

/* This should always refer to the last type code (for sanity checks) */
#define FFIM_TYPE_LAST       FFIM_TYPE_POINTER

#ifdef __cplusplus
}
#endif

#endif /* !defined (LIBFFI_MINI_H) */
