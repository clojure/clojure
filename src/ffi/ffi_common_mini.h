/* -----------------------------------------------------------------------
   ffi_common_mini.h - Copyright (C) 2011, 2012, 2013  Anthony Green
                       Copyright (C) 2007  Free Software Foundation, Inc
                       Copyright (c) 1996  Red Hat, Inc.
                  
   Common internal definitions and macros. Only necessary for building
   libffi.
   ----------------------------------------------------------------------- */

#ifndef FFI_COMMON_MINI_H
#define FFI_COMMON_MINI_H

#ifdef __cplusplus
extern "C" {
#endif

#include <fficonfig_mini.h>
#include <alloca.h>
#include <string.h>

#if defined (FFI_DEBUG)
#include <stdio.h>
#endif

#if defined (FFI_DEBUG)
extern void ffi_mini_assert(char *expr, char *file, int line);
extern void ffi_mini_stop_here(void);
extern void ffi_mini_type_test(ffim_type *a, char *file, int line);

#define FFI_ASSERT(x) ((x) ? (void)0 : ffi_mini_assert(#x, __FILE__,__LINE__))
#define FFI_ASSERT_AT(x, f, l) ((x) ? 0 : ffi_mini_assert(#x, (f), (l)))
#define FFI_ASSERT_VALID_TYPE(x) ffi_mini_type_test (x, __FILE__, __LINE__)
#else
#define FFI_ASSERT(x)
#define FFI_ASSERT_AT(x, f, l)
#define FFI_ASSERT_VALID_TYPE(x)
#endif

#define ALIGN(v, a)  (((((size_t) (v))-1) | ((a)-1))+1)
#define ALIGN_DOWN(v, a) (((size_t) (v)) & -a)

/* Perform machine dependent cif processing */
ffim_status ffi_mini_prep_cif_machdep(ffim_cif *cif);
ffim_status ffi_mini_prep_cif_machdep_var(ffim_cif *cif,
	 unsigned int nfixedargs, unsigned int ntotalargs);

/* Extended cif, used in callback from assembly routine */
typedef struct {
  ffim_cif *cif;
  void *rvalue;
  void **avalue;
} extended_cif;

/* Terse sized type definitions.  */
typedef unsigned int UINT8  __attribute__((__mode__(__QI__)));
typedef signed int   SINT8  __attribute__((__mode__(__QI__)));
typedef unsigned int UINT16 __attribute__((__mode__(__HI__)));
typedef signed int   SINT16 __attribute__((__mode__(__HI__)));
typedef unsigned int UINT32 __attribute__((__mode__(__SI__)));
typedef signed int   SINT32 __attribute__((__mode__(__SI__)));
typedef unsigned int UINT64 __attribute__((__mode__(__DI__)));
typedef signed int   SINT64 __attribute__((__mode__(__DI__)));
typedef float FLOAT32;

#ifndef __GNUC__
#define __builtin_expect(x, expected_value) (x)
#endif
#define LIKELY(x)    __builtin_expect(!!(x),1)
#define UNLIKELY(x)  __builtin_expect((x)!=0,0)

#ifdef __cplusplus
}
#endif

#endif
