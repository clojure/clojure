/*	Manually created fficonfig_mini.h for libffi on Darwin.

	This file is manually generated to do away with the need for autoconf and
	therefore make it easier to cross-compile and build fat binaries.
*/

#if defined(__APPLE__) && defined(__MACH__) && !defined(MACOSX)
# define MACOSX
#endif

#ifndef MACOSX
# error "This file is only supported on OS X or iOS."
#endif

#ifndef LIBFFI_CONFIG_MINI_H
#define LIBFFI_CONFIG_MINI_H

#if defined(__i386__)
# undef FFI_EXEC_TRAMPOLINE_TABLE
# undef FFI_MMAP_EXEC_EMUTRAMP_PAX
# define FFI_MMAP_EXEC_WRIT 1
# define HAVE_LONG_DOUBLE 1
/* #undef HAVE_LONG_DOUBLE_VARIANT */
# define SIZEOF_DOUBLE 8
# define SIZEOF_LONG_DOUBLE 16
# define SIZEOF_SIZE_T 4
# undef WORDS_BIGENDIAN
# define X86_DARWIN
#elif defined(__x86_64__)
# undef FFI_EXEC_TRAMPOLINE_TABLE
# undef FFI_MMAP_EXEC_EMUTRAMP_PAX
# define FFI_MMAP_EXEC_WRIT 1
# define HAVE_LONG_DOUBLE 1
/* #undef HAVE_LONG_DOUBLE_VARIANT */
# define SIZEOF_DOUBLE 8
# define SIZEOF_LONG_DOUBLE 16
# define SIZEOF_SIZE_T 8
# undef WORDS_BIGENDIAN
# define X86_DARWIN
#elif defined(__arm__)
# define FFI_EXEC_TRAMPOLINE_TABLE 1
# undef FFI_MMAP_EXEC_EMUTRAMP_PAX
# undef FFI_MMAP_EXEC_WRIT
# undef HAVE_LONG_DOUBLE
/* #undef HAVE_LONG_DOUBLE_VARIANT */
# define SIZEOF_DOUBLE 8
# define SIZEOF_LONG_DOUBLE 8
# define SIZEOF_SIZE_T 4
# undef WORDS_BIGENDIAN
# define ARM
#elif defined(__arm64__)
# undef FFI_EXEC_TRAMPOLINE_TABLE
# undef FFI_MMAP_EXEC_EMUTRAMP_PAX
# define FFI_MMAP_EXEC_WRIT 1
# undef HAVE_LONG_DOUBLE
/* #undef HAVE_LONG_DOUBLE_VARIANT */
# define SIZEOF_DOUBLE 8
# define SIZEOF_LONG_DOUBLE 8
# define SIZEOF_SIZE_T 8
# undef WORDS_BIGENDIAN
# define AARCH64
#else
# error "Unknown Darwin CPU type"
#endif

#if defined (DEBUG)
#define FFI_DEBUG 1
#else
/* #undef FFI_DEBUG */
#endif /* defined (DEBUG) */

#define AC_APPLE_UNIVERSAL_BUILD 1
/* #undef CRAY_STACKSEG_END */
/* #undef C_ALLOCA */
#define EH_FRAME_FLAGS "aw"
#define FFI_NO_RAW_API 1
/* #undef FFI_NO_STRUCTS */
#define HAVE_ALLOCA 1
#define HAVE_ALLOCA_H 1
/* #undef HAVE_AS_ASCII_PSEUDO_OP */
#define HAVE_AS_CFI_PSEUDO_OP 1
/* #undef HAVE_AS_REGISTER_PSEUDO_OP */
/* #undef HAVE_AS_SPARC_UA_PCREL */
/* #undef HAVE_AS_STRING_PSEUDO_OP */
/* #undef HAVE_AS_X86_64_UNWIND_SECTION_TYPE */
/* #undef HAVE_AS_X86_PCREL */
#define HAVE_DLFCN_H 1
#define HAVE_HIDDEN_VISIBILITY_ATTRIBUTE 1
#define HAVE_INTTYPES_H 1
#define HAVE_MEMCPY 1
#define HAVE_MEMORY_H 1
#define HAVE_MMAP 1
#define HAVE_MMAP_ANON 1
/* #undef HAVE_MMAP_DEV_ZERO */
#define HAVE_MMAP_FILE 1
/* #undef HAVE_RO_EH_FRAME */
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
/* #undef LT_OBJDIR */
/* #undef NO_MINUS_C_MINUS_O */
/* #undef STACK_DIRECTION */
#define STDC_HEADERS 1
/* #undef SYMBOL_UNDERSCORE */
/* #undef USING_PURIFY */
/* #undef size_t */

#define PACKAGE "libffi"
#define PACKAGE_BUGREPORT "http://github.com/atgreen/libffi/issues"
#define PACKAGE_NAME "libffi"
#define PACKAGE_STRING "libffi 3.0.14-rc0-mini"
#define PACKAGE_TARNAME "libffi"
#define PACKAGE_VERSION "3.0.14-rc0-mini"
#define VERSION "3.0.14-rc0-mini"

#ifdef HAVE_HIDDEN_VISIBILITY_ATTRIBUTE
# ifdef LIBFFI_ASM
#  define FFI_HIDDEN(name) .hidden name
# else
#  define FFI_HIDDEN __attribute__ ((visibility ("hidden")))
# endif
#else
# ifdef LIBFFI_ASM
#  define FFI_HIDDEN(name)
# else
#  define FFI_HIDDEN
# endif
#endif

#endif /* !defined(LIBFFI_CONFIG_MINI_H) */
