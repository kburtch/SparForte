/* Borrowed from postgresql-7.2.1 sources :
 * $Id: pgtypes.h,v 1.2 2005/02/11 02:59:45 ken Exp $
 *
 * See PG_COPYRIGHT file.
 *
 * The contents of this file is extracted from various portions
 * of other postgresql-7.2.1 works.
 */
#ifndef _PGTYPES_H_
#define _PGTYPES_H_

#include <assert.h>

#define palloc malloc
#define prealloc realloc
#define pfree free
#define pstrdup strdup
#define Assert assert

#define VARHDRSZ          ((int32) sizeof(int32))

/*
 * NULL
 *              Null pointer.
 */
#ifndef NULL
#define NULL    ((void *) 0)
#endif

typedef int bool;
#define FALSE 0
#define TRUE  1

/* ----------------------------------------------------------------
 *				Section 3:	standard system types
 * ----------------------------------------------------------------
 */

/*
 * Pointer
 *		Variable holding address of any memory resident object.
 *
 *		XXX Pointer arithmetic is done with this, so it can't be void *
 *		under "true" ANSI compilers.
 */
typedef char *Pointer;

/*
 * intN
 *		Signed integer, EXACTLY N BITS IN SIZE,
 *		used for numerical computations and the
 *		frontend/backend protocol.
 */
#ifndef HAVE_INT8
typedef signed char int8;		/* == 8 bits */
typedef signed short int16;		/* == 16 bits */
typedef signed int int32;		/* == 32 bits */
#endif /* not HAVE_INT8 */

/*
 * uintN
 *		Unsigned integer, EXACTLY N BITS IN SIZE,
 *		used for numerical computations and the
 *		frontend/backend protocol.
 */
/* Also defined in interfaces/odbc/md5.h */
#ifndef HAVE_UINT8
typedef unsigned char uint8;	/* == 8 bits */
typedef unsigned short uint16;	/* == 16 bits */
typedef unsigned int uint32;	/* == 32 bits */
#endif /* not HAVE_UINT8 */

/*
 * boolN
 *		Boolean value, AT LEAST N BITS IN SIZE.
 */
typedef uint8 bool8;			/* >= 8 bits */
typedef uint16 bool16;			/* >= 16 bits */
typedef uint32 bool32;			/* >= 32 bits */

/*
 * bitsN
 *		Unit of bitwise operation, AT LEAST N BITS IN SIZE.
 */
typedef uint8 bits8;			/* >= 8 bits */
typedef uint16 bits16;			/* >= 16 bits */
typedef uint32 bits32;			/* >= 32 bits */

/*
 * wordN
 *		Unit of storage, AT LEAST N BITS IN SIZE,
 *		used to fetch/store data.
 */
typedef uint8 word8;			/* >= 8 bits */
typedef uint16 word16;			/* >= 16 bits */
typedef uint32 word32;			/* >= 32 bits */

/*
 * floatN
 *		Floating point number, AT LEAST N BITS IN SIZE,
 *		used for numerical computations.
 *
 *		Since sizeof(floatN) may be > sizeof(char *), always pass
 *		floatN by reference.
 *
 * XXX: these typedefs are now deprecated in favor of float4 and float8.
 * They will eventually go away.
 */
typedef float float32data;
typedef double float64data;
typedef float *float32;
typedef double *float64;

/*
 * 64-bit integers
 */
#ifdef HAVE_LONG_INT_64
/* Plain "long int" fits, use it */

#ifndef HAVE_INT64
typedef long int int64;
#endif
#ifndef HAVE_UINT64
typedef unsigned long int uint64;
#endif

#elif defined(HAVE_LONG_LONG_INT_64)
/* We have working support for "long long int", use that */

#ifndef HAVE_INT64
typedef long long int int64;
#endif
#ifndef HAVE_UINT64
typedef unsigned long long int uint64;
#endif

#else /* not HAVE_LONG_INT_64 and not HAVE_LONG_LONG_INT_64 */

/* Won't actually work, but fall back to long int so that code compiles */
#ifndef HAVE_INT64
typedef long int int64;
#endif
#ifndef HAVE_UINT64
typedef unsigned long int uint64;
#endif

#define INT64_IS_BUSTED

#endif /* not HAVE_LONG_INT_64 and not HAVE_LONG_LONG_INT_64 */

/* sig_atomic_t is required by ANSI C, but may be missing on old platforms */
#ifndef HAVE_SIG_ATOMIC_T
typedef int sig_atomic_t;
#endif

/*
 * Size
 *		Size of any memory resident object, as returned by sizeof.
 */
typedef size_t Size;

/*
 * Index
 *		Index into any memory resident array.
 *
 * Note:
 *		Indices are non negative.
 */
typedef unsigned int Index;

/*
 * Offset
 *		Offset into any memory resident array.
 *
 * Note:
 *		This differs from an Index in that an Index is always
 *		non negative, whereas Offset may be negative.
 */
typedef signed int Offset;

/*
 * Common Postgres datatype names (as used in the catalogs)
 */
typedef int16 int2;
typedef int32 int4;
typedef float float4;
typedef double float8;

#endif

/* End $Source: /home/cvsroot/bush/src/apq-2.1/pgtypes.h,v $ */
