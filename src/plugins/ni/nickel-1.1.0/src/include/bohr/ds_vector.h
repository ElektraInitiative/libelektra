// lgtm [cpp/missing-header-guard]

/******************************************************************************
 * Darmstadtium - a library of data structures
 * Ds_vector - a vector (growable array) library
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: ds_vector.h 317 2008-01-05 21:45:34Z chaz $
 ******************************************************************************/


// You must be careful about including this file multiple times.  Each time it's
// included in the same source file it must have a different Ds_VECTOR_SUFFIX.

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <internal/utility/old_helper.h>


// Controls inlining of this library's functions.
#ifndef Ds_VECTOR_INLINE
#ifdef Ds_INLINE
#define Ds_VECTOR_INLINE Ds_INLINE
#else
#define Ds_VECTOR_INLINE inline static
#endif
#endif

// Nix non-critical C99 keywords in compilers that don't support them.
#if ((!defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L) && !defined(restrict))
#define restrict
#define _Ds_VECTOR_DEFINED_RESTRICT
#endif


// How to grow when the buffer is insufficient.  The following values are
// available: 1 - the buffer will grow to the exact needed size; 2 - the  buffer
// will double in size until sufficient (default); or 4 - the buffer will
// quadruple in size until sufficient.
#ifndef Ds_VECTOR_BEHAVIOR
#define Ds_VECTOR_BEHAVIOR 2
#endif

// The type stored in Ds_vectors for this instance of the library.
#ifndef Ds_VECTOR_TYPE
#define Ds_VECTOR_TYPE int
#endif

// Suffix appended to all symbols exported by this header.
#ifndef Ds_VECTOR_SUFFIX
#define Ds_VECTOR_SUFFIX
#endif

// Macros to make symbols including a defined suffix (there's a seemingly
// extraneous middle step because it must pass through the macro processor twice
// to get what Ds_VECTOR_SUFFIX is defined as, instead of "Ds_VECTOR_SUFFIX").
#define __Ds_VECTOR_SYMBOL_CAT(symbol, suffix) symbol##suffix
#define _Ds_VECTOR_SYMBOL_CAT(symbol, suffix) __Ds_VECTOR_SYMBOL_CAT (symbol, suffix)
#define _Ds_VECTOR_SYMBOL(symbol) _Ds_VECTOR_SYMBOL_CAT (symbol, Ds_VECTOR_SUFFIX)

// Define symbols that in reality have a suffix attached to them.
#define Ds_vector _Ds_VECTOR_SYMBOL (Ds_vector)
#define Ds_InitVector _Ds_VECTOR_SYMBOL (Ds_InitVector)
#define Ds_FreeVector _Ds_VECTOR_SYMBOL (Ds_FreeVector)
#define Ds_InsertVectorItems _Ds_VECTOR_SYMBOL (Ds_InsertVectorItems)
#define Ds_RemoveVectorItems _Ds_VECTOR_SYMBOL (Ds_RemoveVectorItems)
#define Ds_ResizeVector _Ds_VECTOR_SYMBOL (Ds_ResizeVector)


// Tells Ds_Insert- and RemoveVectorItems() to use the last items in the vector.
#define Ds_VECTOR_END SIZE_MAX


// A growable array.
typedef struct Ds_vector
{
	Ds_VECTOR_TYPE * buf; // the memory buffer
	size_t num;	      // how many items in it
	size_t cap;	      // how many items the buffer can hold

} Ds_vector;
#define Ds_VECTOR_INIT                                                                                                                     \
	{                                                                                                                                  \
		NULL, 0, 0                                                                                                                 \
	} // initializer


/* Reserves an initial capacity for the Ds_vector.  Returns 0/nonzero on
 * failure/success.
 */
Ds_VECTOR_INLINE int Ds_InitVector (Ds_vector * restrict v, size_t cap)
{
	*v = (Ds_vector) Ds_VECTOR_INIT;

	if (cap > 0)
	{
		if (!(v->buf = (Ds_VECTOR_TYPE *) elektraMalloc (cap * sizeof (Ds_VECTOR_TYPE)))) return 0;

		v->cap = cap;
	}

	return 1;
}

/* Frees all memory associated with the Ds_vector.
 */
Ds_VECTOR_INLINE void Ds_FreeVector (Ds_vector * restrict v)
{
	if (v->buf)
	{
		elektraFree (v->buf);
	}
	*v = (Ds_vector) Ds_VECTOR_INIT;
}

/* Inserts items somewhere into the Ds_vector.  pos is the 0-based offset to
 * place the inserted items--use Ds_VECTOR_END to mean the end of the existing
 * items.  Returns 0/nonzero on failure/success.  The vector won't be modified
 * if it fails.
 */
Ds_VECTOR_INLINE int Ds_InsertVectorItems (Ds_vector * restrict v, const Ds_VECTOR_TYPE * restrict items, size_t num, size_t pos)
{
	size_t new_cap;

#if (Ds_VECTOR_BEHAVIOR == 1)
	new_cap = v->num + num;
#else
	new_cap = (v->cap ? v->cap : 1);
	while (new_cap < v->num + num)
#if (Ds_VECTOR_BEHAVIOR == 4)
		new_cap <<= 2; // the same as *= 4
#else
		new_cap <<= 1; // the same as *= 2
#endif
#endif
	if (new_cap > v->cap)
	{
		Ds_VECTOR_TYPE * new_buf;
		if (!(new_buf = (Ds_VECTOR_TYPE *) realloc (v->buf, new_cap * sizeof (Ds_VECTOR_TYPE))))
		{
			return 0;
		}
		v->buf = new_buf;
		v->cap = new_cap;
	}

	if (pos > v->num)
		pos = v->num;
	else if (pos < v->num)
	{
		memmove (v->buf + pos + num, v->buf + pos, (v->num - pos) * sizeof (Ds_VECTOR_TYPE));
	}

	memcpy (v->buf + pos, items, num * sizeof (Ds_VECTOR_TYPE));
	v->num += num;

	return 1;
}

/* Takes items out of the Ds_vector, optionally copying them to a buffer.
 */
Ds_VECTOR_INLINE void Ds_RemoveVectorItems (Ds_vector * restrict v, Ds_VECTOR_TYPE * restrict items, size_t num, size_t pos)
{
	if (num > v->num) num = v->num;
	if (pos > v->num - num) pos = v->num - num;

	if (items) memcpy (items, v->buf + pos, num * sizeof (Ds_VECTOR_TYPE));

	if (v->num - num - pos > 0)
	{
		memmove (v->buf + pos, v->buf + num + pos, (v->num - num - pos) * sizeof (Ds_VECTOR_TYPE));
	}
	v->num -= num;
}

/* Resizes the Ds_vector.  Returns 0/nonzero on failure/success.
 */
Ds_VECTOR_INLINE int Ds_ResizeVector (Ds_vector * restrict v, size_t cap)
{
	if (cap > 0)
	{
		Ds_VECTOR_TYPE * new_buf;

		if (!(new_buf = (Ds_VECTOR_TYPE *) realloc (v->buf, cap * sizeof (Ds_VECTOR_TYPE))))
		{
			return 0;
		}

		v->buf = new_buf;
		v->cap = cap;

		if (v->num > cap) v->num = cap;
	}

	return 1;
}


// We don't want these internal names clogging up the global namespace.
#undef Ds_ResizeVector
#undef Ds_RemoveVectorItems
#undef Ds_InsertVectorItems
#undef Ds_FreeVector
#undef Ds_InitVector
#undef Ds_vector
#undef _Ds_VECTOR_SYMBOL
#undef _Ds_VECTOR_SYMBOL_CAT
#undef __Ds_VECTOR_SYMBOL_CAT

// Undefine these so it's easier to change them and include this file again.
#undef Ds_VECTOR_SUFFIX
#undef Ds_VECTOR_TYPE
#undef Ds_VECTOR_BEHAVIOR

#ifdef _Ds_VECTOR_DEFINED_RESTRICT
#undef _Ds_VECTOR_DEFINED_RESTRICT
#undef restrict
#endif
