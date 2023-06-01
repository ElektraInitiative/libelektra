/******************************************************************************
 * Darmstadtium - a library of data structures
 * Ds_str - a string library
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: ds_str.h 323 2008-01-06 03:55:26Z chaz $
 ******************************************************************************/


#ifndef __bohr_ds_str_h__
#define __bohr_ds_str_h__

#include <internal/utility/old_helper.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// Controls inlining of this library's functions.
#ifndef Ds_STR_INLINE
#ifdef Ds_INLINE
#define Ds_STR_INLINE Ds_INLINE
#else
#define Ds_STR_INLINE inline static
#endif
#endif

// Nix non-critical C99 keywords in compilers that don't support them.
#if ((!defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L) && !defined(restrict))
#define restrict
#define _Ds_STR_DEFINED_RESTRICT
#endif


// How to grow when the buffer is insufficient.  The following values are
// available: 1 - the buffer will grow to the exact needed size; 2 - the  buffer
// will double in size until sufficient (default); or 4 - the buffer will
// quadruple in size until sufficient.
#ifndef Ds_STR_BEHAVIOR
#define Ds_STR_BEHAVIOR 2
#endif


// A string.
typedef struct Ds_str
{
	char * str; // the string itself
	int len;    // its length
	int size;   // how big the str buffer is

} Ds_str;
#define Ds_STR_INIT                                                                                                                        \
	{                                                                                                                                  \
		NULL, 0, 0                                                                                                                 \
	} // initializer


/* Initializes the Ds_str.  Only necessary to reserve initial space (if the
 * Ds_str was initialized to Ds_STR_INIT as it was declared, it can be used
 * without first having Ds_InitStr() called on it).  Returns 0/nonzero on
 * failure/success.  Will not fail if size is 0.
 */
Ds_STR_INLINE int Ds_InitStr (Ds_str * restrict s, int size)
{
	*s = (Ds_str) Ds_STR_INIT;

	if (size > 0)
	{
		if (!(s->str = (char *) malloc (size * sizeof (char)))) return 0;

		s->str[0] = '\0';
		s->size = size;
	}

	return 1;
}

/* Frees all memory associated with the Ds_str.
 */
Ds_STR_INLINE void Ds_FreeStr (Ds_str * restrict s)
{
	if (s->str)
	{
		elektraFree (s->str);
	}
	*s = (Ds_str) Ds_STR_INIT;
}

/* Appends a string onto the Ds_str (note: setting the destination's length to
 * 0 before calling this function makes it copy instead of append).  If the
 * source string length isn't known, use a negative number.  Returns the source
 * string length (as given or calculated) on success, or a negative number on
 * failure.
 */
Ds_STR_INLINE int Ds_StrCat (Ds_str * restrict dest, const char * restrict source, int source_len)
{
	int new_size;

	if (source_len < 0) source_len = (int) strlen (source);

#if (Ds_STR_BEHAVIOR == 1)
	new_size = dest->len + source_len + 1;
#else
	new_size = (dest->size ? dest->size : 1);
	while (new_size < dest->len + source_len + 1)
#if (Ds_STR_BEHAVIOR == 4)
		new_size <<= 2; // the same as *= 4
#else
		new_size <<= 1;		// the same as *= 2
#endif
#endif
	if (new_size > dest->size)
	{
		char * new_str;
		if (!(new_str = (char *) realloc (dest->str, new_size * sizeof (char)))) return -1;
		dest->str = new_str;
		dest->size = new_size;
	}

	memcpy (dest->str + dest->len, source, source_len * sizeof (char));
	dest->len += source_len;
	dest->str[dest->len] = '\0';

	return source_len;
}

/* Appends a printf()-formatted string onto the Ds_str (note: setting the
 * destination's length to 0 before calling this function makes it copy instead
 * of append).  Returns the length of the formatted string on success, or a
 * negative number on failure (in which case the contents of the destination
 * may have changed, unfortunately).
 */
Ds_STR_INLINE int Ds_StrCatVPrint (Ds_str * restrict dest, const char * restrict format, va_list args)
{
	va_list args_copy;
	int len;

	va_copy (args_copy, args);
	len = vsnprintf (dest->str + dest->len, dest->size - dest->len, format, args_copy);
	va_end (args_copy);

	if (len >= dest->size - dest->len)
	{
		int new_size;
#if (Ds_STR_BEHAVIOR == 1)
		new_size = dest->len + len + 1;
#else
		new_size = (dest->size ? dest->size : 1);
		while (new_size < dest->len + len + 1)
#if (Ds_STR_BEHAVIOR == 4)
			new_size <<= 2; // the same as *= 4
#else
			new_size <<= 1; // the same as *= 2
#endif
#endif
		if (new_size > dest->size)
		{
			char * new_str;
			if (!(new_str = (char *) realloc (dest->str, new_size * sizeof (char)))) return -1;
			dest->str = new_str;
			dest->size = new_size;
		}

		len = vsnprintf (dest->str + dest->len, dest->size - dest->len, format, args);
	}

	if (len >= dest->size - dest->len)
	{
		len = -1;
	}
	else
	{
		dest->len += len;
	}

	return len;
}

/* Same as Ds_StrCatVPrint(), except it takes an argument list instead of a
 * va_list.
 */
Ds_STR_INLINE int Ds_StrCatPrint (Ds_str * restrict dest, const char * restrict format, ...)
{
	int len;
	va_list args;

	va_start (args, format);
	len = Ds_StrCatVPrint (dest, format, args);
	va_end (args);

	return len;
}

/* Manually sets the buffer size for the Ds_str, truncating it if necessary.
 * Use a size of s->len+1 to shrink the buffer down to exactly the necessary
 * size to hold its current contents.  Returns 0/nonzero on failure/success.
 * Succeeds but does nothing if size is 0 or less.
 */
Ds_STR_INLINE int Ds_ResizeStr (Ds_str * restrict s, int size)
{
	if (size > 0)
	{
		char * new_str;

		if (!(new_str = (char *) realloc (s->str, size * sizeof (char)))) return 0;

		s->str = new_str;
		s->size = size;

		if (s->len > size - 1)
		{
			s->str[s->len = size - 1] = '\0';
		}
	}

	return 1;
}


#ifdef _Ds_STR_DEFINED_RESTRICT
#undef _Ds_STR_DEFINED_RESTRICT
#undef restrict
#endif

#endif
