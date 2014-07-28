/***************************************************************************
                      internal.c  -  Helper functions for elektra
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/**
 * @brief Internal Methods for Elektra
 *
 * To use them:
 * @code
 * #include <kdbinternal.h>
 * @endcode
 *
 * There are some areas where libraries have to reimplement
 * some basic functions to archive support for non-standard
 * systems, for testing purposes or to provide a little more
 * convenience.
 *
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#include "kdbinternal.h"

/**
 * Copies the key array2 into where array1 points.
 * It copies size elements.
 *
 * Overlapping is prohibited, use elektraMemmove() instead.
 *
 * @param array1 the destination
 * @param array2 the source
 * @param size how many pointer to Keys to copy
 * @return -1 on null pointers
 * @return 0 if nothing was done
 * @return size how many keys were copied
 */
ssize_t elektraMemcpy (Key** array1, Key** array2, size_t size)
{
	if (!array1) return -1;
	if (!array2) return -1;
	if (size > SSIZE_MAX) return -1;
	if (size == 0) return 0;
	memcpy (array1, array2, size * sizeof(Key*));
	return size;
}

/**
 * Copies the key array2 into where array1 points.
 * It copies size elements.
 *
 * Overlapping is ok. If they do not overlap consider
 * elektraMemcpy() instead.
 *
 * @param array1 the destination
 * @param array2 the source
 * @param size how many pointer to Keys to copy
 * @return -1 on null pointers
 * @return 0 if nothing was done
 * @return size how many keys were copied
 */
ssize_t elektraMemmove (Key** array1, Key** array2, size_t size)
{
	if (!array1) return -1;
	if (!array2) return -1;
	if (size > SSIZE_MAX) return -1;
	if (size == 0) return 0;
	memmove (array1, array2, size * sizeof(Key*));
	return size;
}

/**Compare Strings using kdb semantics.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 *
 * / is handled special, it will always be prefered
 * for any other character.
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @return a number less than, equal to or greater than zero if
 *    s1 is found, respectively, to be less than, to match, or
 *    be greater than s2.
 **/
int elektraStrCmp (const char *s1, const char *s2)
{
	int c1;
	int c2;

	if (s1 == s2) return 0;

	for(; *s1 == *s2; ++s1, ++s2)
		if(*s1 == 0)
			return 0;

	c1 = *(unsigned char *)s1;
	c2 = *(unsigned char *)s2;
	if (c1 == '\0')
	{
		c1 = 0;
	} else {
		if (c1 == '/') c1 = 1;
		else ++c1;
	}

	if (c2 == '\0')
	{
		c2 = 0;
	} else {
		if (c2 == '/') c2 = 1;
		else ++c2;
	}

	return c1 - c2;
}


/**Compare Strings ignoring case using kdb semantics.
 *
 * TODO: semantics not correct
 * Does not work with binary sort.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @return 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 **/
int elektraStrCaseCmp (const char *s1, const char *s2)
{
	const unsigned char *p1 = (const unsigned char *)s1;
	const unsigned char *p2 = (const unsigned char *)s2;
	int result;

	if (p1 == p2) return 0;

	while ((result = tolower(*p1)-tolower(*p2 ++)) == 0)
	{
		if (*p1++ == '\0') break;
	}

	return result;
}

/**Reallocate Storage in a save way.
 *
 *@code
if (elektraRealloc ((void **) & buffer, new_length) < 0) {
	// here comes the failure handler
	// you can still use the old buffer
#if DEBUG
	fprintf (stderr, "Reallocation error\n");
#endif
	free (buffer);
	buffer = 0;
	// return with error
}
 *@endcode
 *
 * @param buffer is a pointer to a malloc
 * @param size is the new size for the memory
 * @return -1 on failure
 * @return 0 on success
 * @ingroup internal
 */
int elektraRealloc (void ** buffer, size_t size)
{
	void * ptr;
	void * svr = *buffer;
	ptr = realloc(*buffer, size);
	if (ptr == NULL)
	{
		*buffer = svr;	/* restore old buffer*/
		return -1;
	} else {
		*buffer = ptr;
		return 0;
	}
}

/**
 * Allocate memory for Elektra.
 *
 * @code
if ((buffer = elektraMalloc (length)) == 0) {
	// here comes the failure handler
	// no allocation happened here, so don't use buffer
#if DEBUG
	fprintf (stderr, "Allocation error");
#endif
	// return with error
}
 * @endcode
 *
 * @param size the requested size
 *
 * This function is compatible to ANSI-C malloc
 * @see elektraFree
 * @see elektraCalloc
 */
void* elektraMalloc (size_t size)
{
	return malloc (size);
}

/**Allocate memory for Elektra.
 *
 * Memory will be set to 0.
 *
 * @param the requested size
 * @see elektraMalloc
 */
void* elektraCalloc (size_t size)
{
	return calloc(1, size);
}

/**Free memory of elektra or its backends.
 *
 *@param ptr the pointer to free
 *
 * @ingroup internal
 *@see elektraMalloc
 */
void elektraFree (void *ptr)
{
	free (ptr);
}


/**Copy string into new allocated memory.
 *
 * You need to free the memory yourself.
 *
 * @note that size is determined at runtime.
 *       So if you have a size information, don't use
 *       that function.
 *
 * @param s the null-terminated string to duplicate
 *
 * @ingroup internal
 * @return 0 if out of memory, a pointer otherwise
 * @pre s must be a c-string.
 * @see elektraFree
 * @see elektraStrLen
 * @see elektraStrNDup
 */
char *elektraStrDup (const char *s)
{
	void *tmp = 0;
	size_t l = 0;

	l = elektraStrLen(s);
	tmp = elektraMalloc (l);
	if (tmp) memcpy (tmp, s, l);

	return tmp;
}

/**Copy buffer into new allocated memory.
 *
 * You need to free the memory yourself.
 *
 * This function also works with \\0 characters
 * in the buffer. The length is taken as given,
 * it must be correct.
 *
 * @return 0 if out of memory, a pointer otherwise
 * @param s must be a allocated buffer
 * @param l the length of s
 * @ingroup internal
 */
char *elektraStrNDup (const char *s, size_t l)
{
	void *tmp = 0;

	tmp = elektraMalloc (l);
	if (tmp) memcpy (tmp, s, l);

	return tmp;
}


/**
 * Calculates the length in bytes of a string.
 *
 * This function differs from strlen() because it is Unicode and multibyte
 * chars safe. While strlen() counts characters and ignores the final NULL,
 * elektraStrLen() count bytes including the ending NULL.
 *
 * @ingroup internal
 * @param s the string to get the length from
 * @return number of bytes used by the string, including the final NULL.
 * @ingroup internal
 */
size_t elektraStrLen(const char *s)
{
	char *found=strchr(s,0);
	if (found) return found-s+1;
	return 0;
}
