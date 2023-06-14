/**
 * @file
 *
 * @brief Helper for memory management.
 *
 * This file contains the implementations for the functions declared in
 * <elektra/utility/alloc.h> and <internal/utility/alloc.h>
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_ALLOC_IMPL_H
#define ELEKTRA_UTILITY_ALLOC_IMPL_H

#include <internal/utility/assert.h>

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**Reallocate Storage in a save way.
 *
 *@code
if (elektraRealloc ((void **) & buffer, new_length) < 0) {
	// here comes the failure handler
	// you can still use the old buffer
#if DEBUG
	fprintf (stderr, "Reallocation error\n");
#endif
	elektraFree (buffer);
	buffer = 0;
	// return with error
}
 *@endcode
 *
 * @param buffer is a pointer to a elektraMalloc
 * @param size is the new size for the memory
 * @retval -1 on failure
 * @retval 0 on success
 * @ingroup internal
 */
int elektraRealloc (void ** buffer, size_t size)
{
	ELEKTRA_ASSERT (size, "Size to allocate is zero (implementation defined behavior)");

	void * ptr = realloc (*buffer, size);
	ELEKTRA_ASSERT (ptr, "Memory (re)allocation failed with size %zu", size);
	if (ptr == NULL)
	{
		return -1;
	}

	*buffer = ptr;
	return 0;
}

/**Free memory of Elektra or its backends.
 *
 * @param ptr the pointer to free
 *
 * If ptr is NULL, no operation is performed.
 *
 * @ingroup internal
 * @see elektraMalloc
 */
void elektraFree (void * ptr)
{
	free (ptr);
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
void * elektraMalloc (size_t size)
{
	ELEKTRA_ASSERT (size, "Size to allocate is zero (implementation defined behavior)");
	void * ret = malloc (size);
	ELEKTRA_ASSERT (ret, "Memory allocation failed with size %zu", size);
	return ret;
}

/**Allocate memory for Elektra.
 *
 * Memory will be set to 0.
 *
 * @param size the requested size
 * @see elektraMalloc
 */
void * elektraCalloc (size_t size)
{
	ELEKTRA_ASSERT (size, "Size to allocate is zero (implementation defined behavior)");
	void * ret = calloc (1, size);
	ELEKTRA_ASSERT (ret, "Memory allocation failed with size %zu", size);
	return ret;
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
 * @see elektraMemDup
 */
char * elektraStrDup (const char * s)
{
	char * tmp = 0;
	size_t l = 0;
	ELEKTRA_ASSERT (s, "Tried to duplicate null pointer");

	l = strlen (s) + 1;
	ELEKTRA_ASSERT (l, "Size of string to duplicate is zero");
#ifdef __cplusplus
	tmp = static_cast<char *> (elektraMalloc (l));
#else
	tmp = elektraMalloc (l);
#endif
	if (tmp)
	{
		memcpy (tmp, s, l);
	}

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
 * @param s must be an allocated buffer
 * @param n the number of bytes to copy from s
 * @ingroup internal
 */
void * elektraMemDup (const void * s, size_t n)
{
	void * tmp = 0;
	ELEKTRA_ASSERT (n, "Size of memory to duplicate is zero");

	tmp = elektraMalloc (n);
	if (tmp) memcpy (tmp, s, n);

	return tmp;
}

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_ALLOC_IMPL_H
