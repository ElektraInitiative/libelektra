/**
 * @file
 *
 * @brief Internal methods for Elektra.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#ifdef HAVE_STDIO_H
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
#include <kdbassert.h>

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

/**
 * Copies the key array2 into where array1 points.
 * It copies size elements.
 *
 * Overlapping is prohibited, use elektraMemmove() instead.
 *
 * @param array1 the destination
 * @param array2 the source
 * @param size how many pointer to Keys to copy
 * @retval -1 on null pointers
 * @retval 0 if nothing was done
 * @return size how many keys were copied
 */
ssize_t elektraMemcpy (Key ** array1, Key ** array2, size_t size)
{
	if (!array1) return -1;
	if (!array2) return -1;
	if (size > SSIZE_MAX) return -1;
	if (size == 0) return 0;
#if DEBUG
	char * a = (char *) array1;
	char * b = (char *) array2;
	for (size_t i = 0; i < size; i++)
	{
		ELEKTRA_ASSERT (a + i != b && b + i != a, "memcpy overlap: %p and %p with size %zu", (void *) a, (void *) b, size);
	}
#endif
	memcpy (array1, array2, size * sizeof (Key *));
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
 * @retval -1 on null pointers
 * @retval 0 if nothing was done
 * @return size how many keys were copied
 */
ssize_t elektraMemmove (Key ** array1, Key ** array2, size_t size)
{
	if (!array1) return -1;
	if (!array2) return -1;
	if (size > SSIZE_MAX) return -1;
	if (size == 0) return 0;
	memmove (array1, array2, size * sizeof (Key *));
	return size;
}

/**@brief Compare Strings.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @retval 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 **/
int elektraStrCmp (const char * s1, const char * s2)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", (void *) s1, (void *) s2);

	return strcmp (s1, s2);
}

/**@brief Compare Strings up to a maximum length.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 * @param n The maximum length to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @retval 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 **/
int elektraStrNCmp (const char * s1, const char * s2, size_t n)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", (void *) s1, (void *) s2);

	return strncmp (s1, s2, n);
}

/**@brief Compare Strings ignoring case.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @retval 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 **/
int elektraStrCaseCmp (const char * s1, const char * s2)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", (void *) s1, (void *) s2);
	return strcasecmp (s1, s2);
}

/**@brief Compare Strings ignoring case up to a maximum length.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 * @param n The maximum length to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @retval 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 **/
int elektraStrNCaseCmp (const char * s1, const char * s2, size_t n)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", (void *) s1, (void *) s2);
	return strncasecmp (s1, s2, n);
}

/**
 * @brief Compare two memory regions but make cmp chars uppercase before
 * comparison.
 *
 * @param s1 The first string to be compared
 * @param s2 The second string to be compared
 * @param size to be compared
 *
 * @ingroup internal
 * @return a negative number if s1 is less than s2
 * @retval 0 if s1 matches s2
 * @return a positive number if s1 is greater than s2
 */
int elektraMemCaseCmp (const char * s1, const char * s2, size_t size)
{
	size_t i;
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", (void *) s1, (void *) s2);
	for (i = 0; i < size; i++)
	{
		const unsigned char cmp1 = s1[i];
		const unsigned char cmp2 = s2[i];
		const int CMP1 = toupper (cmp1);
		const int CMP2 = toupper (cmp2);
		const int diff = CMP1 - CMP2;
		if (diff) return diff;
	}
	return 0;
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
	void * ptr;
	void * svr = *buffer;

	ptr = realloc (*buffer, size);
	ELEKTRA_ASSERT (ptr, "Memory (re)allocation failed with size %zu", size);
	if (ptr == NULL)
	{
		*buffer = svr; /* restore old buffer*/
		return -1;
	}
	else
	{
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
char * elektraStrDup (const char * s)
{
	void * tmp = 0;
	size_t l = 0;
	ELEKTRA_ASSERT (s, "Tried to duplicate null pointer");

	l = elektraStrLen (s);
	ELEKTRA_ASSERT (l, "Size of string to duplicate is zero");
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
 * @param s must be an allocated buffer
 * @param l the length of s
 * @ingroup internal
 */
char * elektraStrNDup (const char * s, size_t l)
{
	void * tmp = 0;
	ELEKTRA_ASSERT (l, "Size for string duplicate is zero");

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
 * It must not be used to search for / in the name, because it does not
 * consider escaping. Instead use the unescaped name.
 *
 * @see keyUnescapedName()
 *
 * @ingroup internal
 * @param s the string to get the length from
 * @return number of bytes used by the string, including the final NULL.
 * @ingroup internal
 */
size_t elektraStrLen (const char * s)
{
	ELEKTRA_ASSERT (s, "Got null pointer");

	char * found = strchr (s, 0);
	if (found) return found - s + 1;
	return 0;
}

/**
 * @brief Does string formatting in fresh allocated memory
 *
 * @param format as in printf()
 * @param ... as in printf()
 *
 * @return new allocated memory (free with elektraFree)
 */
char * elektraFormat (const char * format, ...)
{
	ELEKTRA_ASSERT (format, "Got null pointer");

	va_list va;
	va_start (va, format);
	char * ret = elektraVFormat (format, va);
	va_end (va);
	return ret;
}

/**
 * @brief Does string formatting in fresh allocated memory
 *
 * @param format as in vprintf()
 * @param arg_list as in vprintf()
 *
 * @return new allocated memory (free with elektraFree)
 */
char * elektraVFormat (const char * format, va_list arg_list)
{
	ELEKTRA_ASSERT (format, "Got null pointer");

	static int const default_size = 512;
	char * buffer = elektraMalloc (default_size);
	if (!buffer) return 0;

	va_list arg_list_adj;
	va_copy (arg_list_adj, arg_list);

	int const calculated_length = vsnprintf (buffer, default_size, format, arg_list);

	if (calculated_length == -1)
	{
		va_end (arg_list_adj);
		elektraFree (buffer);
		// before Glibc 2.0.6, always -1 is returned
		// we won't do Glibc job, please upgrade
		return 0;
	}

	if (calculated_length < default_size)
	{
		va_end (arg_list_adj);
		// content was written successfully into
		// default sized buffer
		return buffer;
	}

	// String is longer than default_size.
	// Allocate an intermediate buffer
	// according to the calculated length from our last try
	size_t const adjusted_buffer_size = calculated_length + 1;
	elektraRealloc ((void **) &buffer, adjusted_buffer_size);
	if (!buffer)
	{
		va_end (arg_list_adj);
		return 0;
	}

	int const ret = vsnprintf (buffer, adjusted_buffer_size, format, arg_list_adj);

	va_end (arg_list_adj);

	if (ret == -1)
	{
		elektraFree (buffer);
		return 0;
	}
	return buffer;
}


/**
 * Validates whether the supplied keyname is valid.
 *
 * The function looks for tangling escape characters in the end
 * and for a minimum length.
 *
 * Does not check for valid namespaces
 *
 * @pre size must be at least 2
 *
 * @param name the key name that is to be checked
 * @param size a elektraStrLen of the key name
 * @retval true if the supplied keyname part is valid
 * @retval false if its invalid
 */
int elektraValidateKeyName (const char * name, size_t size)
{
	ELEKTRA_ASSERT (name, "Got null pointer");
	ELEKTRA_ASSERT (size >= 2, "size too small %zu", size);

	size_t escapeCount = 0;

	size -= 2; // forward null character to last character

	// now do backwards iteration
	while (size && name[size] == '\\')
	{
		++escapeCount;
		--size;
	}

	return (escapeCount % 2) == 0; // only allow equal number of escapes in the end
}

/**
 * @internal
 *
 * @brief Write number backslashes to dest
 *
 * @param dest where to write to, will be updated to position after
 *        the written backslashes
 * @param number of backslashes to write
 */
static void elektraWriteBackslashes (char ** dest, size_t number)
{
	ELEKTRA_ASSERT (dest, "Got null pointer");
	ELEKTRA_ASSERT (*dest, "Got null pointer (*dest)");

	char * dp = *dest;
	while (number)
	{
		*dp = '\\';
		++dp;
		--number;
	}
	*dest = dp;
}

/**
 * @internal
 *
 * @brief Unescapes the beginning of the key name part
 *
 * If there was something to escape in the begin, then it is guaranteed
 * that nothing more needs to be escaped.
 *
 * Otherwise this method does not change anything
 *
 * @param source the source to read from
 * @param size the number of bytes to process from source
 * @param [in] dest the destination to write to
 * @param [out] dest pointer after writing to it (w/o null, 1 after the
 *        last character)
 *
 * @retval 0 if nothing was done (dest unmodified) and escaping of
 *         string needs to be done
 * @retval 1 if key name part was handled correctly (dest might be
 *         updated if it was needed)
 */
int elektraUnescapeKeyNamePartBegin (const char * source, size_t size, char ** dest)
{
	const char * sp = source;
	char * dp = *dest;

	ELEKTRA_ASSERT (sp != NULL && dp != NULL, "Got null pointer sp: %p dp: %p", (void *) sp, (void *) dp);

	if (!strncmp ("%", sp, size))
	{
		// nothing to do, but part is finished
		return 1;
	}

	size_t skippedBackslashes = 0;
	// skip all backslashes, but one, at start of a name
	while (*sp == '\\')
	{
		++sp;
		++skippedBackslashes;
	}
	size -= skippedBackslashes;

	if (skippedBackslashes > 0)
	{
		// correct by one (avoid lookahead in loop)
		--sp;
		++size;
		--skippedBackslashes;
	}

	if (size <= 1)
	{
		// matches below would be wrong
		return 0;
	}

	if (!strncmp ("\\%", sp, size))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, "%");
		*dest = dp + 1;
		return 1;
	}

	if (!strncmp ("\\.", sp, size))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, ".");
		*dest = dp + 1;
		return 1;
	}

	if (size <= 2)
	{
		// matches below would be wrong
		return 0;
	}

	if (!strncmp ("\\..", sp, size))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, "..");
		*dest = dp + 2;
		return 1;
	}

	return 0;
}


/**
 * @internal
 *
 * @brief Unescapes (a part of) a key name.
 *
 * As described in Syntax for Key Names, slashes are
 * prefixed with a \\ (or uneven number thereof). This method removes all \\ that are such
 * escape characters.
 *
 * The new string will be written to dest.
 * May only need half the storage than the source string.
 * It is not safe to use the same string for source and dest.
 *
 * @param source the source to read from
 * @param size the number of bytes to process from source
 * @param dest the destination to write to
 *
 * @return the destination pointer how far it was written to
 */
char * elektraUnescapeKeyNamePart (const char * source, size_t size, char * dest)
{
	const char * sp = source;
	char * dp = dest;
	size_t count = 0;

	ELEKTRA_ASSERT (sp != NULL && dp != NULL, "Got null pointer sp: %p dp: %p", (void *) sp, (void *) dp);

	while (size)
	{
		if (*sp == '\\')
		{
			++count;
		}
		else if (*sp == '/')
		{
			// we escape a part, so there had to be a backslash
			ELEKTRA_ASSERT (count > 0, "no backslash found, count is %zu", count);
			ELEKTRA_ASSERT ((count % 2) == 1, "counted uneven number of backslashes: %zu", count);

			count /= 2;
			while (count)
			{
				*dp = '\\';
				++dp;
				--count;
			}

			*dp = *sp;
			++dp;
		}
		else
		{
			// output delayed backslashes
			while (count)
			{
				*dp = '\\';
				++dp;
				--count;
			}

			*dp = *sp;
			++dp;
		}
		++sp;
		--size;
	}

	ELEKTRA_ASSERT ((count % 2) == 0, "uneven number of backslashes: %zu", count);
	count /= 2;
	while (count)
	{
		*dp = '\\';
		++dp;
		--count;
	}
	return dp;
}

/**
 * @internal
 *
 * @brief Unescapes a key name.
 *
 * Writes a null terminated sequence of key name parts to dest.
 *
 * May only need half the storage than the source string.
 * It is not safe to use the same string for source and dest.
 **/
size_t elektraUnescapeKeyName (const char * source, char * dest)
{
	const char * sp = source;
	char * dp = dest;
	size_t size = 0;

	ELEKTRA_ASSERT (sp != NULL && dp != NULL, "Got null pointer sp: %p dp: %p", (void *) sp, (void *) dp);

	// if there is nothing to unescape, just make a copy and replace / with \0
	if (strpbrk (sp, "\\%") == NULL)
	{
		strcpy (dest, sp);
		char * last = dp;
		while ((dp = strchr (dp, '/')) != NULL)
		{
			*dp = '\0';
			++dp;
			last = dp;
		}
		// add 1, if we didn't end with \0 already
		return last - dest + strlen (last) + (*last != '\0');
	}

	if (*sp == '/')
	{
		// handling for cascading names
		*dp = 0;
		++dp;
	}
	while (*(sp = keyNameGetOneLevel (sp + size, &size)))
	{
		if (!elektraUnescapeKeyNamePartBegin (sp, size, &dp))
		{
			dp = elektraUnescapeKeyNamePart (sp, size, dp);
		}
		*dp = 0;
		++dp;
	}
	return dp - dest;
}

/**
 * @internal
 *
 * Escapes (a part of) a key name.
 */
int elektraEscapeKeyNamePartBegin (const char * source, char * dest)
{
	const char * sp = source;
	char * dp = dest;

	ELEKTRA_ASSERT (sp != NULL && dp != NULL, "Got null pointer sp: %p dp: %p", (void *) sp, (void *) dp);

	if (!strcmp ("", sp))
	{
		strcpy (dp, "%");
		return 1;
	}

	size_t skippedBackslashes = 0;
	// skip all backslashes at start of a name
	while (*sp == '\\')
	{
		++sp;
		++skippedBackslashes;
	}

	if (!strcmp ("%", sp))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, "\\%");
		return 1;
	}

	if (!strcmp (".", sp))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, "\\.");
		return 1;
	}

	if (!strcmp ("..", sp))
	{
		elektraWriteBackslashes (&dp, skippedBackslashes);
		strcpy (dp, "\\..");
		return 1;
	}

	return 0;
}


/**
 * @internal
 *
 * @brief Escapes character in the part of a key name.
 *
 * As described in Syntax for Key Names, special characters will be
 * prefixed with a \\. No existing escaping is assumed. That means
 * that even sequences that look like escapings will be escaped again.
 * For example, \\/ will be escaped (or quoted) to \\\\\\/.
 *
 * The string will be written to dest.
 *
 * @note May need twice the storage than the source string.
 *       Do not use the source string as destination string.
 *
 * @param source the source pointer where escaping should start
 * @param dest the destination to write to (twice the size as sp)
 *
 * @return pointer to destination
 */
char * elektraEscapeKeyNamePart (const char * source, char * dest)
{
	if (elektraEscapeKeyNamePartBegin (source, dest))
	{
		return dest;
	}

	size_t count = 0;

	const char * sp = source;
	char * dp = dest;

	ELEKTRA_ASSERT (sp != NULL && dp != NULL, "Got null pointer sp: %p dp: %p", (void *) sp, (void *) dp);

	while (*sp)
	{
		if (*sp == '\\')
		{
			++count;
		}
		else if (*sp == '/')
		{
			// escape every slash
			*dp = '\\';
			++dp;
			// and print escaped slashes
			while (count)
			{
				*dp = '\\';
				++dp;
				--count;
			}
		}
		else
		{
			count = 0;
		}
		*dp = *sp;
		++dp;
		++sp;
	}
	// print other escaped backslashes at end of part
	while (count)
	{
		*dp = '\\';
		++dp;
		--count;
	}
	*dp = 0;
	return dest;
}
