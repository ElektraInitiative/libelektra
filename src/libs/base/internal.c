/**
 * @file
 *
 * @brief Internal methods for Elektra.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <elektra/core/errors.h>
#include <elektra/core/key.h>
#include <elektra/core/namespace.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>

#include <internal/config.h>
#include <internal/macros/os.h>
#include <internal/pluginload/module.h>
#include <internal/utility/alloc.h>
#include <internal/utility/assert.h>
#include <internal/utility/format.h>
#include <internal/utility/logger.h>
#include <internal/utility/string.h>

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

elektraNamespace elektraReadNamespace (const char * namespaceStr, size_t len)
{
	if (len == 0) return KEY_NS_NONE;
	if (len == strlen ("system") && strncmp (namespaceStr, "system", len) == 0) return KEY_NS_SYSTEM;
	if (len == strlen ("user") && strncmp (namespaceStr, "user", len) == 0) return KEY_NS_USER;
	if (len == strlen ("dir") && strncmp (namespaceStr, "dir", len) == 0) return KEY_NS_DIR;
	if (len == strlen ("proc") && strncmp (namespaceStr, "proc", len) == 0) return KEY_NS_PROC;
	if (len == strlen ("spec") && strncmp (namespaceStr, "spec", len) == 0) return KEY_NS_SPEC;
	if (len == strlen ("meta") && strncmp (namespaceStr, "meta", len) == 0) return KEY_NS_META;
	if (len == strlen ("default") && strncmp (namespaceStr, "default", len) == 0) return KEY_NS_DEFAULT;
	return KEY_NS_NONE;
}
