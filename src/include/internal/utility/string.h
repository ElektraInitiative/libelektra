/**
 * @file
 *
 * @brief Convenience Methods for String Handling
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_STRING_H
#define ELEKTRA_UTILITY_STRING_H

#include <internal/utility/assert.h>
#include <string.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

char * elektraLskip (char const * const keyname);
char * elektraRstrip (char * const start, char ** end);
char * elektraStrip (char * text);

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
static inline size_t elektraStrLen (const char * s)
{
	ELEKTRA_ASSERT (s, "Got null pointer");

	return strlen (s) + 1;
}


#ifdef __cplusplus
}
}
#endif


#endif // ELEKTRA_UTILITY_STRING_H
