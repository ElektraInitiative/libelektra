/**
 * @file
 *
 * @brief Helpers for comparing
 *
 * Mostly static inline functions with additional asserts.
 * The static inline should improve compiler optimizations.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_COMPARE_H
#define ELEKTRA_UTILITY_COMPARE_H

#include <internal/utility/assert.h>

#include <stddef.h>
#include <string.h>
#include <strings.h>

#ifdef __cplusplus
#define VOID_CAST(x) (static_cast<const void *> (x))
namespace ckdb
{
extern "C" {
#else
#define VOID_CAST(x) ((const void *) (x))
#endif

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
static inline int elektraStrCmp (const char * s1, const char * s2)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", VOID_CAST (s1), VOID_CAST (s2));

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
static inline int elektraStrNCmp (const char * s1, const char * s2, size_t n)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", VOID_CAST (s1), VOID_CAST (s2));

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
static inline int elektraStrCaseCmp (const char * s1, const char * s2)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", VOID_CAST (s1), VOID_CAST (s2));
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
static inline int elektraStrNCaseCmp (const char * s1, const char * s2, size_t n)
{
	ELEKTRA_ASSERT (s1 != NULL && s2 != NULL, "Got null pointer s1: %p s2: %p", VOID_CAST (s1), VOID_CAST (s2));
	return strncasecmp (s1, s2, n);
}

int elektraMemCaseCmp (const char * s1, const char * s2, size_t size);

#undef VOID_CAST

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_COMPARE_H
