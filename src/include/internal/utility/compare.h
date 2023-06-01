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

int elektraStrCmp (const char * s1, const char * s2);
int elektraStrNCmp (const char * s1, const char * s2, size_t n);
int elektraStrCaseCmp (const char * s1, const char * s2);
int elektraStrNCaseCmp (const char * s1, const char * s2, size_t n);

int elektraMemCaseCmp (const char * s1, const char * s2, size_t size);

#undef VOID_CAST

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_COMPARE_H
