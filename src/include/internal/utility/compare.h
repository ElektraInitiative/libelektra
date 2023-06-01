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

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraStrCmp (const char * s1, const char * s2);
int elektraStrNCmp (const char * s1, const char * s2, size_t n);
int elektraStrCaseCmp (const char * s1, const char * s2);
int elektraStrNCaseCmp (const char * s1, const char * s2, size_t n);

int elektraMemCaseCmp (const char * s1, const char * s2, size_t size);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_COMPARE_H
