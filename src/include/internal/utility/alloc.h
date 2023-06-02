/**
 * @file
 *
 * @brief Helper for memory management.
 *
 * Should be always preferred.
 * Can be used for profiling and tracing.
 *
 * Mostly static inline functions with additional asserts.
 * The static inline should improve compiler optimizations.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_ALLOC_H
#define ELEKTRA_UTILITY_ALLOC_H

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

int elektraRealloc (void ** buffer, size_t size);
void * elektraMalloc (size_t size);
void * elektraCalloc (size_t size);
void elektraFree (void * ptr);

char * elektraStrDup (const char * s);
void * elektraMemDup (const void * s, size_t n);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_UTILITY_ALLOC_H
