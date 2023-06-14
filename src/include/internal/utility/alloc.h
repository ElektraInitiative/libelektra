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

#ifndef ELEKTRA_UTILITY_ALLOC_INTERNAL_H
#define ELEKTRA_UTILITY_ALLOC_INTERNAL_H

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

static inline void * elektraMalloc (size_t size);
static inline void * elektraCalloc (size_t size);
static inline int elektraRealloc (void ** buffer, size_t size);
static inline void elektraFree (void * ptr);

static inline char * elektraStrDup (const char * s);
static inline void * elektraMemDup (const void * s, size_t n);

#ifdef __cplusplus
}
}
#endif

// now that we have declared the functions as static inline,
// we include a copy of their implementation
#include <internal/utility/alloc_impl.h>

#endif // ELEKTRA_UTILITY_ALLOC_INTERNAL_H
