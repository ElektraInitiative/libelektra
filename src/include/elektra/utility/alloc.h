/**
 * @file
 *
 * @brief Helper for memory management.
 *
 * May be used to match the internally used allocator.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_UTILITY_ALLOC_H
#define ELEKTRA_UTILITY_ALLOC_H

#include <stddef.h>

void * elektraMalloc (size_t size);
void * elektraCalloc (size_t size);
int elektraRealloc (void ** buffer, size_t size);
void elektraFree (void * ptr);

char * elektraStrDup (const char * s);
void * elektraMemDup (const void * s, size_t n);

#endif // ELEKTRA_UTILITY_ALLOC_H
