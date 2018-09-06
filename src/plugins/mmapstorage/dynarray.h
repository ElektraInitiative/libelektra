/**
 * @file
 *
 * @brief Header for DynArray, a simple dynamic array for meta-key deduplication.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#ifndef ELEKTRA_MMAPSTORAGE_DYNARRAY_H
#define ELEKTRA_MMAPSTORAGE_DYNARRAY_H

#include <kdbprivate.h>

#include <sys/types.h> // size_t

struct _dynArray
{
	size_t size;
	size_t alloc;
	Key ** keyArray;
	Key ** mappedKeyArray;
};

typedef struct _dynArray DynArray;

// DynArray functions
DynArray * ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayNew) (void);
void ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayDelete) (DynArray * dynArray);
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayFindOrInsert) (Key * key, DynArray * dynArray);
ssize_t ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayFind) (Key * key, DynArray * dynArray);

#endif
