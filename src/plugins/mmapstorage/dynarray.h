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

struct _dynArray
{
	size_t size;
	size_t alloc;
	ElektraKey ** keyArray;
	ElektraKey ** mappedKeyArray;
};

typedef struct _dynArray DynArray;

// DynArray functions
DynArray * ELEKTRA_PLUGIN_FUNCTION (dynArrayNew) (void);
void ELEKTRA_PLUGIN_FUNCTION (dynArrayDelete) (DynArray * dynArray);
int ELEKTRA_PLUGIN_FUNCTION (dynArrayFindOrInsert) (ElektraKey * key, DynArray * dynArray);
ssize_t ELEKTRA_PLUGIN_FUNCTION (dynArrayFind) (ElektraKey * key, DynArray * dynArray);

#endif
