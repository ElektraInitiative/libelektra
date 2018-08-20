/**
 * @file
 *
 * @brief Source for DynArray, a simple dynamic array for meta-key deduplication.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "dynarray.h"

#include <kdblogger.h>

/* -- DynArray Implementation ----------------------------------------------------------------------------------------------------------- */

/**
 * @brief Allocator for DynArray.
 *
 * @return newly allocated DynArray
 */
DynArray * elektraMmapDynArrayNew (void)
{
	DynArray * dynArray = elektraCalloc (sizeof (DynArray));
	dynArray->keyArray = elektraCalloc (sizeof (Key *) * 8);
	dynArray->mappedKeyArray = 0;
	dynArray->size = 0;
	dynArray->alloc = 8;
	return dynArray;
}

/**
 * @brief Deallocator for DynArray.
 *
 * @param dynArray to be free()'d
 */
void elektraMmapDynArrayDelete (DynArray * dynArray)
{
	if (!dynArray)
	{
		return;
	}
	if (dynArray->keyArray)
	{
		elektraFree (dynArray->keyArray);
	}
	if (dynArray->mappedKeyArray)
	{
		elektraFree (dynArray->mappedKeyArray);
	}
	elektraFree (dynArray);
}

/**
 * @brief Find position or insert Key pointer into DynArray.
 *
 * @param key to be found or inserted
 * @param dynArray where the key should be searched for or inserted
 *
 * @retval -1 on memory error (malloc failed), or size exceeded
 * @retval 0 if the key was inserted
 * @retval 1 if the key was found
 */
int elektraMmapDynArrayFindOrInsert (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_DEBUG ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
		{
			h = m;
		}
		else if (dynArray->keyArray[m] < key)
		{
			l = ++m;
		}
		else
		{
			return 1; // found
		}
	}
	// insert key at index l
	if (dynArray->size == dynArray->alloc)
	{
		// doubling the array size to keep reallocations logarithmic
		size_t oldAllocSize = dynArray->alloc;
		if (oldAllocSize > (SIZE_MAX / 2))
		{
			return -1; // error
		}
		Key ** new = elektraCalloc ((2 * oldAllocSize) * sizeof (Key *));
		memcpy (new, dynArray->keyArray, dynArray->size * sizeof (Key *));
		elektraFree (dynArray->keyArray);
		dynArray->keyArray = new;
		dynArray->alloc = 2 * oldAllocSize;
	}

	memmove ((void *) (dynArray->keyArray + l + 1), (void *) (dynArray->keyArray + l), ((dynArray->size) - l) * (sizeof (size_t)));
	dynArray->keyArray[l] = key;
	dynArray->size += 1;

	return 0; // inserted
}

/**
 * @brief Find Key pointer in the DynArray.
 *
 * @param key Key pointer to search for
 * @param dynArray where the Key should be searched for
 *
 * @return position of the Key pointer in the DynArray, or -1 if not found or size exceeded
 */
ssize_t elektraMmapDynArrayFind (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_DEBUG ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
		{
			h = m;
		}
		else if (dynArray->keyArray[m] < key)
		{
			l = ++m;
		}
		else
		{
			if (m < SSIZE_MAX)
			{
				return m; // found
			}
			else
			{
				return -1;
			}
		}
	}

	return -1;
}
