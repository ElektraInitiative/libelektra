/**
 * @file
 *
 * @brief Source for DynArray, a simple dynamic array for meta-key deduplication.
 *
 * The DynArray is used to store pointers of meta-keys. The dynArrayFindOrInsert function
 * searches for a pointer in the structure. If it is not yet in the array, it will be inserted.
 * If the underlying array is too small, it is resized such that it can accomodate further elements.
 * The dynArrayFind function only searches for elements.
 *
 * The mmapstorage plugin uses the DynArray to deduplicate meta-keys.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./dynarray.h"

#include <internal/macros/os.h>
#include <internal/utility/logger.h>

/* -- DynArray Implementation ----------------------------------------------------------------------------------------------------------- */

#define ELEKTRA_MMAP_DYNARRAY_MINSIZE (8)

/**
 * @brief Allocator for DynArray.
 *
 * @return newly allocated DynArray
 */
DynArray * ELEKTRA_PLUGIN_FUNCTION (dynArrayNew) (void)
{
	DynArray * dynArray = elektraCalloc (sizeof (DynArray));
	dynArray->keyArray = elektraCalloc (sizeof (Key *) * ELEKTRA_MMAP_DYNARRAY_MINSIZE);
	dynArray->mappedKeyArray = 0;
	dynArray->size = 0;
	dynArray->alloc = ELEKTRA_MMAP_DYNARRAY_MINSIZE;
	return dynArray;
}

/**
 * @brief Deallocator for DynArray.
 *
 * @param dynArray to be free()'d
 */
void ELEKTRA_PLUGIN_FUNCTION (dynArrayDelete) (DynArray * dynArray)
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
int ELEKTRA_PLUGIN_FUNCTION (dynArrayFindOrInsert) (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		size_t m = (l + h) >> 1;
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
ssize_t ELEKTRA_PLUGIN_FUNCTION (dynArrayFind) (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		size_t m = (l + h) >> 1;
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
