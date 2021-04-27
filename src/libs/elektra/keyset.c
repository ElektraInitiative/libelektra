/**
 * @file
 *
 * @brief Methods for key sets.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbprivate.h"
#include <elektra/kdb.h>
#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <kdbtypes.h>

#include "kdbinternal.h"
#include <kdbassert.h>
#include <kdbrand.h>


#define ELEKTRA_MAX_PREFIX_SIZE sizeof ("namespace/")
#define ELEKTRA_MAX_NAMESPACE_SIZE sizeof ("system")

/**
 * @internal
 *
 * @brief KeySets OPMPHM cleaner.
 *
 * Must be invoked by every function that changes a Key name in a KeySet, adds a Key or
 * removes a Key.
 * Set also the KS_FLAG_NAME_CHANGE KeySet flag.
 *
 * @param ks the KeySet
 */
static void elektraOpmphmInvalidate (KeySet * ks ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	set_bit (ks->flags, KS_FLAG_NAME_CHANGE);
	if (ks && ks->opmphm) opmphmClear (ks->opmphm);
#endif
}

/**
 * @internal
 *
 * @brief KeySets OPMPHM and OPMPHM predictor copy.
 *
 * Should be invoked by every function making a copy of a KeySet.
 *
 * @param dest the destination KeySet
 * @param source the source KeySet
 */
static void elektraOpmphmCopy (KeySet * dest ELEKTRA_UNUSED, const KeySet * source ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (!source || !dest)
	{
		return;
	}
	// nothing to copy
	// OPMPHM predictor
	if (source->opmphmPredictor)
	{
		if (!dest->opmphmPredictor)
		{
			dest->opmphmPredictor = opmphmPredictorNew ();
		}
		if (dest->opmphmPredictor)
		{
			opmphmPredictorCopy (dest->opmphmPredictor, source->opmphmPredictor);
		}
	}
	if (!opmphmIsBuild (source->opmphm))
	{
		return;
	}
	// OPMPHM
	if (!dest->opmphm)
	{
		dest->opmphm = opmphmNew ();
	}
	if (dest->opmphm)
	{
		opmphmCopy (dest->opmphm, source->opmphm);
	}
#endif
}

/** @class doxygenFlatCopy
 *
 * @brief .
 *
 * @note Because the key is not copied,
 * also the pointer to the current metadata keyNextMeta()
 * will be shared.
 */

/**
 * @defgroup keyset KeySet
 * @brief Methods to manipulate KeySets.
 *
 * A KeySet is a set of keys.
 *
 * Most important properties of a KeySet:
 *
 * - Allows us to iterate over all keys (in any depth)
 * - Iteration is always sorted
 * - Fast key lookup
 * - A Key may be shared among many KeySets.
 *
 * The most important methods of KeySet:
 *
 * - With ksNew() you can create a new KeySet.
 * - You can append keys with ksAppendKey() or
 *   with ksAppend() you can append a whole keyset.
 * - Using ksLookup() you can lookup (or pop with #KDB_O_POP) a key.
 * - With ksRewind() and ksNext() you can iterate through the keyset.
 *   Be assured that you will get every key of the set in a stable
 *   order (parents before children).
 *
 * @copydetails doxygenFlatCopy
 *
 * KeySets have an @link ksCurrent() internal cursor @endlink.
 * Methods should avoid to change this cursor, unless they want
 * to communicate something with it.
 * The internal cursor is used:
 *
 * - in ksLookup(): points to the found key
 * - in kdbSet(): points to the key which caused an error
 *
 * KeySet is the most important data structure in Elektra. It makes it possible
 * to get and store many keys at once inside the database. In addition to
 * that, the class can be used as high level datastructure in applications
 * and it can be used in plugins to manipulate or check configuration.
 *
 * With ksLookupByName() it is possible to fetch easily specific keys
 * out of the list of keys.
 *
 * You can easily create and iterate keys:
 *
 * @snippet ksNewExample.c Full Example
 *
 * @{
 */


/**
 * Allocate, initialize and return a new KeySet object.
 *
 * Objects created with ksNew() must be destroyed with ksDel().
 *
 * You can use an arbitrary long list of parameters to preload the KeySet
 * with a list of Keys. Either your first and only parameter is 0 or
 * your last parameter must be KS_END.
 *
 * So, terminate with ksNew(0, KS_END) or ksNew(20, ..., KS_END)
 *
 * @warning Never use ksNew(0, keyNew(...), KS_END).
 * If the first parameter is 0, other arguments are ignored.
 *
 * The first parameter @p alloc defines how many Keys can be added
 * without reallocation.
 * If you pass any alloc size greater than 0, but less than 16,
 * it will default to 16.
 *
 * For most uses
 *
 * @snippet ksNew.c Simple
 *
 * will be fine. The alloc size will be 16 and will double whenever
 * size reaches alloc size, so it also performs well with large KeySets.
 *
 * You can defer the allocation of the internal array that holds
 * the Keys, by passing 0 as the alloc size. This is useful if it is
 * unclear whether your KeySet will actually hold any Keys
 * and you want to avoid a malloc call.
 *
 * @snippet ksNew.c No Allocation
 *
 * If the size of the KeySet is known in advance, use the
 * @p alloc parameter to hint the size of the KeySet.
 *
 * If your application only needs up to 15 Keys you can request a KeySet of size 15:
 *
 * @snippet ksNew.c Length 15
 *
 * If you start having 3 Keys, and your application needs approximately
 * 200 up to 500 Keys, you can use:
 *
 * @snippet ksNew.c Hint 500
 *
 * Alloc size is 500, the size of the KeySet will be 3 after ksNew.
 * This means the KeySet will reallocate when appending more than
 * 497 keys.
 *
 * The main benefit of taking a list of variant length parameters is to be able
 * to have one C-Statement for any possible KeySet.
 * If you prefer, you can always create an empty KeySet and use ksAppendKey().
 *
 * @post the KeySet is rewinded properly
 *
 * @param alloc gives a hint for how many Keys may be stored initially
 *
 * @return a ready to use KeySet object
 * @retval 0 on memory error
 *
 * @since 1.0.0
 * @see ksDel() to free the KeySet afterwards
 * @see ksDup() to duplicate an existing KeySet
 * @see ksAppendKey() to append individual Keys to a KeySet
 */
KeySet * ksNew (size_t alloc, ...)
{
	KeySet * ks;
	va_list va;

	va_start (va, alloc);
	ks = ksVNew (alloc, va);
	va_end (va);

	return ks;
}

/**
 * @copydoc ksNew
 *
 * @pre caller must call va_start and va_end
 * @par va the list of arguments
 * @param alloc the allocation size
 * @param va the list of variable arguments
 **/
KeySet * ksVNew (size_t alloc, va_list va)
{
	KeySet * keyset = 0;

	keyset = (KeySet *) elektraMalloc (sizeof (KeySet));
	if (!keyset)
	{
		return 0;
	}

	ksInit (keyset);

	if (alloc == 0) return keyset;

	alloc++; /* for ending null byte */
	if (alloc < KEYSET_SIZE)
		keyset->alloc = KEYSET_SIZE;
	else
		keyset->alloc = alloc;

	keyset->array = elektraMalloc (sizeof (struct _Key *) * keyset->alloc);
	if (!keyset->array)
	{
		return 0;
	}
	keyset->array[0] = 0;

	if (alloc != 0)
	{
		Key * key = (struct _Key *) va_arg (va, struct _Key *);
		while (key)
		{
			ksAppendKey (keyset, key);
			key = (struct _Key *) va_arg (va, struct _Key *);
		}
	}

	ksRewind (keyset); // ksAppendKey changed the internal cursor
	return keyset;
}

/**
 * Return a duplicate of a KeySet.
 *
 * Objects created with ksDup() must be destroyed with ksDel().
 *
 * Memory will be allocated as needed for dynamic properties,
 * so you need to ksDel() the returned pointer.
 *
 * A flat copy is made, so the Keys will not be duplicated,
 * but their reference counter is updated, so both KeySets
 * need to be deleted via ksDel().
 *
 * @param source has to be an initialized KeySet
 *
 * @return a flat copy of source on success
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @see ksNew() for creating a new KeySet
 * @see ksDel() for deleting a KeySet
 * @see keyDup() for Key duplication
 */
KeySet * ksDup (const KeySet * source)
{
	if (!source) return 0;

	size_t size = source->alloc;
	if (size < KEYSET_SIZE)
	{
		size = KEYSET_SIZE;
	}

	KeySet * keyset = ksNew (size, KS_END);
	ksAppend (keyset, source);
	elektraOpmphmCopy (keyset, source);
	return keyset;
}

/**
 * @internal
 * @brief Deeply copies from source to dest.
 *
 * The keyset as well as its containing keys are duplicated.
 * This means that you have to keyDel() the contained keys and
 * ksDel() the returned keyset..
 *
 * the sync status will be as in the original KeySet
 *
 * @param source has to be an initialized source KeySet
 * @return a deep copy of source on success
 * @retval 0 on NULL pointer or a memory error happened
 * @see ksNew(), ksDel()
 * @see keyDup() for key duplication
 * @see ksDup() for flat copy
 */
KeySet * ksDeepDup (const KeySet * source)
{
	if (!source) return 0;

	size_t s = source->size;
	size_t i = 0;
	KeySet * keyset = 0;

	keyset = ksNew (source->alloc, KS_END);
	for (i = 0; i < s; ++i)
	{
		Key * k = source->array[i];
		Key * d = keyDup (k, KEY_CP_ALL);
		if (!test_bit (k->flags, KEY_FLAG_SYNC))
		{
			keyClearSync (d);
		}
		if (ksAppendKey (keyset, d) == -1)
		{
			ksDel (keyset);
			return 0;
		}
	}

	elektraOpmphmCopy (keyset, source);
	return keyset;
}


/**
 * Replace the content of a KeySet with another one.
 *
 * Most often you may want a duplicate of a KeySet, see
 * ksDup() or append keys, see ksAppend().
 * In some situations you need to copy Keys from a
 * KeySet to another KeySet, for which this function
 * exists.
 *
 * @note You can also use it to clear a KeySet when you pass
 * a NULL pointer as @p source.
 *
 * @par Implementation:
 * First all Keys in @p dest will be deleted. Afterwards
 * the content of @p source will be added to the destination
 * and ksCurrent() will be set properly in @p dest.
 *
 * A flat copy is made, so Keys will not be duplicated,
 * but their reference counter is updated, so both KeySets
 * need to be deleted via ksDel().
 *
 * @copydetails doxygenFlatCopy
 *
 * @code
int f (KeySet *ks)
{
	KeySet *c = ksNew (20, ..., KS_END);
	// c receives keys
	ksCopy (ks, c); // pass the KeySet to the caller

	ksDel (c);
}	// caller needs to ksDel (ks)
 * @endcode
 *
 * @param source an initialized KeySet or NULL
 * @param dest an initialized KeySet, where the Keys from @p source get copied to
 *
 * @retval 1 on success
 * @retval 0 if @p dest was cleared successfully (@p source is NULL)
 * @retval -1 when @p dest is a NULL pointer
 *
 * @since 1.0.0
 * @see ksNew() for creating a new KeySet
 * @see ksDel() for deleting an existing KeySet
 * @see ksDup() for duplicating an existing KeySet
 * @see keyCopy() for copying Keys
 */
int ksCopy (KeySet * dest, const KeySet * source)
{
	if (!dest) return -1;
	ksClear (dest);
	if (!source) return 0;

	ksAppend (dest, source);
	ksSetCursor (dest, ksGetCursor (source));

	elektraOpmphmCopy (dest, source);
	return 1;
}


/**
 * A destructor for KeySet objects.
 *
 * Cleans all internal dynamic attributes, decrements all reference pointers
 * to all Keys and then calls keyDel() on all contained Keys.
 * Afterwards elektraFree() is used to release the KeySet's object memory
 * (that was previously allocated by ksNew()).
 *
 * @param ks the KeySet that should be deleted
 *
 * @retval 0 when the KeySet was successfully freed
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @see ksNew() for creating a new KeySet
 */
int ksDel (KeySet * ks)
{
	int rc;

	if (!ks) return -1;

	rc = ksClose (ks);

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (ks->opmphm)
	{
		opmphmDel (ks->opmphm);
	}
	if (ks->opmphmPredictor)
	{
		opmphmPredictorDel (ks->opmphmPredictor);
	}

#endif

	if (!test_bit (ks->flags, KS_FLAG_MMAP_STRUCT))
	{
		elektraFree (ks);
	}

	return rc;
}

/**
 * @internal
 *
 * KeySet object cleaner.
 *
 * Will keyDel() all contained keys, reset internal pointers and counters.
 *
 * After this call you can use the empty keyset again.
 *
 * @param ks the keyset object to work with
 * @see ksAppendKey() for details on how keys are inserted in KeySets
 * @retval 0 on success
 * @retval -1 on failure (memory) or ks == NULL
 */
int ksClear (KeySet * ks)
{
	if (ks == NULL) return -1;
	ksClose (ks);
	// ks->array empty now

	if ((ks->array = elektraMalloc (sizeof (struct _Key *) * KEYSET_SIZE)) == 0)
	{
		ks->size = 0;
		return -1;
	}
	ks->alloc = KEYSET_SIZE;

	elektraOpmphmInvalidate (ks);
	return 0;
}


/**
 * @brief Compare by unescaped name only (not by owner, they are equal)
 *
 * @internal
 *
 * Other non-case Cmp* are based on this one.
 *
 * Is suitable for binary search (but may return wrong owner)
 *
 */
static int keyCompareByName (const void * p1, const void * p2)
{
	Key * k1 = *(Key **) p1;
	Key * k2 = *(Key **) p2;

	int k1Shorter = k1->keyUSize < k2->keyUSize;
	size_t size = k1Shorter ? k1->keyUSize : k2->keyUSize;
	int cmp = memcmp (k1->ukey, k2->ukey, size);
	if (cmp != 0 || k1->keyUSize == k2->keyUSize)
	{
		return cmp;
	}
	return k1Shorter ? -1 : 1;
}

/**
 * Compare the name of two Keys.
 *
 * The comparison is based on a memcmp of the Key's names.
 * If the names match, the Keys are found to be exactly the
 * same and 0 is returned. These two keys can't be used in the same
 * KeySet.
 *
 * keyCmp() defines the sorting order for a KeySet.
 *
 * The following 3 points are the rules for NULL values:
 *
 * - A NULL pointer will be found to be smaller than every other
 * Key. If both are NULL pointers, 0 is returned.
 *
 * - A NULL name will be found to be smaller than every other
 * name. If both are NULL names, 0 is returned.
 *
 * If the name is equal then:
 *
 * - No owner will be found to be smaller than every other owner.
 * If both don't have an owner, 0 is returned.
 *
 * @note the owner will only be used if the names are equal.
 *
 * Given any Keys k1 and k2 constructed with keyNew(), following
 * equation hold true:
 *
 * @snippet testabi_rel.c cmp null
 *
 * Here are some more examples:
 * @code
Key *k1 = keyNew("user:/a", KEY_END);
Key *k2 = keyNew("user:/b", KEY_END);

// keyCmp(k1,k2) < 0
// keyCmp(k2,k1) > 0
 * @endcode
 *
 *
 * Do not strcmp the keyName() yourself, because
 * the result differs from simple ascii comparison.
 *
 * @param k1 the first Key to be compared
 * @param k2 the second Key to be compared
 *
 * @retval <0 if k1 < k2
 * @retval 0 if k1 == k2
 * @retval >0 if k1 > k2
 *
 * @since 1.0.0
 * @ingroup keytest
 * @see ksAppendKey(), ksAppend() will compare Keys via keyCmp() when appending
 * @see ksLookup() will compare Keys via keyCmp() during searching
 */
int keyCmp (const Key * k1, const Key * k2)
{
	if (!k1 && !k2) return 0;
	if (!k1) return -1;
	if (!k2) return 1;

	if (!k1->key && !k2->key) return 0;
	if (!k1->key) return -1;
	if (!k2->key) return 1;

	return keyCompareByName (&k1, &k2);
}

/**
 * Checks if KeySet needs sync.
 *
 * When Keys are changed, this is reflected into keyNeedSync().
 *
 * But when Keys are popped from a KeySet this can't be seen
 * by looking at individual Keys.
 *
 * ksNeedSync() allows the backends to know if a Key was
 * popped from the KeySet, to know that this KeySet needs
 * to be written out.
 *
 * @deprecated Backends now work differently and do not rely on this
 * information.
 *
 * @param ks the KeySet to work with
 *
 * @retval -1 on NULL KeySet
 * @retval 0 if @p ks does not need sync
 * @retval 1 if @p ks needs sync
 *
 * @since 1.0.0
 * @see keyNeedSync() for checking whether a single Key needs to be synced
 */
int ksNeedSync (const KeySet * ks)
{
	if (!ks) return -1;

	return (ks->flags & KS_FLAG_SYNC) == KS_FLAG_SYNC;
}


/**
 * Return the number of Keys that @p ks contains.
 *
 * @param ks the KeySet object to get the size from
 *
 * @return the number of Keys that @p ks contains.
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 */
ssize_t ksGetSize (const KeySet * ks)
{
	if (!ks) return -1;

	return ks->size;
}


/*******************************************
 *           Filling up KeySets            *
 *******************************************/


/**
 * @internal
 *
 * Binary search in a KeySet that informs where a key should be inserted.
 *
 * @code

ssize_t result = ksSearchInternal(ks, toAppend);

if (result >= 0)
{
	ssize_t position = result;
	// Seems like the key already exist.
} else {
	ssize_t insertpos = -result-1;
	// Seems like the key does not exist.
}
 * @endcode
 *
 * @param ks the keyset to work with
 * @param toAppend the key to check
 * @return position where the key is (>=0) if the key was found
 * @return -insertpos -1 (< 0) if the key was not found
 *    so to get the insertpos simple do: -insertpos -1
 */
ssize_t ksSearchInternal (const KeySet * ks, const Key * toAppend)
{
	ssize_t left = 0;
	ssize_t right = ks->size;
	--right;
	register int cmpresult;
	ssize_t middle = -1;
	ssize_t insertpos = 0;

	if (ks->size == 0)
	{
		return -1;
	}

	cmpresult = keyCompareByName (&toAppend, &ks->array[right]);
	if (cmpresult > 0)
	{
		return -((ssize_t) ks->size) - 1;
	}
	cmpresult = 1;

	while (1)
	{
		if (right < left)
		{
			/* Nothing was found */
			break;
		}
		middle = left + ((right - left) / 2);
		cmpresult = keyCompareByName (&toAppend, &ks->array[middle]);
		if (cmpresult > 0)
		{
			insertpos = left = middle + 1;
		}
		else if (cmpresult == 0)
		{
			/* We have found it */
			break;
		}
		else
		{
			insertpos = middle;
			right = middle - 1;
		}
	}

	if (!cmpresult)
	{
		return middle;
	}
	else
	{
		return -insertpos - 1;
	}
}

/**
 * Appends a Key to the end of @p ks.
 *
 * Hands the ownership of the Key @p toAppend to the KeySet @p ks.
 * ksDel(ks) uses keyDel(k) to delete every Key unless it got its reference counter
 * incremented by keyIncRef(), e.g. by another KeySet that contains this Key.
 *
 * The reference counter of the Key will be incremented
 * to indicate this ownership, and thus @p toAppend is not const.
 *
 * @copydetails doxygenFlatCopy
 *
 * @see keyGetRef().
 *
 * If the Key's name already exists in the KeySet, it will be replaced with
 * the new Key.
 *
 * ksAppendKey() will also lock the Key's name from `toAppend`.
 * This is necessary so that the order of the KeySet cannot
 * be destroyed via calls to keySetName().
 *
 * The KeySet internal cursor will be set to the new Key.
 *
 * It is safe to directly append newly created Keys:
 * @snippet keyset.c simple append
 *
 * If you want the key to outlive the KeySet, make sure to
 * do proper ref counting:
 * @snippet keyset.c ref append
 *
 * You can duplicate the Key to avoid aliasing,
 * but then the Key in the KeySet has another identity:
 * @snippet keyset.c dup append
 *
 * @param ks KeySet where @p toAppend should be append
 * @param toAppend Key that will be appended to @p ks or deleted
 *
 * @return the size of the KeySet after appending
 * @retval -1 on NULL pointers
 * @retval -1 if appending failed (only on memory problems). The Key will be
 * deleted then.
 *
 * @since 1.0.0
 * @see ksAppend() for appending a KeySet to another KeySet
 * @see keyIncRef() for manually increasing a Key's reference counter
 */
ssize_t ksAppendKey (KeySet * ks, Key * toAppend)
{
	ssize_t result = -1;

	if (!ks) return -1;
	if (!toAppend) return -1;
	if (!toAppend->key)
	{
		// needed for ksAppendKey(ks, keyNew(0))
		keyDel (toAppend);
		return -1;
	}

	keyLock (toAppend, KEY_LOCK_NAME);

	result = ksSearchInternal (ks, toAppend);

	if (result >= 0)
	{
		/* Seems like the key already exist. */
		if (toAppend == ks->array[result])
		{
			/* user tried to insert the key with same identity */
			return ks->size;
		}

		/* Pop the key in the result */
		keyDecRef (ks->array[result]);
		keyDel (ks->array[result]);

		/* And use the other one instead */
		keyIncRef (toAppend);
		ks->array[result] = toAppend;
		ksSetCursor (ks, result);
	}
	else
	{
		ssize_t insertpos = -result - 1;

		/* We want to append a new key
		  in position insertpos */
		++ks->size;
		if (ks->size >= ks->alloc)
		{

			size_t newSize = ks->alloc == 0 ? KEYSET_SIZE : ks->alloc * 2;
			--newSize;

			if (ksResize (ks, newSize) == -1)
			{
				keyDel (toAppend);
				--ks->size;
				return -1;
			}

			/* If the array was null before ksResize,
			it was newly allocated and the size is 0.
			So we redo the increment from earlier */
			if (ks->size == 0)
			{
				++ks->size;
			}
		}
		keyIncRef (toAppend);

		if (insertpos == (ssize_t) ks->size - 1)
		{
			/* Append it to the very end */
			ks->array[ks->size - 1] = toAppend;
			ks->array[ks->size] = 0;
			ksSetCursor (ks, ks->size - 1);
		}
		else
		{
			size_t n = ks->size - insertpos;
			memmove (ks->array + (insertpos + 1), ks->array + insertpos, n * sizeof (struct Key *));
			/*
			printf ("memmove -- ks->size: %zd insertpos: %zd n: %zd\n",
				ks->size, insertpos, n);
			*/
			ks->array[insertpos] = toAppend;
			ksSetCursor (ks, insertpos);
		}
		elektraOpmphmInvalidate (ks);
	}

	return ks->size;
}


/**
 * Append all Keys in @p toAppend to the end of the KeySet @p ks.
 *
 * @p toAppend KeySet will be left unchanged.
 *
 * If a Key is both in @p toAppend and @p ks, the Key in @p ks will be
 * overwritten.
 *
 * @copydetails doxygenFlatCopy
 *
 * @post Sorted KeySet ks with all Keys it had before and additionally
 *       the Keys from toAppend
 *
 * @param ks the KeySet that will receive the Keys
 * @param toAppend the KeySet that provides the Keys that will be transferred
 *
 * @return the size of the KeySet @p ks after transfer
 * @retval -1 on NULL pointers
 *
 * @since 1.0.0
 * @see ksAppendKey()
 */
ssize_t ksAppend (KeySet * ks, const KeySet * toAppend)
{
	size_t toAlloc = 0;

	if (!ks) return -1;
	if (!toAppend) return -1;

	if (toAppend->size == 0) return ks->size;
	if (toAppend->array == NULL) return ks->size;

	if (ks->array == NULL)
		toAlloc = KEYSET_SIZE;
	else
		toAlloc = ks->alloc;

	/* Do only one resize in advance */
	for (; ks->size + toAppend->size >= toAlloc; toAlloc *= 2)
		;
	ksResize (ks, toAlloc - 1);

	/* TODO: here is lots of room for optimizations */
	for (size_t i = 0; i < toAppend->size; ++i)
	{
		ksAppendKey (ks, toAppend->array[i]);
	}
	return ks->size;
}

/**
 * The core rename loop of ksRename()
 */
static size_t ksRenameInternal (KeySet * ks, size_t start, size_t end, const Key * root, const Key * newRoot)
{
	for (size_t it = start; it < end; ++it)
	{
		if (ks->array[it]->ksReference == 1)
		{
			// only referenced in this KeySet -> just override read-only flag
			clear_bit (ks->array[it]->flags, KEY_FLAG_RO_NAME);
		}
		else
		{
			// key has other references -> dup in-place so we can safely rename it
			Key * dup = keyDup (ks->array[it], KEY_CP_ALL);
			keyDecRef (ks->array[it]);
			dup->ksReference = 1;
			ks->array[it] = dup;
		}
		keyReplacePrefix (ks->array[it], root, newRoot);
		// lock key with new name
		set_bit (ks->array[it]->flags, KEY_FLAG_RO_NAME);
	}
	return end - start;
}

/**
 * Moves all keys below @p root to below @p newRoot
 *
 * Only keys below @p root will be modified. The rest of
 * @p ks remains untouched.
 *
 * This functions is similar to the following snippet, but
 * there are some differences.
 *
 * @code{.c}
 * KeySet * toRename = ksCut (ks, root);
 * for (elektraCursor cursor = 0; cursor < ksGetSize (toRename); cursor++)
 * {
 *     Key * cur = keyDup (ksAtCursor (ks, cursor));
 *     keyReplacePrefix (cur, root, newRoot);
 *     ksAppendKey (ks, cur);
 * }
 * ksDel (toRename);
 * @endcode
 *
 * Firstly, the optimizations only work, if @p ks doesn't
 * contain any keys below @p newRoot that aren't below @p root.
 * If such keys exist, ksRename() will still work, but it will fall
 * back to code similar to the for-loop above.
 *
 * The second difference is that ksRename() will modify the keys in
 * @p ks directly, if they aren't referenced from anywhere else
 * (if their reference count is 1 (see keyGetRef())).
 * Normally, this shouldn't cause problems, but if you have a direct
 * `Key *` pointer to a key in @p ks or hold a reference to some data
 * within a key of @p ks, you may need to call keyIncRef() to ensure
 * the key isn't modified.
 *
 * @param ks      the keyset to manipulate
 * @param root    the old prefix that will be removed, must not be a cascading key
 * @param newRoot the new prefix the will replace the old one, must not be a cascading key
 *
 * @retval -1 if any of @p ks, @p root, @p newRoot is NULL, or if @p root or @p newRoot are cascading keys
 * @retval -2 if @p ks already contains keys below @p newRoot
 * @retval  0 if @p ks contains no keys below @p root (and also not @p root itself)
 * @returns   otherwise, the number of keys that have been renamed
 */
ssize_t ksRename (KeySet * ks, const Key * root, const Key * newRoot)
{
	if (ks == NULL || root == NULL || newRoot == NULL) return -1;
	if (keyGetNamespace (root) == KEY_NS_CASCADING || keyGetNamespace (newRoot) == KEY_NS_CASCADING) return -1;

	// search the root
	elektraCursor end;
	size_t start = ksFindHierarchy (ks, root, &end);

	// we found nothing
	if (start == ks->size) return 0;

	if (keyCmp (root, newRoot) == 0)
	{
		// same root, just return count
		return end - start;
	}

	size_t newStart = ksFindHierarchy (ks, newRoot, NULL);
	if (newStart < ks->size && keyIsBelowOrSame (newRoot, ks->array[newStart]) == 1)
	{
		// has keys below newRoot
		if (start == newStart)
		{
			// first key below newRoot is also first key below root
			// -> we can just rename keys and everything will be fine
			// we don't need to re-sort/invalidate the hashmap,
			// because the renamed subset is already in the right place
			return ksRenameInternal (ks, start, end, root, newRoot);
		}

		// possible name collisions after rename
		// -> ksCut and ksAppend to be safe
		KeySet * toRename = ksCut (ks, root);
		size_t renamed = ksRenameInternal (toRename, 0, ksGetSize (toRename), root, newRoot);
		ksAppend (ks, toRename);
		ksDel (toRename);
		return renamed;
	}

	// rename everything below root
	size_t renamed = ksRenameInternal (ks, start, end, root, newRoot);

	// fix order and invalidate hashmap after renaming
	qsort (ks->array, ks->size, sizeof (struct _Key *), keyCompareByName);
	elektraOpmphmInvalidate (ks);

	return renamed;
}

/**
 * @internal
 *
 * Copies all Keys until the end of the array from a position
 * in the array to an position in the array.
 *
 * @param ks the keyset where this should be done
 * @param to the position where it should be copied to
 * @param from the position where it should be copied from
 * @return the number of moved elements
 */
ssize_t ksCopyInternal (KeySet * ks, size_t to, size_t from)
{
	ssize_t ssize = ks->size;
	ssize_t sto = to;
	ssize_t sfrom = from;

	ssize_t sizediff = sto - sfrom;
	ssize_t length = ssize - sfrom;
	size_t ret = 0;

	ELEKTRA_ASSERT (length >= 0, "length %zu too small", length);
	ELEKTRA_ASSERT (ks->size >= to, "ks->size %zu smaller than %zu", ks->size, to);

	ks->size = ssize + sizediff;

	if (length != 0)
	{
		ret = elektraMemmove (ks->array + to, ks->array + from, length);
	}

	ks->array[ks->size] = 0;

	if (ret) elektraOpmphmInvalidate (ks);

	return ret;
}

/**
 * Searches for the start and optionally end of the key hierachy rooted at @p root in @p ks.
 *
 * The main use-case for this function is this kind of loop:
 *
 * @code{.c}
 * elektraCursor end;
 * for (elektraCursor it = ksFindHierarchy (ks, root, &end); it < end; ++it)
 * {
 * 	Key * cur = ksAtCursor (ks, it);
 * }
 * @endcode
 *
 * @param ks   The keyset to search in
 * @param root The root of the hierachy to find
 * @param end  If this is not NULL, it will be set to position of the first
 *             key after @p root that is not below @p root. This is useful
 *             for loops like the one above.
 *             If not keys below @p root exist in @p ks, @p end will always
 *             be set to the size of @p ks. This way a loop like the one above
 *             will still work correctly.
 *
 * @retval -1 if @p ks or @p root are NULL
 * @return the position of either @p root itself or the first key below
 *         @p root that is part of @p ks. If no keys below @p root exist
 *         in @p ks, the size of @p ks is returned. The snippet above
 *         shows why this is useful.
 */
elektraCursor ksFindHierarchy (const KeySet * ks, const Key * root, elektraCursor * end)
{
	if (ks == NULL || root == NULL) return -1;

	ssize_t search = ksSearchInternal (ks, root);
	size_t it = search < 0 ? -search - 1 : search;
	if (it == ks->size || keyGetNamespace (root) != keyGetNamespace (ks->array[it]) || keyIsBelowOrSame (root, ks->array[it]) != 1)
	{
		if (end != NULL)
		{
			*end = ks->size;
		}
		return ks->size;
	}

	if (end != NULL)
	{
		if (root->keyUSize == 3)
		{

			// special handling for root keys
			// we just increment the namespace byte and search
			// for the next theoretically possible namespace
			root->ukey[0]++;
			ssize_t endSearch = ksSearchInternal (ks, root);
			root->ukey[0]--;
			*end = endSearch < 0 ? -endSearch - 1 : endSearch;
		}
		else
		{
			// Very much a HACK to avoid allocating a new key name
			// Overwriting the null terminator works fine, because
			// all accesses to root->ukey inside of ksSearchInternal()
			// use root->keyUSize explicitly.
			root->ukey[root->keyUSize - 1] = '\1';
			ssize_t endSearch = ksSearchInternal (ks, root);
			root->ukey[root->keyUSize - 1] = '\0';
			*end = endSearch < 0 ? -endSearch - 1 : endSearch;
		}
	}

	return it;
}

/**
 * Searches for the start and end indicies corresponding to the given cutpoint.
 *
 * @see ksCut() for explanation of cutpoints
 *
 * @param ks       the keyset to cut
 * @param cutpoint the cutpoint
 * @param from     we will store the start index here
 * @param to       we will store the end index here
 *
 * @retval -1 if the cutpoint wasn't found
 * @retval  1 if the cursor has to updated to match ks->current
 * @retval  0 otherwise
 */
static int elektraKsFindCutpoint (KeySet * ks, const Key * cutpoint, size_t * from, size_t * to)
{
	int set_cursor = 0;

	// search the cutpoint
	ssize_t search = ksSearchInternal (ks, cutpoint);
	size_t it = search < 0 ? -search - 1 : search;

	// we found nothing
	if (it == ks->size) return -1;

	// we found the cutpoint
	size_t found = it;

	// search the end of the keyset to cut
	while (it < ks->size && keyIsBelowOrSame (cutpoint, ks->array[it]) == 1)
	{
		++it;
	}

	// correct cursor if cursor is in cut keyset
	if (ks->current >= found && ks->current < it)
	{
		if (found == 0)
		{
			ksRewind (ks);
		}
		else
		{
			ks->current = found - 1;
			set_cursor = 1;
		}
	}

	// correct the cursor for the keys after the cut keyset
	if (ks->current >= it)
	{
		if (it >= ks->size)
		{
			ksRewind (ks);
		}
		else
		{
			ks->current = found + ks->current - it;
			set_cursor = 1;
		}
	}

	*from = it;
	*to = found;

	return set_cursor;
}

/**
 * Cuts out all Keys from KeySet @p ks that are below or at @p cutpoint
 *
 * Searches for the @p cutpoint inside the KeySet @p ks.
 * If found, it cuts out this Key and everything which is below
 * (see keyIsBelow()) this Key. These Keys will be missing in the keyset @p ks.
 * Instead, they will be moved to the returned KeySet.
 * If @p cutpoint is not found an empty KeySet is returned and @p ks
 * is not changed.
 *
 * The cursor will stay at the same Key as it was before.
 * If the cursor was inside the region of cut (moved)
 * Keys, the cursor will be set to the Key before
 * the @p cutpoint.
 *
 * If you use ksCut() on a KeySet you got from kdbGet() and plan to use
 * kdbSet() later, make sure that you keep all Keys that should not
 * be removed permanently. You have to keep the KeySet that was returned
 * and the KeySet @p ks.
 *
 * @par Example:
 *
 * You have the keyset @p ks:
 * - @p system:/mountpoint/interest
 * - @p system:/mountpoint/interest/folder
 * - @p system:/mountpoint/interest/folder/key1
 * - @p system:/mountpoint/interest/folder/key2
 * - @p system:/mountpoint/other/key1
 *
 * When you use
 * @snippet ksCut.c cut
 *
 * Then in @p returned are:
 * - @p system:/mountpoint/interest
 * - @p system:/mountpoint/interest/folder
 * - @p system:/mountpoint/interest/folder/key1
 * - @p system:/mountpoint/interest/folder/key2
 *
 * And in @p ks are:
 * - @p system:/mountpoint/other/key1
 *
 * So kdbSet() permanently removes all keys at or below
 * @p system:/mountpoint/interest.
 *
 * @param ks the Keyset to cut. It will be modified by removing
 *           all Keys at or below the cutpoint.
 * @param cutpoint the point where to cut out the Keyset
 *
 * @return a new allocated KeySet which needs to deleted with ksDel().
 *         The KeySet consists of all Keys (of the original KeySet ks)
 *         below the cutpoint. If the Key cutpoint exists, it will
 *         also be appended.
 * @retval 0 on NULL pointers, no Key name or allocation problems
 *
 * @since 1.0.0
 * @see kdbGet() for an explanation on why you might get more Keys than you
 * requested.
 */
KeySet * ksCut (KeySet * ks, const Key * cutpoint)
{
	KeySet * returned = 0;
	KeySet * ret = 0; // for cascading version
	size_t found = 0;
	size_t it = 0;
	size_t newsize = 0;
	int set_cursor = 0;

	if (!ks) return 0;
	if (!cutpoint) return 0;

	if (!ks->array) return ksNew (0, KS_END);

	char * name = cutpoint->key;
	if (!name) return 0;
	if (strcmp (name, "") == 0) return 0;

	elektraOpmphmInvalidate (ks);

	if (cutpoint->ukey[0] == KEY_NS_CASCADING)
	{
		ret = ksNew (0, KS_END);

		// HACK: ksCut does not use escaped name (key->key), so we don't need to change it
		for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
		{
			int validNS = 1;
			switch (ns)
			{
			case KEY_NS_SPEC:
			case KEY_NS_PROC:
			case KEY_NS_DIR:
			case KEY_NS_USER:
			case KEY_NS_SYSTEM:
			case KEY_NS_META:
				((Key *) cutpoint)->ukey[0] = ns;
				break;
			case KEY_NS_NONE:
			case KEY_NS_CASCADING:
			case KEY_NS_DEFAULT:
				validNS = 0;
			}
			if (validNS)
			{
				KeySet * n = ksCut (ks, cutpoint);
				ksAppend (ret, n);
				ksDel (n);
			}
		}

		// restore old cascading name
		((Key *) cutpoint)->ukey[0] = KEY_NS_CASCADING;

		// now look for cascading keys
		// TODO: cascading keys shouldn't be allowed in KeySet anymore
	}

	set_cursor = elektraKsFindCutpoint (ks, cutpoint, &it, &found);
	if (set_cursor < 0) return ret ? ret : ksNew (0, KS_END);

	newsize = it - found;

	returned = ksNew (newsize, KS_END);
	elektraMemcpy (returned->array, ks->array + found, newsize);
	returned->size = newsize;
	if (returned->size > 0) returned->array[returned->size] = 0;

	ksCopyInternal (ks, found, it);

	if (set_cursor) ks->cursor = ks->array[ks->current];

	if (ret)
	{
		ksAppend (returned, ret);
		ksDel (ret);
	}

	return returned;
}


/**
 * Remove and return the last Key of @p ks.
 *
 * The reference counter of the Key will be decremented by one.
 *
 * The KeySet's cursor will not be affected if it did not
 * point to the popped Key.
 *
 * @note You need to keyDel() the Key afterwards, if
 * you don't append it to another KeySet. It has the
 * same semantics like a Key allocated with keyNew()
 * or keyDup().
 *
 *@code
ks1=ksNew(0, KS_END);
ks2=ksNew(0, KS_END);

k1=keyNew("user:/name", KEY_END); // ref counter 0
ksAppendKey(ks1, k1); // ref counter 1
ksAppendKey(ks2, k1); // ref counter 2

k1=ksPop (ks1); // ref counter 1
k1=ksPop (ks2); // ref counter 0, like after keyNew()

ksAppendKey(ks1, k1); // ref counter 1

ksDel (ks1); // key is deleted too
ksDel (ks2);
 *@endcode
 *
 * @param ks KeySet to pop a Key from
 *
 * @return the last Key of @p ks
 * @retval NULL if @p ks is empty or a NULL pointer
 *
 * @since 1.0.0
 * @see ksLookup() to pop Keys by name
 * @see ksCopy() to pop all Keys
 * @see ksTail() for getting the last Key of a KeySet without removing it
 */
Key * ksPop (KeySet * ks)
{
	Key * ret = 0;

	if (!ks) return 0;

	ks->flags |= KS_FLAG_SYNC;

	if (ks->size == 0) return 0;

	elektraOpmphmInvalidate (ks);

	--ks->size;
	if (ks->size + 1 < ks->alloc / 2) ksResize (ks, ks->alloc / 2 - 1);
	ret = ks->array[ks->size];
	ks->array[ks->size] = 0;
	keyDecRef (ret);

	return ret;
}


/*******************************************
 *           KeySet browsing methods       *
 *******************************************/


/**
 * Rewinds the KeySet internal cursor.
 *
 * Use it to set the cursor to the beginning of the KeySet.
 * ksCurrent() will always return NULL afterwards. So
 * you want to use ksNext() first.
 *
 * @code
ksRewind (ks);
while ((key = ksNext (ks))!=0) {}
 * @endcode
 *
 * @param ks the KeySet that should be rewound
 *
 * @retval 0 on success
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @see ksNext() for moving the cursor to the next entry in the KeySet
 * @see ksCurrent() for getting the current element in the KeySet
 */
int ksRewind (KeySet * ks)
{
	if (!ks) return -1;

	ks->cursor = 0;
	ks->current = 0;
	return 0;
}


/**
 * Returns the next Key in a KeySet.
 *
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksNext() is called, the cursor is incremented and the new current Key
 * is returned.
 *
 * You'll get a NULL pointer if the Key at the end of the KeySet has been reached.
 * On subsequent calls of ksNext() it will still return the NULL pointer.
 *
 * The @p ks internal cursor will be changed, so it is not const.
 *
 * @note You must not delete or change the Key, use ksPop() if you want to delete it.
 *
 * @note That applications must do ksLookup() with an cascading Key for every single
 * Key before using it, because specifications allow to hide or override Keys.
 *
 * @param ks the KeySet object to work with
 *
 * @return the new current Key
 * @retval 0 when the end of the KeySet has been reached
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @see ksRewind() for resetting the internal cursor of the KeySet
 * @see ksCurrent() for getting the Key the cursor currently points at
 * @see ksLookup() to honor specifications
 */
Key * ksNext (KeySet * ks)
{
	if (!ks) return 0;

	if (ks->size == 0) return 0;
	if (ks->current >= ks->size)
	{
		return 0;
	}

	if (ks->cursor) ks->current++;
	return ks->cursor = ks->array[ks->current];
}


/**
 * Return the current Key.
 *
 * The returned pointer is NULL if you reached the end or after
 * ksRewind().
 *
 * @note You must not delete the Key or change the Key,
 *    use ksPop() if you want to delete it.
 *
 * @param ks the KeySet object to get the current Key from
 *
 * @return pointer to the Key pointed by @p ks's cursor
 * @retval 0 on NULL pointer
 *
 * @since 1.0.0
 * @see ksNext() to get the next Key in the KeySet
 * @see ksRewind() for resetting the internal cursor of the KeySet
 */
Key * ksCurrent (const KeySet * ks)
{
	if (!ks) return 0;

	return ks->cursor;
}


/**
 * Return the first Key in the KeySet.
 *
 * The KeySet's cursor will not be affected.
 *
 * If ksCurrent()==ksHead() you know you are
 * on the first Key.
 *
 * @param ks the KeySet object to get the first Key from
 *
 * @return the first Key of a KeySet
 * @retval 0 on NULL pointer or empty KeySet
 *
 * @since 1.0.0
 * @see ksTail() for getting the last Key of the KeySet
 * @see ksRewind(), ksCurrent() and ksNext() for iterating over the KeySet
 */
Key * ksHead (const KeySet * ks)
{
	if (!ks) return 0;

	if (ks->size > 0)
		return ks->array[0];
	else
		return 0;
}


/**
 * Return the last Key in the KeySet.
 *
 * The KeySet's cursor will not be affected.
 *
 * If ksCurrent()==ksTail() you know you
 * are on the last key. ksNext() will return
 * a NULL pointer afterwards.
 *
 * @param ks the KeySet object to get the last Key from
 *
 * @return the last Key of a KeySet
 * @retval 0 on NULL pointer or empty KeySet
 *
 * @since 1.0.0
 * @see ksHead() for getting the first Key of a KeySet
 * @see ksRewind(), ksCurrent() and ksNext() for iterating over the KeySet
 */
Key * ksTail (const KeySet * ks)
{
	if (!ks) return 0;

	if (ks->size > 0)
		return ks->array[ks->size - 1];
	else
		return 0;
}


/**
 * Get the internal cursor of the KeySet.
 *
 * @warning Cursors are getting invalid when the Key
 * was ksPop()ed or ksLookup() with KDB_O_POP was
 * used.
 *
 * @section readahead Read ahead
 *
 * With the cursors it is possible to read ahead
 * in a KeySet:
 *
 * @code
elektraCursor jump;
ksRewind (ks);
while ((key = keyNextMeta (ks))!=0)
{
	// now mark this key
	jump = ksGetCursor(ks);

	//code..
	keyNextMeta (ks); // now browse on
	// use ksCurrent(ks) to check the keys
	//code..

	// jump back to the position marked before
	ksSetCursor(ks, jump);
}
 * @endcode
 *
 * @section restore Restoring state
 *
 * It can also be used to restore the state of a
 * KeySet in a function
 *
 * @code
int f (KeySet *ks)
{
	elektraCursor state = ksGetCursor(ks);

	// work with keyset

	// now bring the keyset to the state before
	ksSetCursor (ks, state);
}
 * @endcode
 *
 * It is of course possible to make the KeySet const
 * and cast its const away to set the cursor.
 * Another way to achieve
 * the same is to ksDup() the KeySet, but it is
 * not as efficient.
 *
 * An invalid cursor will be returned directly after
 * ksRewind(). When you set an invalid cursor ksCurrent()
 * is 0 and ksNext() == ksHead().
 *
 * @section cursor_directly Using Cursor directly
 *
 * You can also use the cursor directly
 * by initializing it to some index in the KeySet
 * and then incrementing or decrementing it, to
 * iterate over the KeySet.
 *
 * @snippet ksIterate.c iterate for
 *
 * You can also use a while loop if you need access to the last cursor position.
 *
 * @snippet ksIterate.c iterate while
 *
 * @note Only use a cursor for the same KeySet which it was
 * made for.
 *
 * @param ks the KeySet object to get the cursor from
 *
 * @return a valid cursor on success
 * @retval -1 on NULL pointer
 * @retval -1 on an invalid internal cursor or after ksRewind
 *
 * @since 1.0.0
 * @see ksNext() for moving the internal cursor forward
 * @see ksSetCursor() for setting the cursor to a specific position
 * @see ksAtCursor() for getting the Key at a specific position
 */
elektraCursor ksGetCursor (const KeySet * ks)
{
	if (!ks) return (elektraCursor) -1;

	if (ks->cursor == 0)
		return (elektraCursor) -1;
	else
		return (elektraCursor) ks->current;
}

/**
 * Return Key at given position @p pos
 *
 * The position is a number starting from 0.
 *
 * @param ks the KeySet to get the Key from
 * @param pos the position of the Key that should be retrieved
 *
 * @return the Key at the cursor @p pos on success
 * @retval NULL on NULL pointer, negative cursor position
 * or a position that does not lie within the KeySet @p ks
 *
 * @since 1.0.0
 * @see ksGetCursor() for getting the cursor at the current position
 * @see ksSetCursor() for setting the cursor to a specific position
 */
Key * ksAtCursor (KeySet * ks, elektraCursor pos)
{
	if (!ks) return 0;
	if (pos < 0) return 0;
	if (ks->size <= (size_t) pos) return 0;
	return ks->array[pos];
}


/**
 * Set the KeySet internal cursor to @p cursor.
 *
 * Use it to set the cursor to a stored position.
 * ksCurrent() will then return the Key at the position of the supplied cursor.
 *
 * @warning Cursors may get invalid when the Key
 * was ksPop()ed or ksLookup() was used together
 * with KDB_O_POP.
 *
 * @code
elektraCursor cursor;
..
// key now in any position here
cursor = ksGetCursor (ks);
while ((key = keyNextMeta (ks))!=0) {}
ksSetCursor (ks, cursor); // reset state
ksCurrent(ks); // in same position as before
 * @endcode
 *
 * An invalid cursor will set the KeySet to its beginning like
 * ksRewind(). When you set an invalid cursor ksCurrent()
 * is 0 and ksNext() == ksHead().
 *
 * @param ks the KeySet object where the cursor should be set
 * @param cursor the cursor to set for @p ks
 *
 * @retval 0 when the KeySet has been ksRewind()ed
 * @retval 1 otherwise
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @see ksGetCursor() for getting the cursor at the current position
 * @see ksNext() for moving the internal cursor forward
 * @see ksCurrent() for getting the Key at the current position
 */
int ksSetCursor (KeySet * ks, elektraCursor cursor)
{
	if (!ks) return -1;

	if ((elektraCursor) -1 == cursor)
	{
		ksRewind (ks);
		return 0;
	}
	ks->current = (size_t) cursor;
	ks->cursor = ks->array[ks->current];
	return 1;
}


/*******************************************
 *    Looking up Keys inside KeySets       *
 *******************************************/

static void elektraCopyCallbackMeta (Key * dest, Key * source)
{
	// possible optimization: only copy when callback is present (keyIsBinary && keyGetValueSize == sizeof(void(int))
	const Key * m = 0;

	keyRewindMeta (dest);
	while ((m = keyNextMeta (dest)))
	{
		const char * metaname = keyName (m);
		if (!strncmp (metaname, "callback/", sizeof ("callback")))
		{
			keySetMeta (dest, metaname, 0);
		}
	}

	keyRewindMeta (source);
	while ((m = keyNextMeta (source)))
	{
		const char * metaname = keyName (m);
		if (!strncmp (metaname, "callback/", sizeof ("callback")))
		{
			keyCopyMeta (dest, source, metaname);
		}
	}
}

/**
 * @internal
 * @brief Writes a elektra array name
 *
 * @param newName the buffer to write to (size must be
 *       #ELEKTRA_MAX_ARRAY_SIZE or more)
 * @param newIndex the index of the array to write
 *
 * @retval 0 on success
 * @retval -1 on error
 */
int elektraWriteArrayNumber (char * newName, kdb_long_long_t newIndex)
{
	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	kdb_long_long_t i = newIndex / 10;

	while (i > 0)
	{
		newName[index++] = '_'; // index n-1 of decimals
		i /= 10;
	}
	if (snprintf (&newName[index], ELEKTRA_MAX_ARRAY_SIZE - index, ELEKTRA_UNSIGNED_LONG_LONG_F, newIndex) < 0)
	{
		return -1;
	}

	return 0;
}

/**
 * @internal
 * @brief Helper for elektraLookupBySpec
 *
 * Lookup using links (fallback or override passed as buffer)
 *
 * @param ks the keyset to search in
 * @param specKey contains metadata as specified in buffer+#<number>
 * @param buffer the buffer used for incrementing numbers
 *
 * @return
 */
static Key * elektraLookupBySpecLinks (KeySet * ks, Key * specKey, char * buffer)
{
	Key * ret = 0;
	Key * k = 0;
	const int prefixSize = ELEKTRA_MAX_PREFIX_SIZE - 2;
	kdb_long_long_t i = 0;
	const Key * m = 0;

	do
	{
		elektraWriteArrayNumber (&buffer[prefixSize], i);
		m = keyGetMeta (specKey, buffer);
		if (!m || keyGetValueSize (m) == 1) break;
		// optimization: lazy instanziation of k
		if (!k)
		{
			k = keyNew (keyString (m), KEY_END);
			keySetBinary (k, keyValue (specKey), keyGetValueSize (specKey));
			elektraCopyCallbackMeta (k, specKey);
		}
		else
			keySetName (k, keyString (m));
		ret = ksLookup (ks, k, KDB_O_NODEFAULT);
		if (ret) break;
		++i;
	} while (m);

	if (k)
	{
		elektraCopyCallbackMeta (specKey, k);
		keyDel (k);
	}
	return ret;
}

/**
 * @internal
 *
 * @param specKey must have a cascading key name
 *
 * @brief Helper for elektraLookupBySpec
 */
static Key * elektraLookupBySpecDefault (KeySet * ks, Key * specKey)
{
	Key * ret = 0;
	const Key * m = 0;

	keySetNamespace (specKey, KEY_NS_DEFAULT);
	ret = ksLookup (ks, specKey, 0);

	if (ret) return ret; // return previous added default key

	m = keyGetMeta (specKey, "default");
	if (!m) return ret;
	ret = keyNew (keyName (specKey), KEY_VALUE, keyString (m), KEY_END);
	ksAppendKey (ks, ret);

	keySetNamespace (specKey, KEY_NS_CASCADING);

	return ret;
}

static Key * elektraLookupByCascading (KeySet * ks, Key * key, elektraLookupFlags options);

/**
 * @internal
 * @brief Helper for elektraLookupBySpec
 *
 * Lookup using namespaces
 *
 * @param ks the keyset to search in
 * @param specKey contains metadata as specified in buffer+#<number>
 * @param buffer the buffer used for incrementing numbers
 */
static Key * elektraLookupBySpecNamespaces (KeySet * ks, Key * specKey, char * buffer)
{
	Key * ret = 0;
	const int prefixSize = ELEKTRA_MAX_PREFIX_SIZE - 1;
	kdb_long_long_t i = 0;
	const Key * m = 0;

	m = keyGetMeta (specKey, buffer);
	// no namespaces specified, so do a default cascading lookup
	// (obviously w/o spec)
	if (!m) return elektraLookupByCascading (ks, specKey, KDB_O_NOSPEC | KDB_O_NODEFAULT);

	do
	{
		// lookup with given namespace
		elektraNamespace ns = elektraReadNamespace (keyString (m), keyGetValueSize (m) - 1);
		if (ns == KEY_NS_NONE) break;
		keySetNamespace (specKey, ns);
		ret = ksLookup (ks, specKey, 0);
		if (ret) break;
		++i; // start with 1 (#0 was already in buffer)

		elektraWriteArrayNumber (&buffer[prefixSize], i);
		m = keyGetMeta (specKey, buffer);
	} while (m);

	// restore old cascading name
	keySetNamespace (specKey, KEY_NS_CASCADING);
	return ret;
}


/**
 * @internal
 * @brief Helper for ksLookup
 */
static Key * elektraLookupBySpec (KeySet * ks, Key * specKey, elektraLookupFlags options)
{
	Key * ret = 0;
	elektraNamespace oldNS = keyGetNamespace (specKey);
	// strip away beginning of specKey
	keySetNamespace (specKey, KEY_NS_CASCADING);

	// lookup by override
	char buffer[ELEKTRA_MAX_PREFIX_SIZE + ELEKTRA_MAX_ARRAY_SIZE] = "override/";
	ret = elektraLookupBySpecLinks (ks, specKey, buffer);
	if (ret) goto finished;

	// lookup by namespaces
	strcpy (buffer, "namespace/#0");
	ret = elektraLookupBySpecNamespaces (ks, specKey, buffer);
	if (ret) goto finished;

	// lookup by fallback
	strcpy (buffer, "fallback/");
	ret = elektraLookupBySpecLinks (ks, specKey, buffer);
	if (ret) goto finished;

	if (!(options & KDB_O_NODEFAULT))
	{
		ret = elektraLookupBySpecDefault (ks, specKey);
	}

finished:
	keySetNamespace (specKey, oldNS);

	return ret;
}

/**
 * @internal
 * @brief Helper for ksLookup
 */
static Key * elektraLookupByCascading (KeySet * ks, Key * key, elektraLookupFlags options)
{
	elektraNamespace oldNS = keyGetNamespace (key);
	Key * found = 0;
	Key * specKey = 0;

	if (!(options & KDB_O_NOSPEC))
	{
		keySetNamespace (key, KEY_NS_SPEC);
		specKey = ksLookup (ks, key, (options & ~KDB_O_DEL) | KDB_O_CALLBACK);
	}

	if (specKey)
	{
		// restore old name
		keySetNamespace (key, oldNS);

		if (strncmp (keyName (specKey), "spec:/", 5))
		{ // the search was modified in a way that not a spec Key was returned
			return specKey;
		}

		// we found a spec key, so we know what to do
		specKey = keyDup (specKey, KEY_CP_ALL);
		keySetBinary (specKey, keyValue (key), keyGetValueSize (key));
		elektraCopyCallbackMeta (specKey, key);
		found = elektraLookupBySpec (ks, specKey, options);
		elektraCopyCallbackMeta (key, specKey);
		keyDel (specKey);
		return found;
	}

	// default cascading:
	keySetNamespace (key, KEY_NS_PROC);
	found = ksLookup (ks, key, options & ~KDB_O_DEL);

	if (!found)
	{
		keySetNamespace (key, KEY_NS_DIR);
		found = ksLookup (ks, key, options & ~KDB_O_DEL);
	}

	if (!found)
	{
		keySetNamespace (key, KEY_NS_USER);
		found = ksLookup (ks, key, options & ~KDB_O_DEL);
	}

	if (!found)
	{
		keySetNamespace (key, KEY_NS_SYSTEM);
		found = ksLookup (ks, key, options & ~KDB_O_DEL);
	}

	if (!found)
	{
		keySetNamespace (key, KEY_NS_DEFAULT);
		found = ksLookup (ks, key, options & ~KDB_O_DEL);
	}

	// restore old cascading name
	keySetNamespace (key, KEY_NS_CASCADING);

	if (!found && !(options & KDB_O_NODEFAULT))
	{
		// search / key itself
		found = ksLookup (ks, key, (options & ~KDB_O_DEL) | KDB_O_NOCASCADING);
	}

	return found;
}

static Key * elektraLookupBinarySearch (KeySet * ks, Key const * key, elektraLookupFlags options)
{
	elektraCursor cursor = 0;
	cursor = ksGetCursor (ks);
	Key ** found;
	size_t jump = 0;
	/*If there is a known offset in the beginning jump could be set*/
	found = (Key **) bsearch (&key, ks->array + jump, ks->size - jump, sizeof (Key *), keyCompareByName);

	if (found)
	{
		cursor = found - ks->array;
		if (options & KDB_O_POP)
		{
			return elektraKsPopAtCursor (ks, cursor);
		}
		else
		{
			ksSetCursor (ks, cursor);
			return (*found);
		}
	}
	else
	{
		ksSetCursor (ks, cursor);
	}
	return 0;
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS

/**
 * @internal
 *
 * @brief Extracts the Key name of Keys
 *
 * @param data the Key
 *
 * @return the Key name
 */
static const char * elektraOpmphmGetString (void * data)
{
	return keyName ((Key *) data);
}

/**
 * @internal
 *
 * @brief Builds the OPMPHM
 *
 * Creates the OPMPHM when not here.
 * The passed KeySet must have a not build OPMPHM.
 *
 * @param ks the KeySet which OPMPHM is to build
 *
 * @retval 0 on success
 * @retval -1 on memory error or to many mapping invocations
 */
static int elektraLookupBuildOpmphm (KeySet * ks)
{
	if (ks->size > KDB_OPMPHM_MAX_N)
	{
		return -1;
	}
	if (!ks->opmphm)
	{
		ks->opmphm = opmphmNew ();
		if (!ks->opmphm)
		{
			return -1;
		}
	}
	ELEKTRA_ASSERT (!opmphmIsBuild (ks->opmphm), "OPMPHM already build");
	// make graph
	uint8_t r = opmphmOptR (ks->size);
	double c = opmphmMinC (r);
	c += opmphmOptC (ks->size);
	OpmphmGraph * graph = opmphmGraphNew (ks->opmphm, r, ks->size, c);
	if (!graph)
	{
		return -1;
	}
	// make init
	OpmphmInit init;
	init.getName = elektraOpmphmGetString;
	init.data = (void **) ks->array;
	init.initSeed = elektraRandGetInitSeed ();

	// mapping
	size_t mappings = 0; // counts mapping invocations
	int ret;
	do
	{
		ret = opmphmMapping (ks->opmphm, graph, &init, ks->size);
		++mappings;
	} while (ret && mappings < 10);
	if (ret && mappings == 10)
	{
		opmphmGraphDel (graph);
		return -1;
	}

	// assign
	if (opmphmAssignment (ks->opmphm, graph, ks->size, 1))
	{
		opmphmGraphDel (graph);
		return -1;
	}

	opmphmGraphDel (graph);
	return 0;
}

/**
 * @internal
 *
 * @brief Searches for a Key in an already build OPMPHM.
 *
 * The OPMPHM must be build.
 *
 * @param ks the KeySet
 * @param key the Key to search for
 * @param options lookup options
 *
 * @return Key * when key found
 * @return NULL when key not found
 *
 */
static Key * elektraLookupOpmphmSearch (KeySet * ks, Key const * key, elektraLookupFlags options)
{
	ELEKTRA_ASSERT (opmphmIsBuild (ks->opmphm), "OPMPHM not build");
	elektraCursor cursor = 0;
	cursor = ksGetCursor (ks);
	size_t index = opmphmLookup (ks->opmphm, ks->size, keyName (key));
	if (index >= ks->size)
	{
		return 0;
	}

	Key * found = ks->array[index];

	if (!strcmp (keyName (found), keyName (key)))
	{
		cursor = index;
		if (options & KDB_O_POP)
		{
			return elektraKsPopAtCursor (ks, cursor);
		}
		else
		{
			ksSetCursor (ks, cursor);
			return found;
		}
	}
	else
	{
		ksSetCursor (ks, cursor);
		return 0;
	}
}

#endif

/**
 * @brief Process Callback + maps to correct binary/hashmap search
 *
 * @return the found key
 */
static Key * elektraLookupSearch (KeySet * ks, Key * key, elektraLookupFlags options)
{
	if (!ks->size) return 0;
	typedef Key * (*callback_t) (KeySet * ks, Key * key, Key * found, elektraLookupFlags options);
	union
	{
		callback_t f;
		void * v;
	} conversation;

	Key * found = 0;

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (!ks->opmphmPredictor && ks->size > opmphmPredictorActionLimit)
	{
		// lazy loading of predictor when over action limit
		ks->opmphmPredictor = opmphmPredictorNew ();
	}

	// predictor
	if (!test_bit (options, (KDB_O_BINSEARCH | KDB_O_OPMPHM)))
	{
		// predictor not overruled
		if (ks->opmphmPredictor)
		{
			if (test_bit (ks->flags, KS_FLAG_NAME_CHANGE))
			{
				// KeySet changed ask predictor
				if (opmphmPredictor (ks->opmphmPredictor, ks->size))
				{
					set_bit (options, KDB_O_OPMPHM);
				}
				else
				{
					set_bit (options, KDB_O_BINSEARCH);
				}
				// resolve flag
				clear_bit (ks->flags, (keyflag_t) KS_FLAG_NAME_CHANGE);
			}
			else
			{
				if (opmphmIsBuild (ks->opmphm))
				{
					opmphmPredictorIncCountOpmphm (ks->opmphmPredictor);
					set_bit (options, KDB_O_OPMPHM);
				}
				else if (opmphmPredictorIncCountBinarySearch (ks->opmphmPredictor, ks->size))
				{
					// endless binary search protection
					set_bit (options, KDB_O_OPMPHM);
				}
				else
				{
					set_bit (options, KDB_O_BINSEARCH);
				}
			}
		}
		else
		{
			// when predictor is not here use binary search as backup
			set_bit (options, KDB_O_BINSEARCH);
		}
	}

	// the actual lookup
	if ((options & (KDB_O_BINSEARCH | KDB_O_OPMPHM)) == KDB_O_OPMPHM)
	{
		if (opmphmIsBuild (ks->opmphm) || !elektraLookupBuildOpmphm (ks))
		{
			found = elektraLookupOpmphmSearch (ks, key, options);
		}
		else
		{
			// when OPMPHM build fails use binary search as backup
			found = elektraLookupBinarySearch (ks, key, options);
		}
	}
	else if ((options & (KDB_O_BINSEARCH | KDB_O_OPMPHM)) == KDB_O_BINSEARCH)
	{
		found = elektraLookupBinarySearch (ks, key, options);
	}
	else
	{
		// both flags set, make the best out of it
		if (opmphmIsBuild (ks->opmphm))
		{
			found = elektraLookupOpmphmSearch (ks, key, options);
		}
		else
		{
			found = elektraLookupBinarySearch (ks, key, options);
		}
	}

	// remove flags to not interfere with callback
	clear_bit (options, (KDB_O_OPMPHM | KDB_O_BINSEARCH));
#else
	found = elektraLookupBinarySearch (ks, key, options);
#endif
	Key * ret = found;

	if (keyGetMeta (key, "callback"))
	{
		if (keyGetBinary (key, &conversation.v, sizeof (conversation)) == sizeof (conversation))
		{
			if (conversation.v != 0)
			{
				ret = (*conversation.f) (ks, key, found, options);
			}
		}
	}

	return ret;
}

static Key * elektraLookupCreateKey (KeySet * ks, Key * key, ELEKTRA_UNUSED elektraLookupFlags options)
{
	Key * ret = keyDup (key, KEY_CP_ALL);
	ksAppendKey (ks, ret);
	return ret;
}


/**
 * Look for a Key contained in @p ks that matches the name of the @p key.
 *
 * @note Applications should only use ksLookup() with cascading
 * Keys (Key name starting with `/`).
 * Furthermore, a lookup should be done for every Key (also when iterating
 * over Keys) so that the specifications are honored correctly.
 * Keys of all namespaces need to be present so that ksLookup()
 * can work correctly, so make sure to also use kdbGet() with a cascading
 * Key.
 *
 * ksLookup() is designed to let you work with a
 * KeySet containing all Keys of the application. The
 * idea is to fully kdbGet() the whole configuration of your application and
 * process it all at once with many @p ksLookup().
 *
 * This function is efficient (at least using binary search). Together with
 * kdbGet(), which you can use to load the whole configuration,
 * you can write very effective and short code for configuration:
 *
 * @snippet kdbget.c basic usage
 *
 * This is the way programs should get their configuration and
 * search for the values. It is guaranteed, that more namespaces can be
 * added easily and that all values can be set by admin and user.
 * Furthermore, using the kdb-tool, it is possible to introspect which values
 * an application will get (by doing the same cascading lookup).
 *
 * If found, @p ks internal cursor will be positioned in the matched Key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor will not move, and a NULL pointer is
 * returned.
 *
 * Cascading lookups will by default search in
 * all namespaces (proc:/, dir:/, user:/ and system:/), but will also correctly consider
 * the specification (=metadata) in spec:/:
 *
 * - @p override/# will make sure that another Key is considered before
 * - @p namespace/# will change the number and/or order in which the
 *   namespaces are searched
 * - @p fallback/# will search for other Keys when the other possibilities
 *   up to now were not successful
 * - @p default to return the given value when not even @p fallback Keys were
 *   found.
 *
 *
 * @note override and fallback work recursively, while default does not.
 *
 * This process is very flexible, but it would be boring to manually follow all this links
 * to find out which Key will be taken in the end. Use `kdb get -v` to trace the Keys.
 *
 *
 * @par KDB_O_POP
 * When ::KDB_O_POP is set the Key which was found will be ksPop()ed. ksCurrent()
 * will not be changed, only iff ksCurrent() is the searched Key, then the KeySet
 * will be ksRewind()ed.
 *
 * @note Like in ksPop() the popped Key always needs to be keyDel() afterwards, even
 * if it is appended to another KeySet.
 *
 * @warning All cursors on the KeySet will be invalid
 * iff you use ::KDB_O_POP, so don't use this if you rely on a cursor, see ksGetCursor().
 *
 * The invalidation of cursors does not matter if you use multiple KeySets, e.g.
 * by using ksDup(). E.g., to separate ksLookup() with ::KDB_O_POP and ksAppendKey():

 * @snippet ksLookupPop.c f
 *
 * This is also a nice example how a complete application with ksLookup() can look like.
 *
 * @par KDB_O_DEL
 * Passing ::KDB_O_DEL will cause the deletion of the parameter @p key using keyDel().
 *
 *
 * @par Hybrid search
 * When Elektra is compiled with `ENABLE_OPTIMIZATIONS=ON` a hybrid search decides
 * dynamically between the binary search and the [OPMPHM](https://master.libelektra.org/doc/dev/data-structures.md#order-preserving-minimal-perfect-hash-map-aka-opmphm).
 * The hybrid search can be overruled by passing ::KDB_O_OPMPHM or ::KDB_O_BINSEARCH in the options to ksLookup().
 *
 *
 * @param ks the KeySet that should be searched
 * @param key the Key object you are looking for
 * @param options of type ::elektraLookupFlags with some @p KDB_O_* option bits - as explained above
 *
 * @return pointer to the Key found
 * @retval 0 if no Key has been found
 * @retval 0 on NULL pointers
 *
 * @since 1.0.0
 * @see ksLookupByName() to search by a name given by a string
 * @see ksCurrent(), ksRewind(), ksNext() for iterating over a KeySet
 */
Key * ksLookup (KeySet * ks, Key * key, elektraLookupFlags options)
{
	if (!ks) return 0;
	if (!key) return 0;

	const char * name = key->key;
	if (!name) return 0;

	Key * ret = 0;
	const int mask = ~KDB_O_DEL & ~KDB_O_CREATE;

	if (options & KDB_O_SPEC)
	{
		Key * lookupKey = key;
		if (test_bit (key->flags, KEY_FLAG_RO_NAME)) lookupKey = keyDup (key, KEY_CP_NAME);
		ret = elektraLookupBySpec (ks, lookupKey, options & mask);
		if (test_bit (key->flags, KEY_FLAG_RO_NAME))
		{
			elektraCopyCallbackMeta (key, lookupKey);
			keyDel (lookupKey);
		}
	}
	else if (!(options & KDB_O_NOCASCADING) && strcmp (name, "") && name[0] == '/')
	{
		Key * lookupKey = key;
		if (test_bit (key->flags, KEY_FLAG_RO_NAME)) lookupKey = keyDup (key, KEY_CP_NAME);
		ret = elektraLookupByCascading (ks, lookupKey, options & mask);
		if (test_bit (key->flags, KEY_FLAG_RO_NAME))
		{
			elektraCopyCallbackMeta (key, lookupKey);
			keyDel (lookupKey);
		}
	}
	else
	{
		ret = elektraLookupSearch (ks, key, options & mask);
	}

	if (!ret && options & KDB_O_CREATE) ret = elektraLookupCreateKey (ks, key, options & mask);

	if (options & KDB_O_DEL) keyDel (key);

	return ret;
}

/**
 * Convenience method to look for a Key contained in @p ks with name @p name.
 *
 * There are several options that can be used in conjunction with this function.
 * All possible option flags can be found in ::elektraLookupFlags
 *
 * @param ks the KeySet that should be searched
 * @param name name of the Key you are looking for
 * @param options some @p KDB_O_* option bits (KDB_O_POP, KDB_O_DEL):
 * 	- See ksLookup() or ::elektraLookupFlags for possible options
 *
 * @return pointer to the Key found
 * @retval 0 if no Key has been found
 * @retval 0 on NULL pointers
 *
 * @since 1.0.0
 * @see ksLookup() for explanation of the functionality and examples.
 * @see ksCurrent(), ksRewind(), ksNext() for iterating over a KeySet
 */
Key * ksLookupByName (KeySet * ks, const char * name, elektraLookupFlags options)
{
	Key * found = 0;

	if (!ks) return 0;
	if (!name) return 0;

	if (!ks->size) return 0;

	struct _Key key;
	key.meta = NULL;
	keyInit (&key);
	keySetName (&key, name);

	found = ksLookup (ks, &key, options);
	elektraFree (key.key);
	elektraFree (key.ukey);
	ksDel (key.meta); // sometimes owner is set
	return found;
}

/*********************************************************************
 *                Data constructors (protected)                      *
 *********************************************************************/


/**
 * @internal
 *
 * Resize keyset.
 *
 * For internal usage only.
 *
 * Don't use that function to be portable. You can give an hint
 * how large the keyset should be in ksNew().
 *
 * Subsequent is the description of the implementation with array.
 * ksResize() enlarge or shrink the internal array to wished
 * size alloc.
 *
 * If you resize it to n, you can be sure to fill in n-1 elements,
 * the n-th element will do an automatic resize to n*2. So give
 * some spare to avoid wasteful duplication.
 *
 * @param ks the keyset which should be resized
 * @param alloc the size to which the array will be resized
 * @retval 1 on success
 * @retval 0 on nothing done because keyset would be too small.
 * @retval -1 if alloc is smaller then current size of keyset.
 * @retval -1 on memory error or null ptr
 */
int ksResize (KeySet * ks, size_t alloc)
{
	if (!ks) return -1;

	alloc++; /* for ending null byte */
	if (alloc == ks->alloc) return 1;
	if (alloc < ks->size) return 0;
	if (alloc < KEYSET_SIZE)
	{
		if (ks->alloc != KEYSET_SIZE)
			alloc = KEYSET_SIZE;
		else
			return 0;
	}

	if (ks->array == NULL)
	{ /* Not allocated up to now */
		ks->alloc = alloc;
		ks->size = 0;
		ks->array = elektraMalloc (sizeof (struct _Key *) * ks->alloc);
		clear_bit (ks->flags, (keyflag_t) KS_FLAG_MMAP_ARRAY);
		if (!ks->array)
		{
			return -1;
		}
	}
	ks->alloc = alloc;

	if (test_bit (ks->flags, KS_FLAG_MMAP_ARRAY))
	{
		// need to move the ks->array out of mmap
		Key ** new = elektraMalloc (sizeof (struct _Key *) * ks->alloc);
		if (!new)
		{
			return -1;
		}
		elektraMemcpy (new, ks->array, ks->size + 1); // copy including ending NULL
		ks->array = new;
		clear_bit (ks->flags, (keyflag_t) KS_FLAG_MMAP_ARRAY);
	}

	if (elektraRealloc ((void **) &ks->array, sizeof (struct _Key *) * ks->alloc) == -1)
	{
		elektraFree (ks->array);
		ks->array = 0;
		return -1;
	}

	return 1;
}

/**
 * @internal
 *
 * Returns current allocation size.
 *
 * This is the maximum size before a reallocation
 * happens.
 *
 * @param ks the keyset object to work with
 * @return allocated size*/
size_t ksGetAlloc (const KeySet * ks)
{
	return ks->alloc - 1;
}


/**
 * @internal
 *
 * KeySet object initializer.
 *
 * You should always use ksNew() instead of ksInit().
 *
 * Every KeySet object that will be used must be initialized first, to setup
 * pointers, counters, etc. After use, all ksInit()ialized KeySets must be
 * cleaned with ksClear().
 *
 * @see ksNew(), ksClose(), keyInit()
 * @retval 0 on success
 */
int ksInit (KeySet * ks)
{
	ks->array = 0;

	ks->size = 0;
	ks->alloc = 0;
	ks->flags = 0;

	ksRewind (ks);

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	ks->opmphm = NULL;
	// first lookup should predict so invalidate it
	elektraOpmphmInvalidate (ks);
	ks->opmphmPredictor = NULL;
#endif

	return 0;
}


/**
 * @internal
 *
 * KeySet object destructor.
 *
 * @see ksDel(), ksNew(), keyInit()
 * @retval 0 on success
 * @retval -1 on ks == NULL
 */
int ksClose (KeySet * ks)
{
	if (ks == NULL) return -1;

	Key * k;

	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
	{
		keyDecRef (k);
		keyDel (k);
	}

	if (ks->array && !test_bit (ks->flags, KS_FLAG_MMAP_ARRAY))
	{
		elektraFree (ks->array);
	}
	clear_bit (ks->flags, (keyflag_t) KS_FLAG_MMAP_ARRAY);

	ks->array = 0;
	ks->alloc = 0;

	ks->size = 0;

	elektraOpmphmInvalidate (ks);

	return 0;
}


/**
 * @}
 */
