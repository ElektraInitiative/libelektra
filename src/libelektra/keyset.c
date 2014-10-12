/***************************************************************************
                          keyset.c  -  Methods for KeySet manipulation
                             -------------------
    begin                : Sun Oct 02 2005
    copyright            : (C) 2005 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/** @class doxygenFlatCopy
 *
 * @note Because the key is not copied,
 * also the pointer to the current metadata keyNextMeta()
 * will be shared.
 */

/**
 * @defgroup keyset KeySet
 * @brief Methods to manipulate KeySets.
 *
 * A KeySet is a sorted set of keys.
 * So the correct name actually would be KeyMap.
 *
 * With ksNew() you can create a new KeySet.
 *
 * You can add keys with ksAppendKey() in the keyset.
 * Using ksAppend() you can append a whole keyset.
 *
 * @copydoc doxygenFlatCopy
 *
 * ksGetSize() tells you the current size of the keyset.
 *
 * With ksRewind() and ksNext() you can navigate through the keyset. Don't
 * expect any particular order, but it is assured that you will get every
 * key of the set.
 *
 * KeySets have an @link ksCurrent() internal cursor @endlink. This is used
 * for ksLookup() and kdbSet().
 *
 * KeySet has a fundamental meaning inside elektra. It makes it possible
 * to get and store many keys at once inside the database. In addition to
 * that the class can be used as high level datastructure in applications.
 * With ksLookupByName() it is possible to fetch easily specific keys
 * out of the list of keys.
 *
 * You can easily create and iterate keys:
 * @code
#include <kdb.h>

// create a new keyset with 3 keys
// with a hint that about 20 keys will be inside
KeySet *myConfig = ksNew(20,
	keyNew ("user/name1", 0),
	keyNew ("user/name2", 0),
	keyNew ("user/name3", 0),
	KS_END);
// append a key in the keyset
ksAppendKey(myConfig, keyNew("user/name4", 0));

Key *current;
ksRewind(myConfig);
while ((current=ksNext(myConfig))!=0)
{
	printf("Key name is %s.\n", keyName (current));
}
ksDel (myConfig); // delete keyset and all keys appended
 * @endcode
 *
 * @{
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && HAVE_STDIO_H
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

#include "kdbinternal.h"



/**
 * Allocate, initialize and return a new KeySet object.
 *
 * Objects created with ksNew() must be destroyed with ksDel().
 *
 * You can use a various long list of parameters to preload the keyset
 * with a list of keys. Either your first and only parameter is 0 or
 * your last parameter must be KEY_END.
 *
 * So, terminate with ksNew(0, KS_END) or ksNew(20, ..., KS_END)
 *
 * For most uses
 * @code
KeySet *keys = ksNew(0, KS_END);
// work with it
ksDel (keys);
 * @endcode
 * goes ok, the alloc size will be 16, defined in kdbprivate.h.
 * The alloc size will be doubled whenever size reaches alloc size,
 * so it also performs out large keysets.
 *
 * But if you have any clue how large your keyset may be you should
 * read the next statements.
 *
 * If you want a keyset with length 15 (because you know of your
 * application that you normally need about 12 up to 15 keys), use:
 * @code
KeySet * keys = ksNew (15,
	keyNew ("user/sw/app/fixedConfiguration/key01", KEY_SWITCH_VALUE, "value01", 0),
	keyNew ("user/sw/app/fixedConfiguration/key02", KEY_SWITCH_VALUE, "value02", 0),
	keyNew ("user/sw/app/fixedConfiguration/key03", KEY_SWITCH_VALUE, "value03", 0),
	// ...
	keyNew ("user/sw/app/fixedConfiguration/key15", KEY_SWITCH_VALUE, "value15", 0),
	KS_END);
// work with it
ksDel (keys);
 * @endcode
 *
 * If you start having 3 keys, and your application needs approximately
 * 200-500 keys, you can use:
 * @code
KeySet * config = ksNew (500,
	keyNew ("user/sw/app/fixedConfiguration/key1", KEY_SWITCH_VALUE, "value1", 0),
	keyNew ("user/sw/app/fixedConfiguration/key2", KEY_SWITCH_VALUE, "value2", 0),
	keyNew ("user/sw/app/fixedConfiguration/key3", KEY_SWITCH_VALUE, "value3", 0),
	KS_END); // don't forget the KS_END at the end!
// work with it
ksDel (config);
 * @endcode
 * Alloc size is 500, the size of the keyset will be 3 after ksNew.
 * This means the keyset will reallocate when appending more than
 * 497 keys.
 *
 * The main benefit of taking a list of variant length parameters is to be able
 * to have one C-Statement for any possible KeySet.
 *
 * @post the keyset is rewinded properly
 *
 * @see ksDel() to free the keyset afterwards
 * @see ksDup() to duplicate an existing keyset
 * @param alloc gives a hint for the size how many Keys may be stored initially
 * @return a ready to use KeySet object
 * @return 0 on memory error
 */
KeySet *ksNew(size_t alloc, ...)
{
	KeySet *ks;
	va_list va;

	va_start(va, alloc);
	ks = ksVNew (alloc, va);
	va_end (va);

	return ks;
}

#define ELEKTRA_MAX_PREFIX_SIZE sizeof("override/")

Key *ksLookupBySpec(KeySet *ks, Key *specKey)
{
	int prefixSize = ELEKTRA_MAX_PREFIX_SIZE - 1;
	char buffer [ELEKTRA_MAX_PREFIX_SIZE + ELEKTRA_MAX_ARRAY_SIZE]
		= "override/";
	int64_t i=0;
	const Key *m = 0;
	Key *k = 0;
	Key *ret = 0;
	do {
		elektraWriteArrayNumber(&buffer[prefixSize], i);
		m = keyGetMeta(specKey, buffer);
		if (!m) break;
		// optimization: lazy instanziation of k
		if (!k) k = keyNew(keyString(m), KDB_O_CASCADING_NAME,
				KEY_END);
		else elektraKeySetName(k, keyString(m),
				KDB_O_CASCADING_NAME);
		ret=ksLookup(ks, k, 0);
		if (ret) goto finished;
		++i;
	} while(m);

	{
		ret=ksLookup(ks, specKey, 0);
		if (ret) goto finished;
	}

	strcpy (buffer, "fallback/");
	i=0;
	m = 0;
	do {
		elektraWriteArrayNumber(&buffer[prefixSize], i);
		m = keyGetMeta(specKey, buffer);
		if (!m) break;
		// optimization: lazy instanziation of k
		if (!k) k = keyNew(keyString(m), KDB_O_CASCADING_NAME,
				KEY_END);
		else elektraKeySetName(k, keyString(m),
				KDB_O_CASCADING_NAME);
		ret=ksLookup(ks, k, 0);
		if (ret) goto finished;
		++i;
	} while(m);

	{
		m = keyGetMeta(specKey, "default");
		if (!m) goto finished;
		ret=keyDup(specKey);
		if (!ret) goto finished;
		keySetString(ret, keyString(m));
		ksAppendKey(ks, ret);
	}

finished:
	keyDel(k);
	return ret;
}

/**
 * @copydoc ksNew
 *
 * @pre caller must call va_start and va_end
 * @par va the list of arguments
 * @param alloc the allocation size
 * @param va the list of variable arguments
 **/
KeySet *ksVNew (size_t alloc, va_list va)
{
	KeySet *keyset=0;
	Key *key = 0;

	keyset= (KeySet *)elektraMalloc(sizeof(KeySet));
	if (!keyset)
	{
		/*errno = KDB_ERR_NOMEM;*/
		return 0;
	}
	ksInit(keyset);

	alloc ++; /* for ending null byte */
	if (alloc < KEYSET_SIZE) keyset->alloc=KEYSET_SIZE;
	else keyset->alloc=alloc;

	keyset->array = elektraMalloc (sizeof(struct _Key *) * keyset->alloc);
	if (!keyset->array)
	{
		/*errno = KDB_ERR_NOMEM;*/
		return 0;
	}
	keyset->array[0] = 0;

	if (va && alloc != 1)
	{
		key = (struct _Key *) va_arg (va, struct _Key *);
		while (key)
		{
			ksAppendKey(keyset, key);
			key = (struct _Key *) va_arg (va, struct _Key *);
		}

	}

	ksRewind(keyset);

	return keyset;
}

/**
 * Return a duplicate of a keyset.
 *
 * Objects created with ksDup() must be destroyed with ksDel().
 *
 * Memory will be allocated as needed for dynamic properties,
 * so you need to ksDel() the returned pointer.
 *
 * A flat copy is made, so the keys will not be duplicated,
 * but there reference counter is updated, so both keysets
 * need ksDel().
 *
 * @param source has to be an initialized source KeySet
 * @return a flat copy of source on success
 * @return 0 on NULL pointer
 * @see ksNew(), ksDel()
 * @see keyDup() for key duplication
 */
KeySet *ksDup (const KeySet * source)
{
	KeySet *keyset=0;

	if (!source) return 0;

	keyset = ksNew(source->alloc,KS_END);
	ksAppend (keyset, source);
	return keyset;
}



/**
 * Copy a keyset.
 *
 * Most often you may want a duplicate of a keyset, see
 * ksDup() or append keys, see ksAppend().
 * But in some situations you need to copy a
 * keyset to a existing keyset, for that this function
 * exists.
 *
 * You can also use it to clear a keyset when you pass
 * a NULL pointer as @p source.
 *
 * Note that all keys in @p dest will be deleted. Afterwards
 * the content of the source will be added to the destination
 * and the ksCurrent() is set properly in @p dest.
 *
 * A flat copy is made, so the keys will not be duplicated,
 * but there reference counter is updated, so both keysets
 * need to be ksDel().
 *
 * @copydoc doxygenFlatCopy
 *
 * @code
int f (KeySet *ks)
{
	KeySet *c = ksNew (20, ..., KS_END);
	// c receives keys
	ksCopy (ks, c); // pass the keyset to the caller

	ksDel (c);
}	// caller needs to ksDel (ks)
 * @endcode
 *
 * @param source has to be an initialized source KeySet or NULL
 * @param dest has to be an initialized KeySet where to write the keys
 * @return 1 on success
 * @return 0 if dest was cleared successfully (source is NULL)
 * @return -1 on NULL pointer
 * @see ksNew(), ksDel(), ksDup()
 * @see keyCopy() for copying keys
 */
int ksCopy (KeySet *dest, const KeySet *source)
{
	if (!dest) return -1;
	ksClear (dest);
	if (!source) return 0;

	ksAppend (dest, source);
	ksSetCursor (dest, ksGetCursor (source));

	return 1;
}



/**
 * A destructor for KeySet objects.
 *
 * Cleans all internal dynamic attributes, decrement all reference pointers
 * to all keys and then keyDel() all contained Keys,
 * and free()s the release the KeySet object memory (that was previously
 * allocated by ksNew()).
 *
 * @param ks the keyset object to work with
 * @return 0 when the keyset was freed
 * @return -1 on null pointer
 * @see ksNew()
 */
int ksDel(KeySet *ks)
{
	int rc;

	if (!ks) return -1;

	rc=ksClose(ks);
	elektraFree(ks);

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
 * @return 0 on sucess
 * @return -1 on failure
 */
int ksClear(KeySet *ks)
{
	ksClose (ks);

	if (ks->array)
	{	/* go back to standard size KEYSET_SIZE */
		if (elektraRealloc ((void**) &ks->array, sizeof(struct _Key *) * KEYSET_SIZE) == -1)
		{
			/*errno = KDB_ERR_NOMEM;*/
			elektraFree (ks->array);
			ks->array = 0;
			ks->size = 0;
			return -1;
		}
	} else {
		if ((ks->array = elektraMalloc (sizeof(struct _Key *) * KEYSET_SIZE)) == 0)
		{
			/*errno = KDB_ERR_NOMEM;*/
			ks->size = 0;
			return -1;
		}
	}
	ks->alloc = KEYSET_SIZE;


	return 0;
}


/**
 * @brief Compare by unescaped name only (not by owner, they are equal)
 *
 * Other Cmp* are based on this one.
 *
 * Is suitable for binary search (but may return wrong owner)
 *
 */
static int keyCmpByName(const void *p1, const void *p2)
{
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const void *name1 = key1->key+key1->keySize;
	const void *name2 = key2->key+key2->keySize;
	size_t const nameSize1 = key1->keyUSize;
	size_t const nameSize2 = key2->keyUSize;
	int ret = 0;
	if (nameSize1 == nameSize2)
	{
		ret = memcmp(name1, name2, nameSize2);
	} else {
		if (nameSize1 < nameSize2)
		{
			ret = memcmp(name1, name2, nameSize1);
			if (ret==0) ret = -1;
		} else {
			ret = memcmp(name1, name2, nameSize2);
			if (ret==0) ret = 1;
		}
	}
	return ret;
}

static int keyCompareByOwner(const void *p1, const void *p2)
{
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *owner1 = keyValue(keyGetMeta(key1, "owner"));
	const char *owner2 = keyValue(keyGetMeta(key2, "owner"));
	if (!owner1 && !owner2) return 0;
	if (!owner1) return -1;
	if (!owner2) return 1;
	return elektraStrCmp(owner1, owner2);
}

/**
 * @internal
 *
 * Defines how keys are sorted in the keyset
 *
 * Compares by unescaped name, and if equal by owner
 *
 * Is suitable for binary search
 *
 * @see keyCmp, keyCmpByName
 */
static int keyCmpByNameOwner(const void *p1, const void *p2)
{
	int ret = keyCmpByName(p1, p2);

	if (ret == 0)
	{
		return keyCompareByOwner(p1,p2);
	}
	return ret;
}

/**
 * @brief Compare by name
 *
 * @return 
 */
static int keyCompareByName(const void *p1, const void *p2)
{
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);

	return elektraStrCmp(name1, name2);
}

static int keyCompareByNameCase(const void *p1, const void *p2)
{
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);

	return elektraStrCaseCmp(name1, name2);
}

static int keyCompareByNameOwner(const void *p1, const void *p2)
{
	int result = keyCompareByName(p1, p2);

	if (result == 0)
	{
		return keyCompareByOwner(p1,p2);
	}
	return result;
}


static int keyCompareByNameOwnerCase(const void *p1, const void *p2)
{
	int result = keyCompareByNameCase(p1, p2);

	if (result == 0)
	{
		return keyCompareByOwner(p1,p2);
	}
	return result;
}



/**
 * Compare the name of two keys.
 *
 * @return a number less than, equal to or greater than zero if
 *    k1 is found, respectively, to be less than, to match, or
 *    be greater than k2.
 *
 * The comparison is based on a strcmp of the keynames, and iff
 * they match a strcmp of the owner will be used to distuingish.
 * If even this matches the keys are found to be exactly the
 * same and 0 is returned. These two keys can't be used in the same
 * KeySet.
 *
 * keyCmp() defines the sorting order for a KeySet.
 *
 * The following 3 points are the rules for null values:
 *
 * - A null pointer will be found to be smaller than every other
 * key. If both are null pointers, 0 is returned.
 *
 * - A null name will be found to be smaller than every other
 * name. If both are null names, 0 is returned.
 *
 * If the name is equal then:
 *
 * - No owner will be found to be smaller then every other owner.
 * If both don't have a owner, 0 is returned.
 *
 * @note the owner will only be used if the names are equal.
 *
 * Often is enough to know if the other key is
 * less then or greater then the other one.
 * But Sometimes you need more precise information,
 * see keyRel().
 *
 * Given any Keys k1 and k2 constructed with keyNew(), following
 * equation hold true:
 *
 * @snippet testabi_rel.c cmp null
 *
 * Here are some more examples:
 * @code
Key *k1 = keyNew("user/a", KEY_END);
Key *k2 = keyNew("user/b", KEY_END);

// keyCmp(k1,k2) < 0
// keyCmp(k2,k1) > 0
 * @endcode
 *
 * And even more:
 * @code
Key *k1 = keyNew("user/a", KEY_OWNER, "markus", KEY_END);
Key *k2 = keyNew("user/a", KEY_OWNER, "max", KEY_END);

// keyCmp(k1,k2) < 0
// keyCmp(k2,k1) > 0
 * @endcode
 *
 * Do not strcmp the keyName() yourself because
 * the result differs from simple ascii comparison.
 *
 * @param k1 the first key object to compare with
 * @param k2 the second key object to compare with
 *
 * @see ksAppendKey(), ksAppend() will compare keys when appending
 * @see ksLookup() will compare keys during searching
 * @ingroup keytest
 */
int keyCmp (const Key *k1, const Key *k2)
{
	if (!k1 && !k2) return 0;
	if (!k1) return -1;
	if (!k2) return 1;

	if (!k1->key && !k2->key) return 0;
	if (!k1->key) return -1;
	if (!k2->key) return 1;

	return keyCmpByNameOwner(&k1, &k2);
}

/**
 * Compare the order metadata of two keys.
 *
 * @return a number less than, equal to or greater than zero if
 *    the order of k1 is found, respectively, to be less than,
 *    to match, or be greater than the order of k2. If one key is
 *    NULL, but the other isn't, the key which is not NULL is considered
 *    to be greater. If both keys are NULL, they are
 *    considered to be equal. If one key does have an order
 *    metadata but the other has not, the key with the metadata
 *    is considered greater. If no key has metadata,
 *    they are considered to be equal.
 *
 * @param ka key to compare with
 * @param kb other key to compare with
 */
int elektraKeyCmpOrder(const Key *ka, const Key *kb)
{

	if (!ka && !kb) return 0;

	if (ka && !kb) return 1;

	if (!ka && kb) return -1;

	int aorder = -1;
	int border = -1;

	const Key *kam = keyGetMeta (ka, "order");
	const Key *kbm = keyGetMeta (kb, "order");

	if (kam) aorder = atoi (keyString (kam));
	if (kbm) border = atoi (keyString (kbm));

	if (aorder > 0 && border > 0) return aorder - border;

	if (aorder < 0 && border < 0) return 0;

	if (aorder < 0 && border >= 0) return -1;

	if (aorder >= 0 && border < 0) return 1;

	/* cannot happen anyway */
	return 0;
}


/**
 * @internal
 *
 * Checks if KeySet needs sync.
 *
 * When keys are changed this is reflected into keyNeedSync().
 *
 * But when keys are popped from a keyset this can't be seen
 * by looking at the individual keys.
 *
 * ksNeedSync() allows the backends to know if a key was
 * popped from the keyset to know that this keyset needs
 * to be written out.
 *
 * @param ks the keyset to work with
 * @return -1 on null keyset
 * @return 0 if it does not need sync
 * @return 1 if it needs sync
 */
int ksNeedSync(const KeySet *ks)
{
	if (!ks) return -1;

	return (ks->flags & KS_FLAG_SYNC) == KS_FLAG_SYNC;
}




/**
 * Return the number of keys that @p ks contains.
 *
 * @param ks the keyset object to work with
 * @return the number of keys that @p ks contains.
 * @return -1 on NULL pointer
 * @see ksNew(0, KS_END), ksDel()
 */
ssize_t ksGetSize(const KeySet *ks)
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
 * Binary search in a keyset that informs where key should be inserted.
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
ssize_t ksSearchInternal(const KeySet *ks, const Key *toAppend)
{
	ssize_t left = 0;
	ssize_t right = ks->size-1;
	register int cmpresult = 1;
	ssize_t middle = -1;
	ssize_t insertpos = 0;
#if DEBUG && VERBOSE
	int c=0;
#endif


	while(1)
	{
#if DEBUG && VERBOSE
		++c;
#endif
		if (right < left)
		{
			/* Nothing was found */
			break;
		}
		middle = left + ((right-left)/2);
		cmpresult = keyCmpByNameOwner(&toAppend, &ks->array[middle]);
		if (cmpresult > 0)
		{
			insertpos = left = middle + 1;
		} else if (cmpresult == 0)
		{
			/* We have found it */
			break;
		} else {
			insertpos = middle;
			right = middle - 1;
		}
#if DEBUG && VERBOSE
		/* This is even for verbose too verbose!
		printf ("bsearch -- c: %d res: %d left: %zd middle: %zd right: %zd insertpos: %zd\n",
			c, cmpresult, left, middle, right, insertpos);
		*/
#endif
	}

	if (!cmpresult)
	{
		return middle;
	} else {
		return -insertpos - 1;
	}
}

/**
 * Appends a Key to the end of @p ks.
 *
 * A pointer to the key will
 * be stored, and not a private copy. So a future ksDel() on
 * @p ks may keyDel() the @p toAppend object, see keyGetRef().
 *
 * The reference counter of the key will be incremented, and
 * thus toAppend is not const.
 *
 * @copydoc doxygenFlatCopy
 *
 * If the keyname already existed, it will be replaced with
 * the new key.
 *
 * The KeySet internal cursor will be set to the new key.
 *
 * It is save to use ksAppendKey(ks, keyNew(..)).
 *
 *
 * @return the size of the KeySet after insertion
 * @return -1 on NULL pointers
 * @return -1 if insertion failed, the key will be deleted then.
 * @param ks KeySet that will receive the key
 * @param toAppend Key that will be appended to ks or deleted
 * @see ksAppend(), keyNew(), ksDel()
 * @see keyIncRef()
 *
 */
ssize_t ksAppendKey(KeySet *ks, Key *toAppend)
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

	keyLock(toAppend, KEY_LOCK_NAME);

	result = ksSearchInternal(ks, toAppend);

	if (result >= 0)
	{
		/* Seems like the key already exist. */
		if (toAppend == ks->array[result])
		{
			/* user tried to insert the same key again */
			return ks->size;
		}

		/* Pop the key in the result */
		keyDecRef (ks->array[result]);
		keyDel (ks->array[result]);

		/* And use the other one instead */
		keyIncRef (toAppend);
		ks->array[result] = toAppend;
		ksSetCursor(ks, result);
	} else {
		ssize_t insertpos = -result-1;

		/* We want to append a new key
		  in position insertpos */
		++ ks->size;
		if (ks->size >= ks->alloc) ksResize (ks, ks->alloc * 2-1);
		keyIncRef (toAppend);

		if (insertpos == (ssize_t)ks->size-1)
		{
			/* Append it to the very end */
			ks->array[ks->size-1] = toAppend;
			ks->array[ks->size] = 0;
			ksSetCursor(ks, ks->size-1);
		} else {
			size_t n = ks->size-insertpos;
			memmove(ks->array+(insertpos+1), ks->array+insertpos, n*sizeof(struct Key*));
			/*
			printf ("memmove -- ks->size: %zd insertpos: %zd n: %zd\n",
				ks->size, insertpos, n);
			*/
			ks->array[insertpos] = toAppend;
			ksSetCursor(ks, insertpos);
		}
	}

	return ks->size;
}



/**
 * Append all @p toAppend contained keys to the end of the @p ks.
 *
 * @p toAppend KeySet will be left unchanged.
 *
 * If a key is both in toAppend and ks, the Key in ks will be
 * overridden.
 *
 * @copydoc doxygenFlatCopy
 *
 * @post Sorted KeySet ks with all keys it had before and additionally
 *       the keys from toAppend
 * @return the size of the KeySet after transfer
 * @return -1 on NULL pointers
 * @param ks the KeySet that will receive the keys
 * @param toAppend the KeySet that provides the keys that will be transferred
 * @see ksAppendKey()
 * 
 */
ssize_t ksAppend(KeySet *ks, const KeySet *toAppend)
{
	size_t toAlloc = 0;

	if (!ks) return -1;
	if (!toAppend) return -1;

	if (toAppend->size <= 0) return ks->size;

	/* Do only one resize in advance */
	for (toAlloc = ks->alloc; ks->size+toAppend->size >= toAlloc; toAlloc *= 2);
	ksResize (ks, toAlloc-1);

	/* TODO: here is lots of room for optimizations */
	for (size_t i=0; i<toAppend->size; ++i)
	{
		ksAppendKey (ks, toAppend->array[i]);
	}
	return ks->size;
}


/**
 * @internal
 *
 * Returns the previous Key in a KeySet.
 *
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksPrev() is called the cursor is decremented and the new current Key
 * is returned.
 *
 * You'll get a NULL pointer if the key before begin of the KeySet was reached.
 *
 * Don't delete the key, use ksPop() if you want to delete it.
 *
 * @return the new current Key
 * @see ksRewind(), ksCurrent()
 *
 */
Key *ksPrev(KeySet *ks)
{
	if (ks->size == 0) return 0;
	if (ks->current <= 0)
	{
		ksRewind (ks);
		return 0;
	}
	ks->current--;
	return ks->cursor = ks->array[ks->current];
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
 * @retval -1 if length is smaller then 0
 * @return the number of moved elements otherwise
 */
ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from)
{
	ssize_t ssize = ks->size;
	ssize_t sto = to;
	ssize_t sfrom = from;

	ssize_t sizediff = sto-sfrom;
	ssize_t length = ssize-sfrom;
	size_t ret = 0;

	if (length < 0) return -1;
	if (ks->size < to) return -1;

	ks->size = ks->size + sizediff;
	ret = elektraMemmove(ks->array + to, ks->array + from, length);
	ks->array[ks->size] = 0;
	return ret;
}

/**
 * Cuts out a keyset at the cutpoint.
 *
 * Searches for the cutpoint inside the KeySet ks.
 * If found it cuts out everything which is below (see keyIsBelow()) this key.
 * These keys will be missing in the keyset @p ks.
 * Instead, they will be moved to the returned keyset.
 * If @p cutpoint is not found an empty keyset is returned and @p ks
 * is not changed.
 *
 * The cursor will stay at the same key as it was before.
 * If the cursor was inside the region of cut (moved)
 * keys, the cursor will be set to the key before
 * the cutpoint.
 *
 * If you use ksCut() on a keyset you got from kdbGet() and plan to make
 * a kdbSet() later, make sure that you keep all keys that should not
 * be removed permanently. You have to keep the KeySet that was returned
 * and the KeySet @p ks.
 *
 * @par Example:
 *
 * You have the keyset @p ks:
 * - @p system/mountpoint/interest
 * - @p system/mountpoint/interest/folder
 * - @p system/mountpoint/interest/folder/key1
 * - @p system/mountpoint/interest/folder/key2
 * - @p system/mountpoint/other/key1
 *
 * When you use
 * @snippet cut.c cut
 *
 * Then in @p returned are:
 * - @p system/mountpoint/interest
 * - @p system/mountpoint/interest/folder
 * - @p system/mountpoint/interest/folder/key1
 * - @p system/mountpoint/interest/folder/key2
 *
 * And in @p ks are:
 * - @p system/mountpoint/other/key1
 *
 * So kdbSet() permanently removes all keys below
 * @p system/mountpoint/interest.
 *
 * @see kdbGet() for explanation why you might get more keys than you
 * requested.
 *
 * @return a new allocated KeySet which needs to deleted with ksDel().
 *         The keyset consists of all keys (of the original keyset ks)
 *         below the cutpoint. If the key cutpoint exists, it will
 *         also be appended.
 * @retval 0 on null pointers, no key name or allocation problems
 * @param ks the keyset to cut. It will be modified by removing
 *           all keys below the cutpoint.
 *           The cutpoint itself will also be removed.
 * @param cutpoint the point where to cut out the keyset
 */
KeySet *ksCut(KeySet *ks, const Key *cutpoint)
{
	KeySet *returned = 0;
	size_t found = 0;
	size_t it = 0;
	size_t newsize = 0;
	int set_cursor = 0;

	if (!ks) return 0;
	if (!cutpoint) return 0;
	if (!cutpoint->key) return 0;

	// search the cutpoint
	while (it < ks->size && keyIsBelowOrSame(cutpoint, ks->array[it]) == 0)
	{
		++it;
	}

	// we found nothing
	if (it == ks->size) return ksNew(0, KS_END);

	// we found the cutpoint
	found = it;

	// search the end of the keyset to cut
	while (it < ks->size && keyIsBelowOrSame(cutpoint, ks->array[it]) == 1)
	{
		++it;
	}

	// correct cursor if cursor is in cutted keyset
	if (ks->current >= found && ks->current < it)
	{
		if (found == 0)
		{
			ksRewind(ks);
		}
		else
		{
			ks->current = found - 1;
			set_cursor = 1;
		}
	}

	// correct the cursor for the keys after the cutted keyset
	if (ks->current >= it)
	{
		if (it >= ks->size)
		{
			ksRewind(ks);
		}
		else
		{
			ks->current = found + ks->current - it;
			set_cursor = 1;
		}
	}

	newsize = it-found;

	returned = ksNew(newsize, KS_END);
	elektraMemcpy (returned->array, ks->array+found, newsize);
	returned->size = newsize;
	returned->array[returned->size] = 0;

	if (ksCopyInternal(ks, found, it) == -1)
	{
#if DEBUG
		printf ("ksCopyInternal returned an error inside ksCut\n");
#endif
	}

	if (set_cursor) ks->cursor = ks->array[ks->current];

	return returned;
}




/**
 * Remove and return the last key of @p ks.
 *
 * The reference counter will be decremented by one.
 *
 * The KeySets cursor will not be effected if it did not
 * point to the popped key.
 *
 * @note You need to keyDel() the key afterwards, if
 * you don't append it to another keyset. It has the
 * same semantics like a key allocated with keyNew()
 * or keyDup().
 *
 *@code
ks1=ksNew(0, KS_END);
ks2=ksNew(0, KS_END);

k1=keyNew("user/name", KEY_END); // ref counter 0
ksAppendKey(ks1, k1); // ref counter 1
ksAppendKey(ks2, k1); // ref counter 2

k1=ksPop (ks1); // ref counter 1
k1=ksPop (ks2); // ref counter 0, like after keyNew()

ksAppendKey(ks1, k1); // ref counter 1

ksDel (ks1); // key is deleted too
ksDel (ks2);
 *@endcode
 *
 * @return the last key of @p ks
 * @return NULL if @p ks is empty or on NULL pointer
 * @param ks KeySet to work with
 * @see ksAppendKey(), ksAppend()
 * @see commandList() for an example
 *
 */
Key *ksPop(KeySet *ks)
{
	Key *ret=0;

	if (!ks) return 0;

	ks->flags |= KS_FLAG_SYNC;

	if (ks->size <= 0) return 0;

	-- ks->size;
	if (ks->size+1 < ks->alloc/2) ksResize (ks, ks->alloc / 2-1);
	ret = ks->array[ks->size];
	ks->array[ks->size] = 0;
	keyDecRef(ret);

	return ret;
}

/**
 * Builds an array of pointers to the keys in the supplied keyset.
 * The keys are not copied, calling keyDel may remove them from
 * the keyset.
 *
 * The size of the buffer can be easily allocated via ksGetSize. Example:
 * @code
 * KeySet *ks = somekeyset;
 * Key **keyArray = calloc (ksGetSize(ks), sizeof (Key *));
 * elektraKsToMemArray (ks, keyArray);
 * ... work with the array ...
 * free (keyArray);
 * @endcode
 *
 * @param ks the keyset object to work with
 * @param buffer the buffer to put the result into
 * @return the number of elements in the array if successful
 * @return a negative number on null pointers or if an error occurred
 */
int elektraKsToMemArray(KeySet *ks, Key **buffer)
{
	if (!ks) return -1;
	if (!buffer) return -1;

	/* clear the received buffer */
	memset (buffer, 0, ksGetSize (ks) * sizeof(Key *));

	cursor_t cursor = ksGetCursor (ks);
	ksRewind (ks);
	size_t idx = 0;

	Key *key;
	while ((key = ksNext (ks)) != 0)
	{
		buffer[idx] = key;
		++idx;
	}
	ksSetCursor (ks, cursor);

	return idx;
}


/*******************************************
 *           KeySet browsing methods       *
 *******************************************/



/**
 * Rewinds the KeySet internal cursor.
 *
 * Use it to set the cursor to the beginning of the KeySet.
 * ksCurrent() will then always return NULL afterwards. So
 * you want to ksNext() first.
 *
 * @code
ksRewind (ks);
while ((key = ksNext (ks))!=0) {}
 * @endcode
 *
 * @param ks the keyset object to work with
 * @return 0 on success
 * @return -1 on NULL pointer
 * @see ksNext(), ksCurrent()
 */
int ksRewind(KeySet *ks)
{
	if (!ks) return -1;

	ks->cursor=0;
	ks->current=0;
	return 0;
}


/**
 * Returns the next Key in a KeySet.
 *
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksNext() is called the cursor is incremented and the new current Key
 * is returned.
 *
 * You'll get a NULL pointer if the key after the end of the KeySet was reached.
 * On subsequent calls of ksNext() it will still return the NULL pointer.
 *
 * The @p ks internal cursor will be changed, so it is not const.
 *
 * @note You must not delete or change the key, use ksPop() if you want to delete it.
 *
 * @param ks the keyset object to work with
 * @return the new current Key
 * @return 0 when the end is reached
 * @return 0 on NULL pointer
 * @see ksRewind(), ksCurrent()
 */
Key *ksNext(KeySet *ks)
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
 * The pointer is NULL if you reached the end or after
 * ksRewind().
 *
 * @note You must not delete the key or change the key,
 *    use ksPop() if you want to delete it.
 *
 * @param ks the keyset object to work with
 * @return pointer to the Key pointed by @p ks's cursor
 * @return 0 on NULL pointer
 * @see ksNext(), ksRewind()
 */
Key *ksCurrent(const KeySet *ks)
{
	if (!ks) return 0;

	return ks->cursor;
}




/**
 * Return the first key in the KeySet.
 *
 * The KeySets cursor will not be effected.
 *
 * If ksCurrent()==ksHead() you know you are
 * on the first key.
 *
 * @param ks the keyset object to work with
 * @return the first Key of a keyset
 * @return 0 on NULL pointer or empty keyset
 * @see ksTail() for the last key
 * @see ksRewind(), ksCurrent() and ksNext() for iterating over the keyset
 */
Key *ksHead(const KeySet *ks)
{
	if (!ks) return 0;

	if (ks->size > 0) return ks->array[0];
	else return 0;
}





/**
 * Return the last key in the KeySet.
 *
 * The KeySets cursor will not be effected.
 *
 * If ksCurrent()==ksTail() you know you
 * are on the last key. ksNext() will return
 * a NULL pointer afterwards.
 *
 * @param ks the keyset object to work with
 * @return the last Key of a keyset
 * @return 0 on NULL pointer or empty keyset
 * @see ksHead() for the first key
 * @see ksRewind(), ksCurrent() and ksNext() for iterating over the keyset
 */
Key *ksTail(const KeySet *ks)
{
	if (!ks) return 0;

	if (ks->size > 0) return ks->array[ks->size-1];
	else return 0;
}



/**
 * Get the KeySet internal cursor.
 *
 * Use it to get the cursor of the actual position.
 *
 * @warning Cursors are getting invalid when the key
 * was ksPop()ed or ksLookup() with KDB_O_POP was
 * used.
 *
 * @section readahead Read ahead
 *
 * With the cursors it is possible to read ahead
 * in a keyset:
 *
 * @code
cursor_t jump;
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
 * keyset in a function
 *
 * @code
int f (KeySet *ks)
{
	cursor_t state = ksGetCursor(ks);

	// work with keyset

	// now bring the keyset to the state before
	ksSetCursor (ks, state);
}
 * @endcode
 *
 * It is of course possible to make the KeySet const
 * and cast its const away to set the cursor.
 * Another way to achieve
 * the same is to ksDup() the keyset, but it is
 * not as efficient.
 *
 * An invalid cursor will be returned directly after
 * ksRewind(). When you set an invalid cursor ksCurrent()
 * is 0 and ksNext() == ksHead().
 *
 * @note Only use a cursor for the same keyset which it was
 * made for.
 *
 * @param ks the keyset object to work with
 * @return a valid cursor on success
 * @return an invalid cursor on NULL pointer or after ksRewind()
 * @see ksNext(), ksSetCursor()
 */
cursor_t ksGetCursor(const KeySet *ks)
{
	if (!ks) return (cursor_t) -1;

	if (ks->cursor == 0) return (cursor_t) -1;
	else return (cursor_t) ks->current;
}

/**
 * @brief return key at given cursor position
 *
 * @param ks the keyset to pop key from
 * @param pos where to get
 * @return the key at the cursor position on success
 * @retval NULL on NULL pointer, negative cursor position
 * or a position that does not lie within the keyset
 */
Key *ksAtCursor(KeySet *ks, cursor_t pos)
{
	if (!ks) return 0;
	if (pos < 0) return 0;
	if (ks->size < (size_t)pos) return 0;
	return ks->array[pos];
}

/**
 * @internal
 *
 * @brief Pop key at given cursor position
 *
 * @param ks the keyset to pop key from
 * @param c where to pop
 *
 * The internal cursor will be rewinded using ksRewind(). You can use
 * ksGetCursor() and ksSetCursor() jump back to the previous position.
 * e.g. to pop at current position within ksNext() loop:
 * @code
 * cursor_t c = ksGetCursor(ks);
 * keyDel (ksPopAtCursor(ks, c));
 * ksSetCursor(ks, c);
 * ksPrev(ks); // to have correct key after next ksNext()
 * @endcode
 *
 * @warning do not use, will be superseded by external iterator API
 *
 * @return the popped key
 * @retval 0 if ks is 0
 */
Key *ksPopAtCursor(KeySet *ks, cursor_t pos)
{
	if (!ks) return 0;
	if (pos<0) return 0;
	if (pos>SSIZE_MAX) return 0;

	size_t c = pos;
	if (c>=ks->size) return 0;

	if (c != ks->size-1)
	{
		Key ** found = ks->array+c;
		Key * k = *found;
		/* Move the array over the place where key was found
		 *
		 * e.g. c = 2
		 *   size = 6
		 *
		 * 0  1  2  3  4  5  6
		 * |--|--|c |--|--|--|size
		 * move to (c/pos is overwritten):
		 * |--|--|--|--|--|
		 *
		 * */
		memmove (found,
			found+1,
			(ks->size-c-1) * sizeof(Key *));
		*(ks->array+ks->size-1) = k; // prepare last element to pop
	}
	else
	{
		// if c is on last position it is just a ksPop..
		// so do nothing..
	}

	ksRewind(ks);

	return ksPop(ks);
}



/**
 * Set the KeySet internal cursor.
 *
 * Use it to set the cursor to a stored position.
 * ksCurrent() will then be the position which you got with.
 *
 * @warning Cursors may get invalid when the key
 * was ksPop()ed or ksLookup() was used together
 * with KDB_O_POP.
 *
 * @code
cursor_t cursor;
..
// key now in any position here
cursor = ksGetCursor (ks);
while ((key = keyNextMeta (ks))!=0) {}
ksSetCursor (ks, cursor); // reset state
ksCurrent(ks); // in same position as before
 * @endcode
 *
 * An invalid cursor will set the keyset to its beginning like
 * ksRewind(). When you set an invalid cursor ksCurrent()
 * is 0 and ksNext() == ksHead().
 *
 * @param cursor the cursor to use
 * @param ks the keyset object to work with
 * @return 0 when the keyset is ksRewind()ed
 * @return 1 otherwise
 * @return -1 on NULL pointer
 * @see ksNext(), ksGetCursor()
 */
int ksSetCursor(KeySet *ks, cursor_t cursor)
{
	if (!ks) return -1;

	if ((cursor_t) -1 == cursor)
	{
		ksRewind (ks);
		return 0;
	}
	ks->current= (size_t)cursor;
	ks->cursor=ks->array[ks->current];
	return 1;
}




/******************************************* 
 *    Looking up Keys inside KeySets       *
 *******************************************/

/**
 * Look for a Key contained in @p ks that matches the name of the @p key.
 *
 * @section Introduction
 *
 * @p ksLookup() is designed to let you work with
 * entirely pre-loaded KeySets, so instead of kdbGetKey(), key by key, the
 * idea is to fully kdbGet() for your application root key and
 * process it all at once with @p ksLookup().
 *
 * This function is very efficient by using binary search. Together with
 * kdbGet() which can you load the whole configuration with only
 * some communication to backends you can write very effective but short
 * code for configuration.
 *
 * @section Usage
 *
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor will not move, and a NULL pointer is
 * returned.
 *
 * Cascading is done if the first character is a /. This leads to ignoring
 * the prefix like system/ and user/.
 * @code
if (kdbGet(handle, "user/myapp", myConfig, 0 ) == -1)
	errorHandler ("Could not get Keys");

if (kdbGet(handle, "system/myapp", myConfig, 0 ) == -1)
	errorHandler ("Could not get Keys");

if ((myKey = ksLookup(myConfig, key, 0)) == NULL)
	errorHandler ("Could not Lookup Key");
 * @endcode
 *
 * This is the way multi user Programs should get there configuration and
 * search after the values. It is guaranteed that more namespaces can be
 * added easily and that all values can be set by admin and user.
 *
 * @subsection KDB_O_NOALL
 *
 * When KDB_O_NOALL is set the keyset will be only searched from ksCurrent()
 * to ksTail(). You need to ksRewind() the keyset yourself. ksCurrent() is
 * always set properly after searching a key, so you can go on searching
 * another key after the found key.
 *
 * When KDB_O_NOALL is not set the cursor will stay untouched and all keys
 * are considered. A much more efficient binary search will be used then.
 *
 * @subsection KDB_O_POP
 *
 * When KDB_O_POP is set the key which was found will be ksPop()ed. ksCurrent()
 * will not be changed, only iff ksCurrent() is the searched key, then the keyset
 * will be ksRewind()ed.
 *
 * @note Like in ksPop() the popped key always needs to be keyDel() afterwards, even
 * if it is appended to another keyset.
 *
 * @warning All cursors on the keyset will be invalid
 * iff you use KDB_O_POP, so don't use this if you rely on a cursor, see ksGetCursor().
 *
 * You can solve this problem by using KDB_O_NOALL, risking you have to iterate n^2 instead of n.
 *
 * The more elegant way is to separate the keyset you use for ksLookup() and ksAppendKey():
 * @code
int f(KeySet *iterator, KeySet *lookup)
{
	KeySet *append = ksNew (ksGetSize(lookup), KS_END);
	Key *key;
	Key *current;

	ksRewind(iterator);
	while (current=ksNext(iterator))
	{
		key = ksLookup (lookup, current, KDB_O_POP);
		// do something...
		ksAppendKey(append, key); // now append it to append, not lookup!
		keyDel (key); // make sure to ALWAYS delete poped keys.
	}
	ksAppend(lookup, append);
	// now lookup needs to be sorted only once, append never
	ksDel (append);
}
 * @endcode
 *
 * @param ks where to look for
 * @param key the key object you are looking for
 * @param options some @p KDB_O_* option bits:
 * 	- @p KDB_O_NOCASE @n
 * 		Lookup ignoring case.
 * 	- @p KDB_O_WITHOWNER @n
 * 		Also consider correct owner.
 * 	- @p KDB_O_NOALL @n
 * 		Only search from ksCurrent() to end of keyset, see above text.
 * 	- @p KDB_O_POP @n
 * 		Pop the key which was found.
 *	- @p KDB_O_DEL @n
 *		Delete the passed key.
 * @return pointer to the Key found, 0 otherwise
 * @return 0 on NULL pointers
 * @see ksLookupByName() to search by a name given by a string
 * @see ksCurrent(), ksRewind(), ksNext() for iterating over a keyset
 */
Key *ksLookup(KeySet *ks, Key * key, option_t options)
{
	cursor_t cursor = 0;

	if (!ks) return 0;

	cursor = ksGetCursor (ks);

	if (!key) return 0;
	char * name = key->key;
	if (!name) return 0;

	if (strcmp(name, "") && name[0] == '/')
	{
		size_t length = strlen (name) + sizeof ("system");
		char newname[length*2];
		strncpy (newname+2, "user", 4);
		strcpy  (newname+6, name);
		key->key = newname+2;
		key->keySize = length-2;
		elektraFinalizeName(key);
		Key *found = ksLookup(ks, key, options); // call me

		if (!found)
		{
			strncpy (newname, "system",6);
			key->key = newname;
			key->keySize = length;
			elektraFinalizeName(key);
			found = ksLookup(ks, key, options); // call me
		}

		key->key = name; // restore old cascading name
		return found;
	}

	if ((options & KDB_O_NOALL)
		// || (options & KDB_O_NOCASE)
		// || (options & KDB_O_WITHOWNER)
		) // binary search with nocase won't work
	{
		Key *current;
		if (!(options & KDB_O_NOALL)) ksRewind(ks);
		while ((current=ksNext(ks)) != 0)
		{
			if ((options & KDB_O_WITHOWNER) && (options & KDB_O_NOCASE))
			{
				if (!keyCompareByNameOwnerCase(&key, &current)) break;
			}
			else if (options & KDB_O_WITHOWNER)
			{
				if (!keyCompareByNameOwner(&key, &current)) break;
			}
			else if (options & KDB_O_NOCASE)
			{
				if (!keyCompareByNameCase(&key, &current)) break;
			}
			else if (!keyCompareByName(&key, &current)) break;
		}
		if (options & KDB_O_DEL) keyDel (key);
		if (current == 0) ksSetCursor (ks, cursor);
		return current;
	} else {
		Key ** found;
		size_t jump = 0;
		/*If there is a known offset in the beginning jump could be set*/
		if ((options & KDB_O_WITHOWNER) && (options & KDB_O_NOCASE))
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByNameOwnerCase);
		else if (options & KDB_O_WITHOWNER)
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCmpByNameOwner);
		else if (options & KDB_O_NOCASE)
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByNameCase);
		else
		found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
			sizeof (Key *), keyCmpByName);
		if (options & KDB_O_DEL) keyDel (key);
		if (found)
		{
			cursor = found-ks->array;
			if (options & KDB_O_POP)
			{
				return ksPopAtCursor(ks, cursor);
			} else {
				ksSetCursor(ks, cursor);
				return (*found);
			}
		} else {
			/*Reset Cursor to old position*/
			ksSetCursor(ks, cursor);
			return 0;
		}
	}
}

/**
 * Convenience method to look for a Key contained in @p ks that matches @p name.
 *
 * @deprecated Do not use this method because it needs to allocate a Key
 * internally. Prefer to use ksLookup() where you can control the
 * minimal allocations of keys.
 *
 * @p ksLookupByName() is designed to let you work with
 * entirely pre-loaded KeySets, so instead of kdbGetKey(), key by key, the
 * idea is to fully kdbGetByName() for your application root key and
 * process it all at once with @p ksLookupByName().
 *
 * This function is very efficient by using binary search. Together with
 * kdbGetByName() which can you load the whole configuration with only
 * some communication to backends you can write very effective but short
 * code for configuration.
 *
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor will not move, and a NULL pointer is
 * returned.
 * If requested to pop the key, the cursor will be rewinded.
 *
 * @section cascading Cascading
 *
 * Cascading is done if the first character is a /. This leads to ignoring
 * the prefix like system/ and user/.
 * @code
if (kdbGet(handle, "user/sw/myapp/current", myConfig, parentKey ) == -1)
	errorHandler ("Could not get Keys", parentKey);

if (kdbGet(handle, "system/sw/myapp/current", myConfig, parentKey ) == -1)
	errorHandler ("Could not get Keys", parentKey);

if ((myKey = ksLookupByName (myConfig, "/myapp/current/key", 0)) == NULL)
	errorHandler ("Could not Lookup Key");
 * @endcode
 *
 * This is the way multi user Programs should get there configuration and
 * search after the values. It is guaranteed that more namespaces can be
 * added easily and that all values can be set by admin and user.
 *
 * It is up to the application to implement a sophisticated cascading
 * algorithm, for e.g. a list of profiles (specific, group and fallback):
 * @code
if ((myKey = ksLookupByName (myConfig, "/myapp/current/specific/key", 0)) == NULL)
	if ((myKey = ksLookupByName (myConfig, "/myapp/current/group/key", 0)) == NULL)
		if ((myKey = ksLookupByName (myConfig, "/myapp/current/fallback/key", 0)) == NULL)
			errorHandler ("All fallbacks failed to lookup key");
 * @endcode
 *
 * Note that for every profile both the user and the system key are
 * searched. The first key found will be used.
 *
 * @section fullsearch Full Search
 *
 * When KDB_O_NOALL is set the keyset will be only searched from ksCurrent()
 * to ksTail(). You need to ksRewind() the keyset yourself. ksCurrent() is
 * always set properly after searching a key, so you can go on searching
 * another key after the found key.
 *
 * When KDB_O_NOALL is not set the cursor will stay untouched and all keys
 * are considered. A much more efficient binary search will be used then.
 * 
 * @param ks where to look for
 * @param name key name you are looking for
 * @param options some @p KDB_O_* option bits:
 * 	- @p KDB_O_NOCASE @n
 * 		Lookup ignoring case.
 * 	- @p KDB_O_WITHOWNER @n
 * 		Also consider correct owner.
 * 	- @p KDB_O_NOALL @n
 * 		Only search from ksCurrent() to end of keyset, see above text.
 * 	- @p KDB_O_POP @n
 * 		Pop the key which was found.
 *
 * 	Currently no options supported.
 * @return pointer to the Key found, 0 otherwise
 * @return 0 on NULL pointers
 * @see keyCompare() for very powerful Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByName(KeySet *ks, const char *name, option_t options)
{
	Key * key=0;
	Key * found=0;

	if (!ks) return 0;
	if (!name) return 0;

	if (! ks->size) return 0;

	key = keyNew (name, KDB_O_CASCADING_NAME, KEY_END);
	if (!key) return 0;
	found = ksLookup(ks, key, options);
	keyDel (key);
	return found;
}


/*
 * Lookup for a Key contained in @p ks KeySet that matches @p value,
 * starting from ks' ksNext() position.
 *
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor won't move, and a NULL pointer is
 * returned.
 *
 * This method skips binary keys.
 *
 * @par Example:
 * @code
ksRewind(ks);
while (key=ksLookupByString(ks,"my value",0))
{
	// show all keys which value="my value"
	keyToStream(key,stdout,0);
}
 * @endcode
 *
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param options some @p KDB_O_* option bits. Currently supported:
 * 	- @p KDB_O_NOALL @n
 * 		Only search from ksCurrent() to end of keyset, see ksLookup().
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 * @return the Key found, 0 otherwise
 * @see ksLookupByBinary()
 * @see keyCompare() for very powerful Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByString(KeySet *ks, const char *value, option_t options)
{
	cursor_t init=0;
	Key *current=0;

	if (!ks) return 0;

	if (!(options & KDB_O_NOALL))
	{
		ksRewind (ks);
		init=ksGetCursor(ks);
	}

	if (!value) return 0;

	while ((current=ksNext(ks)) != 0)
	{
		if (!keyIsString(current)) continue;

		/*fprintf (stderr, "Compare %s with %s\n", keyValue(current), value);*/

		if ((options & KDB_O_NOCASE) && 
			!elektraStrCaseCmp(keyValue(current),value)) break;
		else if (!strcmp(keyValue(current),value)) break;
	}

	/* reached end of KeySet */
	if (!(options & KDB_O_NOALL))
	{
		ksSetCursor (ks, init);
	}

	return current;
}



/*
 * Lookup for a Key contained in @p ks KeySet that matches the binary @p value,
 * starting from ks' ksNext() position.
 *
 * If found, @p ks internal cursor will be positioned in the matched key.
 * That means it is also accessible by ksCurrent(). A pointer to the Key
 * is returned. If not found, @p ks internal cursor won't move, and a 
 * NULL pointer is returned.
 *
 * This method skips string keys.
 *
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param size the size of @p value
 * @param options some @p KDB_O_* option bits:
 * 	- @p KDB_O_NOALL @n
 * 		Only search from ksCurrent() to end of keyset, see above text.
 * @return the Key found, NULL otherwise
 * @return 0 on NULL pointer
 * @see ksLookupByString()
 * @see keyCompare() for very powerful Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByBinary(KeySet *ks, const void *value, size_t size,
		option_t options)
{
	cursor_t init=0;
	Key *current=0;

	if (!ks) return 0;

	if (!(options & KDB_O_NOALL))
	{
		ksRewind (ks);
		init=ksGetCursor(ks);
	}

	while ((current=ksNext(ks)))
	{
		if (!keyIsBinary(current)) continue;

		if (size != current->dataSize) continue;

		if (!value)
		{
			if (!current->data.v) break;
			else continue;
		}

		if (current->data.v && 
			!memcmp(current->data.v,value,size)) break;
	}

	/* reached end of KeySet */
	if (!(options & KDB_O_NOALL))
	{
		ksSetCursor (ks, init);
	}

	return 0;
}



/******************************************* 
 *    Other operations                     *
 *******************************************/




/**
 * @internal
 *
 * Calculates the common parent to all keys in @p ks.
 *
 * This is a c-helper function, you need not implement it in bindings.
 *
 * Given the @p ks KeySet, calculates the parent name for all the keys.
 * So if @p ks contains this keys:
 *
 * @code
 *   system/sw/xorg/Monitors/Monitor1/vrefresh
 *   system/sw/xorg/Monitors/Monitor1/hrefresh
 *   system/sw/xorg/Devices/Device1/driver
 *   system/sw/xorg/Devices/Device1/mode
 * @endcode
 *
 * The common parent is @p system/sw/xorg .
 *
 * On the other hand, if we have this KeySet:
 *
 * @code
 *   system/some/thing
 *   system/other/thing
 *   user/unique/thing
 * @endcode
 *
 * No common parent is possible, so @p returnedCommonParent will contain nothing.
 *
 * @param working the Keyset to work with
 * @param returnedCommonParent a pre-allocated buffer that will receive the
 *        common parent, if found
 * @param maxSize size of the pre-allocated @p returnedCommonParent buffer
 * @return size in bytes of the parent name, or 0 if there is no common parent,
 *         or -1 to indicate an error, then @p errno must be checked.
 */
ssize_t ksGetCommonParentName(const KeySet *working,char *returnedCommonParent, size_t maxSize)
{
	size_t parentSize=0;
	Key *current=0;
	cursor_t init;
	KeySet *ks;
	ssize_t sMaxSize;

	if (maxSize > SSIZE_MAX) return -1;
	sMaxSize = maxSize;

	init = ksGetCursor (working);
	ks = (KeySet *) working;

	if (ks->size < 1) return 0;

	ksRewind(ks);
	current = ksNext(ks);
	if (keyGetNameSize(current) > sMaxSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		returnedCommonParent[0]=0;
		return -1;
	}

	strcpy(returnedCommonParent,keyName(current));
	parentSize=elektraStrLen(returnedCommonParent);

	while (*returnedCommonParent)
	{
		ksRewind(ks);
		while ((current = ksNext(ks)) != 0)
		{
			/* Test if a key doesn't match */
			if (memcmp(returnedCommonParent,keyName(current),parentSize-1)) break;
		}
		if (current)
		{
			/* some key failed to be a child */
			/* parent will be the parent of current parent... */
			char *delim=0;

			if ((delim=strrchr(returnedCommonParent,KDB_PATH_SEPARATOR)))
			{
				*delim=0;
				parentSize=elektraStrLen(returnedCommonParent);
			} else {
				*returnedCommonParent=0;
				parentSize=0;
				break; /* Better don't make comparision with parentSize-1 now */
			}
		} else {
			/* All keys matched (current==0) */
			/* We have our common parent to return in commonParent */
			ksSetCursor (ks, init );
			return parentSize;
		}
	}
	ksSetCursor (ks, init );
	return parentSize; /* if reached, will be zero */
}



/*********************************************************************
 *                Data constructors (protected)                      *
 *********************************************************************/


/**
 * @internal
 *
 * Resize keyset.
 *
 * For internal useage only.
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
 * @return 1 on success
 * @return 0 on nothing done because keyset would be too small.
 * @return -1 if alloc is smaller then current size of keyset.
 * @return -1 on memory error or null ptr
 */
int ksResize (KeySet *ks, size_t alloc)
{
	if (!ks) return -1;

	alloc ++; /* for ending null byte */
	if (alloc == ks->alloc) return 1;
	if (alloc < ks->size) return 0;
	if (alloc < KEYSET_SIZE)
	{
		if (ks->alloc != KEYSET_SIZE) alloc = KEYSET_SIZE;
		else return 0;
	}

	if (ks->array == NULL)
	{	/* Not allocated up to now */
		ks->alloc = alloc;
		ks->size = 0;
		ks->array = elektraMalloc (sizeof(struct _Key *) * ks->alloc);
		if (!ks->array)
		{
			/*errno = KDB_ERR_NOMEM;*/
			return -1;
		}
	}
/* This is much too verbose
#if DEBUG && VERBOSE
	printf ("Resize from %d to %d\n",(int) ks->alloc,(int) alloc);
#endif
*/
	ks->alloc=alloc;


	if (elektraRealloc((void**) &ks->array, sizeof(struct _Key *) * ks->alloc) == -1)
	{
#if DEBUG
		fprintf (stderr, "Reallocation error\n");
#endif
		elektraFree (ks->array);
		ks->array = 0;
		/*errno = KDB_ERR_NOMEM;*/
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
size_t ksGetAlloc (const KeySet *ks)
{
	return ks->alloc-1;
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
 * @return 1 on success
 */
int ksInit(KeySet *ks)
{
	ks->array = 0;

	ks->size=0;
	ks->alloc=0;
	ks->flags=0;

	ksRewind(ks);

	return 1;
}


/**
 * @internal
 *
 * KeySet object initializer.
 *
 * @see ksDel(), ksNew(), keyInit()
 * @return 1 on success
 */
int ksClose(KeySet *ks)
{
	Key * k;

	ksRewind(ks);
	while ((k = ksNext(ks)) != 0)
	{
		keyDecRef (k);
		keyDel (k);
	}

	if (ks->array) elektraFree (ks->array);
	ks->array = 0;
	ks->alloc = 0;

	ks->size = 0;

	return 0;
}


/**
 * @}
 */

