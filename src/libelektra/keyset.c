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


/**
 * @defgroup keyset KeySet :: Class Methods
 * @brief Methods to manipulate KeySets.
 *
 * A KeySet is a unsorted set of keys.
 *
 * Terminate with ksNew(0) or ksNew(20, ..., KS_END)
 * This is because there is a list of Key* required and
 * KS_END has the length of (Key*).
 *
 * It can be implemented in various ways like a linked list or with a
 * dynamically allocated array.
 *
 * With ksNew() you can create a new KeySet.
 *
 * You can add keys with ksAppendKey() in the keyset. ksGetSize() tells you
 * the current size of the keyset.
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
while ((current=ksNext(myConfig))!=0) {
	printf("Key name is %s.\n", keyName (current));
}
ksDel (myConfig); // delete keyset and all keys appended
 * @endcode
 *
 * @{
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
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

#include "kdbbackend.h"



/**
 * Allocate, initialize and return a new KeySet object.
 *
 * Objects created with ksNew() must be destroyed with ksDel().
 *
 * You can use a various long list of parameters to preload the keyset
 * with a list of keys. Either your first and only parameter is 0 or
 * your last parameter must be KEY_END.
 *
 * For most uses
 * @code
KeySet *keys = ksNew(0);
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
 * application that you normally need about 12 up to 14 keys), use:
 * @code
KeySet * keys = ksNew (15, KS_END);
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
 * Due to ABI compatibility, the @p KeySet structure is only declared in kdb.h,
 * and not defined. So you can only declare @p pointers to @p KeySets in your
 * program.
 * See http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html#AEN135
 *
 * @see ksDel() to free the keyset afterwards
 * @see ksDup() to duplicate an existing keyset
 * @param alloc gives a hint for the size how many Keys may be stored initially
 * @return a ready to use KeySet object
 * @return 0 on memory error
 */
KeySet *ksNew(size_t alloc, ...) {
	KeySet *ks;
	va_list va;

	if (alloc) va_start(va, alloc);
	ks = ksVNew (alloc, va);
	if (alloc) va_end (va);

	return ks;
}

KeySet *ksVNew (size_t alloc, va_list va)
{
	KeySet *keyset=0;
	Key *key = 0;

	keyset= (KeySet *)kdbiMalloc(sizeof(KeySet));
	if (!keyset)
	{
		/*errno = KDB_ERR_NOMEM;*/
		return 0;
	}
	ksInit(keyset);

	if (alloc < KEYSET_SIZE) keyset->alloc=KEYSET_SIZE;
	else keyset->alloc=alloc;

	keyset->array = kdbiMalloc (sizeof(struct _Key *) * keyset->alloc);
	if (!keyset->array)
	{
		/*errno = KDB_ERR_NOMEM;*/
		return 0;
	}
	keyset->array[0] = 0;
	

	if (alloc) {
		key = (struct _Key *) va_arg (va, struct _Key *);
		while (key) {
			ksAppendKey(keyset, key);
			key = (struct _Key *) va_arg (va, struct _Key *);
		}

	}

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
 * @param source has to be an initializised source KeySet
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
 * need to be  ksDel().
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
	kdbiFree(ks);

	return rc;
}

/*
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
		if (kdbiRealloc ((void**) &ks->array, sizeof(struct _Key *) * KEYSET_SIZE) == -1)
		{
			/*errno = KDB_ERR_NOMEM;*/
			kdbiFree (ks->array);
			ks->array = 0;
			ks->size = 0;
			return -1;
		}
	} else {
		if ((ks->array = kdbiMalloc (sizeof(struct _Key *) * KEYSET_SIZE)) == 0)
		{
			/*errno = KDB_ERR_NOMEM;*/
			ks->size = 0;
			return -1;
		}
	}
	ks->alloc = KEYSET_SIZE;


	return 0;
}



/*
 * Returns if keyset needs sort.
 *
 * It is very inefficient to resort the keyset every time when a
 * key or a keyset is appended, so only a flag will be set to
 * show that the keyset is not sorted any more.
 *
 * Before operations where the code depends on a sorted keyset,
 * namely in kdbGet(), kdbSet() and ksLookup(), this simple code
 * sorts when needed:
 *
 * @code
if (ksNeedSort(ks)) ksSort(ks);
 * @endcode
 *
 * @warning ksNeedSort() does not track every change which actually
 * affects sorting: changing of names with keySetName() and
 * marking keys remove with keyRemove() won't be recognized.
 * But any ks* Method will leave the KeySet sorted or
 * set the flag.
 *
 * So if your code might rename keys or flag them to remove,
 * make sure that you use
 *
 * @code
ksSort(ks);
 * @endcode
 *
 * somewhere afterwards before using ksLookup(). Otherwise the
 * renamed key won't be found.
 *
 * kdbSet() will have no problems with that issue, because it
 * duplicates its keysets using ksDup().
 *
 * @param ks the keyset object to work with
 * @see ksAppendKey(), ksAppend() and ksSort()
 * @return 1 if sort is needed
 *  0 if the keyset is sorted
 */
int ksNeedSort (const KeySet *ks)
{
	return (ks->flags & KS_FLAG_DIRTY) == KS_FLAG_DIRTY;
}


/* Used as a callback by the qsort() function */
static int keyCompareWithRemove(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);
	int ret = strcmp(name1, name2);

	/* one key remove, sort in order of KEY_FLAG_REMOVE */
	if (keyNeedRemove(key1) && !keyNeedRemove(key2)) return -1;
	else if (!keyNeedRemove(key1) && keyNeedRemove(key2)) return 1;


	if (keyNeedRemove(key1) && keyNeedRemove(key2))
	{	/* both keys remove, sort reverse */
		if (ret < 0) return 1;
		else if (ret > 0) return -1;
		else return 0;
	} else { /* sort by owner */
		if (ret == 0)
		{
			const char *owner1 = keyOwner(key1);
			const char *owner2 = keyOwner(key2);
			return strcmp(owner1, owner2);
		}
		else return ret;
	}
}


/**
 * Sorts a KeySet alphabetically by Key name.
 *
 * You need ksSort() only in few cases directly.
 *
 * @section sortlookup Don't need to sort before using ksLookup
 *
 * You don't need it if you just just kdbGet() and subsequent
 * ksLookup().
 * @code
KeySet *ks = ksNew(0);
kdbGet(h, ks, k, 0);
// ksPop(), ksAppend() and ksAppendKey() allowed here
ksLookup(ks, s, 0); // you dont need to sort ks
 * @endcode
 *
 * This is because the KeySet tracks if it needs to be sorted
 * and ksLookup() will sort when needed.
 *
 * @section sortiterate Sort when iterating
 *
 * Before you iterate over a keyset you have to sort it, if
 * you need it in a sorted way.
 *
 * To achieve that you can pass option_t::KDB_O_SORT to kdbGet()
 * and kdbSet(). Then you will receive a already sorted keyset
 * over which you can iterate.
 * @code
KeySet *ks = ksNew(0);
kdbGet(h, ks, k, KDB_O_SORT);
// no changes to keyset allowed
ksRewind(ks);
// now you can iterate over a sorted keyset
 * @endcode
 *
 * Its of course also possible to use ksLookup() once, because it
 * will sort the keyset too.
 *
 * @section sortkey Sort when changing key
 *
 * @warning You should not use keySetName() or keyRemove() when a
 * key belongs to a keyset. When you are doing this, you always need to @p manually
 * sort @p all keysets where the key was before using ksLookup() (otherwise ksLookup()
 * won't find that keys), kdbGet() or kdbSet() methods.
 *
 * When you change a key's name or its remove status the order
 * which was previously correctly is then wrong. The keyset
 * does not recognize this, so the programmer has to take care
 * that ksSort() is called before any operation which needs
 * a sorted keyset (which are all ksLookup(), all kdbGet()
 * and all kdbSet() functions).
 *
 * @note You can remember that easily that all functions which get options
 * require one of the following:
 * - that you did not manipulate a keys name or a remove status
 * - that you pass KDB_O_SORT when you know that you manipulated at least one key
 * - that you ksSort() yourself after manipulating keys
 *
 * @section dirty Dirty KeySet
 *
 * When you use ksAppend(), ksAppendKey(), ksPop() the keyset is dirty
 * afterwards, which means that it needs to be sorted. This is done
 * automatically using a ksLookup() method and in ksGet() or ksSet()
 * (All methods which accept options).
 *
 * It won't be done if you just iterate over the keyset, so you might
 * use a ksLookup() or ksSort() first. ksLookup() will be more efficient
 * in that case, because it will only sort when needed. Don't pass
 * KDB_O_NOALL (it will deactivate the sorting feature),
 * see ksLookup() for more information.
 *
 * @param ks KeySet to be sorted
 * @see kdbGet(), kdbSet(), ksLookup() for some functions which may
 *     need sorting before they are called. (All functions which take
 *     options as arguments)
 * @see keySetName(), keySetBaseName(), keyAddBaseName() and keyRemove()
 *     for all methods which change the sorting state where the keyset
 *     can't track the change.
 * @see ksAppend(), ksAppendKey(), ksPop() for all methods which make
 *     a keyset dirty.
 */
void ksSort(KeySet *ks)
{
	if (!ks) return;

	ks->flags = ~KS_FLAG_DIRTY & ks->flags;
	if (! ks->size) return;

	qsort(ks->array,ks->size,sizeof(Key *),keyCompareWithRemove);
}




/**
 * Return the number of keys that @p ks contains.
 *
 * @param ks the keyset object to work with
 * @return the number of keys that @p ks contains.
 * @return -1 on NULL pointer
 * @see ksNew(0), ksDel()
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
 * Appends a Key to the end of @p ks.
 *
 * A pointer to the key will
 * be stored, and not a private copy. So a future ksDel() on
 * @p ks may keyDel() the @p toAppend object, see keyGetRef().
 *
 * The reference counter of the key will be incremented, and
 * thus toAppend is not const.
 *
 * The KeySet internal cursor is not moved.
 *
 * Makes the keyset dirty, see ksSort().
 *
 * @return the size of the KeySet after insertion
 * @return -1 on NULL pointers
 * @param ks KeySet that will receive the key
 * @param toAppend Key that will be appended to ks
 * @see ksInsert(), ksInsertKeys(), ksAppend(), keyNew(), ksDel()
 * @see keyIncRef()
 *
 */
ssize_t ksAppendKey(KeySet *ks, Key *toAppend)
{
	if (!ks) return -1;
	if (!toAppend) return -1;

	ks->flags |= KS_FLAG_DIRTY;
	++ ks->size;
	if (keyNeedRemove (toAppend)) ++ ks->rsize;
	if (ks->size >= ks->alloc) ksResize (ks, ks->alloc * 2);
	keyIncRef (toAppend);
	ks->array[ks->size-1] = toAppend;
	ks->array[ks->size] = 0;
	return ks->size;
}



/**
 * Append all @p toAppend contained keys to the end of the @p ks.
 *
 * @p toAppend KeySet will be left unchanged.
 *
 * Makes the keyset dirty, see ksSort().
 *
 * @return the size of the KeySet after transfer
 * @return -1 on NULL pointers
 * @param ks the KeySet that will receive the keys
 * @param toAppend the KeySet that provides the keys that will be transfered
 * @see ksAppendKey(), ksInsert(), ksInsertKeys()
 * 
 */
ssize_t ksAppend(KeySet *ks, const KeySet *toAppend)
{
	size_t oldSize = 0;
	int i = 0;
	int toAlloc = 0;

	if (!ks) return -1;
	if (!toAppend) return -1;

	oldSize = ks->size;
	toAlloc = ks->alloc;

	if (toAppend->size <= 0) return ks->size;
	ks->flags |= KS_FLAG_DIRTY;
	ks->size += toAppend->size;
	ks->rsize += toAppend->rsize;
	while (ks->size >= toAlloc) toAlloc *= 2;
	ksResize (ks, toAlloc);
	for (i=0; i<toAppend->size; i++) keyIncRef(toAppend->array[i]);
	memcpy (ks->array + oldSize, toAppend->array, toAppend->size * sizeof (Key *));
	ks->array[ks->size] = 0;
	return ks->size;
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
ks1=ksNew(0);
ks2=ksNew(0);

k1=keyNew(0); // ref counter 0
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

	if (ks->size <= 0) return 0;
	-- ks->size;
	if (ks->size+1 < ks->alloc/2) ksResize (ks, ks->alloc / 2);
	ret = ks->array[ks->size];
	ks->array[ks->size] = 0;
	keyDecRef(ret);

	return ret;
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
while ((key = keyNext (ks))!=0) {}
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
 * On subsequent calls of ksNext it will still return the NULL pointer.
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
 * @see kdbMonitorKeys() for a usage example
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
while ((key = keyNext (ks))!=0)
{
	// now mark this key
	jump = ksGetCursor(ks);

	//code..
	keyNext (ks); // now browse on
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
while ((key = keyNext (ks))!=0) {}
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




/*
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
static Key *ksPrev(KeySet *ks)
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




/******************************************* 
 *    Looking up Keys inside KeySets       *
 *******************************************/

static int keyCompareByName(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);

	return strcmp(name1, name2);
}

static int keyCompareByNameCase(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);

	return kdbiStrCaseCmp(name1, name2);
}

static int keyCompareByNameOwner(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);
	int result = strcmp(name1, name2);

	if (result == 0)
	{
		const char *owner1 = keyOwner(key1);
		const char *owner2 = keyOwner(key2);
		return strcmp(owner1, owner2);
	}
	else return result;
}


static int keyCompareByNameOwnerCase(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);
	int result = kdbiStrCaseCmp(name1, name2);

	if (result == 0)
	{
		const char *owner1 = keyOwner(key1);
		const char *owner2 = keyOwner(key2);
		return kdbiStrCaseCmp(owner1, owner2);
	}
	else return result;
}


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
                ErrorHandler ("Could not get Keys");

        if (kdbGet(handle, "system/myapp", myConfig, 0 ) == -1)
                ErrorHandler ("Could not get Keys");

        if ((myKey = ksLookup(myConfig, key, 0)) == NULL)
                ErrorHandler ("Could not Lookup Key");
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
 * So if you change keys, e.g. rename (keySetName()) or remove (keyRemove()) them
 * make sure to sort the keyset with ksSort(). When the keyset is dirty,
 * see ksNeedSort() it will be sorted automatically when needed.
 *
 * @subsection KDB_O_POP
 *
 * When KDB_O_POP is set the key which was found will be ksPop()ed. ksCurrent()
 * will not be changed, only iff ksCurrent() is the searched key, then the keyset
 * will be ksRewind()ed.
 *
 * @note Note that keyRemove() keys won't be found after the first time the keyset
 * is resorted. Lookup automatically sorts the keyset, if needed, but it
 * can't find it out when only keys are changed, not the keyset.
 *
 * @note Like in ksPop() the popped key always needs to be keyDel() afterwards, even
 * if it is appended to another keyset.
 *
 * @warning All cursors on the keyset will be invalid
 * iff you use KDB_O_POP, so don't use this if you rely on a cursor, see ksGetCursor().
 *
 * @note Never use ksLookup() with KDB_O_POP and ksAppendKey() or ksAppend() together in a loop.
 * Otherwise ksLookup() will need to resort the keyset every iteration and spend 99.96% of the
 * time in ksSort() (benchmarked with above 500k iterations).
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
 * 	- @p KDB_O_SORT @n
 * 		Force sorting before searching, see ksSort().
 * 		Together with KDB_O_NOALL the search will start from beginning.
 * @return pointer to the Key found, 0 otherwise
 * @return 0 on NULL pointers
 * @see ksLookupByName() to search by a name given by a string
 * @see ksCurrent(), ksRewind(), ksNext() for iterating over a keyset
 * @see ksSort() to understand how keyset sort themself
 */
Key *ksLookup(KeySet *ks, Key * key, option_t options)
{
	Key *current;
	Key ** found;
	cursor_t cursor = 0;
	size_t jump = 0;

	if (!ks) return 0;

	jump = ks->rsize;
	cursor = ksGetCursor (ks);

	if (options & KDB_O_SORT)
	{
		ksSort (ks);
		ksRewind (ks);
	}
	else if (!(options & KDB_O_NOALL) && ks->flags & KS_FLAG_DIRTY) ksSort(ks);

	if (!key) return 0;

	if (options & KDB_O_NOALL)
	{
		while ((current=ksNext(ks)) != 0) {
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
		if ((options & KDB_O_WITHOWNER) && (options & KDB_O_NOCASE))
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByNameOwnerCase);
		else if (options & KDB_O_WITHOWNER)
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByNameOwner);
		else if (options & KDB_O_NOCASE)
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByNameCase);
		else
			found = (Key **) bsearch (&key, ks->array+jump, ks->size-jump,
				sizeof (Key *), keyCompareByName);
		if (options & KDB_O_DEL) keyDel (key);
		if (found)
		{
			if (options & KDB_O_POP)
			{
				Key * k = *found;
				/* Move the array over the place where key was found */
				memmove (found, found+1, ks->size*sizeof(Key *)-(found-ks->array)-sizeof(Key *));
				*(ks->array+ks->size-1) = k;
				if (found < ks->array+ks->current)
				{
					ksPrev(ks);
				}
				else if (found == ks->array+ks->current)
				{
					ksRewind(ks);
				}
				return ksPop(ks);
			} else {
				cursor = found-ks->array;
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
 * Look for a Key contained in @p ks that matches @p name.
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
 *
 * @section cascading Cascading
 *
 * Cascading is done if the first character is a /. This leads to ignoring
 * the prefix like system/ and user/.
 * @code
        if (kdbGetByName(handle, "/sw/myapp/current", myConfig, 0 ) == -1)
                ErrorHandler ("Could not get Keys");

        if ((myKey = ksLookupByName (myConfig, "/myapp/current/key", 0)) == NULL)
                ErrorHandler ("Could not Lookup Key");
 * @endcode
 * 
 * This is the way multi user Programs should get there configuration and
 * search after the values. It is guaranteed that more namespaces can be
 * added easily and that all values can be set by admin and user.
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
 * 	- @p KDB_O_SORT @n
 * 		Force sorting before searching, see ksSort().
 * 		Together with KDB_O_NOALL the search will start from beginning.
 *
 * 	Currently no options supported.
 * @return pointer to the Key found, 0 otherwise
 * @return 0 on NULL pointers
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByName(KeySet *ks, const char *name, option_t options)
{
	Key * key=0;
	Key * found=0;
	char * newname=0;

	if (!ks) return 0;
	if (!name) return 0;

	if (! ks->size) return 0;

	if (name[0] == '/')
	{
		/* Will be freed by second key */
		newname = kdbiMalloc (strlen (name) + sizeof ("system") + 1);
		if (!newname)
		{
			/*errno = KDB_ERR_NOMEM;*/
			return 0;
		}
		strncpy (newname+2, "user",4);
		strcpy  (newname+6, name);
		key = keyNew (newname+2, KEY_END);
		found = ksLookup(ks, key, options);
		keyDel (key);

		if (!found)
		{
			strncpy (newname, "system",6);
			key = keyNew (newname, KEY_END);
			found = ksLookup(ks, key, options);
			keyDel (key);
		}

		kdbiFree (newname);
		return found;
	} else {
		key = keyNew (name, KEY_END);
		if (!key) return 0;
		found = ksLookup(ks, key, options);
		keyDel (key);
		return found;
	}
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
while (key=ksLookupByString(ks,"my value",0)) {
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
 * 	- @p KDB_O_SORT @n
 * 		Force sorting before searching, see ksSort().
 * 		Together with KDB_O_NOALL the search will start from beginning.
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 * @return the Key found, 0 otherwise
 * @see ksLookupByBinary()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByString(KeySet *ks, const char *value, option_t options)
{
	cursor_t init=0;
	Key *current=0;

	if (!ks) return 0;

	if (options & KDB_O_SORT)
	{
		ksSort (ks);
		ksRewind (ks);
	}
	else if (!(options & KDB_O_NOALL) && ks->flags & KS_FLAG_DIRTY) ksSort(ks);

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
			!kdbiStrCaseCmp(keyValue(current),value)) break;
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
 * 	- @p KDB_O_SORT @n
 * 		Force sorting before searching, see ksSort().
 * 		Together with KDB_O_NOALL the search will start from beginning.
 * @return the Key found, NULL otherwise
 * @return 0 on NULL pointer
 * @see ksLookupByString()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByBinary(KeySet *ks, const void *value, size_t size,
		option_t options)
{
	cursor_t init=0;
	Key *current=0;

	if (!ks) return 0;

	if (options & KDB_O_SORT)
	{
		ksSort (ks);
		ksRewind (ks);
	}
	else if (!(options & KDB_O_NOALL) && ks->flags & KS_FLAG_DIRTY) ksSort(ks);

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
			if (!current->data) break;
			else continue;
		}

		if (current->data && 
			!memcmp(current->data,value,size)) break;
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




/*
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
 * This method will work correctly only on @link ksSort() sorted KeySets @endlink.
 *
 * @param working the Keyset to work with
 * @param returnedCommonParent a pre-allocated buffer that will receive the
 *        common parent, if found
 * @param maxSize size of the pre-allocated @p returnedCommonParent buffer
 * @return size in bytes of the parent name, or 0 if there is no common parent,
 *         or -1 to indicate an error, then @p errno must be checked.
 */
ssize_t ksGetCommonParentName(const KeySet *working,char *returnedCommonParent, size_t maxSize) {
	size_t parentSize=0;
	Key *current=0;
	cursor_t init;
	KeySet *ks;

	init = ksGetCursor (working);
	ks = (KeySet *) working;

	if (ks->size < 1) return 0;

	ksRewind(ks);
	current = ksNext(ks);
	if (keyGetNameSize(current) > maxSize) {
		/*errno=KDB_ERR_TRUNC;*/
		returnedCommonParent[0]=0;
		return -1;
	}

	strcpy(returnedCommonParent,keyName(current));
	parentSize=kdbiStrLen(returnedCommonParent);

	while (*returnedCommonParent) {
		ksRewind(ks);
		while ((current = ksNext(ks)) != 0) {
			/* Test if a key doesn't match */
			if (memcmp(returnedCommonParent,keyName(current),parentSize-1)) break;
		}
		if (current) {
			/* some key failed to be a child */
			/* parent will be the parent of current parent... */
			char *delim=0;

			if ((delim=strrchr(returnedCommonParent,PATH_SEPARATOR))) {
				*delim=0;
				parentSize=kdbiStrLen(returnedCommonParent);
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


/*
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
 * @return -1 on memory error
 */
int ksResize (KeySet *ks, size_t alloc)
{
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
		ks->array = kdbiMalloc (sizeof(struct _Key *) * ks->alloc);
		if (!ks->array)
		{
			/*errno = KDB_ERR_NOMEM;*/
			return -1;
		}
	}

#if DEBUG && VERBOSE
	printf ("Resize from %d to %d\n",(int) ks->alloc,(int) alloc);
#endif
	ks->alloc=alloc;


	if (kdbiRealloc((void**) &ks->array, sizeof(struct _Key *) * ks->alloc) == -1)
	{
#if DEBUG
		fprintf (stderr, "Reallocation error\n");
#endif
		kdbiFree (ks->array);
		ks->array = 0;
		/*errno = KDB_ERR_NOMEM;*/
		return -1;
	}

	return 1;
}

/*
 * Returns current allocation size.
 *
 * @param ks the keyset object to work with
 * @return allocated size*/
size_t ksGetAlloc (const KeySet *ks)
{
	return ks->alloc;
}



/*
 * KeySet object initializer.
 *
 * You should always use ksNew() instead of ksInit().
 *
 * Every KeySet object that will be used must be initialized first, to setup
 * pointers, counters, etc. After use, all ksInit()ialized KeySets must be
 * cleaned with ksClear().
 * 
 * @deprecated Thus you can't get a Keyset without ksNew, this function is useless.
 * @see ksNew(), ksClose(), keyInit()
 * @return 1 on success
 */
int ksInit(KeySet *ks) {
	ks->array = 0;
	ks->flags = KS_FLAG_DIRTY;

	ks->size=0;
	ks->rsize=0;
	ks->alloc=0;

	ksRewind(ks);

	return 1;
}


/*
 * KeySet object initializer.
 *
 * @deprecated Thus you can't get a Keyset without ksNew, this function is useless.
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

	if (ks->array) kdbiFree (ks->array);
	ks->array = 0;
	ks->alloc = 0;

	ks->size = 0;
	ks->rsize= 0;

	return 0;
}


/**
 * @}
 */

