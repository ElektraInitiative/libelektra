/***************************************************************************
            split.c  -  Functions for splitting keysets for kdbSet
                             -------------------
    begin                : Fri 21 Mar 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


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

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <kdbinternal.h>

/**
 * @defgroup split Split :: Represents splitted keysets
 * @brief used internally for kdbSet()
 *
 * Splits up a keyset into multiple keysets where each
 * of them will passed to the correct kdbSet().
**/


/**
 * Allocates a new split object.
 *
 * Initially the size is APPROXIMATE_NR_OF_BACKENDS.
 *
 * @return a fresh allocated split object
 * @ingroup split
 * @see elektraSplitDel()
**/
Split * elektraSplitNew(void)
{
	Split *ret = elektraCalloc (sizeof (Split));

	ret->size = 0;
	ret->alloc = APPROXIMATE_NR_OF_BACKENDS;

	ret->keysets=elektraCalloc(sizeof(KeySet*) * ret->alloc);
	ret->handles=elektraCalloc(sizeof(KDB *) * ret->alloc);
	ret->parents=elektraCalloc(sizeof(Key *) * ret->alloc);
	ret->syncbits=elektraCalloc(sizeof(int) * ret->alloc);

	return ret;
}


/**
 * Delete a split object.
 *
 * Will free all allocated resources of a splitted keyset.
 *
 * @param keysets the split object to work with
 * @ingroup split
 */
void elektraSplitDel(Split *keysets)
{
	for (size_t i=0; i<keysets->size; ++i)
	{
		ksDel (keysets->keysets[i]);
		keyDecRef (keysets->parents[i]);
		keyDel (keysets->parents[i]);
	}
	elektraFree (keysets->keysets);
	elektraFree (keysets->handles);
	elektraFree (keysets->parents);
	elektraFree (keysets->syncbits);
	elektraFree (keysets);
}

/**
 * Doubles the size of how many parts of keysets can be appended.
 *
 * @param ret the split object to work with
 * @ingroup split
 */
void elektraSplitResize(Split *split)
{
	split->alloc *= 2;

	elektraRealloc((void**) &split->keysets, split->alloc * sizeof(KeySet *));
	elektraRealloc((void**) &split->handles, split->alloc * sizeof(KDB *));
	elektraRealloc((void**) &split->parents, split->alloc * sizeof(Key *));
	elektraRealloc((void**) &split->syncbits, split->alloc * sizeof(int));
}

/**
 * Increases the size of split and initializes
 * the element at size-1 to be used.
 *
 * Will automatically resize split if needed.
 *
 * @param ret the split object to work with
 * @ingroup split
 */
void elektraSplitAppend(Split *split, Backend *backend, Key *parentKey, int syncbits)
{
	++ split->size;
	if (split->size > split->alloc) elektraSplitResize(split);

	split->keysets[split->size-1]=ksNew(0);
	split->handles[split->size-1]=backend;
	split->parents[split->size-1]=parentKey;
	split->syncbits[split->size-1]=syncbits;
}

/**
 * Determines if the backend is already inserted or not.
 *
 * @warning If no parent Key is given, the default/root backends won't
 * be searched.
 *
 * @param split the split object to work with
 * @param backend the backend to search for
 * @param parent the key to check for domains in default/root backends.
 * @return pos of backend if it already exist
 * @return -1 if it does not exist
 * @ingroup split
 */
ssize_t elektraSplitSearchBackend(Split *split, Backend *backend, Key *parent)
{
	/* TODO: possible optimization: use an index to find already inserted backends */
	for (size_t i=0; i<split->size; ++i)
	{
		if (backend == split->handles[i])
		{
			if (split->syncbits[i] & 2)
			{
				if ((keyIsUser(parent) == 1 && keyIsUser(split->parents[i]) == 1)  ||
				    (keyIsSystem(parent) == 1 && keyIsSystem(split->parents[i]) == 1))
				{
					return i;
				}
				continue;
			}
			/* We already have this backend, so leave */
			return i;
		}
	}
	return -1;
}


/**
 * @returns 1 if one of the backends in split has all
 *          keys below parentKey
 * @ingroup split
 */
int elektraSplitSearchRoot(Split *split, Key *parentKey)
{
	size_t splitSize = split->size;

	for (size_t i=0; i<splitSize; ++i)
	{
		if (keyRel (split->parents[i], parentKey) >= 0)
			return 1;
	}

	return 0;
}


/*Needed for recursive implementation*/
static int elektraSplitSearchTrie(Split *split, Trie *trie, Key *parentKey)
{
	int hasAdded = 0;
	int i;

	if (trie==NULL) return 0;

	for (i=0;i<MAX_UCHAR;i++)
	{
		if (trie->text[i]!=NULL)
		{
			Backend *cur = trie->value[i];
			hasAdded += elektraSplitSearchTrie(split, trie->children[i], parentKey);

			if (!cur)
			{
				/* We now might have the situation that the root backend is needed
				   additionally. In that case we could work here with the empty_value.
				   However we simply check afterwards if we need to add root/default
				   backend because of clearness. */
				continue;
			}
			if (keyRel(cur->mountpoint, parentKey) >= 0)
			{
				/* if (elektraSplitSearchBackend(split, cur, 0) >= 0) continue; */
				elektraSplitAppend(split, cur, keyDup(cur->mountpoint), 0);
				++hasAdded;
			}
		}
	}
	return hasAdded;
}


/**
 * Walks through the trie and adds all backends below parentKey.
 *
 * Sets syncbits to 2 if it is a default or root backend (which needs splitting).
 *
 * @pre split needs to be empty, directly after creation with elektraSplitNew().
 *
 * @pre there needs to be a valid defaultBackend
 *      but its ok not to have a trie inside KDB.
 *
 * @pre parentKey must be a valid key! (could be implemented more generally,
 *      but that would require splitting up of keysets of the same backend)
 *
 * @ingroup split
 * @return -1 on error
 * @return 1
 */
int elektraSplitBuildup (Split *split, KDB *handle, Key *parentKey)
{
	Trie *trie = handle->trie;
	Key *userKey = 0;
	Key *systemKey = 0;

	if (!parentKey)
	{
		userKey = keyNew("user", KEY_END);
		systemKey = keyNew("system", KEY_END);
	}

	if (!handle->trie)
	{
		Backend *defaultBackend = handle->defaultBackend;

		/* If parentKey is null it will be true for keyIsUser and keyIsSystem below */
		if (keyIsUser(parentKey))
		{
			elektraSplitAppend (split, defaultBackend, keyNew("user", KEY_VALUE, "default", KEY_END), 2);
		}

		if (keyIsSystem(parentKey))
		{
			elektraSplitAppend (split, defaultBackend, keyNew("system", KEY_VALUE, "default", KEY_END), 2);
		}

		if (!parentKey) goto finish;
		else return 1;
	}


	elektraSplitSearchTrie(split, trie, parentKey);

	/* We may have found something in the trie, but is it enough? */
	if (!parentKey)
	{
		/* Do we lack one (or two) of the root backends? */
		if (elektraSplitSearchRoot(split, userKey) == 0)
		{
			Backend *backend = elektraTrieLookup(trie, userKey);
			elektraSplitAppend (split, backend, keyNew("user", KEY_VALUE, "root", KEY_END), 2);
		}

		if (elektraSplitSearchRoot(split, systemKey) == 0)
		{
			Backend *backend = elektraTrieLookup(trie, systemKey);
			elektraSplitAppend (split, backend, keyNew("system", KEY_VALUE, "root", KEY_END), 2);
		}
		goto finish;
	} else {
		if (elektraSplitSearchRoot(split, parentKey) == 1) return 1;

		/* Seems like we are lacking a root backend (in the domain of parentKey) */

		Backend *backend = elektraTrieLookup(trie, parentKey);

		if (keyIsUser(parentKey))
		{
			elektraSplitAppend (split, backend, keyNew("user", KEY_VALUE, "root", KEY_END), 2);
		}

		if (keyIsSystem(parentKey))
		{
			elektraSplitAppend (split, backend, keyNew("system", KEY_VALUE, "root", KEY_END), 2);
		}
		return 1;
	}

	/* We have not found a root backend either -> not allowed */


finish:
	keyDel (userKey);
	keyDel (systemKey);
	return 1;
}



/**
 * Splits up the keysets and search for a sync bit.
 *
 * It does not check if there were removed keys,
 * see elektraSplitRemove() for the next step.
 *
 * It does not create new backends, this has to be
 * done by buildup before.
 *
 * @pre elektraSplitBuildup() need to be executed before.
 *
 * @return 0 if there were no sync bits
 * @return 1 if there were sync bits
 * @ingroup split
 */
int elektraSplitDivide (Split *split, KDB *handle, KeySet *ks)
{
	ssize_t curFound = 0; /* If key could be appended to any of the existing splitted keysets */
	int needsSync = 0;
	Key *curKey = 0;
	Backend *curHandle = 0;

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		curHandle = kdbGetBackend(handle, curKey);

		curFound = elektraSplitSearchBackend(split, curHandle, curKey);

		if (curFound == -1) continue;

		ksAppendKey (split->keysets[curFound], curKey);
		if (keyNeedSync(curKey) == 1)
		{
			split->syncbits[curFound] |= 1;
			needsSync = 1;
		}
	}

	return needsSync;
}

/** Add sync bits everywhere keys were removed.
 *
 * Only this function can really decide if sync is needed or not.
 *
 * @pre split needs to be processed with elektraSplitDivide() before.
 *
 * @return 0 if kdbSet() is not needed
 * @return 1 if kdbSet() is needed
 * @pre user/system was splitted before.
 * @ingroup split
 *
**/
int elektraSplitSync(Split *split)
{
	int needsSync = 0;

	for (size_t i=0; i<split->size; ++i)
	{
		if (split->syncbits[i] & 1)
		{
			needsSync = 1;
			continue;
		}

		if (!strncmp(keyName(split->parents[i]), "system", 6))
		{
			/* Check for system keyset for removed keys */
			if (split->handles[i]->systemsize != ksGetSize(split->keysets[i]))
			{
				split->syncbits[i] |= 1;
				needsSync = 1;
			}
		} else if (!strncmp(keyName(split->parents[i]), "user", 4)) {
			/* Check for user keyset for removed keys */
			if (split->handles[i]->usersize != ksGetSize(split->keysets[i]))
			{
				split->syncbits[i] |= 1;
				needsSync = 1;
			}
		}
	}

	return needsSync;
}

/** Prepares for kdbSet() mainloop afterwards.
 *
 * All splits which do not need sync are removed and a deep copy
 * of the remaining keysets is done.
 *
 * @param split the split object to work with
 * @ingroup split
 */
int elektraSplitPrepare (Split *split)
{
	return 0;
}

