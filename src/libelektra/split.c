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
 * Increases the size of split and appends a new empty keyset.
 *
 * Initializes the element with the given parameters
 * at size-1 to be used.
 *
 * Will automatically resize split if needed.
 *
 * @param split the split object to work with
 * @param backend the backend which should be appended
 * @param parentKey the parentKey which should be appended
 * @param syncbits the initial syncstate which should be appended
 * @ingroup split
 */
ssize_t elektraSplitAppend(Split *split, Backend *backend, Key *parentKey, int syncbits)
{
	++ split->size;
	if (split->size > split->alloc) elektraSplitResize(split);

	split->keysets[split->size-1]=ksNew(0);
	split->handles[split->size-1]=backend;
	split->parents[split->size-1]=parentKey;
	split->syncbits[split->size-1]=syncbits;

	return split->size-1;
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
 * @return 1 if one of the backends in split has all
 *          keys below parentKey
 * @return 0 if parentKey == 0 or there are keys below
 *          or same than parentKey which do not fit
 *          in any of splitted keysets
 * @ingroup split
 */
int elektraSplitSearchRoot(Split *split, Key *parentKey)
{
	size_t splitSize = split->size;

	if (!parentKey) return 0;

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

			if (!cur) continue;

			if (keyRel(cur->mountpoint, parentKey) >= 0)
			{
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
 * @return always 1
 */
int elektraSplitBuildup (Split *split, KDB *kdb, Key *parentKey)
{

	if (!parentKey || !parentKey->key)
	{
		parentKey = 0;
	}

	elektraSplitSearchTrie(split, kdb->trie, parentKey);

	/* If parentKey is null it will be true for keyIsUser and keyIsSystem below */
	if (keyIsUser(parentKey))
	{
		/* Do we lack one (or two) of the default backends? */
		if (elektraSplitSearchRoot(split, parentKey) == 0)
		{
			elektraSplitAppend (split, kdb->defaultBackend, keyNew("user", KEY_VALUE, "default", KEY_END), 2);
		}
	}

	if (keyIsSystem(parentKey))
	{
		if (elektraSplitSearchRoot(split, parentKey) == 0)
		{
			elektraSplitAppend (split, kdb->defaultBackend, keyNew("system", KEY_VALUE, "default", KEY_END), 2);
		}
	}

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
 * @return -1 if no backend was found for a key
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
		curHandle = elektraMountGetBackend(handle, curKey);
		if (!curHandle) return -1;

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


/**
 * Appoints all keys from ks to yet unsynced splits.
 *
 * @pre elektraSplitBuildup() need to be executed before.
 *
 * @return 1 on success
 * @return -1 if no backend was found for a key
 * @ingroup split
 */
int elektraSplitAppoint (Split *split, KDB *handle, KeySet *ks)
{
	ssize_t curFound = 0; /* If key could be appended to any of the existing splitted keysets */
	Key *curKey = 0;
	Backend *curHandle = 0;
	ssize_t defFound = elektraSplitAppend (split, 0, 0, 0);

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		curHandle = elektraMountGetBackend(handle, curKey);
		if (!curHandle) return -1;

		curFound = elektraSplitSearchBackend(split, curHandle, curKey);

		if (curFound == -1) curFound = defFound;

		if (split->syncbits[curFound] & 1)
		{
			continue;
		}

		ksAppendKey (split->keysets[curFound], curKey);
	}

	return 1;
}

/**
 * Does some work after getting of backends is finished.
 *
 * @pre elektraSplitAppoint() needs to be executed before.
 *
 * - check if keys are in correct backend
 * - remove syncbits
 * - update usersize and systemsize
 *
 * @return 1 on success
 * @return -1 if no backend was found for a key
 * @ingroup split
 */
int elektraSplitGet (Split *split, KDB *handle)
{
	Key *cur = 0;
	Backend *curHandle = 0;

	/* Dont iterate the default split part */
	for (size_t i=0; i<split->size-1; ++i)
	{
		if (!(split->syncbits[i] & 1))
		{
			/* Dont process keysets which come from the user
			   and not from the backends */
			continue;
		}

		ksRewind (split->keysets[i]);
		while ((cur = ksNext(split->keysets[i])) != 0)
		{
			curHandle = elektraMountGetBackend(handle, cur);
			if (!curHandle) return -1;
			if (curHandle != split->handles[i])
			{
				/* drop the key */
				keyDel (ksLookup(split->keysets[i], cur, KDB_O_POP));
				continue;
			}
			if (!strncmp(keyName(cur), "system", 6) && strncmp(keyName(split->parents[i]), "system", 6))
			{
				/* parent is system, but key is not -> drop it */
				keyDel (ksLookup(split->keysets[i], cur, KDB_O_POP));
				continue;
			}
			if (!strncmp(keyName(split->parents[i]), "user", 4) && strncmp(keyName(split->parents[i]), "user", 4))
			{
				/* parent is user, but key is not -> drop it */
				keyDel (ksLookup(split->keysets[i], cur, KDB_O_POP));
				continue;
			}

			keyClearSync (cur);
		}

		/* Update sizes */
		if (!strncmp(keyName(split->parents[i]), "system", 6))
		{
			split->handles[i]->systemsize = ksGetSize(split->keysets[i]);
		}
		else if (!strncmp(keyName(split->parents[i]), "user", 4))
		{
			split->handles[i]->usersize = ksGetSize(split->keysets[i]);
		}
	}

	return 1;
}

/**
 * Also update sizes after kdbSet() to recognize multiple kdbSet() attempts.
 *
 * @warning cant use the same code with elektraSplitGet because there is
 * no default split part for kdbSet().
 */
int elektraSplitUpdateSize (Split *split)
{
	/* Iterate everything */
	for (size_t i=0; i<split->size; ++i)
	{
		if (!strncmp(keyName(split->parents[i]), "system", 6))
		{
			split->handles[i]->systemsize = ksGetSize(split->keysets[i]);
		}
		else if (!strncmp(keyName(split->parents[i]), "user", 4))
		{
			split->handles[i]->usersize = ksGetSize(split->keysets[i]);
		}
	}
	return 1;
}

/**
 * Merges together all parts of split into dest.
 *
 * @return 1 on success
 * @ingroup split
 */
int elektraSplitMerge (Split *split, KeySet *dest)
{
	/* Iterate everything */
	for (size_t i=0; i<split->size; ++i)
	{
		ksAppend (dest, split->keysets[i]);
	}
	return 1;
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

/* Deeply copies from source to dest.
 */
KeySet* ksDeepDup(const KeySet *source)
{
	size_t s = source->size;
	size_t i = 0;
	KeySet *keyset = 0;

	keyset = ksNew(source->alloc,KS_END);
	for (i=0; i<s; ++i)
	{
		ksAppendKey(keyset, keyDup(source->array[i]));
	}

	return keyset;
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
	size_t size = split->size;
	for (size_t i=0; i<size; ++i)
	{
		if ((split->syncbits[i] & 1) == 1)
		{
			KeySet *n = ksDeepDup(split->keysets[i]);
			ksDel (split->keysets[i]);
			split->keysets[i] = n;
			continue;
		}

		if ((split->syncbits[i] & 1) == 0)
		{
			/* We dont need i anymore */
			for (size_t j = i+1; j<size; ++j)
			{
				if ((split->syncbits[j] & 1) == 1)
				{
					Backend *tmpBackend = 0;
					Key *tmpParent = 0;
					int tmpSyncbits = 0;

					/* Ohh, we have found an important j... lets swap j and i */
					ksDel (split->keysets[i]);
					split->keysets[i] = ksDup(split->keysets[j]);

					tmpBackend = split->handles[i];
					split->handles[i] = split->handles[j];
					split->handles[j] = tmpBackend;

					tmpParent = split->parents[i];
					split->parents[i] = split->parents[j];
					split->parents[j] = tmpParent;

					tmpSyncbits = split->syncbits[i];
					split->syncbits[i] = split->syncbits[j];
					split->syncbits[j] = tmpSyncbits;
					goto cont; /* Continue the outer loop */
				}
			}

			/* We are finished, search did not find anything which needed sync, so lets remove the rest */
			for (size_t j = i; j<size; ++j)
			{
				if ((split->syncbits[j] & 1) == 0)
				{
					ksDel (split->keysets[j]);
					keyDecRef (split->parents[j]);
					keyDel (split->parents[j]);

					split->keysets[j]=0;
					split->handles[j]=0;
					split->parents[j]=0;
					split->syncbits[j]=0;
					--split->size;
				}
			}
			break;
		}
cont: ;
	}

	return 0;
}

