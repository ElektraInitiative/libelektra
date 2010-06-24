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
void elektraSplitResize(Split *ret)
{
	ret->alloc *= 2;

	elektraRealloc((void**) &ret->keysets, ret->alloc * sizeof(KeySet *));
	elektraRealloc((void**) &ret->handles, ret->alloc * sizeof(KDB *));
	elektraRealloc((void**) &ret->parents, ret->alloc * sizeof(Key *));
	elektraRealloc((void**) &ret->syncbits, ret->alloc * sizeof(int));
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
void elektraSplitAppend(Split *ret)
{
	++ ret->size;
	if (ret->size > ret->alloc) elektraSplitResize(ret);

	ret->keysets[ret->size-1]=NULL;
	ret->handles[ret->size-1]=NULL;
	ret->parents[ret->size-1]=NULL;
	ret->syncbits[ret->size-1]=0;
}

void elektraSplitAppendEmpty(Split *split, Backend *backend, Key *parentKey)
{
	elektraSplitAppend(split);

	split->keysets[split->size-1]=ksNew(0);
	split->handles[split->size-1]=backend;
	split->parents[split->size-1]=parentKey;
	split->syncbits[split->size-1]=1;
}

/**
 * Splits up the keysets and search for a sync bit.
 *
 * It does not check if there were removed keys,
 * see elektraSplitRemove() for the next step.
 *
 * @return 0 if there were no sync bits
 * @return 1 if there were sync bits
 * @ingroup split
 */
int elektraSplitSync(Split *split, KDB *handle, KeySet *ks)
{
	int curFound = 0; /* If key could be appended to any of the existing splitted keysets */
	int needsSync = 0;
	Key *curKey = 0;
	Backend *curHandle = 0;

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		curHandle = kdbGetBackend(handle, curKey);
		curFound = 0;

		/* TODO: optimization: use an index to find already inserted backends */
		for (size_t i=0; i<split->size; ++i)
		{
			if (curHandle == split->handles[i])
			{
				curFound = 1;
				ksAppendKey(split->keysets[i],curKey);
				if (!split->syncbits[i] && keyNeedSync (curKey) == 1)
				{
					needsSync = 1;
					split->syncbits[i] = 1;
				}
			}
		}

		if (!curFound)
		{
			elektraSplitAppend (split);

			split->keysets[split->size-1] = ksNew (ksGetSize (ks) / APPROXIMATE_NR_OF_BACKENDS + 2, KS_END);
			ksAppendKey(split->keysets[split->size-1],curKey);
			split->handles[split->size-1] = curHandle;
			if (curHandle)
			{
				split->parents[split->size-1] = curHandle->mountpoint;
				keyIncRef (split->parents[split->size-1]);
			}
			if (keyNeedSync (curKey) == 1)
			{
				needsSync = 1;
				split->syncbits[split->size-1] = 1;
			}
		}
	}

	return needsSync;
}

/** Add sync bits everywhere keys were removed.
 *
 * @pre user/system was splitted before.
 * @ingroup split
 *
**/
int elektraSplitRemove(Split *split, KDB *handle, KeySet *ks)
{
	int needsSync = 0;

	for (size_t i=0; i<split->size; ++i)
	{
		if (!strncmp(keyName(ksHead(split->keysets[0])), "system", 6))
		{
			/* Check for system keyset */
			if (split->handles[i]->systemsize != ksGetSize(split->keysets[i]))
			{
				split->syncbits[i] = 1;
				needsSync = 1;
			}
		} else if (!strncmp(keyName(ksTail(split->keysets[0])), "user", 4)) {
			/* Check for user keyset */
			if (split->handles[i]->usersize != ksGetSize(split->keysets[i]))
			{
				split->syncbits[i] = 1;
				needsSync = 1;
			}
		}
	}

	return needsSync;
}

/** Determine parentKey for the keysets.
 * Removes sync bits for keysets which are not below parentKey.
 * Split keysets so that user and system are separated.
 * @ingroup split
 */
int elektraSplitParent(Split *split, KeySet *ks, Key *parentKey)
{
	int needsSync = 0;
	Key *curParent;

	for (size_t i=0; i<split->size; ++i)
	{
		if (split->syncbits[i])
		{
			if (parentKey && keyName(parentKey))
			{
				split->syncbits[i] = keyIsBelowOrSame (parentKey, ksHead(split->keysets[0]));
			}

			/* We removed the syncbit because the keyset is not below the parentKey */

			if (!split->syncbits[i]) continue;
			curParent = keyDup (parentKey);
			keySetName(parentKey, keyName(split->handles[i]->mountpoint));
			needsSync = 1;
		}
	}

	return needsSync;
}


/**
 * Splits already splitted keysets again when they need to be synced
 * and have both "user" and "system".
 *
 * Marks both parts to be synced.
 *
 * TODO: should split domains too
 * @ingroup split
 */
int elektraSplitDomains (Split *split, KeySet *ks, Key *parentKey)
{
	int needsSync = 0;
	size_t splitSize = split->size;

	for (size_t i=0; i<splitSize; ++i)
	{
		if (split->syncbits[i])
		{
			if (!strncmp(keyName(ksHead(split->keysets[0])), "system", 6))
			{
				if (!strncmp(keyName(ksTail(split->keysets[0])), "user", 4))
				{
					/* Seems like we need user/system separation for that keyset */
					Key *userKey = keyNew("user", KEY_END);

					elektraSplitAppend(split);
					split->keysets[split->size-1] = ksCut(split->keysets[i], userKey);
					split->handles[split->size-1] = split->handles[i];
					needsSync = 1;
					split->syncbits[split->size-1] = 1;

					keyDel (userKey);
				}
			}
		}
	}

	return needsSync;
}

void elektraSplitSearchTrie(Split *split, Trie *trie)
{
	int i;

	if (trie==NULL) return;

	for (i=0;i<MAX_UCHAR;i++)
	{
		if (trie->text[i]!=NULL)
		{
			Backend *cur = trie->value[i];
			elektraSplitSearchTrie(split, trie->children[i]);
			if (cur) elektraSplitAppendEmpty(split, cur, keyDup(cur->mountpoint));
		}
	}
	if (trie->empty_value)
	{
		Backend *cur = trie->empty_value;
		elektraSplitAppendEmpty(split, cur,
				keyDup(cur->mountpoint));
	}
}

/**
 * Walks through the trie and adds all backends with size > 0
 * and below parentKey.
 *
 * @pre split needs to be empty
 *
 * @ingroup split
 */
int elektraSplitTrie (Split *split, KDB *handle, Key *parentKey)
{
	Trie *trie = handle->trie;
	int needsSync = 0;

	Backend *defaultBackend = handle->defaultBackend;

	if (defaultBackend->usersize > 0)
	{
		elektraSplitAppendEmpty (split, defaultBackend, keyNew("user", KEY_END));
		needsSync = 1;
	}

	if (defaultBackend->systemsize > 0)
	{
		elektraSplitAppendEmpty (split, defaultBackend, keyNew("system", KEY_END));
		needsSync = 1;
	}

	elektraSplitSearchTrie(split, trie);

	return needsSync;
}

