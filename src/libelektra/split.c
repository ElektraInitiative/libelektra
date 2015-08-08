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


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && defined(HAVE_STDIO_H)
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

#include <kdberrors.h>
#include <kdbinternal.h>


/**
 * Allocates a new split object.
 *
 * Splits up a keyset into multiple keysets where each
 * of them will passed to the correct kdbSet().
 *
 * Initially the size is 0 and alloc is APPROXIMATE_NR_OF_BACKENDS.
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
 * Will free all allocated resources of a split keyset.
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
 * @brief Remove one part of split.
 *
 * @param split the split object to work with
 * @param where the position to cut away
 *
 * @pre where must be within the size of the split
 * @post split will be removed
 *
 * @ingroup split
 */
void elektraSplitRemove(Split *split, size_t where)
{
	ELEKTRA_ASSERT(where < split->size && "cannot remove behind size");
	ksDel(split->keysets[where]);
	keyDel(split->parents[where]);
	--split->size; // reduce size
	for(size_t i=where; i<split->size; ++i)
	{
		split->keysets[i] = split->keysets[i+1];
		split->handles[i] = split->handles[i+1];
		split->parents[i] = split->parents[i+1];
		split->syncbits[i] = split->syncbits[i+1];
	}
}

/**
 * Doubles the size of how many parts of keysets can be appended.
 *
 * @param split the split object to work with
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
 * @retval -1 if no split is found
 * @return the position of the new element: size-1
 */
ssize_t elektraSplitAppend(Split *split, Backend *backend, Key *parentKey, int syncbits)
{
	if (!split)
	{
		/* To make test cases work & valgrind clean */
		keyDel (parentKey);
		return -1;
	}

	++ split->size;
	if (split->size > split->alloc) elektraSplitResize(split);

	// index of the new element
	const int n = split->size-1;

	split->keysets[n]=ksNew(0, KS_END);
	split->handles[n]=backend;
	split->parents[n]=parentKey;
	split->syncbits[n]=syncbits;

	return n;
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
 * @retval -1 if it does not exist
 * @ingroup split
 */
ssize_t elektraSplitSearchBackend(Split *split, Backend *backend, Key *parent)
{
	for (size_t i=0; i<split->size; ++i)
	{
		if (backend == split->handles[i])
		{
			if (test_bit(split->syncbits[i], SPLIT_FLAG_CASCADING))
			{
				switch (keyGetNamespace(parent))
				{
				case KEY_NS_SPEC: if (keyIsSpec(split->parents[i])) return i; break;
				case KEY_NS_DIR: if (keyIsDir(split->parents[i])) return i; break;
				case KEY_NS_USER: if (keyIsUser(split->parents[i])) return i; break;
				case KEY_NS_SYSTEM: if (keyIsSystem(split->parents[i])) return i; break;
				case KEY_NS_PROC: return -1;
				case KEY_NS_EMPTY: return -1;
				case KEY_NS_NONE: return -1;
				case KEY_NS_META: return -1;
				case KEY_NS_CASCADING: return -1;
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
 * @retval 1 if one of the backends in split has all
 *          keys below parentKey
 * @retval 0 if parentKey == 0 or there are keys below
 *          or same than parentKey which do not fit
 *          in any of split keysets
 * @param split the split object to work with
 * @param parentKey the key which relation is searched for
 * @ingroup split
 */
int elektraSplitSearchRoot(Split *split, Key *parentKey)
{
	if (!parentKey) return 0;

	for (size_t i=0; i<split->size; ++i)
	{
		if (keyRel (split->parents[i], parentKey) >= 0)
			return 1;
	}

	return 0;
}


/**
 * @brief Map namespace to string and decide if it should be used for kdbGet()
 *
 * @param parentKey the key name that should be changed
 * @param ns the namespace it should be changed to
 *
 * @retval 0 invalid namespace for kdbGet(), no action required
 * @retval 1 valid namespace for kdbGet()
 */
static int elektraKeySetNameByNamespace(Key *parentKey, elektraNamespace ns)
{
	switch (ns)
	{
	case KEY_NS_SPEC:
		keySetName(parentKey, "spec");
		break;
	case KEY_NS_PROC:
		/* only transient, should fail */
		return 0;
	case KEY_NS_DIR:
		keySetName(parentKey, "dir");
		break;
	case KEY_NS_USER:
		keySetName(parentKey, "user");
		break;
	case KEY_NS_SYSTEM:
		keySetName(parentKey, "system");
		break;
	case KEY_NS_EMPTY:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		return 0;
	}
	return 1;
}


/**
 * Walks through kdb->split and adds all backends below parentKey to split.
 *
 * Sets syncbits to 2 if it is a default or root backend (which needs splitting).
 * The information is copied from kdb->split.
 *
 * @pre split needs to be empty, directly after creation with elektraSplitNew().
 *
 * @pre there needs to be a valid defaultBackend
 *      but its ok not to have a trie inside KDB.
 *
 * @pre parentKey must be a valid key! (could be implemented more generally,
 *      but that would require splitting up of keysets of the same backend)
 *
 * @param split will get all backends appended
 * @param kdb the handle to get information about backends
 * @param parentKey the information below which key the backends are from interest
 * @ingroup split
 * @retval 1 always
 */
int elektraSplitBuildup (Split *split, KDB *kdb, Key *parentKey)
{
	/* For compatibility reasons invalid names are accepted, too.
	 * This solution is faster than checking the name of parentKey
	 * every time in loop.
	 * The parentKey might be null in some unit tests, so also check
	 * for this. */
	const char *name = keyName(parentKey);
	if (!parentKey || !name || !strcmp(name, "") || !strcmp(name, "/"))
	{
		parentKey = 0;
	}
	else if (name[0] == '/')
	{
		Key *key = keyNew(0, KEY_END);
		for (elektraNamespace ins=KEY_NS_FIRST; ins<=KEY_NS_LAST; ++ins)
		{
			if (!elektraKeySetNameByNamespace(key, ins)) continue;
			keyAddName(key, keyName(parentKey));
			elektraSplitBuildup(split, kdb, key);
		}
		keyDel (key);
		return 1;
	}

	/* Returns the backend the key is in or the default backend
	   otherwise */
	Backend * backend = elektraMountGetBackend(kdb, parentKey);

#if DEBUG && VERBOSE
	printf (" with parent %s\n", keyName(parentKey));
#endif
	for (size_t i=0; i < kdb->split->size; ++i)
	{
#if DEBUG && VERBOSE
		printf ("  %zu with parent %s\n", i, keyName(kdb->split->parents[i]));
#endif
		if (!parentKey)
		{
#if DEBUG && VERBOSE
			printf ("   def add %s\n", keyName(kdb->split->parents[i]));
#endif
			/* Catch all: add all mountpoints */
			elektraSplitAppend (split, kdb->split->handles[i], keyDup(kdb->split->parents[i]), kdb->split->syncbits[i]);
		}
		else if (backend == kdb->split->handles[i] && keyRel(kdb->split->parents[i], parentKey) >= 0)
		{
#if DEBUG && VERBOSE
			printf ("   exa add %s\n", keyName(kdb->split->parents[i]));
#endif
			/* parentKey is exactly in this backend, so add it! */
			elektraSplitAppend (split, kdb->split->handles[i], keyDup(kdb->split->parents[i]), kdb->split->syncbits[i]);
		}
		else if (keyRel(parentKey, kdb->split->parents[i]) >= 0)
		{
#if DEBUG && VERBOSE
			printf ("   rel add %s\n", keyName(kdb->split->parents[i]));
#endif
			/* this backend is completely below the parentKey, so lets add it. */
			elektraSplitAppend (split, kdb->split->handles[i], keyDup(kdb->split->parents[i]), kdb->split->syncbits[i]);
		}
	}

	return 1;
}



/**
 * Splits up the keysets and search for a sync bit in every key.
 *
 * It does not check if there were removed keys,
 * see elektraSplitSync() for the next step.
 *
 * It does not create new backends, this has to be
 * done by buildup before.
 *
 * @pre elektraSplitBuildup() need to be executed before.
 *
 * @param split the split object to work with
 * @param handle to get information where the individual keys belong
 * @param ks the keyset to divide
 *
 * @retval 0 if there were no sync bits
 * @retval 1 if there were sync bits
 * @retval -1 if no backend was found for any key
 * @ingroup split
 */
int elektraSplitDivide (Split *split, KDB *handle, KeySet *ks)
{
	ssize_t curFound = 0; /* If key could be appended to any of the existing split keysets */
	int needsSync = 0;
	Key *curKey = 0;
	Backend *curHandle = 0;

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		// TODO: handle keys in wrong namespaces
		curHandle = elektraMountGetBackend(handle, curKey);
		if (!curHandle) return -1;

		curFound = elektraSplitSearchBackend(split, curHandle, curKey);

		if (curFound == -1) continue; // key not relevant in this kdbSet

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
 * @brief Update the (configuration) file name for the parent key
 *
 * @param split the split to work with
 * @param handle the handle to work with
 * @param key the parentKey that should be updated (name must be
 * correct)
 */
void elektraSplitUpdateFileName (Split *split, KDB *handle, Key *key)
{
	Backend *curHandle = elektraMountGetBackend(handle, key);
	if (!curHandle) return;
	ssize_t curFound = elektraSplitSearchBackend(split, curHandle, key);
	if (curFound == -1) return;
#if DEBUG && VERBOSE
	printf ("Update string from %s to %s\n", keyString(key), keyString(split->parents[curFound]));
	printf ("Names are: %s and %s\n\n", keyName(key), keyName(split->parents[curFound]));
#endif
	keySetString(key, keyString(split->parents[curFound]));
}


/**
 * Appoints all keys from ks to yet unsynced splits.
 *
 * @pre elektraSplitBuildup() need to be executed before.
 *
 * @param split the split object to work with
 * @param handle to determine to which backend a key belongs
 * @param ks the keyset to appoint to split
 *
 * @retval 1 on success
 * @retval -1 if no backend was found for a key
 * @ingroup split
 */
int elektraSplitAppoint (Split *split, KDB *handle, KeySet *ks)
{
	ssize_t curFound = 0; /* If key could be appended to any of the existing split keysets */
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

		if (split->syncbits[curFound] & SPLIT_FLAG_SYNC)
		{
			continue;
		}

		ksAppendKey (split->keysets[curFound], curKey);
	}

	return 1;
}

static void elektraDropCurrentKey(KeySet *ks,
		Key *warningKey,
		const Backend *curHandle,
		const char *msg)
{
	const Key *k = ksCurrent(ks);

	const size_t sizeOfStaticText = 100;
	char *warningMsg = elektraMalloc(
			keyGetNameSize(curHandle->mountpoint) +
			keyGetValueSize(curHandle->mountpoint) +
			keyGetNameSize(k) +
			strlen(msg) +
			sizeOfStaticText);
	strcpy(warningMsg, "drop key ");
	const char *name = keyName(k);
	if (name) strcat(warningMsg, name);
	else strcat(warningMsg, "(no name)");
	strcat(warningMsg, " not belonging to ");
	strcat(warningMsg, keyName(curHandle->mountpoint));
	strcat(warningMsg, " with name ");
	strcat(warningMsg, keyString(curHandle->mountpoint));
	strcat(warningMsg, " because ");
	strcat(warningMsg, msg);
	ELEKTRA_ADD_WARNING(79, warningKey, warningMsg);
	elektraFree(warningMsg);
	cursor_t c = ksGetCursor(ks);
	keyDel (ksPopAtCursor(ks, c));
	ksSetCursor(ks, c);
	ksPrev(ks); // next ksNext() will point correctly again
}


/**
 * @brief Filter out keys not in the correct keyset
 *
 * @param split the split where to do it
 * @param i for which split
 * @param warningKey the key
 * @param handle where to do backend lookups
 *
 * @retval -1 on error (no backend, wrong namespace)
 * @retval 0 otherwise
 */
static int elektraSplitPostprocess (Split *split, int i, Key *warningKey, KDB *handle)
{
	Key *cur = 0;
	Backend *curHandle = 0;
	ksRewind (split->keysets[i]);
	while ((cur = ksNext(split->keysets[i])) != 0)
	{
		curHandle = elektraMountGetBackend(handle, cur);
		if (!curHandle) return -1;

		keyClearSync (cur);

		if (curHandle != split->handles[i])
		{
			elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it is hidden by other mountpoint");
		}
		else switch (keyGetNamespace(cur))
		{
		case KEY_NS_SPEC:
			if (!keyIsSpec(split->parents[i]))
				elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it is not spec");
			break;
		case KEY_NS_DIR:
			if (!keyIsDir(split->parents[i]))
				elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it is not dir");
			break;
		case KEY_NS_USER:
			if (!keyIsUser(split->parents[i]))
				elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it is not user");
			break;
		case KEY_NS_SYSTEM:
			if (!keyIsSystem(split->parents[i]))
				elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it is not system");
			break;
		case KEY_NS_PROC:
			elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it has a proc key name");
			break;
		case KEY_NS_EMPTY:
			elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it has an empty name");
			break;
		case KEY_NS_META:
			elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it has a meta name");
			break;
		case KEY_NS_CASCADING:
			elektraDropCurrentKey(split->keysets[i], warningKey, curHandle, "it has a cascading name");
			break;
		case KEY_NS_NONE:
			ELEKTRA_ASSERT(0 && "wrong key namespace, should not be none");
			return -1;
		}
	}
	return 0;
}


/**
 * Does some work after getting of backends is finished.
 *
 * - Update sizes
 * - Removal of wrong keys
 *
 * @pre elektraSplitAppoint() needs to be executed before.
 *
 * - check if keys are in correct backend
 * - remove syncbits
 * - update sizes in the backends
 *
 * @param split the split object to work with
 * @param warningKey postcondition violations are reported here
 * @param handle the handle to preprocess the keys
 * @retval 1 on success
 * @retval -1 if no backend was found for a key or split->parents
 *         has invalid namespace
 * @ingroup split
 */
int elektraSplitGet (Split *split, Key *warningKey, KDB *handle)
{
	int ret = 1;
	/* Dont iterate the default split part */
	const int bypassedSplits = 1;

	for (size_t i=0; i<split->size-bypassedSplits; ++i)
	{
		if (test_bit(split->syncbits[i], 0))
		{
			/* Dont process keysets which come from the user
			   and not from the backends */
			continue;
		}

		/* Update sizes */
#if DEBUG && VERBOSE
		printf ("Update size for %s\n", keyName(split->parents[i]));
#endif
		// first we need postprocessing because that might
		// reduce sizes
		if (elektraSplitPostprocess(split, i, warningKey, handle) == -1) ret = -1;
		// then we can set the size
		if (elektraBackendUpdateSize(split->handles[i], split->parents[i], ksGetSize(split->keysets[i])) == -1) ret = -1;
	}

	return ret;
}

/**
 * @brief Check if any of the split is uninitialized
 *
 * @param split
 *
 * @retval -1 if size is wrong
 * @retval 1 if everything is ok
 */
int elektraSplitCheckSize (Split *split)
{
	/* Iterate everything */
	for (size_t i=0; i<split->size; ++i)
	{
		switch (keyGetNamespace(split->parents[i]))
		{
		case KEY_NS_SPEC:
			if (split->handles[i]->specsize == -1) return -1;
			break;
		case KEY_NS_DIR:
			if (split->handles[i]->dirsize == -1) return -1;
			break;
		case KEY_NS_USER:
			if (split->handles[i]->usersize == -1) return -1;
			break;
		case KEY_NS_SYSTEM:
			if (split->handles[i]->systemsize == -1) return -1;
			break;
		case KEY_NS_PROC:
		case KEY_NS_EMPTY:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			return -1;
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
		switch (keyGetNamespace(split->parents[i]))
		{
		case KEY_NS_SPEC:
			split->handles[i]->specsize = ksGetSize(split->keysets[i]);
			break;
		case KEY_NS_DIR:
			split->handles[i]->dirsize = ksGetSize(split->keysets[i]);
			break;
		case KEY_NS_USER:
			split->handles[i]->usersize = ksGetSize(split->keysets[i]);
			break;
		case KEY_NS_SYSTEM:
			split->handles[i]->systemsize = ksGetSize(split->keysets[i]);
			break;
		case KEY_NS_PROC:
		case KEY_NS_EMPTY:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			return -1;
		}
	}
	return 1;
}

/**
 * Merges together all parts of split into dest.
 *
 * @param split the split object to work with
 * @param dest the destination keyset where all keysets are appended.
 * @retval 1 on success
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

/** Add sync bits everywhere keys were removed/added.
 *
 * - checks if the size of a previous kdbGet() is unchanged.
 * - checks if in correct state (kdbGet() needs to be executed before)
 *
 * Only elektraSplitDivide() together with this function can really decide
 * if sync is needed or not.
 *
 * @pre split needs to be processed with elektraSplitDivide() before.
 *
 * @retval 0 if kdbSet() is not needed
 * @retval 1 if kdbSet() is needed
 * @retval -1 on wrong keys (also has assert, should not happen)
 * @retval -2 wrong spec state: kdbGet() was not executed before
 * @retval -3 wrong dir state: kdbGet() was not executed before
 * @retval -4 wrong user state: kdbGet() was not executed before
 * @retval -5 wrong system state: kdbGet() was not executed before
 * @pre user/system was split before.
 * @param split the split object to work with
 * @ingroup split
 *
**/
int elektraSplitSync(Split *split)
{
	int needsSync = 0;

	for (size_t i=0; i<split->size; ++i)
	{
		// first check for wrong states etc.
		switch (keyGetNamespace(split->parents[i]))
		{
		case KEY_NS_SPEC:
			// Check if we are in correct state
			if (split->handles[i]->specsize == -1)
			{
				return -i-2;
			}
			/* Check for spec keyset for removed keys */
			if (split->handles[i]->specsize != ksGetSize(split->keysets[i]))
			{
				set_bit(split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case KEY_NS_DIR:
			// Check if we are in correct state
			if (split->handles[i]->dirsize == -1)
			{
				return -i-2;
			}
			/* Check for dir keyset for removed keys */
			if (split->handles[i]->dirsize != ksGetSize(split->keysets[i]))
			{
				set_bit(split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case KEY_NS_USER:
			// Check if we are in correct state
			if (split->handles[i]->usersize == -1)
			{
				return -i-2;
			}
			/* Check for user keyset for removed keys */
			if (split->handles[i]->usersize != ksGetSize(split->keysets[i]))
			{
				set_bit(split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case KEY_NS_SYSTEM:
			// Check if we are in correct state
			if (split->handles[i]->systemsize == -1)
			{
				return -i-2;
			}
			/* Check for system keyset for removed keys */
			if (split->handles[i]->systemsize != ksGetSize(split->keysets[i]))
			{
				set_bit(split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case KEY_NS_PROC:
		case KEY_NS_EMPTY:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
		case KEY_NS_NONE:
			ELEKTRA_ASSERT(0 && "Got keys that should not be here");
			return -1;
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
void elektraSplitPrepare (Split *split)
{
	for (size_t i=0; i<split->size; ++i)
	{
		if ((split->syncbits[i] & 1) == 1)
		{
			KeySet *n = ksDeepDup(split->keysets[i]);
			ksDel (split->keysets[i]);
			split->keysets[i] = n;
		}
		else
		{
			/* We don't need i anymore */
			elektraSplitRemove(split, i);
			--i;
		}
	}
}
