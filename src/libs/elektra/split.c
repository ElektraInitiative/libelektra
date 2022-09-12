/**
 * @file
 *
 * @brief Interna of splitting functionality.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if VERBOSE && defined(HAVE_STDIO_H)
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

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbinternal.h>

#if 1 == 0
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
 * @see splitDel()
 **/
Split * splitNew (void)
{
	Split * ret = elektraCalloc (sizeof (Split));

	ret->size = 0;
	ret->alloc = APPROXIMATE_NR_OF_BACKENDS;

	ret->keysets = elektraCalloc (sizeof (ElektraKeyset *) * ret->alloc);
	ret->handles = elektraCalloc (sizeof (ElektraKdb *) * ret->alloc);
	ret->parents = elektraCalloc (sizeof (ElektraKey *) * ret->alloc);
	ret->syncbits = elektraCalloc (sizeof (int) * ret->alloc);
	ret->systemsizes = elektraCalloc (sizeof (ssize_t) * ret->alloc);
	ret->usersizes = elektraCalloc (sizeof (ssize_t) * ret->alloc);
	ret->specsizes = elektraCalloc (sizeof (ssize_t) * ret->alloc);
	ret->dirsizes = elektraCalloc (sizeof (ssize_t) * ret->alloc);

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
void splitDel (Split * keysets)
{
	for (size_t i = 0; i < keysets->size; ++i)
	{
		elektraKeysetDel (keysets->keysets[i]);
		elektraKeyDecRef (keysets->parents[i]);
		elektraKeyDel (keysets->parents[i]);
	}
	elektraFree (keysets->keysets);
	elektraFree (keysets->handles);
	elektraFree (keysets->parents);
	elektraFree (keysets->syncbits);
	elektraFree (keysets->systemsizes);
	elektraFree (keysets->usersizes);
	elektraFree (keysets->specsizes);
	elektraFree (keysets->dirsizes);
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
void splitRemove (Split * split, size_t where)
{
	ELEKTRA_ASSERT (where < split->size, "cannot remove behind size: %zu smaller than %zu", where, split->size);
	elektraKeysetDel (split->keysets[where]);
	elektraKeyDel (split->parents[where]);
	--split->size; // reduce size
	for (size_t i = where; i < split->size; ++i)
	{
		split->keysets[i] = split->keysets[i + 1];
		split->handles[i] = split->handles[i + 1];
		split->parents[i] = split->parents[i + 1];
		split->syncbits[i] = split->syncbits[i + 1];
		split->systemsizes[i] = split->systemsizes[i + 1];
		split->usersizes[i] = split->usersizes[i + 1];
		split->specsizes[i] = split->specsizes[i + 1];
		split->dirsizes[i] = split->dirsizes[i + 1];
	}
}

/**
 * Doubles the size of how many parts of keysets can be appended.
 *
 * @param split the split object to work with
 * @ingroup split
 */
static void splitResize (Split * split)
{
	split->alloc *= 2;

	elektraRealloc ((void **) &split->keysets, split->alloc * sizeof (ElektraKeyset *));
	elektraRealloc ((void **) &split->handles, split->alloc * sizeof (ElektraKdb *));
	elektraRealloc ((void **) &split->parents, split->alloc * sizeof (ElektraKey *));
	elektraRealloc ((void **) &split->syncbits, split->alloc * sizeof (int));
	elektraRealloc ((void **) &split->systemsizes, split->alloc * sizeof (ssize_t));
	elektraRealloc ((void **) &split->usersizes, split->alloc * sizeof (ssize_t));
	elektraRealloc ((void **) &split->specsizes, split->alloc * sizeof (ssize_t));
	elektraRealloc ((void **) &split->dirsizes, split->alloc * sizeof (ssize_t));
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
ssize_t splitAppend (Split * split, Plugin * backend, ElektraKey * parentKey, int syncbits)
{
	if (!split)
	{
		/* To make test cases work & valgrind clean */
		elektraKeyDel (parentKey);
		return -1;
	}

	++split->size;
	if (split->size > split->alloc) splitResize (split);

	// index of the new element
	const int n = split->size - 1;

	split->keysets[n] = elektraKeysetNew (0, ELEKTRA_KS_END);
	split->handles[n] = backend;
	split->parents[n] = parentKey;
	split->syncbits[n] = syncbits;

	// FIXME: somehow find correct sizes
	split->systemsizes[n] = 0;
	split->usersizes[n] = 0;
	split->specsizes[n] = 0;
	split->dirsizes[n] = 0;

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
static ssize_t splitSearchBackend (Split * split, Plugin * backend, ElektraKey * parent)
{
	for (size_t i = 0; i < split->size; ++i)
	{
		if (backend == split->handles[i])
		{
			if (test_bit (split->syncbits[i], SPLIT_FLAG_CASCADING))
			{
				switch (elektraKeyGetNamespace (parent))
				{
				case ELEKTRA_NS_SPEC:
					if (elektraKeyIsSpec (split->parents[i])) return i;
					break;
				case ELEKTRA_NS_DIR:
					if (elektraKeyIsDir (split->parents[i])) return i;
					break;
				case ELEKTRA_NS_USER:
					if (elektraKeyIsUser (split->parents[i])) return i;
					break;
				case ELEKTRA_NS_SYSTEM:
					if (elektraKeyIsSystem (split->parents[i])) return i;
					break;
				case ELEKTRA_NS_PROC:
					return -1;
				case ELEKTRA_NS_NONE:
					return -1;
				case ELEKTRA_NS_META:
					return -1;
				case ELEKTRA_NS_CASCADING:
					return -1;
				case ELEKTRA_NS_DEFAULT:
					return -1;
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
 * @brief Map namespace to string and decide if it should be used for kdbGet()
 *
 * @param parentKey the key name that should be changed
 * @param ns the namespace it should be changed to
 *
 * @retval 0 invalid namespace for kdbGet(), no action required
 * @retval 1 valid namespace for kdbGet()
 */
static int elektraKeySetNameByNamespace (ElektraKey * parentKey, elektraNamespace ns)
{
	switch (ns)
	{
	case ELEKTRA_NS_SPEC:
		elektraKeySetName (parentKey, "spec:/");
		break;
	case ELEKTRA_NS_PROC:
		/* only transient, should fail */
		return 0;
	case ELEKTRA_NS_DIR:
		elektraKeySetName (parentKey, "dir:/");
		break;
	case ELEKTRA_NS_USER:
		elektraKeySetName (parentKey, "user:/");
		break;
	case ELEKTRA_NS_SYSTEM:
		elektraKeySetName (parentKey, "system:/");
		break;
	case ELEKTRA_NS_NONE:
	case ELEKTRA_NS_META:
	case ELEKTRA_NS_CASCADING:
	case ELEKTRA_NS_DEFAULT:
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
 * @pre split needs to be empty, directly after creation with splitNew().
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
int splitBuildup (Split * split, ElektraKdb * kdb, ElektraKey * parentKey)
{
	/* For compatibility reasons invalid names are accepted, too.
	 * This solution is faster than checking the name of parentKey
	 * every time in loop.
	 * The parentKey might be null in some unit tests, so also check
	 * for this. */
	const char * name = elektraKeyName (parentKey);
	if (!parentKey || !name || !strcmp (name, "") || !strcmp (name, "/"))
	{
		parentKey = 0;
	}
	else if (name[0] == '/')
	{
		ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
		for (elektraNamespace ins = ELEKTRA_NS_FIRST; ins <= ELEKTRA_NS_LAST; ++ins)
		{
			if (!elektraKeySetNameByNamespace (key, ins)) continue;
			elektraKeyAddName (key, elektraKeyName (parentKey));
			splitBuildup (split, kdb, key);
		}
		elektraKeyDel (key);
		return 1;
	}

	/* Returns the backend the key is in or the default backend
	   otherwise */
	Plugin * backend = mountGetBackend (kdb, parentKey);

#if DEBUG && VERBOSE
	printf (" with parent %s\n", elektraKeyName (parentKey));
#endif
	for (size_t i = 0; i < kdb->split->size; ++i)
	{
#if DEBUG && VERBOSE
		printf ("  %zu with parent %s\n", i, elektraKeyName (kdb->split->parents[i]));
#endif
		if (!parentKey)
		{
#if DEBUG && VERBOSE
			printf ("   def add %s\n", elektraKeyName (kdb->split->parents[i]));
#endif
			/* Catch all: add all mountpoints */
			splitAppend (split, kdb->split->handles[i], elektraKeyDup (kdb->split->parents[i], ELEKTRA_KEY_CP_ALL), kdb->split->syncbits[i]);
		}
		else if (backend == kdb->split->handles[i] &&
			 (elektraKeyCmp (kdb->split->parents[i], parentKey) == 0 || elektraKeyIsBelow (kdb->split->parents[i], parentKey) == 1))
		{
#if DEBUG && VERBOSE
			printf ("   exa add %s\n", elektraKeyName (kdb->split->parents[i]));
#endif
			/* parentKey is exactly in this backend, so add it! */
			splitAppend (split, kdb->split->handles[i], elektraKeyDup (kdb->split->parents[i], ELEKTRA_KEY_CP_ALL), kdb->split->syncbits[i]);
		}
		else if (elektraKeyCmp (parentKey, kdb->split->parents[i]) == 0 || elektraKeyIsBelow (parentKey, kdb->split->parents[i]) == 1)
		{
#if DEBUG && VERBOSE
			printf ("   rel add %s\n", elektraKeyName (kdb->split->parents[i]));
#endif
			/* this backend is completely below the parentKey, so lets add it. */
			splitAppend (split, kdb->split->handles[i], elektraKeyDup (kdb->split->parents[i], ELEKTRA_KEY_CP_ALL), kdb->split->syncbits[i]);
		}
	}

	return 1;
}


/**
 * Splits up the keysets and search for a sync bit in every key.
 *
 * It does not check if there were removed keys,
 * see splitSync() for the next step.
 *
 * It does not create new backends, this has to be
 * done by buildup before.
 *
 * @pre splitBuildup() need to be executed before.
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
int splitDivide (Split * split, ElektraKdb * handle, ElektraKeyset * ks)
{
	int needsSync = 0;
	ElektraKey * curKey = 0;

	elektraKeysetRewind (ks);
	while ((curKey = elektraKeysetNext (ks)) != 0)
	{
		// TODO: handle keys in wrong namespaces
		Plugin * curHandle = mountGetBackend (handle, curKey);
		if (!curHandle) return -1;

		/* If key could be appended to any of the existing split keysets */
		ssize_t curFound = splitSearchBackend (split, curHandle, curKey);

		if (curFound == -1)
		{
			ELEKTRA_LOG_DEBUG ("SKIPPING NOT RELEVANT KEY: %p key: %s, string: %s", (void *) curKey, elektraKeyName (curKey),
					   elektraKeyString (curKey));
			continue; // key not relevant in this kdbSet
		}

		elektraKeysetAppendKey (split->keysets[curFound], curKey);
		if (elektraKeyNeedSync (curKey) == 1)
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
void splitUpdateFileName (Split * split, ElektraKdb * handle, ElektraKey * key)
{
	Plugin * curHandle = mountGetBackend (handle, key);
	if (!curHandle) return;
	ssize_t curFound = splitSearchBackend (split, curHandle, key);
	if (curFound == -1) return;
#if DEBUG && VERBOSE
	printf ("Update string from %s to %s\n", elektraKeyString (key), elektraKeyString (split->parents[curFound]));
	printf ("Names are: %s and %s\n\n", elektraKeyName (key), elektraKeyName (split->parents[curFound]));
#endif
	elektraKeySetString (key, elektraKeyString (split->parents[curFound]));
}


/**
 * Appoints all keys from ks to yet unsynced splits.
 *
 * @pre splitBuildup() need to be executed before.
 *
 * @param split the split object to work with
 * @param handle to determine to which backend a key belongs
 * @param ks the keyset to appoint to split
 *
 * @retval 1 on success
 * @retval -1 if no backend was found for a key
 * @ingroup split
 */
int splitAppoint (Split * split, ElektraKdb * handle, ElektraKeyset * ks)
{
	ElektraKey * curKey = 0;
	ssize_t defFound = splitAppend (split, 0, 0, 0);

	elektraKeysetRewind (ks);
	while ((curKey = elektraKeysetNext (ks)) != 0)
	{
		Plugin * curHandle = mountGetBackend (handle, curKey);
		if (!curHandle) return -1;

		/* If key could be appended to any of the existing split keysets */
		ssize_t curFound = splitSearchBackend (split, curHandle, curKey);

		if (curFound == -1) curFound = defFound;

		if (split->syncbits[curFound] & SPLIT_FLAG_SYNC)
		{
			continue;
		}

		elektraKeysetAppendKey (split->keysets[curFound], curKey);
	}

	return 1;
}

static void elektraDropCurrentKey (ElektraKeyset * ks, ElektraKey * warningKey, const Plugin * curHandle, const Plugin * otherHandle, const char * msg)
{
	const ElektraKey * k = elektraKeysetCurrent (ks);
	const char * name = elektraKeyName (k);
	const ElektraKey * mountpoint = backendGetMountpoint (curHandle);
	const ElektraKey * otherMountpoint = backendGetMountpoint (otherHandle);

	if (otherHandle)
	{
		ELEKTRA_ADD_INTERFACE_WARNINGF (
			warningKey,
			"Postcondition of backend was violated: drop key %s not belonging to \"%s\" with name \"%s\" but "
			"instead to \"%s\" with name \"%s\" because %s ",
			name ? name : "(no name)", mountpoint ? elektraKeyName (mountpoint) : "(default mountpoint)", elektraKeyString (mountpoint),
			elektraKeyName (otherMountpoint), elektraKeyString (otherMountpoint), msg);
	}
	else
	{
		ELEKTRA_ADD_INTERFACE_WARNINGF (
			warningKey,
			"Postcondition of backend was violated: drop key %s not belonging to \"%s\" with name \"%s\" because %s ",
			name ? name : "(no name)", mountpoint ? elektraKeyName (mountpoint) : "(default mountpoint)", elektraKeyString (mountpoint), msg);
	}
	elektraCursor c = elektraKeysetGetCursor (ks);
	elektraKeyDel (elektraKsPopAtCursor (ks, c));
	elektraKeysetSetCursor (ks, c - 1); // next ksNext() will point correctly again
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
static int elektraSplitPostprocess (Split * split, int i, ElektraKey * warningKey, ElektraKdb * handle)
{
	ElektraKey * cur = 0;
	elektraKeysetRewind (split->keysets[i]);
	while ((cur = elektraKeysetNext (split->keysets[i])) != 0)
	{
		Plugin * curHandle = mountGetBackend (handle, cur);
		if (!curHandle) return -1;

		elektraKeyClearSync (cur);

		if (curHandle != split->handles[i])
		{
			elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, split->handles[i],
					       "it is hidden by other mountpoint");
		}
		else
			switch (elektraKeyGetNamespace (cur))
			{
			case ELEKTRA_NS_SPEC:
				if (!elektraKeyIsSpec (split->parents[i]))
					elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it is not spec");
				break;
			case ELEKTRA_NS_DIR:
				if (!elektraKeyIsDir (split->parents[i]))
					elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it is not dir");
				break;
			case ELEKTRA_NS_USER:
				if (!elektraKeyIsUser (split->parents[i]))
					elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it is not user");
				break;
			case ELEKTRA_NS_SYSTEM:
				if (!elektraKeyIsSystem (split->parents[i]))
					elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it is not system");
				break;
			case ELEKTRA_NS_PROC:
				elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it has a proc key name");
				break;
			case ELEKTRA_NS_META:
				elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it has a metaname");
				break;
			case ELEKTRA_NS_CASCADING:
				elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it has a cascading name");
				break;
			case ELEKTRA_NS_DEFAULT:
				elektraDropCurrentKey (split->keysets[i], warningKey, curHandle, 0, "it has a default name");
				break;
			case ELEKTRA_NS_NONE:
				ELEKTRA_ASSERT (0, "wrong key namespace `none'");
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
 * @pre splitAppoint() needs to be executed before.
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
int splitGet (Split * split, ElektraKey * warningKey, ElektraKdb * handle)
{
	int ret = 1;
	/* Dont iterate the default split part */
	const int bypassedSplits = 1;

	for (size_t i = 0; i < split->size - bypassedSplits; ++i)
	{
		if (test_bit (split->syncbits[i], 0))
		{
			/* Dont process keysets which come from the user
			   and not from the backends */
			continue;
		}

/* Update sizes */
#if DEBUG && VERBOSE
		printf ("Update size for %s\n", elektraKeyName (split->parents[i]));
#endif
		// first we need postprocessing because that might
		// reduce sizes
		if (elektraSplitPostprocess (split, i, warningKey, handle) == -1) ret = -1;
		// then we can set the size
		ELEKTRA_LOG_DEBUG ("splitGet : backendUpdateSize thingy");
		if (backendUpdateSize (split, i, split->parents[i], elektraKeysetGetSize (split->keysets[i])) == -1) ret = -1;
	}

	return ret;
}

/**
 * Also update sizes after kdbSet() to recognize multiple kdbSet() attempts.
 *
 * @warning cant use the same code with splitGet because there is
 * no default split part for kdbSet().
 */
int splitUpdateSize (Split * split)
{
	/* Iterate everything */
	for (size_t i = 0; i < split->size; ++i)
	{
		switch (elektraKeyGetNamespace (split->parents[i]))
		{
		case ELEKTRA_NS_SPEC:
			split->specsizes[i] = elektraKeysetGetSize (split->keysets[i]);
			break;
		case ELEKTRA_NS_DIR:
			split->dirsizes[i] = elektraKeysetGetSize (split->keysets[i]);
			break;
		case ELEKTRA_NS_USER:
			split->usersizes[i] = elektraKeysetGetSize (split->keysets[i]);
			break;
		case ELEKTRA_NS_SYSTEM:
			split->systemsizes[i] = elektraKeysetGetSize (split->keysets[i]);
			break;
		case ELEKTRA_NS_PROC:
		case ELEKTRA_NS_NONE:
		case ELEKTRA_NS_META:
		case ELEKTRA_NS_CASCADING:
		case ELEKTRA_NS_DEFAULT:
			return -1;
		}
	}
	return 1;
}

/**
 * Merges together the backend based parts of split into dest,
 * but bypasses the default split.
 *
 * @param split the split object to work with
 * @param dest the destination keyset where all keysets are appended.
 * @retval 1 on success
 * @ingroup split
 */
int splitMergeBackends (Split * split, ElektraKeyset * dest)
{
	/* Bypass default split */
	const int bypassedSplits = 1;
	for (size_t i = 0; i < split->size - bypassedSplits; ++i)
	{
		if (test_bit (split->syncbits[i], 0))
		{
			/* Dont process keysets which come from the user
			   and not from the backends */
			continue;
		}
		elektraKeysetAppend (dest, split->keysets[i]);
	}
	return 1;
}

/**
 * Merges the default split into dest.
 *
 * @param split the split object to work with
 * @param dest the destination keyset where all keysets are appended.
 * @retval 1 on success
 * @ingroup split
 */
int splitMergeDefault (Split * split, ElektraKeyset * dest)
{
	/* Merge default split */
	elektraKeysetAppend (dest, split->keysets[split->size - 1]);
	return 1;
}

/** Add sync bits everywhere keys were removed/added.
 *
 * - checks if the size of a previous kdbGet() is unchanged.
 * - checks if in correct state (kdbGet() needs to be executed before)
 *
 * Only splitDivide() together with this function can really decide
 * if sync is needed or not.
 *
 * @pre split needs to be processed with splitDivide() before.
 *
 * @retval 0 if kdbSet() is not needed
 * @retval 1 if kdbSet() is needed
 * @retval -1 on wrong keys (also has assert, should not happen)
 * @retval -2 wrong spec state: kdbGet() was not executed before
 * @retval -3 wrong dir state: kdbGet() was not executed before
 * @retval -4 wrong user state: kdbGet() was not executed before
 * @retval -5 wrong system state: kdbGet() was not executed before
 * @pre user:/system was split before.
 * @param split the split object to work with
 * @ingroup split
 *
 **/
int splitSync (Split * split)
{
	int needsSync = 0;

	for (size_t i = 0; i < split->size; ++i)
	{
		// first check for wrong states etc.
		switch (elektraKeyGetNamespace (split->parents[i]))
		{
		case ELEKTRA_NS_SPEC:
			// Check if we are in correct state
			if (split->specsizes[i] == -1)
			{
				return -(int) i - 2;
			}
			/* Check for spec keyset for removed keys */
			if (split->specsizes[i] != elektraKeysetGetSize (split->keysets[i]))
			{
				set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case ELEKTRA_NS_DIR:
			// Check if we are in correct state
			if (split->dirsizes[i] == -1)
			{
				return -(int) i - 2;
			}
			/* Check for dir keyset for removed keys */
			if (split->dirsizes[i] != elektraKeysetGetSize (split->keysets[i]))
			{
				set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case ELEKTRA_NS_USER:
			// Check if we are in correct state
			if (split->usersizes[i] == -1)
			{
				return -(int) i - 2;
			}
			/* Check for user keyset for removed keys */
			if (split->usersizes[i] != elektraKeysetGetSize (split->keysets[i]))
			{
				set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case ELEKTRA_NS_SYSTEM:
			// Check if we are in correct state
			if (split->systemsizes[i] == -1)
			{
				return -(int) i - 2;
			}
			/* Check for system keyset for removed keys */
			if (split->systemsizes[i] != elektraKeysetGetSize (split->keysets[i]))
			{
				set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
				needsSync = 1;
			}
			break;
		case ELEKTRA_NS_PROC:
		case ELEKTRA_NS_META:
		case ELEKTRA_NS_CASCADING:
		case ELEKTRA_NS_NONE:
		case ELEKTRA_NS_DEFAULT:
			ELEKTRA_ASSERT (0, "Got keys in wrong namespace %d in split %zu", elektraKeyGetNamespace (split->parents[i]), i);
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
void splitPrepare (Split * split)
{
	for (size_t i = 0; i < split->size;)
	{
		int inc = 1;
		if ((split->syncbits[i] & 1) == 1)
		{
			ElektraKeyset * n = elektraKeysetDeepDup (split->keysets[i]);
			elektraKeysetDel (split->keysets[i]);
			split->keysets[i] = n;
		}
		else
		{
			/* We don't need i anymore */
			splitRemove (split, i);
			inc = 0;
		}
		i += inc;
	}
}

static char * elektraStrConcat (const char * a, const char * b)
{
	size_t len = strlen (a) + strlen (b) + 1;
	char * ret = elektraMalloc (len);
	ret = strcpy (ret, a);
	ret = strcat (ret, b);
	return ret;
}

void splitCacheStoreState (ElektraKdb * handle, Split * split, ElektraKeyset * global, ElektraKey * parentKey, ElektraKey * initialParent)
{
	ElektraKey * mountPoint = mountGetMountpoint (handle, parentKey);
	const char * mountPointName = mountPoint == NULL ? "/" : elektraKeyName (mountPoint);
	const char * mountPointValue = mountPoint == NULL ? "" : elektraKeyString (mountPoint);
	ElektraKey * lastParentName = elektraKeyNew (KDB_CACHE_PREFIX "/lastParentName", ELEKTRA_KEY_VALUE, mountPointName, ELEKTRA_KEY_END);
	ElektraKey * lastParentValue = elektraKeyNew (KDB_CACHE_PREFIX "/lastParentValue", ELEKTRA_KEY_VALUE, mountPointValue, ELEKTRA_KEY_END);
	ElektraKey * lastInitalParentName = elektraKeyNew (KDB_CACHE_PREFIX "/lastInitialParentName", ELEKTRA_KEY_VALUE, elektraKeyName (initialParent), ELEKTRA_KEY_END);
	ElektraKey * lastSplitSize =
		elektraKeyNew (KDB_CACHE_PREFIX "/lastSplitSize", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (size_t), ELEKTRA_KEY_VALUE, &(split->size), ELEKTRA_KEY_END);
	elektraKeysetAppendKey (global, lastParentName);
	elektraKeysetAppendKey (global, lastParentValue);
	elektraKeysetAppendKey (global, lastInitalParentName);
	elektraKeysetAppendKey (global, lastSplitSize);

	ELEKTRA_LOG_DEBUG ("SIZE STORAGE STORE STUFF");
	for (size_t i = 0; i < split->size; ++i)
	{
		ElektraKey * backendMountpoint = backendGetMountpoint (split->handles[i]);
		// TODO: simplify this code below, seems like this affects only the last split anyway
		if (!split->handles[i] || !backendMountpoint)
		{
			ELEKTRA_LOG_DEBUG (">>>> Skipping split->handle[%ld]: pseudo-backend or no mountpoint", i);
			ELEKTRA_ASSERT (i == (split->size - 1), "ERROR: NOT THE LAST SPLIT");
			continue;
		}
		if (test_bit (split->syncbits[i], 0))
		{
			/* Dont process keysets which come from the user
			   and not from the backends */
			ELEKTRA_LOG_DEBUG (">>>> Skipping split->handle[%ld]: syncbits is 0, keyset from user", i);
			ELEKTRA_ASSERT (i == (split->size - 1), "ERROR: NOT THE LAST SPLIT");
			continue;
		}
		// TODO: simplify this code above, seems like this affects only the last split anyway

		char * name = 0;
		if (strlen (elektraKeyName (backendMountpoint)) != 0)
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/mountpoint/", elektraKeyName (backendMountpoint));
		}
		else
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/", "default/");
		}
		// Append parent name for uniqueness (spec, dir, user, system, ...)
		char * tmp = name;
		name = elektraStrConcat (name, elektraKeyName (split->parents[i]));
		elektraFree (tmp);

		ELEKTRA_LOG_DEBUG (">>>> STORING split->handle[%ld] with name: %s :::: parentName: %s, parentValue: %s", i, name,
				   elektraKeyName (split->parents[i]), elektraKeyString (split->parents[i]));

		ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "splitParentName");
		elektraKeySetString (key, elektraKeyName (split->parents[i]));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "splitParentValue");
		elektraKeySetString (key, elektraKeyString (split->parents[i]));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "specsize");
		elektraKeySetBinary (key, &(split->specsizes[i]), sizeof (ssize_t));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "dirsize");
		elektraKeySetBinary (key, &(split->dirsizes), sizeof (ssize_t));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "usersize");
		elektraKeySetBinary (key, &(split->usersizes[i]), sizeof (ssize_t));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "systemsize");
		elektraKeySetBinary (key, &(split->systemsizes[i]), sizeof (ssize_t));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		key = elektraKeyNew (name, ELEKTRA_KEY_END);
		elektraKeyAddBaseName (key, "syncbits");
		elektraKeySetBinary (key, &(split->syncbits[i]), sizeof (splitflag_t));
		elektraKeysetAppendKey (global, key);
		ELEKTRA_LOG_DEBUG (">>>> STORING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
				   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));

		elektraFree (name);
	}
}

int splitCacheCheckState (Split * split, ElektraKeyset * global)
{
	ELEKTRA_LOG_DEBUG ("SIZE STORAGE CHCK");
	ElektraKey * key = 0;
	char * name = 0;

	ElektraKey * lastSplitSize = elektraKeysetLookupByName (global, KDB_CACHE_PREFIX "/lastSplitSize", ELEKTRA_KDB_O_NONE);
	if (lastSplitSize && elektraKeyGetValueSize (lastSplitSize) == sizeof (size_t))
	{
		size_t lastSize = 0;
		elektraKeyGetBinary (lastSplitSize, &lastSize, sizeof (size_t));
		ELEKTRA_LOG_DEBUG ("Split size check: lastSize %ld, cur size: %ld", lastSize, split->size);
		int bypassedSplits = 1;
		if (lastSize != split->size + bypassedSplits) return -1;
	}
	else
	{
		return -1;
	}

	for (size_t i = 0; i < split->size; ++i)
	{
		ElektraKey * backendMountpoint = backendGetMountpoint (split->handles[i]);
		if (strlen (elektraKeyName (backendMountpoint)) != 0)
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/mountpoint/", elektraKeyName (backendMountpoint));
		}
		else
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/", "default/");
		}
		// Append parent name for uniqueness (spec, dir, user, system, ...)
		char * tmp = name;
		name = elektraStrConcat (name, elektraKeyName (split->parents[i]));
		elektraFree (tmp);
		key = elektraKeyNew (name, ELEKTRA_KEY_END);

		elektraKeyAddBaseName (key, "splitParentName");
		ElektraKey * found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraStrCmp (elektraKeyString (found), elektraKeyName (split->parents[i])) == 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "splitParentValue");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraStrCmp (elektraKeyString (found), elektraKeyString (split->parents[i])) == 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "specsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraKeyGetValueSize (found) == sizeof (ssize_t)) || (split->specsizes[i] > 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "dirsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraKeyGetValueSize (found) == sizeof (ssize_t)) || (split->dirsizes[i] > 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "usersize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraKeyGetValueSize (found) == sizeof (ssize_t)) || (split->usersizes[i] > 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "systemsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraKeyGetValueSize (found) == sizeof (ssize_t)) || (split->systemsizes[i] > 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "syncbits");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraKeyGetValueSize (found) == sizeof (splitflag_t)) || test_bit (split->syncbits[i], 0))
		{
			goto error;
		}

		elektraFree (name);
		name = 0;
		elektraKeyDel (key);
	}

	return 0;

error:
	ELEKTRA_LOG_WARNING ("SIZE STORAGE KEY NOT FOUND");
	ELEKTRA_LOG_DEBUG (">>>> MISSING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
			   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));
	elektraKeyDel (key);
	if (name) elektraFree (name);
	return -1;
}

int splitCacheLoadState (Split * split, ElektraKeyset * global)
{
	ELEKTRA_LOG_DEBUG ("SIZE STORAGE LOAD");
	ElektraKey * key = 0;
	char * name = 0;

	ElektraKey * lastSplitSize = elektraKeysetLookupByName (global, KDB_CACHE_PREFIX "/lastSplitSize", ELEKTRA_KDB_O_NONE);
	if (lastSplitSize && elektraKeyGetValueSize (lastSplitSize) == sizeof (size_t))
	{
		size_t lastSize = 0;
		elektraKeyGetBinary (lastSplitSize, &lastSize, sizeof (size_t));
		ELEKTRA_LOG_DEBUG ("Split size check: lastSize %ld, cur size: %ld", lastSize, split->size);
		int bypassedSplits = 1;
		if (lastSize != split->size + bypassedSplits) return -1;
	}
	else
	{
		return -1;
	}

	for (size_t i = 0; i < split->size; ++i)
	{
		ElektraKey * backendMountpoint = backendGetMountpoint (split->handles[i]);
		if (strlen (elektraKeyName (backendMountpoint)) != 0)
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/mountpoint/", elektraKeyName (backendMountpoint));
		}
		else
		{
			name = elektraStrConcat (KDB_CACHE_PREFIX "/splitState/", "default/");
		}
		// Append parent name for uniqueness (spec, dir, user, system, ...)
		char * tmp = name;
		name = elektraStrConcat (name, elektraKeyName (split->parents[i]));
		elektraFree (tmp);
		key = elektraKeyNew (name, ELEKTRA_KEY_END);

		elektraKeyAddBaseName (key, "splitParentName");
		ElektraKey * found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraStrCmp (elektraKeyString (found), elektraKeyName (split->parents[i])) == 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "splitParentValue");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (!(found && elektraStrCmp (elektraKeyString (found), elektraKeyString (split->parents[i])) == 0))
		{
			goto error;
		}

		elektraKeySetBaseName (key, "specsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (found && elektraKeyGetValueSize (found) == sizeof (ssize_t))
		{
			elektraKeyGetBinary (found, &(split->specsizes[i]), sizeof (ssize_t));
		}
		else
		{
			goto error;
		}

		elektraKeySetBaseName (key, "dirsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (found && elektraKeyGetValueSize (found) == sizeof (ssize_t))
		{
			elektraKeyGetBinary (found, &(split->dirsizes[i]), sizeof (ssize_t));
		}
		else
		{
			goto error;
		}

		elektraKeySetBaseName (key, "usersize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (found && elektraKeyGetValueSize (found) == sizeof (ssize_t))
		{
			elektraKeyGetBinary (found, &(split->usersizes[i]), sizeof (ssize_t));
		}
		else
		{
			goto error;
		}

		elektraKeySetBaseName (key, "systemsize");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (found && elektraKeyGetValueSize (found) == sizeof (ssize_t))
		{
			elektraKeyGetBinary (found, &(split->systemsizes[i]), sizeof (ssize_t));
		}
		else
		{
			goto error;
		}

		elektraKeySetBaseName (key, "syncbits");
		found = elektraKeysetLookup (global, key, ELEKTRA_KDB_O_NONE);
		if (found && elektraKeyGetValueSize (found) == sizeof (splitflag_t))
		{
			elektraKeyGetBinary (found, &(split->syncbits[i]), sizeof (splitflag_t));
		}
		else
		{
			goto error;
		}

		elektraFree (name);
		name = 0;
		elektraKeyDel (key);
	}

	return 0;

error:
	ELEKTRA_LOG_WARNING ("SIZE STORAGE KEY NOT FOUND");
	ELEKTRA_LOG_DEBUG (">>>> MISSING key: %s, string: %s, strlen: %ld, valSize: %ld", elektraKeyName (key), elektraKeyString (key),
			   strlen (elektraKeyString (key)), elektraKeyGetValueSize (key));
	elektraKeyDel (key);
	if (name) elektraFree (name);
	return -1;
}
#endif

ElektraKey * backendsFindParent (ElektraKeyset * backends, const ElektraKey * key)
{
	// TODO (kodebach): performance? Should be fine?
	// With m = number of parts in key, n = size of backends
	// this should be O(m) if backends uses the hashmap, but O(m*log(n)) otherwise
	// The old trie solution would be O(k) where k is the length of the name of key
	// The expectation is that k is bigger than m*log(n) in most cases

	ElektraKey * lookup = elektraKeyDup (key, ELEKTRA_KEY_CP_NAME);
	while (elektraKeyGetUnescapedNameSize (lookup) > 3)
	{
		ElektraKey * parent = elektraKeysetLookup (backends, lookup, 0);
		if (parent != NULL)
		{
			elektraKeyDel (lookup);
			return parent;
		}
		elektraKeySetBaseName (lookup, NULL);
	}

	// lookup root key or fallback to default:/
	ElektraKey * parent = elektraKeysetLookup (backends, lookup, 0);
	return parent != NULL ? parent : elektraKeysetLookupByName (backends, "default:/", 0);
}

ElektraKeyset * backendsForParentKey (ElektraKeyset * backends, ElektraKey * parentKey)
{
	ElektraKeyset * selected = ksBelow (backends, parentKey);
	if (elektraKeyGetNamespace (parentKey) == ELEKTRA_NS_CASCADING)
	{
		// FIXME (kodebach): properly handle cascading backends
		for (elektraNamespace ns = ELEKTRA_NS_FIRST; ns <= ELEKTRA_NS_LAST; ++ns)
		{
			switch (ns)
			{
			case ELEKTRA_NS_PROC:
			case ELEKTRA_NS_DIR:
			case ELEKTRA_NS_USER:
			case ELEKTRA_NS_SYSTEM:
			case ELEKTRA_NS_SPEC:
			case ELEKTRA_NS_DEFAULT:
				elektraKeySetNamespace (parentKey, ns);
				elektraKeysetAppendKey (selected, backendsFindParent (backends, parentKey));
				break;
			case ELEKTRA_NS_META:
			case ELEKTRA_NS_NONE:
			case ELEKTRA_NS_CASCADING:
				break;
			}
		}
		elektraKeySetNamespace (parentKey, ELEKTRA_NS_CASCADING);
	}
	else
	{
		elektraKeysetAppendKey (selected, backendsFindParent (backends, parentKey));
	}
	elektraKeysetAppendKey (selected, elektraKeysetLookupByName (backends, "default:/", 0));
	return selected;
}

static elektraCursor backendsDivideInternal (ElektraKeyset * backends, elektraCursor * curBackend, const ElektraKeyset * ks, elektraCursor cur)
{
	ElektraKey * defaultBackendKey = elektraKeysetLookupByName (backends, "default:/", 0);
	if (defaultBackendKey == NULL && *curBackend < 0)
	{
		// happens during bootstrap
		*curBackend = 0;
	}

	const BackendData * defaultBackendData = elektraKeyValue (defaultBackendKey);
	ElektraKey * backendKey = *curBackend < 0 ? defaultBackendKey : elektraKeysetAtCursor (backends, *curBackend);
	BackendData * backendData = (BackendData *) elektraKeyValue (backendKey);

	while (cur < elektraKeysetGetSize (ks))
	{
		ElektraKey * k = elektraKeysetAtCursor (ks, cur);
		ElektraKey * nextBackendKey = *curBackend >= elektraKeysetGetSize (backends) - 1 ? defaultBackendKey : elektraKeysetAtCursor (backends, *curBackend + 1);

		if (elektraKeyIsBelowOrSame (defaultBackendKey, k) == 1)
		{
			elektraKeysetAppendKey (defaultBackendData->keys, elektraKeyDup (k, ELEKTRA_KEY_CP_ALL));
		}
		// nextBackendKey == NULL happens during bootstrap
		else if (nextBackendKey != NULL && elektraKeyCmp (k, nextBackendKey) >= 0)
		{
			++*curBackend;
			cur = backendsDivideInternal (backends, curBackend, ks, cur);
			continue;
		}
		else if (*curBackend < 0 || elektraKeyIsBelowOrSame (backendKey, k) == 1)
		{
			backendData->keyNeedsSync = backendData->keyNeedsSync || elektraKeyNeedSync (k) == 1;
			elektraKeysetAppendKey (backendData->keys, elektraKeyDup (k, ELEKTRA_KEY_CP_ALL));
		}
		else
		{
			break;
		}

		cur++;
	}

	return cur;
}

bool backendsDivide (ElektraKeyset * backends, const ElektraKeyset * ks)
{
	for (elektraCursor i = 0; i < elektraKeysetGetSize (backends); i++)
	{
		BackendData * backendData = (BackendData *) elektraKeyValue (elektraKeysetAtCursor (backends, i));
		backendData->keyNeedsSync = false;
		elektraKeysetClear (backendData->keys);
	}


	elektraCursor curBackend = -1;
	elektraCursor ret = backendsDivideInternal (backends, &curBackend, ks, 0);
	return ret == elektraKeysetGetSize (ks);
}

void backendsMerge (ElektraKeyset * backends, ElektraKeyset * ks)
{
	for (elektraCursor i = 0; i < elektraKeysetGetSize (backends); i++)
	{
		const ElektraKey * backendKey = elektraKeysetAtCursor (backends, i);
		BackendData * backendData = (BackendData *) elektraKeyValue (backendKey);

		if (elektraKeyGetNamespace (backendKey) != ELEKTRA_NS_DEFAULT)
		{
			ssize_t size = elektraKeysetGetSize (backendData->keys);
			backendData->getSize = size;
			for (elektraCursor j = 0; j < size; j++)
			{
				elektraKeyClearSync (elektraKeysetAtCursor (backendData->keys, j));
			}
			elektraKeysetAppend (ks, backendData->keys);
		}
	}
}
