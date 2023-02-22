#include <kdbprivate.h>
#include <kdbchangetracking.h>

/**
 * Returns the changetracking context of the given KDB instance
 *
 * @param kdb the KDB instance
 * @return the changetracking context or @p NULL if @p kdb is NULL
 */
const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb)
{
	if (kdb == NULL)
	{
		return NULL;
	}

	return &kdb->changeTrackingContext;
}

/**
 * @internal
 *
 * @brief For testing purposes only: Create a ChangeTrackingContext from a keyset
 *
 * @param oldKeys the old keys
 * @return a new changetracking context
 */
ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys)
{
	ChangeTrackingContext * context = elektraCalloc (sizeof (ChangeTrackingContext));

	ksIncRef (oldKeys);
	context->oldKeys = oldKeys;

	return context;
}

/**
 * @internal
 *
 * @brief For testing purposes only: Delete a ChangeTrackingContext object
 *
 * @param context the object to delete
 */
void elektraChangeTrackingContextDel (ChangeTrackingContext * context)
{
	if (context->oldKeys != NULL)
	{
		ksDecRef (context->oldKeys);
		ksDel (context->oldKeys);
		context->oldKeys = NULL;
	}

	elektraFree (context);
}

/**
 * Determines whether two keys have different value
 *
 * @param new new key
 * @param old old key
 * @return true if @p new and @p old have a different value
 */
static bool keyValueDifferent (Key * new, Key * old)
{
	if (new->keyData == old->keyData)
	{
		return false;
	}

	if (new->keyData == NULL || old->keyData == NULL)
	{
		// one is NULL, the other one isn't -> modified
		return true;
	}

	if (keyIsString (new) != keyIsString (old))
	{
		// different data types -> modified
		return true;
	}

	if (keyIsString (new))
	{
		if (strcmp (keyString (new), keyString (old)) != 0)
		{
			// different value -> modified
			return true;
		}
	}
	else
	{
		if (new->keyData->dataSize != old->keyData->dataSize)
		{
			// different datasize -> modified
			return true;
		}

		if (memcmp (new->keyData->data.v, old->keyData->data.v, new->keyData->dataSize) != 0)
		{
			// different value -> modified
			return true;
		}
	}

	return false;
}

/**
 * Find the differences between two keysets
 *
 * @param new the new keyset
 * @param old the old keyset
 * @param addedKeys adds keys present in @p new but not in @p old
 * @param removedKeys adds keys present in @p old but not in @p new
 * @param modifiedKeys adds keys present in both @p new and @p old, but with changes in value or in the meta keys
 * @param parentKey parent key - if this parameter is not NULL, only keys below or same are processed.
 */
static void findDifferences (KeySet * new, KeySet * old, KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKeys, const Key * parentKey)
{
	KeySet * metaAdded = ksNew (0, KS_END);
	KeySet * metaRemoved = ksNew(0, KS_END);
	KeySet * metaModified = ksNew(0, KS_END);

	for (elektraCursor itOld = 0; itOld < ksGetSize (old); itOld++)
	{
		Key * needle = ksAtCursor (old, itOld);

		if (parentKey != NULL && !keyIsBelowOrSame (parentKey, needle))
		{
			continue;
		}

		Key * found = ksLookup (new, needle, 0);

		if(found == NULL)
		{
			// key is present in old key set, but not in new --> removed
			ksAppendKey (removedKeys, needle);
		}
		else
		{
			// key is present in both new and old key sets --> check for modifications

			if (keyValueDifferent (found, needle))
			{
				// The value of the key changed --> modified
				ksAppendKey (modifiedKeys, needle);
			}
			else if (keyGetNamespace (found) != KEY_NS_META)
			{
				// Check whether something in the meta keys has changed

				KeySet * oldMeta = keyMeta (needle);
				KeySet * newMeta = keyMeta (found);

				ksClear (metaAdded);
				ksClear (metaRemoved);
				ksClear (metaModified);

				findDifferences (newMeta, oldMeta, metaAdded, metaRemoved, metaModified, NULL);

				if (ksGetSize (addedKeys) > 0 || ksGetSize (removedKeys) > 0 || ksGetSize (modifiedKeys) > 0)
				{
					// there was a change in the meta keys --> modified
					ksAppendKey (modifiedKeys, needle);
				}
			}
		}
	}

	for (elektraCursor itNew = 0; itNew < ksGetSize (new); itNew++)
	{
		Key * needle = ksAtCursor (new, itNew);

		if (parentKey != NULL && !keyIsBelowOrSame (parentKey, needle))
		{
			continue;
		}

		Key * found = ksLookup (old, needle, 0);

		if(found == NULL)
		{
			// Key is present in the new keyset but not in the old --> added
			ksAppendKey (addedKeys, needle);
		}
	}

	ksDel (metaAdded);
	ksDel (metaRemoved);
	ksDel (metaModified);
}

/**
 * Calculates the changes between the provided KeySet and the given ChangeTrackingContext.
 *
 * @param newKeys the KeySet
 * @param context the ChangeTrackingContext
 * @param parentKey if not @p NULL, only keys below or same of this key are considered
 * @return the changes between @p newKeys and @p context OR @p NULL if either one of them is @p NULL
 */
KeySetDiff * elektraChangeTrackingCalculateFromContext (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey)
{
	if (newKeys == NULL || context == NULL)
	{
		return NULL;
	}

	return elektraChangeTrackingCalculateFromKeySets (newKeys, context->oldKeys, parentKey);
}

/**
 * Calculate the changes between the provided KeySets.
 *
 * @param newKeys the new keys
 * @param oldKeys the old keys
 * @param parentKey if not @p NULL, only keys below or same of this key are considered
 * @return the changes between @p newKeys and @p oldKeys OR @p NULL if either one of them is @p NULL
 */
KeySetDiff * elektraChangeTrackingCalculateFromKeySets (KeySet * newKeys, KeySet * oldKeys, Key * parentKey)
{
	if (newKeys == NULL || oldKeys == NULL)
	{
		return NULL;
	}

	KeySetDiff * ksd = elektraCalloc (sizeof (KeySetDiff));

	ksd->parentKey = keyDup (parentKey, KEY_CP_ALL);
	ksd->addedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->addedKeys);
	ksd->removedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->removedKeys);
	ksd->modifiedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->modifiedKeys);

	findDifferences (newKeys, oldKeys, ksd->addedKeys, ksd->removedKeys, ksd->modifiedKeys, parentKey);

	return ksd;
}

/**
 * Mostly for testing purposes: Create a KeySetDiff.
 * The returned KeySetDiff contains the same KeySets that are passed in, so be sure
 * to @p ksIncRef them if you plan to use them after deleting the KeySetDiff.
 *
 * @param addedKeys the added keys
 * @param removedKeys the removed keys
 * @param modifiedKey the modified keys
 * @param parentKey the parent key
 * @return KeySetDiff with the provided parameters
 */
KeySetDiff * elektraChangeTrackingCreateKeySetDiff (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKey, Key * parentKey)
{
	KeySetDiff * ksd = elektraCalloc (sizeof (KeySetDiff));

	ksd->parentKey = keyDup (parentKey, KEY_CP_ALL);

	ksIncRef (addedKeys);
	ksd->addedKeys = addedKeys;

	ksIncRef (removedKeys);
	ksd->removedKeys = removedKeys;

	ksIncRef (modifiedKey);
	ksd->modifiedKeys = modifiedKey;

	return ksd;
}

/**
 * Delete a KeySetDiff
 * @param ksd the KeySetDiff to delete
 */
void elektraChangeTrackingKeySetDiffDel (KeySetDiff * ksd)
{
	if (ksd == NULL)
	{
		return;
	}

	if (ksd->refs > 0)
	{
		return;
	}

	keyDel (ksd->parentKey);

	ksDecRef (ksd->modifiedKeys);
	ksDel (ksd->modifiedKeys);

	ksDecRef (ksd->removedKeys);
	ksDel (ksd->removedKeys);

	ksDecRef (ksd->addedKeys);
	ksDel (ksd->addedKeys);

	elektraFree (ksd);
}

/**
 * Get the parent key of the given KeySetDiff
 * @param ksd the KeySetDiff
 * @return the parent key (which may be NULL) OR NULL if @p ksd is @p NULL
 */
const Key * elektraChangeTrackingKeySetDiffGetParentKey (const KeySetDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksd->parentKey;
}

/**
 * Get the added keys
 *
 * @param ksd the KeySetDiff
 * @return a new KeySet containing the added keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraChangeTrackingGetAddedKeys (const KeySetDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup(ksd->addedKeys);
}

/**
 * Get the removed keys
 *
 * @param ksd the KeySetDiff
 * @return a new KeySet containing the removed keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraChangeTrackingGetRemovedKeys (const KeySetDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup(ksd->removedKeys);
}

/**
 * Get the modified keys.
 * This will return the old keys.
 *
 * @param ksd the KeySetDiff
 * @return a new KeySet containing the modified keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraChangeTrackingGetModifiedKeys (const KeySetDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup(ksd->modifiedKeys);
}

/**
 * Check whether the value of a specific key has been modified.
 * If it returns @p true, the value of the key has been modified.
 *
 * @param ksd
 * @param key
 * @return @p true if the value of the key was modified OR
 *         @p false if it hasn't been OR
 *         @p ksd is NULL OR
 *         @p key is NULL OR
 *         @p key is not contained in @p ksd
 */
bool elektraChangeTrackingValueChanged (const KeySetDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return false;
	}

	Key * old = ksLookup (ksd->modifiedKeys, key, 0);
	return keyValueDifferent (key, old);
}

/**
 * Check whether the metadata of a specific key has been modified.
 * If it returns @p true, the metadata of the key has been modified.
 *
 * @param ksd
 * @param key
 * @return @p true if the metadata of the key was modified OR
 *         @p false if it hasn't been OR
 *         @p ksd is NULL OR
 *         @p key is NULL OR
 *         @p key is not contained in @p ksd
 */
bool elektraChangeTrackingMetaChanged (const KeySetDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return false;
	}

	Key * old = ksLookup (ksd->modifiedKeys, key, 0);

	// if the value is not different -> meta is different
	return !keyValueDifferent (key, old);
}

/**
 * Get metakeys added to the specific keys.
 *
 * @param ksd the KeySetDiff
 * @param key the key of which you want to get added metadata
 * @return a new KeySet that contains metadata added to the key OR
 *         @p NULL if @p ksd is NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraChangeTrackingGetAddedMetaKeys (const KeySetDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return NULL;
	}

	Key * oldKey = ksLookup (ksd->modifiedKeys, key, 0);

	if (oldKey == NULL)
	{
		return NULL;
	}

	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	findDifferences (keyMeta (key), keyMeta (oldKey), addedKeys, removedKeys, modifiedKeys, NULL);

	ksDel (removedKeys);
	ksDel (modifiedKeys);

	return addedKeys;
}

/**
 * Get metakeys removed from the specific keys.
 *
 * @param ksd the KeySetDiff
 * @param key the key of which you want to get removed metadata
 * @return a new KeySet that contains metadata removed from the key OR
 *         @p NULL if @p ksd is NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraChangeTrackingGetRemovedMetaKeys (const KeySetDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return NULL;
	}

	Key * oldKey = ksLookup (ksd->modifiedKeys, key, 0);

	if (oldKey == NULL)
	{
		return NULL;
	}

	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	findDifferences (keyMeta (key), keyMeta (oldKey), addedKeys, removedKeys, modifiedKeys, NULL);

	ksDel (addedKeys);
	ksDel (modifiedKeys);

	return removedKeys;
}

/**
 * Get modified metakeys of the specific keys.
 *
 * @param ksd the KeySetDiff
 * @param key the key of which you want to get modified metadata
 * @return a new KeySet that contains modified metadata of the key OR
 *         @p NULL if @p ksd is NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraChangeTrackingGetModifiedMetaKeys (const KeySetDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return NULL;
	}

	Key * oldKey = ksLookup (ksd->modifiedKeys, key, 0);

	if (oldKey == NULL)
	{
		return NULL;
	}

	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	findDifferences (keyMeta (key), keyMeta (oldKey), addedKeys, removedKeys, modifiedKeys, NULL);

	ksDel (removedKeys);
	ksDel (addedKeys);

	return modifiedKeys;
}

/**
 * @brief Increment the reference counter of a KeySetDiff object
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p ksd's reference counter is > 0
 * @post @p ksd's reference counter is <= UINT16_MAX - 1
 *
 * @param ksd the KeySetDiff object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 */
uint16_t elektraChangeTrackingKeySetDiffIncRef (KeySetDiff * ksd)
{
	if (!ksd)
	{
		return UINT16_MAX;
	}

	if (ksd->refs == UINT16_MAX - 1)
	{
		return UINT16_MAX;
	}

	ksd->refs++;
	return ksd->refs;
}

/**
 * @brief Decrement the reference counter of a KeySetDiff object
 *
 * @post @p ksd's reference counter is >= 0
 * @post @p ksd's reference counter is < SSIZE_MAX
 *
 * @param ksd the KeySetDiff object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 */
uint16_t elektraChangeTrackingKeySetDiffDecRef (KeySetDiff * ksd)
{
	if (!ksd)
	{
		return UINT16_MAX;
	}

	if (ksd->refs == 0)
	{
		return 0;
	}

	ksd->refs--;
	return ksd->refs;
}
