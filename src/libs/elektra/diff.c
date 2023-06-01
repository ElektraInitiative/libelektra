#include <elektra/diff.h>

#include <internal/diff.h>
#include <internal/kdbprivate.h>

/**
 * Determines whether two keys have a different value
 *
 * @param new new key
 * @param old old key
 * @return true if @p new and @p old have a different value
 */
static inline bool keyValueDifferent (Key * new, Key * old)
{
	if (new == old)
	{
		return false;
	}

	if (new == NULL || old == NULL)
	{
		return true;
	}

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
 * Any of the out parameters (@p addedKeys, @p removedKey, @p modifiedKeys) may be @p NULL.
 * They will then not be calculated.
 *
 * @param[in] new the new keyset
 * @param[in] old the old keyset
 * @param[out] addedKeys adds keys present in @p new but not in @p old
 * @param[out] removedKeys adds keys present in @p old but not in @p new
 * @param[out] modifiedKeys adds keys present in both @p new and @p old, but with changes in value or in the meta keys
 * @param[in] parentKey parent key - if this parameter is not @p NULL, only keys below or same are processed.
 */
static void findDifferences (KeySet * new, KeySet * old, KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKeys,
			     const Key * parentKey)
{
	KeySet * metaAdded = ksNew (0, KS_END);
	KeySet * metaRemoved = ksNew (0, KS_END);
	KeySet * metaModified = ksNew (0, KS_END);

	ssize_t oldSize = ksGetSize (old);
	ssize_t newSize = ksGetSize (new);

	elektraCursor iOld = 0;
	elektraCursor iNew = 0;

	for (ssize_t i = 0; i < oldSize + newSize; i++)
	{
		Key * oldKey = ksAtCursor (old, iOld);
		Key * newKey = ksAtCursor (new, iNew);

		if (oldKey == NULL && newKey == NULL)
		{
			break;
		}

		if (newKey == NULL)
		{
			// No more keys in new --> must have been removed

			if (removedKeys && keyGetNamespace (oldKey) != KEY_NS_CASCADING &&
			    (parentKey == NULL || keyIsBelowOrSame (parentKey, oldKey)))
			{
				ksAppendKey (removedKeys, oldKey);
			}

			iOld++;
			continue;
		}

		if (oldKey == NULL)
		{
			// No more keys in old --> must have been added

			if (addedKeys && keyGetNamespace (newKey) != KEY_NS_CASCADING &&
			    (parentKey == NULL || keyIsBelowOrSame (parentKey, newKey)))
			{
				ksAppendKey (addedKeys, newKey);
			}

			iNew++;
			continue;
		}

		int cmpRes = keyCmp (oldKey, newKey);

		if (modifiedKeys && cmpRes == 0)
		{
			// keys have the same name --> check if modified

			if (keyGetNamespace (oldKey) == KEY_NS_CASCADING)
			{
				iOld++;
				iNew++;
				continue;
			}

			if (parentKey != NULL && !keyIsBelowOrSame (parentKey, newKey))
			{
				iOld++;
				iNew++;
				continue;
			}

			if (keyValueDifferent (newKey, oldKey))
			{
				// The value of the key changed --> modified
				ksAppendKey (modifiedKeys, oldKey);
			}
			else if (keyGetNamespace (oldKey) != KEY_NS_META)
			{
				// Check whether something in the meta keys has changed

				KeySet * oldMeta = keyMetaNoAlloc (oldKey);
				KeySet * newMeta = keyMetaNoAlloc (newKey);

				ssize_t oldMetaSize = oldMeta ? ksGetSize (oldMeta) : 0;
				ssize_t newMetaSize = newMeta ? ksGetSize (newMeta) : 0;

				if (oldMetaSize != newMetaSize)
				{
					// there was a change in the meta keys --> modified
					ksAppendKey (modifiedKeys, oldKey);
				}
				else if (oldMetaSize > 0 || newMetaSize > 0)
				{
					ksClear (metaAdded);
					ksClear (metaRemoved);
					ksClear (metaModified);

					findDifferences (newMeta, oldMeta, metaAdded, metaRemoved, metaModified, NULL);

					if (ksGetSize (metaAdded) > 0 || ksGetSize (metaRemoved) > 0 || ksGetSize (metaModified) > 0)
					{
						// there was a change in the meta keys --> modified
						ksAppendKey (modifiedKeys, oldKey);
					}
				}
			}

			iOld++;
			iNew++;
		}
		else if (cmpRes == 0)
		{
			iOld++;
			iNew++;
		}
		else if (cmpRes < 0)
		{
			// key is present in old but not in new
			if (removedKeys && keyGetNamespace (oldKey) != KEY_NS_CASCADING &&
			    (parentKey == NULL || keyIsBelowOrSame (parentKey, oldKey)))
			{
				ksAppendKey (removedKeys, oldKey);
			}

			iOld++;
		}
		else
		{
			// key is present in new but not in old
			if (addedKeys && keyGetNamespace (newKey) != KEY_NS_CASCADING &&
			    (parentKey == NULL || keyIsBelowOrSame (parentKey, newKey)))
			{
				ksAppendKey (addedKeys, newKey);
			}

			iNew++;
		}
	}

	ksDel (metaAdded);
	ksDel (metaRemoved);
	ksDel (metaModified);
}

/**
 * Calculate the changes between the provided KeySets.
 *
 * @param newKeys the new keys
 * @param oldKeys the old keys
 * @param parentKey if not @p NULL, only keys below or same of this key are considered
 * @return the changes between @p newKeys and @p oldKeys OR @p NULL if either one of them is @p NULL
 */
ElektraDiff * elektraDiffCalculate (KeySet * newKeys, KeySet * oldKeys, Key * parentKey)
{
	if (newKeys == NULL || oldKeys == NULL)
	{
		return NULL;
	}

	ElektraDiff * ksd = elektraCalloc (sizeof (ElektraDiff));

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
 * Mostly for testing purposes: Create a ElektraDiff.
 * The returned ElektraDiff contains the same KeySets that are passed in, so be sure
 * to @p ksIncRef them if you plan to use them after deleting the ElektraDiff.
 *
 * @param addedKeys the added keys
 * @param removedKeys the removed keys
 * @param modifiedKey the modified keys
 * @param parentKey the parent key
 * @return ElektraDiff with the provided parameters
 */
ElektraDiff * elektraDiffNew (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKey, Key * parentKey)
{
	ElektraDiff * ksd = elektraCalloc (sizeof (ElektraDiff));

	if (parentKey != NULL)
	{
		ksd->parentKey = keyDup (parentKey, KEY_CP_ALL);
	}

	ksIncRef (addedKeys);
	ksd->addedKeys = addedKeys;

	ksIncRef (removedKeys);
	ksd->removedKeys = removedKeys;

	ksIncRef (modifiedKey);
	ksd->modifiedKeys = modifiedKey;

	return ksd;
}

/**
 * Delete a ElektraDiff
 * @param ksd the ElektraDiff to delete
 */
void elektraDiffDel (ElektraDiff * ksd)
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
 * Determine whether the given diff is empty
 * @param ksd the diff
 * @return @p true if diff is empty, @p false otherwise. Will also return @p true if @p ksd is @p NULL.
 */
bool elektraDiffIsEmpty (const ElektraDiff * ksd)
{
	if (ksd == NULL)
	{
		return true;
	}

	ssize_t keys = ksGetSize (ksd->addedKeys) + ksGetSize (ksd->removedKeys) + ksGetSize (ksd->modifiedKeys);
	return keys == 0;
}

/**
 * Get the parent key of the given ElektraDiff
 * @param ksd the ElektraDiff
 * @return the parent key (which may be @p NULL) OR @p NULL if @p ksd is @p NULL
 */
const Key * elektraDiffGetParentKey (const ElektraDiff * ksd)
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
 * @param ksd the ElektraDiff
 * @return a new KeySet containing the added keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraDiffGetAddedKeys (const ElektraDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup (ksd->addedKeys);
}

/**
 * Get the removed keys
 *
 * @param ksd the ElektraDiff
 * @return a new KeySet containing the removed keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraDiffGetRemovedKeys (const ElektraDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup (ksd->removedKeys);
}

/**
 * Get the modified keys.
 * This will return the old keys (pre modification).
 *
 * @param ksd the ElektraDiff
 * @return a new KeySet containing the modified keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraDiffGetModifiedKeys (const ElektraDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	return ksDup (ksd->modifiedKeys);
}

/**
 * Check whether the value of a specific key has been modified.
 * If it returns @p true, the value of the key has been modified.
 *
 * @param ksd
 * @param key
 * @return @p true if the value of the key has been modified OR
 *         @p false if it hasn't been OR
 *         @p ksd is @p NULL OR
 *         @p key is @p NULL OR
 *         @p key is not contained in @p ksd
 */
bool elektraDiffKeyValueChanged (const ElektraDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return false;
	}

	Key * old = ksLookup (ksd->modifiedKeys, key, 0);
	if (old == NULL)
	{
		return false;
	}

	return keyValueDifferent (key, old);
}

/**
 * Check whether the metadata of a specific key has been modified.
 * If it returns @p true, the metadata of the key has been modified.
 *
 * @param ksd
 * @param key
 * @return @p true if the metadata of the key has been modified OR
 *         @p false if it hasn't been OR
 *         @p ksd is @p NULL OR
 *         @p key is @p NULL OR
 *         @p key is not contained in @p ksd
 */
bool elektraDiffKeyOnlyMetaChanged (const ElektraDiff * ksd, Key * key)
{
	if (ksd == NULL || key == NULL)
	{
		return false;
	}

	Key * old = ksLookup (ksd->modifiedKeys, key, 0);
	if (old == NULL)
	{
		return false;
	}

	// if the value is not different -> meta is different
	return !keyValueDifferent (key, old);
}

/**
 * Get metakeys added to the specific keys.
 *
 * @param ksd the ElektraDiff
 * @param key the key of which you want to get added metadata
 * @return a new KeySet that contains metadata added to the key OR
 *         @p NULL if @p ksd is @p NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraDiffGetAddedMetaKeys (const ElektraDiff * ksd, Key * key)
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

	findDifferences (keyMeta (key), keyMeta (oldKey), addedKeys, NULL, NULL, NULL);

	return addedKeys;
}

/**
 * Get metakeys removed from the specific keys.
 *
 * @param ksd the ElektraDiff
 * @param key the key of which you want to get removed metadata
 * @return a new KeySet that contains metadata removed from the key OR
 *         @p NULL if @p ksd is @p NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraDiffGetRemovedMetaKeys (const ElektraDiff * ksd, Key * key)
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

	KeySet * removedKeys = ksNew (0, KS_END);

	findDifferences (keyMeta (key), keyMeta (oldKey), NULL, removedKeys, NULL, NULL);

	return removedKeys;
}

/**
 * Get modified metakeys of the specific keys.
 *
 * @param ksd the ElektraDiff
 * @param key the key of which you want to get modified metadata
 * @return a new KeySet that contains modified metadata of the key OR
 *         @p NULL if @p ksd is @p NULL OR @p key is @p NULL OR @p ksd does not contain @p key.
 */
KeySet * elektraDiffGetModifiedMetaKeys (const ElektraDiff * ksd, Key * key)
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

	KeySet * modifiedKeys = ksNew (0, KS_END);

	findDifferences (keyMeta (key), keyMeta (oldKey), NULL, NULL, modifiedKeys, NULL);

	return modifiedKeys;
}

/**
 * @brief Increment the reference counter of a ElektraDiff object
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p ksd's reference counter is > 0
 * @post @p ksd's reference counter is <= UINT16_MAX - 1
 *
 * @param ksd the ElektraDiff object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on @p NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 */
uint16_t elektraDiffIncRef (ElektraDiff * ksd)
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
 * @brief Decrement the reference counter of a ElektraDiff object
 *
 * @post @p ksd's reference counter is >= 0
 * @post @p ksd's reference counter is < UINT16_MAX - 1
 *
 * @param ksd the ElektraDiff object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on @p NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 */
uint16_t elektraDiffDecRef (ElektraDiff * ksd)
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

/**
 * @brief Returns the reference counter of a ElektraDiff object

 * @param ksd the ElektraDiff object whose reference counter should get returned
 *
 * @return the value of the reference counter
 * @retval UINT16_MAX on @p NULL pointer
 */
uint16_t elektraDiffGetRef (ElektraDiff * ksd)
{
	if (!ksd)
	{
		return UINT16_MAX;
	}

	return ksd->refs;
}
