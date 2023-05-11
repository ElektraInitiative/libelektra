#include <kdbdiff.h>
#include <kdberrors.h>
#include <kdbprivate.h>

static void findDifferences (KeySet * new, KeySet * old, KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKeys,
			     KeySet * modifiedKeysNewValues, const Key * parentKey);

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
 * Determines whether the metadata of two keys are different
 *
 * @param[in] oldKey old key
 * @param[in] newKey new key
 * @param[out] metaAdded storage for the meta keys added in new key
 * @param[out] metaRemoved storage for the meta keys removed in new key
 * @param[out] metaModified storage for the meta keys modified in new key
 * @return @p true if they have different values. @p false otherwise.
 */
static inline bool keyMetaDifferent (Key * oldKey, Key * newKey, KeySet * metaAdded, KeySet * metaRemoved, KeySet * metaModified)
{
	KeySet * oldMeta = keyMetaNoAlloc (oldKey);
	KeySet * newMeta = keyMetaNoAlloc (newKey);

	if (oldMeta == newMeta)
	{
		return false;
	}

	if (oldMeta != NULL && newMeta != NULL && oldMeta->data == newMeta->data)
	{
		return false;
	}

	ssize_t oldMetaSize = oldMeta ? ksGetSize (oldMeta) : 0;
	ssize_t newMetaSize = newMeta ? ksGetSize (newMeta) : 0;

	if (oldMetaSize != newMetaSize)
	{
		// there was a change in the meta keys --> modified
		return true;
	}
	else if (oldMetaSize > 0 || newMetaSize > 0)
	{
		ksClear (metaAdded);
		ksClear (metaRemoved);
		ksClear (metaModified);

		findDifferences (newMeta, oldMeta, metaAdded, metaRemoved, metaModified, NULL, NULL);

		if (ksGetSize (metaAdded) > 0 || ksGetSize (metaRemoved) > 0 || ksGetSize (metaModified) > 0)
		{
			// there was a change in the meta keys --> modified
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
 * @param[out] modifiedKeys adds keys present in both @p new and @p old, but with changes in value or in the meta keys. contains the OLD
 * keys
 * @param[out] modifiedKeysNewValues adds keys present in both @p new and @p old, but with changes in value or in the meta keys. contains
 * the NEW keys
 * @param[in] parentKey parent key - if this parameter is not @p NULL, only keys below or same are processed.
 */
static void findDifferences (KeySet * new, KeySet * old, KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKeys,
			     KeySet * modifiedKeysNewValues, const Key * parentKey)
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
				if (modifiedKeysNewValues != NULL)
				{
					ksAppendKey (modifiedKeysNewValues, newKey);
				}
			}
			else if (keyGetNamespace (oldKey) != KEY_NS_META)
			{
				// Check whether something in the meta keys has changed
				if (keyMetaDifferent (oldKey, newKey, metaAdded, metaRemoved, metaModified))
				{
					ksAppendKey (modifiedKeys, oldKey);
					if (modifiedKeysNewValues != NULL)
					{
						ksAppendKey (modifiedKeysNewValues, newKey);
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
	keyIncRef (ksd->parentKey);
	ksd->addedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->addedKeys);
	ksd->removedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->removedKeys);
	ksd->modifiedKeys = ksNew (0, KS_END);
	ksIncRef (ksd->modifiedKeys);
	ksd->modifiedNewKeys = ksNew (0, KS_END);
	ksIncRef (ksd->modifiedNewKeys);

	findDifferences (newKeys, oldKeys, ksd->addedKeys, ksd->removedKeys, ksd->modifiedKeys, ksd->modifiedNewKeys, parentKey);

	return ksd;
}

/**
 * Create an ElektraDiff.
 * The returned ElektraDiff contains the same KeySets that are passed in, so be sure
 * to @p ksIncRef them if you plan to use them after deleting the ElektraDiff.
 *
 * Same goes for the parent key, please use @p keyIncRef if you plan to use it after
 * deleting the ElektraDiff
 *
 * @param addedKeys the added keys
 * @param removedKeys the removed keys
 * @param modifiedKeys the modified keys
 * @param modifiedNewKeys the modified keys with the new values
 * @param parentKey the parent key
 * @return ElektraDiff with the provided parameters
 */
ElektraDiff * elektraDiffNew (KeySet * addedKeys, KeySet * removedKeys, KeySet * modifiedKeys, KeySet * modifiedNewKeys, Key * parentKey)
{
	ElektraDiff * ksd = elektraCalloc (sizeof (ElektraDiff));

	keyIncRef (parentKey);
	ksd->parentKey = parentKey;

	ksIncRef (addedKeys);
	ksd->addedKeys = addedKeys;

	ksIncRef (removedKeys);
	ksd->removedKeys = removedKeys;

	ksIncRef (modifiedKeys);
	ksd->modifiedKeys = modifiedKeys;

	if (modifiedNewKeys != NULL)
	{
		ksIncRef (modifiedNewKeys);
		ksd->modifiedNewKeys = modifiedNewKeys;
	}

	return ksd;
}

/**
 * Duplicate the given diff
 * @param original the diff to duplicate
 * @return duplicated version of the given diff
 */
ElektraDiff * elektraDiffDup (const ElektraDiff * original)
{
	return elektraDiffNew (ksDup (original->addedKeys), ksDup (original->removedKeys), ksDup (original->modifiedKeys),
			       ksDup (original->modifiedNewKeys), keyDup (original->parentKey, KEY_CP_ALL));
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

	keyDecRef (ksd->parentKey);
	keyDel (ksd->parentKey);

	ksDecRef (ksd->modifiedKeys);
	ksDel (ksd->modifiedKeys);

	ksDecRef (ksd->removedKeys);
	ksDel (ksd->removedKeys);

	ksDecRef (ksd->addedKeys);
	ksDel (ksd->addedKeys);

	if (ksd->modifiedNewKeys)
	{
		ksDecRef (ksd->modifiedNewKeys);
		ksDel (ksd->modifiedNewKeys);
	}

	elektraFree (ksd);
}

/**
 * Removes all keys from the diff that are same or below the given cutpoint
 *
 * @param ksd the diff where the keys should be removed
 * @param cutpoint the cutpoint
 */
void elektraDiffRemoveSameOrBelow (ElektraDiff * ksd, const Key * cutpoint)
{
	if (ksd == NULL || cutpoint == NULL)
	{
		return;
	}

	if (ksd->addedKeys) ksDel (ksCut (ksd->addedKeys, cutpoint));
	if (ksd->removedKeys) ksDel (ksCut (ksd->removedKeys, cutpoint));
	if (ksd->modifiedKeys) ksDel (ksCut (ksd->modifiedKeys, cutpoint));
	if (ksd->modifiedNewKeys) ksDel (ksCut (ksd->modifiedNewKeys, cutpoint));
}

/**
 * Creates a new diff with all keys from @p original that are same or below @p cutpoint.
 * The affected keys are removed from @p original
 *
 * @param original the original diff
 * @param cutpoint the cutpoint
 * @return new diff with all the keys same or below @p cutpoint,
 *         OR @p NULL if @p original is @p NULL
 *         OR @p NULL if @p cutpoint is @p NULL
 */
ElektraDiff * elektraDiffCut (ElektraDiff * original, const Key * cutpoint)
{
	if (original == NULL || cutpoint == NULL)
	{
		return NULL;
	}

	ElektraDiff * new = elektraCalloc (sizeof (ElektraDiff));
	new->parentKey = keyDup (cutpoint, KEY_CP_ALL);
	if (new->parentKey) keyIncRef (new->parentKey);

	new->addedKeys = ksCut (original->addedKeys, cutpoint);
	new->removedKeys = ksCut (original->removedKeys, cutpoint);
	new->modifiedKeys = ksCut (original->modifiedKeys, cutpoint);
	new->modifiedNewKeys = ksCut (original->modifiedNewKeys, cutpoint);

	if (new->addedKeys) ksIncRef (new->addedKeys);
	if (new->removedKeys) ksIncRef (new->removedKeys);
	if (new->modifiedKeys) ksIncRef (new->modifiedKeys);
	if (new->modifiedNewKeys) ksIncRef (new->modifiedNewKeys);

	return new;
}

/**
 * @internal
 *
 * Removes all keys that are not at or below @p cutpoint from the keyset
 * After execution, @p ks will point to another keyset!
 *
 * @param ks the keyset that shall be cut.
 *           It will be pointed to another keyset!
 *           The original keyset will be deleted.
 * @param cutpoint the point to preserve
 */
static void cutOtherKeys (KeySet ** ks, const Key * cutpoint)
{
	KeySet * below = ksCut (*ks, cutpoint);

	ksDecRef (*ks);
	ksDel (*ks);

	ksIncRef (below);
	*ks = below;
}

/**
 * Removes all the keys from the diff that are NOT same or below the given parentkey
 *
 * @param ksd the diff where the keys should be removed
 * @param parentKey the parent key
 */
void elektraDiffRemoveOther (ElektraDiff * ksd, const Key * parentKey)
{
	if (ksd == NULL || parentKey == NULL)
	{
		return;
	}

	if (ksd->parentKey != NULL)
	{
		keyDecRef (ksd->parentKey);
		keyDel (ksd->parentKey);
	}

	ksd->parentKey = keyDup (parentKey, KEY_CP_ALL);
	if (ksd->parentKey) keyIncRef (ksd->parentKey);

	if (ksd->addedKeys) cutOtherKeys (&ksd->addedKeys, parentKey);
	if (ksd->removedKeys) cutOtherKeys (&ksd->removedKeys, parentKey);
	if (ksd->modifiedKeys) cutOtherKeys (&ksd->modifiedKeys, parentKey);
	if (ksd->modifiedNewKeys) cutOtherKeys (&ksd->modifiedNewKeys, parentKey);
}

/**
 * Remove the given key from the diff
 *
 * @param ksd the diff
 * @param toRemove the key to remove
 */
void elektraDiffRemoveKey (ElektraDiff * ksd, Key * toRemove)
{
	if (ksd == NULL || toRemove == NULL)
	{
		return;
	}

	if (ksd->addedKeys) keyDel (ksLookup (ksd->addedKeys, toRemove, KDB_O_POP));
	if (ksd->removedKeys) keyDel (ksLookup (ksd->removedKeys, toRemove, KDB_O_POP));
	if (ksd->modifiedKeys) keyDel (ksLookup (ksd->modifiedKeys, toRemove, KDB_O_POP));
	if (ksd->modifiedNewKeys) keyDel (ksLookup (ksd->modifiedNewKeys, toRemove, KDB_O_POP));
}

/**
 * Appends a diff to another diff.
 *
 * @param target the diff where the other diff should be appended
 * @param source the diff that should be appended to the other diff
 * @param parentKey the parent key.
 *                  only keys below or same this key are appended.
 *                  if errors or warnings occur, they are stored as metadata in this key
 */
void elektraDiffAppend (ElektraDiff * target, const ElektraDiff * source, Key * parentKey)
{
	if (target == NULL || source == NULL)
	{
		return;
	}

	if (source->modifiedNewKeys == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "the passed ElektraDiff in source does not include the new values for the modified keys");
		return;
	}

	if (target->addedKeys == NULL) target->addedKeys = ksNew (0, KS_END);
	if (target->modifiedKeys == NULL) target->modifiedKeys = ksNew (0, KS_END);
	if (target->modifiedNewKeys == NULL) target->modifiedNewKeys = ksNew (0, KS_END);
	if (target->removedKeys == NULL) target->removedKeys = ksNew (0, KS_END);

	KeySet * metaAdded = ksNew (0, KS_END);
	KeySet * metaRemoved = ksNew (0, KS_END);
	KeySet * metaModified = ksNew (0, KS_END);

	for (elektraCursor i = 0; i < ksGetSize (source->addedKeys); i++)
	{
		Key * key = ksAtCursor (source->addedKeys, i);
		if (!keyIsBelowOrSame (parentKey, key))
		{
			continue;
		}

		Key * targetKey = ksLookup (target->addedKeys, key, 0);
		if (targetKey != NULL)
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Can't add already added key %s", keyName (key));
			continue;
		}

		targetKey = ksLookup (target->removedKeys, key, KDB_O_POP);
		if (targetKey != NULL)
		{
			if (keyValueDifferent (targetKey, key) || keyMetaDifferent (targetKey, key, metaAdded, metaRemoved, metaModified))
			{
				// key was added again, but with different value -> modified
				// as modified keys include the old value --> append targetKey
				ksAppendKey (target->modifiedKeys, targetKey);
				if (target->modifiedNewKeys)
				{
					ksAppendKey (target->modifiedNewKeys, key);
				}
				continue;
			}
			else
			{
				// key was added again but with the same value it was before --> remove from target
				keyDel (targetKey);
				continue;
			}
		}

		targetKey = ksLookup (target->modifiedKeys, key, 0);
		if (targetKey == NULL)
		{
			// Key was NOT found in modified keys --> add to added keys
			ksAppendKey (target->addedKeys, key);
			continue;
		}
		else
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Can't add already modified key %s", keyName (key));
			continue;
		}
	}

	// calculating on modified keys only works if we have the new values for the modified keys in source
	for (elektraCursor i = 0; i < ksGetSize (source->modifiedNewKeys); i++)
	{
		Key * key = ksAtCursor (source->modifiedNewKeys, i);
		if (!keyIsBelowOrSame (parentKey, key))
		{
			continue;
		}

		Key * targetKey = ksLookup (target->addedKeys, key, KDB_O_NONE);
		if (targetKey != NULL)
		{
			// Key was added and now modified -> still in added
			ksAppendKey (target->addedKeys, key);
			continue;
		}

		ssize_t pos = ksSearch (target->modifiedKeys, key);
		if (pos >= 0)
		{
			targetKey = ksAtCursor (target->modifiedKeys, pos);
			if (keyValueDifferent (targetKey, key) || keyMetaDifferent (targetKey, key, metaAdded, metaRemoved, metaModified))
			{
				// key value is different from the old key --> still modified
				// the "old" value stays the same, but we set the new value to the value of this key
				if (target->modifiedNewKeys != NULL)
				{
					ksAppendKey (target->modifiedNewKeys, key);
				}
				continue;
			}
			else
			{
				// key was modified back to original value --> remove from target
				keyDel (elektraKsPopAtCursor (target->modifiedKeys, pos));
				if (target->modifiedNewKeys != NULL)
				{
					keyDel (elektraKsPopAtCursor (target->modifiedNewKeys, pos));
				}
				continue;
			}
		}

		targetKey = ksLookup (target->removedKeys, key, 0);
		if (targetKey != NULL)
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Can't modify already removed key %s", keyName (key));
			continue;
		}

		// Everything else has been checked already
		// Key was not found in target so add it to modified keys
		// We need to take the key with the old value. Luckily, both keysets must contain the exact same keys
		ksAppendKey (target->modifiedKeys, ksAtCursor (source->modifiedKeys, i));
		if (target->modifiedKeys)
		{
			ksAppendKey (target->modifiedNewKeys, key);
		}
	}

	for (elektraCursor i = 0; i < ksGetSize (source->removedKeys); i++)
	{
		Key * key = ksAtCursor (source->removedKeys, i);
		if (!keyIsBelowOrSame (parentKey, key))
		{
			continue;
		}

		Key * targetKey = ksLookup (target->addedKeys, key, KDB_O_POP);
		if (targetKey != NULL)
		{
			// Key was added and now removed -> untracked
			keyDel (targetKey);
			continue;
		}

		targetKey = ksLookup (target->modifiedKeys, key, KDB_O_POP);
		if (targetKey != NULL)
		{
			// removedKeys always stores the old value
			// thus we have to add the key from target
			ksAppendKey (target->removedKeys, targetKey);
			if (target->modifiedNewKeys != NULL)
			{
				keyDel (ksLookup (target->modifiedNewKeys, targetKey, KDB_O_POP));
			}
			continue;
		}

		targetKey = ksLookup (target->removedKeys, key, 0);
		if (targetKey == NULL)
		{
			ksAppendKey (target->removedKeys, key);
			continue;
		}
		else
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Can't remove already removed key %s", keyName (key));
			continue;
		}
	}

	ksDel (metaAdded);
	ksDel (metaRemoved);
	ksDel (metaModified);
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
 * Get the modified keys with their new value
 *
 * @param ksd the ElektraDiff
 * @return a new KeySet containing the modified keys OR @p NULL if @p ksd is @p NULL
 */
KeySet * elektraDiffGetModifiedNewKeys (const ElektraDiff * ksd)
{
	if (ksd == NULL)
	{
		return NULL;
	}

	if (!ksd->modifiedNewKeys)
	{
		return ksNew (0, KS_END);
	}

	return ksDup (ksd->modifiedNewKeys);
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

	findDifferences (keyMeta (key), keyMeta (oldKey), addedKeys, NULL, NULL, NULL, NULL);

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

	findDifferences (keyMeta (key), keyMeta (oldKey), NULL, removedKeys, NULL, NULL, NULL);

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

	findDifferences (keyMeta (key), keyMeta (oldKey), NULL, NULL, modifiedKeys, NULL, NULL);

	return modifiedKeys;
}

/**
 * Undos the changes that are presented in the diff.
 *  - Added keys get removed from @p ks
 *  - Removed keys get added back to @p ks,
 *  - Modified keys will get their old values and meta data in @p ks
 *
 * @param ksd the diff
 * @param ks the keyset that shall be undone
 */
void elektraDiffUndo (ElektraDiff * ksd, KeySet * ks)
{
	ksSubtract (ks, ksd->addedKeys);
	ksAppend (ks, ksd->modifiedKeys);
	ksAppend (ks, ksd->removedKeys);
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
