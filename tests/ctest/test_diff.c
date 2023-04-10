#include <kdbdiff.h>
#include <tests_internal.h>

ElektraDiff * demoDiff = NULL;

static void initializeDemoDiff (void)
{
	Key * demoParent = keyNew ("system:/test", KEY_END);

	KeySet * addedKeys = ksNew (1, keyNew ("system:/test/added/1", KEY_VALUE, "first added key", KEY_END), KS_END);

	KeySet * removedKeys = ksNew (1, keyNew ("system:/test/removed/1", KEY_VALUE, "first removed key", KEY_END), KS_END);

	KeySet * modifiedKeys =
		ksNew (1, keyNew ("system:/test/modified/value", KEY_VALUE, "modified value", KEY_END),
		       keyNew ("system:/test/modified/meta", KEY_VALUE, "modified meta", KEY_META, "meta:/something", "test", KEY_END),
		       keyNew ("system:/test/modified/metaAndValue", KEY_VALUE, "modified meta and value", KEY_META, "meta:/something",
			       "test", KEY_END),
		       KS_END);

	demoDiff = elektraDiffNew (addedKeys, removedKeys, modifiedKeys, NULL, demoParent);

	keyDel (demoParent);
}

static void test_elektraDiffNew_shouldIncreaseRefCount (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * added = ksNew (0, KS_END);
	KeySet * removed = ksNew (0, KS_END);
	KeySet * modified = ksNew (0, KS_END);

	ksIncRef (added);
	ksIncRef (removed);
	ksIncRef (modified);

	// Act
	ElektraDiff * diff = elektraDiffNew (added, removed, modified, NULL, NULL);

	// Assert
	succeed_if (ksGetRef (added) == 2, "should increase refcount of added");
	succeed_if (ksGetRef (removed) == 2, "should increase refcount of removed");
	succeed_if (ksGetRef (modified) == 2, "should increase refcount of modified");

	elektraDiffDel (diff);

	ksDecRef (added);
	ksDecRef (removed);
	ksDecRef (modified);

	ksDel (added);
	ksDel (removed);
	ksDel (modified);
}

static void test_elektraDiffGetParentKey_returnsParentKey (void)
{
	printf ("Test %s\n", __func__);

	const Key * parentKey = elektraDiffGetParentKey (demoDiff);

	succeed_if (parentKey != NULL, "parent key should not be NULL");
	succeed_if (strcmp (keyName (parentKey), "system:/test") == 0, "name should be system:/test");
}

static void test_elektraDiffGetParentKey_forNullDiff_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	succeed_if (elektraDiffGetParentKey (NULL) == NULL, "should return NULL on NULL");
}

static void test_elektraDiffGetParentKey_ifNoParentKey_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	ElektraDiff * diff = elektraDiffNew (ksNew (0, KS_END), ksNew (0, KS_END), ksNew (0, KS_END), ksNew (0, KS_END), NULL);

	succeed_if (elektraDiffGetParentKey (diff) == NULL, "should return NULL");

	elektraDiffDel (diff);
}

static void test_elektraDiffGetAddedKeys_shouldReturnAddedKeys (void)
{
	printf ("Test %s\n", __func__);

	KeySet * added = elektraDiffGetAddedKeys (demoDiff);

	succeed_if (added != NULL, "keyset should not be NULL");
	succeed_if (ksGetSize (added) == 1, "should have 1 entry");
	succeed_if (ksLookupByName (added, "system:/test/added/1", 0) != 0, "should contain key system:/test/added/1");

	ksDel (added);
}

static void test_elektraDiffGetAddedKeys_forNullDiff_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	succeed_if (elektraDiffGetAddedKeys (NULL) == NULL, "should return NULL");
}

static void test_elektraDiffGetRemovedKeys_shouldReturnRemovedKeys (void)
{
	printf ("Test %s\n", __func__);

	KeySet * removed = elektraDiffGetRemovedKeys (demoDiff);

	succeed_if (removed != NULL, "keyset should not be NULL");
	succeed_if (ksGetSize (removed) == 1, "should have 1 entry");
	succeed_if (ksLookupByName (removed, "system:/test/removed/1", 0) != 0, "should contain key system:/test/removed/1");

	ksDel (removed);
}

static void test_elektraDiffGetRemovedKeys_forNullDiff_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	succeed_if (elektraDiffGetRemovedKeys (NULL) == NULL, "should return NULL");
}

static void test_elektraDiffGetModifiedKeys_shouldReturnModifiedKeys (void)
{
	printf ("Test %s\n", __func__);

	KeySet * modified = elektraDiffGetModifiedKeys (demoDiff);

	succeed_if (modified != NULL, "keyset should not be NULL");
	succeed_if (ksGetSize (modified) == 3, "should have 3 entries");
	succeed_if (ksLookupByName (modified, "system:/test/modified/value", 0) != 0, "should contain key system:/test/modified/value");
	succeed_if (ksLookupByName (modified, "system:/test/modified/meta", 0) != 0, "should contain key system:/test/modified/meta");
	succeed_if (ksLookupByName (modified, "system:/test/modified/metaAndValue", 0) != 0,
		    "should contain key system:/test/modified/metaAndValue");

	ksDel (modified);
}

static void test_elektraDiffGetModifiedKeys_forNullDiff_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	succeed_if (elektraDiffGetModifiedKeys (NULL) == NULL, "should return NULL");
}

static void test_elektraDiffKeyValueChanged_onChangedValue_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/value", KEY_VALUE, "other value", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyValueChanged (demoDiff, otherValue) == true, "value should have changed");

	keyDel (otherValue);
}

static void test_elektraDiffKeyValueChanged_onChangedMetaAndValue_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/metaAndValue", KEY_VALUE, "other value", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyValueChanged (demoDiff, otherValue) == true, "value should have changed");

	keyDel (otherValue);
}

static void test_elektraDiffKeyValueChanged_onChangedMeta_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "modified meta", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyValueChanged (demoDiff, otherValue) == false, "value should not have changed");

	keyDel (otherValue);
}

static void test_elektraDiffKeyValueChanged_onNonExistingKey_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/nonexisting", KEY_VALUE, "nonexisting key", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyValueChanged (demoDiff, otherValue) == false, "value should not have changed");

	keyDel (otherValue);
}

static void test_elektraDiffKeyValueChanged_onNullParameters_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/nonexisting", KEY_VALUE, "nonexisting key", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyValueChanged (NULL, otherValue) == false, "NULL diff should return false");
	succeed_if (elektraDiffKeyValueChanged (demoDiff, NULL) == false, "NULL key should return false");

	keyDel (otherValue);
}

static void test_elektraDiffKeyOnlyMetaChanged_onChangedMeta_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "modified meta", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyOnlyMetaChanged (demoDiff, otherValue) == true, "should return true");

	keyDel (otherValue);
}

static void test_elektraDiffKeyOnlyMetaChanged_onChangedMetaAndValue_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/metaAndValue", KEY_VALUE, "other value", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyOnlyMetaChanged (demoDiff, otherValue) == false, "should return false");

	keyDel (otherValue);
}

static void test_elektraDiffKeyOnlyMetaChanged_onChangedValue_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/value", KEY_VALUE, "other value", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyOnlyMetaChanged (demoDiff, otherValue) == false, "should return false");

	keyDel (otherValue);
}

static void test_elektraDiffKeyOnlyMetaChanged_onNonExistingKey_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/nonexisting", KEY_VALUE, "nonexisting key", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyOnlyMetaChanged (demoDiff, otherValue) == false, "should return false");

	keyDel (otherValue);
}

static void test_elektraDiffKeyOnlyMetaChanged_onNullParameters_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "meta modified", KEY_END);

	// Act & Assert
	succeed_if (elektraDiffKeyOnlyMetaChanged (NULL, otherValue) == false, "NULL diff should return false");
	succeed_if (elektraDiffKeyOnlyMetaChanged (demoDiff, NULL) == false, "NULL key should return false");

	keyDel (otherValue);
}

static void test_elektraDiffGetAddedMetaKeys_shouldGetAddedMetaKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "meta modified", KEY_META, "meta:/new", "hello", KEY_END);

	// Act
	KeySet * added = elektraDiffGetAddedMetaKeys (demoDiff, otherValue);

	// Assert
	succeed_if (added != NULL, "should not be NULL");
	succeed_if (ksGetSize (added) == 1, "should have 1 added meta key");
	succeed_if (ksLookupByName (added, "meta:/new", 0) != NULL, "should find meta:/added");

	ksDel (added);
	keyDel (otherValue);
}

static void test_elektraDiffGetAddedMetaKeys_diffOrKeyIsNull_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/modified/meta", KEY_END);

	succeed_if (elektraDiffGetAddedMetaKeys (NULL, key) == NULL, "NULL diff should return NULL");
	succeed_if (elektraDiffGetAddedMetaKeys (demoDiff, NULL) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_elektraDiffGetAddedMetaKeys_unknownKey_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/unknown", KEY_END);

	succeed_if (elektraDiffGetAddedMetaKeys (demoDiff, key) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_elektraDiffGetRemovedMetaKeys_shouldGetRemovedMetaKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "meta modified", KEY_META, "meta:/new", "hello", KEY_END);

	// Act
	KeySet * removed = elektraDiffGetRemovedMetaKeys (demoDiff, otherValue);

	// Assert
	succeed_if (removed != NULL, "should not be NULL");
	succeed_if (ksGetSize (removed) == 1, "should have 1 removed meta key");
	succeed_if (ksLookupByName (removed, "meta:/something", 0) != NULL, "should find meta:/something");

	ksDel (removed);
	keyDel (otherValue);
}

static void test_elektraDiffGetRemovedMetaKeys_diffOrKeyIsNull_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/modified/meta", KEY_END);

	succeed_if (elektraDiffGetRemovedMetaKeys (NULL, key) == NULL, "NULL diff should return NULL");
	succeed_if (elektraDiffGetRemovedMetaKeys (demoDiff, NULL) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_elektraDiffGetRemovedMetaKeys_unknownKey_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/unknown", KEY_END);

	succeed_if (elektraDiffGetRemovedMetaKeys (demoDiff, key) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_elektraDiffGetModifiedMetaKeys_shouldGetModifiedMetaKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * otherValue = keyNew ("system:/test/modified/meta", KEY_VALUE, "meta modified", KEY_META, "meta:/something", "howdy", KEY_END);

	// Act
	KeySet * modified = elektraDiffGetModifiedMetaKeys (demoDiff, otherValue);

	// Assert
	succeed_if (modified != NULL, "should not be NULL");
	succeed_if (ksGetSize (modified) == 1, "should have 1 removed meta key");
	succeed_if (ksLookupByName (modified, "meta:/something", 0) != NULL, "should find meta:/something");

	ksDel (modified);
	keyDel (otherValue);
}

static void test_elektraDiffGetModifiedMetaKeys_diffOrKeyIsNull_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/modified/meta", KEY_END);

	succeed_if (elektraDiffGetModifiedMetaKeys (NULL, key) == NULL, "NULL diff should return NULL");
	succeed_if (elektraDiffGetModifiedMetaKeys (demoDiff, NULL) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_elektraDiffGetModifiedMetaKeys_unknownKey_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	Key * key = keyNew ("system:/test/unknown", KEY_END);

	succeed_if (elektraDiffGetModifiedMetaKeys (demoDiff, key) == NULL, "NULL key should return NULL");

	keyDel (key);
}

static void test_refCounting (void)
{
	printf ("Test %s\n", __func__);

	ElektraDiff * diff = elektraDiffNew (ksNew (0, KS_END), ksNew (0, KS_END), ksNew (0, KS_END), NULL, NULL);

	succeed_if (elektraDiffGetRef (diff) == 0, "refcounter should be 0");

	elektraDiffIncRef (diff);
	succeed_if (elektraDiffGetRef (diff) == 1, "refcounter should be 1");

	elektraDiffDecRef (diff);
	succeed_if (elektraDiffGetRef (diff) == 0, "refcounter should be 0");

	elektraDiffDecRef (diff);
	succeed_if (elektraDiffGetRef (diff) == 0, "refcounter should stay 0");

	elektraDiffDel (diff);
}

static void test_elektraDiffCalculate_nullKeySets_shouldReturnNull (void)
{
	printf ("Test %s\n", __func__);

	KeySet * ks = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system:/test", KEY_END);

	succeed_if (elektraDiffCalculate (NULL, ks, parentKey) == NULL, "NULL newkeys should return NULL");
	succeed_if (elektraDiffCalculate (ks, NULL, parentKey) == NULL, "NULL oldkeys should return NULL");

	ksDel (ks);
	keyDel (parentKey);
}

static void test_elektraDiffCalculate_shouldReturnDiff (void)
{
	printf ("Test %s\n", __func__);

	KeySet * ks = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system:/test", KEY_END);

	ElektraDiff * diff = elektraDiffCalculate (ks, ks, parentKey);

	succeed_if (diff != NULL, "diff should not be NULL");

	ksDel (ks);
	keyDel (parentKey);
	elektraDiffDel (diff);
}

static void test_elektraDiffAppend_addedKeys_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * targetAdded = ksNew (1, keyNew ("system:/key/addedInTarget", KEY_END), KS_END);
	KeySet * targetModified = ksNew (1, keyNew ("system:/key/modifiedInTarget", KEY_VALUE, "test", KEY_END), KS_END);
	KeySet * targetRemoved =
		ksNew (1, keyNew ("system:/key/completelyRemoved", KEY_VALUE, "test", KEY_END),
		       keyNew ("system:/key/willBeUntracked", KEY_VALUE, "test2", KEY_END),
		       keyNew ("system:/key/willBeUntrackedAlso", KEY_VALUE, "test3", KEY_META, "meta:/test", "metavalue", KEY_END),
		       keyNew ("system:/key/willBeModified", KEY_VALUE, "test4", KEY_END), KS_END);
	ElektraDiff * target = elektraDiffNew (targetAdded, targetRemoved, targetModified, NULL, NULL);

	KeySet * sourceAdded = ksNew (
		1, keyNew ("system:/key/addedInSource", KEY_END), keyNew ("system:/key/willBeUntracked", KEY_VALUE, "test2", KEY_END),
		keyNew ("system:/key/willBeUntrackedAlso", KEY_VALUE, "test3", KEY_META, "meta:/test", "metavalue", KEY_END),
		keyNew ("system:/key/willBeModified", KEY_VALUE, "modified-test4", KEY_END), KS_END);
	KeySet * sourceModified = ksNew (0, KS_END);
	KeySet * sourceRemoved = ksNew (0, KS_END);
	ElektraDiff * source = elektraDiffNew (sourceAdded, sourceRemoved, sourceModified, ksNew (0, KS_END), NULL);

	// Act
	elektraDiffAppend (target, source, NULL);

	// Assert
	KeySet * addedKeys = elektraDiffGetAddedKeys (target);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (target);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (target);

	succeed_if_fmt (ksGetSize (addedKeys) == 2, "Added keys should have 2 keys, was %zu", ksGetSize (addedKeys));
	succeed_if (ksLookupByName (addedKeys, "system:/key/addedInTarget", 0) != NULL,
		    "Added keys should contain system:/key/addedInTarget");
	succeed_if (ksLookupByName (addedKeys, "system:/key/addedInSource", 0) != NULL,
		    "Added keys should contain system:/key/addedInSource");

	succeed_if_fmt (ksGetSize (removedKeys) == 1, "Removed keys should have 1 key, was %zu", ksGetSize (removedKeys));
	succeed_if (ksLookupByName (removedKeys, "system:/key/completelyRemoved", 0) != NULL,
		    "Removed keys should contain system:/key/completelyRemoved");

	succeed_if_fmt (ksGetSize (modifiedKeys) == 2, "Modified keys should have 2 keys, was %zu", ksGetSize (modifiedKeys));
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/willBeModified", 0) != NULL,
		    "Modified keys should contain system:/key/willBeModified");
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/modifiedInTarget", 0) != NULL,
		    "Modified keys should contain system:/key/modifiedInTarget");
	succeed_if (strcmp (keyString (ksLookupByName (modifiedKeys, "system:/key/willBeModified", 0)), "test4") == 0,
		    "Modified keys should only contain old value");

	elektraDiffDel (target);
	elektraDiffDel (source);
	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
}

static void test_elektraDiffAppend_modifiedKeys_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * targetAdded = ksNew (1, keyNew ("system:/key/addedInTarget", KEY_VALUE, "target", KEY_END), KS_END);
	KeySet * targetModified =
		ksNew (1, keyNew ("system:/key/modifiedInTarget", KEY_VALUE, "test", KEY_END),
		       keyNew ("system:/key/modifiedInTargetAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "metavalue", KEY_END),
		       keyNew ("system:/key/willBeUntracked", KEY_VALUE, "test", KEY_END),
		       keyNew ("system:/key/willBeUntrackedAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "metavalue", KEY_END), KS_END);
	KeySet * targetRemoved = ksNew (1, keyNew ("system:/key/completelyRemoved", KEY_VALUE, "test", KEY_END), KS_END);
	ElektraDiff * target = elektraDiffNew (targetAdded, targetRemoved, targetModified, NULL, NULL);

	KeySet * sourceAdded = ksNew (0, KS_END);
	KeySet * sourceModified =
		ksNew (1, keyNew ("system:/key/addedInTarget", KEY_VALUE, "target", KEY_END),
		       keyNew ("system:/key/modifiedInTarget", KEY_VALUE, "gugu", KEY_END),
		       keyNew ("system:/key/modifiedInTargetAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "gugu", KEY_END),
		       keyNew ("system:/key/willBeUntracked", KEY_VALUE, "gugu", KEY_END),
		       keyNew ("system:/key/willBeUntrackedAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "gugu", KEY_END),
		       keyNew ("system:/key/modifiedInSource", KEY_VALUE, "original", KEY_END), KS_END);
	KeySet * sourceModifiedNew = ksNew (
		1, keyNew ("system:/key/addedInTarget", KEY_VALUE, "source", KEY_END),
		keyNew ("system:/key/modifiedInTarget", KEY_VALUE, "modified-test", KEY_END),
		keyNew ("system:/key/modifiedInTargetAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "modified-metavalue", KEY_END),
		keyNew ("system:/key/willBeUntracked", KEY_VALUE, "test", KEY_END),
		keyNew ("system:/key/willBeUntrackedAlso", KEY_VALUE, "test", KEY_META, "meta:/test", "metavalue", KEY_END),
		keyNew ("system:/key/modifiedInSource", KEY_VALUE, "new", KEY_END), KS_END);
	KeySet * sourceRemoved = ksNew (0, KS_END);
	ElektraDiff * source = elektraDiffNew (sourceAdded, sourceRemoved, sourceModified, sourceModifiedNew, NULL);

	// Act
	elektraDiffAppend (target, source, NULL);

	// Assert
	KeySet * addedKeys = elektraDiffGetAddedKeys (target);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (target);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (target);

	succeed_if_fmt (ksGetSize (addedKeys) == 1, "Added keys should have 1 key, was %zu", ksGetSize (addedKeys));
	succeed_if (ksLookupByName (addedKeys, "system:/key/addedInTarget", 0) != NULL,
		    "Added keys should contain system:/key/addedInTarget");
	succeed_if (strcmp (keyString (ksLookupByName (addedKeys, "system:/key/addedInTarget", 0)), "source") == 0,
		    "Added key should be updated");

	succeed_if_fmt (ksGetSize (removedKeys) == 1, "Removed keys should have 1 key, was %zu", ksGetSize (removedKeys));
	succeed_if (ksLookupByName (removedKeys, "system:/key/completelyRemoved", 0) != NULL,
		    "Removed keys should contain system:/key/completelyRemoved");

	succeed_if_fmt (ksGetSize (modifiedKeys) == 3, "Modified keys should have 3 keys, was %zu", ksGetSize (modifiedKeys));
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/modifiedInTarget", 0) != NULL,
		    "Modified keys should contain system:/key/modifiedInTarget");
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/modifiedInTargetAlso", 0) != NULL,
		    "Modified keys should contain system:/key/modifiedInTargetAlso");
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/modifiedInSource", 0) != NULL,
		    "Modified keys should contain system:/key/modifiedInSource");
	succeed_if (strcmp (keyString (ksLookupByName (modifiedKeys, "system:/key/modifiedInTarget", 0)), "test") == 0,
		    "Modified key modifiedInTarget should only contain old value");
	succeed_if (strcmp (keyString (ksLookupByName (modifiedKeys, "system:/key/modifiedInTargetAlso", 0)), "test") == 0,
		    "Modified key modifiedInTargetAlso should only contain old value");
	succeed_if (strcmp (keyString (ksLookupByName (modifiedKeys, "system:/key/modifiedInSource", 0)), "original") == 0,
		    "Modified key modifiedInSource should only contain old value");

	elektraDiffDel (target);
	elektraDiffDel (source);
	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
}

static void test_elektraDiffAppend_removedKeys_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * targetAdded =
		ksNew (1, keyNew ("system:/key/addedInTarget", KEY_END), keyNew ("system:/key/addedWillBeRemoved", KEY_END), KS_END);
	KeySet * targetModified = ksNew (1, keyNew ("system:/key/modifiedInTarget", KEY_VALUE, "test", KEY_END),
					 keyNew ("system:/key/modifiedWillBeRemoved", KEY_VALUE, "test", KEY_END), KS_END);
	KeySet * targetRemoved = ksNew (1, keyNew ("system:/key/completelyRemoved", KEY_VALUE, "test", KEY_END), KS_END);

	ElektraDiff * target = elektraDiffNew (targetAdded, targetRemoved, targetModified, NULL, NULL);

	KeySet * sourceAdded = ksNew (0, KS_END);
	KeySet * sourceModified = ksNew (0, KS_END);
	KeySet * sourceRemoved =
		ksNew (1, keyNew ("system:/key/addedWillBeRemoved", KEY_END), keyNew ("system:/key/modifiedWillBeRemoved", KEY_END),
		       keyNew ("system:/key/removedInSource", KEY_END), KS_END);
	ElektraDiff * source = elektraDiffNew (sourceAdded, sourceRemoved, sourceModified, ksNew (0, KS_END), NULL);

	// Act
	elektraDiffAppend (target, source, NULL);

	// Assert
	KeySet * addedKeys = elektraDiffGetAddedKeys (target);
	KeySet * modifiedKeys = elektraDiffGetModifiedKeys (target);
	KeySet * removedKeys = elektraDiffGetRemovedKeys (target);

	succeed_if_fmt (ksGetSize (addedKeys) == 1, "Added keys should have 1 key, was %zu", ksGetSize (addedKeys));
	succeed_if (ksLookupByName (addedKeys, "system:/key/addedInTarget", 0) != NULL,
		    "Added keys should contain system:/key/addedInTarget");

	succeed_if_fmt (ksGetSize (modifiedKeys) == 1, "Modified keys should have 1 key, was %zu", ksGetSize (modifiedKeys));
	succeed_if (ksLookupByName (modifiedKeys, "system:/key/modifiedInTarget", 0) != NULL,
		    "Modified keys should contain system:/key/modifiedInTarget");

	succeed_if_fmt (ksGetSize (removedKeys) == 3, "Removed keys should have 3 keys, was %zu", ksGetSize (removedKeys));
	succeed_if (ksLookupByName (removedKeys, "system:/key/completelyRemoved", 0) != NULL,
		    "Removed keys should contain system:/key/completelyRemoved");
	succeed_if (ksLookupByName (removedKeys, "system:/key/modifiedWillBeRemoved", 0) != NULL,
		    "Removed keys should contain system:/key/modifiedWillBeRemoved");
	succeed_if (ksLookupByName (removedKeys, "system:/key/removedInSource", 0) != NULL,
		    "Removed keys should contain system:/key/removedInSource");

	elektraDiffDel (target);
	elektraDiffDel (source);
	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
}

static void test_elektraDiffRemoveSameOrBelow_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	ElektraDiff * diff = elektraDiffNew (
		ksNew (2, keyNew ("system:/a/added", KEY_END), keyNew ("system:/b/added", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/removed", KEY_END), keyNew ("system:/b/removed", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modified", KEY_END), keyNew ("system:/b/modified", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modifiedNew", KEY_END), keyNew ("system:/b/modifiedNew", KEY_END), KS_END), NULL);

	Key * toRemove = keyNew ("system:/a", KEY_END);

	// Act
	elektraDiffRemoveSameOrBelow (diff, toRemove);

	// Assert
	succeed_if (diff->addedKeys != NULL, "added keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->addedKeys) == 1, "added keys should have 1 key, was %zu", ksGetSize (diff->addedKeys));
	succeed_if (ksLookupByName (diff->addedKeys, "system:/b/added", 0) != NULL, "system:/b/added should still be in added keys");

	succeed_if (diff->removedKeys != NULL, "removed keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->removedKeys) == 1, "removed keys should have 1 key, was %zu", ksGetSize (diff->removedKeys));
	succeed_if (ksLookupByName (diff->removedKeys, "system:/b/removed", 0) != NULL,
		    "system:/b/removed should still be in removed keys");

	succeed_if (diff->modifiedKeys != NULL, "modified keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedKeys) == 1, "modified keys should have 1 key, was %zu", ksGetSize (diff->modifiedKeys));
	succeed_if (ksLookupByName (diff->modifiedKeys, "system:/b/modified", 0) != NULL,
		    "system:/b/modified should still be in added keys");

	succeed_if (diff->modifiedNewKeys != NULL, "modified new keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedNewKeys) == 1, "modified new keys should have 1 key, was %zu",
			ksGetSize (diff->modifiedNewKeys));
	succeed_if (ksLookupByName (diff->modifiedNewKeys, "system:/b/modifiedNew", 0) != NULL,
		    "system:/b/modifiedNew should still be in added keys");

	elektraDiffDel (diff);
	keyDel (toRemove);
}

static void test_elektraDiffRemoveOther_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	ElektraDiff * diff = elektraDiffNew (
		ksNew (2, keyNew ("system:/a/added", KEY_END), keyNew ("system:/b/added", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/removed", KEY_END), keyNew ("system:/b/removed", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modified", KEY_END), keyNew ("system:/b/modified", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modifiedNew", KEY_END), keyNew ("system:/b/modifiedNew", KEY_END), KS_END), NULL);

	Key * toKeep = keyNew ("system:/b", KEY_END);

	// Act
	elektraDiffRemoveOther (diff, toKeep);

	// Assert
	succeed_if (diff->addedKeys != NULL, "added keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->addedKeys) == 1, "added keys should have 1 key, was %zu", ksGetSize (diff->addedKeys));
	succeed_if (ksLookupByName (diff->addedKeys, "system:/b/added", 0) != NULL, "system:/b/added should still be in added keys");

	succeed_if (diff->removedKeys != NULL, "removed keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->removedKeys) == 1, "removed keys should have 1 key, was %zu", ksGetSize (diff->removedKeys));
	succeed_if (ksLookupByName (diff->removedKeys, "system:/b/removed", 0) != NULL,
		    "system:/b/removed should still be in removed keys");

	succeed_if (diff->modifiedKeys != NULL, "modified keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedKeys) == 1, "modified keys should have 1 key, was %zu", ksGetSize (diff->modifiedKeys));
	succeed_if (ksLookupByName (diff->modifiedKeys, "system:/b/modified", 0) != NULL,
		    "system:/b/modified should still be in added keys");

	succeed_if (diff->modifiedNewKeys != NULL, "modified new keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedNewKeys) == 1, "modified new keys should have 1 key, was %zu",
			ksGetSize (diff->modifiedNewKeys));
	succeed_if (ksLookupByName (diff->modifiedNewKeys, "system:/b/modifiedNew", 0) != NULL,
		    "system:/b/modifiedNew should still be in added keys");

	succeed_if (keyCmp (toKeep, diff->parentKey) == 0, "diff should now have same parent key");

	elektraDiffDel (diff);
	keyDel (toKeep);
}

static void test_elektraDiffCut_shouldWork (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	ElektraDiff * diff = elektraDiffNew (
		ksNew (2, keyNew ("system:/a/added", KEY_END), keyNew ("system:/b/added", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/removed", KEY_END), keyNew ("system:/b/removed", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modified", KEY_END), keyNew ("system:/b/modified", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modifiedNew", KEY_END), keyNew ("system:/b/modifiedNew", KEY_END), KS_END), NULL);

	Key * toKeep = keyNew ("system:/b", KEY_END);

	// Act
	ElektraDiff * newDiff = elektraDiffCut (diff, toKeep);

	// Assert
	succeed_if (diff->addedKeys != NULL, "added keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->addedKeys) == 1, "added keys should have 1 key, was %zu", ksGetSize (diff->addedKeys));
	succeed_if (ksLookupByName (diff->addedKeys, "system:/a/added", 0) != NULL, "system:/a/added should still be in added keys");

	succeed_if (diff->removedKeys != NULL, "removed keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->removedKeys) == 1, "removed keys should have 1 key, was %zu", ksGetSize (diff->removedKeys));
	succeed_if (ksLookupByName (diff->removedKeys, "system:/a/removed", 0) != NULL,
		    "system:/a/removed should still be in removed keys");

	succeed_if (diff->modifiedKeys != NULL, "modified keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedKeys) == 1, "modified keys should have 1 key, was %zu", ksGetSize (diff->modifiedKeys));
	succeed_if (ksLookupByName (diff->modifiedKeys, "system:/a/modified", 0) != NULL,
		    "system:/a/modified should still be in added keys");

	succeed_if (diff->modifiedNewKeys != NULL, "modified new keys should not be NULL");
	succeed_if_fmt (ksGetSize (diff->modifiedNewKeys) == 1, "modified new keys should have 1 key, was %zu",
			ksGetSize (diff->modifiedNewKeys));
	succeed_if (ksLookupByName (diff->modifiedNewKeys, "system:/a/modifiedNew", 0) != NULL,
		    "system:/a/modifiedNew should still be in added keys");

	succeed_if (newDiff->addedKeys != NULL, "added keys should not be NULL");
	succeed_if_fmt (ksGetSize (newDiff->addedKeys) == 1, "added keys should have 1 key, was %zu", ksGetSize (newDiff->addedKeys));
	succeed_if (ksLookupByName (newDiff->addedKeys, "system:/b/added", 0) != NULL, "system:/b/added should still be in added keys");

	succeed_if (newDiff->removedKeys != NULL, "removed keys should not be NULL");
	succeed_if_fmt (ksGetSize (newDiff->removedKeys) == 1, "removed keys should have 1 key, was %zu", ksGetSize (newDiff->removedKeys));
	succeed_if (ksLookupByName (newDiff->removedKeys, "system:/b/removed", 0) != NULL,
		    "system:/b/removed should still be in removed keys");

	succeed_if (newDiff->modifiedKeys != NULL, "modified keys should not be NULL");
	succeed_if_fmt (ksGetSize (newDiff->modifiedKeys) == 1, "modified keys should have 1 key, was %zu",
			ksGetSize (newDiff->modifiedKeys));
	succeed_if (ksLookupByName (newDiff->modifiedKeys, "system:/b/modified", 0) != NULL,
		    "system:/b/modified should still be in added keys");

	succeed_if (newDiff->modifiedNewKeys != NULL, "modified new keys should not be NULL");
	succeed_if_fmt (ksGetSize (newDiff->modifiedNewKeys) == 1, "modified new keys should have 1 key, was %zu",
			ksGetSize (newDiff->modifiedNewKeys));
	succeed_if (ksLookupByName (newDiff->modifiedNewKeys, "system:/b/modifiedNew", 0) != NULL,
		    "system:/b/modifiedNew should still be in added keys");

	succeed_if (keyCmp (toKeep, newDiff->parentKey) == 0, "newDiff should now have same parent key");

	elektraDiffDel (diff);
	elektraDiffDel (newDiff);
	keyDel (toKeep);
}

static void test_elektraDiffDup_shouldDuplicate (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("system:/a", KEY_END);
	ElektraDiff * diff = elektraDiffNew (
		ksNew (2, keyNew ("system:/a/added", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/removed", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modified", KEY_END), KS_END),
		ksNew (2, keyNew ("system:/a/modifiedNew", KEY_END), KS_END),
		       parentKey);

	// Act
	ElektraDiff * duped = elektraDiffDup (diff);

	// Assert
	succeed_if (duped != NULL, "duplicated diff should not be NULL");
	succeed_if (duped->addedKeys != NULL, "added keys should not be NULL");
	succeed_if (duped->removedKeys != NULL, "removed keys should not be NULL");
	succeed_if (duped->modifiedKeys != NULL, "modified keys should not be NULL");
	succeed_if (duped->modifiedNewKeys != NULL, "modified new keys should not be NULL");
	succeed_if (duped->parentKey != NULL, "parent key should not be NULL");

	succeed_if (duped->addedKeys != diff->addedKeys, "added keys should point to different keysets");
	succeed_if (duped->removedKeys != diff->removedKeys, "removed keys should point to different keysets");
	succeed_if (duped->modifiedKeys != diff->modifiedKeys, "modified keys should point to different keysets");
	succeed_if (duped->modifiedNewKeys != diff->modifiedNewKeys, "modified new keys should point to different keysets");
	succeed_if (duped->parentKey != diff->parentKey, "parent key should point to different key");

	succeed_if (ksLookupByName (duped->addedKeys, "system:/a/added", 0) != NULL, "added keys should contain key");
	succeed_if (ksLookupByName (duped->removedKeys, "system:/a/removed", 0) != NULL, "removed keys should contain key");
	succeed_if (ksLookupByName (duped->modifiedKeys, "system:/a/modified", 0) != NULL, "modified keys should contain key");
	succeed_if (ksLookupByName (duped->modifiedNewKeys, "system:/a/modifiedNew", 0) != NULL, "modified new keys should contain key");
	succeed_if (strcmp (keyName (duped->parentKey), "system:/a") == 0, "parent key should be correct");

	elektraDiffDel (diff);
	elektraDiffDel (duped);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("DIFF                 TESTS\n");
	printf ("==========================\n\n");

	init (argc, argv);
	initializeDemoDiff ();

	test_elektraDiffNew_shouldIncreaseRefCount ();

	test_elektraDiffGetParentKey_returnsParentKey ();
	test_elektraDiffGetParentKey_forNullDiff_shouldReturnNull ();
	test_elektraDiffGetParentKey_ifNoParentKey_shouldReturnNull ();

	test_elektraDiffGetAddedKeys_shouldReturnAddedKeys ();
	test_elektraDiffGetAddedKeys_forNullDiff_shouldReturnNull ();

	test_elektraDiffGetRemovedKeys_shouldReturnRemovedKeys ();
	test_elektraDiffGetRemovedKeys_forNullDiff_shouldReturnNull ();

	test_elektraDiffGetModifiedKeys_shouldReturnModifiedKeys ();
	test_elektraDiffGetModifiedKeys_forNullDiff_shouldReturnNull ();

	test_elektraDiffKeyValueChanged_onChangedValue_shouldReturnTrue ();
	test_elektraDiffKeyValueChanged_onChangedMetaAndValue_shouldReturnTrue ();
	test_elektraDiffKeyValueChanged_onChangedMeta_shouldReturnFalse ();
	test_elektraDiffKeyValueChanged_onNonExistingKey_shouldReturnFalse ();
	test_elektraDiffKeyValueChanged_onNullParameters_shouldReturnFalse ();

	test_elektraDiffKeyOnlyMetaChanged_onChangedMeta_shouldReturnTrue ();
	test_elektraDiffKeyOnlyMetaChanged_onChangedMetaAndValue_shouldReturnFalse ();
	test_elektraDiffKeyOnlyMetaChanged_onChangedValue_shouldReturnFalse ();
	test_elektraDiffKeyOnlyMetaChanged_onNonExistingKey_shouldReturnFalse ();
	test_elektraDiffKeyOnlyMetaChanged_onNullParameters_shouldReturnFalse ();

	test_elektraDiffGetAddedMetaKeys_shouldGetAddedMetaKeys ();
	test_elektraDiffGetAddedMetaKeys_diffOrKeyIsNull_shouldReturnNull ();
	test_elektraDiffGetAddedMetaKeys_unknownKey_shouldReturnNull ();

	test_elektraDiffGetRemovedMetaKeys_shouldGetRemovedMetaKeys ();
	test_elektraDiffGetRemovedMetaKeys_diffOrKeyIsNull_shouldReturnNull ();
	test_elektraDiffGetRemovedMetaKeys_unknownKey_shouldReturnNull ();

	test_elektraDiffGetModifiedMetaKeys_shouldGetModifiedMetaKeys ();
	test_elektraDiffGetModifiedMetaKeys_diffOrKeyIsNull_shouldReturnNull ();
	test_elektraDiffGetModifiedMetaKeys_unknownKey_shouldReturnNull ();

	test_refCounting ();

	test_elektraDiffCalculate_nullKeySets_shouldReturnNull ();
	test_elektraDiffCalculate_shouldReturnDiff ();

	test_elektraDiffAppend_addedKeys_shouldWork ();
	test_elektraDiffAppend_modifiedKeys_shouldWork ();
	test_elektraDiffAppend_removedKeys_shouldWork ();

	test_elektraDiffRemoveSameOrBelow_shouldWork ();
	test_elektraDiffRemoveOther_shouldWork ();
	test_elektraDiffCut_shouldWork ();

	test_elektraDiffDup_shouldDuplicate ();

	elektraDiffDel (demoDiff);

	printf ("\ntest_diff RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
