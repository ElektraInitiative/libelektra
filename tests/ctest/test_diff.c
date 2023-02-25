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

	demoDiff = elektraDiffNew (addedKeys, removedKeys, modifiedKeys, demoParent);

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
	ElektraDiff * diff = elektraDiffNew (added, removed, modified, NULL);

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

	ElektraDiff * diff = elektraDiffNew (ksNew (0, KS_END), ksNew (0, KS_END), ksNew (0, KS_END), NULL);

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

	ElektraDiff * diff = elektraDiffNew (ksNew (0, KS_END), ksNew (0, KS_END), ksNew (0, KS_END), NULL);

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

	elektraDiffDel (demoDiff);

	printf ("\ntest_diff RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
