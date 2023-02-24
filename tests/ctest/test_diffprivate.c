#include "../../src/libs/elektra/diff.c"
#include <tests_internal.h>

static void test_keyValueDifferent_bothNull_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Act
	bool result = keyValueDifferent (NULL, NULL);

	// Act
	succeed_if (result == false, "should NOT be different");
}

static void test_keyValueDifferent_sameKey_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/elektra", KEY_VALUE, "test", KEY_END);

	// Act
	bool result = keyValueDifferent (key, key);

	// Act
	succeed_if (result == false, "should NOT be different");

	keyDel (key);
}

static void test_keyValueDifferent_sameName_sameString_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_VALUE, "test", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_VALUE, "test", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == false, "should NOT be different");

	keyDel (key1);
	keyDel (key2);
}

static void test_keyValueDifferent_sameName_sameBinary_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == false, "should NOT be different");

	keyDel (key1);
	keyDel (key2);
}

static void test_keyValueDifferent_sameName_withoutValue_shouldReturnFalse (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == false, "should NOT be different");

	keyDel (key1);
	keyDel (key2);
}

static void test_keyValueDifferent_oneNull_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/elektra", KEY_VALUE, "test", KEY_END);

	// Act & Assert
	succeed_if (keyValueDifferent (key, NULL) == true, "should be different (key, NULL)");
	succeed_if (keyValueDifferent (NULL, key) == true, "should be different (NULL, key)");

	keyDel (key);
}

static void test_keyValueDifferent_sameName_differentStringValues_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_VALUE, "test1", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_VALUE, "test2", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == true, "should be different");

	keyDel (key1);
	keyDel (key2);
}

static void test_keyValueDifferent_sameName_differentBinaryValues_sameSize_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test1", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test2", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == true, "should be different");

	keyDel (key1);
	keyDel (key2);
}


static void test_keyValueDifferent_sameName_differentBinaryValues_differentSize_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test1", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test_2", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == true, "should be different");

	keyDel (key1);
	keyDel (key2);
}


static void test_keyValueDifferent_sameName_withAndWithoutValue_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_VALUE, "test1", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == true, "should be different");

	keyDel (key1);
	keyDel (key2);
}

void test_keyValueDifferent_sameName_differentValueTypes_shouldReturnTrue (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key1 = keyNew ("system:/elektra", KEY_VALUE, "test1", KEY_END);
	Key * key2 = keyNew ("system:/elektra", KEY_BINARY, KEY_VALUE, "test1", KEY_END);

	// Act
	bool result = keyValueDifferent (key1, key2);

	// Act
	succeed_if (result == true, "should be different");

	keyDel (key1);
	keyDel (key2);
}


static void test_findDifferences_NULL_shouldReturnEmptyKeySets (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (NULL, NULL, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "should not find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
}

static void test_findDifferences_emptyKeySets_shouldReturnEmptyKeySets (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (0, KS_END);
	KeySet * old = ksNew (0, KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "should not find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_fullAndEmptyKeySet_shouldReturnAddedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/hello", KEY_END), keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_END), KS_END);

	KeySet * old = ksNew (0, KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 2, "should find 2 added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "should not find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_emptyAndFullKeySet_shouldReturnRemovedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (0, KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/hello", KEY_END), keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 2, "should find 2 removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "should not find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_modifiedValue_shouldReturnModifiedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new =
		ksNew (1, keyNew ("system:/test/hello", KEY_END), keyNew ("system:/test/name", KEY_VALUE, "franz1", KEY_END), KS_END);
	KeySet * old =
		ksNew (1, keyNew ("system:/test/hello", KEY_END), keyNew ("system:/test/name", KEY_VALUE, "franz2", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 1, "should find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_addedMeta_shouldReturnModifiedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "world", KEY_END), KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 1, "should find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_removedMeta_shouldReturnModifiedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_END), KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "franz", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 1, "should find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_modifiedMeta_shouldReturnModifiedKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "world", KEY_END), KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "franz", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 1, "should find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_sameMeta_shouldReturnEmptyKeySets (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "world", KEY_END), KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_META, "meta:/hello", "world", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if (ksGetSize (addedKeys) == 0, "should not find added keys");
	succeed_if (ksGetSize (removedKeys) == 0, "should not find removed keys");
	succeed_if (ksGetSize (modifiedKeys) == 0, "should not find modified keys");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_modifiedShouldContainOldKeys (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "franz", KEY_END), KS_END);
	KeySet * old = ksNew (1, keyNew ("system:/test/name", KEY_VALUE, "hans", KEY_END), KS_END);
	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	Key * modifiedKey = ksLookupByName (modifiedKeys, "system:/test/name", KDB_O_NONE);
	succeed_if (modifiedKey != NULL, "system:/test/name should be modified!");
	succeed_if_fmt (strcmp ("hans", keyString (modifiedKey)) == 0, "should have old value (hans), was %s", keyString (modifiedKey));

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}

static void test_findDifferences_realWorld_withParentKey (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * parentKey = keyNew ("system:/test", KEY_END);

	KeySet * new = ksNew (1, keyNew ("system:/test/display", KEY_VALUE, "screen1", KEY_META, "meta:/connected", "2022-01-01", KEY_END),
			      keyNew ("system:/test/display/background", KEY_VALUE, "blue", KEY_END),
			      keyNew ("system:/test/display/brightness", KEY_VALUE, "85", KEY_END),
			      keyNew ("system:/test/display/colorspace", KEY_VALUE, "sRGB", KEY_END),
			      keyNew ("system:/test/display/zulu", KEY_VALUE, "hi", KEY_END),
			      keyNew ("system:/test/display/hdrCapable", KEY_VALUE, "no", KEY_END),
			      keyNew ("system:/untouched/modified", KEY_VALUE, "123", KEY_END),
			      keyNew ("system:/untouched/added", KEY_VALUE, "123", KEY_END),

			      KS_END);

	KeySet * old = ksNew (1, keyNew ("system:/test/display", KEY_VALUE, "screen1", KEY_META, "meta:/connected", "2022-01-02", KEY_END),
			      keyNew ("system:/test/display/background", KEY_VALUE, "red", KEY_END),
			      keyNew ("system:/test/display/model", KEY_VALUE, "Generic Display", KEY_END),
			      keyNew ("system:/test/display/colorspace", KEY_VALUE, "sRGB", KEY_END),
			      keyNew ("system:/test/display/zulu", KEY_VALUE, "hey", KEY_END),
			      keyNew ("system:/test/display/builtinSpeakerVolume", KEY_VALUE, "100", KEY_END),
			      keyNew ("system:/test/display/primary", KEY_VALUE, "yes", KEY_END),
			      keyNew ("system:/test/display/extended", KEY_VALUE, "no", KEY_END),
			      keyNew ("system:/untouched/modified", KEY_VALUE, "456", KEY_END),
			      keyNew ("system:/untouched/removed", KEY_VALUE, "789", KEY_END), KS_END);

	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, parentKey);

	// Assert
	succeed_if_fmt (ksGetSize (addedKeys) == 2, "should find 2 added keys (was %zd)", ksGetSize (addedKeys));
	succeed_if_fmt (ksGetSize (removedKeys) == 4, "should find 4 removed keys (was %zd)", ksGetSize (removedKeys));
	succeed_if_fmt (ksGetSize (modifiedKeys) == 3, "should find 3 modified keys (was %zd)", ksGetSize (modifiedKeys));

	succeed_if (ksLookupByName (modifiedKeys, "system:/untouched/modified", KDB_O_NONE) == NULL,
		    "should not find modified keys outside of parent key");
	succeed_if (ksLookupByName (addedKeys, "system:/untouched/added", KDB_O_NONE) == NULL,
		    "should not find modified keys outside of parent key");
	succeed_if (ksLookupByName (removedKeys, "system:/untouched/removed", KDB_O_NONE) == NULL,
		    "should not find modified keys outside of parent key");

	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display", KDB_O_NONE) != NULL, "system:/test/display should be modified");
	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display/background", KDB_O_NONE) != NULL,
		    "system:/test/display/background should be modified");
	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display/zulu", KDB_O_NONE) != NULL,
		    "system:/test/display/zulu should be modified");

	succeed_if (ksLookupByName (addedKeys, "system:/test/display/brightness", KDB_O_NONE) != NULL,
		    "system:/test/display/brightness should be added");
	succeed_if (ksLookupByName (addedKeys, "system:/test/display/hdrCapable", KDB_O_NONE) != NULL,
		    "system:/test/display/hdrCapable should be added");

	succeed_if (ksLookupByName (removedKeys, "system:/test/display/model", KDB_O_NONE) != NULL,
		    "system:/test/display/model should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/builtinSpeakerVolume", KDB_O_NONE) != NULL,
		    "system:/test/display/builtinSpeakerVolume should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/primary", KDB_O_NONE) != NULL,
		    "system:/test/display/primary should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/extended", KDB_O_NONE) != NULL,
		    "system:/test/display/extended should be removed");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);

	keyDel (parentKey);
}

static void test_findDifferences_realWorld_withoutParentKey (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	KeySet * new = ksNew (1, keyNew ("system:/test/display", KEY_VALUE, "screen1", KEY_META, "meta:/connected", "2022-01-01", KEY_END),
			      keyNew ("system:/test/display/background", KEY_VALUE, "blue", KEY_END),
			      keyNew ("system:/test/display/brightness", KEY_VALUE, "85", KEY_END),
			      keyNew ("system:/test/display/colorspace", KEY_VALUE, "sRGB", KEY_END),
			      keyNew ("system:/test/display/zulu", KEY_VALUE, "hi", KEY_END),
			      keyNew ("system:/test/display/hdrCapable", KEY_VALUE, "no", KEY_END),
			      keyNew ("system:/untouched/modified", KEY_VALUE, "123", KEY_END),
			      keyNew ("system:/untouched/added", KEY_VALUE, "123", KEY_END),

			      KS_END);

	KeySet * old = ksNew (1, keyNew ("system:/test/display", KEY_VALUE, "screen1", KEY_META, "meta:/connected", "2022-01-02", KEY_END),
			      keyNew ("system:/test/display/background", KEY_VALUE, "red", KEY_END),
			      keyNew ("system:/test/display/model", KEY_VALUE, "Generic Display", KEY_END),
			      keyNew ("system:/test/display/colorspace", KEY_VALUE, "sRGB", KEY_END),
			      keyNew ("system:/test/display/zulu", KEY_VALUE, "hey", KEY_END),
			      keyNew ("system:/test/display/builtinSpeakerVolume", KEY_VALUE, "100", KEY_END),
			      keyNew ("system:/test/display/primary", KEY_VALUE, "yes", KEY_END),
			      keyNew ("system:/test/display/extended", KEY_VALUE, "no", KEY_END),
			      keyNew ("system:/untouched/modified", KEY_VALUE, "456", KEY_END),
			      keyNew ("system:/untouched/removed", KEY_VALUE, "789", KEY_END), KS_END);

	KeySet * addedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);
	KeySet * modifiedKeys = ksNew (0, KS_END);

	// Act
	findDifferences (new, old, addedKeys, removedKeys, modifiedKeys, NULL);

	// Assert
	succeed_if_fmt (ksGetSize (addedKeys) == 3, "should find 3 added keys (was %zd)", ksGetSize (addedKeys));
	succeed_if_fmt (ksGetSize (removedKeys) == 5, "should find 5 removed keys (was %zd)", ksGetSize (removedKeys));
	succeed_if_fmt (ksGetSize (modifiedKeys) == 4, "should find 4 modified keys (was %zd)", ksGetSize (modifiedKeys));

	succeed_if (ksLookupByName (modifiedKeys, "system:/untouched/modified", KDB_O_NONE) != NULL,
		    "should find system:/untouched/modified");
	succeed_if (ksLookupByName (addedKeys, "system:/untouched/added", KDB_O_NONE) != NULL, "should find system:/untouched/added");
	succeed_if (ksLookupByName (removedKeys, "system:/untouched/removed", KDB_O_NONE) != NULL, "should find system:/untouched/removed");

	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display", KDB_O_NONE) != NULL, "system:/test/display should be modified");
	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display/background", KDB_O_NONE) != NULL,
		    "system:/test/display/background should be modified");
	succeed_if (ksLookupByName (modifiedKeys, "system:/test/display/zulu", KDB_O_NONE) != NULL,
		    "system:/test/display/zulu should be modified");

	succeed_if (ksLookupByName (addedKeys, "system:/test/display/brightness", KDB_O_NONE) != NULL,
		    "system:/test/display/brightness should be added");
	succeed_if (ksLookupByName (addedKeys, "system:/test/display/hdrCapable", KDB_O_NONE) != NULL,
		    "system:/test/display/hdrCapable should be added");

	succeed_if (ksLookupByName (removedKeys, "system:/test/display/model", KDB_O_NONE) != NULL,
		    "system:/test/display/model should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/builtinSpeakerVolume", KDB_O_NONE) != NULL,
		    "system:/test/display/builtinSpeakerVolume should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/primary", KDB_O_NONE) != NULL,
		    "system:/test/display/primary should be removed");
	succeed_if (ksLookupByName (removedKeys, "system:/test/display/extended", KDB_O_NONE) != NULL,
		    "system:/test/display/extended should be removed");

	ksDel (addedKeys);
	ksDel (removedKeys);
	ksDel (modifiedKeys);
	ksDel (new);
	ksDel (old);
}


int main (int argc, char ** argv)
{
	printf ("DIFF (PRIVATE)       TESTS\n");
	printf ("==========================\n\n");

	init (argc, argv);

	test_keyValueDifferent_bothNull_shouldReturnFalse ();
	test_keyValueDifferent_sameKey_shouldReturnFalse ();
	test_keyValueDifferent_sameName_sameString_shouldReturnFalse ();
	test_keyValueDifferent_sameName_sameBinary_shouldReturnFalse ();
	test_keyValueDifferent_sameName_withoutValue_shouldReturnFalse ();
	test_keyValueDifferent_oneNull_shouldReturnTrue ();
	test_keyValueDifferent_sameName_differentStringValues_shouldReturnTrue ();
	test_keyValueDifferent_sameName_differentBinaryValues_sameSize_shouldReturnTrue ();
	test_keyValueDifferent_sameName_differentBinaryValues_differentSize_shouldReturnTrue ();
	test_keyValueDifferent_sameName_withAndWithoutValue_shouldReturnTrue ();
	test_keyValueDifferent_sameName_differentValueTypes_shouldReturnTrue ();

	test_findDifferences_NULL_shouldReturnEmptyKeySets ();
	test_findDifferences_emptyKeySets_shouldReturnEmptyKeySets ();
	test_findDifferences_fullAndEmptyKeySet_shouldReturnAddedKeys ();
	test_findDifferences_emptyAndFullKeySet_shouldReturnRemovedKeys ();
	test_findDifferences_modifiedValue_shouldReturnModifiedKeys ();
	test_findDifferences_addedMeta_shouldReturnModifiedKeys ();
	test_findDifferences_removedMeta_shouldReturnModifiedKeys ();
	test_findDifferences_modifiedMeta_shouldReturnModifiedKeys ();
	test_findDifferences_sameMeta_shouldReturnEmptyKeySets ();
	test_findDifferences_modifiedShouldContainOldKeys ();

	test_findDifferences_realWorld_withParentKey ();
	test_findDifferences_realWorld_withoutParentKey ();

	printf ("\ntest_diffprivate RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
