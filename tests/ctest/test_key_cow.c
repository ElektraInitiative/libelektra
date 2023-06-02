/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void keyNew_should_only_set_keyName (void)
{
	printf ("Test %s\n", __func__);

	// Arrange & Act
	Key * key = keyNew ("system:/hello", KEY_END);

	// Assert
	succeed_if (key->keyName != NULL, "keyName is NULL");
	succeed_if (key->keyData == NULL, "keyData should be NULL");
	succeed_if (key->meta == NULL, "meta should be NULL");

	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");

	keyDel (key);
}

static void keyNew_should_set_name_and_data (void)
{
	printf ("Test %s\n", __func__);

	// Arrange & Act
	Key * key = keyNew ("system:/test", KEY_VALUE, "original", KEY_END);

	// Assert
	succeed_if (key->keyName != NULL, "keyName is NULL");
	succeed_if (key->keyData != NULL, "keyData is NULL");
	succeed_if (key->meta == NULL, "meta should be NULL");

	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");
	succeed_if (key->keyData->refs == 1, "keyData should only have 1 reference");

	keyDel (key);
}

static void keyDetachKeyName_should_do_nothing_if_no_other_references (void)
{
	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keyDetachKeyName (key);

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");

	keyDel (key);
}

static void keyDetachKeyName_should_copy_keyName_if_other_references (void)
{
	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keyDetachKeyName (key);

	// Assert
	succeed_if (key->keyName != original, "keyName of modified key should have been replaced");
	succeed_if (copy->keyName == original, "keyName of copied key should not have been replaced");

	succeed_if (key->keyName->refs == 1, "keyName of modified key should only have 1 reference");
	succeed_if (copy->keyName->refs == 1, "keyName of copied key should only have 1 reference");

	succeed_if_same_string (keyName (key), keyName (copy));
	succeed_if (key->keyName->key != copy->keyName->key, "pointer to key should be different!");

	keyDel (key);
	keyDel (copy);
}

static void keyCopy_should_copy_and_set_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_VALUE, "original", KEY_END);

	// Act
	Key * copied = keyCopy (keyNew ("/copy", KEY_END), key, KEY_CP_ALL);

	// Assert
	succeed_if (key->keyName == copied->keyName, "keyName should point to same struct");
	succeed_if (key->keyData == copied->keyData, "keyData should point to same struct");

	succeed_if (key->keyName->refs == 2, "keyName should have 2 references");
	succeed_if (key->keyData->refs == 2, "keyData should have 2 references");

	keyDel (key);
	keyDel (copied);
}

static void keyDel_should_decrease_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_VALUE, "original", KEY_END);

	// Act
	Key * copied = keyCopy (keyNew ("/copy", KEY_END), key, KEY_CP_ALL);

	succeed_if (copied->keyName->refs == 2, "keyName should have 2 references");
	succeed_if (copied->keyData->refs == 2, "keyData should have 2 references");

	keyDel (key);

	// Assert
	succeed_if (copied->keyName->refs == 1, "keyName should have 1 references");
	succeed_if (copied->keyData->refs == 1, "keyData should have 1 references");

	keyDel (copied);
}

static void keySetString_should_not_replace_keyData_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_VALUE, "original", KEY_END);
	const struct _KeyData * original = key->keyData;

	// Act
	keySetString (key, "replaced");

	// Assert
	succeed_if (key->keyData == original, "keyData should not have been replaced");
	succeed_if (key->keyData->refs == 1, "keyData should only have 1 reference");
	succeed_if_same_string (key->keyData->data.c, "replaced");

	keyDel (key);
}

static void keySetString_should_replace_keyData_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_VALUE, "original", KEY_END);
	const struct _KeyData * original = key->keyData;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keySetString (key, "replaced");

	// Assert
	succeed_if (key->keyData != original, "keyData should have been replaced");
	succeed_if (key->keyData->refs == 1, "key keyData should only have 1 reference");
	succeed_if (copy->keyData->refs == 1, "copy keyData should only have 1 reference");
	succeed_if_same_string (key->keyData->data.c, "replaced");

	keyDel (key);
	keyDel (copy);
}

static void keySetBinary_should_not_replace_keyData_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	const uint8_t originalBinary[3] = { 1, 2, 3 };
	const uint8_t newBinary[4] = { 4, 3, 2, 1 };

	Key * key = keyNew ("system:/test", KEY_BINARY, KEY_SIZE, 3, KEY_VALUE, originalBinary, KEY_END);
	const struct _KeyData * original = key->keyData;

	// Act
	keySetBinary (key, newBinary, 4);

	// Assert
	succeed_if (key->keyData == original, "keyData should not have been replaced");
	succeed_if (key->keyData->refs == 1, "keyData should only have 1 reference");
	succeed_if (key->keyData->dataSize == 4, "new data size should be 4");

	keyDel (key);
}

static void keySetBinary_should_replace_keyData_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	const uint8_t originalBinary[3] = { 1, 2, 3 };
	const uint8_t newBinary[4] = { 4, 3, 2, 1 };

	Key * key = keyNew ("system:/test", KEY_BINARY, KEY_SIZE, 3, KEY_VALUE, originalBinary, KEY_END);
	const struct _KeyData * original = key->keyData;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keySetBinary (key, newBinary, 4);

	// Assert
	succeed_if (key->keyData != original, "keyData should have been replaced");
	succeed_if (key->keyData->refs == 1, "key keyData should only have 1 reference");
	succeed_if (copy->keyData->refs == 1, "copy keyData should only have 1 reference");
	succeed_if (key->keyData->dataSize == 4, "new data size should be 4");

	keyDel (key);
	keyDel (copy);
}

static void keySetName_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keySetName (key, "system:/newTest");

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/newTest");

	keyDel (key);
}

static void keySetName_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keySetName (key, "system:/newTest");

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should only have 1 reference");
	succeed_if (copy->keyName->refs == 1, "copy keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/newTest");

	keyDel (key);
	keyDel (copy);
}

static void keySetBaseName_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test/x", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keySetBaseName (key, "newTest");

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/test/newTest");

	keyDel (key);
}

static void keySetBaseName_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test/x", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KS_END), key, KEY_CP_ALL);

	// Act
	keySetBaseName (key, "newTest");

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should only have 1 reference");
	succeed_if (copy->keyName->refs == 1, "copy keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/test/newTest");

	keyDel (key);
	keyDel (copy);
}

static void keyAddName_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keyAddName (key, "added");

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/test/added");

	keyDel (key);
}

static void keyAddName_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keyAddName (key, "added");

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should only have 1 reference");
	succeed_if (copy->keyName->refs == 1, "key keyName should only have 1 reference");

	succeed_if_same_string (key->keyName->key, "system:/test/added");

	keyDel (key);
	keyDel (copy);
}

static void keyAddBaseName_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keyAddBaseName (key, "added");

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should only have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/test/added");

	keyDel (key);
}

static void keyAddBaseName_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keyAddBaseName (key, "added");

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should only have 1 reference");
	succeed_if (copy->keyName->refs == 1, "copy keyName should only have 1 reference");

	succeed_if_same_string (key->keyName->key, "system:/test/added");

	keyDel (key);
	keyDel (copy);
}

static void keyReplacePrefix_same_as_original_should_replace_with_new (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);

	Key * oldPrefix = keyNew ("system:/test", KEY_END);
	Key * newPrefix = keyNew ("system:/bla/bla", KEY_END);

	// Act
	keyReplacePrefix (key, oldPrefix, newPrefix);

	// Assert
	succeed_if (key->keyName == newPrefix->keyName, "keyName should be same as newPrefix");
	succeed_if (key->keyName->refs == 2, "keyName should have 2 references");

	keyDel (key);
	keyDel (oldPrefix);
	keyDel (newPrefix);
}

static void keyReplacePrefix_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test/first", KEY_END);
	const struct _KeyName * original = key->keyName;

	Key * oldPrefix = keyNew ("system:/test", KEY_END);
	Key * newPrefix = keyNew ("system:/bla/bla", KEY_END);

	// Act
	keyReplacePrefix (key, oldPrefix, newPrefix);

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should have 1 reference");
	succeed_if_same_string (key->keyName->key, "system:/bla/bla/first");

	keyDel (key);
	keyDel (oldPrefix);
	keyDel (newPrefix);
}

static void keyReplacePrefix_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test/first", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	Key * oldPrefix = keyNew ("system:/test", KEY_END);
	Key * newPrefix = keyNew ("system:/bla/bla", KEY_END);

	// Act
	keyReplacePrefix (key, oldPrefix, newPrefix);

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should have 1 reference");
	succeed_if (copy->keyName->refs == 1, "copy keyName should have 1 reference");

	succeed_if_same_string (key->keyName->key, "system:/bla/bla/first");

	keyDel (key);
	keyDel (oldPrefix);
	keyDel (newPrefix);
	keyDel (copy);
}

static void keySetNamespace_should_not_replace_keyName_when_no_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;

	// Act
	keySetNamespace (key, KEY_NS_USER);

	// Assert
	succeed_if (key->keyName == original, "keyName should not have been replaced");
	succeed_if (key->keyName->refs == 1, "keyName should have 1 references");
	succeed_if_same_string (key->keyName->key, "user:/test");

	keyDel (key);
}

static void keySetNamespace_should_replace_keyName_when_other_references (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * key = keyNew ("system:/test", KEY_END);
	const struct _KeyName * original = key->keyName;
	Key * copy = keyCopy (keyNew ("/", KEY_END), key, KEY_CP_ALL);

	// Act
	keySetNamespace (key, KEY_NS_USER);

	// Assert
	succeed_if (key->keyName != original, "keyName should have been replaced");
	succeed_if (key->keyName->refs == 1, "key keyName should have 1 references");
	succeed_if (copy->keyName->refs == 1, "copy keyName should have 1 references");

	succeed_if_same_string (key->keyName->key, "user:/test");

	keyDel (key);
	keyDel (copy);
}

static void keyLock_should_work_with_copies (void)
{
	printf ("Test %s\n", __func__);

	// Arrange
	Key * original = keyNew ("system:/original", KEY_VALUE, "orig", KEY_END);
	keySetMeta (original, "meta:/what", "nothing");

	Key * copy = keyCopy (keyNew ("/", KEY_END), original, KEY_CP_ALL);

	keyLock (original, KEY_LOCK_NAME | KEY_LOCK_VALUE | KEY_LOCK_META);

	// Act
	keyAddName (copy, "modified");
	keySetString (copy, "modified value");
	keySetMeta (copy, "meta:/what", "idontknow");

	// Assert
	succeed_if (keyIsLocked (original, KEY_LOCK_NAME), "original key name should be locked");
	succeed_if (keyIsLocked (original, KEY_LOCK_VALUE), "original key value should be locked");
	succeed_if (keyIsLocked (original, KEY_LOCK_META), "original key meta should be locked");

	succeed_if (!keyIsLocked (copy, KEY_LOCK_NAME), "copied key name should not be locked");
	succeed_if (!keyIsLocked (copy, KEY_LOCK_VALUE), "copied key value should not be locked");
	succeed_if (!keyIsLocked (copy, KEY_LOCK_META), "copied key meta should not be locked");

	succeed_if (strcmp (keyName (original), "system:/original") == 0, "original key value should not have been changed");
	succeed_if (strcmp (keyString (original), "orig") == 0, "original key name should not have been changed");
	succeed_if (strcmp (keyString (keyGetMeta (original, "meta:/what")), "nothing") == 0,
		    "original key meta should not have been changed");

	succeed_if (strcmp (keyName (copy), "system:/original/modified") == 0, "copied key value should have been changed");
	succeed_if (strcmp (keyString (copy), "modified value") == 0, "copied key name should have been changed");
	succeed_if (strcmp (keyString (keyGetMeta (copy, "meta:/what")), "idontknow") == 0, "copied key meta should have been changed");

	keyDel (original);
	keyDel (copy);
}

static void test_mmap_flag_methods (void)
{
	printf ("Test %s\n", __func__);

	struct _KeyData * data = elektraCalloc (sizeof (struct _KeyData));
	succeed_if (isKeyDataInMmap (data) == false, "newly created key data should not have MMAP flag");

	setKeyDataIsInMmap (data, true);
	succeed_if (isKeyDataInMmap (data) == true, "key data should have MMAP flag");

	setKeyDataIsInMmap (data, false);
	succeed_if (isKeyDataInMmap (data) == false, "key data should not have MMAP flag");

	elektraFree (data);


	struct _KeyName * name = elektraCalloc (sizeof (struct _KeyName));
	succeed_if (isKeyNameInMmap (name) == false, "newly created key name should not have MMAP flag");

	setKeyNameIsInMmap (name, true);
	succeed_if (isKeyNameInMmap (name) == true, "key name should have MMAP flag");

	setKeyNameIsInMmap (name, false);
	succeed_if (isKeyNameInMmap (name) == false, "key name should not have MMAP flag");

	elektraFree (name);
}

int main (int argc, char ** argv)
{
	printf ("KEY COW      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	keyNew_should_only_set_keyName ();
	keyNew_should_set_name_and_data ();

	keyDetachKeyName_should_do_nothing_if_no_other_references ();
	keyDetachKeyName_should_copy_keyName_if_other_references ();

	keyCopy_should_copy_and_set_references ();

	keyDel_should_decrease_references ();

	keySetString_should_not_replace_keyData_when_no_other_references ();
	keySetString_should_replace_keyData_when_other_references ();

	keySetBinary_should_not_replace_keyData_when_no_other_references ();
	keySetBinary_should_replace_keyData_when_other_references ();

	keySetName_should_not_replace_keyName_when_no_other_references ();
	keySetName_should_replace_keyName_when_other_references ();

	keySetBaseName_should_not_replace_keyName_when_no_other_references ();
	keySetBaseName_should_replace_keyName_when_other_references ();

	keyAddName_should_not_replace_keyName_when_no_other_references ();
	keyAddName_should_replace_keyName_when_other_references ();

	keyAddBaseName_should_not_replace_keyName_when_no_other_references ();
	keyAddBaseName_should_replace_keyName_when_other_references ();

	keyReplacePrefix_same_as_original_should_replace_with_new ();
	keyReplacePrefix_should_not_replace_keyName_when_no_other_references ();
	keyReplacePrefix_should_replace_keyName_when_other_references ();

	keySetNamespace_should_not_replace_keyName_when_no_other_references ();
	keySetNamespace_should_replace_keyName_when_other_references ();

	keyLock_should_work_with_copies ();

	test_mmap_flag_methods ();

	print_result ("test_key_cow");
	return nbError;
}
