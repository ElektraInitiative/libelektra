/**
 * @file
 *
 * @brief A plugin for renaming
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

/* used for asprintf */
#define _GNU_SOURCE

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "rename.h"
#include <tests_plugin.h>


static KeySet * createSimpleTestKeys (void)
{
	return ksNew (20, keyNew ("user:/tests/rename/will/be/stripped/key1", KEY_VALUE, "value1", KEY_END),
		      keyNew ("user:/tests/rename/will/be/stripped/key2", KEY_VALUE, "value2", KEY_END),
		      keyNew ("user:/tests/rename/will/be/stripped", KEY_VALUE, "value3", KEY_END),
		      keyNew ("user:/tests/rename/will/not/be/stripped/key4", KEY_VALUE, "value4", KEY_END), KS_END);
}

static KeySet * createSimpleMetaTestKeys (void)
{
	// clang-format off
	return ksNew (20,
			keyNew("user:/tests/rename/will/be/stripped/key1",
					KEY_VALUE, "value1",
					KEY_META, "rename/cut", "will/be/stripped",
					KEY_END),
			keyNew("user:/tests/rename/will/be/stripped/key2",
					KEY_VALUE, "value2",
					KEY_META, "rename/cut", "will/be/stripped",
					KEY_END),
			keyNew("user:/tests/rename/will/be/stripped",
					KEY_VALUE, "value3",
					KEY_END),
			keyNew("user:/tests/rename/will/not/be/stripped/key4",
					KEY_VALUE, "value4",
					KEY_END),
			KS_END);
	// clang-format on
}

static void checkSimpleTestKeys (KeySet * ks)
{
	/* the first two keys should have been renamed */
	Key * key = ksLookupByName (ks, "user:/tests/rename/key1", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly renamed");
	key = ksLookupByName (ks, "user:/tests/rename/key2", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");
	/* the fourth key was not renamed because the prefix did not match */
	key = ksLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if (key, "key4 was renamed although its prefix did not match");
}

static void compareKeySets (KeySet * ks, KeySet * expected)
{
	succeed_if (ksGetSize (expected) == ksGetSize (ks), "KeySet on set does not contain the same amount of keys");

	for (elektraCursor it = 0; it < ksGetSize (expected); ++it)
	{
		Key * current = ksAtCursor (expected, it);
		Key * key = ksLookup (ks, current, KDB_O_NONE);
		succeed_if (key, "Expected key was not found in KeySet");
		succeed_if (!strcmp (keyString (key), keyString (current)), "Value of key was modified");
	}
}

static void test_simpleCutOnGet (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "will/be/stripped", KEY_END), KS_END);
	PLUGIN_OPEN ("rename");

	KeySet * ks = createSimpleTestKeys ();
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");


	checkSimpleTestKeys (ks);
	ksDel (ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_metaCutOnGet (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("rename");

	KeySet * ks = createSimpleMetaTestKeys ();
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	checkSimpleTestKeys (ks);

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_simpleCutRestoreOnSet (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	Key * parentKeyCopy = keyDup (parentKey, KEY_CP_ALL);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "will/be/stripped", KEY_END), KS_END);
	PLUGIN_OPEN ("rename");

	KeySet * ks = createSimpleTestKeys ();
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	/* test that the keys have been correctly restored */
	KeySet * expected = createSimpleTestKeys ();

	/* the parent key is restored from user:/tests/rename/will/be/stripped
	 * and therefore will have its key value
	 */
	keySetString (parentKeyCopy, "value3");
	ksAppendKey (expected, parentKeyCopy);

	compareKeySets (ks, expected);
	ksDel (expected);
	ksDel (ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_withoutConfig (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	Key * parentKeyCopy = keyDup (parentKey, KEY_CP_ALL);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("rename");

	KeySet * ks = createSimpleTestKeys ();
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	KeySet * expected = createSimpleTestKeys ();
	ksAppendKey (expected, parentKeyCopy);

	compareKeySets (ks, expected);
	ksDel (expected);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_metaConfigTakesPrecedence (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "will/be", KEY_END), KS_END);
	PLUGIN_OPEN ("rename");

	KeySet * ks = createSimpleMetaTestKeys ();
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* the first two keys should have been renamed by their metadata */
	Key * key = ksLookupByName (ks, "user:/tests/rename/key1", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly renamed");
	key = ksLookupByName (ks, "user:/tests/rename/key2", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");

	/* the third key should have been renamed by the global config */
	key = ksLookupByName (ks, "user:/tests/rename/stripped", KDB_O_NONE);
	succeed_if (key, "key3 was renamed but would replace the parent key");

	/* the fourth key was not renamed because the prefix did not match */
	key = ksLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if (key, "key4 was renamed although its prefix did not match");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_keyCutNamePart (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	Key * result = elektraKeyCreateNewName (parentKey, parentKey, "wont/cut/this", NULL, NULL, NULL, 0);
	succeed_if (!result, "parentKey was modified although it should have been ignored");

	/* cutting works correctly without trailing slash */
	Key * testKey = keyNew ("user:/tests/rename/will/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "will/cut/this", NULL, NULL, NULL, 0);
	succeed_if (result, "key1 was not cut")
		succeed_if (!strcmp (keyName (result), "user:/tests/rename/key1"), "cutting key1 did not yield the expected result");
	keyDel (testKey);
	keyDel (result);

	/* cutting works correctly with trailing slash */
	testKey = keyNew ("user:/tests/rename/will/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "will/cut/this/", NULL, NULL, NULL, 0);
	succeed_if (result, "key1 was not cut")
		succeed_if (!strcmp (keyName (result), "user:/tests/rename/key1"), "cutting key1 did not yield the expected result");
	keyDel (testKey);
	keyDel (result);

	/* disallow leading slashes */
	testKey = keyNew ("user:/tests/rename/wont/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "/wont/cut/this", NULL, NULL, NULL, 0);
	succeed_if (!result, "key was cut although it the cutpath contained a leading slash");
	keyDel (testKey);
	keyDel (parentKey);
}

static void test_rebaseOfNewKeys (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "new/base", KEY_END), KS_END);
	PLUGIN_OPEN ("rename");

	// clang-format off
	KeySet *ks = ksNew(20,
			/* this key was seen by rename before and wont be changed */
			keyNew("user:/tests/rename/key1",
					KEY_VALUE, "value1",
					KEY_META, ELEKTRA_ORIGINAL_NAME_META, "user:/tests/rename/key1",
					KEY_END),
			/* this key was not seen by rename before and will be renamed */
			keyNew("user:/tests/rename/key2",
					KEY_VALUE, "value2",
					KEY_END),
			KS_END);
	// clang-format on

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	Key * key = ksLookupByName (ks, "user:/tests/rename/key1", KDB_O_NONE);
	succeed_if (key, "key1 was not found anymore, but it should not have been renamed");

	key = ksLookupByName (ks, "user:/tests/rename/new/base/key2", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_addNewBaseToParentKey (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "new/base", KEY_END), KS_END);

	PLUGIN_OPEN ("rename");

	KeySet * ks = ksNew (0, KS_END);
	keyIncRef (parentKey);
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	Key * key = ksLookupByName (ks, "user:/tests/rename/new/base", 0);
	succeed_if (key, "new base was not correctly appended to parent key");

	ksDel (ks);
	keyDecRef (parentKey);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_replaceString (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/cut", KEY_VALUE, "will/be/stripped", KEY_END),
			       keyNew ("system:/replacewith", KEY_VALUE, "stripped/it/is", KEY_END), KS_END);

	KeySet * ks = createSimpleTestKeys ();
	ksAppendKey (ks, parentKey);

	PLUGIN_OPEN ("rename");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/stripped/it/is/key1", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/stripped/it/is/key2", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if (key, "key4 was not correctly rename");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_toUpper (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/toupper", KEY_VALUE, "0", KEY_END), KS_END);
	KeySet * ks = createSimpleTestKeys ();
	ksAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/WILL/BE/STRIPPED/KEY1", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/WILL/BE/STRIPPED/KEY2", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/WILL/NOT/BE/STRIPPED/KEY4", KDB_O_NONE);
	succeed_if (key, "key4 was not correctly rename");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_toLower (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/tolower", KEY_VALUE, "0", KEY_END), KS_END);
	KeySet * ks = ksNew (20, keyNew ("user:/tests/rename/AM/I/LOWERCASE", KEY_VALUE, "val1", KEY_END),
			     keyNew ("user:/tests/rename/I/HOPE/IM/LOWERCASE/TOO", KEY_VALUE, "val2", KEY_END), KS_END);
	ksAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/am/i/lowercase", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/i/hope/im/lowercase/too", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mixCase (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/tolower", KEY_VALUE, "1", KEY_END), keyNew ("system:/toupper", KEY_VALUE, "4", KEY_END),
			       KS_END);
	KeySet * ks =
		ksNew (20, keyNew ("user:/tests/rename/am/i/LOWERCASE", KEY_VALUE, "val1", KEY_END),
		       keyNew ("user:/tests/rename/hopefullystilllower/upper/upper/upper/LOWERCASE", KEY_VALUE, "val2", KEY_END), KS_END);
	ksAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/AM/I/lowercase", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = ksLookupByName (ks, "user:/tests/rename/hopefullystilllower/UPPER/UPPER/UPPER/lowercase", KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_write (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("system:/tolower", KEY_VALUE, "1", KEY_END), keyNew ("system:/get/case", KEY_VALUE, "toupper", KEY_END),
		       keyNew ("system:/set/case", KEY_VALUE, "keyname", KEY_END), KS_END);
	KeySet * ks = ksNew (20, keyNew ("user:/tests/rename/uppercase/uppercase/uppercase/LOWERCASE", KEY_VALUE, "test", KEY_END), KS_END);
	ksAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/lowercase", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	key = ksLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/lowercase", KDB_O_NONE);
	succeed_if (key, "key1s name was not correctly saved");
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_write2 (void)
{
	Key * parentKey = keyNew ("user:/tests/rename", KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("system:/tolower", KEY_VALUE, "1", KEY_END), keyNew ("system:/get/case", KEY_VALUE, "tolower", KEY_END),
		       keyNew ("system:/set/case", KEY_VALUE, "toupper", KEY_END), KS_END);
	KeySet * ks = ksNew (20, keyNew ("user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/LOWERCASE", KEY_VALUE, "test", KEY_END), KS_END);
	ksAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/rename/uppercase/uppercase/uppercase/lowercase", KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	key = ksLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/LOWERCASE", KDB_O_NONE);
	succeed_if (key, "key1s name was not correctly saved");
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}
int main (int argc, char ** argv)
{
	printf ("RENAME       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_withoutConfig ();
	test_simpleCutOnGet ();
	test_simpleCutRestoreOnSet ();
	test_metaCutOnGet ();
	test_metaConfigTakesPrecedence ();
	test_rebaseOfNewKeys ();
	test_addNewBaseToParentKey ();

	test_keyCutNamePart ();
	test_toUpper ();
	test_toLower ();
	test_mixCase ();
	test_replaceString ();
	test_write ();
	test_write2 ();
	print_result ("test_rename");

	return nbError;
}
