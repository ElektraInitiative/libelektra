/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

/* used for asprintf */
#define _GNU_SOURCE

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>
#include "rename.h"


static KeySet *createSimpleTestKeys()
{
	return ksNew (20,
			keyNew("user/tests/rename/will/be/stripped/key1", KEY_VALUE, "value1", KEY_END),
			keyNew("user/tests/rename/will/be/stripped/key2", KEY_VALUE, "value2", KEY_END),
			keyNew("user/tests/rename/will/be/stripped", KEY_VALUE, "value3", KEY_END),
			keyNew("user/tests/rename/will/not/be/stripped/key4", KEY_VALUE, "value4", KEY_END),
			KS_END);
}

static KeySet *createSimpleMetaTestKeys()
{
// clang-format off
	return ksNew (20,
			keyNew("user/tests/rename/will/be/stripped/key1",
					KEY_VALUE, "value1",
					KEY_META, "rename/cut", "will/be/stripped",
					KEY_END),
			keyNew("user/tests/rename/will/be/stripped/key2",
					KEY_VALUE, "value2",
					KEY_META, "rename/cut", "will/be/stripped",
					KEY_END),
			keyNew("user/tests/rename/will/be/stripped",
					KEY_VALUE, "value3",
					KEY_END),
			keyNew("user/tests/rename/will/not/be/stripped/key4",
					KEY_VALUE, "value4",
					KEY_END),
			KS_END);
// clang-format on
}

static void checkSimpleTestKeys(KeySet* ks)
{
	/* the first two keys should have been renamed */
	Key* key = ksLookupByName (ks, "user/tests/rename/key1", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly renamed");
	key = ksLookupByName (ks, "user/tests/rename/key2", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly renamed");
	/* the fourth key was not renamed because the prefix did not match */
	key = ksLookupByName (ks, "user/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if(key, "key4 was renamed although its prefix did not match");
}

static void compareKeySets(KeySet* ks, KeySet* expected)
{
	succeed_if(ksGetSize (expected) == ksGetSize (ks), "KeySet on set does not contain the same amount of keys");
	Key* current;
	ksRewind (expected);
	while ((current = ksNext (expected)))
	{
		Key *key = ksLookup (ks, current, KDB_O_NONE);
		succeed_if (key, "Expected key was not found in KeySet");
		succeed_if (!strcmp(keyString(key), keyString(current)), "Value of key was modified");
	}
}

static void test_simpleCutOnGet () 
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/cut", KEY_VALUE, "will/be/stripped", KEY_END), KS_END);
	PLUGIN_OPEN("rename");

	KeySet *ks = createSimpleTestKeys();
	ksAppendKey(ks, parentKey);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	
	checkSimpleTestKeys (ks);
	ksDel(ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	keyDel(parentKey);
	PLUGIN_CLOSE ();
}


static void test_metaCutOnGet()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	KeySet *conf = ksNew (0, KS_END);
	PLUGIN_OPEN("rename");

	KeySet *ks = createSimpleMetaTestKeys();
	ksAppendKey(ks, parentKey);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	checkSimpleTestKeys(ks);

	keyDel (parentKey);
	ksDel(ks);
	PLUGIN_CLOSE ();

}

static void test_simpleCutRestoreOnSet () {
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	Key *parentKeyCopy = keyDup(parentKey);
	KeySet *conf = ksNew (20,
			keyNew ("system/cut", KEY_VALUE, "will/be/stripped", KEY_END), KS_END);
	PLUGIN_OPEN("rename");

	KeySet *ks = createSimpleTestKeys();
	ksAppendKey(ks, parentKey);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) >= 1,
			"call to kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	/* test that the keys have been correctly restored */
	KeySet *expected = createSimpleTestKeys();

	/* the parent key is restored from user/tests/rename/will/be/stripped
	 * and therefore will have its key value
	 */
	keySetString (parentKeyCopy, "value3");
	ksAppendKey (expected, parentKeyCopy);

	compareKeySets (ks, expected);
	ksDel(expected);
	ksDel(ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	keyDel(parentKey);
	PLUGIN_CLOSE ();
}

static void test_withoutConfig()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	Key *parentKeyCopy = keyDup(parentKey);
	KeySet *conf = ksNew (0, KS_END);
	PLUGIN_OPEN("rename");

	KeySet *ks = createSimpleTestKeys();
	ksAppendKey(ks, parentKey);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	KeySet *expected = createSimpleTestKeys();
	ksAppendKey(expected, parentKeyCopy);

	compareKeySets(ks, expected);
	keyDel (parentKey);
	keyDel (parentKeyCopy);
	ksDel(expected);
	ksDel(ks);
	PLUGIN_CLOSE ();
}

static void test_metaConfigTakesPrecedence()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/cut", KEY_VALUE, "will/be", KEY_END), KS_END);
	PLUGIN_OPEN("rename");

	KeySet *ks = createSimpleMetaTestKeys();
	ksAppendKey(ks, parentKey);

	succeed_if(plugin->kdbGet (plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	succeed_if(output_error (parentKey), "error in kdbGet");
	succeed_if(output_warnings (parentKey), "warnings in kdbGet");

	/* the first two keys should have been renamed by their metadata */
	Key* key = ksLookupByName (ks, "user/tests/rename/key1", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly renamed");
	key = ksLookupByName (ks, "user/tests/rename/key2", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly renamed");

	/* the third key should have been renamed by the global config */
	key = ksLookupByName (ks, "user/tests/rename/stripped", KDB_O_NONE);
	succeed_if(key, "key3 was renamed but would replace the parent key");

	/* the fourth key was not renamed because the prefix did not match */
	key = ksLookupByName (ks, "user/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if(key, "key4 was renamed although its prefix did not match");

	keyDel (parentKey);
	ksDel(ks);
	PLUGIN_CLOSE ();
}

static void test_keyCutNamePart()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	Key *result = elektraKeyCreateNewName(parentKey, parentKey, "wont/cut/this", NULL, NULL, NULL);
	succeed_if (!result, "parentKey was modified although it should have been ignored");

	/* cutting works correctly without trailing slash */
	Key *testKey = keyNew ("user/tests/rename/will/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName(testKey, parentKey, "will/cut/this", NULL, NULL, NULL);
	succeed_if (result, "key1 was not cut")
	succeed_if (!strcmp(keyName(result), "user/tests/rename/key1"), "cutting key1 did not yield the expected result");
	keyDel(testKey);
	keyDel(result);

	/* cutting works correctly with trailing slash */
	testKey = keyNew ("user/tests/rename/will/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName(testKey, parentKey, "will/cut/this/", NULL, NULL, NULL);
	succeed_if (result, "key1 was not cut")
	succeed_if (!strcmp(keyName(result), "user/tests/rename/key1"), "cutting key1 did not yield the expected result");
	keyDel(testKey);
	keyDel(result);

	/* disallow leading slashes */
	testKey = keyNew ("user/tests/rename/wont/cut/this/key1", KEY_END);
	result = elektraKeyCreateNewName(testKey, parentKey, "/wont/cut/this", NULL, NULL, NULL);
	succeed_if (!result, "key was cut although it the cutpath contained a leading slash");
	keyDel(testKey);
	keyDel(parentKey);
}

static void test_rebaseOfNewKeys()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/cut", KEY_VALUE, "new/base", KEY_END), KS_END);
	PLUGIN_OPEN("rename");

// clang-format off
	KeySet *ks = ksNew(20,
			/* this key was seen by rename before and wont be changed */
			keyNew("user/tests/rename/key1",
					KEY_VALUE, "value1",
					KEY_META, ELEKTRA_ORIGINAL_NAME_META, "user/tests/rename/key1",
					KEY_END),
			/* this key was not seen by rename before and will be renamed */
			keyNew("user/tests/rename/key2",
					KEY_VALUE, "value2",
					KEY_END),
			KS_END);
// clang-format on

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) >= 1,
			"call to kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	Key* key = ksLookupByName (ks, "user/tests/rename/key1", KDB_O_NONE);
	succeed_if(key, "key1 was not found anymore, but it should not have been renamed");

	key = ksLookupByName (ks, "user/tests/rename/new/base/key2", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly renamed");

	keyDel (parentKey);
	ksDel(ks);
	PLUGIN_CLOSE ();
}

static void test_addNewBaseToParentKey()
{
	Key *parentKey = keyNew ("user/tests/rename", KEY_END);
	KeySet *conf = ksNew (20,
			keyNew ("system/cut", KEY_VALUE, "new/base", KEY_END), KS_END);

	PLUGIN_OPEN("rename");

	KeySet *ks = ksNew(0, KS_END);
	keyIncRef(parentKey);
	ksAppendKey (ks, parentKey);

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) >= 1,
			"call to kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	Key *key = ksLookupByName (ks, "user/tests/rename/new/base", 0);
	succeed_if (key, "new base was not correctly appended to parent key");

	ksDel(ks);
	keyDecRef(parentKey);
	keyDel(parentKey);
	PLUGIN_CLOSE ();

}

static void test_replaceString()
{
	Key *parentKey = keyNew("user/tests/rename", KEY_END);
	KeySet *conf = ksNew(20, 
			keyNew("system/cut", KEY_VALUE, "will/be/stripped", KEY_END),
			keyNew("system/replacewith", KEY_VALUE, "stripped/it/is", KEY_END),
			KS_END);
	
	KeySet *ks = createSimpleTestKeys();
	ksAppendKey(ks, parentKey);

	PLUGIN_OPEN("rename");

	succeed_if(plugin->kdbGet(plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	Key *key = ksLookupByName(ks, "user/tests/rename/stripped/it/is/key1", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/stripped/it/is/key2", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/will/not/be/stripped/key4", KDB_O_NONE);
	succeed_if(key, "key4 was not correctly rename");
	
	keyDel(parentKey);
	ksDel(ks);
	PLUGIN_CLOSE();

}

static void test_toUpper()
{
	Key *parentKey = keyNew("user/tests/rename", KEY_END);
	KeySet *conf = ksNew(20,
			keyNew("system/toupper", KEY_VALUE, "0", KEY_END),
			KS_END);
	KeySet *ks = createSimpleTestKeys();
	ksAppendKey(ks, parentKey);
	PLUGIN_OPEN("rename");
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	Key *key = ksLookupByName(ks, "user/tests/rename/WILL/BE/STRIPPED/KEY1", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/WILL/BE/STRIPPED/KEY2", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/WILL/NOT/BE/STRIPPED/KEY4", KDB_O_NONE);
	succeed_if(key, "key4 was not correctly rename");
	
	keyDel(parentKey);
	ksDel(ks);
	PLUGIN_CLOSE();
}

static void test_toLower()
{
	Key *parentKey = keyNew("user/tests/rename", KEY_END);
	KeySet *conf = ksNew(20,
			keyNew("system/tolower", KEY_VALUE, "0", KEY_END),
			KS_END);
	KeySet *ks = ksNew(20,
			keyNew("user/tests/rename/AM/I/LOWERCASE", KEY_VALUE, "val1", KEY_END),
			keyNew("user/tests/rename/I/HOPE/IM/LOWERCASE/TOO", KEY_VALUE, "val2", KEY_END),
			KS_END);
	ksAppendKey(ks, parentKey);
	PLUGIN_OPEN("rename");
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	Key *key = ksLookupByName(ks, "user/tests/rename/am/i/lowercase", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/i/hope/im/lowercase/too", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly rename");
	
	keyDel(parentKey);
	ksDel(ks);
	PLUGIN_CLOSE();
}

static void test_mixCase()
{
	Key *parentKey = keyNew("user/tests/rename", KEY_END);
	KeySet *conf = ksNew(20,
			keyNew("system/tolower", KEY_VALUE, "1", KEY_END),
			keyNew("system/toupper", KEY_VALUE, "4", KEY_END),
			KS_END);
	KeySet *ks = ksNew(20,
			keyNew("user/tests/rename/am/i/LOWERCASE", KEY_VALUE, "val1", KEY_END),
			keyNew("user/tests/rename/hopefullystilllower/upper/upper/upper/LOWERCASE", KEY_VALUE, "val2", KEY_END),
			KS_END);
	ksAppendKey(ks, parentKey);
	PLUGIN_OPEN("rename");
	succeed_if(plugin->kdbGet(plugin, ks, parentKey) >= 1,
			"call to kdbGet was not successful");
	Key *key = ksLookupByName(ks, "user/tests/rename/AM/I/lowercase", KDB_O_NONE);
	succeed_if(key, "key1 was not correctly rename");
	key = ksLookupByName(ks, "user/tests/rename/hopefullystilllower/UPPER/UPPER/UPPER/lowercase", KDB_O_NONE);
	succeed_if(key, "key2 was not correctly rename");
	
	keyDel(parentKey);
	ksDel(ks);
	PLUGIN_CLOSE();
}

int main(int argc, char** argv)
{
	printf ("RENAME       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_withoutConfig();
	test_simpleCutOnGet();
	test_simpleCutRestoreOnSet();
	test_metaCutOnGet();
	test_metaConfigTakesPrecedence();
	test_rebaseOfNewKeys();
	test_addNewBaseToParentKey();

	test_keyCutNamePart();
	test_toUpper();
	test_toLower();
	test_mixCase();
	test_replaceString();

	printf ("\ntest_rename RESULTS: %d test(s) done. %d error(s).\n", nbTest,
			nbError);

	return nbError;
}

