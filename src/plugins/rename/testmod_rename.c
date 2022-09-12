/**
 * @file
 *
 * @brief A plugin for renaming
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#include "rename.h"
#include <tests_plugin.h>


static ElektraKeyset * createSimpleTestKeys (void)
{
	return elektraKeysetNew (20, elektraKeyNew ("user:/tests/rename/will/be/stripped/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/rename/will/be/stripped/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/rename/will/be/stripped", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/rename/will/not/be/stripped/key4", ELEKTRA_KEY_VALUE, "value4", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static ElektraKeyset * createSimpleMetaTestKeys (void)
{
	// clang-format off
	return elektraKeysetNew (20,
			elektraKeyNew("user:/tests/rename/will/be/stripped/key1",
					ELEKTRA_KEY_VALUE, "value1",
					ELEKTRA_KEY_META, "rename/cut", "will/be/stripped",
					ELEKTRA_KEY_END),
			elektraKeyNew("user:/tests/rename/will/be/stripped/key2",
					ELEKTRA_KEY_VALUE, "value2",
					ELEKTRA_KEY_META, "rename/cut", "will/be/stripped",
					ELEKTRA_KEY_END),
			elektraKeyNew("user:/tests/rename/will/be/stripped",
					ELEKTRA_KEY_VALUE, "value3",
					ELEKTRA_KEY_END),
			elektraKeyNew("user:/tests/rename/will/not/be/stripped/key4",
					ELEKTRA_KEY_VALUE, "value4",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on
}

static void checkSimpleTestKeys (ElektraKeyset * ks)
{
	/* the first two keys should have been renamed */
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/key1", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly renamed");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/key2", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");
	/* the fourth key was not renamed because the prefix did not match */
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key4 was renamed although its prefix did not match");
}

static void compareKeySets (ElektraKeyset * ks, ElektraKeyset * expected)
{
	succeed_if (elektraKeysetGetSize (expected) == elektraKeysetGetSize (ks), "KeySet on set does not contain the same amount of keys");
	ElektraKey * current;
	elektraKeysetRewind (expected);
	while ((current = elektraKeysetNext (expected)))
	{
		ElektraKey * key = elektraKeysetLookup (ks, current, ELEKTRA_KDB_O_NONE);
		succeed_if (key, "Expected key was not found in KeySet");
		succeed_if (!strcmp (elektraKeyString (key), elektraKeyString (current)), "Value of key was modified");
	}
}

static void test_simpleCutOnGet (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "will/be/stripped", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = createSimpleTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");


	checkSimpleTestKeys (ks);
	elektraKeysetDel (ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_metaCutOnGet (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = createSimpleMetaTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	checkSimpleTestKeys (ks);

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_simpleCutRestoreOnSet (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKey * parentKeyCopy = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "will/be/stripped", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = createSimpleTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	/* test that the keys have been correctly restored */
	ElektraKeyset * expected = createSimpleTestKeys ();

	/* the parent key is restored from user:/tests/rename/will/be/stripped
	 * and therefore will have its key value
	 */
	elektraKeySetString (parentKeyCopy, "value3");
	elektraKeysetAppendKey (expected, parentKeyCopy);

	compareKeySets (ks, expected);
	elektraKeysetDel (expected);
	elektraKeysetDel (ks);

	/*
	 * this has to be done because the parentKey is not
	 * part of ks anymore due to renaming
	 */
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_withoutConfig (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKey * parentKeyCopy = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = createSimpleTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKeyset * expected = createSimpleTestKeys ();
	elektraKeysetAppendKey (expected, parentKeyCopy);

	compareKeySets (ks, expected);
	elektraKeysetDel (expected);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_metaConfigTakesPrecedence (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "will/be", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = createSimpleMetaTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* the first two keys should have been renamed by their metadata */
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/key1", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly renamed");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/key2", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");

	/* the third key should have been renamed by the global config */
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/stripped", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key3 was renamed but would replace the parent key");

	/* the fourth key was not renamed because the prefix did not match */
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key4 was renamed although its prefix did not match");

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_keyCutNamePart (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKey * result = elektraKeyCreateNewName (parentKey, parentKey, "wont/cut/this", NULL, NULL, NULL, 0);
	succeed_if (!result, "parentKey was modified although it should have been ignored");

	/* cutting works correctly without trailing slash */
	ElektraKey * testKey = elektraKeyNew ("user:/tests/rename/will/cut/this/key1", ELEKTRA_KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "will/cut/this", NULL, NULL, NULL, 0);
	succeed_if (result, "key1 was not cut")
		succeed_if (!strcmp (elektraKeyName (result), "user:/tests/rename/key1"), "cutting key1 did not yield the expected result");
	elektraKeyDel (testKey);
	elektraKeyDel (result);

	/* cutting works correctly with trailing slash */
	testKey = elektraKeyNew ("user:/tests/rename/will/cut/this/key1", ELEKTRA_KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "will/cut/this/", NULL, NULL, NULL, 0);
	succeed_if (result, "key1 was not cut")
		succeed_if (!strcmp (elektraKeyName (result), "user:/tests/rename/key1"), "cutting key1 did not yield the expected result");
	elektraKeyDel (testKey);
	elektraKeyDel (result);

	/* disallow leading slashes */
	testKey = elektraKeyNew ("user:/tests/rename/wont/cut/this/key1", ELEKTRA_KEY_END);
	result = elektraKeyCreateNewName (testKey, parentKey, "/wont/cut/this", NULL, NULL, NULL, 0);
	succeed_if (!result, "key was cut although it the cutpath contained a leading slash");
	elektraKeyDel (testKey);
	elektraKeyDel (parentKey);
}

static void test_rebaseOfNewKeys (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "new/base", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("rename");

	// clang-format off
	ElektraKeyset *ks = elektraKeysetNew(20,
			/* this key was seen by rename before and wont be changed */
			elektraKeyNew("user:/tests/rename/key1",
					ELEKTRA_KEY_VALUE, "value1",
					ELEKTRA_KEY_META, ELEKTRA_ORIGINAL_NAME_META, "user:/tests/rename/key1",
					ELEKTRA_KEY_END),
			/* this key was not seen by rename before and will be renamed */
			elektraKeyNew("user:/tests/rename/key2",
					ELEKTRA_KEY_VALUE, "value2",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
	// clang-format on

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/key1", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not found anymore, but it should not have been renamed");

	key = elektraKeysetLookupByName (ks, "user:/tests/rename/new/base/key2", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly renamed");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_addNewBaseToParentKey (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "new/base", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	PLUGIN_OPEN ("rename");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeyIncRef (parentKey);
	elektraKeysetAppendKey (ks, parentKey);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/new/base", 0);
	succeed_if (key, "new base was not correctly appended to parent key");

	elektraKeysetDel (ks);
	elektraKeyDecRef (parentKey);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_replaceString (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "will/be/stripped", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/replacewith", ELEKTRA_KEY_VALUE, "stripped/it/is", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ks = createSimpleTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);

	PLUGIN_OPEN ("rename");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/stripped/it/is/key1", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/stripped/it/is/key2", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/will/not/be/stripped/key4", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key4 was not correctly rename");

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_toUpper (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/toupper", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = createSimpleTestKeys ();
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/WILL/BE/STRIPPED/KEY1", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/WILL/BE/STRIPPED/KEY2", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/WILL/NOT/BE/STRIPPED/KEY4", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key4 was not correctly rename");

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_toLower (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/tolower", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("user:/tests/rename/AM/I/LOWERCASE", ELEKTRA_KEY_VALUE, "val1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/rename/I/HOPE/IM/LOWERCASE/TOO", ELEKTRA_KEY_VALUE, "val2", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/am/i/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/i/hope/im/lowercase/too", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mixCase (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/tolower", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("system:/toupper", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END),
			       ELEKTRA_KS_END);
	ElektraKeyset * ks =
		elektraKeysetNew (20, elektraKeyNew ("user:/tests/rename/am/i/LOWERCASE", ELEKTRA_KEY_VALUE, "val1", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/tests/rename/hopefullystilllower/upper/upper/upper/LOWERCASE", ELEKTRA_KEY_VALUE, "val2", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/AM/I/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/hopefullystilllower/UPPER/UPPER/UPPER/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key2 was not correctly rename");

	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_write (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("system:/tolower", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("system:/get/case", ELEKTRA_KEY_VALUE, "toupper", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/set/case", ELEKTRA_KEY_VALUE, "keyname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("user:/tests/rename/uppercase/uppercase/uppercase/LOWERCASE", ELEKTRA_KEY_VALUE, "test", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1s name was not correctly saved");
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_write2 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/rename", ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("system:/tolower", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("system:/get/case", ELEKTRA_KEY_VALUE, "tolower", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/set/case", ELEKTRA_KEY_VALUE, "toupper", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (20, elektraKeyNew ("user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/LOWERCASE", ELEKTRA_KEY_VALUE, "test", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("rename");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/rename/uppercase/uppercase/uppercase/lowercase", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1 was not correctly rename");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	key = elektraKeysetLookupByName (ks, "user:/tests/rename/UPPERCASE/UPPERCASE/UPPERCASE/LOWERCASE", ELEKTRA_KDB_O_NONE);
	succeed_if (key, "key1s name was not correctly saved");
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
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
