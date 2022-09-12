/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
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

#include <tests_plugin.h>


#include <stdio.h>

static ElektraKey * createMergingKey (int i)
{
	char * name;
	char * value;
	char * order;
	if (asprintf (&name, "user:/convertkey%d", i) == -1 || asprintf (&value, "meta line %d", i) == -1 ||
	    asprintf (&order, "%i", i) == -1)
	{
		fprintf (stderr, "Unable to create key attributes");
		exit (EXIT_FAILURE);
	}
	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "order", order, ELEKTRA_KEY_END);
	elektraFree (name);
	elektraFree (value);
	elektraFree (order);
	return key;
}

// clang-format off
static ElektraKeyset* createSimpleTestKeys(void)
{
	/* the keys to be converted are simply appended to the next
	 * or the previous key.
	 */
	return elektraKeysetNew (20,
			elektraKeyNew ("user:/normalkey1",
					ELEKTRA_KEY_META, "order", "10", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey1",
					ELEKTRA_KEY_VALUE, "testvalue1",
					ELEKTRA_KEY_META, "order", "20",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey2",
					ELEKTRA_KEY_META, "order", "30",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey3",
					ELEKTRA_KEY_META, "order", "40",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey2",
					ELEKTRA_KEY_VALUE, "testvalue2",
					ELEKTRA_KEY_META, "order", "50",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "previous",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey1/subkey",
					ELEKTRA_KEY_VALUE, "testvalue3",
					ELEKTRA_KEY_META, "order", "60",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "parent",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}

static ElektraKeyset* createMergeTestkeys(void)
{
	/* the keys to be converted are merged together
	 * into a single metadata
	 */
	ElektraKeyset* ks = elektraKeysetNew(0, ELEKTRA_KS_END);
	for (int i = 1; i <= 3; i++)
	{
		ElektraKey* key = createMergingKey (i);
		elektraKeySetMeta (key, "convert/metaname", "testmeta");
		elektraKeySetMeta (key, "convert/append", "next");
		elektraKeysetAppendKey (ks, key);
	}
	elektraKeysetAppendKey (ks,
			elektraKeyNew ("user:/normalkey1", ELEKTRA_KEY_META, "order", "10", ELEKTRA_KEY_END));
	elektraKeysetAppendKey (ks,
			elektraKeyNew ("user:/normalkey2", ELEKTRA_KEY_META, "order", "20", ELEKTRA_KEY_END));
	for (int i = 30; i <= 32; i++)
	{
		ElektraKey* key = createMergingKey (i);
		elektraKeySetMeta (key, "convert/metaname", "testmeta");
		elektraKeySetMeta (key, "convert/append", "previous");
		elektraKeysetAppendKey (ks, key);
	}
	return ks;
}

static ElektraKeyset* createSkipMergeTestKeys(void)
{
	/* the keys to be converted are interweaved with keys
	 * of the other directio
	 */
	return elektraKeysetNew (20,
			elektraKeyNew ("user:/normalkey1",
					ELEKTRA_KEY_META, "order", "10", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey1",
					ELEKTRA_KEY_VALUE, "meta line1",
					ELEKTRA_KEY_META, "order", "20",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "previous",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey2",
					ELEKTRA_KEY_VALUE, "meta line2",
					ELEKTRA_KEY_META, "order", "30",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey3",
					ELEKTRA_KEY_VALUE, "meta line3",
					ELEKTRA_KEY_META, "order", "40",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "previous",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey4",
					ELEKTRA_KEY_VALUE, "meta line4",
					ELEKTRA_KEY_META, "order", "50",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey2",
					ELEKTRA_KEY_META, "order", "60",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}

static ElektraKeyset *createParentTestKeys(void)
{

	/* all keys to be converted are appended to the
	 * parent key of the keyset for any of the possible reasons
	 */
	return elektraKeysetNew (20,
			elektraKeyNew ("user:/parentkey1",
					ELEKTRA_KEY_META, "order", "10", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey1/convertkeydirect",
					ELEKTRA_KEY_VALUE, "testvalue1",
					ELEKTRA_KEY_META, "order", "20",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "parent",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey2",
					ELEKTRA_KEY_META, "order", "30", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey2/subparent/convertkeyhole",
					ELEKTRA_KEY_VALUE, "testvalue2",
					ELEKTRA_KEY_META, "order", "40",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "parent",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey3",
					ELEKTRA_KEY_META, "order", "50",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey1",
					ELEKTRA_KEY_META, "order", "60",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey3/convertkeyprev",
					ELEKTRA_KEY_VALUE, "testvalue3",
					ELEKTRA_KEY_META, "order", "70",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "previous",
					ELEKTRA_KEY_META, "convert/append/samelevel", "",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey4",
					ELEKTRA_KEY_META, "order", "80",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/parentkey4/convertkeynext",
					ELEKTRA_KEY_VALUE, "testvalue4",
					ELEKTRA_KEY_META, "order", "90",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_META, "convert/append/samelevel", "",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey2",
					ELEKTRA_KEY_META, "order", "100",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}

static ElektraKeyset* createDifferentMetaNameTestKeys(void)
{
	return elektraKeysetNew (20,
			elektraKeyNew ("user:/convertkey1",
					ELEKTRA_KEY_VALUE, "meta line1",
					ELEKTRA_KEY_META, "order", "10",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta1",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/convertkey2",
					ELEKTRA_KEY_VALUE, "meta line2",
					ELEKTRA_KEY_META, "order", "20",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta2",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/normalkey1",
					ELEKTRA_KEY_META, "order", "30",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}

static ElektraKeyset* createSameLevelTestKeys(void)
{
	return elektraKeysetNew (20,
			elektraKeyNew ("user:/levelkey1",
					ELEKTRA_KEY_META, "order", "10",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/levelkey1/convertkey1",
					ELEKTRA_KEY_VALUE, "convertkey1value",
					ELEKTRA_KEY_META, "order","20",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_META, "convert/append/samelevel", "",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/levelkey1/childkey1",
					ELEKTRA_KEY_META, "order", "30",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/levelkey1/convertkey2",
					ELEKTRA_KEY_VALUE, "convertkey2value",
					ELEKTRA_KEY_META, "order", "40",
					ELEKTRA_KEY_META, "convert/metaname", "testmeta",
					ELEKTRA_KEY_META, "convert/append", "next",
					ELEKTRA_KEY_META, "convert/append/samelevel", "",
					ELEKTRA_KEY_END),
			elektraKeyNew ("user:/levelkey2",
					ELEKTRA_KEY_META, "order", "50",
					ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
}
// clang-format on

void test_parentAppendMode (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createParentTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* parentkey1 must contain meta information generated from convertkeydirect (via parent) */
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/parentkey1", 0);
	succeed_if (key, "parentkey1 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "parentkey1 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey1), "testvalue1"), "metakey of parentkey1 contained incorrect data");

	/* parentkey2 must contain meta information generated from convertkeyhole (via parent) */
	key = elektraKeysetLookupByName (ks, "user:/parentkey2", 0);
	succeed_if (key, "parentkey2 was removed");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "parentkey2 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey2), "testvalue2"), "metakey of parentkey2 contained incorrect data");

	/* parentkey3 must contain meta information generated from convertkeyprev
	 * (via previous append samelevel which falls back to parent) */
	key = elektraKeysetLookupByName (ks, "user:/parentkey3", 0);
	succeed_if (key, "parentkey3 was removed");

	const ElektraKey * metaKey3 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey3, "parentkey3 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey3), "testvalue3"), "metakey of parentkey3 contained incorrect data");

	/* normalkey1 must not contain metadata */
	key = elektraKeysetLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");
	succeed_if (!elektraKeyGetMeta (key, "testmeta"), "normalkey1 should not contain any metadata");

	/* parentkey4 must contain meta information generated from convertkeynext
	 * (via next append samelevel which falls back to parent) */
	key = elektraKeysetLookupByName (ks, "user:/parentkey4", 0);
	succeed_if (key, "parentkey4 was removed");

	const ElektraKey * metaKey4 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey4, "parentkey4 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey4), "testvalue4"), "metakey of parentkey4 contained incorrect data");

	/* normalkey2 must not contain metadata */
	key = elektraKeysetLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");
	succeed_if (!elektraKeyGetMeta (key, "testmeta"), "normalkey2 should not contain any metadata");


	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}


void test_simpleAppendModes (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");


	ElektraKeyset * ks = createSimpleTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* converted keys must be removed from the result */
	succeed_if (!elektraKeysetLookupByName (ks, "user:/convertkey1", 0), "convertkey1 was not converted");
	succeed_if (!elektraKeysetLookupByName (ks, "user:/convertkey2", 0), "convertkey2 was not converted");

	/* normalkey2 must contain meta information generated from convertkey1 (via next) */
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey2 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey1), "testvalue1"), "metakey of normalkey2 contained incorrect data");

	/* normalkey3 must contain meta information generated from convertkey2 (via previous) */
	key = elektraKeysetLookupByName (ks, "user:/normalkey3", 0);
	succeed_if (key, "normalkey3 was removed");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey3 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey2), "testvalue2"), "metakey of normalkey3 contained incorrect data");

	/* normalkey1 must contain meta information generated from subkey (via parent) */
	key = elektraKeysetLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const ElektraKey * metaKey3 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey3, "normalkey1 contained no metakey");
	succeed_if (!strcmp (elektraKeyString (metaKey3), "testvalue3"), "metakey of normalkey1 contained incorrect data");


	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}


void test_metaMerging (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createMergeTestkeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey1 contained no metakey");
	const char * expected1 = "meta line 1\nmeta line 2\nmeta line 3";
	succeed_if (!strcmp (elektraKeyString (metaKey1), expected1), "metakey of normalkey1 contained incorrect data");

	key = elektraKeysetLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey2 contained no metakey");
	const char * expected2 = "meta line 30\nmeta line 31\nmeta line 32";
	succeed_if (!strcmp (elektraKeyString (metaKey2), expected2), "metakey of normalkey2 contained incorrect data");

	/* change the value of the middle key */
	elektraKeySetMeta (key, "testmeta", "meta line 30\nchanged meta line\nmeta line 32");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	key = elektraKeysetLookupByName (ks, "user:/convertkey30", 0);
	succeed_if (key, "convertkey30 was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "meta line 30"), "value of convertkey30 was modified");

	key = elektraKeysetLookupByName (ks, "user:/convertkey31", 0);
	succeed_if (key, "convertkey31 was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "changed meta line"), "meta information was not written back to convertkey31");

	key = elektraKeysetLookupByName (ks, "user:/convertkey32", 0);
	succeed_if (key, "convertkey32 was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "meta line 32"), "value of convertkey32 was modified");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

void test_metaSkipMerge (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createSkipMergeTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey1 contained no metakey");
	const char * expected1 = "meta line1\nmeta line3";
	succeed_if (!strcmp (elektraKeyString (metaKey1), expected1), "metakey of normalkey1 contained incorrect data");

	key = elektraKeysetLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey2 contained no metakey");
	const char * expected2 = "meta line2\nmeta line4";
	succeed_if (!strcmp (elektraKeyString (metaKey2), expected2), "metakey of normalkey2 contained incorrect data");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

void test_differentMetaNames (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createDifferentMetaNameTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (key, "testmeta1");
	succeed_if (metaKey1, "normalkey1 contained no meta testmeta1");
	const char * expected1 = "meta line1";
	succeed_if (!strcmp (elektraKeyString (metaKey1), expected1), "metakey testmeta1 of normalkey1 contained incorrect data");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (key, "testmeta2");
	succeed_if (metaKey2, "normalkey1 contained no meta testmeta1");
	const char * expected2 = "meta line2";
	succeed_if (!strcmp (elektraKeyString (metaKey2), expected2), "metakey testmeta1 of normalkey1 contained incorrect data");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

void test_appendSameLevel (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createSameLevelTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* convertkey1 should be converted to childkey1 as childkey1 is on the same level as requested */
	ElektraKey * childKey = elektraKeysetLookupByName (ks, "user:/levelkey1/childkey1", 0);
	succeed_if (childKey, "childkey1 was removed");

	const ElektraKey * metaKey1 = elektraKeyGetMeta (childKey, "testmeta");
	succeed_if (metaKey1, "childkey1 contained no meta testmeta");
	const char * expected1 = "convertkey1value";
	succeed_if (!strcmp (elektraKeyString (metaKey1), expected1), "metakey testmeta of childkey1 contained incorrect data");

	/* convertkey2 should be converted to levelkey as the next key in order is not on the same level */
	ElektraKey * levelkey1 = elektraKeysetLookupByName (ks, "user:/levelkey1", 0);
	succeed_if (levelkey1, "levelkey1 was removed");

	const ElektraKey * metaKey2 = elektraKeyGetMeta (levelkey1, "testmeta");
	succeed_if (metaKey2, "levelkey1 contained no meta testmeta");
	const char * expected2 = "convertkey2value";
	succeed_if (!strcmp (elektraKeyString (metaKey2), expected2), "metakey testmeta of levelkey1 contained incorrect data");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

void test_restoreOnSet (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/keytometa", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("keytometa");

	ElektraKeyset * ks = createSimpleTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	/* change the meta information stored in normalkey2
	 * (was generated from convertkey1)
	 */
	elektraKeySetMeta (key, "testmeta", "changedtestvalue");

	/* do not touch the meta information of normalkey3
	 * (was generated from convertkey2)
	 */

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	key = elektraKeysetLookupByName (ks, "user:/convertkey1", 0);
	succeed_if (key, "convertkey1 was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "changedtestvalue"), "meta information was not written back to convertkey1");

	key = elektraKeysetLookupByName (ks, "user:/convertkey2", 0);
	succeed_if (key, "convertkey2 was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "testvalue2"), " value of convertkey2 was modified");

	key = elektraKeysetLookupByName (ks, "user:/normalkey1/subkey", 0);
	succeed_if (key, "subkey was not restored");
	succeed_if (!strcmp (elektraKeyString (key), "testvalue3"), " value of subkey was modified");


	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("KEYTOMETA       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_simpleAppendModes ();
	test_parentAppendMode ();
	test_metaMerging ();
	test_metaSkipMerge ();
	test_differentMetaNames ();
	test_appendSameLevel ();
	test_restoreOnSet ();

	print_result ("test_keytometa");

	return nbError;
}
