/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
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

static Key * createMergingKey (int i)
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
	Key * key = keyNew (name, KEY_VALUE, value, KEY_META, "order", order, KEY_END);
	elektraFree (name);
	elektraFree (value);
	elektraFree (order);
	return key;
}

// clang-format off
static KeySet* createSimpleTestKeys(void)
{
	/* the keys to be converted are simply appended to the next
	 * or the previous key.
	 */
	return ksNew (20,
			keyNew ("user:/normalkey1",
					KEY_META, "order", "10", KEY_END),
			keyNew ("user:/convertkey1",
					KEY_VALUE, "testvalue1",
					KEY_META, "order", "20",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_END),
			keyNew ("user:/normalkey2",
					KEY_META, "order", "30",
					KEY_END),
			keyNew ("user:/normalkey3",
					KEY_META, "order", "40",
					KEY_END),
			keyNew ("user:/convertkey2",
					KEY_VALUE, "testvalue2",
					KEY_META, "order", "50",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "previous",
					KEY_END),
			keyNew ("user:/normalkey1/subkey",
					KEY_VALUE, "testvalue3",
					KEY_META, "order", "60",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "parent",
					KEY_END),
			KS_END);
}

static KeySet* createMergeTestkeys(void)
{
	/* the keys to be converted are merged together
	 * into a single metadata
	 */
	KeySet* ks = ksNew(0, KS_END);
	for (int i = 1; i <= 3; i++)
	{
		Key* key = createMergingKey (i);
		keySetMeta (key, "convert/metaname", "testmeta");
		keySetMeta (key, "convert/append", "next");
		ksAppendKey (ks, key);
	}
	ksAppendKey (ks,
			keyNew ("user:/normalkey1", KEY_META, "order", "10", KEY_END));
	ksAppendKey (ks,
			keyNew ("user:/normalkey2", KEY_META, "order", "20", KEY_END));
	for (int i = 30; i <= 32; i++)
	{
		Key* key = createMergingKey (i);
		keySetMeta (key, "convert/metaname", "testmeta");
		keySetMeta (key, "convert/append", "previous");
		ksAppendKey (ks, key);
	}
	return ks;
}

static KeySet* createSkipMergeTestKeys(void)
{
	/* the keys to be converted are interweaved with keys
	 * of the other directio
	 */
	return ksNew (20,
			keyNew ("user:/normalkey1",
					KEY_META, "order", "10", KEY_END),
			keyNew ("user:/convertkey1",
					KEY_VALUE, "meta line1",
					KEY_META, "order", "20",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "previous",
					KEY_END),
			keyNew ("user:/convertkey2",
					KEY_VALUE, "meta line2",
					KEY_META, "order", "30",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_END),
			keyNew ("user:/convertkey3",
					KEY_VALUE, "meta line3",
					KEY_META, "order", "40",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "previous",
					KEY_END),
			keyNew ("user:/convertkey4",
					KEY_VALUE, "meta line4",
					KEY_META, "order", "50",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_END),
			keyNew ("user:/normalkey2",
					KEY_META, "order", "60",
					KEY_END),
			KS_END);
}

static KeySet *createParentTestKeys(void)
{

	/* all keys to be converted are appended to the
	 * parent key of the keyset for any of the possible reasons
	 */
	return ksNew (20,
			keyNew ("user:/parentkey1",
					KEY_META, "order", "10", KEY_END),
			keyNew ("user:/parentkey1/convertkeydirect",
					KEY_VALUE, "testvalue1",
					KEY_META, "order", "20",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "parent",
					KEY_END),
			keyNew ("user:/parentkey2",
					KEY_META, "order", "30", KEY_END),
			keyNew ("user:/parentkey2/subparent/convertkeyhole",
					KEY_VALUE, "testvalue2",
					KEY_META, "order", "40",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "parent",
					KEY_END),
			keyNew ("user:/parentkey3",
					KEY_META, "order", "50",
					KEY_END),
			keyNew ("user:/normalkey1",
					KEY_META, "order", "60",
					KEY_END),
			keyNew ("user:/parentkey3/convertkeyprev",
					KEY_VALUE, "testvalue3",
					KEY_META, "order", "70",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "previous",
					KEY_META, "convert/append/samelevel", "",
					KEY_END),
			keyNew ("user:/parentkey4",
					KEY_META, "order", "80",
					KEY_END),
			keyNew ("user:/parentkey4/convertkeynext",
					KEY_VALUE, "testvalue4",
					KEY_META, "order", "90",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_META, "convert/append/samelevel", "",
					KEY_END),
			keyNew ("user:/normalkey2",
					KEY_META, "order", "100",
					KEY_END),
			KS_END);
}

static KeySet* createDifferentMetaNameTestKeys(void)
{
	return ksNew (20,
			keyNew ("user:/convertkey1",
					KEY_VALUE, "meta line1",
					KEY_META, "order", "10",
					KEY_META, "convert/metaname", "testmeta1",
					KEY_META, "convert/append", "next",
					KEY_END),
			keyNew ("user:/convertkey2",
					KEY_VALUE, "meta line2",
					KEY_META, "order", "20",
					KEY_META, "convert/metaname", "testmeta2",
					KEY_META, "convert/append", "next",
					KEY_END),
			keyNew ("user:/normalkey1",
					KEY_META, "order", "30",
					KEY_END),
			KS_END);
}

static KeySet* createSameLevelTestKeys(void)
{
	return ksNew (20,
			keyNew ("user:/levelkey1",
					KEY_META, "order", "10",
					KEY_END),
			keyNew ("user:/levelkey1/convertkey1",
					KEY_VALUE, "convertkey1value",
					KEY_META, "order","20",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_META, "convert/append/samelevel", "",
					KEY_END),
			keyNew ("user:/levelkey1/childkey1",
					KEY_META, "order", "30",
					KEY_END),
			keyNew ("user:/levelkey1/convertkey2",
					KEY_VALUE, "convertkey2value",
					KEY_META, "order", "40",
					KEY_META, "convert/metaname", "testmeta",
					KEY_META, "convert/append", "next",
					KEY_META, "convert/append/samelevel", "",
					KEY_END),
			keyNew ("user:/levelkey2",
					KEY_META, "order", "50",
					KEY_END),
			KS_END);
}
// clang-format on

void test_parentAppendMode (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createParentTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* parentkey1 must contain meta information generated from convertkeydirect (via parent) */
	Key * key = ksLookupByName (ks, "user:/parentkey1", 0);
	succeed_if (key, "parentkey1 was removed");

	const Key * metaKey1 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "parentkey1 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey1), "testvalue1"), "metakey of parentkey1 contained incorrect data");

	/* parentkey2 must contain meta information generated from convertkeyhole (via parent) */
	key = ksLookupByName (ks, "user:/parentkey2", 0);
	succeed_if (key, "parentkey2 was removed");

	const Key * metaKey2 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "parentkey2 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey2), "testvalue2"), "metakey of parentkey2 contained incorrect data");

	/* parentkey3 must contain meta information generated from convertkeyprev
	 * (via previous append samelevel which falls back to parent) */
	key = ksLookupByName (ks, "user:/parentkey3", 0);
	succeed_if (key, "parentkey3 was removed");

	const Key * metaKey3 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey3, "parentkey3 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey3), "testvalue3"), "metakey of parentkey3 contained incorrect data");

	/* normalkey1 must not contain metadata */
	key = ksLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");
	succeed_if (!keyGetMeta (key, "testmeta"), "normalkey1 should not contain any metadata");

	/* parentkey4 must contain meta information generated from convertkeynext
	 * (via next append samelevel which falls back to parent) */
	key = ksLookupByName (ks, "user:/parentkey4", 0);
	succeed_if (key, "parentkey4 was removed");

	const Key * metaKey4 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey4, "parentkey4 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey4), "testvalue4"), "metakey of parentkey4 contained incorrect data");

	/* normalkey2 must not contain metadata */
	key = ksLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");
	succeed_if (!keyGetMeta (key, "testmeta"), "normalkey2 should not contain any metadata");


	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


void test_simpleAppendModes (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");


	KeySet * ks = createSimpleTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* converted keys must be removed from the result */
	succeed_if (!ksLookupByName (ks, "user:/convertkey1", 0), "convertkey1 was not converted");
	succeed_if (!ksLookupByName (ks, "user:/convertkey2", 0), "convertkey2 was not converted");

	/* normalkey2 must contain meta information generated from convertkey1 (via next) */
	Key * key = ksLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const Key * metaKey1 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey2 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey1), "testvalue1"), "metakey of normalkey2 contained incorrect data");

	/* normalkey3 must contain meta information generated from convertkey2 (via previous) */
	key = ksLookupByName (ks, "user:/normalkey3", 0);
	succeed_if (key, "normalkey3 was removed");

	const Key * metaKey2 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey3 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey2), "testvalue2"), "metakey of normalkey3 contained incorrect data");

	/* normalkey1 must contain meta information generated from subkey (via parent) */
	key = ksLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const Key * metaKey3 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey3, "normalkey1 contained no metakey");
	succeed_if (!strcmp (keyString (metaKey3), "testvalue3"), "metakey of normalkey1 contained incorrect data");


	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


void test_metaMerging (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createMergeTestkeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const Key * metaKey1 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey1 contained no metakey");
	const char * expected1 = "meta line 1\nmeta line 2\nmeta line 3";
	succeed_if (!strcmp (keyString (metaKey1), expected1), "metakey of normalkey1 contained incorrect data");

	key = ksLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const Key * metaKey2 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey2 contained no metakey");
	const char * expected2 = "meta line 30\nmeta line 31\nmeta line 32";
	succeed_if (!strcmp (keyString (metaKey2), expected2), "metakey of normalkey2 contained incorrect data");

	/* change the value of the middle key */
	keySetMeta (key, "testmeta", "meta line 30\nchanged meta line\nmeta line 32");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	key = ksLookupByName (ks, "user:/convertkey30", 0);
	succeed_if (key, "convertkey30 was not restored");
	succeed_if (!strcmp (keyString (key), "meta line 30"), "value of convertkey30 was modified");

	key = ksLookupByName (ks, "user:/convertkey31", 0);
	succeed_if (key, "convertkey31 was not restored");
	succeed_if (!strcmp (keyString (key), "changed meta line"), "meta information was not written back to convertkey31");

	key = ksLookupByName (ks, "user:/convertkey32", 0);
	succeed_if (key, "convertkey32 was not restored");
	succeed_if (!strcmp (keyString (key), "meta line 32"), "value of convertkey32 was modified");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

void test_metaSkipMerge (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createSkipMergeTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const Key * metaKey1 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey1, "normalkey1 contained no metakey");
	const char * expected1 = "meta line1\nmeta line3";
	succeed_if (!strcmp (keyString (metaKey1), expected1), "metakey of normalkey1 contained incorrect data");

	key = ksLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	const Key * metaKey2 = keyGetMeta (key, "testmeta");
	succeed_if (metaKey2, "normalkey2 contained no metakey");
	const char * expected2 = "meta line2\nmeta line4";
	succeed_if (!strcmp (keyString (metaKey2), expected2), "metakey of normalkey2 contained incorrect data");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

void test_differentMetaNames (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createDifferentMetaNameTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user:/normalkey1", 0);
	succeed_if (key, "normalkey1 was removed");

	const Key * metaKey1 = keyGetMeta (key, "testmeta1");
	succeed_if (metaKey1, "normalkey1 contained no meta testmeta1");
	const char * expected1 = "meta line1";
	succeed_if (!strcmp (keyString (metaKey1), expected1), "metakey testmeta1 of normalkey1 contained incorrect data");

	const Key * metaKey2 = keyGetMeta (key, "testmeta2");
	succeed_if (metaKey2, "normalkey1 contained no meta testmeta1");
	const char * expected2 = "meta line2";
	succeed_if (!strcmp (keyString (metaKey2), expected2), "metakey testmeta1 of normalkey1 contained incorrect data");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

void test_appendSameLevel (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createSameLevelTestKeys ();
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	/* convertkey1 should be converted to childkey1 as childkey1 is on the same level as requested */
	Key * childKey = ksLookupByName (ks, "user:/levelkey1/childkey1", 0);
	succeed_if (childKey, "childkey1 was removed");

	const Key * metaKey1 = keyGetMeta (childKey, "testmeta");
	succeed_if (metaKey1, "childkey1 contained no meta testmeta");
	const char * expected1 = "convertkey1value";
	succeed_if (!strcmp (keyString (metaKey1), expected1), "metakey testmeta of childkey1 contained incorrect data");

	/* convertkey2 should be converted to levelkey as the next key in order is not on the same level */
	Key * levelkey1 = ksLookupByName (ks, "user:/levelkey1", 0);
	succeed_if (levelkey1, "levelkey1 was removed");

	const Key * metaKey2 = keyGetMeta (levelkey1, "testmeta");
	succeed_if (metaKey2, "levelkey1 contained no meta testmeta");
	const char * expected2 = "convertkey2value";
	succeed_if (!strcmp (keyString (metaKey2), expected2), "metakey testmeta of levelkey1 contained incorrect data");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

void test_restoreOnSet (void)
{
	Key * parentKey = keyNew ("user:/tests/keytometa", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("keytometa");

	KeySet * ks = createSimpleTestKeys ();

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");

	Key * key = ksLookupByName (ks, "user:/normalkey2", 0);
	succeed_if (key, "normalkey2 was removed");

	/* change the meta information stored in normalkey2
	 * (was generated from convertkey1)
	 */
	keySetMeta (key, "testmeta", "changedtestvalue");

	/* do not touch the meta information of normalkey3
	 * (was generated from convertkey2)
	 */

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	key = ksLookupByName (ks, "user:/convertkey1", 0);
	succeed_if (key, "convertkey1 was not restored");
	succeed_if (!strcmp (keyString (key), "changedtestvalue"), "meta information was not written back to convertkey1");

	key = ksLookupByName (ks, "user:/convertkey2", 0);
	succeed_if (key, "convertkey2 was not restored");
	succeed_if (!strcmp (keyString (key), "testvalue2"), " value of convertkey2 was modified");

	key = ksLookupByName (ks, "user:/normalkey1/subkey", 0);
	succeed_if (key, "subkey was not restored");
	succeed_if (!strcmp (keyString (key), "testvalue3"), " value of subkey was modified");


	keyDel (parentKey);
	ksDel (ks);
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
