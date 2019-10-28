/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <assert.h>
#include <kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"

#define PREFIX "user/tests/toml"

static void testReadCompare (const char * filename, KeySet * expected);
static void testReadCompareError (const char * filename, KeySet * expected);
static void testCompareMetakey (Key * expected, Key * found, const char * metaKeyName);
static void testCompareErrors (Key * expected, Key * found);
static void printKs (KeySet * ks, const char * name);
static void showDiff (KeySet * expected, KeySet * is, const char * name, bool stopOnFirstDiff);


static void testPositiveCompareKeySets (void)
{
	testReadCompare ("toml/positive/basic.toml",
#include "toml/positive/basic.h"
	);
	testReadCompare ("toml/positive/utf8.toml",
#include "toml/positive/utf8.h"
	);

	testReadCompare ("toml/positive/multiline_strings.toml",
#include "toml/positive/multiline_strings.h"
    	);

	testReadCompare ("toml/positive/date.toml",
#include "toml/positive/date.h"
	);

	testReadCompare ("toml/positive/array.toml",
#include "toml/positive/array.h"
	);

	testReadCompare ("toml/positive/simple_table.toml",
#include "toml/positive/simple_table.h"
	);

	testReadCompare ("toml/positive/table_array.toml",
#include "toml/positive/table_array.h"
	);

	testReadCompare ("toml/positive/table_array_nested.toml",
#include "toml/positive/table_array_nested.h"
	);

	testReadCompare ("toml/positive/table_array_table_mixed.toml",
#include "toml/positive/table_array_table_mixed.h"
	);

	testReadCompare ("toml/positive/inline_table.toml",
#include "toml/positive/inline_table.h"
	);

	testReadCompare ("toml/positive/inline_table_empty.toml",
#include "toml/positive/inline_table_empty.h"
	);

	testReadCompare ("toml/positive/inline_table_multiline_values.toml",
#include "toml/positive/inline_table_multiline_values.h"
	);

	testReadCompare ("toml/positive/comment.toml",
#include "toml/positive/comment.h"
	);
}

static void testNegativeCompareErrors (void)
{
	testReadCompareError ("toml/negative/duplicate_key_01.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/duplicate_key_02.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/duplicate_key_03.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/empty_assignment.toml",
#include "toml/error/syntax.h"
	);
	testReadCompareError ("toml/negative/bare_string_rhs.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/array_missing_closing_brackets.toml",
#include "toml/error/syntax.h"
	);
	testReadCompareError ("toml/negative/date_invalid_day.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/date_invalid_month.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/date_invalid_year.toml",
#include "toml/error/semantic.h"
	);
	testReadCompareError ("toml/negative/date_invalid_feb.toml",
#include "toml/error/semantic.h"
	);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	testPositiveCompareKeySets ();
	testNegativeCompareErrors ();

	print_result ("testmod_toml");
	return nbError;
}

static void testReadCompare (const char * filename, KeySet * expected)
{
	printf ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	Key * root = ksLookupByName (expected, PREFIX, KDB_O_POP);
	if (root != NULL)
	{
		if (strcmp (keyString (root), "@CONFIG_FILEPATH@") == 0)
		{
			keySetString (root, srcdir_file (filename));
			ksAppendKey (expected, root);
		}
	}
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Expected kdbGet to succeed, but got failure.");
	compare_keyset (expected, ks);

	showDiff (expected, ks, filename, true);

	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testReadCompareError (const char * filename, KeySet * expected)
{
	printf ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	Key * root = ksLookupByName (expected, PREFIX, KDB_O_POP);
	if (root != NULL)
	{
		if (strcmp (keyString (root), "@CONFIG_FILEPATH@") == 0)
		{
			keySetString (root, srcdir_file (filename));
			ksAppendKey (expected, root);
		}
	}
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "Expected kdbGet to fail, but got success.");

	Key * foundRoot = ksLookupByName (ks, PREFIX, 0);
	succeed_if (foundRoot != NULL, "Could not find root key");
	testCompareErrors (root, foundRoot);

	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testCompareErrors (Key * expected, Key * found)
{
	assert (expected != NULL);
	assert (found != NULL);
	char * metaNames[] = { "error/module", "error/description", NULL };
	for (int i = 0; metaNames[i] != NULL; i++)
	{
		testCompareMetakey (expected, found, metaNames[i]);
	}
}

static void testCompareMetakey (Key * expected, Key * found, const char * metaKeyName)
{
	// printf ("Comparing metakey '%s'\n", metaKeyName);
	keyRewindMeta (expected);
	keyRewindMeta (found);
	const Key * metaExpected = keyNextMeta (expected);
	const Key * metaFound = keyNextMeta (found);
	while (metaExpected != NULL)
	{
		if (strcmp (keyName (metaExpected), metaKeyName) == 0)
		{
			// printf ("\tExpected\t= %s\n", keyString (metaExpected));
			break;
		}
		metaExpected = keyNextMeta (expected);
	}
	while (metaFound != NULL)
	{
		if (strcmp (keyName (metaFound), metaKeyName) == 0)
		{
			// printf ("\tFound\t\t= %s\n", keyString (metaFound));
			break;
		}
		metaFound = keyNextMeta (found);
	}
	succeed_if (metaExpected != NULL, "Could not find metakey in expected key");
	succeed_if (metaFound != NULL, "Could not find metakey in found key");
	if (metaExpected == NULL || metaFound == NULL)
	{
		return;
	}
	succeed_if (strcmp (keyString (metaExpected), keyString (metaFound)) == 0, "Different metakey values");
}

static void showDiff (KeySet * expected, KeySet * is, const char * name, bool stopOnFirstDiff)
{
	bool headerPrinted = false;
	ksRewind (expected);
	ksRewind (is);
	Key * kExp = ksNext (expected);
	Key * kIs = ksNext (is);
	while (kExp != NULL && kIs != NULL)
	{
		if (keyCmp (kExp, kIs) != 0 || strcmp (keyString (kExp), keyString (kIs)) != 0)
		{
			if (!headerPrinted)
			{
				printf ("###### Diffs in '%s'\n", name);
				headerPrinted = true;
			}
			printf ("Key diff:\n\texpected\t= '%s'\n\tcontent:\t'%s'\n\tfound\t\t= '%s'\n\tcontent:\t: '%s'\n", keyName (kExp),
				keyString (kExp), keyName (kIs), keyString (kIs));
			if (stopOnFirstDiff)
			{
				return;
			}
		}
		keyRewindMeta (kExp);
		keyRewindMeta (kIs);
		const Key * metaExp = keyNextMeta (kExp);
		const Key * metaIs = keyNextMeta (kIs);
		while (metaExp != NULL && metaIs != NULL)
		{
			if (keyCmp (metaExp, metaIs) != 0 || strcmp (keyString (metaExp), keyString (metaIs)) != 0)
			{
				if (!headerPrinted)
				{
					printf ("###### Diffs in '%s'\n", name);
					headerPrinted = true;
				}
				printf ("MetaKey diff:\n\texpected\t= '%s'\n\tcontent:\t'%s' = '%s'\n\tfound\t\t= '%s'\n\tcontent:\t'%s' = "
					"'%s'\n",
					keyName (kExp), keyName (metaExp), keyString (metaExp), keyName (kIs), keyName (metaIs),
					keyString (metaIs));
				if (stopOnFirstDiff)
				{
					return;
				}
			}
			metaExp = keyNextMeta (kExp);
			metaIs = keyNextMeta (kIs);
		}
		if (metaExp != NULL || metaIs != NULL)
		{
			if (!headerPrinted)
			{
				printf ("###### Diffs in '%s'\n", name);
				headerPrinted = true;
			}
			printf ("Mismatching metakeys count, there are %s metakeys generated than expected\n",
				metaIs == NULL ? "less" : "too much");
			printf ("Affected keys:\n\texpected\t= '%s'\n\tfound\t\t= '%s'\n", keyName (kExp), keyName (kIs));
			Key * overhead = metaExp != NULL ? kExp : kIs;
			const Key * meta = metaExp != NULL ? metaExp : metaIs;
			do
			{
				printf ("%s Metakeys: '%s': '%s'\n", metaIs == NULL ? "Missing" : "Overhead", keyName (meta),
					keyString (meta));
				meta = keyNextMeta (overhead);
			} while (meta != NULL);
			if (stopOnFirstDiff)
			{
				return;
			}
		}
		kExp = ksNext (expected);
		kIs = ksNext (is);
	}
	if (kExp != NULL || kIs != NULL)
	{
		if (!headerPrinted)
		{
			printf ("###### Diffs in '%s'\n", name);
			headerPrinted = true;
		}
		printf ("Mismatching keyset size, there are %s keys generated than expected\n", kIs == NULL ? "less" : "too much");
		return;
	}
}

static void printKs (KeySet * ks, const char * name)
{
	printf ("######KEYSET: %s\n", name);
	ksRewind (ks);
	Key * key = ksNext (ks);
	while (key != NULL)
	{
		printf ("Key: '%s'\t->\t'%s'", keyName (key), keyString (key));

		keyRewindMeta (key);
		const Key * meta = keyNextMeta (key);
		while (meta != NULL)
		{
			printf ("\n\tMeta: '%s'\t->\t'%s'", keyName (meta), keyString (meta));
			meta = keyNextMeta (key);
		}
		key = ksNext (ks);
		printf ("\n");
	}
}
