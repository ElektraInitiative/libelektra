/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbassert.h>
#include <kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"

#define PREFIX "user/tests/toml"

static void testPositiveCompareKeySets (void);
static void testNegativeCompareErrors (void);
static void testRoundTrip (const char * filename);
static void testReadCompare (const char * filename, KeySet * expected);
static void testReadCompareError (const char * filename, KeySet * expected);
static void testCompareMetakey (Key * expected, Key * found, const char * metaKeyName);
static void testCompareErrors (Key * expected, Key * found);
static void testWriteReads (void);
static void testWriteRead (KeySet * expected);

int main (int argc, char ** argv)
{
	init (argc, argv);

	testPositiveCompareKeySets ();
	testNegativeCompareErrors ();
	// testWriteReads ();

	print_result ("testmod_toml");
	return nbError;
}

static void testPositiveCompareKeySets (void)
{
	testReadCompare ("toml/positive/basic.toml",
#include "toml/positive/basic.h"
	);
	testReadCompare ("toml/positive/string_utf8.toml",
#include "toml/positive/string_utf8.h"
	);
	testReadCompare ("toml/positive/string_basic_escape.toml",
#include "toml/positive/string_basic_escape.h"
	);
	testReadCompare ("toml/positive/string_multiline.toml",
#include "toml/positive/string_multiline.h"
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

static void testWriteReads (void)
{
	testWriteRead (ksNew( 16,
		keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),
		keyNew (PREFIX "/a", KEY_VALUE, "1", KEY_END)
		));
}

static void testWriteRead (KeySet * expected)
{
	const char * filename = "storage_toml_test.toml";
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
	succeed_if (plugin->kdbSet (plugin, expected, root) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Expected kdbGet to succeed, but got failure.");
	
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, root) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Expected kdbGet to succeed, but got failure.");
	compare_keyset (expected, ks);

	ksDel (ks);
	keyDel (root);
	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testReadCompare (const char * filename, KeySet * expected)
{
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
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

	ksDel (ks);
	keyDel (root);
	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testReadCompareError (const char * filename, KeySet * expected)
{
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
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

	ksDel (ks);
	keyDel (root);
	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testCompareErrors (Key * expected, Key * found)
{
	ELEKTRA_ASSERT (expected != NULL, "Expected key is NULL");
	ELEKTRA_ASSERT (found != NULL, "Found key is NULL");
	char * metaNames[] = { "error/module", "error/description", NULL };
	for (int i = 0; metaNames[i] != NULL; i++)
	{
		testCompareMetakey (expected, found, metaNames[i]);
	}
}

static void testCompareMetakey (Key * expected, Key * found, const char * metaKeyName)
{
	keyRewindMeta (expected);
	keyRewindMeta (found);
	const Key * metaExpected = keyNextMeta (expected);
	const Key * metaFound = keyNextMeta (found);
	while (metaExpected != NULL)
	{
		if (strcmp (keyName (metaExpected), metaKeyName) == 0)
		{
			break;
		}
		metaExpected = keyNextMeta (expected);
	}
	while (metaFound != NULL)
	{
		if (strcmp (keyName (metaFound), metaKeyName) == 0)
		{
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
