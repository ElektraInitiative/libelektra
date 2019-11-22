/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbassert.h>
#include <kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"
#include "utility.h"

#define PREFIX "user/tests/toml"

#define TEST_RW_HEAD(t)                                                                                                                    \
	const char * expectedType = t;                                                                                                     \
	size_t order = 0;                                                                                                                  \
	KeySet * writeKs = ksNew (0, KS_END);                                                                                              \
	KeySet * expectedKs = ksNew (0, KS_END)
#define TEST_RW_FOOT                                                                                                                       \
	testWriteRead (writeKs, expectedKs);                                                                                               \
	ksDel (expectedKs);                                                                                                                \
	ksDel (writeKs)
#define ADD_KEY_PAIR(name, value) addKeyPair (writeKs, expectedKs, name, value, NULL, expectedType, order++)
#define ADD_KEY_PAIR_ORIG(name, value, orig) addKeyPair (writeKs, expectedKs, name, value, orig, expectedType, order++)

static void testPositiveCompareKeySets (void);
static void testNegativeCompareErrors (void);
static void testReadCompare (const char * filename, KeySet * expected);
static void testReadMustError (const char * filename);
static void testWriteRead (KeySet * ksWrite, KeySet * expected);
static void testWriteReadInteger (void);
static void testWriteReadIntegerOtherBase (void);
static void testWriteReadFloat (void);
static void testWriteReadDate (void);
static void testWriteReadBoolean (void);
static void testWriteReadCheckSparseHierarchy (void);
static Key * buildFullKey (const char * name, const char * value, const char * origValue, const char * type, size_t order);
static Key * buildSimpleKey (const char * name, const char * value);
static void addKeyPair (KeySet * writeKs, KeySet * expectedKs, const char * name, const char * value, const char * origValue,
			const char * type, size_t order);

int main (int argc, char ** argv)
{
	init (argc, argv);

	testPositiveCompareKeySets ();
	testNegativeCompareErrors ();

	testWriteReadInteger ();
	testWriteReadIntegerOtherBase ();
	testWriteReadFloat ();
	testWriteReadDate ();
	testWriteReadBoolean ();

	testWriteReadCheckSparseHierarchy ();

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
	testReadMustError ("toml/negative/duplicate_key_01.toml");
	testReadMustError ("toml/negative/duplicate_key_02.toml");
	testReadMustError ("toml/negative/duplicate_key_03.toml");
	testReadMustError ("toml/negative/empty_assignment.toml");
	testReadMustError ("toml/negative/bare_string_rhs.toml");
	testReadMustError ("toml/negative/array_missing_closing_brackets.toml");
	testReadMustError ("toml/negative/date_invalid_day.toml");
	testReadMustError ("toml/negative/date_invalid_month.toml");
	testReadMustError ("toml/negative/date_invalid_year.toml");
	testReadMustError ("toml/negative/date_invalid_feb.toml");
}


static void testWriteReadInteger (void)
{
	TEST_RW_HEAD ("long_long");
	ADD_KEY_PAIR ("int1", "+1337");
	ADD_KEY_PAIR ("int2", "-666");
	ADD_KEY_PAIR ("int3", "0");
	ADD_KEY_PAIR ("int4", "3000");
	ADD_KEY_PAIR_ORIG ("int5", "+1999000", "+1_999_000");
	TEST_RW_FOOT;
}

static void testWriteReadIntegerOtherBase (void)
{
	TEST_RW_HEAD ("unsigned_long_long");
	ADD_KEY_PAIR_ORIG ("a/hex1", "11251456", "0xA_Baf00");
	ADD_KEY_PAIR_ORIG ("a/hex2", "1", "0x00_1");
	ADD_KEY_PAIR_ORIG ("a/hex3", "0", "0x0_0");
	ADD_KEY_PAIR_ORIG ("b/oct1", "735", "0o13_37");
	ADD_KEY_PAIR_ORIG ("b/oct2", "0", "0o0_000");
	ADD_KEY_PAIR_ORIG ("b/oct3", "735", "0o1_3_3_7");
	ADD_KEY_PAIR_ORIG ("c/bin1", "0", "0b0_0_0_0");
	ADD_KEY_PAIR_ORIG ("c/bin2", "8", "0b100_0");
	ADD_KEY_PAIR_ORIG ("c/bin3", "0", "0b000");
	TEST_RW_FOOT;
}

static void testWriteReadFloat (void)
{
	TEST_RW_HEAD ("double");
	ADD_KEY_PAIR ("float1", "+0.3");
	ADD_KEY_PAIR_ORIG ("float2", "-7.1313E+10", "-7.1_313E+1_0");
	ADD_KEY_PAIR ("float3", "+2e-3");
	ADD_KEY_PAIR_ORIG ("float4", "+200.003", "+20_0.003");
	ADD_KEY_PAIR ("float5", "nan");
	ADD_KEY_PAIR ("float6", "+nan");
	ADD_KEY_PAIR ("float7", "-inf");
	TEST_RW_FOOT;
}

static void testWriteReadDate (void)
{
	TEST_RW_HEAD ("string");
	ADD_KEY_PAIR ("date1", "2000-12-31T10:00:00Z");
	ADD_KEY_PAIR ("date2", "1990-12-31T23:59:60Z");
	ADD_KEY_PAIR ("date3", "1937-01-01T12:00:27.87+00:20");
	ADD_KEY_PAIR ("date4", "23:59:59.99999");
	ADD_KEY_PAIR ("date5", "00:00:00");
	TEST_RW_FOOT;
}

static void testWriteReadBoolean (void)
{
	// can't check without active type plugin, must check how to add plugin dependency to tests
	/* TEST_RW_HEAD("boolean");
	ADD_KEY_PAIR_ORIG("bool1", "1", "true");
	ADD_KEY_PAIR_ORIG("bool2", "0", "false");
	TEST_RW_FOOT;*/
}

static void testWriteReadCheckSparseHierarchy (void)
{
	TEST_RW_HEAD ("string");
	ADD_KEY_PAIR ("a", "hello");
	ADD_KEY_PAIR ("a/b/c/d", "hello");
	TEST_RW_FOOT;
}

static void testWriteRead (KeySet * ksWrite, KeySet * expected)
{
	const char * filename = "test_write_read.toml";
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");

	succeed_if (plugin->kdbSet (plugin, ksWrite, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not write keys");

	KeySet * ksRead = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ksRead, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not read written keys");

	dumpKS (ksRead);
	compare_keyset (expected, ksRead);

	PLUGIN_CLOSE ();
	ksDel (ksRead);
}

static void testReadCompare (const char * filename, KeySet * expected)
{
	printf ("Reading '%s'\n", filename);
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Expected kdbGet to succeed, but got failure.");
	compare_keyset (expected, ks);

	ksDel (ks);
	PLUGIN_CLOSE ();
	ksDel (expected);
}

static void testReadMustError (const char * filename)
{
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "Expected kdbGet to fail, but got success.");

	ksDel (ks);
	PLUGIN_CLOSE ();
}

static Key * buildFullKey (const char * name, const char * value, const char * origValue, const char * type, size_t order)
{
	char orderStr[64];
	snprintf (orderStr, 64, "%lu", order);
	Key * key = keyNew (PREFIX, KEY_VALUE, value, KEY_META, "type", type, KEY_META, "order", orderStr, KEY_END);
	keyAddName (key, "/");
	keyAddName (key, name);
	if (origValue != NULL)
	{
		keySetMeta (key, "origvalue", origValue);
	}
	return key;
}

static Key * buildSimpleKey (const char * name, const char * value)
{
	Key * key = keyNew (PREFIX, KEY_VALUE, value, KEY_END);
	keyAddName (key, "/");
	keyAddName (key, name);
	return key;
}

static void addKeyPair (KeySet * writeKs, KeySet * expectedKs, const char * name, const char * value, const char * origValue,
			const char * type, size_t order)
{
	if (origValue != NULL)
	{
		ksAppendKey (writeKs, buildSimpleKey (name, origValue));
	}
	else
	{
		ksAppendKey (writeKs, buildSimpleKey (name, value));
	}
	ksAppendKey (expectedKs, buildFullKey (name, value, origValue, type, order));
}
