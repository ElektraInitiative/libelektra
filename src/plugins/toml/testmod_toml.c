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

#define TEST_RW_HEAD                                                                                                                       \
	size_t order = 0;                                                                                                                  \
	KeySet * writeKs = ksNew (0, KS_END);                                                                                              \
	KeySet * expectedKs = ksNew (0, KS_END)
#define TEST_RW_FOOT                                                                                                                       \
	testWriteRead (writeKs, expectedKs);                                                                                               \
	ksDel (expectedKs);                                                                                                                \
	ksDel (writeKs)
#define ADD_KEY_PAIR(name, value) addKeyPair (writeKs, expectedKs, name, value, NULL, NULL, order++)
#define ADD_KEY_PAIR_EXPECT_ORIG(name, value, orig) addKeyPair (writeKs, expectedKs, name, value, orig, NULL, order++)
#define ADD_KEY_PAIR_EXPECT_TYPED(name, value, type) addKeyPair (writeKs, expectedKs, name, value, NULL, type, order++)
#define ADD_KEY_PAIR_EXPECT_ORIG_TYPED(name, value, orig, type) addKeyPair (writeKs, expectedKs, name, value, orig, type, order++)
#define SET_META_WRITE_KS(name, metaName, metaValue)                                                                                       \
	{                                                                                                                                  \
		Key * k = ksLookupByName (writeKs, PREFIX "/" name, 0);                                                                    \
		if (k != NULL)                                                                                                             \
		{                                                                                                                          \
			keySetMeta (k, metaName, metaValue);                                                                               \
		}                                                                                                                          \
	}


static void testPositiveCompareKeySets (void);
static void testNegativeCompareErrors (void);
static void testReadCompare (const char * filename, KeySet * expected);
static void testReadMustError (const char * filename);
static void testWriteRead (KeySet * ksWrite, KeySet * expected);
static void testWriteReadString (void);
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

	testWriteReadString ();
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
	testReadCompare ("toml/basic.toml",
#include "toml/basic.h"
	);
	testReadCompare ("toml/string_utf8.toml",
#include "toml/string_utf8.h"
	);
	testReadCompare ("toml/string_basic_escape.toml",
#include "toml/string_basic_escape.h"
	);
	testReadCompare ("toml/string_multiline.toml",
#include "toml/string_multiline.h"
	);
	testReadCompare ("toml/date.toml",
#include "toml/date.h"
	);
	testReadCompare ("toml/array.toml",
#include "toml/array.h"
	);
	testReadCompare ("toml/simple_table.toml",
#include "toml/simple_table.h"
	);
	testReadCompare ("toml/table_array.toml",
#include "toml/table_array.h"
	);
	testReadCompare ("toml/table_array_nested.toml",
#include "toml/table_array_nested.h"
	);
	testReadCompare ("toml/table_array_table_mixed.toml",
#include "toml/table_array_table_mixed.h"
	);
	testReadCompare ("toml/inline_table.toml",
#include "toml/inline_table.h"
	);
	testReadCompare ("toml/inline_table_empty.toml",
#include "toml/inline_table_empty.h"
	);
	testReadCompare ("toml/inline_table_multiline_values.toml",
#include "toml/inline_table_multiline_values.h"
	);
	testReadCompare ("toml/comment.toml",
#include "toml/comment.h"
	);
}

static void testNegativeCompareErrors (void)
{
	testReadMustError ("toml/bad_duplicate_key_01.toml");
	testReadMustError ("toml/bad_duplicate_key_02.toml");
	testReadMustError ("toml/bad_duplicate_key_03.toml");
	testReadMustError ("toml/bad_empty_assignment.toml");
	testReadMustError ("toml/bad_bare_string_rhs.toml");
	testReadMustError ("toml/bad_array_missing_closing_brackets.toml");
	testReadMustError ("toml/bad_date_invalid_day.toml");
	testReadMustError ("toml/bad_date_invalid_month.toml");
	testReadMustError ("toml/bad_date_invalid_year.toml");
	testReadMustError ("toml/bad_date_invalid_feb.toml");
}

static void testWriteReadString (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR_EXPECT_TYPED("a/multiline1", "first line\nsecond line", "string");
	ADD_KEY_PAIR_EXPECT_ORIG_TYPED("b/withescapechars", "first line\nsecond line\r", "first line\\nsecond line\\r", "string");

	ADD_KEY_PAIR_EXPECT_TYPED("c/numberstring01", "1337", "string");
	SET_META_WRITE_KS("c/numberstring01", "type", "string");
	
	ADD_KEY_PAIR_EXPECT_TYPED("c/numberstring02", "13_37", "string");
	SET_META_WRITE_KS("c/numberstring02", "type", "string");
	
	ADD_KEY_PAIR_EXPECT_TYPED("c/numberstring03", "+3e-7", "string");
	SET_META_WRITE_KS("c/numberstring03", "type", "string");

	ADD_KEY_PAIR_EXPECT_TYPED("d/datestring", "2000-01-01", "string");
	SET_META_WRITE_KS("d/datestring", "type", "string");

	TEST_RW_FOOT;
}

static void testWriteReadInteger (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR ("int1", "+1337");
	ADD_KEY_PAIR ("int2", "-666");
	ADD_KEY_PAIR ("int3", "0");
	ADD_KEY_PAIR ("int4", "3000");
	ADD_KEY_PAIR_EXPECT_ORIG ("int5", "+1999000", "+1_999_000");
	TEST_RW_FOOT;
}

static void testWriteReadIntegerOtherBase (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR_EXPECT_ORIG ("a/hex1", "11251456", "0xA_Baf00");
	ADD_KEY_PAIR_EXPECT_ORIG ("a/hex2", "1", "0x00_1");
	ADD_KEY_PAIR_EXPECT_ORIG ("a/hex3", "0", "0x0_0");
	ADD_KEY_PAIR_EXPECT_ORIG ("b/oct1", "735", "0o13_37");
	ADD_KEY_PAIR_EXPECT_ORIG ("b/oct2", "0", "0o0_000");
	ADD_KEY_PAIR_EXPECT_ORIG ("b/oct3", "735", "0o1_3_3_7");
	ADD_KEY_PAIR_EXPECT_ORIG ("c/bin1", "0", "0b0_0_0_0");
	ADD_KEY_PAIR_EXPECT_ORIG ("c/bin2", "8", "0b100_0");
	ADD_KEY_PAIR_EXPECT_ORIG ("c/bin3", "0", "0b000");
	TEST_RW_FOOT;
}

static void testWriteReadFloat (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR ("float1", "+0.3");
	ADD_KEY_PAIR_EXPECT_ORIG ("float2", "-7.1313E+10", "-7.1_313E+1_0");
	ADD_KEY_PAIR ("float3", "+2e-3");
	ADD_KEY_PAIR_EXPECT_ORIG ("float4", "+200.003", "+20_0.003");
	ADD_KEY_PAIR ("float5", "nan");
	ADD_KEY_PAIR ("float6", "+nan");
	ADD_KEY_PAIR ("float7", "-inf");
	TEST_RW_FOOT;
}

static void testWriteReadDate (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR ("date1", "2000-12-31T10:00:00Z");
	ADD_KEY_PAIR ("date2", "1990-12-31T23:59:60Z");
	ADD_KEY_PAIR ("date3", "1937-01-01T12:00:27.87+00:20");
	ADD_KEY_PAIR ("date4", "23:59:59.99999");
	ADD_KEY_PAIR ("date5", "00:00:00");
	TEST_RW_FOOT;
}

static void testWriteReadBoolean (void)
{
	TEST_RW_HEAD;

	// only written as boolean, if has metakey boolean
	ADD_KEY_PAIR_EXPECT_TYPED ("bool1", "1", "boolean");
	SET_META_WRITE_KS ("bool1", "type", "boolean");
	ADD_KEY_PAIR_EXPECT_TYPED ("bool2", "0", "boolean");
	SET_META_WRITE_KS ("bool2", "type", "boolean");

	// if have to write true/false without boolean metakey, it's written (and then read) as string
	ADD_KEY_PAIR_EXPECT_TYPED ("bool3", "true", "string");
	ADD_KEY_PAIR_EXPECT_TYPED ("bool4", "false", "string");

	// if have to write 0/1 without boolean metakey, it's written (and then read) as number
	ADD_KEY_PAIR ("bool5", "0");
	ADD_KEY_PAIR ("bool6", "1");

	TEST_RW_FOOT;
}

static void testWriteReadCheckSparseHierarchy (void)
{
	TEST_RW_HEAD;
	ADD_KEY_PAIR_EXPECT_TYPED ("a", "hello", "string");
	ADD_KEY_PAIR_EXPECT_TYPED ("a/b/c/d", "hello", "string");
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

	compare_keyset (expected, ksRead);

	PLUGIN_CLOSE ();
	ksDel (ksRead);
	remove(filename);
}

static void testReadCompare (const char * filename, KeySet * expected)
{
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
	Key * key = keyNew (PREFIX, KEY_VALUE, value, KEY_META, "order", orderStr, KEY_END);
	keyAddName (key, "/");
	keyAddName (key, name);
	if (origValue != NULL)
	{
		keySetMeta (key, "origvalue", origValue);
	}
	if (type != NULL)
	{
		keySetMeta (key, "type", type);
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
