/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/kdb.h>
#include <kdbassert.h>
#include <kdblogger.h>
#include <elektra/kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"
#include "utility.h"

static const char * prefix = NULL;

#define TEST_WR_HEAD                                                                                                                       \
	printf ("Start Test: %s\n", __func__);                                                                                             \
	Key * lastKey = NULL;                                                                                                              \
	KeySet * writeKs = ksNew (0, KS_END);                                                                                              \
	KeySet * expectedKs = ksNew (0, KS_END)
#define TEST_WR_FOOT                                                                                                                       \
	testWriteReadCompare (writeKs, expectedKs);                                                                                        \
	ksDel (expectedKs);                                                                                                                \
	ksDel (writeKs);                                                                                                                   \
	printf ("End Test: %s\n\n", __func__)

// Macros to be used in TEST_WR environments
#define WRITE_KV(name, value)                                                                                                              \
	{                                                                                                                                  \
		lastKey = addKey (writeKs, name, value, 0, NULL, NULL, NULL, NULL, -1);                                                    \
	}
#define WRITE_KV_BIN(name, value, size)                                                                                                    \
	{                                                                                                                                  \
		lastKey = addKey (writeKs, name, value, size, NULL, NULL, NULL, NULL, -1);                                                 \
	}

#define WRITE_KEY(name)                                                                                                                    \
	{                                                                                                                                  \
		lastKey = addKey (writeKs, name, NULL, 0, NULL, NULL, NULL, NULL, -1);                                                     \
	}
#define EXPECTED_KV(name, value)                                                                                                           \
	{                                                                                                                                  \
		lastKey = addKey (expectedKs, name, value, 0, NULL, NULL, NULL, NULL, -1);                                                 \
	}
#define EXPECTED_KEY(name)                                                                                                                 \
	{                                                                                                                                  \
		lastKey = addKey (expectedKs, name, NULL, 0, NULL, NULL, NULL, NULL, -1);                                                  \
	}
#define DUP_EXPECTED                                                                                                                       \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			lastKey = keyDup (lastKey, KEY_CP_ALL);                                                                            \
			ksAppendKey (expectedKs, lastKey);                                                                                 \
		}                                                                                                                          \
	}
#define SET_VALUE(value)                                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetString (lastKey, value);                                                                        \
	}
#define VALUE_TO_ORIG_NEW_VALUE(value)                                                                                                     \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			char * oldVal = elektraStrDup (keyString (lastKey));                                                               \
			keySetString (lastKey, value);                                                                                     \
			keySetMeta (lastKey, "origvalue", oldVal);                                                                         \
			elektraFree (oldVal);                                                                                              \
		}                                                                                                                          \
	}
#define SET_TYPE(type)                                                                                                                     \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "type", type);                                                                   \
	}
#define SET_STRING_TYPE(tomlType)                                                                                                          \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "type", "string");                                                               \
		if (lastKey != NULL) keySetMeta (lastKey, "tomltype", tomlType);                                                           \
	}
#define SET_ORIG_VALUE(orig)                                                                                                               \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "origvalue", orig);                                                              \
	}
#define SET_ARRAY(array)                                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			keySetMeta (lastKey, "array", array);                                                                              \
			keySetMeta (lastKey, "binary", NULL);                                                                              \
		}                                                                                                                          \
	}
#define SET_TOML_TYPE(type)                                                                                                                \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			keySetMeta (lastKey, "tomltype", type);                                                                            \
			keySetMeta (lastKey, "binary", NULL);                                                                              \
		}                                                                                                                          \
	}

#define SET_META(name, value)                                                                                                              \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			keySetMeta (lastKey, name, value);                                                                                 \
		}                                                                                                                          \
	}

#define CLEAR_META(name)                                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL)                                                                                                       \
		{                                                                                                                          \
			keySetMeta (lastKey, name, NULL);                                                                                  \
		}                                                                                                                          \
	}
#define SET_ORDER(order)                                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL) setOrderForKey (lastKey, order);                                                                      \
	}
#define SET_COMMENT(index, text, start)                                                                                                    \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, text, start, index);                                                             \
	}
#define SET_INLINE_COMMENT(text, start)                                                                                                    \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, text, start, 0);                                                                 \
	}
#define SET_EMPTY_LINE(index)                                                                                                              \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, NULL, "", index);                                                                \
	}

#define SET_BINARY                                                                                                                         \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "binary", "");                                                                   \
	}
#define CLEAR_BINARY                                                                                                                       \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "binary", NULL);                                                                 \
	}

static bool writeFile (const char * filename, KeySet * ksWrite, int pluginStatus);
static void testRoundtrip (const char * filePath);
static void testRead (void);
static void testReadRoot (void);
static void testWriteRead (const char * _prefix);
static void testReadCompare (const char * filename, KeySet * expected);
static void testReadMustError (const char * filename);
static void testWriteReadCompare (KeySet * ksWrite, KeySet * expected);
static void testWriteReadAssignments (void);
static void testWriteReadEmptyKeyName (void);
static void testWriteReadArray (void);
static void testWriteReadArrayNested (void);
static void testWriteReadInlineTable (void);
static void testWriteReadInlineTableNested (void);
static void testWriteReadInlineTableInArray (void);
static void testWriteReadArrayInlineTableAlternating (void);
static void testWriteReadTable (void);
static void testWriteReadTableNested (void);
static void testWriteReadTableArray (void);
static void testWriteReadTableArrayWithComments (void);
static void testWriteReadSimpleTableInTableArray (void);
static void testWriteReadSimpleTableBeforeTableArray (void);
static void testWriteReadString (void);
static void testWriteReadInteger (void);
static void testWriteReadFloat (void);
static void testWriteReadDate (void);
static void testWriteReadBoolean (void);
static void testWriteReadCheckSparseHierarchy (void);
static void testWriteReadComments (void);
static void testWriteReadCommentsArray (void);
static void testWriteReadOrderTableNonTable (void);
static void testWriteReadNull (void);
static void testWriteReadBogusMetaMustError (void);
// static void testWriteReadBase64(void);
static Key * addKey (KeySet * ks, const char * name, const char * value, size_t size, const char * orig, const char * type,
		     const char * array, const char * tomltype, int order);
static void setComment (Key * key, const char * comment, const char * start, size_t index);

static bool roundtripFile (const char * filenameIn, const char * filenameOut);
static bool compareFilesIgnoreWhitespace (const char * filenameA, const char * filenameB);
static void test_toml_1_0_0 (const char * _prefix);

int main (int argc, char ** argv)
{
	init (argc, argv);

	printf ("### Testing with user:/tests/toml ###\n");
	testRead ();
	testWriteRead ("user:/tests/toml");
	test_toml_1_0_0 ("user:/tests/toml");

	printf ("### Testing with user:/ ###\n");
	testReadRoot ();
	testWriteRead ("user:/");
	test_toml_1_0_0 ("user:/");

	print_result ("testmod_toml");
	return nbError;
}

static void test_toml_1_0_0 (const char * _prefix)
{
	prefix = _prefix;
#define PATH_PREFIX "toml/checks_toml_1_0_0/"
	testRoundtrip (PATH_PREFIX "roundtrip.toml");
	testReadMustError (PATH_PREFIX "invalid_read/array_redefined_table_array.toml");
	testReadMustError (PATH_PREFIX "invalid_read/empty_assignment.toml");
	testReadMustError (PATH_PREFIX "invalid_read/floats.toml");
	testReadMustError (PATH_PREFIX "invalid_read/key_redefinition.toml");
	testReadMustError (PATH_PREFIX "invalid_read/key_redefinition_quotes.toml");
	testReadMustError (PATH_PREFIX "invalid_read/no_keyname.toml");
	testReadMustError (PATH_PREFIX "invalid_read/table_redefine_value.toml");
	testReadMustError (PATH_PREFIX "invalid_read/table_redefinition.toml");
	testReadMustError (PATH_PREFIX "invalid_read/two_assignments_one_line.toml");
	testReadMustError (PATH_PREFIX "invalid_read/apostroph_too_much.toml");
	// TODO: These reads should fail, but they do not fail yet
	// testReadMustError (PATH_PREFIX "invalid_read/inline_appending.toml");
	// testReadMustError (PATH_PREFIX "invalid_read/inline_appending2.toml");
	// testReadMustError (PATH_PREFIX "invalid_read/integer_redefined_table.toml");
	// testReadMustError (PATH_PREFIX "invalid_read/simple_redefined_table_array.toml");
	// testReadMustError (PATH_PREFIX "invalid_read/table_array_simple_table_same_name.toml");
#undef PATH_PREFIX

	prefix = NULL;
}

static void testRead (void)
{
#define PREFIX "user:/tests/toml"
	prefix = PREFIX;

	testReadCompare ("toml/basic.toml",
#include "toml/basic.h"
	);
	testReadCompare ("toml/integer.toml",
#include "toml/integer.h"
	);
	testReadCompare ("toml/key_names_empty.toml",
#include "toml/key_names_empty.h"
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
	testReadCompare ("toml/string_null.toml",
#include "toml/string_null.h"
	);
	testReadCompare ("toml/string_base64.toml",
#include "toml/string_base64.h"
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
#undef PREFIX

	testReadMustError ("toml/bad_duplicate_key_01.toml");
	testReadMustError ("toml/bad_duplicate_key_02.toml");
	testReadMustError ("toml/bad_duplicate_key_03.toml");
	testReadMustError ("toml/bad_empty_assignment.toml");
	testReadMustError ("toml/bad_bare_string_rhs.toml");
	testReadMustError ("toml/bad_date_invalid_day.toml");
	testReadMustError ("toml/bad_date_invalid_month.toml");
	testReadMustError ("toml/bad_date_invalid_year.toml");
	testReadMustError ("toml/bad_date_invalid_feb.toml");
	testReadMustError ("toml/bad_string_single_with_nl_literal.toml");
	testReadMustError ("toml/bad_string_single_with_nl_basic.toml");
	testReadMustError ("toml/integer_overflow/binary.toml");
	testReadMustError ("toml/integer_overflow/octal.toml");
	testReadMustError ("toml/integer_overflow/decimal.toml");
	testReadMustError ("toml/integer_overflow/decimal_under.toml");
	testReadMustError ("toml/integer_overflow/hexadecimal.toml");

	prefix = NULL;
}

static void testWriteRead (const char * _prefix)
{
	prefix = _prefix;
	if (strcmp (prefix, "user:/") != 0)
	{
		testWriteReadEmptyKeyName ();
	}
	testWriteReadAssignments ();
	testWriteReadArray ();
	testWriteReadArrayNested ();
	testWriteReadTableArray ();
	testWriteReadTableArrayWithComments ();
	testWriteReadTable ();
	testWriteReadTableNested ();
	testWriteReadInlineTable ();
	testWriteReadInlineTableNested ();
	testWriteReadString ();
	testWriteReadNull ();
	// testWriteReadBase64();
	testWriteReadInteger ();
	testWriteReadFloat ();
	testWriteReadDate ();
	testWriteReadBoolean ();
	testWriteReadCheckSparseHierarchy ();
	testWriteReadComments ();
	testWriteReadCommentsArray ();
	testWriteReadSimpleTableInTableArray ();
	testWriteReadSimpleTableBeforeTableArray ();
	testWriteReadInlineTableInArray ();
	testWriteReadArrayInlineTableAlternating ();
	testWriteReadOrderTableNonTable ();
	testWriteReadBogusMetaMustError ();
	prefix = NULL;
}

static void testReadRoot (void)
{
#define PREFIX "user:/"
	prefix = PREFIX;

	testReadCompare ("toml/basic.toml",
#include "toml/basic.h"
	);
	testReadCompare ("toml/integer.toml",
#include "toml/integer.h"
	);
	//	testReadCompare ("toml/key_names_empty.toml",
	//#include "toml/key_names_empty.h"
	//	);
	testReadCompare ("toml/string_utf8.toml",
#include "toml/string_utf8.h"
	);
	testReadCompare ("toml/string_basic_escape.toml",
#include "toml/string_basic_escape.h"
	);
	testReadCompare ("toml/string_multiline.toml",
#include "toml/string_multiline.h"
	);
	testReadCompare ("toml/string_null.toml",
#include "toml/string_null.h"
	);
	testReadCompare ("toml/string_base64.toml",
#include "toml/string_base64.h"
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
#undef PREFIX

	testReadMustError ("toml/bad_duplicate_key_01.toml");
	testReadMustError ("toml/bad_duplicate_key_02.toml");
	testReadMustError ("toml/bad_duplicate_key_03.toml");
	testReadMustError ("toml/bad_empty_assignment.toml");
	testReadMustError ("toml/bad_bare_string_rhs.toml");
	testReadMustError ("toml/bad_date_invalid_day.toml");
	testReadMustError ("toml/bad_date_invalid_month.toml");
	testReadMustError ("toml/bad_date_invalid_year.toml");
	testReadMustError ("toml/bad_date_invalid_feb.toml");
	testReadMustError ("toml/bad_string_single_with_nl_literal.toml");
	testReadMustError ("toml/bad_string_single_with_nl_basic.toml");
	testReadMustError ("toml/integer_overflow/binary.toml");
	testReadMustError ("toml/integer_overflow/octal.toml");
	testReadMustError ("toml/integer_overflow/decimal.toml");
	testReadMustError ("toml/integer_overflow/decimal_under.toml");
	testReadMustError ("toml/integer_overflow/hexadecimal.toml");

	prefix = NULL;
}

static void testWriteReadEmptyKeyName (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("%", "discouraged, but valid");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("%/%", "also discouraged, but valid");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");


	TEST_WR_FOOT;
}

static void testWriteReadNull (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("null_valued_key", "@NULL");
	SET_ORDER (0);

	EXPECTED_KV ("null_valued_key", NULL);
	SET_ORDER (0);
	SET_META ("tomltype", "string_basic");

	TEST_WR_FOOT;
}

static void testWriteReadBogusMetaMustError (void)
{
	TEST_WR_HEAD;

	char * fileName = "test_write_read.toml";
	WRITE_KEY ("asdf");
	SET_META ("asdf", "asdf");
	writeFile (fileName, writeKs, ELEKTRA_PLUGIN_STATUS_ERROR);
	keyDel (lastKey);
	ksDel (expectedKs);
	ksDel (writeKs);
	printf ("End Test: %s\n\n", __func__);
	remove (fileName);
}

/*static void testWriteReadBase64 (void)
{
	TEST_WR_HEAD;

	WRITE_KV_BIN("base64_valued_key", "\x00\x01\x02\xFF", 4);
	SET_ORDER (0);

	DUP_EXPECTED;

	TEST_WR_FOOT;
}*/

static void testWriteReadOrderTableNonTable (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("table");
	SET_ORDER (0);
	SET_TOML_TYPE ("simpletable");

	WRITE_KV ("table/a", "0");
	SET_ORDER (1);

	WRITE_KV ("b", "1");
	SET_ORDER (2);

	// b is expected to get ordered before the table on writing,
	// otherwise it would get a member of the table on subsequent readings
	EXPECTED_KEY ("table");
	SET_ORDER (1);
	SET_TOML_TYPE ("simpletable");

	EXPECTED_KV ("table/a", "0");
	SET_ORDER (2);
	SET_TYPE ("long_long");

	EXPECTED_KV ("b", "1");
	SET_ORDER (0);
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadInlineTableInArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("array");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_ARRAY ("#1");

	WRITE_KEY ("array/#0");
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KV ("array/#0/c", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/b", "1");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/a", "2");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("array/#1");
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KV ("array/#1/c", "3");
	SET_ORDER (4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#1/b", "4");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#1/a", "5");
	SET_ORDER (6);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadArrayInlineTableAlternating (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("inline");
	SET_ORDER (0);
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KEY ("inline/array");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_ARRAY ("#1");

	WRITE_KEY ("inline/array/#0");
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KEY ("inline/array/#0/array2");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_ARRAY ("#0");

	WRITE_KV ("inline/array/#0/array2/#0", "0");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("inline/array/#1");
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KEY ("inline/array/#1/array2");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_ARRAY ("#0");

	WRITE_KV ("inline/array/#1/array2/#0", "1");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadSimpleTableInTableArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("ta");
	SET_ORDER (0);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#1");

	WRITE_KV ("ta/#0/a", "1337");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta/#0/table");
	SET_ORDER (2);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("/ta/#0/table/b", "666");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta/#1/a", "111");
	SET_ORDER (4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta/#1/table");
	SET_ORDER (5);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("/ta/#1/table/b", "222");
	SET_ORDER (6);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadSimpleTableBeforeTableArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("table");
	SET_ORDER (0);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;


	WRITE_KV ("table/b", "123");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta");
	SET_ORDER (2);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#0");

	WRITE_KV ("ta/#0/a", "1337");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadAssignments (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("a", "0");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("b", "hello");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("c", "3.1415");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("double");

	WRITE_KV ("3/14", "PI");
	SET_ORDER (3);
	DUP_EXPECTED
	SET_STRING_TYPE ("string_basic");

	TEST_WR_FOOT;
}

static void testWriteReadString (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("multiline1", "first line\nsecond line");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_ORIG_VALUE ("first line\\nsecond line");
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("multiline2", "first line\nsecond line\nthird line");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_ml_basic");

	WRITE_KV ("withescapechars", "first line\\nsecond line\\r");
	SET_ORDER (2);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("first line\nsecond line\r");
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("numberstring01", "1337");
	SET_ORDER (3);
	SET_STRING_TYPE ("string_basic");
	DUP_EXPECTED;

	WRITE_KV ("numberstring02", "13_37");
	SET_ORDER (4);
	SET_STRING_TYPE ("string_literal");
	DUP_EXPECTED;

	WRITE_KV ("numberstring03", "+3e-7");
	SET_ORDER (5);
	SET_STRING_TYPE ("string_ml_basic");
	DUP_EXPECTED;

	WRITE_KV ("datestring", "2000-01-01");
	SET_ORDER (6);
	SET_STRING_TYPE ("string_ml_literal");
	DUP_EXPECTED;

	TEST_WR_FOOT;
}


static void testWriteReadArray (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("b/alphabetically/after/array/but/with/zero/order", "0");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("aa/alphabetically/before/array/and/no/order", "0");
	DUP_EXPECTED;
	SET_ORDER (1);
	SET_TYPE ("long_long");

	WRITE_KV ("array/#3", "1337");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0", "666");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#2", "1000");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#1", "3");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	EXPECTED_KEY ("array");
	SET_ORDER (2);
	SET_ARRAY ("#3");

	WRITE_KV ("b/alphabetically/after/no/order", "0");
	DUP_EXPECTED;
	SET_ORDER (3);
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadArrayNested (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("b/alphabetically/after/array/but/zero/order", "0");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/#0/#0", "0");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/#0/#1", "1");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/#1/#0", "2");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/#1/#1", "3");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#0/#1/#2", "4");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	EXPECTED_KEY ("array");
	SET_ARRAY ("#0");
	SET_ORDER (1);
	EXPECTED_KEY ("array/#0");
	SET_ARRAY ("#1");
	EXPECTED_KEY ("array/#0/#0");
	SET_ARRAY ("#1");
	EXPECTED_KEY ("array/#0/#1");
	SET_ARRAY ("#2");

	TEST_WR_FOOT;
}

static void testWriteReadInlineTable (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("inl_table");
	SET_ORDER (0);
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KV ("inl_table/b", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("inl_table/a", "1");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("inl_table/c", "2");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadInlineTableNested (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("inl_table");
	SET_ORDER (0);
	SET_TOML_TYPE ("inlinetable");
	DUP_EXPECTED;

	WRITE_KV ("inl_table/value0", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("inl_table/nest");
	SET_TOML_TYPE ("inlinetable");
	SET_ORDER (2);
	DUP_EXPECTED;

	WRITE_KV ("inl_table/nest/value1", "1");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("inl_table/nest/another/nest");
	SET_TOML_TYPE ("inlinetable");
	SET_ORDER (4);
	DUP_EXPECTED;

	WRITE_KV ("inl_table/nest/another/nest/value2", "2");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadTable (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("table");
	SET_ORDER (0);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("table/b", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("table/a", "1");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("another/table");
	SET_ORDER (3);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("another/table/b", "0");
	SET_ORDER (4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("another/table/a", "1");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadTableNested (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("table");
	SET_ORDER (0);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("table/y", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("table/nested");
	SET_ORDER (2);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("table/nested/a", "1");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("table/nested/another/three/levels");
	SET_ORDER (4);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("table/nested/another/three/levels/b", "2");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("unrelated/table");
	SET_ORDER (6);
	SET_TOML_TYPE ("simpletable");
	DUP_EXPECTED;

	WRITE_KV ("unrelated/table/c", "3");
	SET_ORDER (7);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadTableArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("ta");
	SET_ORDER (0);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#0");

	WRITE_KV ("ta/#0/b", "0");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta/#0/a", "1");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta_nest");
	SET_ORDER (3);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#1");

	WRITE_KEY ("ta_nest/#0/nested");
	SET_ORDER (4);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#1");

	WRITE_KV ("ta_nest/#0/nested/#0/a", "0");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta_nest/#0/nested/#1/a", "1");
	SET_ORDER (6);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta_nest/#1/nested");
	SET_ORDER (7);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#2");

	WRITE_KV ("ta_nest/#1/nested/#0/a", "2");
	SET_ORDER (8);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta_nest/#1/nested/#1/a", "3");
	SET_ORDER (9);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta_nest/#1/nested/#2/a", "4");
	SET_ORDER (10);
	DUP_EXPECTED;
	SET_TYPE ("long_long");


	TEST_WR_FOOT;
}

static void testWriteReadTableArrayWithComments (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("key", "0");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("ta");
	SET_ORDER (1);
	SET_TOML_TYPE ("tablearray");
	DUP_EXPECTED;
	SET_ARRAY ("#0");

	WRITE_KEY ("ta/#0");
	SET_INLINE_COMMENT (" inline comment", " ");
	SET_COMMENT (1, " top-most preceding comment", "");
	SET_COMMENT (2, " preceding comment", "");
	CLEAR_BINARY;
	DUP_EXPECTED;

	WRITE_KV ("ta/#0/a", "1");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("ta/#0/b", "2");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadInteger (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("binary_min", "0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000");
	SET_ORDER (0);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("binary_max", "0b11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111");
	SET_ORDER (1);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("18446744073709551615");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("binary_overflow", "0b1_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");


	WRITE_KV ("octal_min", "0o0000000000000000000000");
	SET_ORDER (3);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("octal_max", "0o1777777777777777777777");
	SET_ORDER (4);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("18446744073709551615");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("octal_overflow", "0o2000000000000000000000");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");


	WRITE_KV ("decimal_min", "-9_223_372_036_854_775_808");
	SET_ORDER (6);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("-9223372036854775808");
	SET_TYPE ("long_long");

	WRITE_KV ("decimal_max", "+9_223_372_036_854_775_807");
	SET_ORDER (7);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("+9223372036854775807");
	SET_TYPE ("long_long");

	WRITE_KV ("decimal_underflow", "-9_223_372_036_854_775_809");
	SET_ORDER (8);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("decimal_overflow", "9_223_372_036_854_775_808");
	SET_ORDER (9);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");


	WRITE_KV ("hexadecimal_min", "0x0000_0000_0000_0000");
	SET_ORDER (10);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("hexadecimal_max", "0xFFFF_FFFF_FFFF_FFFF");
	SET_ORDER (11);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("18446744073709551615");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("hexadecimal_overflow", "0x1_0000_0000_0000_0000");
	SET_ORDER (12);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");

	TEST_WR_FOOT;
}

static void testWriteReadFloat (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("float1", "+0.3");
	DUP_EXPECTED;
	SET_ORDER (0);
	SET_TYPE ("double");

	WRITE_KV ("float2", "-7.1_313E+1_0");
	DUP_EXPECTED;
	SET_ORDER (1);
	VALUE_TO_ORIG_NEW_VALUE ("-7.1313E+10");
	SET_TYPE ("double");

	WRITE_KV ("float3", "+2e-3");
	DUP_EXPECTED;
	SET_ORDER (2);
	SET_TYPE ("double");

	WRITE_KV ("float4", "+20_0.003");
	SET_TYPE ("double");
	DUP_EXPECTED;
	SET_ORDER (3);
	VALUE_TO_ORIG_NEW_VALUE ("+200.003");
	SET_TYPE ("double");

	WRITE_KV ("float5", "+2e-3");
	DUP_EXPECTED;
	SET_ORDER (4);
	SET_TYPE ("double");

	WRITE_KV ("float6", "-2e-3");
	DUP_EXPECTED;
	SET_ORDER (5);
	SET_TYPE ("double");

	WRITE_KV ("float7", "nan");
	DUP_EXPECTED;
	SET_ORDER (6);
	SET_TYPE ("double");

	WRITE_KV ("float8", "+nan");
	DUP_EXPECTED;
	SET_ORDER (7);
	SET_TYPE ("double");

	WRITE_KV ("float9", "-inf");
	DUP_EXPECTED;
	SET_ORDER (8);
	SET_TYPE ("double");

	WRITE_KV ("float91", "-2e-003");
	DUP_EXPECTED;
	SET_ORDER (9);
	SET_TYPE ("double");

	WRITE_KV ("float92", "+2E+030");
	DUP_EXPECTED;
	SET_ORDER (10);
	SET_TYPE ("double");


	TEST_WR_FOOT;
}

static void testWriteReadDate (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("date1", "2000-12-31T10:00:00Z");
	DUP_EXPECTED;
	SET_ORDER (0);

	WRITE_KV ("date2", "1990-12-31T23:59:60Z");
	DUP_EXPECTED;
	SET_ORDER (1);

	WRITE_KV ("date3", "1937-01-01T12:00:27.87+00:20");
	DUP_EXPECTED;
	SET_ORDER (2);

	WRITE_KV ("date4", "23:59:59.99999");
	DUP_EXPECTED;
	SET_ORDER (3);

	WRITE_KV ("date5", "00:00:00");
	DUP_EXPECTED;
	SET_ORDER (4);

	TEST_WR_FOOT;
}

static void testWriteReadBoolean (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("bool1", "1");
	SET_ORDER (0);
	SET_TYPE ("boolean");
	DUP_EXPECTED;

	WRITE_KV ("bool2", "0");
	SET_ORDER (1);
	SET_TYPE ("boolean");
	DUP_EXPECTED;

	WRITE_KV ("bool3", "true");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_STRING_TYPE ("string_basic");

	WRITE_KV ("bool4", "false");
	SET_ORDER (3);
	SET_STRING_TYPE ("string_literal");
	DUP_EXPECTED;

	WRITE_KV ("bool5", "0");
	SET_ORDER (4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("bool6", "1");
	SET_ORDER (5);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadCheckSparseHierarchy (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("a", "0");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("a/b/c/d", "1");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadComments (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("a", "0");
	SET_ORDER (0);
	SET_EMPTY_LINE (1);
	SET_EMPTY_LINE (2);
	SET_COMMENT (3, "test comment 1", "    ");
	SET_COMMENT (4, "test comment 2", "");
	SET_INLINE_COMMENT ("inline test", "    ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("b", "1");
	SET_ORDER (1);
	SET_EMPTY_LINE (1);
	SET_COMMENT (2, "test comment 3", "    ");
	SET_EMPTY_LINE (3);
	SET_COMMENT (4, "test comment 4", "");
	SET_EMPTY_LINE (5);
	SET_INLINE_COMMENT ("inline test 1", "    ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("c", "2");
	SET_ORDER (2);
	SET_EMPTY_LINE (1);
	SET_COMMENT (2, "", "    ");
	SET_EMPTY_LINE (3);
	SET_COMMENT (4, "", "");
	SET_EMPTY_LINE (5);
	SET_INLINE_COMMENT ("", " ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("table");
	SET_TOML_TYPE ("simpletable");
	SET_ORDER (3);
	SET_EMPTY_LINE (1);
	SET_COMMENT (2, "test comment 5", "    ");
	SET_EMPTY_LINE (3);
	SET_COMMENT (4, "test comment 6", "");
	SET_INLINE_COMMENT ("inline test 3", "\t");
	DUP_EXPECTED;

	TEST_WR_FOOT;
}

static void testWriteReadCommentsArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("array");
	SET_ORDER (0);
	SET_ARRAY ("#3");
	SET_INLINE_COMMENT ("array inline comment", " ");
	DUP_EXPECTED;

	WRITE_KV ("array/#0", "0");
	SET_COMMENT (1, "element 1 comment", "\t");
	SET_INLINE_COMMENT ("element 1 inline", "    ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#1", "1");
	SET_COMMENT (1, "element 2 comment", "  ");
	SET_INLINE_COMMENT ("element 2 inline", "  ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#2", "2");
	SET_COMMENT (1, " element 3 comment", "  ");
	DUP_EXPECTED;
	SET_EMPTY_LINE (0); // This newline is because the next array element has a comment in front of it
	SET_TYPE ("long_long");


	WRITE_KV ("array/#3", "3");
	SET_COMMENT (1, "element 4 comment", "  ");
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testRoundtrip (const char * filePath)
{
	char fileOut[512];
	snprintf (fileOut, 512, "%s.out", filePath);
	bool roundtripSuccess = roundtripFile (filePath, fileOut);
	succeed_if (roundtripSuccess, "Could not roundtrip file!");
	if (roundtripSuccess)
	{
		succeed_if (compareFilesIgnoreWhitespace (srcdir_file (filePath), srcdir_file (fileOut)),
			    "Roundtripped files do not match!");
	}
	remove (fileOut);
}

static KeySet * readFile (const char * filename)
{
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);

	int getStatus = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (getStatus == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not read keys");

	if (getStatus != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		output_error (parentKey);
		ksDel (ks);
		ks = NULL;
	}
	PLUGIN_CLOSE ();
	keyDel (parentKey);
	return ks;
}

static bool writeFile (const char * filename, KeySet * ksWrite, int pluginStatus)
{
	bool success = true;
	ELEKTRA_LOG_DEBUG ("Writing '%s'\n", filename);
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (filename), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");

	int setStatus = plugin->kdbSet (plugin, ksWrite, parentKey);
	succeed_if (setStatus == pluginStatus, "Writing keys did not return with expected Status");
	if (setStatus != pluginStatus)
	{
		output_error (parentKey);
		success = false;
	}
	PLUGIN_CLOSE ();
	keyDel (parentKey);
	return success;
}

static void testWriteReadCompare (KeySet * ksWrite, KeySet * expected)
{
	const char * filename = "test_write_read.toml";

	if (writeFile (filename, ksWrite, ELEKTRA_PLUGIN_STATUS_SUCCESS))
	{
		{
			KeySet * ksRead = readFile (filename);
			if (ksRead != NULL)
			{
				compare_keyset (expected, ksRead);
				ksDel (ksRead);
			}
		}
	}
	remove (filename);
}

static bool roundtripFile (const char * filenameIn, const char * filenameOut)
{
	KeySet * ksRead = readFile (filenameIn);
	if (ksRead == NULL)
	{
		return false;
	}
	bool success = writeFile (filenameOut, ksRead, ELEKTRA_PLUGIN_STATUS_SUCCESS);
	ksDel (ksRead);
	return success;
}

static void testReadCompare (const char * filename, KeySet * expected)
{
	printf ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);

	int getStatus = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (getStatus == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not read keys");

	if (getStatus != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		output_error (parentKey);
	}
	else
	{
		compare_keyset (expected, ks);
		/*printf("EXPECTED:\n");
		output_keyset(expected);
		printf("FOUND:\n");
		output_keyset(ks);*/
	}

	ksDel (ks);
	PLUGIN_CLOSE ();
	keyDel (parentKey);
	ksDel (expected);
}

static void testReadMustError (const char * filename)
{
	printf ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "Expected kdbGet to fail, but got success.");

	ksDel (ks);
	PLUGIN_CLOSE ();
	keyDel (parentKey);
}

static Key * addKey (KeySet * ks, const char * name, const char * value, size_t size, const char * orig, const char * type,
		     const char * array, const char * tomltype, int order)
{
	Key * key = keyNew (prefix, KEY_END);
	if (name != NULL)
	{
		keyAddName (key, name);
	}
	if (value != NULL)
	{
		if (size == 0)
		{
			keySetString (key, value);
		}
		else
		{
			keySetBinary (key, (const void *) value, size);
		}
	}
	else if (tomltype == NULL && array == NULL)
	{
		keySetBinary (key, NULL, 0);
	}
	if (orig != NULL)
	{
		keySetMeta (key, "origvalue", orig);
	}
	if (type != NULL)
	{
		keySetMeta (key, "type", type);
	}
	if (array != NULL)
	{
		keySetMeta (key, "array", array);
	}
	if (tomltype != NULL)
	{
		keySetMeta (key, "tomltype", tomltype);
	}
	if (order != -1)
	{
		setOrderForKey (key, order);
	}
	ksAppendKey (ks, key);
	return key;
}

static void setComment (Key * key, const char * comment, const char * space, size_t index)
{
	char commentBase[64];
	char commentKey[128];
	char * indexStr = indexToArrayString (index);
	snprintf (commentBase, 64, "comment/%s", indexStr);
	elektraFree (indexStr);
	if (comment != NULL)
	{
		keySetMeta (key, commentBase, comment);
		snprintf (commentKey, 128, "%s/start", commentBase);
		keySetMeta (key, commentKey, "#");
	}
	else
	{
		snprintf (commentKey, 128, "%s/start", commentBase);
		keySetMeta (key, commentKey, "");
	}
	snprintf (commentKey, 128, "%s/space", commentBase);
	keySetMeta (key, commentKey, space);
}

static bool compareFilesIgnoreWhitespace (const char * filenameA, const char * filenameB)
{
#define LINE_SIZE 512
	const char * WHITELIST_CHARS = " \t\"'";
	FILE * fA = fopen (filenameA, "r");
	FILE * fB = fopen (filenameB, "r");
	char lineA[LINE_SIZE];
	char lineB[LINE_SIZE];

	size_t line = 0;

	if (fA == NULL)
	{
		printf ("Could not open file '%s'\n", filenameA);

		fclose (fB);
		return false;
	}
	if (fB == NULL)
	{
		printf ("Could not open file '%s'\n", filenameB);

		fclose (fA);
		return false;
	}

	while (1)
	{
		char * ptrA = fgets (lineA, LINE_SIZE, fA);
		char * ptrB = fgets (lineB, LINE_SIZE, fB);
		line++;
		while (1)
		{
			while (ptrA != NULL && *ptrA != 0 && strchr (WHITELIST_CHARS, (int) *ptrA) != NULL)
			{
				ptrA++;
			}
			while (ptrB != NULL && *ptrB != 0 && strchr (WHITELIST_CHARS, (int) *ptrB) != NULL)
			{
				ptrB++;
			}
			if (ptrA == NULL || ptrB == NULL)
			{
				if (ptrA == NULL && ptrB == NULL)
				{

					fclose (fA);
					fclose (fB);
					return true;
				}
				else if (ptrA != NULL)
				{
					printf ("Second file at EOF, but first file not:\n%s", ptrA);

					fclose (fA);
					fclose (fB);
					return false;
				}
				else if (ptrB != NULL)
				{
					printf ("First file at EOF, but second file not:\n%s", ptrB);

					fclose (fA);
					fclose (fB);
					return false;
				}
			}
			if (*ptrA != *ptrB)
			{
				printf ("Lines do not match at line %lu:\nfirst file:\n%ssecond file:\n%s", line, lineA, lineB);

				fclose (fA);
				fclose (fB);
				return false;
			}
			if (*ptrA == 0)
			{
				break;
			}
			ptrA++;
			ptrB++;
		}
	}

	fclose (fA);
	fclose (fB);
#undef LINE_SIZE
}
