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

#define TEST_WR_HEAD                                                                                                                       \
	Key * lastKey = NULL;                                                                                                              \
	KeySet * writeKs = ksNew (0, KS_END);                                                                                              \
	KeySet * expectedKs = ksNew (0, KS_END)
#define TEST_WR_FOOT                                                                                                                       \
	testWriteReadCompare (writeKs, expectedKs);                                                                                        \
	ksDel (expectedKs);                                                                                                                \
	ksDel (writeKs)
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
			lastKey = keyDup (lastKey);                                                                                        \
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
#define SET_ORDER(order)                                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL) setOrderForKey (lastKey, order);                                                                      \
	}
#define SET_COMMENT(index, text, spaces)                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, text, "#", spaces, index);                                                       \
	}
#define SET_INLINE_COMMENT(text, spaces)                                                                                                   \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, text, "#", spaces, 0);                                                           \
	}
#define SET_EMPTY_LINE(index)                                                                                                              \
	{                                                                                                                                  \
		if (lastKey != NULL) setComment (lastKey, NULL, "", 0, index);                                                             \
	}

#define SET_BINARY                                                                                                                         \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "binary", "");                                                                   \
	}
#define CLEAR_BINARY                                                                                                                       \
	{                                                                                                                                  \
		if (lastKey != NULL) keySetMeta (lastKey, "binary", NULL);                                                                 \
	}

static void testRead (void);
static void testWriteRead (void);
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
static void testWriteReadIntegerOtherBase (void);
static void testWriteReadFloat (void);
static void testWriteReadDate (void);
static void testWriteReadBoolean (void);
static void testWriteReadCheckSparseHierarchy (void);
static void testWriteReadComments (void);
static void testWriteReadCommentsArray (void);
static void testWriteReadOrderTableNonTable (void);
static void testWriteReadNull (void);
// static void testWriteReadBase64(void);
static void printError (Key * parent);
static Key * addKey (KeySet * ks, const char * name, const char * value, size_t size, const char * orig, const char * type,
		     const char * array, const char * tomltype, int order);
static void setComment (Key * key, const char * comment, const char * start, size_t spaces, size_t index);

int main (int argc, char ** argv)
{
	init (argc, argv);

	testRead ();
	testWriteRead ();

	print_result ("testmod_toml");
	return nbError;
}

static void testRead (void)
{
	testReadCompare ("toml/basic.toml",
#include "toml/basic.h"
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
}

static void testWriteRead (void)
{
	testWriteReadEmptyKeyName ();
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
	testWriteReadIntegerOtherBase ();
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
}

static void testWriteReadEmptyKeyName (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("%", "discouraged, but valid");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("string");

	WRITE_KV ("%/%", "also discouraged, but valid");
	SET_ORDER (1);
	DUP_EXPECTED;
	SET_TYPE ("string");


	TEST_WR_FOOT;
}

static void testWriteReadNull (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("null_valued_key", NULL);
	CLEAR_BINARY;
	SET_ORDER (0);

	DUP_EXPECTED;

	TEST_WR_FOOT;
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
	SET_TYPE ("string");

	WRITE_KV ("c", "3.1415");
	SET_ORDER (2);
	DUP_EXPECTED;
	SET_TYPE ("double");

	WRITE_KV ("3/14", "PI");
	SET_ORDER (3);
	DUP_EXPECTED
	SET_TYPE ("string");

	TEST_WR_FOOT;
}

static void testWriteReadString (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("multiline1", "first line\nsecond line");
	SET_ORDER (0);
	DUP_EXPECTED;
	SET_TYPE ("string");

	WRITE_KV ("withescapechars", "first line\\nsecond line\\r");
	SET_ORDER (1);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("first line\nsecond line\r");
	SET_TYPE ("string");

	WRITE_KV ("numberstring01", "1337");
	SET_ORDER (2);
	SET_TYPE ("string");
	DUP_EXPECTED;

	WRITE_KV ("numberstring02", "13_37");
	SET_ORDER (3);
	SET_TYPE ("string");
	DUP_EXPECTED;

	WRITE_KV ("numberstring03", "+3e-7");
	SET_ORDER (4);
	SET_TYPE ("string");
	DUP_EXPECTED;

	WRITE_KV ("datestring", "2000-01-01");
	SET_ORDER (5);
	SET_TYPE ("string");
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
	SET_COMMENT (0, " inline comment", 4);
	SET_COMMENT (1, " top-most preceding comment", 0);
	SET_COMMENT (2, " preceding comment", 0);
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

	WRITE_KV ("int1", "+1337");
	DUP_EXPECTED;
	SET_ORDER (0);
	SET_TYPE ("long_long");

	WRITE_KV ("int2", "-666");
	DUP_EXPECTED;
	SET_ORDER (1);
	SET_TYPE ("long_long");

	WRITE_KV ("int3", "0");
	DUP_EXPECTED;
	SET_ORDER (2);
	SET_TYPE ("long_long");

	WRITE_KV ("int4", "3000");
	DUP_EXPECTED;
	SET_ORDER (3);
	SET_TYPE ("long_long");

	WRITE_KV ("int5", "+1_999_000");
	DUP_EXPECTED;
	SET_ORDER (4);
	VALUE_TO_ORIG_NEW_VALUE ("+1999000");
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadIntegerOtherBase (void)
{
	TEST_WR_HEAD;

	WRITE_KV ("hex1", "0xA_Baf00");
	SET_ORDER (0);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("11251456");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("hex2", "0x00_1");
	SET_ORDER (1);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("1");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("hex3", "0x0_0");
	SET_ORDER (2);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("oct1", "0o13_37");
	SET_ORDER (3);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("735");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("oct2", "0o0_000");
	SET_ORDER (4);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("oct3", "0o1_3_3_7");
	SET_ORDER (5);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("735");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("bin1", "0b0_0_0_0");
	SET_ORDER (6);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("bin2", "0b100_0");
	SET_ORDER (7);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("8");
	SET_TYPE ("unsigned_long_long");

	WRITE_KV ("bin3", "0b000");
	SET_ORDER (8);
	DUP_EXPECTED;
	VALUE_TO_ORIG_NEW_VALUE ("0");
	SET_TYPE ("unsigned_long_long");

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
	SET_TYPE ("string");

	WRITE_KV ("bool4", "false");
	SET_ORDER (3);
	DUP_EXPECTED;
	SET_TYPE ("string");

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
	SET_COMMENT (3, "test comment 1", 4);
	SET_COMMENT (4, "test comment 2", 0);
	SET_INLINE_COMMENT ("inline test", 4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("b", "1");
	SET_ORDER (1);
	SET_EMPTY_LINE (1);
	SET_COMMENT (2, "test comment 3", 4);
	SET_EMPTY_LINE (3);
	SET_COMMENT (4, "test comment 4", 0);
	SET_EMPTY_LINE (5);
	SET_INLINE_COMMENT ("inline test 1", 4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KEY ("table");
	SET_TOML_TYPE ("simpletable");
	SET_ORDER (2);
	SET_EMPTY_LINE (1);
	SET_COMMENT (2, "test comment 5", 4);
	SET_EMPTY_LINE (3);
	SET_COMMENT (4, "test comment 6", 0);
	SET_INLINE_COMMENT ("inline test 3", 4);
	DUP_EXPECTED;

	TEST_WR_FOOT;
}

static void testWriteReadCommentsArray (void)
{
	TEST_WR_HEAD;

	WRITE_KEY ("array");
	SET_ORDER (0);
	SET_ARRAY ("#2");
	SET_INLINE_COMMENT ("array inline comment", 1);
	DUP_EXPECTED;

	WRITE_KV ("array/#0", "0");
	SET_COMMENT (1, "element 1 comment", 4);
	SET_INLINE_COMMENT ("element 1 inline", 4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#1", "1");
	SET_COMMENT (1, "element 2 comment", 4);
	SET_INLINE_COMMENT ("element 2 inline", 4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	WRITE_KV ("array/#2", "2");
	SET_COMMENT (1, "element 3 comment", 4);
	SET_INLINE_COMMENT ("element 3 inline", 4);
	DUP_EXPECTED;
	SET_TYPE ("long_long");

	TEST_WR_FOOT;
}

static void testWriteReadCompare (KeySet * ksWrite, KeySet * expected)
{
	const char * filename = "test_write_read.toml";
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");

	int setStatus = plugin->kdbSet (plugin, ksWrite, parentKey);
	succeed_if (setStatus == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not write keys");
	if (setStatus != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		printError (parentKey);
	}
	else
	{
		KeySet * ksRead = ksNew (0, KS_END);
		int getStatus = plugin->kdbGet (plugin, ksRead, parentKey);
		succeed_if (getStatus == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not read written keys");
		if (getStatus != ELEKTRA_PLUGIN_STATUS_SUCCESS)
		{
			printError (parentKey);
		}
		else
		{
			compare_keyset (expected, ksRead);
			/*printf ("EXPECTED:\n");
			dumpKS (expected);
			printf ("READ:\n");
			dumpKS (ksRead);*/
		}
		ksDel (ksRead);
	}

	PLUGIN_CLOSE ();
	keyDel (parentKey);
	remove (filename);
}

static void testReadCompare (const char * filename, KeySet * expected)
{
	ELEKTRA_LOG_DEBUG ("Reading '%s'\n", filename);
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);

	int getStatus = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (getStatus == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not read keys");

	if (getStatus != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		printError (parentKey);
	}
	else
	{
		compare_keyset (expected, ks);
		/*printf("EXPECTED:\n");
		dumpKS(expected);
		printf("FOUND:\n");
		dumpKS(ks);*/
	}

	ksDel (ks);
	PLUGIN_CLOSE ();
	keyDel (parentKey);
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
	keyDel (parentKey);
}

static void printError (Key * parent)
{
	const Key * meta = keyGetMeta (parent, "error/reason");
	if (meta != NULL)
	{
		fprintf (stderr, "ERROR: %s\n", keyString (meta));
	}
}

static Key * addKey (KeySet * ks, const char * name, const char * value, size_t size, const char * orig, const char * type,
		     const char * array, const char * tomltype, int order)
{
	Key * key = keyNew (PREFIX "/", KEY_END);
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

static void setComment (Key * key, const char * comment, const char * start, size_t spaces, size_t index)
{
	char commentBase[64];
	char commentKey[128];
	char * indexStr = indexToArrayString (index);
	snprintf (commentBase, 64, "comment/%s", indexStr);
	elektraFree (indexStr);
	if (comment != NULL)
	{
		keySetMeta (key, commentBase, comment);
	}
	snprintf (commentKey, 128, "%s/start", commentBase);
	keySetMeta (key, commentKey, start);

	snprintf (commentKey, 128, "%s/space", commentBase);
	setPlainIntMeta (key, commentKey, spaces);
}
