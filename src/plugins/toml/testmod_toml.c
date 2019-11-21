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
static void testReadCompare (const char * filename, KeySet * expected);
static void testReadMustError (const char * filename);

int main (int argc, char ** argv)
{
	init (argc, argv);

	testPositiveCompareKeySets ();
	testNegativeCompareErrors ();

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
