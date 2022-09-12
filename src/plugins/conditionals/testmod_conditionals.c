/**
 * @file
 *
 * @brief Tests for conditionals plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_ifthenelseint (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(../totest== '153') ? (../bla/val1 == '100') : (../bla/val2 == '100')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthenint (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(     ../totest   !=     '15'  ) ? (../bla/val1 == '100')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthenltint (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(     ./   <    '153'    ) ? (../bla/val1 == '100')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthengtint (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(./>'153') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_ifthenkey (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(../totest>@/bla/val3) ? (../bla/val1 == '100') :  (../bla/val2 <= '1')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_emptyisempty (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/condition",
				     "(../totest=='') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_notempty (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "153", ELEKTRA_KEY_META, "check/condition",
				     "(../totest!='') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_ifsetthenval (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/condition",
				     "(../totest=='') ? (../totest := 'BLA')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "BLA") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifsetthenkey (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/condition",
				     "(./=='') ? (./ := ../bla/val1)", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "100") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignThen (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition",
				     "(../totest=='Hello') ? ('World')", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "World") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignThen2 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition",
				     "(../totest=='Hello') ? ('World') : ('Fail')", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "World") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignElse (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition",
				     "(../totest=='Hell') ? ('World') : ('Fail')", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "Fail") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_assignKeyThen (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition",
				     "(../totest=='Hello') ? (../then/key)", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/then/key", ELEKTRA_KEY_VALUE, "World", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/elseVal", ELEKTRA_KEY_VALUE, "Fail", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "World") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_assignKeyElse (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition",
				     "(../totest=='Hell') ? (../then/key) : (../elseVal)", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/then/key", ELEKTRA_KEY_VALUE, "World", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/elseVal", ELEKTRA_KEY_VALUE, "Fail", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "Fail") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doesntExistSuccess (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? (! ../bla/val4)", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doesntExistFail (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? (! ../bla/val1)", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested1Success (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/result == 'result1') || (../bla/result == 'result3'))", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested1Fail (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/result == 'result1') || (../bla/result == 'result2'))", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_nested2Success (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/val1 == '100') && ((../bla/result == 'result1') || "
				     "(../bla/result == 'result3')))",
				     ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested2Fail (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/val1 == '100') && ((../bla/result == 'result1') || "
				     "(../bla/result == 'result2')))",
				     ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_suffix (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "2%", ELEKTRA_KEY_META, "check/condition",
				     "(../totest >= '10%') ? (../bla/val1 == '50%') : (../bla/val3 == '3%')", ELEKTRA_KEY_META,
				     "condition/validsuffix", "'%', '$'", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100%", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50%", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3%", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50'): (../bla/val3 == '3')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace2 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50') :(../bla/val3 == '3')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace3 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50'):(../bla/val3 == '3')", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doubleUp (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks =
		elektraKeysetNew (5,
		       elektraKeyNew ("user:/tests/conditionals/bla/totest", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_META, "check/condition",
			       "(../../bla/totest<../../bla/val1) ? ((../../bla/val1 == '100') && ((../../bla/result == 'result1') || "
			       "(../../bla/result == 'result2')))",
			       ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/tests/conditionals/bla/val1", ELEKTRA_KEY_VALUE, "100", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/tests/conditionals/bla/val2", ELEKTRA_KEY_VALUE, "50", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/tests/conditionals/bla/val3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/tests/conditionals/bla/result", ELEKTRA_KEY_VALUE, "result3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_multiCondAny (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Sun", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "check/condition/any", "#1", ELEKTRA_KEY_META,
				     "check/condition/any/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/any/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2Any (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Moon", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Bye", ELEKTRA_KEY_META, "check/condition/any", "#1", ELEKTRA_KEY_META,
				     "check/condition/any/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/any/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCondAll (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Sun", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "check/condition/all", "#1", ELEKTRA_KEY_META,
				     "check/condition/all/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/all/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2All (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Moon", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Bye", ELEKTRA_KEY_META, "check/condition/all", "#1", ELEKTRA_KEY_META,
				     "check/condition/all/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/all/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCondNoFail (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Sun", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "check/condition/none", "#1",
				     ELEKTRA_KEY_META, "check/condition/none/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/none/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2NoFail (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/conditionals/compare", ELEKTRA_KEY_VALUE, "Moon", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Bye", ELEKTRA_KEY_META, "check/condition/none", "#1", ELEKTRA_KEY_META,
				     "check/condition/none/#0", "(../totest=='Bye') ? (../compare == 'Moon')", ELEKTRA_KEY_META,
				     "check/condition/none/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_multiAssign (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_META, "assign/condition", "#1", ELEKTRA_KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", ELEKTRA_KEY_META, "assign/condition/#1",
				     "(../totest=='Hello') ? ('Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "Sun") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiAssign2 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Bye", ELEKTRA_KEY_META, "assign/condition", "#1", ELEKTRA_KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", ELEKTRA_KEY_META, "assign/condition/#1",
				     "(../totest=='Hello') ? ('Sun') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "Moon") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiAssign3 (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/conditionals", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5,
			     elektraKeyNew ("user:/tests/conditionals/totest", ELEKTRA_KEY_VALUE, "Bye", ELEKTRA_KEY_META, "assign/condition", "#1", ELEKTRA_KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", ELEKTRA_KEY_META, "assign/condition/#1",
				     "(../totest=='Bye') ? ('FAIL') ", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("conditionals");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (elektraKeyString (key), "Moon") == 0, "error setting then value");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("CONDITIONALS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ifthenelseint ();
	test_ifthenint ();
	test_ifthenltint ();
	test_ifthengtint ();
	test_ifthenkey ();
	test_emptyisempty ();
	test_notempty ();
	test_ifsetthenval ();
	test_ifsetthenkey ();
	test_assignThen ();
	test_assignThen2 ();
	test_assignElse ();
	test_assignKeyThen ();
	test_assignKeyElse ();
	test_nested1Success ();
	test_nested1Fail ();
	test_nested2Success ();
	test_nested2Fail ();
	test_doesntExistSuccess ();
	test_doesntExistFail ();
	test_suffix ();
	test_elseWhitespace ();
	test_elseWhitespace2 ();
	test_elseWhitespace3 ();
	test_doubleUp ();
	test_multiCondAny ();
	test_multiCondAll ();
	test_multiCondNoFail ();
	test_multiAssign ();
	test_multiCond2Any ();
	test_multiCond2All ();
	test_multiCond2NoFail ();
	test_multiAssign2 ();
	test_multiAssign3 ();
	print_result ("testmod_conditionals");

	return nbError;
}
