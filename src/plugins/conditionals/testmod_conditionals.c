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

#include <internal/kdb/config.h>

#include <tests_plugin.h>

static void test_ifthenelseint (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(../totest== '153') ? (../bla/val1 == '100') : (../bla/val2 == '100')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthenint (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(     ../totest   !=     '15'  ) ? (../bla/val1 == '100')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthenltint (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(     ./   <    '153'    ) ? (../bla/val1 == '100')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifthengtint (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(./>'153') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_ifthenkey (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(../totest>@/bla/val3) ? (../bla/val1 == '100') :  (../bla/val2 <= '1')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_emptyisempty (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition",
				     "(../totest=='') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_notempty (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "153", KEY_META, "check/condition",
				     "(../totest!='') ? (../bla/val1 == '100') : (../bla/val2 <= '1')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_ifsetthenval (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition",
				     "(../totest=='') ? (../totest := 'BLA')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "BLA") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_ifsetthenkey (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "", KEY_META, "check/condition",
				     "(./=='') ? (./ := ../bla/val1)", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "100") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignThen (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition",
				     "(../totest=='Hello') ? ('World')", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "World") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignThen2 (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition",
				     "(../totest=='Hello') ? ('World') : ('Fail')", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "World") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_assignElse (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition",
				     "(../totest=='Hell') ? ('World') : ('Fail')", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "Fail") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_assignKeyThen (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition",
				     "(../totest=='Hello') ? (../then/key)", KEY_END),
			     keyNew ("user:/tests/conditionals/then/key", KEY_VALUE, "World", KEY_END),
			     keyNew ("user:/tests/conditionals/elseVal", KEY_VALUE, "Fail", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "World") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_assignKeyElse (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition",
				     "(../totest=='Hell') ? (../then/key) : (../elseVal)", KEY_END),
			     keyNew ("user:/tests/conditionals/then/key", KEY_VALUE, "World", KEY_END),
			     keyNew ("user:/tests/conditionals/elseVal", KEY_VALUE, "Fail", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "Fail") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doesntExistSuccess (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? (! ../bla/val4)", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doesntExistFail (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? (! ../bla/val1)", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested1Success (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/result == 'result1') || (../bla/result == 'result3'))", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested1Fail (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/result == 'result1') || (../bla/result == 'result2'))", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_nested2Success (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/val1 == '100') && ((../bla/result == 'result1') || "
				     "(../bla/result == 'result3')))",
				     KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_nested2Fail (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "5", KEY_META, "check/condition",
				     "(../totest<../bla/val1) ? ((../bla/val1 == '100') && ((../bla/result == 'result1') || "
				     "(../bla/result == 'result2')))",
				     KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_suffix (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "2%", KEY_META, "check/condition",
				     "(../totest >= '10%') ? (../bla/val1 == '50%') : (../bla/val3 == '3%')", KEY_META,
				     "condition/validsuffix", "'%', '$'", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100%", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50%", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3%", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "2", KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50'): (../bla/val3 == '3')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace2 (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "2", KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50') :(../bla/val3 == '3')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_elseWhitespace3 (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "2", KEY_META, "check/condition",
				     "(../totest >= '10') ? (../bla/val1 == '50'):(../bla/val3 == '3')", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
			     keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_doubleUp (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks =
		ksNew (5,
		       keyNew ("user:/tests/conditionals/bla/totest", KEY_VALUE, "5", KEY_META, "check/condition",
			       "(../../bla/totest<../../bla/val1) ? ((../../bla/val1 == '100') && ((../../bla/result == 'result1') || "
			       "(../../bla/result == 'result2')))",
			       KEY_END),
		       keyNew ("user:/tests/conditionals/bla/val1", KEY_VALUE, "100", KEY_END),
		       keyNew ("user:/tests/conditionals/bla/val2", KEY_VALUE, "50", KEY_END),
		       keyNew ("user:/tests/conditionals/bla/val3", KEY_VALUE, "3", KEY_END),
		       keyNew ("user:/tests/conditionals/bla/result", KEY_VALUE, "result3", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_multiCondAny (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Sun", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "check/condition/any", "#1", KEY_META,
				     "check/condition/any/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/any/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2Any (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Moon", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Bye", KEY_META, "check/condition/any", "#1", KEY_META,
				     "check/condition/any/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/any/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCondAll (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Sun", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "check/condition/all", "#1", KEY_META,
				     "check/condition/all/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/all/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2All (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Moon", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Bye", KEY_META, "check/condition/all", "#1", KEY_META,
				     "check/condition/all/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/all/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCondNoFail (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Sun", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "check/condition/none", "#1",
				     KEY_META, "check/condition/none/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/none/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiCond2NoFail (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/tests/conditionals/compare", KEY_VALUE, "Moon", KEY_END),
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Bye", KEY_META, "check/condition/none", "#1", KEY_META,
				     "check/condition/none/#0", "(../totest=='Bye') ? (../compare == 'Moon')", KEY_META,
				     "check/condition/none/#1", "(../totest=='Hello') ? (../compare == 'Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "error");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void test_multiAssign (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Hello", KEY_META, "assign/condition", "#1", KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", KEY_META, "assign/condition/#1",
				     "(../totest=='Hello') ? ('Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "Sun") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiAssign2 (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Bye", KEY_META, "assign/condition", "#1", KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", KEY_META, "assign/condition/#1",
				     "(../totest=='Hello') ? ('Sun') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "Moon") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_multiAssign3 (void)
{
	Key * parentKey = keyNew ("user:/tests/conditionals", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (5,
			     keyNew ("user:/tests/conditionals/totest", KEY_VALUE, "Bye", KEY_META, "assign/condition", "#1", KEY_META,
				     "assign/condition/#0", "(../totest=='Bye') ? ('Moon')", KEY_META, "assign/condition/#1",
				     "(../totest=='Bye') ? ('FAIL') ", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("conditionals");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "error");
	Key * key = ksLookupByName (ks, "user:/tests/conditionals/totest", 0);
	succeed_if (strcmp (keyString (key), "Moon") == 0, "error setting then value");
	ksDel (ks);
	keyDel (parentKey);
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
