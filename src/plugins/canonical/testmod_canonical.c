/**
 * @file
 *
 * @brief Tests for canonical plugin
 *
 * @copyright BSD License (see LICENSE.md or https:/www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void testSensitiveListSingle ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (
		20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical",
			    "list:  'TRUE' , 'true'  ,    'ON' ,'ENABLE'  , 'enable' ", KEY_META, "transform/canonical/set", "1", KEY_END),
		keyNew ("user/tests/canonical/match2", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical",
			"list:'TRUE' ,  'true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),
		keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "TRue", KEY_META, "transform/canonical",
			"list:'TRUE'  , 'true',  'ON' ,  'ENABLE' , 'enable' ", KEY_META, "transform/canonical/set", "1", KEY_END),
		keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "ENAble", KEY_META, "transform/canonical",
			"list:'TRUE','true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),

		KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1"), "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1"), "kdbGet failed on match2");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "TRue"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch2");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testInsensitiveListSingle ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks =
		ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical",
				   "caselist:'TRUE','true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),
		       keyNew ("user/tests/canonical/match2", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical",
			       "caselist:'TRUE','true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),
		       keyNew ("user/tests/canonical/match3", KEY_VALUE, "TRue", KEY_META, "transform/canonical",
			       "caselist:'TRUE','true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),
		       keyNew ("user/tests/canonical/match4", KEY_VALUE, "ENAble", KEY_META, "transform/canonical",
			       "caselist:'TRUE','true','ON','ENABLE','enable'", KEY_META, "transform/canonical/set", "1", KEY_END),

		       KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1"), "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1"), "kdbGet failed on match2");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "1"), "kdbGet failed on match3");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match4", KDB_O_NONE)), "1"), "kdbGet failed on match4");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void testSensitiveListArray ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (
		20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical", "#2", KEY_META,
			    "transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			    "transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			    "transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/match2", KEY_VALUE, "ON", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		keyNew ("user/tests/canonical/match3", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "On", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		keyNew ("user/tests/canonical/nomatch3", KEY_VALUE, "ENAble", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "list:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "list:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "list:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1true"),
		    "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1on"), "kdbGet failed on match2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "1enable"),
		    "kdbGet failed on match3");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "TRue"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "On"),
		    "kdbGet failed on nomatch2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch3", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testInsensitiveListArray ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (
		20,
		keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/match2", KEY_VALUE, "ON", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		keyNew ("user/tests/canonical/match3", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/match4", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		keyNew ("user/tests/canonical/match5", KEY_VALUE, "On", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		keyNew ("user/tests/canonical/match6", KEY_VALUE, "ENAble", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'TRUE','true'", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			"transform/canonical/#1", "caselist:'ON'", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			"transform/canonical/#2", "caselist:'enable','ENABLE'", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1true"),
		    "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1on"), "kdbGet failed on match2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "1enable"),
		    "kdbGet failed on match3");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match4", KDB_O_NONE)), "1true"),
		    "kdbGet failed on match4");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match5", KDB_O_NONE)), "1on"), "kdbGet failed on match5");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match6", KDB_O_NONE)), "1enable"),
		    "kdbGet failed on match6");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testRegexSingle ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical",
					 "regex:TR[uU][eE]", KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical",
				     "regex:TR[uU][eE]", KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/match2", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "regex:TR[uU][eE]",
				     KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "ENAble", KEY_META, "transform/canonical",
				     "regex:TR[uU][eE]", KEY_META, "transform/canonical/set", "1", KEY_END),

			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1"), "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1"), "kdbGet failed on match2");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "ENABLE"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch2");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testRegexArray ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks =
		ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical", "#2", KEY_META,
				   "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
				   "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
				   "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/match2", KEY_VALUE, "ON", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/match3", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		       keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "on", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/nomatch3", KEY_VALUE, "ENAble", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "regex:(TRUE|true)", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "ON", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "regex:.*ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1true"),
		    "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1on"), "kdbGet failed on match2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "1enable"),
		    "kdbGet failed on match3");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "TRue"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "on"),
		    "kdbGet failed on nomatch2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch3", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void testFNMatchSingle ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical", "fnm:TR??",
					 KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical", "fnm:TR??",
				     KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/match2", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "fnm:TR??",
				     KEY_META, "transform/canonical/set", "1", KEY_END),
			     keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "ENAble", KEY_META, "transform/canonical", "fnm:TR??",
				     KEY_META, "transform/canonical/set", "1", KEY_END),

			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1"), "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1"), "kdbGet failed on match2");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "ENABLE"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch2");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testFNMatchArray ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks =
		ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "TRUE", KEY_META, "transform/canonical", "#2", KEY_META,
				   "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
				   "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
				   "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/match2", KEY_VALUE, "ON", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/match3", KEY_VALUE, "ENABLE", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),

		       keyNew ("user/tests/canonical/nomatch1", KEY_VALUE, "TRue", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/nomatch2", KEY_VALUE, "on", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       keyNew ("user/tests/canonical/nomatch3", KEY_VALUE, "ENAble", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "fnm:*UE", KEY_META, "transform/canonical/#0/set", "1true", KEY_META,
			       "transform/canonical/#1", "fnm:?N", KEY_META, "transform/canonical/#1/set", "1on", KEY_META,
			       "transform/canonical/#2", "fnm:??ABLE", KEY_META, "transform/canonical/#2/set", "1enable", KEY_END),
		       KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "1true"),
		    "kdbGet failed on match1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1on"), "kdbGet failed on match2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "1enable"),
		    "kdbGet failed on match3");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch1", KDB_O_NONE)), "TRue"),
		    "kdbGet failed on nomatch1");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch2", KDB_O_NONE)), "on"),
		    "kdbGet failed on nomatch2");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/nomatch3", KDB_O_NONE)), "ENAble"),
		    "kdbGet failed on nomatch3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void testTristateList ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks =
		ksNew (20, keyNew ("user/tests/canonical/TTrue", KEY_VALUE, "On", KEY_META, "transform/canonical", "#2", KEY_META,
				   "transform/canonical/#0", "caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#0/set",
				   "1", KEY_META, "transform/canonical/#1", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META,
				   "transform/canonical/#1/set", "0", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''",
				   KEY_META, "transform/canonical/#2/set", "2", KEY_END),
		       keyNew ("user/tests/canonical/TFalse", KEY_VALUE, "Disable", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#0/set",
			       "1", KEY_META, "transform/canonical/#1", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META,
			       "transform/canonical/#1/set", "0", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''",
			       KEY_META, "transform/canonical/#2/set", "2", KEY_END),
		       keyNew ("user/tests/canonical/TOther", KEY_VALUE, "", KEY_META, "transform/canonical", "#2", KEY_META,
			       "transform/canonical/#0", "caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#0/set",
			       "1", KEY_META, "transform/canonical/#1", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META,
			       "transform/canonical/#1/set", "0", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''",
			       KEY_META, "transform/canonical/#2/set", "2", KEY_END),

		       KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TTrue", KDB_O_NONE)), "1"), "kdbGet failed on TTrue");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TFalse", KDB_O_NONE)), "0"), "kdbGet failed on TFalse");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TOther", KDB_O_NONE)), "2"), "kdbGet failed on match3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testTristateListToIndex ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (
		20,
		keyNew ("user/tests/canonical/TTrue", KEY_VALUE, "On", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META, "transform/canonical/#1",
			"caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''", KEY_END),
		keyNew ("user/tests/canonical/TFalse", KEY_VALUE, "Disable", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META, "transform/canonical/#1",
			"caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''", KEY_END),
		keyNew ("user/tests/canonical/TOther", KEY_VALUE, "Other", KEY_META, "transform/canonical", "#2", KEY_META,
			"transform/canonical/#0", "caselist:'FALSE','OFF','CLOSED','DISABLE'", KEY_META, "transform/canonical/#1",
			"caselist:'TRUE','ON','OPEN','ENABLE'", KEY_META, "transform/canonical/#2", "caselist:'OTHER','NONE',''", KEY_END),


		KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TTrue", KDB_O_NONE)), "1"), "kdbGet failed on TTrue");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TFalse", KDB_O_NONE)), "0"), "kdbGet failed on TFalse");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/TOther", KDB_O_NONE)), "2"), "kdbGet failed on match3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testMixedArray ()
{
	Key * parentKey = keyNew ("user/tests/canonical", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (20, keyNew ("user/tests/canonical/match1", KEY_VALUE, "ABC", KEY_META, "transform/canonical", "#2", KEY_META,
					 "transform/canonical/#0", "regex:[a-zA-Z]", KEY_META, "transform/canonical/#1", "fnm:??3",
					 KEY_META, "transform/canonical/#2", "list:'+++','---','==='", KEY_END),
			     keyNew ("user/tests/canonical/match2", KEY_VALUE, "123", KEY_META, "transform/canonical", "#2", KEY_META,
				     "transform/canonical/#0", "regex:[a-zA-Z]", KEY_META, "transform/canonical/#1", "fnm:??3", KEY_META,
				     "transform/canonical/#2", "list:'+++','---','==='", KEY_END),
			     keyNew ("user/tests/canonical/match3", KEY_VALUE, "===", KEY_META, "transform/canonical", "#2", KEY_META,
				     "transform/canonical/#0", "regex:[a-zA-Z]", KEY_META, "transform/canonical/#1", "fnm:??3", KEY_META,
				     "transform/canonical/#2", "list:'+++','---','==='", KEY_END),
			     KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("canonical");

	ksRewind (ks);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != (-1), "kdbGet failed");

	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match1", KDB_O_NONE)), "0"), "kdbGet failed on TTrue");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match2", KDB_O_NONE)), "1"), "kdbGet failed on TFalse");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user/tests/canonical/match3", KDB_O_NONE)), "2"), "kdbGet failed on match3");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("CANONICAL     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testSensitiveListSingle ();
	testInsensitiveListSingle ();
	testSensitiveListArray ();
	testInsensitiveListArray ();
	testRegexSingle ();
	testRegexArray ();
	testFNMatchSingle ();
	testFNMatchArray ();
	testTristateList ();
	testTristateListToIndex ();
	testMixedArray ();
	printf ("\ntestmod_canonical RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
