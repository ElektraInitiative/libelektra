/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.h>

#include <kdbprivate.h>

static void test_lookupSingle (void)
{
	printf ("Test lookup single\n");

	ElektraKey * specKey = keyNew ("user:/abc", ELEKTRA_KEY_META, "override/#0", "user:/something", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = ksNew (20, k = keyNew ("user:/else", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "fallback/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta (specKey, "fallback/#0", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find override key");
	keySetMeta (specKey, "override/#0", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupChain (void)
{
	printf ("Test lookup chain\n");

	ElektraKey * specKey = keyNew ("user:/4", ELEKTRA_KEY_META, "override/#0", "user:/something", ELEKTRA_KEY_END);
	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	ElektraKeyset * ks = ksNew (20, k1 = keyNew ("user:/1", ELEKTRA_KEY_END), k2 = keyNew ("user:/2", ELEKTRA_KEY_END), k3 = keyNew ("user:/3", ELEKTRA_KEY_END),
			     k4 = keyNew ("user:/4", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#1", "user:/wrong");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#2", "user:/3");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta (specKey, "override/#1", "user:/2");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/1");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupChainLast (void)
{
	printf ("Test lookup chain last\n");

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	// clang-format off
	ElektraKeyset *ks= ksNew(20,
		k1 = keyNew("spec:/key",
			ELEKTRA_KEY_VALUE, "spec value",
			ELEKTRA_KEY_META, "override/#0", "/something",
			ELEKTRA_KEY_META, "override/#1", "/something_else",
			ELEKTRA_KEY_META, "override/#2", "/override",
			ELEKTRA_KEY_END),
		k2 = keyNew("user:/key", ELEKTRA_KEY_VALUE, "wrong user value", ELEKTRA_KEY_END),
		k3 = keyNew("dir:/key", ELEKTRA_KEY_VALUE, "wrong dir value", ELEKTRA_KEY_END),
		k4 = keyNew("user:/override", ELEKTRA_KEY_VALUE, "ok", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on

	ElektraKey * found = ksLookupByName (ks, "/key", 0);
	succeed_if (found == k4, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/override");
	succeed_if_same_string (keyString (found), "ok");

	ElektraKey * searchKey = keyNew ("/key", ELEKTRA_KEY_END);
	found = ksLookup (ks, searchKey, 0);
	succeed_if (found == k4, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/override");
	succeed_if_same_string (keyString (found), "ok");
	keyDel (searchKey);

	ksDel (ks);
}


static void test_lookupChainRealWorld (void)
{
	printf ("Test lookup chain real world\n");

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	// clang-format off
	ElektraKeyset *ks= ksNew(20,
		k1 = keyNew("spec:/sw/P/current/editor",
			ELEKTRA_KEY_META, "example", "vim",
			ELEKTRA_KEY_META, "override/#0", "/sw/P/override/editor",
			ELEKTRA_KEY_META, "override/#1", "/sw/override/editor",
			ELEKTRA_KEY_META, "override/#2", "/sw/defaults/editor",
			ELEKTRA_KEY_END),
		k2 = keyNew("user:/sw/defaults/editor", ELEKTRA_KEY_VALUE, "ok", ELEKTRA_KEY_END),
		k3 = keyNew("dir:/sw/P/current/editor", ELEKTRA_KEY_VALUE, "wrong dir value", ELEKTRA_KEY_END),
		k4 = keyNew("user:/sw/P/current/editor", ELEKTRA_KEY_VALUE, "wrong user value", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on

	ElektraKey * found = ksLookupByName (ks, "/sw/P/current/editor", 0);
	succeed_if (found == k2, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/sw/defaults/editor");
	succeed_if_same_string (keyString (found), "ok");

	ElektraKey * searchKey = keyNew ("/sw/P/current/editor", ELEKTRA_KEY_END);
	found = ksLookup (ks, searchKey, 0);
	succeed_if (found == k2, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/sw/defaults/editor");
	succeed_if_same_string (keyString (found), "ok");
	keyDel (searchKey);

	ksDel (ks);
}

static void test_lookupNoOverride (void)
{
	printf ("Test lookup with override not found\n");

	// clang-format off
	ElektraKey *specKey = keyNew("/test/lift/limit",
			ELEKTRA_KEY_META, "default", "1",
			ELEKTRA_KEY_META, "override/#0", "/test/person_lift/limit",
			ELEKTRA_KEY_META, "override/#1", "/test/material_lift/limit",
			ELEKTRA_KEY_META, "override/#2", "/test/heavy_material_lift/limit",
			ELEKTRA_KEY_END);
	// clang-format on
	ElektraKey * dup = keyDup (specKey, ELEKTRA_KEY_CP_ALL);

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKeyset * ks = ksNew (20, k1 = keyNew ("user:/test/lift/limit", ELEKTRA_KEY_VALUE, "22", ELEKTRA_KEY_END),
			     k2 = keyNew ("/test/person_lift/limit", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (ksLookup (ks, dup, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	keySetName (dup, "/test/lift/limit");
	succeed_if (ksLookup (ks, dup, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (ksLookup (ks, dup, ELEKTRA_KDB_O_SPEC | ELEKTRA_KDB_O_CREATE) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
	keyDel (dup);
}

static void test_lookupDefault (void)
{
	printf ("Test lookup default\n");

	ElektraKey * specKey = keyNew ("user:/abc", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = ksNew (20, ELEKTRA_KS_END);

	succeed_if (ksGetSize (ks) == 0, "wrong size");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta (specKey, "default", "xyz");

	k = ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (keyName (k), "default:/abc");
	succeed_if_same_string (keyString (k), "xyz");

	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	keySetMeta (specKey, "default", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupNoascading (void)
{
	printf ("Test lookup without cascading\n");

	ElektraKey * specKey = keyNew ("/abc", ELEKTRA_KEY_END);

	ElektraKey * d = keyDup (specKey, ELEKTRA_KEY_CP_ALL);
	keySetString (d, "dup");
	succeed_if_same_string (keyName (specKey), "/abc");
	succeed_if_same_string (keyName (d), "/abc");

	succeed_if (!keyCmp (d, specKey), "comparision to duplicate failed");
	succeed_if_same_string (keyName (d), "/abc");
	succeed_if_same_string (keyName (specKey), "/abc");

	ElektraKeyset * ks = ksNew (20, d, ELEKTRA_KS_END);

	ElektraKey * k = ksLookup (ks, specKey, ELEKTRA_KDB_O_NOCASCADING);
	succeed_if_same_string (keyName (specKey), "/abc");
	succeed_if (k != 0, "did not find cascading key");
	succeed_if (k != specKey, "should not be specKey");
	succeed_if (k == d, "should be dup key");

	ElektraKey * a = keyNew (keyName (specKey), ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_END);
	ksAppendKey (ks, a);

	for (int i = 0; i < 5; ++i)
	{
		k = ksLookup (ks, specKey, ELEKTRA_KDB_O_NOCASCADING);
		succeed_if (keyGetNameSize (specKey) == 5, "size of spec key wrong");
		succeed_if_same_string (keyName (specKey), "/abc");
		succeed_if (k != 0, "did not find cascading key");
		succeed_if (k != specKey, "should not be specKey");
		succeed_if (k == a, "should be dup key");

		// search without cascading
		k = ksLookup (ks, specKey, 0);
		succeed_if (keyGetNameSize (specKey) == 5, "size of spec key wrong");
		succeed_if_same_string (keyName (specKey), "/abc");
		succeed_if (k != 0, "did not find cascading key");
		succeed_if (k != specKey, "should not be specKey");
		succeed_if (k == a, "should be dup key");
	}

	ksDel (ks);
	keyDel (specKey);
}

static void test_lookupDefaultCascading (void)
{
	printf ("Test lookup default with cascading\n");

	ElektraKey * specKey = keyNew ("/abc", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = ksNew (20, ELEKTRA_KS_END);

	succeed_if (ksGetSize (ks) == 0, "wrong size");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta (specKey, "default", "xyz");
	k = ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (keyName (k), "default:/abc");
	succeed_if_same_string (keyString (k), "xyz");

	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	keySetMeta (specKey, "default", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupLongChain (void)
{
	printf ("Test lookup long chain\n");

	// clang-format off
	ElektraKey *specKey = keyNew("user:/4",
			ELEKTRA_KEY_META, "override/#0", "user:/something",
			ELEKTRA_KEY_META, "override/#1", "user:/something",
			ELEKTRA_KEY_META, "override/#2", "user:/something",
			ELEKTRA_KEY_META, "override/#3", "user:/something",
			ELEKTRA_KEY_META, "override/#3", "user:/something",
			ELEKTRA_KEY_META, "override/#4", "user:/something",
			ELEKTRA_KEY_META, "override/#5", "user:/something",
			ELEKTRA_KEY_META, "override/#6", "user:/something",
			ELEKTRA_KEY_META, "override/#7", "user:/something",
			ELEKTRA_KEY_META, "override/#8", "user:/something",
			ELEKTRA_KEY_META, "override/#9", "user:/something",
			ELEKTRA_KEY_META, "override/#_10", "user:/something",
			ELEKTRA_KEY_META, "override/#_11", "user:/something",
			ELEKTRA_KEY_META, "override/#_12", "user:/something",
			ELEKTRA_KEY_META, "override/#_13", "user:/something",
			ELEKTRA_KEY_META, "override/#_14", "user:/something",
			ELEKTRA_KEY_META, "override/#_15", "user:/something",
			ELEKTRA_KEY_END);
	// clang-format on
	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	ElektraKeyset * ks = ksNew (20, k1 = keyNew ("user:/1", ELEKTRA_KEY_END), k2 = keyNew ("user:/2", ELEKTRA_KEY_END), k3 = keyNew ("user:/3", ELEKTRA_KEY_END),
			     k4 = keyNew ("user:/4", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_16", "user:/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_17", "user:/wrong");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_18", "user:/3");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta (specKey, "override/#_10", "user:/2");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta (specKey, "override/#5", "user:/1");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupCascading (void)
{
	printf ("Test lookup cascading\n");

	ElektraKey * specKey = keyNew ("/abc", ELEKTRA_KEY_META, "override/#0", "/something", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = ksNew (20, k = keyNew ("user:/else", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "fallback/#0", "/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta (specKey, "fallback/#0", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "override/#0", "/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find override key");
	keySetMeta (specKey, "override/#0", "");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	keySetName (specKey, "/else");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key itself");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupNamespace (void)
{
	printf ("Test lookup namespace\n");

	ElektraKey * specKey = keyNew ("/abc", ELEKTRA_KEY_META, "namespace/#0", "system", ELEKTRA_KEY_END);
	ElektraKey * k = 0;

	ElektraKeyset * ks = ksNew (20, k = keyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta (specKey, "namespace/#0", "user");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel (ks);

	ks = ksNew (20, k = keyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	keySetMeta (specKey, "namespace/#0", "user");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta (specKey, "namespace/#0", "system");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel (ks);


	ks = ksNew (20, keyNew ("system:/abc", ELEKTRA_KEY_END), k = keyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	keySetMeta (specKey, "namespace/#0", "user");
	keySetMeta (specKey, "namespace/#1", "system");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel (ks);


	ks = ksNew (20, k = keyNew ("system:/abc", ELEKTRA_KEY_END), keyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	keySetMeta (specKey, "namespace/#0", "system");
	keySetMeta (specKey, "namespace/#1", "user");
	succeed_if (ksLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel (ks);

	keyDel (specKey);
}

static void test_lookupIndirect (void)
{
	printf ("Test lookup by indirect spec\n");

	ElektraKey * s;
	ElektraKey * p;
	ElektraKey * d;
	ElektraKey * u;
	ElektraKey * y;
	ElektraKey * e;
	ElektraKeyset * ks = ksNew (20, s = keyNew ("spec:/abc", ELEKTRA_KEY_END), p = keyNew ("proc:/abc", ELEKTRA_KEY_END), d = keyNew ("dir:/abc", ELEKTRA_KEY_END),
			     u = keyNew ("user:/abc", ELEKTRA_KEY_END), y = keyNew ("system:/abc", ELEKTRA_KEY_END), e = keyNew ("system:/else", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	succeed_if (ksGetSize (ks) == 6, "wrong size");

	ElektraKey * k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");

	keySetMeta (s, "namespace/#0", "no");
	keySetMeta (s, "default", "80");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k != 0, "should find default");
	succeed_if (ksGetSize (ks) == 7, "default key not added");
	succeed_if_same_string (keyString (k), "80");

	ElektraKey * k2 = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == k2, "did not get same default");

	keySetMeta (s, "fallback/#0", "/else");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == e, "did not find else");

	keySetMeta (s, "namespace/#0", "system");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "system");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "proc");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");

	keySetMeta (s, "override/#0", "/else");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == e, "did not find override key");

	ksDel (ks);
}

static void test_lookupDoubleIndirect (void)
{
	printf ("Test lookup by double indirect spec\n");

	ElektraKey * s;
	ElektraKey * p;
	ElektraKey * d;
	ElektraKey * u;
	ElektraKey * y;
	ElektraKey * se;
	ElektraKey * pe;
	ElektraKeyset * ks = ksNew (20, se = keyNew ("spec:/first", ELEKTRA_KEY_END), pe = keyNew ("proc:/first", ELEKTRA_KEY_END),
			     s = keyNew ("spec:/abc", ELEKTRA_KEY_END), p = keyNew ("proc:/abc", ELEKTRA_KEY_END), d = keyNew ("dir:/abc", ELEKTRA_KEY_END),
			     u = keyNew ("user:/abc", ELEKTRA_KEY_END), y = keyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (ksGetSize (ks) == 7, "wrong size");

	ElektraKey * k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find proc key");

	keySetMeta (se, "override/#0", "/abc");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc:/abc");

	keySetMeta (s, "namespace/#0", "system");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "system");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "proc");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc key");

	keySetMeta (s, "override/#0", "proc:/first");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find override key (double indirect)");

	ksDel (ks);
}

static void test_lookupDoubleIndirectDefault (void)
{
	printf ("Test lookup by double indirect spec with default\n");

	ElektraKey * s;
	ElektraKey * p;
	ElektraKey * u;
	ElektraKey * y;
	ElektraKey * se;
	ElektraKey * pe;
	ElektraKeyset * ks =
		ksNew (20, se = keyNew ("spec:/first", ELEKTRA_KEY_END), pe = keyNew ("proc:/first", ELEKTRA_KEY_END), s = keyNew ("spec:/abc", ELEKTRA_KEY_END),
		       p = keyNew ("proc:/abc", ELEKTRA_KEY_END), u = keyNew ("user:/abc", ELEKTRA_KEY_END), y = keyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (ksGetSize (ks) == 6, "wrong size");
	keySetMeta (se, "default", "default is ok");
	keySetMeta (s, "default", "default is NOT ok");

	ElektraKey * k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find proc key");

	keySetMeta (se, "namespace/#0", "system");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if_same_string (keyString (k), "default is ok");

	keySetMeta (se, "override/#0", "/abc");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc:/abc");

	keySetMeta (s, "namespace/#0", "system");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "system");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	keySetMeta (s, "namespace/#0", "proc");
	keySetMeta (s, "namespace/#1", "user");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc key");

	keySetMeta (s, "namespace/#0", "dir");
	keySetMeta (s, "namespace/#1", 0);
	k = ksLookupByName (ks, "/first", 0);
	succeed_if_same_string (keyString (k), "default is ok");

	keySetMeta (s, "override/#0", "proc:/first");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find override key (double indirect)");

	keySetMeta (s, "override/#0", "dir:/first");
	k = ksLookupByName (ks, "/first", 0);
	succeed_if_same_string (keyString (k), "default is ok");

	ksDel (ks);
}


int main (int argc, char ** argv)
{
	printf ("SPEC  TESTS\n");
	printf ("==========================\n\n");

	init (argc, argv);

	test_lookupSingle ();
	test_lookupChain ();
	test_lookupChainLast ();
	test_lookupChainRealWorld ();
	test_lookupNoOverride ();
	test_lookupDefault ();
	test_lookupNoascading ();
	test_lookupDefaultCascading ();
	test_lookupLongChain ();
	test_lookupCascading ();
	test_lookupNamespace ();
	test_lookupIndirect ();
	test_lookupDoubleIndirect ();
	test_lookupDoubleIndirectDefault ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);

	return nbError;
}
