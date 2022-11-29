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

	Key * specKey = keyNew ("user:/abc", KEY_META, "override/#0", "user:/something", KEY_END);
	Key * k = 0;
	KeySet * ks = ksNew (20, k = keyNew ("user:/else", KEY_END), KS_END);
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "fallback/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta (specKey, "fallback/#0", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find override key");
	keySetMeta (specKey, "override/#0", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupChain (void)
{
	printf ("Test lookup chain\n");

	Key * specKey = keyNew ("user:/4", KEY_META, "override/#0", "user:/something", KEY_END);
	Key * k1 = 0;
	Key * k2 = 0;
	Key * k3 = 0;
	Key * k4 = 0;
	KeySet * ks = ksNew (20, k1 = keyNew ("user:/1", KEY_END), k2 = keyNew ("user:/2", KEY_END), k3 = keyNew ("user:/3", KEY_END),
			     k4 = keyNew ("user:/4", KEY_END), KS_END);

	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#1", "user:/wrong");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#2", "user:/3");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta (specKey, "override/#1", "user:/2");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta (specKey, "override/#0", "user:/1");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupChainLast (void)
{
	printf ("Test lookup chain last\n");

	Key * k1 = 0;
	Key * k2 = 0;
	Key * k3 = 0;
	Key * k4 = 0;
	// clang-format off
	KeySet *ks= ksNew(20,
		k1 = keyNew("spec:/key",
			KEY_VALUE, "spec value",
			KEY_META, "override/#0", "/something",
			KEY_META, "override/#1", "/something_else",
			KEY_META, "override/#2", "/override",
			KEY_END),
		k2 = keyNew("user:/key", KEY_VALUE, "wrong user value", KEY_END),
		k3 = keyNew("dir:/key", KEY_VALUE, "wrong dir value", KEY_END),
		k4 = keyNew("user:/override", KEY_VALUE, "ok", KEY_END),
		KS_END);
	// clang-format on

	Key * found = ksLookupByName (ks, "/key", 0);
	succeed_if (found == k4, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/override");
	succeed_if_same_string (keyString (found), "ok");

	Key * searchKey = keyNew ("/key", KEY_END);
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

	Key * k1 = 0;
	Key * k2 = 0;
	Key * k3 = 0;
	Key * k4 = 0;
	// clang-format off
	KeySet *ks= ksNew(20,
		k1 = keyNew("spec:/sw/P/current/editor",
			KEY_META, "example", "vim",
			KEY_META, "override/#0", "/sw/P/override/editor",
			KEY_META, "override/#1", "/sw/override/editor",
			KEY_META, "override/#2", "/sw/defaults/editor",
			KEY_END),
		k2 = keyNew("user:/sw/defaults/editor", KEY_VALUE, "ok", KEY_END),
		k3 = keyNew("dir:/sw/P/current/editor", KEY_VALUE, "wrong dir value", KEY_END),
		k4 = keyNew("user:/sw/P/current/editor", KEY_VALUE, "wrong user value", KEY_END),
		KS_END);
	// clang-format on

	Key * found = ksLookupByName (ks, "/sw/P/current/editor", 0);
	succeed_if (found == k2, "found wrong key");
	succeed_if_same_string (keyName (found), "user:/sw/defaults/editor");
	succeed_if_same_string (keyString (found), "ok");

	Key * searchKey = keyNew ("/sw/P/current/editor", KEY_END);
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
	Key *specKey = keyNew("/test/lift/limit",
			KEY_META, "default", "1",
			KEY_META, "override/#0", "/test/person_lift/limit",
			KEY_META, "override/#1", "/test/material_lift/limit",
			KEY_META, "override/#2", "/test/heavy_material_lift/limit",
			KEY_END);
	// clang-format on
	Key * dup = keyDup (specKey, KEY_CP_ALL);

	Key * k1 = 0;
	Key * k2 = 0;
	KeySet * ks = ksNew (20, k1 = keyNew ("user:/test/lift/limit", KEY_VALUE, "22", KEY_END),
			     k2 = keyNew ("/test/person_lift/limit", KEY_VALUE, "10", KEY_END), KS_END);

	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (ksLookup (ks, dup, KDB_O_SPEC) == k1, "found wrong key");
	keySetName (dup, "/test/lift/limit");
	succeed_if (ksLookup (ks, dup, KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (ksLookup (ks, dup, KDB_O_SPEC) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
	keyDel (dup);
}

static void test_lookupDefault (void)
{
	printf ("Test lookup default\n");

	Key * specKey = keyNew ("user:/abc", KEY_END);
	Key * k = 0;
	KeySet * ks = ksNew (20, KS_END);

	succeed_if (ksGetSize (ks) == 0, "wrong size");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta (specKey, "default", "xyz");

	k = ksLookup (ks, specKey, KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (keyName (k), "default:/abc");
	succeed_if_same_string (keyString (k), "xyz");

	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	keySetMeta (specKey, "default", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupNoascading (void)
{
	printf ("Test lookup without cascading\n");

	Key * specKey = keyNew ("/abc", KEY_END);

	Key * d = keyDup (specKey, KEY_CP_ALL);
	keySetString (d, "dup");
	succeed_if_same_string (keyName (specKey), "/abc");
	succeed_if_same_string (keyName (d), "/abc");

	succeed_if (!keyCmp (d, specKey), "comparision to duplicate failed");
	succeed_if_same_string (keyName (d), "/abc");
	succeed_if_same_string (keyName (specKey), "/abc");

	KeySet * ks = ksNew (20, d, KS_END);

	Key * k = ksLookup (ks, specKey, KDB_O_NOCASCADING);
	succeed_if_same_string (keyName (specKey), "/abc");
	succeed_if (k != 0, "did not find cascading key");
	succeed_if (k != specKey, "should not be specKey");
	succeed_if (k == d, "should be dup key");

	Key * a = keyNew (keyName (specKey), KEY_VALUE, "a", KEY_END);
	ksAppendKey (ks, a);

	for (int i = 0; i < 5; ++i)
	{
		k = ksLookup (ks, specKey, KDB_O_NOCASCADING);
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

	Key * specKey = keyNew ("/abc", KEY_END);
	Key * k = 0;
	KeySet * ks = ksNew (20, KS_END);

	succeed_if (ksGetSize (ks) == 0, "wrong size");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta (specKey, "default", "xyz");
	k = ksLookup (ks, specKey, KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (keyName (k), "default:/abc");
	succeed_if_same_string (keyString (k), "xyz");

	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	keySetMeta (specKey, "default", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (ksGetSize (ks) == 1, "wrong size");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupLongChain (void)
{
	printf ("Test lookup long chain\n");

	// clang-format off
	Key *specKey = keyNew("user:/4",
			KEY_META, "override/#0", "user:/something",
			KEY_META, "override/#1", "user:/something",
			KEY_META, "override/#2", "user:/something",
			KEY_META, "override/#3", "user:/something",
			KEY_META, "override/#3", "user:/something",
			KEY_META, "override/#4", "user:/something",
			KEY_META, "override/#5", "user:/something",
			KEY_META, "override/#6", "user:/something",
			KEY_META, "override/#7", "user:/something",
			KEY_META, "override/#8", "user:/something",
			KEY_META, "override/#9", "user:/something",
			KEY_META, "override/#_10", "user:/something",
			KEY_META, "override/#_11", "user:/something",
			KEY_META, "override/#_12", "user:/something",
			KEY_META, "override/#_13", "user:/something",
			KEY_META, "override/#_14", "user:/something",
			KEY_META, "override/#_15", "user:/something",
			KEY_END);
	// clang-format on
	Key * k1 = 0;
	Key * k2 = 0;
	Key * k3 = 0;
	Key * k4 = 0;
	KeySet * ks = ksNew (20, k1 = keyNew ("user:/1", KEY_END), k2 = keyNew ("user:/2", KEY_END), k3 = keyNew ("user:/3", KEY_END),
			     k4 = keyNew ("user:/4", KEY_END), KS_END);

	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_16", "user:/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_17", "user:/wrong");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta (specKey, "override/#_18", "user:/3");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta (specKey, "override/#_10", "user:/2");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta (specKey, "override/#5", "user:/1");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k1, "found wrong key");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupCascading (void)
{
	printf ("Test lookup cascading\n");

	Key * specKey = keyNew ("/abc", KEY_META, "override/#0", "/something", KEY_END);
	Key * k = 0;
	KeySet * ks = ksNew (20, k = keyNew ("user:/else", KEY_END), KS_END);
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "fallback/#0", "/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta (specKey, "fallback/#0", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta (specKey, "override/#0", "/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find override key");
	keySetMeta (specKey, "override/#0", "");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetName (specKey, "/else");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find key itself");

	keyDel (specKey);
	ksDel (ks);
}

static void test_lookupNamespace (void)
{
	printf ("Test lookup namespace\n");

	Key * specKey = keyNew ("/abc", KEY_META, "namespace/#0", "system", KEY_END);
	Key * k = 0;

	KeySet * ks = ksNew (20, k = keyNew ("user:/abc", KEY_END), KS_END);
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta (specKey, "namespace/#0", "user");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel (ks);

	ks = ksNew (20, k = keyNew ("system:/abc", KEY_END), KS_END);
	keySetMeta (specKey, "namespace/#0", "user");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta (specKey, "namespace/#0", "system");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel (ks);


	ks = ksNew (20, keyNew ("system:/abc", KEY_END), k = keyNew ("user:/abc", KEY_END), KS_END);
	keySetMeta (specKey, "namespace/#0", "user");
	keySetMeta (specKey, "namespace/#1", "system");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel (ks);


	ks = ksNew (20, k = keyNew ("system:/abc", KEY_END), keyNew ("user:/abc", KEY_END), KS_END);
	keySetMeta (specKey, "namespace/#0", "system");
	keySetMeta (specKey, "namespace/#1", "user");
	succeed_if (ksLookup (ks, specKey, KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel (ks);

	keyDel (specKey);
}

static void test_lookupIndirect (void)
{
	printf ("Test lookup by indirect spec\n");

	Key * s;
	Key * p;
	Key * d;
	Key * u;
	Key * y;
	Key * e;
	KeySet * ks = ksNew (20, s = keyNew ("spec:/abc", KEY_END), p = keyNew ("proc:/abc", KEY_END), d = keyNew ("dir:/abc", KEY_END),
			     u = keyNew ("user:/abc", KEY_END), y = keyNew ("system:/abc", KEY_END), e = keyNew ("system:/else", KEY_END),
			     KS_END);
	succeed_if (ksGetSize (ks) == 6, "wrong size");

	Key * k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");

	keySetMeta (s, "namespace/#0", "no");
	keySetMeta (s, "default", "80");
	k = ksLookupByName (ks, "/abc", 0);
	succeed_if (k != 0, "should find default");
	succeed_if (ksGetSize (ks) == 7, "default key not added");
	succeed_if_same_string (keyString (k), "80");

	Key * k2 = ksLookupByName (ks, "/abc", 0);
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

	Key * s;
	Key * p;
	Key * d;
	Key * u;
	Key * y;
	Key * se;
	Key * pe;
	KeySet * ks = ksNew (20, se = keyNew ("spec:/first", KEY_END), pe = keyNew ("proc:/first", KEY_END),
			     s = keyNew ("spec:/abc", KEY_END), p = keyNew ("proc:/abc", KEY_END), d = keyNew ("dir:/abc", KEY_END),
			     u = keyNew ("user:/abc", KEY_END), y = keyNew ("system:/abc", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 7, "wrong size");

	Key * k = ksLookupByName (ks, "/first", 0);
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

	Key * s;
	Key * p;
	Key * u;
	Key * y;
	Key * se;
	Key * pe;
	KeySet * ks =
		ksNew (20, se = keyNew ("spec:/first", KEY_END), pe = keyNew ("proc:/first", KEY_END), s = keyNew ("spec:/abc", KEY_END),
		       p = keyNew ("proc:/abc", KEY_END), u = keyNew ("user:/abc", KEY_END), y = keyNew ("system:/abc", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 6, "wrong size");
	keySetMeta (se, "default", "default is ok");
	keySetMeta (s, "default", "default is NOT ok");

	Key * k = ksLookupByName (ks, "/first", 0);
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
