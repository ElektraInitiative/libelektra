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

	ElektraKey * specKey = elektraKeyNew ("user:/abc", ELEKTRA_KEY_META, "override/#0", "user:/something", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, k = elektraKeyNew ("user:/else", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetMeta (specKey, "fallback/#0", "user:/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find fallback key");
	elektraKeySetMeta (specKey, "fallback/#0", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find override key");
	elektraKeySetMeta (specKey, "override/#0", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupChain (void)
{
	printf ("Test lookup chain\n");

	ElektraKey * specKey = elektraKeyNew ("user:/4", ELEKTRA_KEY_META, "override/#0", "user:/something", ELEKTRA_KEY_END);
	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, k1 = elektraKeyNew ("user:/1", ELEKTRA_KEY_END), k2 = elektraKeyNew ("user:/2", ELEKTRA_KEY_END), k3 = elektraKeyNew ("user:/3", ELEKTRA_KEY_END),
			     k4 = elektraKeyNew ("user:/4", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#0", "user:/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#1", "user:/wrong");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#2", "user:/3");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k3, "did not find override key");
	elektraKeySetMeta (specKey, "override/#1", "user:/2");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k2, "found wrong key");
	elektraKeySetMeta (specKey, "override/#0", "user:/1");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupChainLast (void)
{
	printf ("Test lookup chain last\n");

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	// clang-format off
	ElektraKeyset *ks= elektraKeysetNew(20,
		k1 = elektraKeyNew("spec:/key",
			ELEKTRA_KEY_VALUE, "spec value",
			ELEKTRA_KEY_META, "override/#0", "/something",
			ELEKTRA_KEY_META, "override/#1", "/something_else",
			ELEKTRA_KEY_META, "override/#2", "/override",
			ELEKTRA_KEY_END),
		k2 = elektraKeyNew("user:/key", ELEKTRA_KEY_VALUE, "wrong user value", ELEKTRA_KEY_END),
		k3 = elektraKeyNew("dir:/key", ELEKTRA_KEY_VALUE, "wrong dir value", ELEKTRA_KEY_END),
		k4 = elektraKeyNew("user:/override", ELEKTRA_KEY_VALUE, "ok", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on

	ElektraKey * found = elektraKeysetLookupByName (ks, "/key", 0);
	succeed_if (found == k4, "found wrong key");
	succeed_if_same_string (elektraKeyName (found), "user:/override");
	succeed_if_same_string (elektraKeyString (found), "ok");

	ElektraKey * searchKey = elektraKeyNew ("/key", ELEKTRA_KEY_END);
	found = elektraKeysetLookup (ks, searchKey, 0);
	succeed_if (found == k4, "found wrong key");
	succeed_if_same_string (elektraKeyName (found), "user:/override");
	succeed_if_same_string (elektraKeyString (found), "ok");
	elektraKeyDel (searchKey);

	elektraKeysetDel (ks);
}


static void test_lookupChainRealWorld (void)
{
	printf ("Test lookup chain real world\n");

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKey * k3 = 0;
	ElektraKey * k4 = 0;
	// clang-format off
	ElektraKeyset *ks= elektraKeysetNew(20,
		k1 = elektraKeyNew("spec:/sw/P/current/editor",
			ELEKTRA_KEY_META, "example", "vim",
			ELEKTRA_KEY_META, "override/#0", "/sw/P/override/editor",
			ELEKTRA_KEY_META, "override/#1", "/sw/override/editor",
			ELEKTRA_KEY_META, "override/#2", "/sw/defaults/editor",
			ELEKTRA_KEY_END),
		k2 = elektraKeyNew("user:/sw/defaults/editor", ELEKTRA_KEY_VALUE, "ok", ELEKTRA_KEY_END),
		k3 = elektraKeyNew("dir:/sw/P/current/editor", ELEKTRA_KEY_VALUE, "wrong dir value", ELEKTRA_KEY_END),
		k4 = elektraKeyNew("user:/sw/P/current/editor", ELEKTRA_KEY_VALUE, "wrong user value", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on

	ElektraKey * found = elektraKeysetLookupByName (ks, "/sw/P/current/editor", 0);
	succeed_if (found == k2, "found wrong key");
	succeed_if_same_string (elektraKeyName (found), "user:/sw/defaults/editor");
	succeed_if_same_string (elektraKeyString (found), "ok");

	ElektraKey * searchKey = elektraKeyNew ("/sw/P/current/editor", ELEKTRA_KEY_END);
	found = elektraKeysetLookup (ks, searchKey, 0);
	succeed_if (found == k2, "found wrong key");
	succeed_if_same_string (elektraKeyName (found), "user:/sw/defaults/editor");
	succeed_if_same_string (elektraKeyString (found), "ok");
	elektraKeyDel (searchKey);

	elektraKeysetDel (ks);
}

static void test_lookupNoOverride (void)
{
	printf ("Test lookup with override not found\n");

	// clang-format off
	ElektraKey *specKey = elektraKeyNew("/test/lift/limit",
			ELEKTRA_KEY_META, "default", "1",
			ELEKTRA_KEY_META, "override/#0", "/test/person_lift/limit",
			ELEKTRA_KEY_META, "override/#1", "/test/material_lift/limit",
			ELEKTRA_KEY_META, "override/#2", "/test/heavy_material_lift/limit",
			ELEKTRA_KEY_END);
	// clang-format on
	ElektraKey * dup = elektraKeyDup (specKey, ELEKTRA_KEY_CP_ALL);

	ElektraKey * k1 = 0;
	ElektraKey * k2 = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, k1 = elektraKeyNew ("user:/test/lift/limit", ELEKTRA_KEY_VALUE, "22", ELEKTRA_KEY_END),
			     k2 = elektraKeyNew ("/test/person_lift/limit", ELEKTRA_KEY_VALUE, "10", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (elektraKeysetLookup (ks, dup, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	elektraKeySetName (dup, "/test/lift/limit");
	succeed_if (elektraKeysetLookup (ks, dup, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");
	succeed_if (elektraKeysetLookup (ks, dup, ELEKTRA_KDB_O_SPEC | ELEKTRA_KDB_O_CREATE) == k1, "found wrong key");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
	elektraKeyDel (dup);
}

static void test_lookupDefault (void)
{
	printf ("Test lookup default\n");

	ElektraKey * specKey = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, ELEKTRA_KS_END);

	succeed_if (elektraKeysetGetSize (ks) == 0, "wrong size");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	elektraKeySetMeta (specKey, "default", "xyz");

	k = elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (elektraKeyName (k), "default:/abc");
	succeed_if_same_string (elektraKeyString (k), "xyz");

	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	elektraKeySetMeta (specKey, "default", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupNoascading (void)
{
	printf ("Test lookup without cascading\n");

	ElektraKey * specKey = elektraKeyNew ("/abc", ELEKTRA_KEY_END);

	ElektraKey * d = elektraKeyDup (specKey, ELEKTRA_KEY_CP_ALL);
	elektraKeySetString (d, "dup");
	succeed_if_same_string (elektraKeyName (specKey), "/abc");
	succeed_if_same_string (elektraKeyName (d), "/abc");

	succeed_if (!elektraKeyCmp (d, specKey), "comparision to duplicate failed");
	succeed_if_same_string (elektraKeyName (d), "/abc");
	succeed_if_same_string (elektraKeyName (specKey), "/abc");

	ElektraKeyset * ks = elektraKeysetNew (20, d, ELEKTRA_KS_END);

	ElektraKey * k = elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_NOCASCADING);
	succeed_if_same_string (elektraKeyName (specKey), "/abc");
	succeed_if (k != 0, "did not find cascading key");
	succeed_if (k != specKey, "should not be specKey");
	succeed_if (k == d, "should be dup key");

	ElektraKey * a = elektraKeyNew (elektraKeyName (specKey), ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_END);
	elektraKeysetAppendKey (ks, a);

	for (int i = 0; i < 5; ++i)
	{
		k = elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_NOCASCADING);
		succeed_if (elektraKeyGetNameSize (specKey) == 5, "size of spec key wrong");
		succeed_if_same_string (elektraKeyName (specKey), "/abc");
		succeed_if (k != 0, "did not find cascading key");
		succeed_if (k != specKey, "should not be specKey");
		succeed_if (k == a, "should be dup key");

		// search without cascading
		k = elektraKeysetLookup (ks, specKey, 0);
		succeed_if (elektraKeyGetNameSize (specKey) == 5, "size of spec key wrong");
		succeed_if_same_string (elektraKeyName (specKey), "/abc");
		succeed_if (k != 0, "did not find cascading key");
		succeed_if (k != specKey, "should not be specKey");
		succeed_if (k == a, "should be dup key");
	}

	elektraKeysetDel (ks);
	elektraKeyDel (specKey);
}

static void test_lookupDefaultCascading (void)
{
	printf ("Test lookup default with cascading\n");

	ElektraKey * specKey = elektraKeyNew ("/abc", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, ELEKTRA_KS_END);

	succeed_if (elektraKeysetGetSize (ks) == 0, "wrong size");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");

	elektraKeySetMeta (specKey, "default", "xyz");
	k = elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC);
	succeed_if (k != 0, "found no default key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if_same_string (elektraKeyName (k), "default:/abc");
	succeed_if_same_string (elektraKeyString (k), "xyz");

	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	elektraKeySetMeta (specKey, "default", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find default key again");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupLongChain (void)
{
	printf ("Test lookup long chain\n");

	// clang-format off
	ElektraKey *specKey = elektraKeyNew("user:/4",
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
	ElektraKeyset * ks = elektraKeysetNew (20, k1 = elektraKeyNew ("user:/1", ELEKTRA_KEY_END), k2 = elektraKeyNew ("user:/2", ELEKTRA_KEY_END), k3 = elektraKeyNew ("user:/3", ELEKTRA_KEY_END),
			     k4 = elektraKeyNew ("user:/4", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#_16", "user:/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#_17", "user:/wrong");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k4, "found wrong key");
	elektraKeySetMeta (specKey, "override/#_18", "user:/3");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k3, "did not find override key");
	elektraKeySetMeta (specKey, "override/#_10", "user:/2");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k2, "found wrong key");
	elektraKeySetMeta (specKey, "override/#5", "user:/1");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k1, "found wrong key");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupCascading (void)
{
	printf ("Test lookup cascading\n");

	ElektraKey * specKey = elektraKeyNew ("/abc", ELEKTRA_KEY_META, "override/#0", "/something", ELEKTRA_KEY_END);
	ElektraKey * k = 0;
	ElektraKeyset * ks = elektraKeysetNew (20, k = elektraKeyNew ("user:/else", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetMeta (specKey, "fallback/#0", "/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find fallback key");
	elektraKeySetMeta (specKey, "fallback/#0", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetMeta (specKey, "override/#0", "/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find override key");
	elektraKeySetMeta (specKey, "override/#0", "");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetName (specKey, "/else");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key itself");

	elektraKeyDel (specKey);
	elektraKeysetDel (ks);
}

static void test_lookupNamespace (void)
{
	printf ("Test lookup namespace\n");

	ElektraKey * specKey = elektraKeyNew ("/abc", ELEKTRA_KEY_META, "namespace/#0", "system", ELEKTRA_KEY_END);
	ElektraKey * k = 0;

	ElektraKeyset * ks = elektraKeysetNew (20, k = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key of other namespace");
	elektraKeySetMeta (specKey, "namespace/#0", "user");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key in correct namespace");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, k = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeySetMeta (specKey, "namespace/#0", "user");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == 0, "found wrong key of other namespace");
	elektraKeySetMeta (specKey, "namespace/#0", "system");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "did not find key in correct namespace");
	elektraKeysetDel (ks);


	ks = elektraKeysetNew (20, elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), k = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeySetMeta (specKey, "namespace/#0", "user");
	elektraKeySetMeta (specKey, "namespace/#1", "system");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "found wrong key of other namespace");
	elektraKeysetDel (ks);


	ks = elektraKeysetNew (20, k = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraKeySetMeta (specKey, "namespace/#0", "system");
	elektraKeySetMeta (specKey, "namespace/#1", "user");
	succeed_if (elektraKeysetLookup (ks, specKey, ELEKTRA_KDB_O_SPEC) == k, "found wrong key of other namespace");
	elektraKeysetDel (ks);

	elektraKeyDel (specKey);
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
	ElektraKeyset * ks = elektraKeysetNew (20, s = elektraKeyNew ("spec:/abc", ELEKTRA_KEY_END), p = elektraKeyNew ("proc:/abc", ELEKTRA_KEY_END), d = elektraKeyNew ("dir:/abc", ELEKTRA_KEY_END),
			     u = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), y = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), e = elektraKeyNew ("system:/else", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	succeed_if (elektraKeysetGetSize (ks) == 6, "wrong size");

	ElektraKey * k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");

	elektraKeySetMeta (s, "namespace/#0", "no");
	elektraKeySetMeta (s, "default", "80");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k != 0, "should find default");
	succeed_if (elektraKeysetGetSize (ks) == 7, "default key not added");
	succeed_if_same_string (elektraKeyString (k), "80");

	ElektraKey * k2 = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == k2, "did not get same default");

	elektraKeySetMeta (s, "fallback/#0", "/else");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == e, "did not find else");

	elektraKeySetMeta (s, "namespace/#0", "system");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "system");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "proc");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");

	elektraKeySetMeta (s, "override/#0", "/else");
	k = elektraKeysetLookupByName (ks, "/abc", 0);
	succeed_if (k == e, "did not find override key");

	elektraKeysetDel (ks);
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
	ElektraKeyset * ks = elektraKeysetNew (20, se = elektraKeyNew ("spec:/first", ELEKTRA_KEY_END), pe = elektraKeyNew ("proc:/first", ELEKTRA_KEY_END),
			     s = elektraKeyNew ("spec:/abc", ELEKTRA_KEY_END), p = elektraKeyNew ("proc:/abc", ELEKTRA_KEY_END), d = elektraKeyNew ("dir:/abc", ELEKTRA_KEY_END),
			     u = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), y = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetGetSize (ks) == 7, "wrong size");

	ElektraKey * k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find proc key");

	elektraKeySetMeta (se, "override/#0", "/abc");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc:/abc");

	elektraKeySetMeta (s, "namespace/#0", "system");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "system");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "proc");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc key");

	elektraKeySetMeta (s, "override/#0", "proc:/first");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find override key (double indirect)");

	elektraKeysetDel (ks);
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
		elektraKeysetNew (20, se = elektraKeyNew ("spec:/first", ELEKTRA_KEY_END), pe = elektraKeyNew ("proc:/first", ELEKTRA_KEY_END), s = elektraKeyNew ("spec:/abc", ELEKTRA_KEY_END),
		       p = elektraKeyNew ("proc:/abc", ELEKTRA_KEY_END), u = elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), y = elektraKeyNew ("system:/abc", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetGetSize (ks) == 6, "wrong size");
	elektraKeySetMeta (se, "default", "default is ok");
	elektraKeySetMeta (s, "default", "default is NOT ok");

	ElektraKey * k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find proc key");

	elektraKeySetMeta (se, "namespace/#0", "system");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if_same_string (elektraKeyString (k), "default is ok");

	elektraKeySetMeta (se, "override/#0", "/abc");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc:/abc");

	elektraKeySetMeta (s, "namespace/#0", "system");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "system");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == y, "did not find system key");

	elektraKeySetMeta (s, "namespace/#0", "proc");
	elektraKeySetMeta (s, "namespace/#1", "user");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == p, "did not find proc key");

	elektraKeySetMeta (s, "namespace/#0", "dir");
	elektraKeySetMeta (s, "namespace/#1", 0);
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if_same_string (elektraKeyString (k), "default is ok");

	elektraKeySetMeta (s, "override/#0", "proc:/first");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if (k == pe, "did not find override key (double indirect)");

	elektraKeySetMeta (s, "override/#0", "dir:/first");
	k = elektraKeysetLookupByName (ks, "/first", 0);
	succeed_if_same_string (elektraKeyString (k), "default is ok");

	elektraKeysetDel (ks);
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
