#include <tests.h>

#include <kdbproposal.h>

static void test_lookupSingle()
{
	printf ("Test lookup single\n");

	Key *specKey = keyNew("user/abc",
			KEY_META, "override/#0", "user/something",
			KEY_END);
	Key *k = 0;
	KeySet *ks= ksNew(20,
		k = keyNew("user/else", KEY_END),
		KS_END);
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta(specKey, "fallback/#0", "user/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta(specKey, "fallback/#0", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta(specKey, "override/#0", "user/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find override key");
	keySetMeta(specKey, "override/#0", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keyDel(specKey);
	ksDel(ks);
}

static void test_lookupChain()
{
	printf ("Test lookup chain\n");

	Key *specKey = keyNew("user/4",
			KEY_META, "override/#0", "user/something",
			KEY_END);
	Key *k1 = 0;
	Key *k2 = 0;
	Key *k3 = 0;
	Key *k4 = 0;
	KeySet *ks= ksNew(20,
		k1 = keyNew("user/1", KEY_END),
		k2 = keyNew("user/2", KEY_END),
		k3 = keyNew("user/3", KEY_END),
		k4 = keyNew("user/4", KEY_END),
		KS_END);

	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#0", "user/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#1", "user/wrong");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#2", "user/3");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta(specKey, "override/#1", "user/2");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta(specKey, "override/#0", "user/1");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k1, "found wrong key");

	keyDel(specKey);
	ksDel(ks);
}

static void test_lookupDefault()
{
	printf ("Test lookup default\n");

	Key *specKey = keyNew("user/abc",
			KEY_END);
	Key *k = 0;
	KeySet *ks= ksNew(20,
		KS_END);

	succeed_if(ksGetSize(ks) == 0, "wrong size");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta(specKey, "default", "xyz");

	k = ksLookup(ks, specKey, KDB_O_SPEC);
	succeed_if(k != 0, "found no default key");
	succeed_if(ksGetSize(ks) == 1, "wrong size");
	succeed_if_same_string(keyName(k), "/abc");
	succeed_if_same_string(keyString(k), "xyz");

	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if(ksGetSize(ks) == 1, "wrong size");
	keySetMeta(specKey, "default", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if(ksGetSize(ks) == 1, "wrong size");

	keyDel(specKey);
	ksDel(ks);

}

static void test_lookupNoascading()
{
	printf ("Test lookup without cascading\n");

	Key *specKey = keyNew("/abc",
			KEY_CASCADING_NAME,
			KEY_END);

	Key *d = keyDup(specKey);
	keySetString(d, "dup");
	succeed_if_same_string(keyName(specKey), "/abc");
	succeed_if_same_string(keyName(d), "/abc");

	succeed_if (!keyCmp(d, specKey), "comparision to duplicate failed");
	succeed_if_same_string(keyName(d), "/abc");
	succeed_if_same_string(keyName(specKey), "/abc");

	KeySet *ks= ksNew(20,
		d,
		KS_END);

	Key *k = ksLookup(ks, specKey, KDB_O_NOCASCADING);
	succeed_if_same_string(keyName(specKey), "/abc");
	succeed_if (k != 0, "did not find cascading key");
	succeed_if (k != specKey, "should not be specKey");
	succeed_if (k == d, "should be dup key");

	Key *a=keyNew(
		keyName(specKey),
		KEY_CASCADING_NAME,
		KEY_VALUE, "a",
		KEY_END);
	ksAppendKey(ks, a);

	for (int i=0; i<5; ++i)
	{
		k = ksLookup(ks, specKey, KDB_O_NOCASCADING);
		succeed_if(keyGetNameSize(specKey) == 5, "size of spec key wrong");
		succeed_if_same_string(keyName(specKey), "/abc");
		succeed_if (k != 0, "did not find cascading key");
		succeed_if (k != specKey, "should not be specKey");
		succeed_if (k == a, "should be dup key");

		// search without cascading
		k = ksLookup(ks, specKey, 0);
		succeed_if (k == 0, "should not be able to find cascading key");
		succeed_if(keyGetNameSize(specKey) == 5, "size of spec key wrong");
	}

	ksDel(ks);
	keyDel(specKey);
}

static void test_lookupDefaultCascading()
{
	printf ("Test lookup default with cascading\n");

	Key *specKey = keyNew("/abc",
			KEY_CASCADING_NAME,
			KEY_END);
	Key *k = 0;
	KeySet *ks= ksNew(20,
		KS_END);

	succeed_if(ksGetSize(ks) == 0, "wrong size");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");

	keySetMeta(specKey, "default", "xyz");
	k = ksLookup(ks, specKey, KDB_O_SPEC);
	succeed_if(k != 0, "found no default key");
	succeed_if(ksGetSize(ks) == 1, "wrong size");
	succeed_if_same_string(keyName(k), "/abc");
	succeed_if_same_string(keyString(k), "xyz");

	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if(ksGetSize(ks) == 1, "wrong size");
	keySetMeta(specKey, "default", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	succeed_if(ksGetSize(ks) == 1, "wrong size");

	keyDel(specKey);
	ksDel(ks);

}

static void test_lookupLongChain()
{
	printf ("Test lookup long chain\n");

	Key *specKey = keyNew("user/4",
			KEY_META, "override/#0", "user/something",
			KEY_META, "override/#1", "user/something",
			KEY_META, "override/#2", "user/something",
			KEY_META, "override/#3", "user/something",
			KEY_META, "override/#3", "user/something",
			KEY_META, "override/#4", "user/something",
			KEY_META, "override/#5", "user/something",
			KEY_META, "override/#6", "user/something",
			KEY_META, "override/#7", "user/something",
			KEY_META, "override/#8", "user/something",
			KEY_META, "override/#9", "user/something",
			KEY_META, "override/#_10", "user/something",
			KEY_META, "override/#_11", "user/something",
			KEY_META, "override/#_12", "user/something",
			KEY_META, "override/#_13", "user/something",
			KEY_META, "override/#_14", "user/something",
			KEY_META, "override/#_15", "user/something",
			KEY_END);
	Key *k1 = 0;
	Key *k2 = 0;
	Key *k3 = 0;
	Key *k4 = 0;
	KeySet *ks= ksNew(20,
		k1 = keyNew("user/1", KEY_END),
		k2 = keyNew("user/2", KEY_END),
		k3 = keyNew("user/3", KEY_END),
		k4 = keyNew("user/4", KEY_END),
		KS_END);

	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#_16", "user/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#_17", "user/wrong");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k4, "found wrong key");
	keySetMeta(specKey, "override/#_18", "user/3");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k3, "did not find override key");
	keySetMeta(specKey, "override/#_10", "user/2");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k2, "found wrong key");
	keySetMeta(specKey, "override/#5", "user/1");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k1, "found wrong key");

	keyDel(specKey);
	ksDel(ks);
}

static void test_lookupCascading()
{
	printf ("Test lookup cascading\n");

	Key *specKey = keyNew("/abc",
			KEY_CASCADING_NAME,
			KEY_META, "override/#0", "/something",
			KEY_END);
	Key *k = 0;
	KeySet *ks= ksNew(20,
		k = keyNew("user/else", KEY_END),
		KS_END);
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta(specKey, "fallback/#0", "/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find fallback key");
	keySetMeta(specKey, "fallback/#0", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	keySetMeta(specKey, "override/#0", "/else");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find override key");
	keySetMeta(specKey, "override/#0", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key");
	elektraKeySetName(specKey, "/else", KEY_CASCADING_NAME);
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find key itself");

	keyDel(specKey);
	ksDel(ks);
}

static void test_lookupNamespace()
{
	printf ("Test lookup namespace\n");

	Key *specKey = keyNew("/abc",
			KEY_CASCADING_NAME,
			KEY_META, "namespace/#0", "system",
			KEY_END);
	Key *k = 0;

	KeySet *ks= ksNew(20,
		k = keyNew("user/abc", KEY_END),
		KS_END);
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta(specKey, "namespace/#0", "user");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel(ks);

	ks= ksNew(20,
		k = keyNew("system/abc", KEY_END),
		KS_END);
	keySetMeta(specKey, "namespace/#0", "user");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == 0, "found wrong key of other namespace");
	keySetMeta(specKey, "namespace/#0", "system");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find key in correct namespace");
	ksDel(ks);


	ks= ksNew(20,
		keyNew("system/abc", KEY_END),
		k = keyNew("user/abc", KEY_END),
		KS_END);
	keySetMeta(specKey, "namespace/#0", "user");
	keySetMeta(specKey, "namespace/#1", "system");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel(ks);


	ks= ksNew(20,
		k = keyNew("system/abc", KEY_END),
		keyNew("user/abc", KEY_END),
		KS_END);
	keySetMeta(specKey, "namespace/#0", "system");
	keySetMeta(specKey, "namespace/#1", "user");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "found wrong key of other namespace");
	ksDel(ks);

	keyDel(specKey);
}

static void test_lookup()
{
	printf ("Test lookup namespace\n");

	Key *s;
	Key *p;
	Key *d;
	Key *u;
	Key *y;
	KeySet *ks= ksNew(20,
		s = keyNew("spec/abc", KEY_END),
		p = keyNew("proc/abc", KEY_END),
		d = keyNew("dir/abc", KEY_END),
		u = keyNew("user/abc", KEY_END),
		y = keyNew("system/abc", KEY_END),
		KS_END);

	Key *k = ksLookupByName(ks, "/abc", 0);
	succeed_if (k == p, "did not find proc key");
	printf ("%s\n", keyName(k));

	ksDel(ks);
}


int main(int argc, char** argv)
{
	printf("SPEC  TESTS\n");
	printf("==========================\n\n");

	init (argc, argv);

	test_lookupSingle();
	test_lookupChain();
	test_lookupDefault();
	test_lookupNoascading();
	test_lookupDefaultCascading();
	test_lookupLongChain();
	test_lookupCascading();
	test_lookupNamespace();
	test_lookup();

	printf("\n%s RESULTS: %d test(s) done. %d error(s).\n",
			argv[0], nbTest, nbError);

	return nbError;
}

