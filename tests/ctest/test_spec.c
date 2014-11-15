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
	succeed_if_same_string(keyName(k), "user/abc");
	succeed_if_same_string(keyString(k), "xyz");

	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");
	keySetMeta(specKey, "default", "");
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find default key again");

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
			KDB_O_CASCADING_NAME,
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
	elektraKeySetName(specKey, "/else", KDB_O_CASCADING_NAME);
	succeed_if(ksLookup(ks, specKey, KDB_O_SPEC) == k, "did not find key itself");

	keyDel(specKey);
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
	test_lookupLongChain();
	test_lookupCascading();

	printf("\n%s RESULTS: %d test(s) done. %d error(s).\n",
			argv[0], nbTest, nbError);

	return nbError;
}

