#include <tests_internal.h>

static void test_ksLookupCase()
{
	printf ("Test bug lookup with case\n");
	KeySet *ks = ksNew(32,
			keyNew("system/ay/key", KEY_VALUE, "aykey", KEY_END),
			keyNew("system/mY/kex", KEY_VALUE, "mykex", KEY_END),
			keyNew("system/xy/key", KEY_VALUE, "xykey", KEY_END),
			keyNew("system/My/key", KEY_VALUE, "Mykey", KEY_END),
			KS_END);
	Key *found = ksLookupByName (ks, "system/my/key", KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key (binary search fails when ignoring case)");
	ksDel (ks);
}

static void test_ksLookupOwner()
{
	printf ("Test bug lookup with owner\n");
	Key *found = 0;
	KeySet *ks = ksNew(32,
			keyNew("user:fritz/my/key", KEY_VALUE, "fritz", KEY_END),
			keyNew("user:frotz/my/key", KEY_VALUE, "frotz", KEY_END),
			keyNew("user/my/key", KEY_VALUE, "current", KEY_END), KS_END);

	found = ksLookupByName (ks, "user/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "current"), "got wrong key");

	found = ksLookupByName (ks, "user:fritz/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "fritz"), "got wrong key");

	found = ksLookupByName (ks, "user:frotz/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "frotz"), "got wrong key");

	found = ksLookupByName (ks, "user:fretz/my/key", KDB_O_WITHOWNER);
	succeed_if (found == 0, "found non existing key");

	found = ksLookupByName (ks, "user/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:fritz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:frotz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:fretz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue(found), "fritz"), "binary search seems to be non-deterministic");

	ksDel (ks);
}

int main(int argc, char** argv)
{
	printf("KEYSET ORDERING      TESTS\n");
	printf("==========================\n\n");

	init (argc, argv);

	test_ksLookupCase();
	test_ksLookupOwner();

	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

