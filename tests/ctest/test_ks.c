#include <tests_internal.h>

ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from);

static KeySet * set_a ()
{
	return ksNew(16,
		keyNew ("user/0", KEY_END),
		keyNew ("user/a", KEY_END),
		keyNew ("user/a/a", KEY_END),
		keyNew ("user/a/a/a", KEY_END),
		keyNew ("user/a/a/b", KEY_END),
		keyNew ("user/a/b", KEY_END),
		keyNew ("user/a/b/a", KEY_END),
		keyNew ("user/a/b/b", KEY_END),
		keyNew ("user/a/c", KEY_END),
		keyNew ("user/a/d", KEY_END),
		keyNew ("user/a/x/a", KEY_END),
		keyNew ("user/a/x/b", KEY_END),
		keyNew ("user/a/x/c", KEY_END),
		keyNew ("user/a/x/c/a", KEY_END),
		keyNew ("user/a/x/c/b", KEY_END),
		keyNew ("user/x", KEY_END),
		KS_END);
}

static void test_copy()
{
	printf ("Testing operation copy (internal)\n");

	KeySet *copy[17][17];
#include "data_copy.c"

	KeySet *current;

	for (int i=0; i<17; ++i)
	{
		for (int j=0; j<17; ++j)
		{
			/* There are some cases which contain duplicates, we have to jump these...*/
			if (i>j) goto cleanup;
			if (i==0 && j==16) goto cleanup;

			current = set_a();
			/* Some blocks are lost in the next operation */
			succeed_if (ksCopyInternal (current, i, j) != -1, "ksCopyInternal failed");
			compare_keyset(current, copy[i][j]);
			ksDel (current);

cleanup:
			ksDel (copy[i][j]);
		}
	}
}

#define MAX_SIZE 200
static void test_ksCommonParentName()
{
	char ret [MAX_SIZE+1];
	KeySet *ks = ksNew (10,
		keyNew("system/sw/xorg/Monitors/Monitor1/vrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor1/hrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor2/vrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor2/hrefresh",0),
		keyNew("system/sw/xorg/Devices/Device1/driver",0),
		keyNew("system/sw/xorg/Devices/Device1/mode",0),KS_END);

	printf ("Test common parentname\n");

	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could not find correct parentname");
	succeed_if (strcmp (ret, "system/sw/xorg") == 0, "parentname not correct");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system",0),
		keyNew("user",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if (strcmp (ret, "") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/some/thing",0),
		keyNew("system/other/thing",0), KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 7, "could find correct parentname");
	succeed_if (strcmp (ret, "system") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if (strcmp (ret, "system/here/in/deep/goes/ok") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if (strcmp (ret, "") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if (strcmp (ret, "user/unique/thing") == 0, "parentname not empty");
	ksDel (ks);
}

static void test_elektraRenameKeys()
{
	KeySet *ks= ksNew(20,
		keyNew("system/some/common/prefix", KEY_END),
		keyNew("system/some/common/prefix/dir", KEY_END),
		keyNew("system/some/common/prefix/dir/keya", KEY_END),
		keyNew("system/some/common/prefix/some", KEY_VALUE, "huhu", KEY_END),
		keyNew("system/some/common/prefix/other", KEY_END),
		KS_END);
	KeySet *cmp= ksNew(20,
		keyNew("user/x/dir", KEY_END),
		keyNew("user/x/dir/keya", KEY_END),
		keyNew("user/x/some", KEY_VALUE, "huhu", KEY_END),
		keyNew("user/x/other", KEY_END),
		KS_END);

	KeySet *result = elektraRenameKeys(ks, "user/x");
	compare_keyset(result, cmp);
	// output_keyset(result);
	ksDel(cmp);
	ksDel(result);
	ksDel(ks);

	ks= ksNew(0, KS_END);
	result = elektraRenameKeys(ks, "user");
	output_keyset(result);

	ksDel(result);
	ksDel(ks);
}

static void test_elektraEmptyKeys()
{
	Key *key = keyNew("", KEY_END);
	KeySet *ks = ksNew(0, KS_END);

	elektraKeySetName(key, "", KDB_O_META_NAME | KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "");
	succeed_if(key->key != 0, "null pointer?");
	ksAppendKey(ks, key);

	succeed_if(ksLookup(ks, key, 0) == key, "could not find empty key");

	ksDel(ks);
}

int main()
{
	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	test_ksCommonParentName();
	test_elektraRenameKeys();
	test_elektraEmptyKeys();

	return nbError;
}

