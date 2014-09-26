#include <tests_internal.h>

ssize_t ksCopyInternal(KeySet *ks, size_t to, size_t from);

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
	succeed_if_same_string (ret, "system/sw/xorg");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system",0),
		keyNew("user",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/some/thing",0),
		keyNew("system/other/thing",0), KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 7, "could find correct parentname");
	succeed_if_same_string (ret, "system");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "system/here/in/deep/goes/ok");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if_same_string (ret, "");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if_same_string (ret, "user/unique/thing");
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

