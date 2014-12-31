#include <tests_internal.h>

static void test_invalid(char *text, Key *which)
{
	printf ("Test %s\n", text);

	KeySet *myConfig = ksNew(0, KS_END);
	Key *parentKey = keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(parentKey);

	exit_if_fail(handle, "got no handle");

	succeed_if(kdbGet(handle, myConfig, parentKey) >= 0, "get failed");

	int s = ksGetSize(myConfig);
	ksAppendKey(myConfig, which);
	succeed_if(ksGetSize(myConfig) == s+1, "key not added");
	printf ("size: %d\n", s);

	//TODO: should fail
	succeed_if(kdbSet(handle, myConfig, parentKey) >= 0, "set did not fail");
	succeed_if(ksGetSize(myConfig) == s+1, "key disappeared after set");
	output_warnings(parentKey);
	//TODO: should point to key
	// succeed_if(ksCurrent(myConfig) == which, "does not point to correct key");

	ksDel (myConfig);

	kdbClose(handle, parentKey);
	keyDel(parentKey);

}

int main(int argc, char** argv)
{
	printf("KDB INVALID  TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_invalid("cascading root key", keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END));
	test_invalid("cascading other key", keyNew(ELEKTRA_TEST_ROOT "/other", KEY_CASCADING_NAME, KEY_END));
	test_invalid("meta key", keyNew("meta/" ELEKTRA_TEST_ROOT, KEY_META_NAME, KEY_END));
	test_invalid("meta other key", keyNew("meta/" ELEKTRA_TEST_ROOT "/other", KEY_META_NAME, KEY_END));

	printf("\ntest_invalid RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
