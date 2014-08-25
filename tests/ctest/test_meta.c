#include <tests_internal.h>

static void test_ro()
{
	Key *key;

	key = keyNew(KEY_END);
	key->flags |= KEY_FLAG_RO_VALUE;

	succeed_if (keySetString(key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary(key, "a", 2) == -1, "read only string, not allowed to set");

	key->flags |= KEY_FLAG_RO_NAME;
	succeed_if (keySetName(key, "user") == -1, "read only name, not allowed to set");

	key->flags |= KEY_FLAG_RO_META;
	succeed_if (keySetMeta(key, "meta", "value") == -1, "read only meta, not allowed to set");

	keyDel (key);
}

int main(int argc, char** argv)
{
	printf("KEY META     TESTS\n");
	printf("==================\n\n");

	init (argc, argv);
	test_ro();


	printf("\ntest_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

