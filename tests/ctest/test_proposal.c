/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ksPopAtCursor()
{
	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *ks_c = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		KS_END);
	ksRewind(ks);
	ksNext(ks);
	ksNext(ks);
	cursor_t c = ksGetCursor(ks);
	keyDel (ksPopAtCursor(ks, c));
	succeed_if(ksCurrent(ks) == 0, "cursor position wrong");

	compare_keyset(ks, ks_c);
	ksDel(ks);
	ksDel(ks_c);
}

static void test_ksToArray()
{
	KeySet *ks = ksNew (5,
			keyNew ("user/test1", KEY_END),
			keyNew ("user/test2", KEY_END),
			keyNew ("user/test3", KEY_END),
	KS_END);

	Key **keyArray = calloc (ksGetSize (ks), sizeof (Key *));
	elektraKsToMemArray(ks, keyArray);

	succeed_if_same_string ("user/test1", keyName(keyArray[0]));
	succeed_if_same_string ("user/test2", keyName(keyArray[1]));
	succeed_if_same_string ("user/test3", keyName(keyArray[2]));

	/* test if cursor is restored */
	ksNext(ks);
	cursor_t cursor = ksGetCursor(ks);
	elektraKsToMemArray(ks, keyArray);

	succeed_if (ksGetCursor(ks) == cursor, "cursor was not restored");

	succeed_if (elektraKsToMemArray(0, keyArray) < 0, "wrong result on null pointer");
	succeed_if (elektraKsToMemArray(ks, 0) < 0, "wrong result on null buffer");
	KeySet *empty = ksNew(0, KS_END);
	succeed_if (elektraKsToMemArray(empty, keyArray) == 0, "wrong result on empty keyset");
	ksDel(empty);

	free (keyArray);
	ksDel (ks);
}

int main(int argc, char** argv)
{
	printf("KEY PROPOSAL TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_ksPopAtCursor();
	test_ksToArray();

	printf("\ntest_proposal RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
}
