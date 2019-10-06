/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ksPopAtCursor (void)
{
	KeySet * ks = ksNew (5, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			     keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END), KS_END);
	KeySet * ks_c = ksNew (5, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			       keyNew ("system/valid/key1", KEY_END), KS_END);
	ksRewind (ks);
	ksNext (ks);
	ksNext (ks);
	cursor_t c = ksGetCursor (ks);
	keyDel (ksPopAtCursor (ks, c));
	succeed_if (ksCurrent (ks) == 0, "cursor position wrong");

	compare_keyset (ks, ks_c);
	ksDel (ks);
	ksDel (ks_c);
}

static void test_ksToArray (void)
{
	KeySet * ks = ksNew (5, keyNew ("user/test1", KEY_END), keyNew ("user/test2", KEY_END), keyNew ("user/test3", KEY_END), KS_END);

	Key ** keyArray = calloc (ksGetSize (ks), sizeof (Key *));
	elektraKsToMemArray (ks, keyArray);

	succeed_if_same_string ("user/test1", keyName (keyArray[0]));
	succeed_if_same_string ("user/test2", keyName (keyArray[1]));
	succeed_if_same_string ("user/test3", keyName (keyArray[2]));

	/* test if cursor is restored */
	ksNext (ks);
	cursor_t cursor = ksGetCursor (ks);
	elektraKsToMemArray (ks, keyArray);

	succeed_if (ksGetCursor (ks) == cursor, "cursor was not restored");

	succeed_if (elektraKsToMemArray (0, keyArray) < 0, "wrong result on null pointer");
	succeed_if (elektraKsToMemArray (ks, 0) < 0, "wrong result on null buffer");
	KeySet * empty = ksNew (0, KS_END);
	succeed_if (elektraKsToMemArray (empty, keyArray) == 0, "wrong result on empty keyset");
	ksDel (empty);

	elektraFree (keyArray);
	ksDel (ks);
}

static void test_keyAsCascading (void)
{
	printf ("test keyAsCascading\n");
	Key * system = keyNew ("system", KEY_END);
	Key * user = keyNew ("user/", KEY_END);
	Key * sysKey = keyNew ("system/test", KEY_END);
	Key * cascadingKey = keyNew ("/test", KEY_END);
	Key * ret;
	ret = keyAsCascading (system);
	succeed_if (!strcmp (keyName (ret), "/"), "Failed turning \"system\" into a cascading Key");
	keyDel (ret);
	ret = keyAsCascading (user);
	succeed_if (!strcmp (keyName (ret), "/"), "Failed turning \"user/\" into a cascading Key");
	keyDel (ret);
	ret = keyAsCascading (sysKey);
	succeed_if (!strcmp (keyName (ret), "/test"), "Failed turning \"system/test\" into a cascading Key");
	keyDel (ret);
	ret = keyAsCascading (cascadingKey);
	succeed_if (!strcmp (keyName (ret), "/test"), "Failed turning \"/test\" into a cascading Key");
	keyDel (ret);
	keyDel (system);
	keyDel (user);
	keyDel (sysKey);
	keyDel (cascadingKey);
}

static void test_keyGetLevelsBelow (void)
{
	printf ("test keyGetLevelsBelow\n");
	Key * grandparent = keyNew ("system/grandparent", KEY_END);
	Key * parent = keyNew ("system/grandparent/parent", KEY_END);
	Key * user = keyNew ("user/grandparent/parent", KEY_END);
	Key * oneLvl = keyNew ("system/grandparent/parent/child", KEY_END);
	Key * threeLvl = keyNew ("system/grandparent/parent/child1/child2/child3", KEY_END);
	succeed_if (keyGetLevelsBelow (parent, oneLvl) == 1, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, threeLvl) == 3, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, parent) == 0, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, user) == 0, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, grandparent) == 0, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (grandparent, parent) == 1, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (grandparent, oneLvl) == 2, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (grandparent, threeLvl) == 4, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (threeLvl, grandparent) == 0, "getLevelsBelow returned wrong value");
	keyDel (grandparent);
	keyDel (parent);
	keyDel (user);
	keyDel (oneLvl);
	keyDel (threeLvl);
}

int main (int argc, char ** argv)
{
	printf ("KEY PROPOSAL TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksPopAtCursor ();
	test_ksToArray ();

	test_keyAsCascading ();
	test_keyGetLevelsBelow ();

	printf ("\ntest_proposal RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
}
