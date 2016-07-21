/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ksPopAtCursor ()
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

static void test_ksToArray ()
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

static void test_keyAsCascading ()
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

static void test_keyGetLevelsBelow ()
{
	printf ("test keyGetLevelsBelow\n");
	Key * parent = keyNew ("system/parent", KEY_END);
	Key * user = keyNew ("user/parent", KEY_END);
	Key * oneLvl = keyNew ("system/parent/child", KEY_END);
	Key * threeLvl = keyNew ("system/parent/child1/child2/child3", KEY_END);
	succeed_if (keyGetLevelsBelow (parent, oneLvl) == 1, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, threeLvl) == 3, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, parent) == 0, "getLevelsBelow returned wrong value");
	succeed_if (keyGetLevelsBelow (parent, user) == 0, "getLevelsBelow returned wrong value");
	keyDel (parent);
	keyDel (user);
	keyDel (oneLvl);
	keyDel (threeLvl);
}

static void test_keyRel2 ()
{
	printf ("test keyRel2\n");

	Key * systemParent = keyNew ("system/parent", KEY_END);
	Key * userParent = keyNew ("system/parent", KEY_END);
	Key * systemChild = keyNew ("system/parent/child", KEY_END);
	Key * systemGrandChild = keyNew ("system/parent/child/grandchild", KEY_END);
	Key * userChild = keyNew ("user/parent/child", KEY_END);
	Key * userGrandChild = keyNew ("user/parent/child/grandchild", KEY_END);
	Key * cascadingChild = keyNew ("/parent/child", KEY_CASCADING_NAME, KEY_END);
	Key * cascadingGrandChild = keyNew ("/parent/child/grandchild", KEY_CASCADING_NAME, KEY_END);
	Key * systemSilbling = keyNew ("system/silbling", KEY_END);
	Key * userSilbling = keyNew ("user/silbling", KEY_END);
	Key * cascadingSilbling = keyNew ("/silbling", KEY_END);
	Key * systemNephew = keyNew ("system/silbling/nephew", KEY_END);
	Key * userNephew = keyNew ("user/silbling/nephew", KEY_END);
	Key * cascadingNephew = keyNew ("/silbling/nephew", KEY_CASCADING_NAME, KEY_END);
	Key * systemGrandNephew = keyNew ("system/silbling/nephew/grandnephew", KEY_END);
	Key * userGrandNephew = keyNew ("user/silbling/nephew/grandnephew", KEY_END);
	Key * cascadingGrandNephew = keyNew ("/silbling/nephew/grandnephew", KEY_CASCADING_NAME, KEY_END);

	succeed_if (keyRel2 (systemParent, systemChild, ELEKTRA_REL_BELOW_SAME_NS) == 1, "ELEKTRA_REL_BELOW_SAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userChild, ELEKTRA_REL_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userChild, ELEKTRA_REL_BELOW_IGNORE_NS) == 1, "ELEKTRA_REL_BELOW_IGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingChild, ELEKTRA_REL_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingChild, ELEKTRA_REL_BELOW_CASCADING_NS) == 1,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 with cascading child failed\n");
	succeed_if (keyRel2 (systemParent, systemGrandChild, ELEKTRA_REL_BELOW_SAME_NS) == 2, "ELEKTRA_REL_BELOW_SAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userGrandChild, ELEKTRA_REL_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userGrandChild, ELEKTRA_REL_BELOW_IGNORE_NS) == 2,
		    "ELEKTRA_REL_BELOW_IGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingGrandChild, ELEKTRA_REL_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingGrandChild, ELEKTRA_REL_BELOW_CASCADING_NS) == 2,
		    "ELEKTRA_REL_BELOW_SAME_NS keyRel2 with cascading child failed\n");
	succeed_if (keyRel2 (systemParent, userParent, ELEKTRA_REL_BELOW_IGNORE_NS) == 0,
		    "ELEKTRA_REL_BELOW_IGNORE_NS keyRel2 with silblings should have returned 0\n");
	succeed_if (keyRel2 (systemParent, systemChild, ELEKTRA_REL_DIRECT_BELOW_SAME_NS) == 1,
		    "ELEKTRA_REL_DIRECT_BELOW_SAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userChild, ELEKTRA_REL_DIRECT_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_DIRECT_BELOW_SAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userChild, ELEKTRA_REL_DIRECT_BELOW_IGNORE_NS) == 1,
		    "ELEKTRA_REL_DIRECT_BELOW_IGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingChild, ELEKTRA_REL_DIRECT_BELOW_SAME_NS) == 0,
		    "ELEKTRA_REL_DIRECT_BELOW_SAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingChild, ELEKTRA_REL_DIRECT_BELOW_CASCADING_NS) == 1,
		    "ELEKTRA_REL_DIRECT_BELOW_SAME_NS keyRel2 with cascading child failed\n");

	succeed_if (keyRel2 (systemParent, systemSilbling, ELEKTRA_REL_SILBLING_SAME_NS) == 1,
		    "ELEKTRA_REL_SILBLINGSAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userSilbling, ELEKTRA_REL_SILBLING_SAME_NS) == 0,
		    "ELEKTRA_REL_SILBLINGSAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userSilbling, ELEKTRA_REL_SILBLING_IGNORE_NS) == 1,
		    "ELEKTRA_REL_SILBLINGIGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingSilbling, ELEKTRA_REL_SILBLING_SAME_NS) == 0,
		    "ELEKTRA_REL_SILBLINGSAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingSilbling, ELEKTRA_REL_SILBLING_CASCADING_NS) == 1,
		    "ELEKTRA_REL_SILBLINGSAME_NS keyRel2 with cascading child failed\n");

	succeed_if (keyRel2 (systemParent, systemNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 1, "NephewSAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 0, "NephewSAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userNephew, ELEKTRA_REL_NEPHEW_IGNORE_NS) == 1, "NephewIGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 0,
		    "NephewSAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingNephew, ELEKTRA_REL_NEPHEW_CASCADING_NS) == 1,
		    "NephewSAME_NS keyRel2 with cascading child failed\n");

	succeed_if (keyRel2 (systemParent, systemGrandNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 2, "NephewSAME_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, userGrandNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 0, "NephewSAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, userGrandNephew, ELEKTRA_REL_NEPHEW_IGNORE_NS) == 2, "NephewIGNORE_NS keyRel2 failed\n");
	succeed_if (keyRel2 (systemParent, cascadingGrandNephew, ELEKTRA_REL_NEPHEW_SAME_NS) == 0,
		    "NephewSAME_NS keyRel2 with cascading child should have failed\n");
	succeed_if (keyRel2 (systemParent, cascadingGrandNephew, ELEKTRA_REL_NEPHEW_CASCADING_NS) == 2,
		    "NephewSAME_NS keyRel2 with cascading child failed\n");

	succeed_if (keyRel2 (systemParent, systemGrandChild, ELEKTRA_REL_NEPHEW_SAME_NS) == 0,
		    "NephewSAME_NS keyRel2 should have failed\n");
	succeed_if (keyRel2 (systemParent, systemChild, ELEKTRA_REL_SILBLING_SAME_NS) == 0,
		    "ELEKTRA_REL_SILBLINGSAME_NS keyRel2 should have failed\n");

	keyDel (systemParent);
	keyDel (userParent);
	keyDel (systemChild);
	keyDel (systemGrandChild);
	keyDel (userChild);
	keyDel (userGrandChild);
	keyDel (cascadingChild);
	keyDel (cascadingGrandChild);
	keyDel (systemSilbling);
	keyDel (userSilbling);
	keyDel (cascadingSilbling);
	keyDel (systemNephew);
	keyDel (userNephew);
	keyDel (cascadingNephew);
	keyDel (systemGrandNephew);
	keyDel (userGrandNephew);
	keyDel (cascadingGrandNephew);
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
	test_keyRel2 ();

	printf ("\ntest_proposal RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
}
