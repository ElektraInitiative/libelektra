/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdbproposal.h>
#include <tests_internal.h>

static void test_ro ()
{
	Key * key;

	key = keyNew (0);
	key->flags |= KEY_FLAG_RO_VALUE;

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	key->flags |= KEY_FLAG_RO_NAME;
	succeed_if (keySetName (key, "user") == -1, "read only name, not allowed to set");

	key->flags |= KEY_FLAG_RO_META;
	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");

	keyDel (key);
}

static void test_uid ()
{
	Key * key;

	key = keyNew ("user/uid", KEY_UID, 100, KEY_END);
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "100");
	succeed_if (keyGetUID (key) == 100, "uid was not set correctly");

	succeed_if (keySetUID (key, 101) == 0, "could not set uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "101");
	succeed_if (keyGetUID (key) == 101, "uid was not set correctly");

	succeed_if (keySetUID (key, 0) == 0, "could not set uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "0");
	succeed_if (keyGetUID (key) == 0, "uid was not set correctly");

	succeed_if (keySetUID (key, (uid_t)-1) == 0, "could not set uid");
	warn_if_fail (!strcmp (keyValue (keyGetMeta (key, "uid")), "-1"),
		      "this is for 64bit, other platforms might have other results here");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "102") == sizeof ("102"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "102");
	succeed_if (keyGetUID (key) == 102, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x") == sizeof ("x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x1") == sizeof ("x1"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "x1");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "2000000") == sizeof ("2000000"), "could not set large uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "2000000");
	succeed_if (keyGetUID (key) == 2000000, "large uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "1x") == sizeof ("1x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "1x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "50x") == sizeof ("50x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "50x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	keyDel (key);

	key = keyNew ("user/uid", KEY_END);
	succeed_if (keyValue (keyGetMeta (key, "uid")) == 0, "got value, but uid was not set up to now");
	succeed_if (keyGetUID (key) == (uid_t)-1, "got value, but uid was not set up to now");

	keyDel (key);
}


static void test_comment ()
{
	Key * key;
	char ret[10];

	succeed_if (key = keyNew (0), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "No comment up to now");

	succeed_if (keySetComment (key, 0) == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "") == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "mycom") == sizeof ("mycom"), "could not set comment");
	succeed_if_same_string (keyValue (keyGetMeta (key, "comment")), "mycom");
	succeed_if_same_string (keyComment (key), "mycom");
	succeed_if (keyGetCommentSize (key) == sizeof ("mycom"), "My comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, 1) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, sizeof ("mycom")) == sizeof ("mycom"), "Could not get my comment");
	succeed_if_same_string (ret, "mycom");
	succeed_if (keyDel (key) == 0, "could not delete key");
}

static void test_owner ()
{
	Key * key;

	succeed_if (key = keyNew (0), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("system/key", KEY_END), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner (key, "markus") == sizeof ("markus"), "could not set owner markus");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "markus");
	succeed_if_same_string (keyOwner (key), "markus");
	succeed_if (keyDel (key) == 0, "could not delete key");


	succeed_if (key = keyNew ("user:markus/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner (key, "markus") == sizeof ("markus"), "could not set owner markus");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "markus");
	succeed_if_same_string (keyOwner (key), "markus");
	succeed_if (keyDel (key) == 0, "could not delete key");

	setenv ("USER", "markus", 1);
	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key with env");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key with env");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key with env");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key with env");
	succeed_if (keySetMeta (key, "owner", "myowner") == 8, "owner set for empty key with env");
	succeed_if_same_string (keyString (keyGetMeta (key, "owner")), "myowner");
	succeed_if (keyDel (key) == 0, "could not delete key with env");
}

static void test_mode ()
{
	Key * key;

	key = keyNew ("user/mode", KEY_MODE, 0100, KEY_END);
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "100");
	succeed_if (keyGetMode (key) == 0100, "mode was not set correctly");

	succeed_if (keySetMode (key, 0101) == 0, "could not set mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "101");
	succeed_if (keyGetMode (key) == 0101, "mode was not set correctly");

	succeed_if (keySetMode (key, 0) == 0, "could not set mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0");
	succeed_if (keyGetMode (key) == 0, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "102") == sizeof ("102"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "102");
	succeed_if (keyGetMode (key) == 0102, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "0103") == sizeof ("0103"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0103");
	succeed_if (keyGetMode (key) == 0103, "mode was not set correctly with leading octal 0");

	succeed_if (keySetMeta (key, "mode", "x") == sizeof ("x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "x1") == sizeof ("x1"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "x1");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

#if SIZEOF_MODE_T > 2
	succeed_if (keySetMeta (key, "mode", "2000000") == sizeof ("2000000"), "could not set large mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "2000000");
	succeed_if (keyGetMode (key) == 02000000, "large mode was not set correctly");
#endif

	succeed_if (keySetMeta (key, "mode", "1x") == sizeof ("1x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "1x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "50x") == sizeof ("50x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "50x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	keyDel (key);

	key = keyNew ("user/mode", KEY_END);
	succeed_if (keyValue (keyGetMeta (key, "mode")) == 0, "got value, but mode was not set up to now");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "KDB_FILE_MODE not default on new key");

	succeed_if (keySetMeta (key, "mode", "") == sizeof (""), "could not set large mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "empty mode should also yield default");

	keyDel (key);
}

static void test_metaKeySet ()
{
	Key * key = keyNew ("user/test", KEY_END);
	keySetMeta (key, "meta/test1", "value1");
	keySetMeta (key, "meta/test2", "value2");
	keySetMeta (key, "meta/test3", "value3");

	KeySet * metaKeys = elektraKeyGetMetaKeySet (key);

	/* test whether the meta keyset contains all keys */
	Key * metaKey = ksLookupByName (metaKeys, "meta/test1", KDB_O_NONE);
	exit_if_fail (metaKey, "the first meta key was not found in the meta keyset");
	succeed_if (!strcmp (keyString (metaKey), "value1"), "the first meta key in the meta keyset has a wrong value");

	metaKey = ksLookupByName (metaKeys, "meta/test2", KDB_O_NONE);
	exit_if_fail (metaKey, "the second meta key was not found in the meta keyset");
	succeed_if (!strcmp (keyString (metaKey), "value2"), "the second meta key in the meta keyset has a wrong value");

	metaKey = ksLookupByName (metaKeys, "meta/test3", KDB_O_NONE);
	exit_if_fail (metaKey, "the third meta key was not found in the meta keyset");
	succeed_if (!strcmp (keyString (metaKey), "value3"), "the third meta key in the meta keyset has a wrong value");

	/* test whether the meta keyset is affected by deletions */
	ksPop (metaKeys);

	const Key * deletedKey = keyGetMeta (key, "meta/test3");
	exit_if_fail (deletedKey, "key deleted from the meta keyset is not present on the original key anymore");
	succeed_if (!strcmp (keyString (deletedKey), "value3"), "key deleted from the meta keyset has a wrong value afterwards");

	ksDel (metaKeys);
	metaKeys = elektraKeyGetMetaKeySet (key);
	ksRewind (metaKeys);
	metaKey = ksNext (metaKeys);
	keySetString (metaKey, "newvalue");

	const Key * modifiedKey = keyGetMeta (key, "meta/test1");
	succeed_if (!strcmp (keyString (modifiedKey), "value1"),
		    "meta key has incorrect value after a key from the meta keyset was modified");

	ksDel (metaKeys);
	keyDel (key);
}

int main (int argc, char ** argv)
{
	printf ("KEY META     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	test_ro ();

	test_uid ();
	test_comment ();
	test_owner ();
	test_mode ();
	test_metaKeySet ();

	printf ("\ntest_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
