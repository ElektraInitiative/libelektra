/**
 * @file
 *
 * @brief Test suite for meta information.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.h>

static void test_basic (void)
{
	Key * key;
	key = keyNew ("user:/key_with_meta", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyGetMeta (key, "hello") == 0, "hello was not set up to now");
	succeed_if (keyGetMeta (key, "error") == 0, "hello was not set up to now");

	keySetMeta (key, "hello", "hello_world");
	succeed_if_same_string (keyValue (keyGetMeta (key, "hello")), "hello_world");

	keySetMeta (key, "mode", "0644");
	succeed_if_same_string (keyValue (keyGetMeta (key, "hello")), "hello_world");

	keySetMeta (key, "time", "1271234264");
	succeed_if_same_string (keyValue (keyGetMeta (key, "hello")), "hello_world");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0644");
	succeed_if_same_string (keyValue (keyGetMeta (key, "time")), "1271234264");

	keySetMeta (key, "hello", "between");
	succeed_if_same_string (keyValue (keyGetMeta (key, "hello")), "between");

	keySetMeta (key, "hello", 0);
	succeed_if (keyValue (keyGetMeta (key, "hello")) == 0, "could not remove metadata");

	keySetMeta (key, "hello", "goodbye");
	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "goodbye"), "could not set meta information again (2x)");

	keySetMeta (key, "empty", "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "empty")), "");

	keySetMeta (key, "owner", "hugo");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "hugo");

	keySetMeta (key, "mode", "775");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "hugo");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "775");

	keySetMeta (key, "", "empty");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "empty");

	keySetMeta (key, "", "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "");

	keySetMeta (key, "", 0);
	succeed_if (keyValue (keyGetMeta (key, "")) == 0, "could not remove empty metadata");


	keyDel (key);
}

static void test_null_pointer (void)
{
	Key * key;
	KeySet * ks;
	key = keyNew ("user:/test1", KEY_END);
	exit_if_fail (key, "could not create new key");

	succeed_if (keyGetMeta (0, "test") == 0, "Could get meta of NULL Key");
	succeed_if (keyGetMeta (key, 0) == 0, "Could get meta of NULL metaName");

	succeed_if ((ks = keyMeta (0)) == 0, "Could get metadata of NULL Key");
	succeed_if (ksAtCursor (ks, 0) == 0, "Could get next meta Key of NULL key");

	succeed_if (keyCopyMeta (0, key, "test") == -1, "Could copy metadata to NULL Key");
	succeed_if (keyCopyMeta (key, 0, "test") == -1, "Could copy metadata from NULL Key");

	succeed_if (keyCopyAllMeta (0, key) == -1, "Could copy all metadata to NULL Key");
	succeed_if (keyCopyAllMeta (key, 0) == -1, "Could copy all metadata from NULL Key");

	succeed_if (keySetMeta (0, "test", "test"), "Could set metadata to NULL Key");
	succeed_if (keySetMeta (key, 0, "test"), "Could set metadata with NULL metaName");

	keyDel (key);
}

static void test_iterate (void)
{
	Key * key;
	KeySet * metaKeys;

	key = keyNew ("user:/test", KEY_END);
	exit_if_fail (key, "could not create new key");

	metaKeys = keyMeta (key);
	succeed_if (ksAtCursor (metaKeys, 0) == 0, "Could get metakey from keyset, even if it should be empty");

	keySetMeta (key, "meta1", "meta_value");
	metaKeys = keyMeta (key);
	succeed_if_same_string (keyName (ksAtCursor (metaKeys, 0)), "meta:/meta1");
	succeed_if_same_string (keyValue (ksAtCursor (metaKeys, 0)), "meta_value");

	succeed_if (ksAtCursor (metaKeys, 1) == 0, "Could get metakey from keyset, even if it should be empty at 2. position");
	succeed_if (ksAtCursor (metaKeys, 2) == 0, "Could get metakey from keyset, even if it should be empty at 3. position");
	succeed_if (ksAtCursor (metaKeys, 3) == 0, "Could get metakey from keyset, even if it should be empty at 4. position");

	keyDel (key);
}

static void test_size (void)
{
	Key * key;
	char * buffer;

	key = keyNew ("user:/test", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "hello")) == 0, "hello was not set up to now");
	succeed_if (keyGetValueSize (keyGetMeta (key, "hello")) == -1, "got wrong size for empty metavalue");

	keySetMeta (key, "hello", "hello_world");
	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "hello_world"), "could not receive previously set meta information");
	succeed_if (keyGetValueSize (keyGetMeta (key, "hello")) == sizeof ("hello_world"), "got wrong size");

	keySetMeta (key, "mode", "0644");
	keySetMeta (key, "time", "1271234264");
	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "hello_world"), "meta info changed unexpectly");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0644");
	succeed_if (keyGetValueSize (keyGetMeta (key, "mode")) == sizeof ("0644"), "got wrong size");
	succeed_if_same_string (keyValue (keyGetMeta (key, "time")), "1271234264");
	succeed_if (keyGetValueSize (keyGetMeta (key, "time")) == sizeof ("1271234264"), "got wrong size");

	keySetMeta (key, "hello", "between");
	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "between"), "could not set meta information again");
	succeed_if (keyGetValueSize (keyGetMeta (key, "hello")) == sizeof ("between"), "got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta (key, "hello")));
	succeed_if (keyGetString (keyGetMeta (key, "hello"), buffer, keyGetValueSize (keyGetMeta (key, "hello"))) ==
			    keyGetValueSize (keyGetMeta (key, "hello")),
		    "could not get meta");
	succeed_if_same_string (buffer, "between");
	elektraFree (buffer);


	keySetMeta (key, "hello", 0);
	succeed_if (keyValue (keyGetMeta (key, "hello")) == 0, "could not remove metadata");
	succeed_if (keyGetValueSize (keyGetMeta (key, "hello")) == -1, "got wrong size");

	keySetMeta (key, "hello", "goodbye");
	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "goodbye"), "could not set meta information again (2x)");
	succeed_if (keyGetValueSize (keyGetMeta (key, "hello")) == sizeof ("goodbye"), "got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta (key, "hello")));
	succeed_if (keyGetString (keyGetMeta (key, "hello"), buffer, keyGetValueSize (keyGetMeta (key, "hello"))) ==
			    keyGetValueSize (keyGetMeta (key, "hello")),
		    "could not get meta");
	succeed_if_same_string (buffer, "goodbye");
	elektraFree (buffer);

	keySetMeta (key, "empty", "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "empty")), "");
	succeed_if (keyGetValueSize (keyGetMeta (key, "empty")) == sizeof (""), "got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta (key, "empty")));
	succeed_if (keyGetString (keyGetMeta (key, "empty"), buffer, keyGetValueSize (keyGetMeta (key, "empty"))) ==
			    keyGetValueSize (keyGetMeta (key, "empty")),
		    "could not get meta");
	succeed_if_same_string (buffer, "");
	elektraFree (buffer);

	keySetMeta (key, "", "empty");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "empty");
	succeed_if (keyGetValueSize (keyGetMeta (key, "")) == sizeof ("empty"), "got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta (key, "")));
	succeed_if (keyGetString (keyGetMeta (key, ""), buffer, keyGetValueSize (keyGetMeta (key, ""))) ==
			    keyGetValueSize (keyGetMeta (key, "")),
		    "could not get meta");
	succeed_if_same_string (buffer, "empty");
	elektraFree (buffer);

	keySetMeta (key, "", "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "");
	succeed_if (keyGetValueSize (keyGetMeta (key, "")) == sizeof (""), "got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta (key, "")));
	succeed_if (keyGetString (keyGetMeta (key, ""), buffer, keyGetValueSize (keyGetMeta (key, ""))) ==
			    keyGetValueSize (keyGetMeta (key, "")),
		    "could not get meta");
	succeed_if_same_string (buffer, "");
	elektraFree (buffer);

	keySetMeta (key, "", 0);
	succeed_if (keyValue (keyGetMeta (key, "")) == 0, "could not remove empty metadata");
	succeed_if (keyGetValueSize (keyGetMeta (key, "")) == -1, "got wrong size");


	keyDel (key);
}

static void test_dup (void)
{
	Key * key;
	Key * dup;

	key = keyNew ("user:/orig", KEY_END);
	succeed_if (keySetMeta (key, "test", "some_meta_test") == sizeof ("some_meta_test"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "test")), "some_meta_test");

	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyValue (keyGetMeta (dup, "test")), "some_meta_test");
	succeed_if (keySetMeta (dup, "test", "some_other_meta_test") == sizeof ("some_other_meta_test"), "sizeof meta test wrong");
	succeed_if_same_string (keyValue (keyGetMeta (dup, "test")), "some_other_meta_test");
	succeed_if_same_string (keyValue (keyGetMeta (key, "test")), "some_meta_test");
	keyDel (dup);

	keyDel (key);
}

Key * g_c;

static void j (Key * k)
{
	size_t size = keyGetValueSize (k);
	char * value = elektraMalloc (size);
	int bstring = keyIsString (k);

	// receive key g_c
	memcpy (value, keyValue (k), size);
	keyCopy (k, g_c, KEY_CP_ALL);
	if (bstring)
		keySetString (k, value);
	else
		keySetBinary (k, value, size);
	elektraFree (value);
	// the caller will see the changed key k
	// with the metadata from g_c
}

static void l (Key * k)
{
	// receive g_c
	keyCopyMeta (k, g_c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from g_c
}

static void test_examples (void)
{
	Key * key;
	key = keyNew ("/", KEY_END);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	g_c = keyNew ("/", KEY_END);
	keySetMeta (g_c, "xef", "ybc");
	keySetMeta (g_c, "xop", "yup");

	j (key);

	succeed_if_same_string (keyValue (keyGetMeta (key, "xef")), "ybc");
	succeed_if_same_string (keyValue (keyGetMeta (key, "xop")), "yup");
	succeed_if (keyValue (keyGetMeta (key, "def")) == 0, "old metadata remained");
	succeed_if (keyValue (keyGetMeta (key, "nop")) == 0, "old metadata remained");

	keyDel (key);
	keyDel (g_c);

	key = keyNew ("/", KEY_END);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	g_c = keyNew ("/", KEY_END);
	keySetMeta (g_c, "type", "boolean");
	keySetMeta (g_c, "xop", "yup");

	l (key);

	succeed_if_same_string (keyValue (keyGetMeta (key, "def")), "abc");
	succeed_if_same_string (keyValue (keyGetMeta (key, "nop")), "cup");
	succeed_if_same_string (keyValue (keyGetMeta (key, "type")), "boolean");
	succeed_if (keyValue (keyGetMeta (key, "xop")) == 0, "this metadata was not requested to be copied");

	keyDel (key);
	keyDel (g_c);
}

static void test_copy (void)
{
	printf ("Test key meta copy\n");

	Key * key1;
	Key * key2;

	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keyCopyMeta (key2, key1, "nonexist") == 0, "could not do anything");

	succeed_if (keyValue (keyGetMeta (key2, "nonexist")) == 0, "should not be there");

	keyDel (key1);
	keyDel (key2);

	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (keyCopyMeta (key2, key1, "mymeta") == 1, "could not copy metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (keyCopyMeta (key1, key2, "mymeta") == 1, "did nothing in the end");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (keyCopyMeta (key2, key1, "mymeta") == 1, "could not copy metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") != keyGetMeta (key2, "mymeta"), "reference to another key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue2") == sizeof ("a longer metavalue2"), "could not set metavalue2");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue2");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") != keyGetMeta (key2, "mymeta"), "reference to another key (with another value)");

	keyDel (key1);
	keyDel (key2);

	Key * k;
	Key * c;

	// clang-format off
	k=keyNew ("user:/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the metadata later",
		KEY_META, "mode", "0775",
		KEY_END);
	// clang-format on
	c = keyNew ("user:/metacopy", KEY_END);

	succeed_if (keyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (keyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (keyGetMeta (c, "t") == 0, "could get metakey not there");
	succeed_if (keyGetMeta (c, "a") == 0, "could get metakey not there");

	succeed_if (keyCopyMeta (c, k, "t") == 1, "could not copy metadata");
	succeed_if (keyGetMeta (k, "t") == keyGetMeta (c, "t"), "not the same metadata after copy");

	succeed_if (keyCopyMeta (c, k, "a") == 1, "could not copy metadata");
	succeed_if (keyGetMeta (k, "a") == keyGetMeta (c, "a"), "not the same metadata after copy");

	keyDel (k);
	keyDel (c);


	// clang-format off
	k=keyNew ("user:/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the metadata later",
		KEY_META, "mode", "0775",
		KEY_END);
	c=keyNew ("user:/metacopy",
		KEY_META, "t", "test1",
		KEY_META, "a", "wrong",
		KEY_META, "old", "will stay",
		KEY_END);
	// clang-format on

	succeed_if (keyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (keyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (keyCopyMeta (c, k, "t") == 1, "could not copy metadata");
	succeed_if (keyGetMeta (k, "t") == keyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if_same_string (keyValue (keyGetMeta (k, "t")), "test1");
	succeed_if_same_string (keyValue (keyGetMeta (c, "t")), "test1");

	succeed_if (keyCopyMeta (c, k, "a") == 1, "could not copy metadata");
	succeed_if (keyGetMeta (k, "a") == keyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if_same_string (keyValue (keyGetMeta (k, "a")), "another");
	succeed_if_same_string (keyValue (keyGetMeta (c, "a")), "another");

	succeed_if_same_string (keyValue (keyGetMeta (c, "old")), "will stay");
	succeed_if (keyGetMeta (c, "cya") == 0, "metakey should not be there");
	succeed_if (keyGetMeta (c, "mode") == 0, "metakey should not be there");

	keyDel (k);
	keyDel (c);

	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keySetMeta (key2, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");

	succeed_if (keyCopyMeta (key2, key1, "mymeta") == 0, "could not copy metavalue");

	succeed_if (keyGetMeta (key1, "mymeta") == 0, "value of mymeta is not NULL");
	succeed_if (keyGetMeta (key2, "mymeta") == 0, "value of mymeta has not been cleared");

	keyDel (key1);
	keyDel (key2);
}

static void test_new (void)
{
	Key * key;
	// clang-format off
	key = keyNew ("user:/test",
		KEY_META, "hello", "hello_world",
		KEY_META, "mode", "0644",
		KEY_META, "time", "1271234264",
		KEY_META, "empty", "",
		KEY_META, "", "empty",
		KEY_END);
	// clang-format on

	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "hello_world"), "could not receive previously set meta information");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0644");
	succeed_if_same_string (keyValue (keyGetMeta (key, "time")), "1271234264");
	succeed_if_same_string (keyValue (keyGetMeta (key, "empty")), "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "empty");

	keySetMeta (key, "", "full");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "full");

	keySetMeta (key, "", 0);
	succeed_if (keyValue (keyGetMeta (key, "")) == 0, "could not remove empty metadata");

	keyDel (key);

	// clang-format off
	key = keyNew ("user:/test",
		KEY_META, "hello", "goodbye",
		KEY_META, "mode", "0775",
		KEY_META, "time", "1271939923",
		KEY_META, "empty", "",
		KEY_META, "", "",
		KEY_END);
	// clang-format on

	succeed_if (!strcmp (keyValue (keyGetMeta (key, "hello")), "goodbye"), "could not receive previously set meta information");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0775");
	succeed_if_same_string (keyValue (keyGetMeta (key, "time")), "1271939923");
	succeed_if_same_string (keyValue (keyGetMeta (key, "empty")), "");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "");

	keySetMeta (key, "", "full");
	succeed_if_same_string (keyValue (keyGetMeta (key, "")), "full");

	keySetMeta (key, "", 0);
	succeed_if (keyValue (keyGetMeta (key, "")) == 0, "could not remove empty metadata");

	keyDel (key);
}


static void test_copyall (void)
{
	printf ("Test key meta copy all\n");

	Key * key1;
	Key * key2;

	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keyCopyAllMeta (key2, key1) == 0, "could not do anything");

	succeed_if (keyValue (keyGetMeta (key2, "nonexist")) == 0, "should not be there");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (keyCopyAllMeta (key2, key1) == 1, "could not copy metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (keyCopyAllMeta (key1, key2) == 1, "did nothing in the end");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew ("/", KEY_END), "could not create key");
	succeed_if (key2 = keyNew ("/", KEY_END), "could not create key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (keyCopyAllMeta (key2, key1) == 1, "could not copy metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") == keyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") != keyGetMeta (key2, "mymeta"), "reference to another key");

	succeed_if (keySetMeta (key1, "mymeta", "a longer metavalue2") == sizeof ("a longer metavalue2"), "could not set metavalue2");
	succeed_if_same_string (keyValue (keyGetMeta (key1, "mymeta")), "a longer metavalue2");
	succeed_if_same_string (keyValue (keyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (keyGetMeta (key1, "mymeta") != keyGetMeta (key2, "mymeta"), "reference to another key (with another value)");

	keyDel (key1);
	keyDel (key2);

	Key * k;
	Key * c;

	// clang-format off
	k=keyNew ("user:/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the metadata later",
		KEY_META, "mode", "0775",
		KEY_END);
	// clang-format on
	c = keyNew ("user:/metacopy", KEY_END);

	succeed_if (keyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (keyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (keyGetMeta (c, "t") == 0, "could get metakey not there");
	succeed_if (keyGetMeta (c, "a") == 0, "could get metakey not there");

	succeed_if (keyCopyAllMeta (c, k) == 1, "could not copy metadata");
	succeed_if (keyGetMeta (k, "t") == keyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "a") == keyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "cya") == keyGetMeta (c, "cya"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "mode") == keyGetMeta (c, "mode"), "not the same metadata after copy");
	succeed_if (keyValue (keyGetMeta (k, "nonexist")) == 0, "should not be there");
	succeed_if (keyValue (keyGetMeta (c, "nonexist")) == 0, "should not be there");

	succeed_if (keyCopyAllMeta (c, k) == 1, "could not copy metadata (again)");
	succeed_if (keyGetMeta (k, "t") == keyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "a") == keyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "cya") == keyGetMeta (c, "cya"), "not the same metadata after copy");
	succeed_if (keyGetMeta (k, "mode") == keyGetMeta (c, "mode"), "not the same metadata after copy");
	succeed_if (keyValue (keyGetMeta (k, "nonexist")) == 0, "should not be there");
	succeed_if (keyValue (keyGetMeta (c, "nonexist")) == 0, "should not be there");

	keyDel (k);
	keyDel (c);
}

static void test_type (void)
{
	Key * key;

	succeed_if (key = keyNew ("/", KEY_END), "could not create a new key");
	succeed_if (keyValue (keyGetMeta (key, "binary")) == 0, "wrong type after key creation");
	succeed_if (keySetString (key, "mystring") == sizeof ("mystring"), "could not set string");
	succeed_if (keyValue (keyGetMeta (key, "binary")) == 0, "wrong type after setting string");
	succeed_if (keySetBinary (key, "mystring", sizeof ("mystring")) == sizeof ("mystring"), "could not set binary");
	succeed_if (keyValue (keyGetMeta (key, "binary")) != 0, "wrong type after setting string");

	keyDel (key);
}

static void test_keyMeta (void)
{
	Key * key = keyNew ("/", KEY_END);

	KeySet * meta = keyMeta (key);

	succeed_if (meta, "meta keyset is null");
	succeed_if (ksGetSize (meta) == 0, "meta keyset is not empty");
	keyDel (key);

	// clang-format off
	key = keyNew ("user:/test",
		KEY_META, "hello", "hello_world",
		KEY_META, "mode", "0644",
		KEY_META, "time", "1271234264",
		KEY_META, "empty", "",
		KEY_META, "", "empty",
		KEY_END);
	// clang-format on

	meta = keyMeta (key);

	const char * value = keyString (ksLookupByName (meta, "meta:/hello", 0));
	succeed_if (!strcmp (value, "hello_world"), "unexpected value");

	value = keyString (ksLookupByName (meta, "meta:/mode", 0));
	succeed_if (!strcmp (value, "0644"), "unexpected value");

	value = keyString (ksLookupByName (meta, "meta:/time", 0));
	succeed_if (!strcmp (value, "1271234264"), "unexpected value");

	value = keyString (ksLookupByName (meta, "meta:/empty", 0));
	succeed_if (!strcmp (value, ""), "unexpected value");

	value = keyString (ksLookupByName (meta, "meta:/", 0));
	succeed_if (!strcmp (value, "empty"), "unexpected value");

	succeed_if (ksGetSize (meta) == 5, "unexpected meta keyset size");

	keyDel (key);
}

int main (int argc, char ** argv)
{
	printf ("KEY META ABI TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basic ();
	test_null_pointer ();
	test_iterate ();
	test_size ();
	test_dup ();
	test_examples ();
	test_type ();
	test_copy ();
	test_new ();
	test_copyall ();
	test_keyMeta ();


	printf ("\ntestabi_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
