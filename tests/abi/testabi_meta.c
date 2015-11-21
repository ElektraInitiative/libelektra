/**
 * @file
 *
 * @brief Test suite for meta information.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests.h>

static void test_basic()
{
	Key *key;
	key = keyNew("user/key_with_meta", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyGetMeta(key, "hello") == 0, "hello was not set up to now");
	succeed_if (keyGetMeta(key, "error") == 0, "hello was not set up to now");

	keySetMeta(key, "hello", "hello_world");
	succeed_if_same_string (keyValue(keyGetMeta(key, "hello")), "hello_world");

	keySetMeta(key, "mode", "0644");
	succeed_if_same_string (keyValue(keyGetMeta(key, "hello")), "hello_world");

	keySetMeta(key, "time", "1271234264");
	succeed_if_same_string (keyValue(keyGetMeta(key, "hello")), "hello_world");
	succeed_if_same_string (keyValue(keyGetMeta(key, "mode")), "0644");
	succeed_if_same_string (keyValue(keyGetMeta(key, "time")), "1271234264");

	keySetMeta(key, "hello", "between");
	succeed_if_same_string (keyValue(keyGetMeta(key, "hello")), "between");

	keySetMeta(key, "hello", 0);
	succeed_if (keyValue(keyGetMeta(key, "hello")) == 0, "could not remove meta data");

	keySetMeta(key, "hello", "goodbye");
	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "goodbye"),
			"could not set meta information again (2x)");

	keySetMeta(key, "empty", "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "empty")), "");

	keySetMeta(key, "owner", "hugo");
	succeed_if_same_string (keyValue(keyGetMeta(key, "owner")), "hugo");

	keySetMeta(key, "mode", "775");
	succeed_if_same_string (keyValue(keyGetMeta(key, "owner")), "hugo");
	succeed_if_same_string (keyValue(keyGetMeta(key, "mode")), "775");

	keySetMeta(key, "", "empty");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "empty");

	keySetMeta(key, "", "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "");

	keySetMeta(key, "", 0);
	succeed_if (keyValue(keyGetMeta(key, "")) == 0, "could not remove empty meta data");


	keyDel (key);
}

static void test_iterate()
{
	Key *key;

	key = keyNew ("user/test", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyRewindMeta(key) == 0, "Could not rewind empty key");
	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty");

	keySetMeta (key, "meta1", "meta_value");
	succeed_if (keyRewindMeta(key) == 0, "Could not rewind key");
	succeed_if_same_string (keyName(keyNextMeta(key)), "meta1");
	succeed_if_same_string (keyValue(keyCurrentMeta(key)), "meta_value");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 2. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 2. iteration");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 3. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 3. iteration");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 4. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 4. iteration");

	keyDel (key);
}

static void test_size()
{
	Key *key;
	char *buffer;

	key = keyNew ("user/test", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyValue(keyGetMeta(key, "hello")) == 0, "hello was not set up to now");
	succeed_if (keyGetValueSize (keyGetMeta(key, "hello")) == -1,
			"got wrong size for empty meta value");

	keySetMeta(key, "hello", "hello_world");
	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "hello_world"),
			"could not receive previously set meta information");
	succeed_if (keyGetValueSize (keyGetMeta(key, "hello")) == sizeof("hello_world"),
			"got wrong size");

	keySetMeta(key, "mode", "0644");
	keySetMeta(key, "time", "1271234264");
	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "hello_world"),
			"meta info changed unexpectly");
	succeed_if_same_string (keyValue(keyGetMeta(key, "mode")), "0644");
	succeed_if (keyGetValueSize (keyGetMeta(key, "mode")) == sizeof("0644"),
			"got wrong size");
	succeed_if_same_string (keyValue(keyGetMeta(key, "time")), "1271234264");
	succeed_if (keyGetValueSize (keyGetMeta(key, "time")) == sizeof("1271234264"),
			"got wrong size");

	keySetMeta(key, "hello", "between");
	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "between"),
			"could not set meta information again");
	succeed_if (keyGetValueSize (keyGetMeta(key, "hello")) == sizeof("between"),
			"got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta(key, "hello")));
	succeed_if (keyGetString (keyGetMeta(key, "hello"), buffer,
				keyGetValueSize (keyGetMeta(key, "hello"))) == keyGetValueSize (keyGetMeta(key, "hello")),
			"could not get meta");
	succeed_if_same_string (buffer, "between");
	free (buffer);


	keySetMeta(key, "hello", 0);
	succeed_if (keyValue(keyGetMeta(key, "hello")) == 0, "could not remove meta data");
	succeed_if (keyGetValueSize (keyGetMeta(key, "hello")) == -1,
			"got wrong size");

	keySetMeta(key, "hello", "goodbye");
	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "goodbye"),
			"could not set meta information again (2x)");
	succeed_if (keyGetValueSize (keyGetMeta(key, "hello")) == sizeof("goodbye"),
			"got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta(key, "hello")));
	succeed_if (keyGetString (keyGetMeta(key, "hello"), buffer,
				keyGetValueSize (keyGetMeta(key, "hello"))) == keyGetValueSize (keyGetMeta(key, "hello")),
			"could not get meta");
	succeed_if_same_string (buffer, "goodbye");
	free (buffer);

	keySetMeta(key, "empty", "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "empty")), "");
	succeed_if (keyGetValueSize (keyGetMeta(key, "empty")) == sizeof(""),
			"got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta(key, "empty")));
	succeed_if (keyGetString (keyGetMeta(key, "empty"), buffer,
				keyGetValueSize (keyGetMeta(key, "empty"))) == keyGetValueSize (keyGetMeta(key, "empty")),
			"could not get meta");
	succeed_if_same_string (buffer, "");
	free (buffer);

	keySetMeta(key, "", "empty");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "empty");
	succeed_if (keyGetValueSize (keyGetMeta(key, "")) == sizeof("empty"),
			"got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta(key, "")));
	succeed_if (keyGetString (keyGetMeta(key, ""), buffer,
				keyGetValueSize (keyGetMeta(key, ""))) == keyGetValueSize (keyGetMeta(key, "")),
			"could not get meta");
	succeed_if_same_string (buffer, "empty");
	free (buffer);

	keySetMeta(key, "", "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "");
	succeed_if (keyGetValueSize (keyGetMeta(key, "")) == sizeof(""),
			"got wrong size");
	buffer = calloc (1, keyGetValueSize (keyGetMeta(key, "")));
	succeed_if (keyGetString (keyGetMeta(key, ""), buffer,
				keyGetValueSize (keyGetMeta(key, ""))) == keyGetValueSize (keyGetMeta(key, "")),
			"could not get meta");
	succeed_if_same_string (buffer, "");
	free (buffer);

	keySetMeta(key, "", 0);
	succeed_if (keyValue(keyGetMeta(key, "")) == 0, "could not remove empty meta data");
	succeed_if (keyGetValueSize (keyGetMeta(key, "")) == -1,
			"got wrong size");


	keyDel (key);

}

static void test_dup()
{
	Key *key;
	Key *dup;

	key = keyNew ("user/orig", KEY_END);
	succeed_if (keySetMeta (key, "test", "some_meta_test") == sizeof("some_meta_test"),
			"could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta(key, "test")), "some_meta_test");

	dup = keyDup (key);
	succeed_if_same_string (keyValue (keyGetMeta(dup, "test")), "some_meta_test");
	succeed_if(keySetMeta (dup, "test", "some_other_meta_test") == sizeof("some_other_meta_test"),
		"sizeof meta test wrong");
	succeed_if_same_string (keyValue (keyGetMeta(dup, "test")), "some_other_meta_test");
	succeed_if_same_string (keyValue (keyGetMeta(key, "test")), "some_meta_test");
	keyDel (dup);

	keyDel (key);
}

Key * g_c;

static void j (Key *k)
{
	size_t size = keyGetValueSize (k);
	char *value = elektraMalloc (size);
	int bstring = keyIsString (k);

	// receive key g_c
	memcpy (value, keyValue(k), size);
	keyCopy (k, g_c);
	if (bstring) keySetString (k, value);
	else keySetBinary (k, value, size);
	free (value);
	// the caller will see the changed key k
	// with the metadata from g_c
}

static void l(Key *k)
{
	// receive g_c
	keyCopyMeta(k, g_c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from g_c
}

static void test_examples()
{
	Key *key;
	key = keyNew(0);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	g_c = keyNew(0);
	keySetMeta (g_c, "xef", "ybc");
	keySetMeta (g_c, "xop", "yup");

	j(key);

	succeed_if_same_string (keyValue(keyGetMeta(key, "xef")), "ybc");
	succeed_if_same_string (keyValue(keyGetMeta(key, "xop")), "yup");
	succeed_if (keyValue(keyGetMeta(key, "def")) == 0, "old meta data remained");
	succeed_if (keyValue(keyGetMeta(key, "nop")) == 0, "old meta data remained");

	keyDel (key);
	keyDel (g_c);

	key = keyNew(0);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	g_c = keyNew(0);
	keySetMeta (g_c, "type", "boolean");
	keySetMeta (g_c, "xop", "yup");

	l (key);

	succeed_if_same_string (keyValue(keyGetMeta(key, "def")), "abc");
	succeed_if_same_string (keyValue(keyGetMeta(key, "nop")), "cup");
	succeed_if_same_string (keyValue(keyGetMeta(key, "type")), "boolean");
	succeed_if (keyValue(keyGetMeta(key, "xop")) == 0, "this meta data was not requested to be copied");

	keyDel (key);
	keyDel (g_c);
}

static void test_copy()
{
	printf ("Test key meta copy\n");

	Key *key1;
	Key *key2;

	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keyCopyMeta(key2, key1, "nonexist") == 0, "could not do anything");

	succeed_if (keyValue(keyGetMeta(key2, "nonexist")) == 0, "should not be there");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if (keyCopyMeta(key2, key1, "mymeta") == 1, "could not copy meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	succeed_if (keyCopyMeta(key1, key2, "mymeta") == 1, "did nothing in the end");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if (keyCopyMeta(key2, key1, "mymeta") == 1, "could not copy meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") != keyGetMeta(key2, "mymeta"), "reference to another key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value2") == sizeof("a longer meta value2"),
			"could not set meta value2");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value2");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") != keyGetMeta(key2, "mymeta"),
			"reference to another key (with another value)");

	keyDel (key1);
	keyDel (key2);

	Key *k;
	Key *c;

	k=keyNew ("user/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the meta data later",
		KEY_META, "mode", "0775",
		KEY_END);
	c=keyNew ("user/metacopy", KEY_END);

	succeed_if (keyGetMeta(k, "t") != 0, "could not get meta key");
	succeed_if (keyGetMeta(k, "a") != 0, "could not get meta key");

	succeed_if (keyGetMeta(c, "t") == 0, "could get meta key not there");
	succeed_if (keyGetMeta(c, "a") == 0, "could get meta key not there");

	succeed_if (keyCopyMeta(c, k, "t") == 1, "could not copy meta data");
	succeed_if (keyGetMeta(k, "t") == keyGetMeta(c, "t"), "not the same meta data after copy");

	succeed_if (keyCopyMeta(c, k, "a") == 1, "could not copy meta data");
	succeed_if (keyGetMeta(k, "a") == keyGetMeta(c, "a"), "not the same meta data after copy");

	keyDel (k);
	keyDel (c);



	k=keyNew ("user/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the meta data later",
		KEY_META, "mode", "0775",
		KEY_END);
	c=keyNew ("user/metacopy",
		KEY_META, "t", "test1",
		KEY_META, "a", "wrong",
		KEY_META, "old", "will stay",
		KEY_END);

	succeed_if (keyGetMeta(k, "t") != 0, "could not get meta key");
	succeed_if (keyGetMeta(k, "a") != 0, "could not get meta key");

	succeed_if (keyCopyMeta(c, k, "t") == 1, "could not copy meta data");
	succeed_if (keyGetMeta(k, "t") == keyGetMeta(c, "t"), "not the same meta data after copy");
	succeed_if_same_string (keyValue(keyGetMeta(k, "t")), "test1");
	succeed_if_same_string (keyValue(keyGetMeta(c, "t")), "test1");

	succeed_if (keyCopyMeta(c, k, "a") == 1, "could not copy meta data");
	succeed_if (keyGetMeta(k, "a") == keyGetMeta(c, "a"), "not the same meta data after copy");
	succeed_if_same_string (keyValue(keyGetMeta(k, "a")), "another");
	succeed_if_same_string (keyValue(keyGetMeta(c, "a")), "another");

	succeed_if_same_string (keyValue(keyGetMeta(c, "old")), "will stay");
	succeed_if (keyGetMeta(c, "cya") == 0, "meta key should not be there");
	succeed_if (keyGetMeta(c, "mode") == 0, "meta key should not be there");

	keyDel (k);
	keyDel (c);
}

static void test_new()
{
	Key *key;
	key = keyNew ("user/test",
		KEY_META, "hello", "hello_world",
		KEY_META, "mode", "0644",
		KEY_META, "time", "1271234264",
		KEY_META, "empty", "",
		KEY_META, "", "empty",
		KEY_END);

	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "hello_world"),
			"could not receive previously set meta information");
	succeed_if_same_string (keyValue(keyGetMeta(key, "mode")), "0644");
	succeed_if_same_string (keyValue(keyGetMeta(key, "time")), "1271234264");
	succeed_if_same_string (keyValue(keyGetMeta(key, "empty")), "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "empty");

	keySetMeta(key, "", "full");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "full");

	keySetMeta(key, "", 0);
	succeed_if (keyValue(keyGetMeta(key, "")) == 0, "could not remove empty meta data");

	keyDel (key);

	key = keyNew ("user/test",
		KEY_META, "hello", "goodbye",
		KEY_META, "mode", "0775",
		KEY_META, "time", "1271939923",
		KEY_META, "empty", "",
		KEY_META, "", "",
		KEY_END);

	succeed_if (!strcmp(keyValue(keyGetMeta(key, "hello")), "goodbye"),
			"could not receive previously set meta information");
	succeed_if_same_string (keyValue(keyGetMeta(key, "mode")), "0775");
	succeed_if_same_string (keyValue(keyGetMeta(key, "time")), "1271939923");
	succeed_if_same_string (keyValue(keyGetMeta(key, "empty")), "");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "");

	keySetMeta(key, "", "full");
	succeed_if_same_string (keyValue(keyGetMeta(key, "")), "full");

	keySetMeta(key, "", 0);
	succeed_if (keyValue(keyGetMeta(key, "")) == 0, "could not remove empty meta data");

	keyDel (key);
}


static void test_copyall()
{
	printf ("Test key meta copy all\n");

	Key *key1;
	Key *key2;

	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keyCopyAllMeta(key2, key1) == 0, "could not do anything");

	succeed_if (keyValue(keyGetMeta(key2, "nonexist")) == 0, "should not be there");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if (keyCopyAllMeta(key2, key1) == 1, "could not copy meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	succeed_if (keyCopyAllMeta(key1, key2) == 1, "did nothing in the end");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	keyDel (key1);
	keyDel (key2);


	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if (keyCopyAllMeta(key2, key1) == 1, "could not copy meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") == keyGetMeta(key2, "mymeta"), "reference to the same key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") != keyGetMeta(key2, "mymeta"), "reference to another key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value2") == sizeof("a longer meta value2"),
			"could not set meta value2");
	succeed_if_same_string (keyValue(keyGetMeta(key1, "mymeta")), "a longer meta value2");
	succeed_if_same_string (keyValue(keyGetMeta(key2, "mymeta")), "a longer meta value");
	succeed_if (keyGetMeta(key1, "mymeta") != keyGetMeta(key2, "mymeta"),
			"reference to another key (with another value)");

	keyDel (key1);
	keyDel (key2);

	Key *k;
	Key *c;

	k=keyNew ("user/metakey",
		KEY_META, "t", "test1",
		KEY_META, "a", "another",
		KEY_META, "cya", "see the meta data later",
		KEY_META, "mode", "0775",
		KEY_END);
	c=keyNew ("user/metacopy", KEY_END);

	succeed_if (keyGetMeta(k, "t") != 0, "could not get meta key");
	succeed_if (keyGetMeta(k, "a") != 0, "could not get meta key");

	succeed_if (keyGetMeta(c, "t") == 0, "could get meta key not there");
	succeed_if (keyGetMeta(c, "a") == 0, "could get meta key not there");

	succeed_if (keyCopyAllMeta(c, k) == 1, "could not copy meta data");
	succeed_if (keyGetMeta(k, "t") == keyGetMeta(c, "t"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "a") == keyGetMeta(c, "a"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "cya") == keyGetMeta(c, "cya"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "mode") == keyGetMeta(c, "mode"), "not the same meta data after copy");
	succeed_if (keyValue(keyGetMeta(k, "nonexist")) == 0, "should not be there");
	succeed_if (keyValue(keyGetMeta(c, "nonexist")) == 0, "should not be there");

	succeed_if (keyCopyAllMeta(c, k) == 1, "could not copy meta data (again)");
	succeed_if (keyGetMeta(k, "t") == keyGetMeta(c, "t"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "a") == keyGetMeta(c, "a"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "cya") == keyGetMeta(c, "cya"), "not the same meta data after copy");
	succeed_if (keyGetMeta(k, "mode") == keyGetMeta(c, "mode"), "not the same meta data after copy");
	succeed_if (keyValue(keyGetMeta(k, "nonexist")) == 0, "should not be there");
	succeed_if (keyValue(keyGetMeta(c, "nonexist")) == 0, "should not be there");

	keyDel (k);
	keyDel (c);
}

static void test_type()
{
	Key *key;

	succeed_if (key = keyNew(0), "could not create a new key");
	succeed_if (keyValue(keyGetMeta(key, "binary")) == 0, "wrong type after key creation");
	succeed_if (keySetString (key, "mystring") == sizeof("mystring"), "could not set string");
	succeed_if (keyValue(keyGetMeta(key, "binary")) == 0, "wrong type after setting string");
	succeed_if (keySetBinary (key, "mystring", sizeof("mystring")) == sizeof("mystring"),
			"could not set binary");
	succeed_if (keyValue(keyGetMeta(key, "binary")) != 0, "wrong type after setting string");

	keyDel (key);
}



int main(int argc, char** argv)
{
	printf("KEY META ABI TESTS\n");
	printf("==================\n\n");

	init (argc, argv);
	test_basic();
	test_iterate();
	test_size();
	test_dup();
	test_examples();
	test_type();
	test_copy();
	test_new();
	test_copyall();


	printf("\ntestabi_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

