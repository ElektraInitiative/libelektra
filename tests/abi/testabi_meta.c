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
	ElektraKey * key;
	key = elektraKeyNew ("user:/key_with_meta", ELEKTRA_KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (elektraKeyGetMeta (key, "hello") == 0, "hello was not set up to now");
	succeed_if (elektraKeyGetMeta (key, "error") == 0, "hello was not set up to now");

	elektraKeySetMeta (key, "hello", "hello_world");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world");

	elektraKeySetMeta (key, "mode", "0644");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world");

	elektraKeySetMeta (key, "time", "1271234264");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "mode")), "0644");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "time")), "1271234264");

	elektraKeySetMeta (key, "hello", "between");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "between");

	elektraKeySetMeta (key, "hello", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "hello")) == 0, "could not remove metadata");

	elektraKeySetMeta (key, "hello", "goodbye");
	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "goodbye"), "could not set meta information again (2x)");

	elektraKeySetMeta (key, "empty", "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "empty")), "");

	elektraKeySetMeta (key, "owner", "hugo");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "owner")), "hugo");

	elektraKeySetMeta (key, "mode", "775");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "owner")), "hugo");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "mode")), "775");

	elektraKeySetMeta (key, "", "empty");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "empty");

	elektraKeySetMeta (key, "", "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "");

	elektraKeySetMeta (key, "", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "")) == 0, "could not remove empty metadata");


	elektraKeyDel (key);
}

static void test_null_pointer (void)
{
	ElektraKey * key;

	key = elektraKeyNew ("user:/test1", ELEKTRA_KEY_END);
	exit_if_fail (key, "could not create new key");

	succeed_if (elektraKeyRewindMeta (0) == -1, "Could rewind NULL Key");

	succeed_if (elektraKeyGetMeta (0, "test") == 0, "Could get meta of NULL Key");
	succeed_if (elektraKeyGetMeta (key, 0) == 0, "Could get meta of NULL metaName");

	succeed_if (elektraKeyMeta (0) == 0, "Could get metadata of NULL Key");
	succeed_if (elektraKeyCurrentMeta (0) == 0, "Could get current meta Key of NULL key");
	succeed_if (elektraKeyNextMeta (0) == 0, "Could get next meta Key of NULL key");

	succeed_if (elektraKeyCopyMeta (0, key, "test") == -1, "Could copy metadata to NULL Key");
	succeed_if (elektraKeyCopyMeta (key, 0, "test") == -1, "Could copy metadata from NULL Key");

	succeed_if (elektraKeyCopyAllMeta (0, key) == -1, "Could copy all metadata to NULL Key");
	succeed_if (elektraKeyCopyAllMeta (key, 0) == -1, "Could copy all metadata from NULL Key");

	succeed_if (elektraKeySetMeta (0, "test", "test"), "Could set metadata to NULL Key");
	succeed_if (elektraKeySetMeta (key, 0, "test"), "Could set metadata with NULL metaName");

	elektraKeyDel (key);
}

static void test_iterate (void)
{
	ElektraKey * key;

	key = elektraKeyNew ("user:/test", ELEKTRA_KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (elektraKeyRewindMeta (key) == 0, "Could not rewind empty key");
	succeed_if (elektraKeyNextMeta (key) == 0, "Could get next metaname, even if it is empty");
	succeed_if (elektraKeyCurrentMeta (key) == 0, "Could get next metavalue, even if it is empty");

	elektraKeySetMeta (key, "meta1", "meta_value");
	succeed_if (elektraKeyRewindMeta (key) == 0, "Could not rewind key");
	succeed_if_same_string (elektraKeyName (elektraKeyNextMeta (key)), "meta:/meta1");
	succeed_if_same_string (elektraKeyValue (elektraKeyCurrentMeta (key)), "meta_value");

	succeed_if (elektraKeyNextMeta (key) == 0, "Could get next metaname, even if it is empty at 2. iteration");
	succeed_if (elektraKeyCurrentMeta (key) == 0, "Could get next metavalue, even if it is empty at 2. iteration");

	succeed_if (elektraKeyNextMeta (key) == 0, "Could get next metaname, even if it is empty at 3. iteration");
	succeed_if (elektraKeyCurrentMeta (key) == 0, "Could get next metavalue, even if it is empty at 3. iteration");

	succeed_if (elektraKeyNextMeta (key) == 0, "Could get next metaname, even if it is empty at 4. iteration");
	succeed_if (elektraKeyCurrentMeta (key) == 0, "Could get next metavalue, even if it is empty at 4. iteration");

	elektraKeyDel (key);
}

static void test_size (void)
{
	ElektraKey * key;
	char * buffer;

	key = elektraKeyNew ("user:/test", ELEKTRA_KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "hello")) == 0, "hello was not set up to now");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")) == -1, "got wrong size for empty metavalue");

	elektraKeySetMeta (key, "hello", "hello_world");
	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world"), "could not receive previously set meta information");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")) == sizeof ("hello_world"), "got wrong size");

	elektraKeySetMeta (key, "mode", "0644");
	elektraKeySetMeta (key, "time", "1271234264");
	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world"), "meta info changed unexpectly");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "mode")), "0644");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "mode")) == sizeof ("0644"), "got wrong size");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "time")), "1271234264");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "time")) == sizeof ("1271234264"), "got wrong size");

	elektraKeySetMeta (key, "hello", "between");
	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "between"), "could not set meta information again");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")) == sizeof ("between"), "got wrong size");
	buffer = calloc (1, elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")));
	succeed_if (elektraKeyGetString (elektraKeyGetMeta (key, "hello"), buffer, elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello"))) ==
			    elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")),
		    "could not get meta");
	succeed_if_same_string (buffer, "between");
	elektraFree (buffer);


	elektraKeySetMeta (key, "hello", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "hello")) == 0, "could not remove metadata");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")) == -1, "got wrong size");

	elektraKeySetMeta (key, "hello", "goodbye");
	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "goodbye"), "could not set meta information again (2x)");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")) == sizeof ("goodbye"), "got wrong size");
	buffer = calloc (1, elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")));
	succeed_if (elektraKeyGetString (elektraKeyGetMeta (key, "hello"), buffer, elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello"))) ==
			    elektraKeyGetValueSize (elektraKeyGetMeta (key, "hello")),
		    "could not get meta");
	succeed_if_same_string (buffer, "goodbye");
	elektraFree (buffer);

	elektraKeySetMeta (key, "empty", "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "empty")), "");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "empty")) == sizeof (""), "got wrong size");
	buffer = calloc (1, elektraKeyGetValueSize (elektraKeyGetMeta (key, "empty")));
	succeed_if (elektraKeyGetString (elektraKeyGetMeta (key, "empty"), buffer, elektraKeyGetValueSize (elektraKeyGetMeta (key, "empty"))) ==
			    elektraKeyGetValueSize (elektraKeyGetMeta (key, "empty")),
		    "could not get meta");
	succeed_if_same_string (buffer, "");
	elektraFree (buffer);

	elektraKeySetMeta (key, "", "empty");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "empty");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "")) == sizeof ("empty"), "got wrong size");
	buffer = calloc (1, elektraKeyGetValueSize (elektraKeyGetMeta (key, "")));
	succeed_if (elektraKeyGetString (elektraKeyGetMeta (key, ""), buffer, elektraKeyGetValueSize (elektraKeyGetMeta (key, ""))) ==
			    elektraKeyGetValueSize (elektraKeyGetMeta (key, "")),
		    "could not get meta");
	succeed_if_same_string (buffer, "empty");
	elektraFree (buffer);

	elektraKeySetMeta (key, "", "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "")) == sizeof (""), "got wrong size");
	buffer = calloc (1, elektraKeyGetValueSize (elektraKeyGetMeta (key, "")));
	succeed_if (elektraKeyGetString (elektraKeyGetMeta (key, ""), buffer, elektraKeyGetValueSize (elektraKeyGetMeta (key, ""))) ==
			    elektraKeyGetValueSize (elektraKeyGetMeta (key, "")),
		    "could not get meta");
	succeed_if_same_string (buffer, "");
	elektraFree (buffer);

	elektraKeySetMeta (key, "", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "")) == 0, "could not remove empty metadata");
	succeed_if (elektraKeyGetValueSize (elektraKeyGetMeta (key, "")) == -1, "got wrong size");


	elektraKeyDel (key);
}

static void test_dup (void)
{
	ElektraKey * key;
	ElektraKey * dup;

	key = elektraKeyNew ("user:/orig", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetMeta (key, "test", "some_meta_test") == sizeof ("some_meta_test"), "could not set meta");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "test")), "some_meta_test");

	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (dup, "test")), "some_meta_test");
	succeed_if (elektraKeySetMeta (dup, "test", "some_other_meta_test") == sizeof ("some_other_meta_test"), "sizeof meta test wrong");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (dup, "test")), "some_other_meta_test");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "test")), "some_meta_test");
	elektraKeyDel (dup);

	elektraKeyDel (key);
}

ElektraKey * g_c;

static void j (ElektraKey * k)
{
	size_t size = elektraKeyGetValueSize (k);
	char * value = elektraMalloc (size);
	int bstring = elektraKeyIsString (k);

	// receive key g_c
	memcpy (value, elektraKeyValue (k), size);
	elektraKeyCopy (k, g_c, ELEKTRA_KEY_CP_ALL);
	if (bstring)
		elektraKeySetString (k, value);
	else
		elektraKeySetBinary (k, value, size);
	elektraFree (value);
	// the caller will see the changed key k
	// with the metadata from g_c
}

static void l (ElektraKey * k)
{
	// receive g_c
	elektraKeyCopyMeta (k, g_c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from g_c
}

static void test_examples (void)
{
	ElektraKey * key;
	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (key, "def", "abc");
	elektraKeySetMeta (key, "nop", "cup");

	g_c = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (g_c, "xef", "ybc");
	elektraKeySetMeta (g_c, "xop", "yup");

	j (key);

	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "xef")), "ybc");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "xop")), "yup");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "def")) == 0, "old metadata remained");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "nop")) == 0, "old metadata remained");

	elektraKeyDel (key);
	elektraKeyDel (g_c);

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (key, "def", "abc");
	elektraKeySetMeta (key, "nop", "cup");

	g_c = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (g_c, "type", "boolean");
	elektraKeySetMeta (g_c, "xop", "yup");

	l (key);

	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "def")), "abc");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "nop")), "cup");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "type")), "boolean");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "xop")) == 0, "this metadata was not requested to be copied");

	elektraKeyDel (key);
	elektraKeyDel (g_c);
}

static void test_copy (void)
{
	printf ("Test key meta copy\n");

	ElektraKey * key1;
	ElektraKey * key2;

	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeyCopyMeta (key2, key1, "nonexist") == 0, "could not do anything");

	succeed_if (elektraKeyValue (elektraKeyGetMeta (key2, "nonexist")) == 0, "should not be there");

	elektraKeyDel (key1);
	elektraKeyDel (key2);


	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (elektraKeyCopyMeta (key2, key1, "mymeta") == 1, "could not copy metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (elektraKeyCopyMeta (key1, key2, "mymeta") == 1, "did nothing in the end");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	elektraKeyDel (key1);
	elektraKeyDel (key2);


	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (elektraKeyCopyMeta (key2, key1, "mymeta") == 1, "could not copy metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") != elektraKeyGetMeta (key2, "mymeta"), "reference to another key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue2") == sizeof ("a longer metavalue2"), "could not set metavalue2");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue2");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") != elektraKeyGetMeta (key2, "mymeta"), "reference to another key (with another value)");

	elektraKeyDel (key1);
	elektraKeyDel (key2);

	ElektraKey * k;
	ElektraKey * c;

	// clang-format off
	k=elektraKeyNew ("user:/metakey",
		ELEKTRA_KEY_META, "t", "test1",
		ELEKTRA_KEY_META, "a", "another",
		ELEKTRA_KEY_META, "cya", "see the metadata later",
		ELEKTRA_KEY_META, "mode", "0775",
		ELEKTRA_KEY_END);
	// clang-format on
	c = elektraKeyNew ("user:/metacopy", ELEKTRA_KEY_END);

	succeed_if (elektraKeyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (elektraKeyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (elektraKeyGetMeta (c, "t") == 0, "could get metakey not there");
	succeed_if (elektraKeyGetMeta (c, "a") == 0, "could get metakey not there");

	succeed_if (elektraKeyCopyMeta (c, k, "t") == 1, "could not copy metadata");
	succeed_if (elektraKeyGetMeta (k, "t") == elektraKeyGetMeta (c, "t"), "not the same metadata after copy");

	succeed_if (elektraKeyCopyMeta (c, k, "a") == 1, "could not copy metadata");
	succeed_if (elektraKeyGetMeta (k, "a") == elektraKeyGetMeta (c, "a"), "not the same metadata after copy");

	elektraKeyDel (k);
	elektraKeyDel (c);


	// clang-format off
	k=elektraKeyNew ("user:/metakey",
		ELEKTRA_KEY_META, "t", "test1",
		ELEKTRA_KEY_META, "a", "another",
		ELEKTRA_KEY_META, "cya", "see the metadata later",
		ELEKTRA_KEY_META, "mode", "0775",
		ELEKTRA_KEY_END);
	c=elektraKeyNew ("user:/metacopy",
		ELEKTRA_KEY_META, "t", "test1",
		ELEKTRA_KEY_META, "a", "wrong",
		ELEKTRA_KEY_META, "old", "will stay",
		ELEKTRA_KEY_END);
	// clang-format on

	succeed_if (elektraKeyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (elektraKeyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (elektraKeyCopyMeta (c, k, "t") == 1, "could not copy metadata");
	succeed_if (elektraKeyGetMeta (k, "t") == elektraKeyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (k, "t")), "test1");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (c, "t")), "test1");

	succeed_if (elektraKeyCopyMeta (c, k, "a") == 1, "could not copy metadata");
	succeed_if (elektraKeyGetMeta (k, "a") == elektraKeyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (k, "a")), "another");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (c, "a")), "another");

	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (c, "old")), "will stay");
	succeed_if (elektraKeyGetMeta (c, "cya") == 0, "metakey should not be there");
	succeed_if (elektraKeyGetMeta (c, "mode") == 0, "metakey should not be there");

	elektraKeyDel (k);
	elektraKeyDel (c);
}

static void test_new (void)
{
	ElektraKey * key;
	// clang-format off
	key = elektraKeyNew ("user:/test",
		ELEKTRA_KEY_META, "hello", "hello_world",
		ELEKTRA_KEY_META, "mode", "0644",
		ELEKTRA_KEY_META, "time", "1271234264",
		ELEKTRA_KEY_META, "empty", "",
		ELEKTRA_KEY_META, "", "empty",
		ELEKTRA_KEY_END);
	// clang-format on

	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "hello_world"), "could not receive previously set meta information");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "mode")), "0644");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "time")), "1271234264");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "empty")), "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "empty");

	elektraKeySetMeta (key, "", "full");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "full");

	elektraKeySetMeta (key, "", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "")) == 0, "could not remove empty metadata");

	elektraKeyDel (key);

	// clang-format off
	key = elektraKeyNew ("user:/test",
		ELEKTRA_KEY_META, "hello", "goodbye",
		ELEKTRA_KEY_META, "mode", "0775",
		ELEKTRA_KEY_META, "time", "1271939923",
		ELEKTRA_KEY_META, "empty", "",
		ELEKTRA_KEY_META, "", "",
		ELEKTRA_KEY_END);
	// clang-format on

	succeed_if (!strcmp (elektraKeyValue (elektraKeyGetMeta (key, "hello")), "goodbye"), "could not receive previously set meta information");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "mode")), "0775");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "time")), "1271939923");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "empty")), "");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "");

	elektraKeySetMeta (key, "", "full");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "")), "full");

	elektraKeySetMeta (key, "", 0);
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "")) == 0, "could not remove empty metadata");

	elektraKeyDel (key);
}


static void test_copyall (void)
{
	printf ("Test key meta copy all\n");

	ElektraKey * key1;
	ElektraKey * key2;

	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeyCopyAllMeta (key2, key1) == 0, "could not do anything");

	succeed_if (elektraKeyValue (elektraKeyGetMeta (key2, "nonexist")) == 0, "should not be there");

	elektraKeyDel (key1);
	elektraKeyDel (key2);


	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (elektraKeyCopyAllMeta (key2, key1) == 1, "could not copy metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (elektraKeyCopyAllMeta (key1, key2) == 1, "did nothing in the end");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	elektraKeyDel (key1);
	elektraKeyDel (key2);


	succeed_if (key1 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");
	succeed_if (key2 = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if (elektraKeyCopyAllMeta (key2, key1) == 1, "could not copy metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") == elektraKeyGetMeta (key2, "mymeta"), "reference to the same key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue") == sizeof ("a longer metavalue"), "could not set metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") != elektraKeyGetMeta (key2, "mymeta"), "reference to another key");

	succeed_if (elektraKeySetMeta (key1, "mymeta", "a longer metavalue2") == sizeof ("a longer metavalue2"), "could not set metavalue2");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key1, "mymeta")), "a longer metavalue2");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key2, "mymeta")), "a longer metavalue");
	succeed_if (elektraKeyGetMeta (key1, "mymeta") != elektraKeyGetMeta (key2, "mymeta"), "reference to another key (with another value)");

	elektraKeyDel (key1);
	elektraKeyDel (key2);

	ElektraKey * k;
	ElektraKey * c;

	// clang-format off
	k=elektraKeyNew ("user:/metakey",
		ELEKTRA_KEY_META, "t", "test1",
		ELEKTRA_KEY_META, "a", "another",
		ELEKTRA_KEY_META, "cya", "see the metadata later",
		ELEKTRA_KEY_META, "mode", "0775",
		ELEKTRA_KEY_END);
	// clang-format on
	c = elektraKeyNew ("user:/metacopy", ELEKTRA_KEY_END);

	succeed_if (elektraKeyGetMeta (k, "t") != 0, "could not get metakey");
	succeed_if (elektraKeyGetMeta (k, "a") != 0, "could not get metakey");

	succeed_if (elektraKeyGetMeta (c, "t") == 0, "could get metakey not there");
	succeed_if (elektraKeyGetMeta (c, "a") == 0, "could get metakey not there");

	succeed_if (elektraKeyCopyAllMeta (c, k) == 1, "could not copy metadata");
	succeed_if (elektraKeyGetMeta (k, "t") == elektraKeyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "a") == elektraKeyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "cya") == elektraKeyGetMeta (c, "cya"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "mode") == elektraKeyGetMeta (c, "mode"), "not the same metadata after copy");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (k, "nonexist")) == 0, "should not be there");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (c, "nonexist")) == 0, "should not be there");

	succeed_if (elektraKeyCopyAllMeta (c, k) == 1, "could not copy metadata (again)");
	succeed_if (elektraKeyGetMeta (k, "t") == elektraKeyGetMeta (c, "t"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "a") == elektraKeyGetMeta (c, "a"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "cya") == elektraKeyGetMeta (c, "cya"), "not the same metadata after copy");
	succeed_if (elektraKeyGetMeta (k, "mode") == elektraKeyGetMeta (c, "mode"), "not the same metadata after copy");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (k, "nonexist")) == 0, "should not be there");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (c, "nonexist")) == 0, "should not be there");

	elektraKeyDel (k);
	elektraKeyDel (c);
}

static void test_type (void)
{
	ElektraKey * key;

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create a new key");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "binary")) == 0, "wrong type after key creation");
	succeed_if (elektraKeySetString (key, "mystring") == sizeof ("mystring"), "could not set string");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "binary")) == 0, "wrong type after setting string");
	succeed_if (elektraKeySetBinary (key, "mystring", sizeof ("mystring")) == sizeof ("mystring"), "could not set binary");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "binary")) != 0, "wrong type after setting string");

	elektraKeyDel (key);
}

static void test_keyMeta (void)
{
	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);

	ElektraKeyset * meta = elektraKeyMeta (key);

	succeed_if (meta, "meta keyset is null");
	succeed_if (elektraKeysetGetSize (meta) == 0, "meta keyset is not empty");
	elektraKeyDel (key);

	// clang-format off
	key = elektraKeyNew ("user:/test",
		ELEKTRA_KEY_META, "hello", "hello_world",
		ELEKTRA_KEY_META, "mode", "0644",
		ELEKTRA_KEY_META, "time", "1271234264",
		ELEKTRA_KEY_META, "empty", "",
		ELEKTRA_KEY_META, "", "empty",
		ELEKTRA_KEY_END);
	// clang-format on

	meta = elektraKeyMeta (key);

	const char * value = elektraKeyString (elektraKeysetLookupByName (meta, "meta:/hello", 0));
	succeed_if (!strcmp (value, "hello_world"), "unexpected value");

	value = elektraKeyString (elektraKeysetLookupByName (meta, "meta:/mode", 0));
	succeed_if (!strcmp (value, "0644"), "unexpected value");

	value = elektraKeyString (elektraKeysetLookupByName (meta, "meta:/time", 0));
	succeed_if (!strcmp (value, "1271234264"), "unexpected value");

	value = elektraKeyString (elektraKeysetLookupByName (meta, "meta:/empty", 0));
	succeed_if (!strcmp (value, ""), "unexpected value");

	value = elektraKeyString (elektraKeysetLookupByName (meta, "meta:/", 0));
	succeed_if (!strcmp (value, "empty"), "unexpected value");

	succeed_if (elektraKeysetGetSize (meta) == 5, "unexpected meta keyset size");

	elektraKeyDel (key);
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
