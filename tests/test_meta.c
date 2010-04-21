/***************************************************************************
 *          test_keymeta.c  -  Test suite for meta information
 *                  -------------------
 *  begin                : Thu Dez 12 2006
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests.h>

void test_basic()
{
	Key *key;
	key = keyNew("user/metakey", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyMeta(key, "hello") == 0, "hello was not set up to now");

	keySetMeta(key, "hello", "hello_world");
	succeed_if (!strcmp(keyMeta(key, "hello"), "hello_world"),
			"could not receive previously set meta information");

	keySetMeta(key, "mode", "0644");
	keySetMeta(key, "time", "1271234264");
	succeed_if (!strcmp(keyMeta(key, "hello"), "hello_world"),
			"meta info changed unexpectly");
	succeed_if (!strcmp(keyMeta(key, "mode"), "0644"), "mode not set correctly");
	succeed_if (!strcmp(keyMeta(key, "time"), "1271234264"), "time not set correctly");

	keySetMeta(key, "hello", "between");
	succeed_if (!strcmp(keyMeta(key, "hello"), "between"),
			"could not set meta information again");

	keySetMeta(key, "hello", 0);
	succeed_if (keyMeta(key, "hello") == 0, "could not remove meta data");

	keySetMeta(key, "hello", "goodbye");
	succeed_if (!strcmp(keyMeta(key, "hello"), "goodbye"),
			"could not set meta information again (2x)");

	keySetMeta(key, "empty", "");
	succeed_if (!strcmp(keyMeta(key, "empty"), ""), "Problem with empty meta string");

	keySetMeta(key, "", "empty");
	succeed_if (!strcmp(keyMeta(key, ""), "empty"), "Problem with empty name");

	keySetMeta(key, "", "");
	succeed_if (!strcmp(keyMeta(key, ""), ""), "Problem with empty name and string");

	keySetMeta(key, "", 0);
	succeed_if (keyMeta(key, "") == 0, "could not remove empty meta data");


	keyDel (key);
}

void test_iterate()
{
	Key *key;

	key = keyNew ("user/test", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyRewindMeta(key) == 0, "Could not rewind empty key");
	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty");

	keySetMeta (key, "meta1", "meta_value");
	succeed_if (keyRewindMeta(key) == 0, "Could not rewind key");
	succeed_if (!strcmp(keyNextMeta(key), "meta1"), "keyNextMeta does not work at 1. iteration");
	succeed_if (!strcmp(keyCurrentMeta(key), "meta_value"), "keyCurrentMeta does not work at 1. iteration");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 2. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 2. iteration");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 3. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 3. iteration");

	succeed_if (keyNextMeta(key) == 0, "Could get next meta name, even if it is empty at 4. iteration");
	succeed_if (keyCurrentMeta(key) == 0, "Could get next meta value, even if it is empty at 4. iteration");

	keyDel (key);
}

void test_size()
{
	Key *key;
	char *buffer;

	key = keyNew ("user/test", KEY_END);
	exit_if_fail (key, "could not create new key");
	succeed_if (keyMeta(key, "hello") == 0, "hello was not set up to now");
	succeed_if (keyGetMetaSize (key, "hello") == 0,
			"got wrong size for empty meta value");

	keySetMeta(key, "hello", "hello_world");
	succeed_if (!strcmp(keyMeta(key, "hello"), "hello_world"),
			"could not receive previously set meta information");
	succeed_if (keyGetMetaSize (key, "hello") == sizeof("hello_world"),
			"got wrong size");

	keySetMeta(key, "mode", "0644");
	keySetMeta(key, "time", "1271234264");
	succeed_if (!strcmp(keyMeta(key, "hello"), "hello_world"),
			"meta info changed unexpectly");
	succeed_if (!strcmp(keyMeta(key, "mode"), "0644"), "mode not set correctly");
	succeed_if (keyGetMetaSize (key, "mode") == sizeof("0644"),
			"got wrong size");
	succeed_if (!strcmp(keyMeta(key, "time"), "1271234264"), "time not set correctly");
	succeed_if (keyGetMetaSize (key, "time") == sizeof("1271234264"),
			"got wrong size");

	keySetMeta(key, "hello", "between");
	succeed_if (!strcmp(keyMeta(key, "hello"), "between"),
			"could not set meta information again");
	succeed_if (keyGetMetaSize (key, "hello") == sizeof("between"),
			"got wrong size");
	buffer = calloc (1, keyGetMetaSize (key, "hello"));
	succeed_if (keyGetMeta (key, "hello", buffer, keyGetMetaSize (key, "hello")) == keyGetMetaSize (key, "hello"),
			"could not get meta");
	succeed_if (!strcmp(buffer, "between"), "buffer was not set correctly");
	free (buffer);


	keySetMeta(key, "hello", 0);
	succeed_if (keyMeta(key, "hello") == 0, "could not remove meta data");
	succeed_if (keyGetMetaSize (key, "hello") == 0,
			"got wrong size");

	keySetMeta(key, "hello", "goodbye");
	succeed_if (!strcmp(keyMeta(key, "hello"), "goodbye"),
			"could not set meta information again (2x)");
	succeed_if (keyGetMetaSize (key, "hello") == sizeof("goodbye"),
			"got wrong size");
	buffer = calloc (1, keyGetMetaSize (key, "hello"));
	succeed_if (keyGetMeta (key, "hello", buffer, keyGetMetaSize (key, "hello")) == keyGetMetaSize (key, "hello"),
			"could not get meta");
	succeed_if (!strcmp(buffer, "goodbye"), "buffer was not set correctly");
	free (buffer);

	keySetMeta(key, "empty", "");
	succeed_if (!strcmp(keyMeta(key, "empty"), ""), "Problem with empty meta string");
	succeed_if (keyGetMetaSize (key, "empty") == sizeof(""),
			"got wrong size");
	buffer = calloc (1, keyGetMetaSize (key, "empty"));
	succeed_if (keyGetMeta (key, "empty", buffer, keyGetMetaSize (key, "empty")) == keyGetMetaSize (key, "empty"),
			"could not get meta");
	succeed_if (!strcmp(buffer, ""), "buffer was not set correctly");
	free (buffer);

	keySetMeta(key, "", "empty");
	succeed_if (!strcmp(keyMeta(key, ""), "empty"), "Problem with empty name");
	succeed_if (keyGetMetaSize (key, "") == sizeof("empty"),
			"got wrong size");
	buffer = calloc (1, keyGetMetaSize (key, ""));
	succeed_if (keyGetMeta (key, "", buffer, keyGetMetaSize (key, "")) == keyGetMetaSize (key, ""),
			"could not get meta");
	succeed_if (!strcmp(buffer, "empty"), "buffer was not set correctly");
	free (buffer);

	keySetMeta(key, "", "");
	succeed_if (!strcmp(keyMeta(key, ""), ""), "Problem with empty name and string");
	succeed_if (keyGetMetaSize (key, "") == sizeof(""),
			"got wrong size");
	buffer = calloc (1, keyGetMetaSize (key, ""));
	succeed_if (keyGetMeta (key, "", buffer, keyGetMetaSize (key, "")) == keyGetMetaSize (key, ""),
			"could not get meta");
	succeed_if (!strcmp(buffer, ""), "buffer was not set correctly");
	free (buffer);

	keySetMeta(key, "", 0);
	succeed_if (keyMeta(key, "") == 0, "could not remove empty meta data");
	succeed_if (keyGetMetaSize (key, "") == 0,
			"got wrong size");


	keyDel (key);

}

void test_uid()
{
	Key *key;

	key = keyNew ("user/uid", KEY_UID, 100, KEY_END);
	succeed_if (!strcmp(keyMeta (key, "uid"), "100"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == 100, "uid was not set correctly");

	succeed_if (keySetUID(key, 101) == 0, "could not set uid");
	succeed_if (!strcmp(keyMeta (key, "uid"), "101"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == 101, "uid was not set correctly");

	succeed_if (keySetUID(key, 0) == 0, "could not set uid");
	succeed_if (!strcmp(keyMeta (key, "uid"), "0"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == 0, "uid was not set correctly");

	succeed_if (keySetUID(key, (uid_t)-1) == 0, "could not set uid");
	warn_if_fail (!strcmp(keyMeta (key, "uid"), "-1"),
			"this is for 64bit, other platforms might have other results here");
	succeed_if (keyGetUID(key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "102") == sizeof("102"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "uid"), "102"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == 102, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x") == sizeof("x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "uid"), "x"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x1") == sizeof("x1"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "uid"), "x1"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "2000000") == sizeof("2000000"), "could not set large uid");
	succeed_if (!strcmp(keyMeta (key, "uid"), "2000000"), "meta value for large uid was not set correctly");
	succeed_if (keyGetUID(key) == 2000000, "large uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "1x") == sizeof("1x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "uid"), "1x"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "50x") == sizeof("50x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "uid"), "50x"), "meta value for uid was not set correctly");
	succeed_if (keyGetUID(key) == (uid_t)-1, "uid was not set correctly");

	keyDel (key);

	key = keyNew ("user/uid", KEY_END);
	succeed_if (keyMeta (key, "uid") == 0, "got value, but uid was not set up to now");
	succeed_if (keyGetUID(key) == (uid_t)-1, "got value, but uid was not set up to now");

	keyDel (key);
}

void test_dup()
{
	Key *key;
	Key *dup;

	key = keyNew ("user/orig", KEY_END);
	succeed_if (keySetMeta (key, "test", "some_meta_test") == sizeof("some_meta_test"),
			"could not set meta");
	succeed_if (!strcmp(keyMeta (key, "test"), "some_meta_test"), "could not set meta value");

	dup = keyDup (key);
	succeed_if (!strcmp(keyMeta (dup, "test"), "some_meta_test"),
			"in duplicated key meta value was not copied");
	succeed_if (keySetMeta (dup, "test", "some_other_meta_test") == sizeof("some_other_meta_test"),
			"could not set meta");
	succeed_if (!strcmp(keyMeta (dup, "test"), "some_other_meta_test"),
			"in duplicated key meta value was not changed");
	succeed_if (!strcmp(keyMeta (key, "test"), "some_meta_test"),
			"in original key the value has changed");
	keyDel (dup);

	keyDel (key);
}

void test_comment()
{
	Key *key;
	char ret[10];

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (strcmp (keyComment(key), "") == 0, "Empty comment problem");
	succeed_if (keyGetCommentSize(key) == 1, "Empty comment size problem");
	succeed_if (keyMeta(key, "comment") == 0, "No comment up to now");

	succeed_if (keySetComment (key,0) == 1, "could not remove comment");
	succeed_if (keyMeta(key, "comment") == 0, "There should be an no comment");
	succeed_if (!strcmp (keyComment(key), ""), "Empty comment problem");
	succeed_if (keyGetCommentSize(key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment(key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment(key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key,"") == 1, "could not remove comment");
	succeed_if (keyMeta(key, "comment") == 0, "There should be an no comment");
	succeed_if (!strcmp (keyComment(key), ""), "Empty comment problem");
	succeed_if (keyGetCommentSize(key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment(key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment(key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key,"mycom") == sizeof("mycom"), "could not set comment");
	succeed_if (!strcmp(keyMeta(key, "comment"), "mycom"), "There should be my comment");
	succeed_if (!strcmp (keyComment(key), "mycom"), "My comment problem");
	succeed_if (keyGetCommentSize(key) == sizeof("mycom"), "My comment size problem");
	succeed_if (keyGetComment(key, ret, 0) == -1, "Could not get my comment");
	succeed_if (keyGetComment(key, ret, 1) == -1, "Could not get my comment");
	succeed_if (keyGetComment(key, ret, sizeof("mycom")) == sizeof("mycom"), "Could not get my comment");
	succeed_if (!strcmp (ret, "mycom"), "keyGetComment did not return my comment");
	succeed_if (keyDel (key) == 0, "could not delete key");
}

void test_owner()
{
	Key *key;

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keyMeta(key, "owner") == 0, "owner set for empty key");
	succeed_if (!strcmp(keyOwner(key), ""), "owner set for empty key");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew("system/key", KEY_END), "could not create new key");
	succeed_if (keyMeta(key, "owner") == 0, "owner set for empty key");
	succeed_if (!strcmp(keyOwner(key), ""), "owner set for empty key");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew("user/key", KEY_END), "could not create new key");
	succeed_if (keyMeta(key, "owner") == 0, "owner set for empty key");
	succeed_if (!strcmp(keyOwner(key), ""), "owner set for empty key");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew("user/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner(key,"markus") == sizeof("markus"), "could not set owner markus");
	succeed_if (!strcmp(keyMeta(key, "owner"), "markus"), "no owner set for key");
	succeed_if (!strcmp(keyOwner(key), "markus"), "no owner set for key");
	succeed_if (keyDel (key) == 0, "could not delete key");


	succeed_if (key = keyNew("user:markus/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner(key,"markus") == sizeof("markus"), "could not set owner markus");
	succeed_if (!strcmp(keyMeta(key, "owner"), "markus"), "no owner set for key");
	succeed_if (!strcmp(keyOwner(key), "markus"), "no owner set for key");
	succeed_if (keyDel (key) == 0, "could not delete key");

	setenv ("USER", "markus", 1);
	succeed_if (key = keyNew("user/key", KEY_END), "could not create new key with env");
	succeed_if (keyMeta(key, "owner") == 0, "owner set for empty key with env");
	succeed_if (!strcmp(keyOwner(key), ""), "owner set for empty key with env");
	succeed_if (keyDel (key) == 0, "could not delete key with env");
}

void test_mode()
{
	Key *key;

	key = keyNew ("user/mode", KEY_MODE, 0100, KEY_END);
	succeed_if (!strcmp(keyMeta (key, "mode"), "100"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == 0100, "mode was not set correctly");

	succeed_if (keySetMode(key, 0101) == 0, "could not set mode");
	succeed_if (!strcmp(keyMeta (key, "mode"), "101"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == 0101, "mode was not set correctly");

	succeed_if (keySetMode(key, 0) == 0, "could not set mode");
	succeed_if (!strcmp(keyMeta (key, "mode"), "0"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == 0, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "102") == sizeof("102"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "102"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == 0102, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "0103") == sizeof("0103"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "0103"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == 0103, "mode was not set correctly with leading octal 0");

	succeed_if (keySetMeta (key, "mode", "x") == sizeof("x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "x"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "x1") == sizeof("x1"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "x1"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "2000000") == sizeof("2000000"), "could not set large mode");
	succeed_if (!strcmp(keyMeta (key, "mode"), "2000000"), "meta value for large mode was not set correctly");
	succeed_if (keyGetMode(key) == 02000000, "large mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "1x") == sizeof("1x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "1x"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "50x") == sizeof("50x"), "could not set meta");
	succeed_if (!strcmp(keyMeta (key, "mode"), "50x"), "meta value for mode was not set correctly");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "mode was not set correctly");

	keyDel (key);

	key = keyNew ("user/mode", KEY_END);
	succeed_if (keyMeta (key, "mode") == 0, "got value, but mode was not set up to now");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "KEY_DEF_MODE not default on new key");

	succeed_if (keySetMeta (key, "mode", "") == sizeof(""), "could not set large mode");
	succeed_if (!strcmp(keyMeta (key, "mode"), ""), "meta value for large mode was not set correctly");
	succeed_if (keyGetMode(key) == KEY_DEF_MODE, "empty mode should also yield default");

	keyDel (key);
}

void test_type()
{
	Key *key;

	succeed_if (key = keyNew(0), "could not create a new key");
	succeed_if (keyMeta(key, "binary") == 0, "wrong type after key creation");
	succeed_if (keySetString (key, "mystring") == sizeof("mystring"), "could not set string");
	succeed_if (keyMeta(key, "binary") == 0, "wrong type after setting string");
	succeed_if (keySetBinary (key, "mystring", sizeof("mystring")) == sizeof("mystring"),
			"could not set binary");
	succeed_if (keyMeta(key, "binary") != 0, "wrong type after setting string");

	keyDel (key);
}

Key *c;

void j (Key *k)
{
	size_t size = keyGetValueSize (k);
	char *value = malloc (size);
	int bstring = keyIsString (k);

	// receive key c
	memcpy (value, keyValue(k), size);
	keyCopy (k, c);
	if (bstring) keySetString (k, value);
	else keySetBinary (k, value, size);
	free (value);
	// the caller will see the changed key k
	// with the metadata from c
}

void l(Key *k)
{
	// receive c
	keyCopyMeta(k, c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from c
}

void test_examples()
{
	Key *key;
	key = keyNew(0);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	c = keyNew(0);
	keySetMeta (c, "xef", "ybc");
	keySetMeta (c, "xop", "yup");

	j(key);

	succeed_if (!strcmp(keyMeta(key, "xef"), "ybc"), "did not copy meta");
	succeed_if (!strcmp(keyMeta(key, "xop"), "yup"), "did not copy meta");
	succeed_if (keyMeta(key, "def") == 0, "old meta data remained");
	succeed_if (keyMeta(key, "nop") == 0, "old meta data remained");

	keyDel (key);
	keyDel (c);

	key = keyNew(0);
	keySetMeta (key, "def", "abc");
	keySetMeta (key, "nop", "cup");

	c = keyNew(0);
	keySetMeta (c, "type", "boolean");
	keySetMeta (c, "xop", "yup");

	l (key);

	succeed_if (!strcmp(keyMeta(key, "def"), "abc"), "old meta data should be unchanged");
	succeed_if (!strcmp(keyMeta(key, "nop"), "cup"), "old meta data should be unchanged");
	succeed_if (!strcmp(keyMeta(key, "type"), "boolean"), "the meta data was not copied");
	succeed_if (keyMeta(key, "xop") == 0, "this meta data was not requested to be copied");

	keyDel (key);
	keyDel (c);
}

void test_copy()
{
	Key *key1;
	Key *key2;

	succeed_if (key1 = keyNew(0), "could not create key");
	succeed_if (key2 = keyNew(0), "could not create key");

	succeed_if (keySetMeta(key1, "mymeta", "a longer meta value") == sizeof("a longer meta value"),
			"could not set meta value");
	succeed_if (keyCopyMeta(key2, key1, "mymeta") == 1, "could not copy meta value");
	succeed_if (!strcmp(keyMeta(key1, "mymeta"), "a longer meta value"), "old meta data should be unchanged");
	succeed_if (!strcmp(keyMeta(key2, "mymeta"), "a longer meta value"), "old meta data should be unchanged");

	keyDel (key1);
	keyDel (key2);
}


int main(int argc, char** argv)
{
	printf("KEY META     TESTS\n");
	printf("==================\n\n");

	init (argc, argv);
	test_basic();
	test_iterate();
	test_size();
	test_uid();
	test_dup();
	test_comment();
	test_owner();
	test_mode();
	test_type();
	test_examples();
	test_copy();


	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

