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


int main(int argc, char** argv)
{
	printf("KEY META     TESTS\n");
	printf("==================\n\n");

	init (argc, argv);
	test_basic();
	test_iterate();
	test_size();


	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

