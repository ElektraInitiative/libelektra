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
	Key *k;
	k = keyNew("user/metakey", KEY_END);
	exit_if_fail (k, "could not create new key");
	succeed_if (keyMeta(k, "hello") == 0, "hello was not set up to now");

	keySetMeta(k, "hello", "hello_world");
	succeed_if (!strcmp(keyMeta(k, "hello"), "hello_world"),
			"could not receive previously set meta information");

	keySetMeta(k, "mode", "0644");
	keySetMeta(k, "time", "1271234264");
	succeed_if (!strcmp(keyMeta(k, "hello"), "hello_world"),
			"meta info changed unexpectly");
	succeed_if (!strcmp(keyMeta(k, "mode"), "0644"), "mode not set correctly");
	succeed_if (!strcmp(keyMeta(k, "time"), "1271234264"), "time not set correctly");

	keySetMeta(k, "hello", "between");
	succeed_if (!strcmp(keyMeta(k, "hello"), "between"),
			"could not set meta information again");

	keySetMeta(k, "hello", 0);
	succeed_if (keyMeta(k, "hello") == 0, "could not remove meta data");

	keySetMeta(k, "hello", "goodbye");
	succeed_if (!strcmp(keyMeta(k, "hello"), "goodbye"),
			"could not set meta information again (2x)");

	keySetMeta(k, "empty", "");
	succeed_if (!strcmp(keyMeta(k, "empty"), ""), "Problem with empty meta string");

	keySetMeta(k, "", "empty");
	succeed_if (!strcmp(keyMeta(k, ""), "empty"), "Problem with empty name");

	keySetMeta(k, "", "");
	succeed_if (!strcmp(keyMeta(k, ""), ""), "Problem with empty name and string");

	keySetMeta(k, "", 0);
	succeed_if (keyMeta(k, "") == 0, "could not remove empty meta data");


	keyDel (k);
}


int main(int argc, char** argv)
{
	printf("KEY META     TESTS\n");
	printf("==================\n\n");

	init (argc, argv);
	test_basic();


	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

