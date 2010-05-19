/***************************************************************************
 *          test_rel.c  -  Relation between keys
 *                  -------------------
 *  begin                : Wed 19 May, 2010
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

#include <tests.h>

void test_equal()
{
	Key *k1 = keyNew(0);
	Key *k2 = keyNew(0);

	succeed_if (keyCmp (0,0)    == 0, "null pointers should be same");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, ""); keySetName (k2, "");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user"); keySetName (k2, "user");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system"); keySetName (k2, "system");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user/a"); keySetName (k2, "user/a");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keyDel (k1);
	keyDel (k2);
}

void test_directbelow()
{
	Key *k1 = keyNew(0);
	Key *k2 = keyNew(0);

	keySetName (k1, "user"); keySetName (k2, "user/a");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");
	// printf ("%d\n", keyRel (k1, k2));

	keySetName (k1, "user/a"); keySetName (k2, "user");
	succeed_if (keyRel (k1, k2) == -1, "should be reverse direct below");

	keySetName (k1, "system"); keySetName (k2, "system/a");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "system/a"); keySetName (k2, "system");
	succeed_if (keyRel (k1, k2) == -1, "should be direct below");

	keySetName (k1, "user"); keySetName (k2, "user/longer_name");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "user/longer_name"); keySetName (k2, "user");
	succeed_if (keyRel (k1, k2) == -1, "should be reverse direct below");

	keySetName (k1, "system"); keySetName (k2, "system/longer_name");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "system/longer_name"); keySetName (k2, "system");
	succeed_if (keyRel (k1, k2) == -1, "should be direct below");

	keySetName (k1, "user/a"); keySetName (k2, "user/a/a");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "user/a/a"); keySetName (k2, "user/a");
	succeed_if (keyRel (k1, k2) == -1, "should be reverse direct below");

	keySetName (k1, "system/a"); keySetName (k2, "system/a/a");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "system/a/a"); keySetName (k2, "system/a");
	succeed_if (keyRel (k1, k2) == -1, "should be direct below");


	keyDel (k1);
	keyDel (k2);
}

void test_below()
{
	Key *k1 = keyNew(0);
	Key *k2 = keyNew(0);

	keySetName (k1, "user"); keySetName (k2, "user/a/a");
	succeed_if (keyRel (k1, k2) == 2, "should be below");
	// printf ("%d\n", keyRel (k1, k2));

	keySetName (k1, "user/a/a"); keySetName (k2, "user");
	succeed_if (keyRel (k1, k2) == -2, "should be reverse below");

	keySetName (k1, "system"); keySetName (k2, "system/a/a");
	succeed_if (keyRel (k1, k2) == 2, "should be below");

	keySetName (k1, "system/a/a"); keySetName (k2, "system");
	succeed_if (keyRel (k1, k2) == -2, "should be below");

	keySetName (k1, "user"); keySetName (k2, "user/longer_name/also_longer_name");
	succeed_if (keyRel (k1, k2) == 2, "should be below");

	keySetName (k1, "user/longer_name/also_longer_name"); keySetName (k2, "user");
	succeed_if (keyRel (k1, k2) == -2, "should be reverse below");

	keySetName (k1, "system"); keySetName (k2, "system/longer_name/also_longer_name");
	succeed_if (keyRel (k1, k2) == 2, "should be below");

	keySetName (k1, "system/longer_name/also_longer_name"); keySetName (k2, "system");
	succeed_if (keyRel (k1, k2) == -2, "should be below");

	keySetName (k1, "user/a"); keySetName (k2, "user/a/a/a/a/a/a");
	succeed_if (keyRel (k1, k2) == 2, "should be below");

	keySetName (k1, "user/a/a/a/a/a/a"); keySetName (k2, "user/a");
	succeed_if (keyRel (k1, k2) == -2, "should be reverse below");

	keySetName (k1, "system/a"); keySetName (k2, "system/a/a/a/a/a/a");
	succeed_if (keyRel (k1, k2) == 2, "should be below");

	keySetName (k1, "system/a/a/a/a/a/a"); keySetName (k2, "system/a");
	succeed_if (keyRel (k1, k2) == -2, "should be below");


	keyDel (k1);
	keyDel (k2);
}

void test_examples()
{
	Key *k1 = keyNew(0);
	Key *k2 = keyNew(0);

	keySetName (k1, "user/key/folder"); keySetName (k2, "user/key/folder/child");
	succeed_if (keyRel (k1, k2) == 1, "should be direct below");
	succeed_if (keyRel (k2, k1) == -1, "should be direct below");

	keySetName (k1, "user/key/folder"); keySetName (k2, "user/key/folder/any/depth/deeper/grand-child");
	succeed_if (keyRel (k1, k2) == 2, "should be below");
	succeed_if (keyRel (k2, k1) == -2, "should be below");

	keyDel (k1);
	keyDel (k2);
}



int main(int argc, char** argv)
{
	printf("KEY RELATION TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_directbelow();
	test_below();
	test_examples();

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
