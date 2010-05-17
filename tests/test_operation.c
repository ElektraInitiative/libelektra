/***************************************************************************
 *      test_operation.c  -  KeySet operation test suite
 *                  -------------------
 *  begin                : Thu Dez 12 2006
 *  copyright            : (C) 2006 by Markus Raab
 *  email                : sizon5@gmail.com
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

KeySet * set_a ()
{
	return ksNew(20,
		keyNew ("user/a", KEY_END),
		keyNew ("user/a/a", KEY_END),
		keyNew ("user/a/a/a", KEY_END),
		keyNew ("user/a/a/b", KEY_END),
		keyNew ("user/a/b", KEY_END),
		keyNew ("user/a/b/a", KEY_END),
		keyNew ("user/a/b/b", KEY_END),
		keyNew ("user/a/c", KEY_END),
		keyNew ("user/a/d", KEY_END),
		keyNew ("user/a/x/a", KEY_END),
		keyNew ("user/a/x/b", KEY_END),
		keyNew ("user/a/x/c", KEY_END),
		keyNew ("user/a/x/c/a", KEY_END),
		keyNew ("user/a/x/c/b", KEY_END),
		KS_END);
}

void test_search()
{
	KeySet *a = set_a();
	Key *s = keyNew("user/a", KEY_END);
	ssize_t result;

	result = ksSearchInternal (a, s);
	succeed_if (result == 0, "key not found");

	keySetName (s, "user/a/b");
	result = ksSearchInternal (a, s);
	succeed_if (result == 4, "middle wrong");

	keySetName (s, "user/a/x");
	result = ksSearchInternal (a, s);
	succeed_if (result == -10, "insertpos wrong");

	keySetName (s, "user/a/e");
	result = ksSearchInternal (a, s);
	succeed_if (result == -10, "insertpos wrong");
}

void test_cut()
{
	KeySet *a = set_a();
	Key *cutpoint_a = keyNew ("user/y", KEY_END);
	Key *cutpoint_b = keyNew ("user/b", KEY_END);
	ksOutput(a, stdout, KEY_VALUE);

	KeySet *aa = ksCut(set_a, cutpoint_a);
	KeySet *ab = ksCut(set_a, cutpoint_b);
}


int main(int argc, char** argv)
{
	printf("OPERATION    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_search();
	// test_cut();

	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

