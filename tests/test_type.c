/***************************************************************************
 *          test_type.c  -  Typing related test suite
 *                  -------------------
 *  begin                : Thu Aug 03 2006
 *  copyright            : (C) 2008 Markus Raab
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

#include "tests.h"

enum
{
	KEY_TYPE_COLOR=KEY_TYPE_STRING+4
};

void test_typeMax(void)
{
	Key *k1 = keyNew(0);

	succeed_if (keyGetType(0) == -1, "null pointer check");
	succeed_if (keySetType(0, KEY_TYPE_COLOR) == -1, "null pointer check");

	succeed_if (keySetType(k1, 256) == -1, "type to large");
	succeed_if (keySetType(k1, 257) == -1, "type to large");
	keyDel (k1);
}

void test_typeColor(void)
{
	Key *k1 = keyNew ("user/sw/oyranos/current/color1",
		KEY_VALUE, "#4B52CA",
		KEY_COMMENT, "a custom color",
		KEY_TYPE, KEY_TYPE_COLOR,
		KEY_END);
	Key *k2 = keyNew ("user/sw/oyranos/current/color2",
		KEY_VALUE, "green",
		KEY_COMMENT, "the green color",
		KEY_TYPE, KEY_TYPE_COLOR,
		KEY_END);

	succeed_if (keyGetType(k1) == KEY_TYPE_COLOR, "not type color");
	succeed_if (keyGetType(k2) == KEY_TYPE_COLOR, "not type color");
	succeed_if (keyIsString(k1) == 1, "key should be string");
	succeed_if (keyIsString(k2) == 1, "key should be string");
	keyDel (k1);
	keyDel (k2);

	k1 = keyNew (0);

	succeed_if (keySetType(k1, KEY_TYPE_COLOR) == 0, "could not set color");
	succeed_if (keyGetType(k1) == KEY_TYPE_COLOR, "not type color");
	succeed_if (keyIsString(k1) == 1, "key should be string");
	keyDel (k1);
}

int main()
{
	printf("KEY  TYPE  TESTS\n");
	printf("==================\n\n");

	init ();
	test_typeMax();
	test_typeColor();

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
