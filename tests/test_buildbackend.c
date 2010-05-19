/*************************************************************************** 
 *      test_buildbackend.c  -  Test cases for how to build
*        a backend out of system/elektra/mountpoints/<name>
 *                  -------------------
 *  begin                : Fri 19 May 2010
 *  copyright            : (C) 2008 by Markus Raab
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


#include <langinfo.h>

#include <tests.h>

/*Needs private declarations*/
#include <kdbbackend.h>


KeySet *set_simple()
{
	return ksNew(20,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/simple", KEY_END),
		keyNew("system/elektra/mountpoints/simple/setplugins/#1#tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/simple/getplugins/#1#tracer", KEY_VALUE, "tracer", KEY_END),
		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),
		KS_END);

}

void test_simple()
{
	printf ("Test simple building of backend");

}

int main(int argc, char** argv)
{
	printf("BUILDBACKEND   TESTS\n");
	printf("====================\n\n");

	init (argc, argv);

	test_simple();

	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

