/*************************************************************************** 
 *  test_backendhelpers.c  - Test suite for helper functions of resolver
 *                  -------------------
 *  begin                : Fri 21 Mar 2008
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
#include "resolver.h"



int main(int argc, char** argv)
{
	printf("  RESOLVER  TESTS\n");
	printf("====================\n\n");

	init (argc, argv);


	printf("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

