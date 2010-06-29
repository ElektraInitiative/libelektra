/*************************************************************************** 
 *           test_splitset.c  - Test suite for splitted keyset data structure
 *                  -------------------
 *  begin                : Tue Jun 29 2010
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




int main(int argc, char** argv)
{
	printf("SPLIT GET   TESTS\n");
	printf("==================\n\n");

	init (argc, argv);


	printf("\ntest_splitget RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

