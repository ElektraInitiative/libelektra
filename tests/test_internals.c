/*************************************************************************** 
 *           test_split.c  - Test suite for splitted keyset data structure
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

#include <tests.h>

/*Needs private declarations*/
#include <kdbbackend.h>

void test_strlen ()
{
	wchar_t multicharSeq [5];

	printf ("Test kdbiStrLen\n");
	multicharSeq [0] = '\323';
	multicharSeq [1] = L'a';
	multicharSeq [2] = L'\20';
	multicharSeq [3] = L'\40';
	multicharSeq [4] = L'\0';

	// printf ("%s %d %d\n", multicharSeq, kdbiStrLen (multicharSeq), strlen(multicharSeq));
	succeed_if(kdbiStrLen ((char*)multicharSeq) == 6, "could not deduce correct multichar sequence length");
}

int test_load_toolslib()
{
	KeySet		*ks = ksNew (0);

	exit_if_fail (loadToolsLib()==0, "Unable to load elektratools");
	exit_if_fail( ksFromXMLfile(ks, "key.xml") == 0, "ksFromXMLfile(key.xml) failed.");
	ksGenerate (ks, stdout, KDB_O_HEADER);

	ksDel (ks);
	return 0;
}

int main()
{
	printf("INTERNALS    TESTS\n");
	printf("==================\n\n");

	init ();

	test_strlen();
	// test_load_toolslib();

	printf("\ntest_internals RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

