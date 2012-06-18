/*************************************************************************** 
 *           test_internal.c  - Test suite for internal data structures
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

#include <tests_internal.h>

void test_simple_strlen ()
{
	char charSeq [5];

	printf ("Test elektraStrLen\n");
	charSeq [0] = '\33';
	charSeq [1] = 'a';
	charSeq [2] = '\20';
	charSeq [3] = '\40';
	charSeq [4] = '\0';

	// printf ("%s %d %d\n", charSeq, elektraStrLen (charSeq), strlen(charSeq));
	succeed_if(elektraStrLen ((char*)charSeq) == 5, "could not deduce correct multichar sequence length");
}

void test_strlen ()
{
	wchar_t multicharSeq [5];

	printf ("Test elektraStrLen with multichar\n");
	multicharSeq [0] = '\323';
	multicharSeq [1] = L'a';
	multicharSeq [2] = L'\20';
	multicharSeq [3] = L'\40';
	multicharSeq [4] = L'\0';

	// printf ("%s %d %d\n", multicharSeq, elektraStrLen (multicharSeq), strlen(multicharSeq));
	succeed_if(elektraStrLen ((char*)multicharSeq) == 6, "could not deduce correct multichar sequence length");
}

int main(int argc, char** argv)
{
	printf("INTERNALS    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_simple_strlen();
	test_strlen();

	printf("\ntest_internals RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

