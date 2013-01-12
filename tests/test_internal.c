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

void test_elektraMalloc()
{
	char * buffer=0;
	buffer = elektraMalloc(50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	elektraRealloc((void**)&buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc((void**)&buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraFree(buffer);

	buffer = elektraCalloc(50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	for (int i=0; i<50; ++i)
	{
		succeed_if (buffer[i] == 0, "elektraCalloc did not initialize buffer with zeros");
	}
	elektraRealloc((void**)&buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc((void**)&buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");

	char * dup = elektraStrNDup(buffer, 20);
	exit_if_fail (dup, "could not duplicate buffer");
	elektraFree(buffer);
	buffer = 0;
	for (int i=0; i<20; ++i)
	{
		succeed_if (dup[i] == 0, "elektraStrNDup did not correctly copy zero-buffer");
	}
	elektraFree(dup);
}

void test_elektraStrLen()
{
	char charSeq [5];

	printf ("Test elektraStrLen\n");
	for (int i=1; i<255; ++i)
	{
		charSeq [0] = '\33';
		charSeq [1] = 'a';
		charSeq [2] = i;     // 1..254
		charSeq [3] = 256-i; // 255..2
		charSeq [4] = '\0';

		// printf ("%s %d %d\n", charSeq, elektraStrLen (charSeq), strlen(charSeq));
		succeed_if(elektraStrLen ((char*)charSeq) == 5, "could not deduce correct multichar sequence length");
	}
}


int main(int argc, char** argv)
{
	printf("INTERNALS    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_elektraMalloc();
	test_elektraStrLen();

	printf("\ntest_internals RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

