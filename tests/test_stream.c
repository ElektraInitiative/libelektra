/***************************************************************************
 *          test_stream.c  -  streaming test suite
 *                  -------------------
 *  begin                : Mon 26 Nov 2007
 *  copyright            : (C) 2007 by Markus Raab
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

void test_keyoutput ()
{
	KeySet		*ks;
	FILE *fout = stdout;

	printf("Testing Read and write xml\n");
	ks = ksNew (0);

//	fout = fopen ("key.txt", "w");
	exit_if_fail( ksFromXMLfile(ks, srcdir_file ("key.xml")) == 0, "ksFromXMLfile(key.xml) failed.");
//	compare_files ("key.xml");

//	ksOutput (ks, fout, KEY_VALUE | KEY_COMMENT);
//	ksOutput (ks, fout, KDB_O_SHOWMETA);
	ksOutput (ks, fout, KDB_O_SHOWFLAGS);
	ksDel (ks);
}

int main()
{
	printf("STREAM TESTS\n");
	printf("==================\n\n");

	init ();

	test_keyoutput();
	printf("\ntest_stream RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
