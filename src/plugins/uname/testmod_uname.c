/*************************************************************************** 
 *           testmod_fstab.c  - Test suite for fstab
 *                  -------------------
 *  begin                : Thu Nov 6 2007
 *  copyright            : (C) 2007 by Patrick Sabin
 *  email                : patricksabin@gmx.at
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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
	printf("UNAME       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);


	printf("\ntestmod_uname RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

