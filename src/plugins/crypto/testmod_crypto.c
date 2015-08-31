/*************************************************************************** 
 *           testmod_crypto.c  - Test suite for crypto
 *                  -------------------
 *  begin                : Mon Aug 31 10:32:51 CEST 2015
 *  copyright            : (C) 2015 by Peter Nirschl
 *  email                : peter.nirschl@gmail.com
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

#include <tests_plugin.h>

// TODO add test cases here

int main(int argc, char** argv)
{
	printf("CYPTO       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	/*
	test_readfstab("fstab/fstab");
	test_writefstab("fstab/fstab-write");
	*/

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

