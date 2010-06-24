/*************************************************************************** 
 *           test_trie.c  - Test suite for trie data structure
 *                  -------------------
 *  begin                : Thu Oct 24 2007
 *  copyright            : (C) 2007 by Patrick Sabin
 *  email                : patricksabin@gmx.at
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
#include <kdbprivate.h>


KeySet *iterate_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "user/tests/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/below", KEY_END),
		keyNew("system/elektra/mountpoints/below/mountpoint", KEY_VALUE, "user/tests/hosts/below", KEY_END),
		KS_END);
}

void test_iterate()
{
	Key *errorKey = keyNew(0);
	Trie *trie = elektraTrieOpen(iterate_config(), 0, errorKey);

	output_warnings (errorKey);

	keyDel (errorKey);
	exit_if_fail (trie, "trie was not build up successfully");

	/*

	k = keyNew ("user/tests/hosts",0);
	s=kdbGetBackend(kdb,k);
	succeed_if(!strcmp("hosts",keyValue(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	succeed_if(!strcmp("user/tests/hosts",keyName(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	printf ("s: %p\n", s);

	k = keyNew ("user/tests/hosts/anything/deeper/here",0);
	s=kdbGetBackend(kdb,k);
	succeed_if(!strcmp("hosts",keyValue(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	succeed_if(!strcmp("user/tests/hosts",keyName(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	printf ("s: %p\n", s);

	k = keyNew ("user/tests/hosts/below/anything/deeper/here",0);
	s=kdbGetBackend(kdb,k);
	succeed_if(!strcmp("hosts",keyValue(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	succeed_if(!strcmp("user/tests/hosts/below",keyName(s->mountpoint)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	printf ("s: %p\n", s);

	// printf ("%s - %s\n", keyName(s->mountpoint), (const char*)keyValue(s->mountpoint));
	printf ("root trie: %p\n", kdb->trie);
	printf ("host trie: %p\n", s->trie);

	*/

	output_trie(trie);

	elektraTrieClose(trie, 0);
}

int main(int argc, char** argv)
{
	printf("TRIE       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_iterate();

	printf("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

