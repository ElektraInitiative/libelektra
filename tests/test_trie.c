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

void *mapper(void *s,char *backend)
{
	return s;
}

void test_kdbTrie()
{
	KDB *kdb = kdbOpen();

	KeySet *ks=ksNew(0);

	/*
	k = keyNew ("user/template/key",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-template",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	*/

	/*
	k = keyNew ("user/key",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-filesys",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	
	k = keyNew ("system/key",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-filesys",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);

	k = keyNew ("system/filesystems/hda/",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-fstab",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	
	printf ("Looking up dynamic mounting\n");
	printf(BACKEND_DIR "fstab2/config/path\n");
	ksAppendKey(ks,keyNew(BACKEND_DIR "fstab2/config/path",KEY_VALUE,"/tmp/fstab2",0));
	kdbMount(kdb, mountpoint=keyNew("system/fstab/",0), "fstab",ks);

	k = keyNew ("system/fstab/dev/hda",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-fstab",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);

	kdbUnmount(kdb,mountpoint);
	keyDel(mountpoint);

	k = keyNew ("system/fstab/dev/hda",0);
	s=kdbGetBackend(kdb,k);
	exit_if_fail(s, "Got null pointer from kdbGetBackend");
	succeed_if(!strcmp("libelektra-filesys",kdbhGetBackendName(s)), "kdbGetBackend: didn't get the correct value");
	keyDel (k);
	*/
	/**/

	ksDel (ks);
	kdbClose (kdb);
}

int main(int argc, char** argv)
{
	printf("TRIE       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_kdbTrie();

	printf("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

