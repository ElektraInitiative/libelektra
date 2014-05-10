/*************************************************************************** 
 *           test_hosts.c  - Test suite for testing backend mounting
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

#include <tests_plugin.h>

void test_readhosts()
{
	Key * parentKey = keyNew ("user/tests/augeas-hosts", KEY_VALUE, srcdir_file("hosts"), KEY_END);
	KeySet *conf = ksNew(20, keyNew ("system/lens", KEY_VALUE, "Hosts.lns", KEY_END), KS_END);
	PLUGIN_OPEN("augeas");

	KeySet *ks=ksNew(0);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/augeas-hosts/1/ipaddr", 0);
	exit_if_fail (key, "ip address of localhost not found");
	succeed_if (strcmp("127.0.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName(ks, "user/tests/augeas-hosts/1/canonical", 0);
	exit_if_fail (key, "name of localhost not found");
	succeed_if (strcmp("localhost", keyValue(key)) == 0, "name not correct");

	ksDel (ks);

	PLUGIN_CLOSE();
}

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_readhosts();

	printf("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

