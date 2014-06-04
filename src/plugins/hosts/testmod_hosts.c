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
	char * filename = srcdir_file("hosts");
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, filename, KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks=ksNew(0);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/hosts/localhost", 0);
	exit_if_fail (key, "hostname localhost kira not found");
	succeed_if (strcmp("127.0.0.1", keyValue(key)) == 0, "address not correct");
	succeed_if (strcmp("will lose that comment", keyComment(key)) == 0, "comment not correct");

	key = ksLookupByName(ks, "user/tests/hosts/gateway.markus-raab.org", 0);
	exit_if_fail (key, "hostname gateway.markus-raab.org not found");
	succeed_if (strcmp("192.168.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName(ks, "user/tests/hosts/kirabyte.markus-raab.org/kira", 0);
	exit_if_fail (key, "hostname alias kira not found");
	succeed_if (strcmp("192.168.0.5", keyValue(key)) == 0, "address not correct");
	succeed_if (strcmp(" leader of the people\n and more\n", keyComment(key)) == 0, "comment not correct");

	ksDel (ks);
	keyDel(parentKey);

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

