/**
 * \file
 *
 * \brief Tests for the hosts plugin
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

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

void test_readHostsSimple(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, srcdir_file(fileName), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks=ksNew(0, KS_END);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/hosts/ipv4/localhost", 0);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp("127.0.0.1", keyValue(key)) == 0, "address not correct");
	succeed_if (strcmp("will lose that comment", keyComment(key)) == 0, "comment not correct");

	key = ksLookupByName(ks, "user/tests/hosts/ipv4/gateway.markus-raab.org", 0);
	exit_if_fail (key, "hostname gateway.markus-raab.org not found");
	succeed_if (strcmp("192.168.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName(ks, "user/tests/hosts/ipv4/kirabyte.markus-raab.org/kira", 0);
	exit_if_fail (key, "hostname alias kira not found");
	succeed_if (strcmp("192.168.0.5", keyValue(key)) == 0, "address not correct");
	succeed_if (strcmp(" leader of the people\n and more\n", keyComment(key)) == 0, "comment not correct");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

void test_readInvalidIpAddress(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, srcdir_file(fileName), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks=ksNew(0, KS_END);
	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/hosts/ipv4/localhost", KDB_O_NONE);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp("noipaddress", keyValue(key)) == 0, "address not correct");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

void test_mixedAddresses(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, srcdir_file(fileName), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks=ksNew(0, KS_END);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/hosts/ipv4/ipv4host", KDB_O_NONE);
	exit_if_fail (key, "hostname ipv4host not found");
	succeed_if (strcmp("127.0.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user/tests/hosts/ipv4/ipv4host/ipv4alias1", KDB_O_NONE);
	succeed_if (key, "ipv4alias1 not found");
	key = ksLookupByName (ks, "user/tests/hosts/ipv4/ipv4host/ipv4alias2", KDB_O_NONE);
	succeed_if (key, "ipv4alias2 not found");

	key = ksLookupByName(ks, "user/tests/hosts/ipv6/ipv6host", KDB_O_NONE);
	exit_if_fail (key, "hostname ipv6host not found");
	succeed_if (strcmp("::1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName (ks, "user/tests/hosts/ipv6/ipv6host/ipv6alias1", KDB_O_NONE);
	succeed_if (key, "ipv6alias1 not found");
	key = ksLookupByName (ks, "user/tests/hosts/ipv6/ipv6host/ipv6alias2", KDB_O_NONE);
	succeed_if (key, "ipv6alias2 not found");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

void test_duplicateEntries(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, srcdir_file(fileName), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks=ksNew(0, KS_END);

	succeed_if (plugin->kdbGet(plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	Key *key = ksLookupByName(ks, "user/tests/hosts/ipv4/localhost", KDB_O_NONE);
	exit_if_fail (key, "hostname localhost not found");
	succeed_if (strcmp("127.0.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName(ks, "user/tests/hosts/ipv4/host", KDB_O_NONE);
	exit_if_fail (key, "hostname host not found");
	succeed_if (strcmp("192.168.0.1", keyValue(key)) == 0, "address not correct");

	key = ksLookupByName(ks, "user/tests/hosts/ipv4/host/alias1", KDB_O_NONE);
	succeed_if (key, "alias1 not found");
	key = ksLookupByName(ks, "user/tests/hosts/ipv4/host/alias2", KDB_O_NONE);
	succeed_if (key, "alias2 not found");

	ksDel (ks);
	keyDel(parentKey);

	PLUGIN_CLOSE();
}

void test_duplicateOrder(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, elektraFilename(), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks = ksNew (20,
			keyNew ("user/tests/hosts/ipv4/host1",
					KEY_VALUE, "192.168.0.1",
					KEY_META, "order", "10",
					KEY_END),
			keyNew ("user/tests/hosts/ipv4/host2",
					KEY_VALUE, "192.168.0.2",
					KEY_META, "order", "20",
					KEY_END),
			keyNew ("user/tests/hosts/ipv4/host3",
					KEY_VALUE, "192.168.0.3",
					KEY_META, "order", "20",
					KEY_END),
			KS_END);

	ksAppendKey (ks, parentKey);

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	succeed_if(compare_line_files (srcdir_file (fileName), keyString (parentKey)),
			"files do not match as expected");

	elektraUnlink (keyString (parentKey));
	ksDel (ks);

	PLUGIN_CLOSE()
	;
}

void test_writeHostsSimple(char *fileName)
{
	Key * parentKey = keyNew ("user/tests/hosts", KEY_VALUE, elektraFilename(), KEY_END);
	KeySet *conf = 0;
	PLUGIN_OPEN("hosts");

	KeySet *ks = ksNew (20,
			keyNew ("user/tests/hosts/ipv4/localhost",
					KEY_VALUE, "127.0.0.1",
					KEY_META, "order", "10",
					KEY_COMMENT, "these are for ipv4\n",
					KEY_END),
			keyNew ("user/tests/hosts/ipv4/testhost",
					KEY_VALUE, "127.0.1.1",
					KEY_META, "order", "20",
					KEY_END),
			keyNew ("user/tests/hosts/ipv4/testhost/testhostalias",
					KEY_END),
			keyNew ("user/tests/hosts/ipv6/localhost",
					KEY_VALUE, "::1",
					KEY_META, "order", "30",
					KEY_COMMENT, "The following lines are desirable for IPv6 capable hosts\n",
					KEY_END),
			keyNew ("user/tests/hosts/ipv6/localhost/ip6-localhost",
					KEY_END),
			keyNew ("user/tests/hosts/ipv6/localhost/ip6-loopback",
					KEY_END),
			keyNew ("user/tests/hosts/ipv6/ip6-allnodes",
					KEY_VALUE, "ff02::1",
					KEY_META, "order", "40",
					KEY_END),
			KS_END);

	ksAppendKey (ks, parentKey);

	succeed_if(plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if(output_error (parentKey), "error in kdbSet");
	succeed_if(output_warnings (parentKey), "warnings in kdbSet");

	succeed_if(compare_line_files (srcdir_file (fileName), keyString (parentKey)),
			"files do not match as expected");

	elektraUnlink (keyString (parentKey));
	ksDel (ks);

	PLUGIN_CLOSE()
	;
}

int main(int argc, char** argv)
{
	printf("MOUNT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_readHostsSimple("hosts/hosts-read-simple");
	test_readInvalidIpAddress("hosts/hosts-invalid");
	test_mixedAddresses("hosts/hosts-mixed");
	test_duplicateEntries("hosts/hosts-duplicate");
	test_duplicateOrder("hosts/hosts-duporder");
	test_writeHostsSimple("hosts/hosts-write-simple");


	printf("\ntest_hosts RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

