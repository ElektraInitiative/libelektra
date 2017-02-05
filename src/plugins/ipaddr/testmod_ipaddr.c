/**
 * @file
 *
 * @brief Tests for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void testIPv4(const char *ip, int ret)
{
    Key *parentKey = keyNew("user/tests/ipaddr", KEY_VALUE, "", KEY_END);
    KeySet *conf = ksNew(0, KS_END);
    KeySet *ks = ksNew(10, keyNew("user/test/ipaddr/totest", KEY_VALUE, ip, KEY_META, "check/ipaddr", "ipv4", KEY_END), KS_END);
    PLUGIN_OPEN("ipaddr");
    succeed_if(plugin->kdbSet(plugin, ks, parentKey) == ret, "validation failed");
    ksDel(ks);
    keyDel(parentKey);
    PLUGIN_CLOSE();
}

static void testIPv6(const char *ip, int ret)
{
    Key *parentKey = keyNew("user/tests/ipaddr", KEY_VALUE, "", KEY_END);
    KeySet *conf = ksNew(0, KS_END);
    KeySet *ks = ksNew(10, keyNew("user/test/ipaddr/totest", KEY_VALUE, ip, KEY_META, "check/ipaddr", "ipv6", KEY_END), KS_END);
    PLUGIN_OPEN("ipaddr");
    succeed_if(plugin->kdbSet(plugin, ks, parentKey) == ret, "validation failed");
    ksDel(ks);
    keyDel(parentKey);
    PLUGIN_CLOSE();
}
int main (int argc, char ** argv)
{
	printf ("IPADDR     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testIPv4("192.168.1.1", 1);
	testIPv4("300.168.1.1", -1);
	testIPv4("192.168.1", -1);
	testIPv4("192.168.a.1", -1);

	testIPv6("2001:0db8:85a3:0000:0000:8a2e:0370:7334", 1);
	testIPv6("2001:0db8:85a3:0:0:8a2e:0370:7334", 1);
	testIPv6("2001:0db8:85a3::8a2e:0370:7334", 1);
	testIPv6(":0db8:85a3:0000:0000:8a2e:0370:7334", 1);
	testIPv6("::1", 1);
	testIPv6("2001::7334", 1);
	testIPv6("::ffff:192.0.2.128", 1);

	testIPv6("2001:0db8:85a3:1234:0000:0000:8a2e:0370:7334", -1);
	testIPv6("2001:0db8:85a3:0:0:z:0370:7334", -1);
	testIPv6("0db8:85a3:0370:7334", -1);
	testIPv6(":0db8:85a3:0000:0000:1234:8a2e:0370:7334", -1);
	testIPv6("::", -1);
	testIPv6("::ffff:192.0.128", -1);

	printf ("\ntestmod_ipaddr RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
