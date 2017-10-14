/**
 * @file
 *
 * @brief Tests for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <tests_plugin.h>

static void testIP (char const * const ip, const int ret, char const * const version)
{
	Key * parentKey = keyNew ("user/tests/ipaddr", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user/test/ipaddr/totest", KEY_VALUE, ip, KEY_META, "check/ipaddr", version, KEY_END), KS_END);
	PLUGIN_OPEN ("ipaddr");
	char message[200];
	(void)snprintf (message, 200, "validation of %s address “%s” failed", version[0] == '\0' ? "IP" : version, ip);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ret, message);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static inline void testIPv6 (char const * const ip, int ret)
{
	testIP (ip, ret, "ipv6");
}

static inline void testIPv4 (char const * const ip, int ret)
{
	testIP (ip, ret, "ipv4");
}

static inline void testIPAny (char const * const ip, int ret)
{
	testIP (ip, ret, "");
}

int main (int argc, char ** argv)
{
	printf ("IPADDR     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testIPv4 ("192.168.1.1", 1);
	testIPv4 ("300.168.1.1", -1);
	testIPv4 ("192.168.1", -1);
	testIPv4 ("192.168.a.1", -1);

	testIPv6 ("2001:0db8:85a3:0000:0000:8a2e:0370:7334", 1);
	testIPv6 ("2001:0db8:85a3:0:0:8a2e:0370:7334", 1);
	testIPv6 ("2001:0db8:85a3::8a2e:0370:7334", 1);
	testIPv6 (":0db8:85a3:0000:0000:8a2e:0370:7334", 1);
	testIPv6 ("::1", 1);
	testIPv6 ("2001::7334", 1);
	testIPv6 ("::ffff:192.0.2.128", 1);

	testIPv6 ("2001:0db8:85a3:1234:0000:0000:8a2e:0370:7334", -1);
	testIPv6 ("2001:0db8:85a3:0:0:z:0370:7334", -1);
	testIPv6 ("0db8:85a3:0370:7334", -1);
	testIPv6 (":0db8:85a3:0000:0000:1234:8a2e:0370:7334", -1);
	testIPv6 ("::", -1);
	testIPv6 ("::ffff:192.0.128", -1);

	testIPAny ("::ffff:192.0.128", -1);
	testIPAny ("1.2.3.", -1);
	testIPAny ("::1", 1);
	testIPAny ("42.42.42.42", 1);

	print_result ("testmod_ipaddr");

	return nbError;
}
