/**
 * @file
 *
 * @brief Tests for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <tests_plugin.h>

static void testIP (char const * const ip, const int ret, char const * const version)
{
	Key * parentKey = keyNew ("user/tests/ipaddr", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user/test/ipaddr/totest", KEY_VALUE, ip, KEY_META, "check/ipaddr", version, KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	const int pluginStatus = plugin->kdbSet (plugin, ks, parentKey);
	char message[200];
	(void) snprintf (message, 200, "validation of %s address “%s” returned %d instead of %d", version[0] == '\0' ? "IP" : version, ip,
			 pluginStatus, ret);
	succeed_if (pluginStatus == ret, message);
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

static void testIPAll (void)
{
	testIPv4 ("192.168.0.1", 1);
	testIPv4 ("192.168.1.1", 1);
	testIPv4 ("0.0.0.0", 1);

	testIPv4 ("192.168.0.x", -1);
	testIPv4 ("300.168.1.1", -1);
	testIPv4 ("192.168.a.1", -1);
	testIPv4 ("localhost", -1);

	testIPv6 ("2001:0db8:85a3:0000:0000:8a2e:0370:7334", 1);
	testIPv6 ("2001:0db8:85a3:0:0:8a2e:0370:7334", 1);
	testIPv6 ("2001:0db8:85a3::8a2e:0370:7334", 1);
	testIPv6 ("::1", 1);
	testIPv6 ("2001::7334", 1);
	testIPv6 ("::ffff:192.0.2.128", 1);

	testIPv6 (":::", -1);
	testIPv6 ("2001:0db8:85a3:1234:0000:0000:8a2e:0370:7334", -1);
	testIPv6 ("2001:0db8:85a3:0:0:z:0370:7334", -1);
	testIPv6 ("0db8:85a3:0370:7334", -1);
	testIPv6 (":0db8:85a3:0000:0000:1234:8a2e:0370:7334", -1);
	testIPv6 ("::ffff:192.0.128", -1);
	testIPv6 ("localhost", -1);

	if (strncmp (PLUGIN_NAME, "network", sizeof ("ipaddr") - 1) == 0)
	{
		// Tested with
		//      - http://formvalidation.io/validators/ip/
		//      - https://www.helpsystems.com/intermapper/ipv6-test-address-validation
		testIPv6 (":0db8:85a3:0000:0000:8a2e:0370:7334", -1); // Invalid
		testIPv6 ("::", 1);				      // Valid
		testIPAny("localhost", 1);
		testIPAny("should_not_exist", -1);
		testIPAny("www.google.com", 1);
	}
	else
	{
		// Tested with
		//      - http://formvalidation.io/validators/ip/
		//      - http://www.csgnetwork.com/directipverify.html?IPvalue=192.168.1
		testIPv4 ("192.168.1", -1); // Invalid
	}

	testIPAny ("::1", 1);
	testIPAny ("192.168.0.1", 1);
	testIPAny ("42.42.42.42", 1);

	testIPAny ("::ffff:192.0.128", -1);
	testIPAny ("1.2.3.", -1);
	testIPAny ("x", -1);
}
