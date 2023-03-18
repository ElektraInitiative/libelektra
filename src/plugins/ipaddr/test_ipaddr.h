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

#include <internal/kdb/config.h>

#ifdef HAVE_FEATURES_H

#include <features.h>
// The function `getaddrinfo` used in the `network` plugin leaks memory, if we use the default value for `ai_flags` on systems that use
// `glibc` 2.19.
// See also: https://travis-ci.org/ElektraInitiative/libelektra/builds/428298531
#if defined(__GLIBC__) && defined(__GLIBC_PREREQ)
#if !(__GLIBC_PREREQ(2, 20))
#include <string.h>
#define PLUGIN_LEAKS_MEMORY (strcmp (PLUGIN_NAME, "network") == 0)
#endif
#endif

#endif // HAVE_FEATURES_H

#ifndef PLUGIN_LEAKS_MEMORY
#define PLUGIN_LEAKS_MEMORY 0
#endif

static void testIP (char const * const ip, const int ret, char const * const version)
{
	Key * parentKey = keyNew ("user:/tests/ipaddr", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user:/test/ipaddr/totest", KEY_VALUE, ip, KEY_META, "check/ipaddr", version, KEY_END), KS_END);
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
		testIPAny ("should_not_exist", -1);
// The following test leaks memory on macOS 10.15
// See also: https://cirrus-ci.com/task/4740944293527552
#if !(defined(__APPLE__) && defined(ENABLE_ASAN))
		testIPAny ("www.google.com", 1);
#endif
		testIPv6 ("", -1);				   // empty string
		testIPv6 ("::1", 1);				   // loopback, compressed, non-routable
		testIPv6 ("::", 1);				   // unspecified, compressed, non-routable
		testIPv6 ("0:0:0:0:0:0:0:1", 1);		   // loopback, full
		testIPv6 ("0:0:0:0:0:0:0:0", 1);		   // unspecified, full
		testIPv6 ("2001:DB8:0:0:8:800:200C:417A", 1);	   // unicast, full
		testIPv6 ("FF01:0:0:0:0:0:0:101", 1);		   // multicast, full
		testIPv6 ("2001:DB8::8:800:200C:417A", 1);	   // unicast, compressed
		testIPv6 ("FF01::101", 1);			   // multicast, compressed
		testIPv6 ("2001:DB8:0:0:8:800:200C:417A:221", -1); // unicast, full
		testIPv6 ("FF01::101::2", -1);			   // multicast, compressed
		testIPv6 ("fe80::217:f2ff:fe07:ed62", 1);

		testIPv6 ("2001:0000:1234:0000:0000:C1C0:ABCD:0876", 1);
		testIPv6 ("3ffe:0b00:0000:0000:0001:0000:0000:000a", 1);
		testIPv6 ("FF02:0000:0000:0000:0000:0000:0000:0001", 1);
		testIPv6 ("0000:0000:0000:0000:0000:0000:0000:0001", 1);
		testIPv6 ("0000:0000:0000:0000:0000:0000:0000:0000", 1);

		// TODO: These two tests actually fail.
		// testIPv6("02001:0000:1234:0000:0000:C1C0:ABCD:0876", -1);// extra 0 not allowed!
		// testIPv6("2001:0000:1234:0000:00001:C1C0:ABCD:0876", -1);// extra 0 not allowed!


		// testIPv6(" 2001:0000:1234:0000:0000:C1C0:ABCD:0876", 1);// leading space
		// testIPv6("2001:0000:1234:0000:0000:C1C0:ABCD:0876", 1);// trailing space
		// testIPv6(" 2001:0000:1234:0000:0000:C1C0:ABCD:0876  ", 1);// leading and trailing space
		testIPv6 ("2001:0000:1234:0000:0000:C1C0:ABCD:0876  0", -1); // junk after valid address
		testIPv6 ("2001:0000:1234: 0000:0000:C1C0:ABCD:0876", -1);   // internal space

		testIPv6 ("3ffe:0b00:0000:0001:0000:0000:000a", -1);	       // seven segments
		testIPv6 ("FF02:0000:0000:0000:0000:0000:0000:0000:0001", -1); // nine segments
		testIPv6 ("3ffe:b00::1::a", -1);			       // double "::"
		testIPv6 ("::1111:2222:3333:4444:5555:6666::", -1);	       // double "::"
		testIPv6 ("2::10", 1);
		testIPv6 ("ff02::1", 1);
		testIPv6 ("fe80::", 1);
		testIPv6 ("2002::", 1);
		testIPv6 ("2001:db8::", 1);
		testIPv6 ("2001:0db8:1234::", 1);
		testIPv6 ("::ffff:0:0", 1);
		testIPv6 ("::1", 1);
		testIPv6 ("1:2:3:4:5:6:7:8", 1);
		testIPv6 ("1:2:3:4:5:6::8", 1);
		testIPv6 ("1:2:3:4:5::8", 1);
		testIPv6 ("1:2:3:4::8", 1);
		testIPv6 ("1:2:3::8", 1);
		testIPv6 ("1:2::8", 1);
		testIPv6 ("1::8", 1);
		testIPv6 ("1::2:3:4:5:6:7", 1);
		testIPv6 ("1::2:3:4:5:6", 1);
		testIPv6 ("1::2:3:4:5", 1);
		testIPv6 ("1::2:3:4", 1);
		testIPv6 ("1::2:3", 1);
		testIPv6 ("1::8", 1);
		testIPv6 ("::2:3:4:5:6:7:8", 1);
		testIPv6 ("::2:3:4:5:6:7", 1);
		testIPv6 ("::2:3:4:5:6", 1);
		testIPv6 ("::2:3:4:5", 1);
		testIPv6 ("::2:3:4", 1);
		testIPv6 ("::2:3", 1);
		testIPv6 ("::8", 1);
		testIPv6 ("1:2:3:4:5:6::", 1);
		testIPv6 ("1:2:3:4:5::", 1);
		testIPv6 ("1:2:3:4::", 1);
		testIPv6 ("1:2:3::", 1);
		testIPv6 ("1:2::", 1);
		testIPv6 ("1::", 1);
		testIPv6 ("1:2:3:4:5::7:8", 1);
		testIPv6 ("1:2:3::4:5::7:8", -1); // Double "::"
		testIPv6 ("12345::6:7:8", -1);
		testIPv6 ("1:2:3:4::7:8", 1);
		testIPv6 ("1:2:3::7:8", 1);
		testIPv6 ("1:2::7:8", 1);
		testIPv6 ("1::7:8", 1);

		// IPv4 addresses as dotted-quads
		testIPv6 ("1:2:3:4:5:6:1.2.3.4", 1);
		testIPv6 ("1:2:3:4:5::1.2.3.4", 1);
		testIPv6 ("1:2:3:4::1.2.3.4", 1);
		testIPv6 ("1:2:3::1.2.3.4", 1);
		testIPv6 ("1:2::1.2.3.4", 1);
		testIPv6 ("1::1.2.3.4", 1);
		testIPv6 ("1:2:3:4::5:1.2.3.4", 1);
		testIPv6 ("1:2:3::5:1.2.3.4", 1);
		testIPv6 ("1:2::5:1.2.3.4", 1);
		testIPv6 ("1::5:1.2.3.4", 1);
		testIPv6 ("1::5:11.22.33.44", 1);
		testIPv6 ("1::5:400.2.3.4", -1);
		testIPv6 ("1::5:260.2.3.4", -1);
		testIPv6 ("1::5:256.2.3.4", -1);
		testIPv6 ("1::5:1.256.3.4", -1);
		testIPv6 ("1::5:1.2.256.4", -1);
		testIPv6 ("1::5:1.2.3.256", -1);
		testIPv6 ("1::5:300.2.3.4", -1);
		testIPv6 ("1::5:1.300.3.4", -1);
		testIPv6 ("1::5:1.2.300.4", -1);
		testIPv6 ("1::5:1.2.3.300", -1);
		testIPv6 ("1::5:900.2.3.4", -1);
		testIPv6 ("1::5:1.900.3.4", -1);
		testIPv6 ("1::5:1.2.900.4", -1);
		testIPv6 ("1::5:1.2.3.900", -1);
		testIPv6 ("1::5:300.300.300.300", -1);
		testIPv6 ("1::5:3000.30.30.30", -1);
		testIPv6 ("1::400.2.3.4", -1);
		testIPv6 ("1::260.2.3.4", -1);
		testIPv6 ("1::256.2.3.4", -1);
		testIPv6 ("1::1.256.3.4", -1);
		testIPv6 ("1::1.2.256.4", -1);
		testIPv6 ("1::1.2.3.256", -1);
		testIPv6 ("1::300.2.3.4", -1);
		testIPv6 ("1::1.300.3.4", -1);
		testIPv6 ("1::1.2.300.4", -1);
		testIPv6 ("1::1.2.3.300", -1);
		testIPv6 ("1::900.2.3.4", -1);
		testIPv6 ("1::1.900.3.4", -1);
		testIPv6 ("1::1.2.900.4", -1);
		testIPv6 ("1::1.2.3.900", -1);
		testIPv6 ("1::300.300.300.300", -1);
		testIPv6 ("1::3000.30.30.30", -1);
		testIPv6 ("::400.2.3.4", -1);
		testIPv6 ("::260.2.3.4", -1);
		testIPv6 ("::256.2.3.4", -1);
		testIPv6 ("::1.256.3.4", -1);
		testIPv6 ("::1.2.256.4", -1);
		testIPv6 ("::1.2.3.256", -1);
		testIPv6 ("::300.2.3.4", -1);
		testIPv6 ("::1.300.3.4", -1);
		testIPv6 ("::1.2.300.4", -1);
		testIPv6 ("::1.2.3.300", -1);
		testIPv6 ("::900.2.3.4", -1);
		testIPv6 ("::1.900.3.4", -1);
		testIPv6 ("::1.2.900.4", -1);
		testIPv6 ("::1.2.3.900", -1);
		testIPv6 ("::300.300.300.300", -1);
		testIPv6 ("::3000.30.30.30", -1);
		testIPv6 ("fe80::217:f2ff:254.7.237.98", 1);
		testIPv6 ("::ffff:192.168.1.26", 1);
		testIPv6 ("2001:1:1:1:1:1:255Z255X255Y255", -1); // garbage instead of "." in IPv4
		testIPv6 ("::ffff:192x168.1.26", -1);		 // ditto
		testIPv6 ("::ffff:192.168.1.1", 1);
		testIPv6 ("0:0:0:0:0:0:13.1.68.3", 1);	      // IPv4-compatible IPv6 address, full, deprecated
		testIPv6 ("0:0:0:0:0:FFFF:129.144.52.38", 1); // IPv4-mapped IPv6 address, full
		testIPv6 ("::13.1.68.3", 1);		      // IPv4-compatible IPv6 address, compressed, deprecated
		testIPv6 ("::FFFF:129.144.52.38", 1);	      // IPv4-mapped IPv6 address, compressed
		testIPv6 ("fe80:0:0:0:204:61ff:254.157.241.86", 1);
		testIPv6 ("fe80::204:61ff:254.157.241.86", 1);
		testIPv6 ("::ffff:12.34.56.78", 1);
		testIPv6 ("::ffff:2.3.4", -1);
		testIPv6 ("::ffff:257.1.2.3", -1);
		testIPv6 ("1.2.3.4", -1);

		testIPv6 ("1.2.3.4:1111:2222:3333:4444::5555", -1); // Aeron
		testIPv6 ("1.2.3.4:1111:2222:3333::5555", -1);
		testIPv6 ("1.2.3.4:1111:2222::5555", -1);
		testIPv6 ("1.2.3.4:1111::5555", -1);
		testIPv6 ("1.2.3.4::5555", -1);
		testIPv6 ("1.2.3.4::", -1);

		// Testing IPv4 addresses represented as dotted-quads
		// Leading zero's in IPv4 addresses not allowed: some systems treat the leading "0" in ".086" as the start of an octal
		// number Update: The BNF in RFC-3986 explicitly defines the dec-octet (for IPv4 addresses) not to have a leading zero
		testIPv6 ("::ffff:192.0.2.128", 1); // but this is OK, since there's a single digit
		testIPv6 ("XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:1.2.3.4", -1);

		// TODO: These tests also fail on some systems
		// testIPv6("fe80:0000:0000:0000:0204:61ff:254.157.241.086", -1);
		// testIPv6("1111:2222:3333:4444:5555:6666:00.00.00.00", -1);
		// testIPv6("1111:2222:3333:4444:5555:6666:000.000.000.000", -1);

		testIPv6 ("1111:2222:3333:4444:5555:6666:256.256.256.256", -1);

		// Not testing address with subnet mask
		// testIPv6("2001:0DB8:0000:CD30:0000:0000:0000:0000/60", 1);// full, with prefix
		// testIPv6("2001:0DB8::CD30:0:0:0:0/60", 1);// compressed, with prefix
		// testIPv6("2001:0DB8:0:CD30::/60", 1);// compressed, with prefix //2
		// testIPv6("::/128", 1);// compressed, unspecified address type, non-routable
		// testIPv6("::1/128", 1);// compressed, loopback address type, non-routable
		// testIPv6("FF00::/8", 1);// compressed, multicast address type
		// testIPv6("FE80::/10", 1);// compressed, link-local unicast, non-routable
		// testIPv6("FEC0::/10", 1);// compressed, site-local unicast, deprecated
		// testIPv6("124.15.6.89/60", -1);// standard IPv4, prefix not allowed

		testIPv6 ("fe80:0000:0000:0000:0204:61ff:fe9d:f156", 1);
		testIPv6 ("fe80:0:0:0:204:61ff:fe9d:f156", 1);
		testIPv6 ("fe80::204:61ff:fe9d:f156", 1);
		testIPv6 ("::1", 1);
		testIPv6 ("fe80::", 1);
		testIPv6 ("fe80::1", 1);
		testIPv6 (":", -1);
		testIPv6 ("::ffff:c000:280", 1);

		// Aeron supplied these test cases
		testIPv6 ("1111:2222:3333:4444::5555:", -1);
		testIPv6 ("1111:2222:3333::5555:", -1);
		testIPv6 ("1111:2222::5555:", -1);
		testIPv6 ("1111::5555:", -1);
		testIPv6 ("::5555:", -1);
		testIPv6 (":::", -1);
		testIPv6 ("1111:", -1);
		testIPv6 (":", -1);

		testIPv6 (":1111:2222:3333:4444::5555", -1);
		testIPv6 (":1111:2222:3333::5555", -1);
		testIPv6 (":1111:2222::5555", -1);
		testIPv6 (":1111::5555", -1);
		testIPv6 (":::5555", -1);
		testIPv6 (":::", -1);


		// Additional test cases
		// from http://rt.cpan.org/Public/Bug/Display.html?id=50693
		testIPv6 ("2001:0db8:85a3:0000:0000:8a2e:0370:7334", 1);
		testIPv6 ("2001:db8:85a3:0:0:8a2e:370:7334", 1);
		testIPv6 ("2001:db8:85a3::8a2e:370:7334", 1);
		testIPv6 ("2001:0db8:0000:0000:0000:0000:1428:57ab", 1);
		testIPv6 ("2001:0db8:0000:0000:0000::1428:57ab", 1);
		testIPv6 ("2001:0db8:0:0:0:0:1428:57ab", 1);
		testIPv6 ("2001:0db8:0:0::1428:57ab", 1);
		testIPv6 ("2001:0db8::1428:57ab", 1);
		testIPv6 ("2001:db8::1428:57ab", 1);
		testIPv6 ("0000:0000:0000:0000:0000:0000:0000:0001", 1);
		testIPv6 ("::1", 1);
		testIPv6 ("::ffff:0c22:384e", 1);
		testIPv6 ("2001:0db8:1234:0000:0000:0000:0000:0000", 1);
		testIPv6 ("2001:0db8:1234:ffff:ffff:ffff:ffff:ffff", 1);
		testIPv6 ("2001:db8:a::123", 1);
		testIPv6 ("fe80::", 1);

		testIPv6 ("123", -1);
		testIPv6 ("ldkfj", -1);
		testIPv6 ("2001::FFD3::57ab", -1);
		testIPv6 ("2001:db8:85a3::8a2e:37023:7334", -1);
		testIPv6 ("2001:db8:85a3::8a2e:370k:7334", -1);
		testIPv6 ("1:2:3:4:5:6:7:8:9", -1);
		testIPv6 ("1::2::3", -1);
		testIPv6 ("1:::3:4:5", -1);
		testIPv6 ("1:2:3::4:5:6:7:8:9", -1);

		// New from Aeron
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:8888", 1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777::", 1);
		testIPv6 ("1111:2222:3333:4444:5555:6666::", 1);
		testIPv6 ("1111:2222:3333:4444:5555::", 1);
		testIPv6 ("1111:2222:3333:4444::", 1);
		testIPv6 ("1111:2222:3333::", 1);
		testIPv6 ("1111:2222::", 1);
		testIPv6 ("1111::", 1);
		// testIPv6("::", 1);     //duplicate
		testIPv6 ("1111:2222:3333:4444:5555:6666::8888", 1);
		testIPv6 ("1111:2222:3333:4444:5555::8888", 1);
		testIPv6 ("1111:2222:3333:4444::8888", 1);
		testIPv6 ("1111:2222:3333::8888", 1);
		testIPv6 ("1111:2222::8888", 1);
		testIPv6 ("1111::8888", 1);
		testIPv6 ("::8888", 1);
		testIPv6 ("1111:2222:3333:4444:5555::7777:8888", 1);
		testIPv6 ("1111:2222:3333:4444::7777:8888", 1);
		testIPv6 ("1111:2222:3333::7777:8888", 1);
		testIPv6 ("1111:2222::7777:8888", 1);
		testIPv6 ("1111::7777:8888", 1);
		testIPv6 ("::7777:8888", 1);
		testIPv6 ("1111:2222:3333:4444::6666:7777:8888", 1);
		testIPv6 ("1111:2222:3333::6666:7777:8888", 1);
		testIPv6 ("1111:2222::6666:7777:8888", 1);
		testIPv6 ("1111::6666:7777:8888", 1);
		testIPv6 ("::6666:7777:8888", 1);
		testIPv6 ("1111:2222:3333::5555:6666:7777:8888", 1);
		testIPv6 ("1111:2222::5555:6666:7777:8888", 1);
		testIPv6 ("1111::5555:6666:7777:8888", 1);
		testIPv6 ("::5555:6666:7777:8888", 1);
		testIPv6 ("1111:2222::4444:5555:6666:7777:8888", 1);
		testIPv6 ("1111::4444:5555:6666:7777:8888", 1);
		testIPv6 ("::4444:5555:6666:7777:8888", 1);
		testIPv6 ("1111::3333:4444:5555:6666:7777:8888", 1);
		testIPv6 ("::3333:4444:5555:6666:7777:8888", 1);
		testIPv6 ("::2222:3333:4444:5555:6666:7777:8888", 1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:123.123.123.123", 1);
		testIPv6 ("1111:2222:3333:4444:5555::123.123.123.123", 1);
		testIPv6 ("1111:2222:3333:4444::123.123.123.123", 1);
		testIPv6 ("1111:2222:3333::123.123.123.123", 1);
		testIPv6 ("1111:2222::123.123.123.123", 1);
		testIPv6 ("1111::123.123.123.123", 1);
		testIPv6 ("::123.123.123.123", 1);
		testIPv6 ("1111:2222:3333:4444::6666:123.123.123.123", 1);
		testIPv6 ("1111:2222:3333::6666:123.123.123.123", 1);
		testIPv6 ("1111:2222::6666:123.123.123.123", 1);
		testIPv6 ("1111::6666:123.123.123.123", 1);
		testIPv6 ("::6666:123.123.123.123", 1);
		testIPv6 ("1111:2222:3333::5555:6666:123.123.123.123", 1);
		testIPv6 ("1111:2222::5555:6666:123.123.123.123", 1);
		testIPv6 ("1111::5555:6666:123.123.123.123", 1);
		testIPv6 ("::5555:6666:123.123.123.123", 1);
		testIPv6 ("1111:2222::4444:5555:6666:123.123.123.123", 1);
		testIPv6 ("1111::4444:5555:6666:123.123.123.123", 1);
		testIPv6 ("::4444:5555:6666:123.123.123.123", 1);
		testIPv6 ("1111::3333:4444:5555:6666:123.123.123.123", 1);
		testIPv6 ("::2222:3333:4444:5555:6666:123.123.123.123", 1);

		// Playing with combinations of "0" and "::"
		// NB: these are all sytactically correct, but are bad form
		//   because "0" adjacent to "::" should be combined into "::"
		testIPv6 ("::0:0:0:0:0:0:0", 1);
		testIPv6 ("::0:0:0:0:0:0", 1);
		testIPv6 ("::0:0:0:0:0", 1);
		testIPv6 ("::0:0:0:0", 1);
		testIPv6 ("::0:0:0", 1);
		testIPv6 ("::0:0", 1);
		testIPv6 ("::0", 1);
		testIPv6 ("0:0:0:0:0:0:0::", 1);
		testIPv6 ("0:0:0:0:0:0::", 1);
		testIPv6 ("0:0:0:0:0::", 1);
		testIPv6 ("0:0:0:0::", 1);
		testIPv6 ("0:0:0::", 1);
		testIPv6 ("0:0::", 1);
		testIPv6 ("0::", 1);

		// New invalid from Aeron
		// Invalid data
		testIPv6 ("XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX", -1);

		// Too many components
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:8888:9999", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:8888::", -1);
		testIPv6 ("::2222:3333:4444:5555:6666:7777:8888:9999", -1);

		// Too few components
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666", -1);
		testIPv6 ("1111:2222:3333:4444:5555", -1);
		testIPv6 ("1111:2222:3333:4444", -1);
		testIPv6 ("1111:2222:3333", -1);
		testIPv6 ("1111:2222", -1);
		testIPv6 ("1111", -1);

		// Missing :
		testIPv6 ("11112222:3333:4444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:22223333:4444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:33334444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:44445555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:55556666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:5555:66667777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:77778888", -1);

		// Missing : intended for ::
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:8888:", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:", -1);
		testIPv6 ("1111:2222:3333:4444:5555:", -1);
		testIPv6 ("1111:2222:3333:4444:", -1);
		testIPv6 ("1111:2222:3333:", -1);
		testIPv6 ("1111:2222:", -1);
		testIPv6 ("1111:", -1);
		testIPv6 (":", -1);
		testIPv6 (":8888", -1);
		testIPv6 (":7777:8888", -1);
		testIPv6 (":6666:7777:8888", -1);
		testIPv6 (":5555:6666:7777:8888", -1);
		testIPv6 (":4444:5555:6666:7777:8888", -1);
		testIPv6 (":3333:4444:5555:6666:7777:8888", -1);
		testIPv6 (":2222:3333:4444:5555:6666:7777:8888", -1);
		testIPv6 (":1111:2222:3333:4444:5555:6666:7777:8888", -1);

		// :::
		testIPv6 (":::2222:3333:4444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:::3333:4444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:::4444:5555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:::5555:6666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:::6666:7777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:5555:::7777:8888", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:::8888", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:::", -1);

		// Double ::");
		testIPv6 ("::2222::4444:5555:6666:7777:8888", -1);
		testIPv6 ("::2222:3333::5555:6666:7777:8888", -1);
		testIPv6 ("::2222:3333:4444::6666:7777:8888", -1);
		testIPv6 ("::2222:3333:4444:5555::7777:8888", -1);
		testIPv6 ("::2222:3333:4444:5555:7777::8888", -1);
		testIPv6 ("::2222:3333:4444:5555:7777:8888::", -1);

		testIPv6 ("1111::3333::5555:6666:7777:8888", -1);
		testIPv6 ("1111::3333:4444::6666:7777:8888", -1);
		testIPv6 ("1111::3333:4444:5555::7777:8888", -1);
		testIPv6 ("1111::3333:4444:5555:6666::8888", -1);
		testIPv6 ("1111::3333:4444:5555:6666:7777::", -1);

		testIPv6 ("1111:2222::4444::6666:7777:8888", -1);
		testIPv6 ("1111:2222::4444:5555::7777:8888", -1);
		testIPv6 ("1111:2222::4444:5555:6666::8888", -1);
		testIPv6 ("1111:2222::4444:5555:6666:7777::", -1);

		testIPv6 ("1111:2222:3333::5555::7777:8888", -1);
		testIPv6 ("1111:2222:3333::5555:6666::8888", -1);
		testIPv6 ("1111:2222:3333::5555:6666:7777::", -1);

		testIPv6 ("1111:2222:3333:4444::6666::8888", -1);
		testIPv6 ("1111:2222:3333:4444::6666:7777::", -1);

		testIPv6 ("1111:2222:3333:4444:5555::7777::", -1);


		// Too many components"
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:8888:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666::1.2.3.4", -1);
		testIPv6 ("::2222:3333:4444:5555:6666:7777:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:1.2.3.4.5", -1);

		// Too few components
		testIPv6 ("1111:2222:3333:4444:5555:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:1.2.3.4", -1);
		testIPv6 ("1111:2222:1.2.3.4", -1);
		testIPv6 ("1111:1.2.3.4", -1);
		testIPv6 ("1.2.3.4", -1);

		// Missing :
		testIPv6 ("11112222:3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:22223333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:33334444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:44445555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:55556666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:5555:66661.2.3.4", -1);

		// Missing .
		testIPv6 ("1111:2222:3333:4444:5555:6666:255255.255.255", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:255.255255.255", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:255.255.255255", -1);

		// Missing : intended for ::
		testIPv6 (":1.2.3.4", -1);
		testIPv6 (":6666:1.2.3.4", -1);
		testIPv6 (":5555:6666:1.2.3.4", -1);
		testIPv6 (":4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":2222:3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":1111:2222:3333:4444:5555:6666:1.2.3.4", -1);

		// :::
		testIPv6 (":::2222:3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:::3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:::4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:::5555:6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:::6666:1.2.3.4", -1);
		testIPv6 ("1111:2222:3333:4444:5555:::1.2.3.4", -1);

		// Double ::
		testIPv6 ("::2222::4444:5555:6666:1.2.3.4", -1);
		testIPv6 ("::2222:3333::5555:6666:1.2.3.4", -1);
		testIPv6 ("::2222:3333:4444::6666:1.2.3.4", -1);
		testIPv6 ("::2222:3333:4444:5555::1.2.3.4", -1);

		testIPv6 ("1111::3333::5555:6666:1.2.3.4", -1);
		testIPv6 ("1111::3333:4444::6666:1.2.3.4", -1);
		testIPv6 ("1111::3333:4444:5555::1.2.3.4", -1);

		testIPv6 ("1111:2222::4444::6666:1.2.3.4", -1);
		testIPv6 ("1111:2222::4444:5555::1.2.3.4", -1);

		testIPv6 ("1111:2222:3333::5555::1.2.3.4", -1);

		// Missing parts
		testIPv6 ("::.", -1);
		testIPv6 ("::..", -1);
		testIPv6 ("::...", -1);
		testIPv6 ("::1...", -1);
		testIPv6 ("::1.2..", -1);
		testIPv6 ("::1.2.3.", -1);
		testIPv6 ("::.2..", -1);
		testIPv6 ("::.2.3.", -1);
		testIPv6 ("::.2.3.4", -1);
		testIPv6 ("::..3.", -1);
		testIPv6 ("::..3.4", -1);
		testIPv6 ("::...4", -1);

		// Extra : in front
		testIPv6 (":1111:2222:3333:4444:5555:6666:7777::", -1);
		testIPv6 (":1111:2222:3333:4444:5555:6666::", -1);
		testIPv6 (":1111:2222:3333:4444:5555::", -1);
		testIPv6 (":1111:2222:3333:4444::", -1);
		testIPv6 (":1111:2222:3333::", -1);
		testIPv6 (":1111:2222::", -1);
		testIPv6 (":1111::", -1);
		testIPv6 (":::", -1);
		testIPv6 (":1111:2222:3333:4444:5555:6666::8888", -1);
		testIPv6 (":1111:2222:3333:4444:5555::8888", -1);
		testIPv6 (":1111:2222:3333:4444::8888", -1);
		testIPv6 (":1111:2222:3333::8888", -1);
		testIPv6 (":1111:2222::8888", -1);
		testIPv6 (":1111::8888", -1);
		testIPv6 (":::8888", -1);
		testIPv6 (":1111:2222:3333:4444:5555::7777:8888", -1);
		testIPv6 (":1111:2222:3333:4444::7777:8888", -1);
		testIPv6 (":1111:2222:3333::7777:8888", -1);
		testIPv6 (":1111:2222::7777:8888", -1);
		testIPv6 (":1111::7777:8888", -1);
		testIPv6 (":::7777:8888", -1);
		testIPv6 (":1111:2222:3333:4444::6666:7777:8888", -1);
		testIPv6 (":1111:2222:3333::6666:7777:8888", -1);
		testIPv6 (":1111:2222::6666:7777:8888", -1);
		testIPv6 (":1111::6666:7777:8888", -1);
		testIPv6 (":::6666:7777:8888", -1);
		testIPv6 (":1111:2222:3333::5555:6666:7777:8888", -1);
		testIPv6 (":1111:2222::5555:6666:7777:8888", -1);
		testIPv6 (":1111::5555:6666:7777:8888", -1);
		testIPv6 (":::5555:6666:7777:8888", -1);
		testIPv6 (":1111:2222::4444:5555:6666:7777:8888", -1);
		testIPv6 (":1111::4444:5555:6666:7777:8888", -1);
		testIPv6 (":::4444:5555:6666:7777:8888", -1);
		testIPv6 (":1111::3333:4444:5555:6666:7777:8888", -1);
		testIPv6 (":::3333:4444:5555:6666:7777:8888", -1);
		testIPv6 (":::2222:3333:4444:5555:6666:7777:8888", -1);
		testIPv6 (":1111:2222:3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":1111:2222:3333:4444:5555::1.2.3.4", -1);
		testIPv6 (":1111:2222:3333:4444::1.2.3.4", -1);
		testIPv6 (":1111:2222:3333::1.2.3.4", -1);
		testIPv6 (":1111:2222::1.2.3.4", -1);
		testIPv6 (":1111::1.2.3.4", -1);
		testIPv6 (":::1.2.3.4", -1);
		testIPv6 (":1111:2222:3333:4444::6666:1.2.3.4", -1);
		testIPv6 (":1111:2222:3333::6666:1.2.3.4", -1);
		testIPv6 (":1111:2222::6666:1.2.3.4", -1);
		testIPv6 (":1111::6666:1.2.3.4", -1);
		testIPv6 (":::6666:1.2.3.4", -1);
		testIPv6 (":1111:2222:3333::5555:6666:1.2.3.4", -1);
		testIPv6 (":1111:2222::5555:6666:1.2.3.4", -1);
		testIPv6 (":1111::5555:6666:1.2.3.4", -1);
		testIPv6 (":::5555:6666:1.2.3.4", -1);
		testIPv6 (":1111:2222::4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":1111::4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":::4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":1111::3333:4444:5555:6666:1.2.3.4", -1);
		testIPv6 (":::2222:3333:4444:5555:6666:1.2.3.4", -1);

		// Extra : at end
		testIPv6 ("1111:2222:3333:4444:5555:6666:7777:::", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666:::", -1);
		testIPv6 ("1111:2222:3333:4444:5555:::", -1);
		testIPv6 ("1111:2222:3333:4444:::", -1);
		testIPv6 ("1111:2222:3333:::", -1);
		testIPv6 ("1111:2222:::", -1);
		testIPv6 ("1111:::", -1);
		testIPv6 (":::", -1);
		testIPv6 ("1111:2222:3333:4444:5555:6666::8888:", -1);
		testIPv6 ("1111:2222:3333:4444:5555::8888:", -1);
		testIPv6 ("1111:2222:3333:4444::8888:", -1);
		testIPv6 ("1111:2222:3333::8888:", -1);
		testIPv6 ("1111:2222::8888:", -1);
		testIPv6 ("1111::8888:", -1);
		testIPv6 ("::8888:", -1);
		testIPv6 ("1111:2222:3333:4444:5555::7777:8888:", -1);
		testIPv6 ("1111:2222:3333:4444::7777:8888:", -1);
		testIPv6 ("1111:2222:3333::7777:8888:", -1);
		testIPv6 ("1111:2222::7777:8888:", -1);
		testIPv6 ("1111::7777:8888:", -1);
		testIPv6 ("::7777:8888:", -1);
		testIPv6 ("1111:2222:3333:4444::6666:7777:8888:", -1);
		testIPv6 ("1111:2222:3333::6666:7777:8888:", -1);
		testIPv6 ("1111:2222::6666:7777:8888:", -1);
		testIPv6 ("1111::6666:7777:8888:", -1);
		testIPv6 ("::6666:7777:8888:", -1);
		testIPv6 ("1111:2222:3333::5555:6666:7777:8888:", -1);
		testIPv6 ("1111:2222::5555:6666:7777:8888:", -1);
		testIPv6 ("1111::5555:6666:7777:8888:", -1);
		testIPv6 ("::5555:6666:7777:8888:", -1);
		testIPv6 ("1111:2222::4444:5555:6666:7777:8888:", -1);
		testIPv6 ("1111::4444:5555:6666:7777:8888:", -1);
		testIPv6 ("::4444:5555:6666:7777:8888:", -1);
		testIPv6 ("1111::3333:4444:5555:6666:7777:8888:", -1);
		testIPv6 ("::3333:4444:5555:6666:7777:8888:", -1);
		testIPv6 ("::2222:3333:4444:5555:6666:7777:8888:", -1);

		// Additional cases: http://crisp.tweakblogs.net/blog/2031/ipv6-validation-%28and-caveats%29.html
		testIPv6 ("0:a:b:c:d:e:f::", 1);
		testIPv6 ("::0:a:b:c:d:e:f", 1); // syntactically correct, but bad form (::0:... could be combined)
		testIPv6 ("a:b:c:d:e:f:0::", 1);
		testIPv6 ("':10.0.0.1", -1);
	}
	else
	{
		// Tested with
		//      - http://formvalidation.io/validators/ip/
		//      - http://www.csgnetwork.com/directipverify.html?IPvalue=192.168.1
		testIPv4 ("192.168.1", -1); // Invalid
	}

	if (!PLUGIN_LEAKS_MEMORY)
	{
		testIPAny ("::1", 1);
		testIPAny ("192.168.0.1", 1);
		testIPAny ("42.42.42.42", 1);

		testIPAny ("::ffff:192.0.128", -1);
		testIPAny ("x", -1);
	}
}
