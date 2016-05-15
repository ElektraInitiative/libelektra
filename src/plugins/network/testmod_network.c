/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#include "network.h"

#include <tests.h>


void test_addr ()
{
	// clang-format off
	Key * k = keyNew ("user/anything",
			KEY_VALUE, "192.168.0.1",
			KEY_META, "check/ipaddr", "ipv4",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not check ipv4 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "192.168.0.x",
			KEY_META, "check/ipaddr", "ipv4",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) != 0, "wrong ipv4 addr '192.168.0.x' was successful");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "0.0.0.0",
			KEY_META, "check/ipaddr", "ipv4",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not check ipv4 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "localhost",
			KEY_META, "check/ipaddr", "ipv4",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) != 0, "wrong ipv4 addr 'localhost' was successful");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, ":::",
			KEY_META, "check/ipaddr", "ipv6",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) != 0, "wrong ipv6 addr ':::' was successful");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "::1",
			KEY_META, "check/ipaddr", "ipv6",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not check ipv6 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "localhost",
			KEY_META, "check/ipaddr", "ipv6",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) != 0, "wrong ipv6 addr 'localhost' was successful");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "192.168.0.1",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not do addr check (without specifying ipv4/ipv6)");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "::1",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not do addr check (without specifying ipv4/ipv6)");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "x",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) != 0, "addr check (without specifying ipv4/ipv6) did not fail with 'x'");
	keyDel (k);
	// clang-format on
}


int main (int argc, char ** argv)
{
	printf ("   ICONV   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_addr ();

	printf ("\ntest_backendhelpers RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
