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
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not check ipv4 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "192.168.0.x",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == EAI_NONAME, "could not check ipv4 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "0.0.0.0",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == 0, "could not check ipv4 addr");
	keyDel (k);

	k = keyNew ("user/anything",
			KEY_VALUE, "localhost",
			KEY_META, "check/ipaddr", "",
			KEY_END);
	succeed_if (elektraNetworkAddrInfo(k) == EAI_NONAME, "could not check ipv4 addr");
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
