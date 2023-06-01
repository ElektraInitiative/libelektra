/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#endif

#include <stdio.h>

#include "./network.h"

#include <tests.h>

#define PLUGIN_NAME "network"

static void testPorts (void);

#include "../ipaddr/test_ipaddr.h"

int main (int argc, char ** argv)
{
	printf ("NETWORK   TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	testIPAll ();
	testPorts ();

	print_result ("testmod_network");

	return nbError;
}

static void testPort (char const * const port, const int ret, char const * const version, char const * const metaName)
{
	Key * parentKey = keyNew ("user:/tests/port", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user:/test/port/totest", KEY_VALUE, port, KEY_META, metaName, version, KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	const int pluginStatus = plugin->kdbSet (plugin, ks, parentKey);
	char message[200];
	(void) snprintf (message, 200, "validation of %s “%s” returned %d instead of %d", version[0] == '\0' ? "Port" : version, port,
			 pluginStatus, ret);
	succeed_if (pluginStatus == ret, message);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static inline void testPortAny (char const * const port, int ret)
{
	testPort (port, ret, "", "check/port");
}

static void testPorts (void)
{
	testPortAny ("0", 1);
	testPortAny ("1234", 1);
	testPortAny ("65535", 1);
	testPortAny ("ssh", 1);
	testPortAny ("https", 1);

	testPortAny ("65536", -1);
	testPortAny ("-1", -1);
	testPortAny ("22d", -1);
	testPortAny ("myInvalidServiceName", -1);

	// Tests for ListenPort are not portable, even system ports in a range from 1-1000 can some short time be reachable
	// https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
}
