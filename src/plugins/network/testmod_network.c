/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#include "network.h"

#include <tests.h>

#define PLUGIN_NAME "network"
#include "../ipaddr/test_ipaddr.h"

int main (int argc, char ** argv)
{
	printf ("NETWORK   TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	testIPAll ();

	print_result ("testmod_network");

	return nbError;
}
