/**
 * @file
 *
 * @brief Tests for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#define PLUGIN_NAME "ipaddr"
#include "./test_ipaddr.h"

int main (int argc, char ** argv)
{
	printf ("IPADDR     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testIPAll ();

	print_result ("testmod_ipaddr");

	return nbError;
}
