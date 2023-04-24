/**
 * @file
 *
 * @brief Tests for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#define PLUGIN_NAME "length"
#include "./test_length.h"


int main (int argc, char ** argv)
{
	printf ("length     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_length ();

	print_result ("testmod_length");

	return nbError;
}
