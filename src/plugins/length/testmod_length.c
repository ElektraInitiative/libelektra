/**
 * @file
 *
 * @brief Tests for lengthgth plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#define PLUGIN_NAME "lengthgth"
#include "test_length.h"


int main (int argc, char ** argv)
{
	printf ("length     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	print_result ("testmod_length");

	test_length();

	return nbError;
}
