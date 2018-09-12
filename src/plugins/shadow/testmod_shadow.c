/**
 * @file
 *
 * @brief Tests for shadow plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

int main (int argc, char ** argv)
{
	printf ("SHADOW     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	print_result ("testmod_shadow");

	return nbError;
}
