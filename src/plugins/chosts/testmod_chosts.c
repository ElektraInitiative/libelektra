/**
 * @file
 *
 * @brief Tests for chosts plugin
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
	printf ("CHOSTS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	print_result ("testmod_chosts");

	return nbError;
}
