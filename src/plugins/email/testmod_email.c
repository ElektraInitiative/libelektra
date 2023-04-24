/**
 * @file
 *
 * @brief Tests for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#define PLUGIN_NAME "email"
#include "./test_email.h"

int main (int argc, char ** argv)
{
	printf ("EMAIL        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testEmailAll ();

	print_result ("testmod_email");

	return nbError;
}
