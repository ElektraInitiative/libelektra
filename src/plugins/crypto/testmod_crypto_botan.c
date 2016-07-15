/**
 * @file
 *
 * @brief test suite for the crypto plugin (Botan compile variant)
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "test_internals.h"

#define PLUGIN_NAME "crypto_botan"

int main (int argc, char ** argv)
{
	printf ("CYPTO        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_init (PLUGIN_NAME);
	test_config_errors (PLUGIN_NAME);
	test_crypto_operations (PLUGIN_NAME);

	printf ("\n" PLUGIN_NAME " RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}
