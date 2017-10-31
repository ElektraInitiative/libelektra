/**
 * @file
 *
 * @brief test suite for the crypto plugin (OpenSSL compile variant)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "test_internals.h"

#define PLUGIN_NAME "crypto_openssl"

int main (int argc, char ** argv)
{
	printf ("CYPTO        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	TEST_SUITE (PLUGIN_NAME);

	print_result (PLUGIN_NAME);
	return nbError;
}
