/**
 * @file
 *
 * @brief test suite for the gpgme plugin.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 * @author Peter Nirschl
 *
 */

#include <gpgme.h>
#include <kdbinternal.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>
#include <unistd.h>

#include "gpgme.h"

// GPG private key for importing and testing
#include "test_key.h"

#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"

static void init_gpgme (void)
{
	gpgme_error_t err;

	gpgme_check_version (NULL);
	setlocale (LC_ALL, "");
	gpgme_set_locale (NULL, LC_CTYPE, setlocale (LC_CTYPE, NULL));
#ifndef HAVE_W32_SYSTEM
	gpgme_set_locale (NULL, LC_MESSAGES, setlocale (LC_MESSAGES, NULL));
#endif

	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	succeed_if (!err, "failed to initialize gpgme");
}

static void test_install_key (void)
{
	gpgme_error_t err;
	gpgme_ctx_t ctx;
	gpgme_genkey_result_t result;

	// generate the GPG key
	err = gpgme_new (&ctx);
	succeed_if (!err, "failed to initialize gpgme handle");

	const char * keyParams =
		"<GnupgKeyParms format=\"internal\">\n"
		"Key-Type: RSA\n"
		"Key-Length: 1024\n"
		"Subkey-Type: RSA\n"
		"Subkey-Length: 1024\n"
		"Name-Real: Elektra Test Key\n"
		"Name-Comment: DO NOT USE THIS KEY IN PRODUCTION\n"
		"Name-Email: test@libelektra.org\n"
		"Expire-Date: 0\n"
		"Passphrase: 1234\n"
		"</GnupgKeyParms>\n";

	err = gpgme_op_genkey (ctx, keyParams, NULL, NULL);
	succeed_if (!err, "failed to generate the GPG test key");

	result = gpgme_op_genkey_result (ctx);
	succeed_if (result, "failed to retrieve the result of the key generation");

	gpgme_release (ctx);
}

int main (int argc, char ** argv)
{
	printf ("GPGME        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	init_gpgme ();
	for (int i = 0; i < 10; i++)
	{
		// to see how the build server responds
		test_install_key ();
	}

	print_result ("gpgme");
	return nbError;
}
