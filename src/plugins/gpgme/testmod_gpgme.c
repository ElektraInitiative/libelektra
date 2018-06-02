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
		"Key-Length: 512\n"
		"Subkey-Type: RSA\n"
		"Subkey-Length: 512\n"
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

static void test_import_key (void)
{
	gpgme_error_t err;
	gpgme_data_t keydata;
	gpgme_import_result_t result;
	gpgme_ctx_t ctx;

	err = gpgme_new (&ctx);
	succeed_if (!err, "failed to initialize gpgme handle");

	err = gpgme_data_new_from_mem (&keydata, test_key_asc, test_key_asc_len, 1);
	succeed_if (!err, "failed to transform the test key to gpgme data structure");

	err = gpgme_op_import (ctx, keydata);
	succeed_if (!err, "failed to import GPG test key");

	if (!err)
	{
		result = gpgme_op_import_result (ctx);
		succeed_if (result->imported + result->unchanged == 1, "the GPG import failed");
	}

	gpgme_release (ctx);
}

int main (int argc, char ** argv)
{
	printf ("GPGME        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	init_gpgme ();
	test_install_key ();
	for (int i = 0; i < 20; i++)
	{
		test_import_key ();
	}

	print_result ("gpgme");
	return nbError;
}
