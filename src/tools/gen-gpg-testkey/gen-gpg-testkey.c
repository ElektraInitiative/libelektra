/**
 * @file
 *
 * @brief tool for creating and finding the GPG test key to be used in Elektra's unit tests.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gpgme.h>
#include <stdio.h>

#define ELEKTRA_GEN_GPG_TESTKEY_DESCRIPTION "elektra testkey (gen-gpg-testkey)"

int main (void)
{
	gpgme_error_t err;
	gpgme_ctx_t ctx;
	gpgme_key_t key;
	gpgme_genkey_result_t res;

	gpgme_check_version (NULL);
	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	if (err)
	{
		fprintf (stderr, "gpgme version check failed: %s\n", gpgme_strerror (err));
		return -1;
	}

	// setup gpgme context
	err = gpgme_new (&ctx);
	if (err)
	{
		fprintf (stderr, "gpgme error: %s\n", gpgme_strerror (err));
		return -1;
	}

	// look for the elektra key
	err = gpgme_op_keylist_start (ctx, ELEKTRA_GEN_GPG_TESTKEY_DESCRIPTION, 1 /* secret keys only! */);
	if (err)
	{
		fprintf (stderr, "gpgme error: %s\n", gpgme_strerror (err));
		goto cleanup;
	}

	err = gpgme_op_keylist_next (ctx, &key);
	if (err && gpg_err_code (err) != GPG_ERR_EOF)
	{
		fprintf (stderr, "gpgme error: %s\n", gpgme_strerror (err));
		goto cleanup;
	}

	if (err && gpg_err_code (err) == GPG_ERR_EOF)
	{
		// create the elektra demo key
		// NOTE new versions of libgpgme provide GPGME_CREATE_NOEXPIRE to create keys that do not expire
		//      and the parameter 0 means "reasonable expiration date" (for whatever that is).
		//      However, in older verions (that do not have GPGME_CREATE_NOEXPIRE) an expiration of 0 means
		//      that the key does not expire.
		//
		//      See https://lists.gnupg.org/pipermail/gnupg-commits/2017-February/013351.html
#ifdef GPGME_CREATE_NOEXPIRE
		err = gpgme_op_createkey (ctx, ELEKTRA_GEN_GPG_TESTKEY_DESCRIPTION, "default", 0, 0, NULL,
					  GPGME_CREATE_SIGN | GPGME_CREATE_ENCR | GPGME_CREATE_NOEXPIRE | GPGME_CREATE_NOPASSWD);
#else
		err = gpgme_op_createkey (ctx, ELEKTRA_GEN_GPG_TESTKEY_DESCRIPTION, "default", 0, 0, NULL,
					  GPGME_CREATE_SIGN | GPGME_CREATE_ENCR | GPGME_CREATE_NOPASSWD);
#endif
		res = gpgme_op_genkey_result (ctx);
		fprintf (stdout, "%s", res->fpr);
	}
	else
	{
		// display the key ID
		fprintf (stdout, "%s", key->subkeys->fpr);
		gpgme_key_release (key);
		gpgme_op_keylist_end (ctx);
	}

	gpgme_release (ctx);
	return 0;

cleanup:
	gpgme_release (ctx);
	return -1;
}
