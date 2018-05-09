/**
 * @file
 *
 * @brief tests if compilation works (include and build paths set correct, etc...)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <openssl/evp.h>

EVP_CIPHER_CTX * nothing (void)
{
	return NULL;
}

int main (void)
{
	nothing ();
	return 0;
}
