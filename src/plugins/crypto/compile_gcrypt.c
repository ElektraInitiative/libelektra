/**
 * @file
 *
 * @brief tests if compilation works (include and build paths set correct, etc...)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gcrypt.h>

gcry_cipher_hd_t nothing (void)
{
	gcry_cipher_hd_t elektraCryptoHandle = NULL;
	return elektraCryptoHandle;
}

int main (void)
{
	nothing ();
	return 0;
}
