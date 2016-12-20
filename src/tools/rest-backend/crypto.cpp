/**
 * @file
 *
 * @brief cryptographic helper functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <iomanip>
#include <sstream>

#include <openssl/sha.h>

#include <crypto.hpp>

namespace kdbrest
{

namespace crypto
{

/**
 * @brief encrypts a string with sha256 encryption
 * 
 * openssl based encryption function that utilizes the sha256
 * encryption algorithm to encrypt a string. useful for password
 * encryption.
 * 
 * @param input the unencrypted input string
 * @param output the target string for the encrypted version
 * @return bool whether the encryption succeeded or not
 */
bool sha256_encrypt (const std::string & input, std::string & output)
{
	unsigned char sha_out[SHA256_DIGEST_LENGTH];
	SHA256_CTX context;
	if (!SHA256_Init (&context))
	{
		return false;
	}

	if (!SHA256_Update (&context, input.c_str (), input.length ()))
	{
		return false;
	}

	if (!SHA256_Final (sha_out, &context))
	{
		return false;
	}

	std::stringstream ss;
	for (int i = 0; i < SHA256_DIGEST_LENGTH; i++)
	{
		ss << std::hex << std::setw (2) << std::setfill ('0') << static_cast<int> (sha_out[i]);
	}
	output = ss.str ();

	return true;
}
}
}
