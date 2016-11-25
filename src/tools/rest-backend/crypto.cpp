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

	if (!SHA256_Update (&context, input.c_str (), input.size ()))
	{
		return false;
	}

	if (!SHA256_Final (sha_out, &context))
	{
		return false;
	}

	output = std::string (reinterpret_cast<const char *> (sha_out));

	return true;
}
}
}
