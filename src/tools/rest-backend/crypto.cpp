#include <openssl/sha.h>

#include <crypto.hpp>

namespace kdbrest
{

namespace crypto
{

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
