#include <openssl/sha.h>

#include <crypto.hpp>

namespace kdbrest
{

namespace crypto
{

bool sha256_encrypt (unsigned char * input, unsigned long length, unsigned char * output)
{
	SHA256_CTX context;
	if (!SHA256_Init (&context))
	{
		return false;
	}

	if (!SHA256_Update (&context, input, length))
	{
		return false;
	}

	if (!SHA256_Final (output, &context))
	{
		return false;
	}

	return true;
}
}
}
