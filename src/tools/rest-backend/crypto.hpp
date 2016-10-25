#ifndef ELEKTRA_REST_CRYPTO_HEADER_GUARD
#define ELEKTRA_REST_CRYPTO_HEADER_GUARD

namespace kdbrest
{

namespace crypto
{

bool sha256_encrypt (unsigned char * input, unsigned long length, unsigned char * output);

} // namespace crypto

} // namespace kdbrest

#endif
