#ifndef ELEKTRA_REST_CRYPTO_HEADER_GUARD
#define ELEKTRA_REST_CRYPTO_HEADER_GUARD

#include <string>

namespace kdbrest
{

namespace crypto
{

bool sha256_encrypt (const std::string & input, std::string & output);

} // namespace crypto

} // namespace kdbrest

#endif
