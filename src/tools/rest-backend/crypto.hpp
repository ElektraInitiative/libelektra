#ifndef ELEKTRA_REST_CRYPTO_HEADER_GUARD
#define ELEKTRA_REST_CRYPTO_HEADER_GUARD

#include <string>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

/**
 * @brief namespace for cryptographic functions
 */
namespace crypto
{

bool sha256_encrypt (const std::string & input, std::string & output);

} // namespace crypto

} // namespace kdbrest

#endif
