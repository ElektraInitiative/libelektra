/**
 * @file
 *
 * @brief header for cryptographic helper functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_CRYPTO_HPP
#define ELEKTRA_REST_CRYPTO_HPP

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
