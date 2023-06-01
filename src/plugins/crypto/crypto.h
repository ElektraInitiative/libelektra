/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_H
#define ELEKTRA_PLUGIN_CRYPTO_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>
#include <stdio.h>

/**
 * List of flags that are put into the crypto payload header.
 * Used to identify the data type of the encrypted content.
 */
enum ElektraCryptoHeaderFlags
{

	/** Regular binary data */
	ELEKTRA_CRYPTO_FLAG_NONE = 0,

	/** Null-terminated string value */
	ELEKTRA_CRYPTO_FLAG_STRING = 1,

	/** Null value */
	ELEKTRA_CRYPTO_FLAG_NULL = 2
};

/**
 * Defines the modes of operation of the crypto plugin.
 */
enum ElektraCryptoOperation
{

	/** Encryption mode */
	ELEKTRA_CRYPTO_ENCRYPT = 0,

	/** Decryption mode */
	ELEKTRA_CRYPTO_DECRYPT = 1
};

/*
 * A NOTE ABOUT THE CRYPTOGRAPHIC PAYLOAD
 *
 * Every compilation variant shall keep to the following format when writing encrypted payload to Keys.
 *
 * The following header is stored before the encrypted data.
 *
 * +----+-------------------------------------------+---------+--------+-----------------------+-----------+
 * |    | Element                                   |  Offset | Length | Data Type             | Encrypted |
 * +----+-------------------------------------------+---------+--------+-----------------------+-----------+
 * |    | magic number #!crypto                     |       0 |    8 B | char                  | NO        |
 * |    | crypto payload version                    |       8 |    2 B | char                  | NO        |
 * | Ls | length of the salt                        |      10 |    4 B | unsigned long integer | NO        |
 * | S  | salt                                      |      14 |   Ls B | byte                  | NO        |
 * | T  | original data type (string, binary, null) | 14 + Ls |    1 B | byte                  | YES       |
 * | Lc | original content length                   | 15 + Ls |    4 B | unsigned long integer | YES       |
 * +----+-------------------------------------------+---------+--------+-----------------------+-----------+
 *
 */
#define ELEKTRA_CRYPTO_PAYLOAD_VERSION "00"
#define ELEKTRA_CRYPTO_MAGIC_NUMBER "#!crypto" ELEKTRA_CRYPTO_PAYLOAD_VERSION
#define ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN (sizeof (ELEKTRA_CRYPTO_MAGIC_NUMBER) - 1)

// plugin defaults
#define ELEKTRA_CRYPTO_DEFAULT_MASTER_PWD_LENGTH (30)
#define ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT (15000)
#define ELEKTRA_CRYPTO_DEFAULT_SALT_LEN (17)

// plugin configuration parameters
#define ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD_LEN "/crypto/masterpasswordlength"
#define ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD "/crypto/masterpassword"
#define ELEKTRA_CRYPTO_PARAM_SHUTDOWN "/shutdown"
#define ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT "/crypto/iterations"

// metakeys
#define ELEKTRA_CRYPTO_META_ENCRYPT "crypto/encrypt"
#define ELEKTRA_CRYPTO_META_SALT "crypto/salt"

#endif
