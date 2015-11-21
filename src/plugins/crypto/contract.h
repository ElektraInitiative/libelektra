/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

keyNew ("system/elektra/modules/crypto", KEY_VALUE, "crypto plugin waits for your orders", KEY_END),
keyNew ("system/elektra/modules/crypto/exports", KEY_END),
keyNew ("system/elektra/modules/crypto/exports/get", KEY_FUNC, elektraCryptoGet, KEY_END),
keyNew ("system/elektra/modules/crypto/exports/set", KEY_FUNC, elektraCryptoSet, KEY_END),
keyNew ("system/elektra/modules/crypto/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
keyNew ("system/elektra/modules/crypto/infos", KEY_VALUE, "Information about crypto plugin is paced in the keys below", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/author", KEY_VALUE, "Peter Nirschl <peter.nirschl@gmail.com>", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/licence", KEY_VALUE, "BSD", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/needs", KEY_VALUE, "", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/provides", KEY_VALUE, "filefilter", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/placements", KEY_VALUE, "postgetstorage presetstorage", KEY_END),
keyNew ("system/elektra/modules/crypto/infos/description", KEY_VALUE, "filter plugin for cryptographic operations", KEY_END),
#include ELEKTRA_README(crypto)
