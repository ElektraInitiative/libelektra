/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_VALUE, "crypto plugin waits for your orders", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open", KEY_FUNC, CRYPTO_PLUGIN_FUNCTION(open), KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close", KEY_FUNC, CRYPTO_PLUGIN_FUNCTION(close), KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", KEY_FUNC, CRYPTO_PLUGIN_FUNCTION(get), KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", KEY_FUNC, CRYPTO_PLUGIN_FUNCTION(set), KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error", KEY_FUNC, CRYPTO_PLUGIN_FUNCTION(error), KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos", KEY_VALUE, "Information about crypto plugin is paced in the keys below", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/author", KEY_VALUE, "Peter Nirschl <peter.nirschl@gmail.com>", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/licence", KEY_VALUE, "BSD", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/needs", KEY_VALUE, "", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/provides", KEY_VALUE, "filefilter", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/placements", KEY_VALUE, "postgetstorage presetstorage", KEY_END),
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/description", KEY_VALUE, "filter plugin for cryptographic operations", KEY_END),
#include ELEKTRA_README(crypto)
