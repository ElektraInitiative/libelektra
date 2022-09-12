/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, ELEKTRA_KEY_VALUE, "crypto plugin waits for your orders", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkconf", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (checkconf),
		ELEKTRA_KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/PAYLOAD_VERSION", ELEKTRA_KEY_VALUE, ELEKTRA_CRYPTO_PAYLOAD_VERSION,
		ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/masterpasswordlength", ELEKTRA_KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_MASTER_PWD_LENGTH), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/iterations", ELEKTRA_KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/saltlength", ELEKTRA_KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_SALT_LEN), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/algorithm", ELEKTRA_KEY_VALUE, "AES256", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/mode", ELEKTRA_KEY_VALUE, "CBC", ELEKTRA_KEY_END),
