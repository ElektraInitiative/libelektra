/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_VALUE, "crypto plugin waits for your orders", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkconf", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (checkconf),
		KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/PAYLOAD_VERSION", KEY_VALUE, ELEKTRA_CRYPTO_PAYLOAD_VERSION,
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/masterpasswordlength", KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_MASTER_PWD_LENGTH), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/iterations", KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/saltlength", KEY_VALUE,
		ELEKTRA_STRINGIFY (ELEKTRA_CRYPTO_DEFAULT_SALT_LEN), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/algorithm", KEY_VALUE, "AES256", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/mode", KEY_VALUE, "CBC", KEY_END),
