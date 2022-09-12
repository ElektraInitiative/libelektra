/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, ELEKTRA_KEY_VALUE, "fcrypt plugin waits for your orders", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkconf", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (checkconf),
		ELEKTRA_KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/DEFAULT_TMPDIR", ELEKTRA_KEY_VALUE, ELEKTRA_FCRYPT_DEFAULT_TMPDIR,
		ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/config/needs" ELEKTRA_FCRYPT_CONFIG_TMPDIR, ELEKTRA_KEY_VALUE,
		ELEKTRA_FCRYPT_DEFAULT_TMPDIR, ELEKTRA_KEY_END),
