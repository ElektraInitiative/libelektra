/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, ELEKTRA_KEY_VALUE, "base64 plugin waits for your orders", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", ELEKTRA_KEY_FUNC, PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", ELEKTRA_KEY_FUNC, PLUGIN_FUNCTION (set), ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/base64Encode", ELEKTRA_KEY_FUNC, base64Encode, ELEKTRA_KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/base64Decode", ELEKTRA_KEY_FUNC, base64Decode, ELEKTRA_KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
