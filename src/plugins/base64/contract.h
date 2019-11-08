/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_VALUE, "base64 plugin waits for your orders", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", KEY_FUNC, PLUGIN_FUNCTION (get), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", KEY_FUNC, PLUGIN_FUNCTION (set), KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/base64Encode", KEY_FUNC, base64Encode, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/base64Decode", KEY_FUNC, base64Decode, KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
