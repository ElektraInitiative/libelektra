/**
 * @file
 *
 * @brief contract for mmapstorage
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew (30,
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "", KEY_VALUE, "mmapstorage plugin waits for your orders", KEY_END),
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(mmapstorage, open), KEY_END),
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(mmapstorage, close), KEY_END),
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(mmapstorage, get), KEY_END),
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(mmapstorage, set), KEY_END),
#include ELEKTRA_README (mmapstorage)
       keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
	       KEY_VALUE, PLUGINVERSION, KEY_END),
       KS_END);
