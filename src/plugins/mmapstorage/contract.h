/**
 * @file
 *
 * @brief contract for mmapstorage
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew (30,
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "", KEY_VALUE, "mmapstorage plugin waits for your orders", KEY_END),
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(open), KEY_END),
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(close), KEY_END),
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(get), KEY_END),
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
	       KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(set), KEY_END),
#include ELEKTRA_README
       keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
	       KEY_VALUE, PLUGINVERSION, KEY_END),
       KS_END);
