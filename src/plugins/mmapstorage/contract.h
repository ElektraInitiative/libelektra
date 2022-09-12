/**
 * @file
 *
 * @brief contract for mmapstorage
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew (30,
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "", ELEKTRA_KEY_VALUE, "mmapstorage plugin waits for your orders", ELEKTRA_KEY_END),
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
	       ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(open), ELEKTRA_KEY_END),
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
	       ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(close), ELEKTRA_KEY_END),
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
	       ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(get), ELEKTRA_KEY_END),
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
	       ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(set), ELEKTRA_KEY_END),
#include ELEKTRA_README
       elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
	       ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
       ELEKTRA_KS_END);
