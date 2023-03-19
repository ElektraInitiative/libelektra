/**
 * @file
 *
 * @brief Header for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_VERSION_H
#define ELEKTRA_PLUGIN_VERSION_H

#include <elektra/plugin/plugin.h>

static inline KeySet * elektraVersionKeySet (void)
{
	return ksNew (
		50,
		keyNew ("system:/elektra/version", KEY_VALUE,
			"Below are version information of the Elektra Library you are currently using", KEY_END),
		keyNew ("system:/elektra/version/constants", KEY_END),
		keyNew ("system:/elektra/version/constants/KDB_VERSION", KEY_VALUE, KDB_VERSION, KEY_END),
		keyNew ("system:/elektra/version/constants/KDB_VERSION_MAJOR", KEY_VALUE, ELEKTRA_STRINGIFY (KDB_VERSION_MAJOR), KEY_END),
		keyNew ("system:/elektra/version/constants/KDB_VERSION_MINOR", KEY_VALUE, ELEKTRA_STRINGIFY (KDB_VERSION_MINOR), KEY_END),
		keyNew ("system:/elektra/version/constants/KDB_VERSION_PATCH", KEY_VALUE, ELEKTRA_STRINGIFY (KDB_VERSION_PATCH), KEY_END),
		keyNew ("system:/elektra/version/constants/SO_VERSION", KEY_VALUE, ELEKTRA_STRINGIFY (SO_VERSION), KEY_END),
		keyNew ("system:/elektra/version/infos", KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system:/elektra/version/infos/author", KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system:/elektra/version/infos/licence", KEY_VALUE, "BSD", KEY_END),
		keyNew ("system:/elektra/version/infos/description", KEY_VALUE, "Information of your Elektra Installation", KEY_END),
		keyNew ("system:/elektra/version/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, KeySet * definition, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
