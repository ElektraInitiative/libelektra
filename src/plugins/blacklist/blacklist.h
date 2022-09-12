/**
 * @file
 *
 * @brief Header for blacklist plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BLACKLIST_H
#define ELEKTRA_PLUGIN_BLACKLIST_H

#include <kdbplugin.h>


int elektraBlacklistGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraBlacklistSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
