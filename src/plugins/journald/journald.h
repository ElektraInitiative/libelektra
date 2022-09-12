/**
 * @file
 *
 * @brief A plugin which logs write operations and errors via the native journald interface
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_JOURNALD_H
#define ELEKTRA_PLUGIN_JOURNALD_H

#include <kdbplugin.h>

int elektraJournaldGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraJournaldSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraJournaldError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
