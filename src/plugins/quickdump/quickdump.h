/**
 * @file
 *
 * @brief Header for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_QUICKDUMP_H
#define ELEKTRA_PLUGIN_QUICKDUMP_H

#include <kdbplugin.h>


int elektraQuickdumpGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraQuickdumpSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
