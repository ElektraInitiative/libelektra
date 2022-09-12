/**
 * @file
 *
 * @brief Header for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_IPADDR_H
#define ELEKTRA_PLUGIN_IPADDR_H

#include <kdbplugin.h>

int elektraIpaddrGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraIpaddrSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
