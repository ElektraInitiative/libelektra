/**
 * @file
 *
 * @brief Header for macaddr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MACADDR_H
#define ELEKTRA_PLUGIN_MACADDR_H

#include <kdbplugin.h>

int elektraMacaddrGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMacaddrSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
