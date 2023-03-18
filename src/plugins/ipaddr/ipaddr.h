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

#include <elektra/plugin/plugin.h>

int elektraIpaddrGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIpaddrSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
