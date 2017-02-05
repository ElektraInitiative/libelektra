/**
 * @file
 *
 * @brief Header for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_IPADDR_H
#define ELEKTRA_PLUGIN_IPADDR_H

#include <kdbplugin.h>


int elektraIpaddrOpen (Plugin * handle, Key * errorKey);
int elektraIpaddrClose (Plugin * handle, Key * errorKey);
int elektraIpaddrGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIpaddrSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIpaddrError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIpaddrCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (ipaddr);

#endif
