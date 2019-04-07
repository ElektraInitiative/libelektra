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

int elektraMacaddrOpen (Plugin * handle, Key * errorKey);
int elektraMacaddrClose (Plugin * handle, Key * errorKey);
int elektraMacaddrGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMacaddrSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMacaddrError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMacaddrCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
