/**
 * @file
 *
 * @brief Header for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MINI_H
#define ELEKTRA_PLUGIN_MINI_H

#include <kdbplugin.h>


int elektraMiniOpen (Plugin * handle, Key * errorKey);
int elektraMiniClose (Plugin * handle, Key * errorKey);
int elektraMiniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMiniSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMiniError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMiniCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (mini);

#endif
