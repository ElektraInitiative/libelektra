/**
 * @file
 *
 * @brief Header for c plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_C_H
#define ELEKTRA_PLUGIN_C_H

#include <kdbplugin.h>


int elektraCOpen (Plugin * handle, Key * errorKey);
int elektraCClose (Plugin * handle, Key * errorKey);
int elektraCGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (c);

#endif
