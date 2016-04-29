/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_HIDDEN_H
#define ELEKTRA_PLUGIN_HIDDEN_H

#include <kdbplugin.h>


int elektraHiddenOpen (Plugin * handle, Key * errorKey);
int elektraHiddenClose (Plugin * handle, Key * errorKey);
int elektraHiddenGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHiddenSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (hidden);

#endif
