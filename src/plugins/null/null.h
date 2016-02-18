/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NULL_H
#define ELEKTRA_PLUGIN_NULL_H

#include <kdbplugin.h>


int elektraNullOpen (Plugin * handle, Key * errorKey);
int elektraNullClose (Plugin * handle, Key * errorKey);
int elektraNullGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNullSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNullError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (null);

#endif
