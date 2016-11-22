/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NI_H
#define ELEKTRA_PLUGIN_NI_H

#include <kdbplugin.h>

#include <bohr/ni.h>


int elektraNiOpen (Plugin * handle, Key * errorKey);
int elektraNiClose (Plugin * handle, Key * errorKey);
int elektraNiGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNiSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNiError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (ni);

#endif
