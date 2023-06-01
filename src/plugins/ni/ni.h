/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NI_H
#define ELEKTRA_PLUGIN_NI_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

#include <bohr/ni.h>


int elektraNiOpen (Plugin * handle, Key * errorKey);
int elektraNiClose (Plugin * handle, Key * errorKey);
int elektraNiGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNiSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNiError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
