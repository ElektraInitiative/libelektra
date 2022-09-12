/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NI_H
#define ELEKTRA_PLUGIN_NI_H

#include <kdbplugin.h>

#include <bohr/ni.h>


int elektraNiOpen (Plugin * handle, ElektraKey * errorKey);
int elektraNiClose (Plugin * handle, ElektraKey * errorKey);
int elektraNiGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraNiSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraNiError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
