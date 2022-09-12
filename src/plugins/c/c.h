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


int elektraCOpen (Plugin * handle, ElektraKey * errorKey);
int elektraCClose (Plugin * handle, ElektraKey * errorKey);
int elektraCGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
