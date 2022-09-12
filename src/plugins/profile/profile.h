/**
 * @file
 *
 * @brief Header for profile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PROFILE_H
#define ELEKTRA_PLUGIN_PROFILE_H

#include <kdbplugin.h>


int elektraProfileOpen (Plugin * handle, ElektraKey * errorKey);
int elektraProfileClose (Plugin * handle, ElektraKey * errorKey);
int elektraProfileGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraProfileSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraProfileError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
