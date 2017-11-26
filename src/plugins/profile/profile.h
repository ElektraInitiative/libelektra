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


int elektraProfileOpen (Plugin * handle, Key * errorKey);
int elektraProfileClose (Plugin * handle, Key * errorKey);
int elektraProfileGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraProfileSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraProfileError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (profile);

#endif
