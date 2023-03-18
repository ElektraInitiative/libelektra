/**
 * @file
 *
 * @brief Header for xfconf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_XFCONF_H
#define ELEKTRA_PLUGIN_XFCONF_H

#include <elektra/plugin/plugin.h>

int elektraXfconfInit (Key * errorKey, int xfconfCode, int xfconfShutdown);
int elektraXfconfOpen (Plugin * handle, Key * errorKey);
int elektraXfconfClose (Plugin * handle, Key * errorKey);
int elektraXfconfGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXfconfSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
