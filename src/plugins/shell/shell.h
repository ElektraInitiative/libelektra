/**
 * @file
 *
 * @brief Header for shell plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SHELL_H
#define ELEKTRA_PLUGIN_SHELL_H

#include <kdbplugin.h>


int elektraShellOpen (Plugin * handle, ElektraKey * errorKey);
int elektraShellClose (Plugin * handle, ElektraKey * errorKey);
int elektraShellGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraShellSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraShellError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
