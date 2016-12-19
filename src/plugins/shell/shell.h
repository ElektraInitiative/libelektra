/**
 * @file
 *
 * @brief Header for shell plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SHELL_H
#define ELEKTRA_PLUGIN_SHELL_H

#include <kdbplugin.h>


int elektraShellOpen (Plugin * handle, Key * errorKey);
int elektraShellClose (Plugin * handle, Key * errorKey);
int elektraShellGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraShellSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraShellError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (shell);

#endif
