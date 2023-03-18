/**
 * @file
 *
 * @brief Header for process plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PROCESS_H
#define ELEKTRA_PLUGIN_PROCESS_H

#include <elektra/plugin/plugin.h>


int elektraProcessOpen (Plugin * handle, Key * errorKey);
int elektraProcessClose (Plugin * handle, Key * errorKey);
int elektraProcessGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraProcessSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
