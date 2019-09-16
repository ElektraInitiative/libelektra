/**
 * @file
 *
 * @brief Header file for the backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_H
#define ELEKTRA_BACKEND_H

#include <kdbplugin.h>

int elektraBackendOpen (Plugin * handle, Key * errorKey);
int elektraBackendClose (Plugin * handle, Key * errorKey);
int elektraBackendGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBackendSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBackendCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif //ELEKTRA_BACKEND_H
