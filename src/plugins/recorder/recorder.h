/**
 * @file
 *
 * @brief Header for recorder plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_RECORDER_H
#define ELEKTRA_PLUGIN_RECORDER_H

#include <kdbplugin.h>


int elektraRecorderOpen (Plugin * handle, Key * errorKey);
int elektraRecorderClose (Plugin * handle, Key * errorKey);
int elektraRecorderGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRecorderRecord (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
