/**
 * @file
 *
 * @brief Header for stdioproc plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_STDIOPROC_H
#define ELEKTRA_PLUGIN_STDIOPROC_H

#include <kdbplugin.h>


int elektraStdioprocOpen (Plugin * handle, Key * errorKey);
int elektraStdioprocClose (Plugin * handle, Key * errorKey);
int elektraStdioprocGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraStdioprocSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraStdioprocError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraStdioprocCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraStdioprocCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
