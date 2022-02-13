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

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
