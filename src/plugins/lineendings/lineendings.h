/**
 * @file
 *
 * @brief Header for lineendings plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LINEENDINGS_H
#define ELEKTRA_PLUGIN_LINEENDINGS_H

#include <elektra/kdbplugin.h>


int elektraLineendingsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLineendingsCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
