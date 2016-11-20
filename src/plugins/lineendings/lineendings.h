/**
 * @file
 *
 * @brief Header for lineendings plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LINEENDINGS_H
#define ELEKTRA_PLUGIN_LINEENDINGS_H

#include <kdbplugin.h>


int elektraLineendingsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLineendingsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (lineendings);

#endif
