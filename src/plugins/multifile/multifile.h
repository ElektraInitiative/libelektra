/**
 * @file
 *
 * @brief Header for multifile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MULTIFILE_H
#define ELEKTRA_PLUGIN_MULTIFILE_H

#include <kdbplugin.h>


int elektraMultifileOpen (Plugin * handle, Key * errorKey);
int elektraMultifileClose (Plugin * handle, Key * errorKey);
int elektraMultifileGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMultifileSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMultifileError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMultifileCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (multifile);

#endif
