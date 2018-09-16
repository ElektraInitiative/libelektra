/**
 * @file
 *
 * @brief Header for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_REFERENCE_H
#define ELEKTRA_PLUGIN_REFERENCE_H

#include <kdbplugin.h>


int elektraReferenceOpen (Plugin * handle, Key * errorKey);
int elektraReferenceClose (Plugin * handle, Key * errorKey);
int elektraReferenceGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (reference);

#endif
