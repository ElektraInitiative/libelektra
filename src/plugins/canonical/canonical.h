/**
 * @file
 *
 * @brief Header for canonical plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CANONICAL_H
#define ELEKTRA_PLUGIN_CANONICAL_H

#include <kdbplugin.h>


int elektraCanonicalOpen (Plugin * handle, Key * errorKey);
int elektraCanonicalClose (Plugin * handle, Key * errorKey);
int elektraCanonicalGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCanonicalSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCanonicalError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCanonicalCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (canonical);

#endif
