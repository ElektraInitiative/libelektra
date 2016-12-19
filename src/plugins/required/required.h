/**
 * @file
 *
 * @brief Header for required plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_REQUIRED_H
#define ELEKTRA_PLUGIN_REQUIRED_H

#include <kdbplugin.h>


int elektraRequiredOpen (Plugin * handle, Key * errorKey);
int elektraRequiredClose (Plugin * handle, Key * errorKey);
int elektraRequiredGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRequiredSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRequiredError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRequiredCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (required);

#endif
