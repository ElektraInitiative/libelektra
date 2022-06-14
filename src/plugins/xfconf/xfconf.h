/**
 * @file
 *
 * @brief Header for xfconf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_XFCONF_H
#define ELEKTRA_PLUGIN_XFCONF_H

#include <kdbplugin.h>


int elektraXfconfOpen (Plugin * handle, Key * errorKey);
int elektraXfconfClose (Plugin * handle, Key * errorKey);
int elektraXfconfGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXfconfSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXfconfError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXfconfCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXfconfCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
