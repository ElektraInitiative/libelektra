/**
 * @file
 *
 * @brief Header for ldif plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LDIF_H
#define ELEKTRA_PLUGIN_LDIF_H

#include <kdbplugin.h>


int elektraLdifOpen (Plugin * handle, Key * errorKey);
int elektraLdifClose (Plugin * handle, Key * errorKey);
int elektraLdifGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLdifSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLdifError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLdifCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLdifCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
