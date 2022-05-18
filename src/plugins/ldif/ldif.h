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


int elektraLdifGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLdifSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
