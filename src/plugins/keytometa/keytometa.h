/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_KEYTOMETA_H
#define ELEKTRA_PLUGIN_KEYTOMETA_H

#include <elektra/kdbextension.h>
#include <elektra/kdbplugin.h>
#include <kdberrors.h>

int elektraKeyToMetaGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraKeyToMetaSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraKeyToMetaClose (Plugin * handle, Key * errorKey);


Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
