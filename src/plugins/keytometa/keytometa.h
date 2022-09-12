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

#include <kdberrors.h>
#include <kdbextension.h>
#include <kdbplugin.h>

int elektraKeyToMetaGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraKeyToMetaSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraKeyToMetaClose (Plugin * handle, ElektraKey * errorKey);


Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
