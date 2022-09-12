/**
 * @file
 *
 * @brief Header for list plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LIST_H
#define ELEKTRA_PLUGIN_LIST_H

#include <kdbioplugin.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>

int elektraListOpen (Plugin * handle, ElektraKey * errorKey);
int elektraListClose (Plugin * handle, ElektraKey * errorKey);
int elektraListGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraListSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraListError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraListAddPlugin (Plugin * handle, ElektraKeyset * pluginConfig);
int elektraListEditPlugin (Plugin * handle, ElektraKeyset * pluginConfig);
int elektraListMountPlugin (Plugin * handle, const char * pluginName, ElektraKeyset * pluginConfig, ElektraKey * errorKey);
int elektraListUnmountPlugin (Plugin * handle, const char * pluginName, ElektraKey * errorKey);
Plugin * elektraListFindPlugin (Plugin * handle, const char * pluginName);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
