/**
 * @file
 *
 * @brief Header for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_BACKENDPRIVATE_H
#define ELEKTRA_BACKENDPRIVATE_H

#include <internal/kdbprivate.h>

typedef struct _PluginList
{
	Plugin * plugin;
	struct _PluginList * next;
} PluginList;

typedef struct
{
	char * path;
	struct
	{
		Plugin * resolver;
		PluginList * prestorage;
		Plugin * storage;
		PluginList * poststorage;
	} getPositions;
	struct
	{
		Plugin * resolver;
		PluginList * prestorage;
		Plugin * storage;
		PluginList * poststorage;
		PluginList * precommit;
		Plugin * commit;
		PluginList * postcommit;
		PluginList * prerollback;
		Plugin * rollback;
		PluginList * postrollback;
	} setPositions;
} BackendHandle;

#endif // ELEKTRA_BACKENDPRIVATE_H
