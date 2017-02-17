/**
 * @file
 *
 * @brief Helpers for global plugins
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdbglobal.h>
#include <kdbprivate.h>

/**
 * @internal
 * Helper functions to execute global plugins
 */

void elektraGlobalGet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbGet (plugin, ks, parentKey);
	}
}

void elektraGlobalSet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbSet (plugin, ks, parentKey);
	}
}

void elektraGlobalError (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbError (plugin, ks, parentKey);
	}
}
