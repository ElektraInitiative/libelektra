/**
 * @file
 *
 * @brief Helpers for global plugins
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */

#include <kdbglobal.h>
#include <kdbinternal.h>

/**
 * @internal
 * Helper functions to execute global plugins
 */

int elektraGlobalGet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	int ret = 0;
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		ret = plugin->kdbGet (plugin, ks, parentKey);
	}
	return ret;
}

int elektraGlobalSet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	int ret = 0;
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		ret = plugin->kdbSet (plugin, ks, parentKey);
	}
	return ret;
}

int elektraGlobalError (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	int ret = 0;
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		ret = plugin->kdbError (plugin, ks, parentKey);
	}
	return ret;
}
