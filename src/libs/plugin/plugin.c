/**
 * @file
 *
 * @brief Access plugin handle.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbinternal.h>


/**
 * @brief Allows one to Export Methods for a Plugin.
 *
 * This function must be called within ELEKTRA_PLUGIN_EXPORT.
 * It define the plugin's methods that will be exported.
 *
 * All KDB methods implemented by the plugin basically could
 * have random names (convention is elektraName*), except
 * ELEKTRA_PLUGIN_EXPORT.
 *
 * This is the single symbol that will be looked up
 * when loading the plugin, and the first method of the backend
 * implementation that will be called.
 *
 * You need to use a macro so that both dynamic and static loading
 * of the plugin works. For example for the doc plugin:
 * @snippet doc.c export
 *
 * The first parameter is the name of the plugin.
 * Then every plugin should have:
 * @c ELEKTRA_PLUGIN_OPEN,
 * @c ELEKTRA_PLUGIN_CLOSE,
 * @c ELEKTRA_PLUGIN_GET,
 * @c ELEKTRA_PLUGIN_SET and optionally
 * @c ELEKTRA_PLUGIN_ERROR.
 *
 * The list is terminated with
 * @c ELEKTRA_PLUGIN_END.
 *
 * You must use static "char arrays" in a read only segment.
 * Don't allocate storage, it won't be freed.
 *
 * @param pluginName the name of this plugin
 * @return an object that contains all plugin information needed by
 * 	libelektra.so
 * @ingroup plugin
 */
Plugin * elektraPluginExport (const char * pluginName, ...)
{
	va_list va;
	Plugin * returned;
	plugin_t method = 0;

	if (pluginName == 0) return 0;

	returned = elektraCalloc (sizeof (struct _Plugin));

	/* Start processing parameters */
	va_start (va, pluginName);
	returned->name = pluginName;

	while ((method = va_arg (va, plugin_t)))
	{
		switch (method)
		{
		case ELEKTRA_PLUGIN_OPEN:
			returned->kdbOpen = va_arg (va, kdbOpenPtr);
			break;
		case ELEKTRA_PLUGIN_CLOSE:
			returned->kdbClose = va_arg (va, kdbClosePtr);
			break;
		case ELEKTRA_PLUGIN_GET:
			returned->kdbGet = va_arg (va, kdbGetPtr);
			break;
		case ELEKTRA_PLUGIN_SET:
			returned->kdbSet = va_arg (va, kdbSetPtr);
			break;
		case ELEKTRA_PLUGIN_ERROR:
			returned->kdbError = va_arg (va, kdbErrorPtr);
			break;
		default:
#if DEBUG
			printf ("plugin passed something unexpected\n");
#endif
		// fallthrough, will end here
		case ELEKTRA_PLUGIN_END:
			va_end (va);
			return returned;
		}
	}
	return returned;
}


/**
 * @brief Returns the configuration of that plugin.
 *
 * - The user/ config holds plugin specific configuration
 * - The system/ config holds backend specific configuration
 *
 * So prefer cascading lookups to honor both.
 *
 * @param handle a pointer to the plugin
 * @ingroup plugin
 * @return keyset to the configuration for that plugin
 */
KeySet * elektraPluginGetConfig (Plugin * handle)
{
	return handle->config;
}

/**
 * @brief Store a pointer to any plugin related data.
 *
 * @param plugin a pointer to the plugin
 * @param data the pointer to the data
 * @ingroup plugin
 */
void elektraPluginSetData (Plugin * plugin, void * data)
{
	plugin->data = data;
}

/**
 * @brief Get a pointer to any plugin related data stored before.
 *
 * @param plugin a pointer to the plugin
 * @return a pointer to the data
 * @ingroup plugin
 */
void * elektraPluginGetData (Plugin * plugin)
{
	return plugin->data;
}
