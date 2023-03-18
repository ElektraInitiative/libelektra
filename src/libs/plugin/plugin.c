/**
 * @file
 *
 * @brief Access plugin handle.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <internal/utility/assert.h>
#include <elektra/kdb.h>
#include <internal/kdb/config.h>
#include <elektra/kdb/errors.h>
#include <elektra/ease/old_ease.h>
#include <elektra/ease/meta.h>
#include <internal/utility/logger.h>
#include <internal/pluginload/module.h>
#include <elektra/plugin/plugin.h>
#include <internal/kdbprivate.h>
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
 * @c ELEKTRA_PLUGIN_ERROR and
 * @c ELEKTRA_PLUGIN_COMMIT.
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
		case ELEKTRA_PLUGIN_INIT:
			returned->kdbInit = va_arg (va, kdbInitPtr);
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
		case ELEKTRA_PLUGIN_COMMIT:
			returned->kdbCommit = va_arg (va, kdbCommitPtr);
			break;
		default:
			ELEKTRA_ASSERT (0, "plugin passed something unexpected");
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
 * - The user:/ config holds plugin specific configuration
 * - The system:/ config holds backend specific configuration
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
 * @brief Store a pointer to plugin specific data.
 *
 * This data is private to one instance of a plugin.
 *
 * @see elektraPluginGetData
 * @param plugin a pointer to the plugin
 * @param data the pointer to the data
 * @ingroup plugin
 */
void elektraPluginSetData (Plugin * plugin, void * data)
{
	plugin->data = data;
}

/**
 * @brief Get a pointer to the plugin specific data stored before.
 *
 * If elektraPluginSetData() was not called earlier, NULL will be returned.
 *
 * This data is private to one instance of a plugin.
 *
 * @see elektraPluginSetData
 * @param plugin a pointer to the plugin
 * @return a pointer to the data
 * @ingroup plugin
 */
void * elektraPluginGetData (Plugin * plugin)
{
	return plugin->data;
}

/**
 * @brief Get a pointer to the global keyset.
 *
 * Initialized for all plugins by the KDB, except for manually
 * created plugins with `elektraPluginOpen()`.
 * The global keyset is tied to a KDB handle, initialized on
 * `kdbOpen()` and deleted on `kdbClose()`.
 *
 * Plugins using this keyset are responsible for cleaning up
 * their parts of the keyset which they do not need any more.
 *
 * @param plugin a pointer to the plugin
 * @return a pointer to the global keyset
 * @ingroup plugin
 */
KeySet * elektraPluginGetGlobalKeySet (Plugin * plugin)
{
	return plugin->global;
}

/**
 * Returns the current phase of the current KDB operation.
 *
 * During kdbGet() this will be one of the `ELEKTRA_KDB_GET_PHASE_*` constants
 * and during kdbSet() it will be one of the `ELEKTRA_KDB_SET_PHASE_*` constants.
 *
 * @param plugin plugin handle
 * @return current phase
 * @retval 0 if @p plugin is `NULL`
 */
ElektraKdbPhase elektraPluginGetPhase (Plugin * plugin)
{
	if (plugin == NULL)
	{
		return 0;
	}

	return *(ElektraKdbPhase *) keyValue (ksLookupByName (plugin->global, "system:/elektra/kdb/backend/phase", 0));
}

/**
 * Retrieves the handle for another plugin in the same mountpoint based on a reference.
 *
 * The plugins of a mountpoint are defined via `system:/elektra/mountpoint/<mp>/pluigns/<ref>` keys
 * in the declaration of the mountpoint. To use this function, you must provide the `<ref>` part as
 * @p ref.
 *
 * @param plugin active plugin handle
 * @param ref reference to another plugin
 * @return the plugin referenced by @p ref
 * @retval NULL if @p plugin, or @p ref are `NULL`, or no plugin was found for @p ref
 */
Plugin * elektraPluginFromMountpoint (Plugin * plugin, const char * ref)
{
	if (plugin == NULL || ref == NULL)
	{
		return NULL;
	}

	KeySet * plugins = *(KeySet **) keyValue (ksLookupByName (plugin->global, "system:/elektra/kdb/backend/plugins", 0));

	Key * lookupHelper = keyNew ("system:/", KEY_END);
	keyAddBaseName (lookupHelper, ref);

	Key * pluginKey = ksLookup (plugins, lookupHelper, 0);
	keyDel (lookupHelper);

	return pluginKey == NULL ? NULL : *(Plugin **) keyValue (pluginKey);
}
