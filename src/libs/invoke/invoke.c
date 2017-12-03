#include <kdbinvoke.h>
#include <kdbmodule.h>
#include <kdbprivate.h> // for elektraPluginOpen/Close

#include <stdio.h>

struct _ElektraInvokeHandle
{
	Plugin * plugin;
	KeySet * modules;
	KeySet * exports;
};

/**
 * @defgroup invoke
 * @brief Functionality to use plugins and invoke functions
 *
 * Allows invoking functions of plugins as needed within applications
 * and plugins inside and outside of the KDB.
 *
 * To use this library, you need to include:
 *
 * @code
 * #include <kdbinvoke.h>`
 * @endcode
 *
 * and link against `libelektra-invoke`.
 * Then you can use it:
 *
 * @code
 * ElektraInvokeHandle * handle = elektraInvokeOpen ("dini", 0);
 * elektraInvoke2Args (handle, "get", ks, k);
 * elektraInvokeClose (handle);
 * @endcode
 *
 * @{
 *
 */

/**
 * @deprecated Do not use.
 *
 * Use `elektraInvokeOpen (name, 0)` instead.
 *
 * @see elektraInvokeOpen()
 */
ElektraInvokeHandle * elektraInvokeInitialize (const char * elektraPluginName)
{
	return elektraInvokeOpen (elektraPluginName, 0);
}

/**
 * @brief Opens a new handle to invoke functions for a plugin.
 *
 * When opening the plugin, it calls the "open" function of the plugin.
 *
 * @param elektraPluginName the plugin on which we want to invoke functions.
 * @param config the config to be passed to the plugin.
 *
 * @return the handle
 * @retval 0 on errors
 */
ElektraInvokeHandle * elektraInvokeOpen (const char * elektraPluginName, KeySet * config)
{
	if (!elektraPluginName)
	{
		return NULL;
	}
	ElektraInvokeHandle * handle = elektraCalloc (sizeof (struct _ElektraInvokeHandle));
	if (!handle)
	{
		return NULL;
	}
	Key * errorKey = keyNew (0, KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	handle->modules = modules;
	elektraModulesInit (modules, NULL);

	if (!config)
	{
		config = ksNew (0, KS_END);
	}
	else
	{
		config = ksDup (config);
	}

	Plugin * plugin = elektraPluginOpen (elektraPluginName, modules, config, errorKey);
	if (!plugin)
	{
		keyDel (errorKey);
		elektraModulesClose (modules, NULL);
		ksDel (modules);
		elektraFree (handle);
		return NULL;
	}
	keyDel (errorKey);
	handle->plugin = plugin;
	return handle;
}

/**
 * @brief Get a function pointer.
 *
 * @param handle the handle to use
 * @param elektraPluginFunctionName the name of the function to use, e.g., get/set
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * Example:
 *
 * @code
typedef int (*elektra2Args) (KeySet*, Key *);
elektra2Args func = *(elektra2Args *)elektraInvokeGetFunction (handle, elektraPluginFunctionName);
if (!func) exit(1);   // no function found, handle error
func (ks, k);         // otherwise, call function
 * @endcode
 *
 * @see elektraInvoke2Args() a convenience function to be used for KeySet,Key arguments.
 *
 * @return a function pointer for the specified function.
 */
const void * elektraInvokeGetFunction (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName)
{
	if (!handle || !elektraPluginFunctionName)
	{
		return NULL;
	}
	Plugin * plugin = handle->plugin;
	KeySet * exports = NULL;

	Key * exportParent = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (exportParent, plugin->name);

	if (handle->exports)
	{
		exports = handle->exports;
	}
	else
	{
		exports = ksNew (0, KS_END);
		handle->exports = exports;
		plugin->kdbGet (plugin, exports, exportParent);
	}
	keyAddBaseName (exportParent, "exports");
	keyAddBaseName (exportParent, elektraPluginFunctionName);
	Key * functionKey = ksLookup (exports, exportParent, 0);
	keyDel (exportParent);
	if (!functionKey)
	{
		return NULL;
	}
	else
	{
		return keyValue (functionKey);
	}
}

/**
 * @brief Get the configuration the plugin uses.
 *
 * @param handle the handle to use
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return the config of the plugin.
 */
KeySet * elektraInvokeGetPluginConfig (ElektraInvokeHandle * handle)
{
	if (!handle) return NULL;
	return handle->plugin->config;
}

/**
 * @brief Get the name of the plugin.
 *
 * The name might differ from the name as passed with elektraInvokeOpen
 * when symlinks are used.
 *
 * @param handle the handle to work with.
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return the name of the plugin 
 */
const char * elektraInvokeGetPluginName (ElektraInvokeHandle * handle)
{
	if (!handle) return NULL;
	return handle->plugin->name;
}

/**
 * @brief Get the data of the plugin.
 *
 * @param handle the handle to work with.
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return a pointer to the plugin's data.
 */
void * elektraInvokeGetPluginData (ElektraInvokeHandle * handle)
{
	if (!handle) return NULL;
	return handle->plugin->data;
}

/**
 * @brief Get the modules used for invoking.
 *
 * @warning The modules are closed within elektraInvokeClose().
 * It is *not* enough to ksDup() the keyset, you must not call
 * elektraInvokeClose() if you want to reuse the modules.
 *
 * @param handle the handle to work with.
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return the modules used for invoking.
 */
KeySet * elektraInvokeGetModules (ElektraInvokeHandle * handle)
{
	if (!handle) return NULL;
	return handle->modules;
}

/**
 * @brief Get the exports from the plugin.
 *
 * @param handle the handle to work with.
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return the exports of the plugin.
 */
KeySet * elektraInvokeGetExports (ElektraInvokeHandle * handle)
{
	if (!handle) return NULL;
	return handle->exports;
}


/**
 * @brief A convenience function to call a function with two arguments.
 *
 * @param handle the handle to work with
 * @param elektraPluginFunctionName the function to call, e.g. "get"
 * @param ks the keyset to be used as first parameter
 * @param k the key to be used as second parameter
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 *
 * @return the return value of the invoked function (i.e. -1, 0, or 1)
 * @retval -2 if the function was not found.
 */
int elektraInvoke2Args (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, KeySet * ks, Key * k)
{
	if (!handle || !elektraPluginFunctionName) return -2;

	typedef int (*elektra2Args) (KeySet*, Key *);
	elektra2Args func = *(elektra2Args *)elektraInvokeGetFunction (handle, elektraPluginFunctionName);

	if (!func)
	{
		return -2;
	}

	return func (ks, k);
}

/**
 * @brief Closes all affairs with the handle.
 *
 * The close function of the plugin will be called.
 *
 * @param handle the handle to work with
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 */
void elektraInvokeClose (ElektraInvokeHandle * handle)
{
	if (!handle)
	{
		return;
	}
	Key * errorKey = keyNew (0, KEY_END);
	elektraPluginClose (handle->plugin, errorKey);
	keyDel (errorKey);
	elektraModulesClose (handle->modules, NULL);
	ksDel (handle->modules);
	ksDel (handle->exports);
	elektraFree (handle);
}

/**
 * @}
 */
