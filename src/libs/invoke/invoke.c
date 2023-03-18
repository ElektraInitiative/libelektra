/**
 * @file
 *
 * @brief Library for invoking exported plugin functions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <internal/utility/assert.h>
#include <elektra/plugin/invoke.h>
#include <internal/pluginload/module.h>
#include <internal/kdbprivate.h> // for elektraPluginOpen/Close

#include <stdio.h>

struct _ElektraInvokeHandle
{
	Plugin * plugin;
	KeySet * modules;
	KeySet * exports;
};

/**
 * Structure for deferred calls
 * @internal
 */
typedef struct _ElektraDeferredCall
{
	char * name;
	KeySet * parameters;
	struct _ElektraDeferredCall * next;
} _ElektraDeferredCall;

/**
 * Structure for internal plugin state
 * @internal
 */
struct _ElektraDeferredCallList
{
	_ElektraDeferredCall * head;
	_ElektraDeferredCall * last;
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
 * Use `elektraInvokeOpen (name, 0, 0)` instead.
 *
 * @see elektraInvokeOpen()
 */
ElektraInvokeHandle * elektraInvokeInitialize (const char * elektraPluginName)
{
	return elektraInvokeOpen (elektraPluginName, 0, 0);
}

/**
 * @brief Opens a new handle to invoke functions for a plugin.
 *
 * When opening the plugin, it calls the "open" function of the plugin.
 *
 * @param elektraPluginName the plugin on which we want to invoke functions.
 * @param config the config to be passed to the plugin.
 * @param errorKey a key where error messages will be stored
 *
 * @return the handle
 * @retval 0 on errors
 */
ElektraInvokeHandle * elektraInvokeOpen (const char * elektraPluginName, KeySet * config, Key * errorKey)
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

	int errorKeyMissing = !errorKey;
	if (errorKeyMissing)
	{
		errorKey = keyNew ("/", KEY_END);
	}

	Plugin * plugin = elektraPluginOpen (elektraPluginName, modules, config, errorKey);
	if (errorKeyMissing)
	{
		keyDel (errorKey);
	}
	if (!plugin)
	{
		elektraModulesClose (modules, NULL);
		ksDel (modules);
		elektraFree (handle);
		return NULL;
	}
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

	Key * exportParent = keyNew ("system:/elektra/modules", KEY_END);
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

	// If we cast this right away although the function wasn't found it will cause a deadlock
	const void * rawFunc = elektraInvokeGetFunction (handle, elektraPluginFunctionName);

	if (!rawFunc) return -2;

	typedef int (*elektra2Args) (Plugin *, KeySet *, Key *);
	elektra2Args func = *(elektra2Args *) rawFunc;

	return func (handle->plugin, ks, k);
}

/**
 * @brief Closes all affairs with the handle.
 *
 * The close function of the plugin will be called.
 *
 * @param handle the handle to work with
 * @param errorKey a key where error messages will be stored
 *
 * @pre handle must be as returned from elektraInvokeOpen()
 */
void elektraInvokeClose (ElektraInvokeHandle * handle, Key * errorKey)
{
	if (!handle)
	{
		return;
	}
	int errorKeyMissing = !errorKey;
	if (errorKeyMissing)
	{
		errorKey = keyNew ("/", KEY_END);
	}
	elektraPluginClose (handle->plugin, errorKey);
	if (errorKeyMissing)
	{
		keyDel (errorKey);
	}
	elektraModulesClose (handle->modules, NULL);
	ksDel (handle->modules);
	ksDel (handle->exports);
	elektraFree (handle);
}

/**
 * Invokes a deferable function on an invoke handle.
 * If the function is exported by the plugin it is directly invoked,
 * if the plugin supports deferring calls, the call is deferred.
 *
 * The parameters key set can be freed afterwards.
 *
 * @param  handle                    invoke handle
 * @param  elektraPluginFunctionName function name
 * @param  parameters                parameter key set
 * @retval 0 on success
 * @retval -1 when the call failed (direct call and deferring not available)
 */
int elektraInvokeCallDeferable (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, KeySet * parameters)
{
	if (!handle)
	{
		return -1;
	}
	return elektraDeferredCall (handle->plugin, elektraPluginFunctionName, parameters);
}

/**
 * Execute deferred calls from list on given invoke handle.
 *
 * Used internally by plugins holding invoke handles.
 *
 * @param handle invoke handle
 * @param list   list
 */
void elektraInvokeExecuteDeferredCalls (ElektraInvokeHandle * handle, ElektraDeferredCallList * list)
{
	if (!handle)
	{
		return;
	}
	elektraDeferredCallsExecute (handle->plugin, list);
}

/**
 * Call a deferrable function on a plugin handle.
 * If the function is exported by the plugin it is directly invoked,
 * if the plugin supports deferring calls, the call is deferred.
 * If both is possible (function is exported and deferred calls are supported),
 * the function is directly called and the call is deferred (i.e. for nested plugins).
 *
 * @param  handle                    invoke handle
 * @param  elektraPluginFunctionName function name
 * @param  parameters                parameter key set. Can bee freed afterwards.
 * @retval 0 on success
 * @retval -1 when the call failed (direct call and deferring not available)
 */
int elektraDeferredCall (Plugin * handle, const char * elektraPluginFunctionName, KeySet * parameters)
{
	ELEKTRA_NOT_NULL (handle);
	ELEKTRA_NOT_NULL (elektraPluginFunctionName);

	int result;
	size_t direct = elektraPluginGetFunction (handle, elektraPluginFunctionName);
	if (direct)
	{
		ElektraDeferredCallable directFn = (ElektraDeferredCallable) direct;
		directFn (handle, parameters);
		result = 0; // success
	}
	else
	{
		// no direct call possible
		result = -1;
	}

	size_t deferredCall = elektraPluginGetFunction (handle, "deferredCall");
	if (deferredCall)
	{
		ElektraDeferredCall deferredCallFn = (ElektraDeferredCall) deferredCall;
		deferredCallFn (handle, elektraPluginFunctionName, parameters);
		result = 0; // success
	}
	else
	{
		// deferred calls not possible
		result = -1;
	}

	return result;
}

/**
 * Add a new deferred call to the deferred call list.
 *
 * Used internally by plugins.
 *
 * @param  list       deferred call list
 * @param  name       function name
 * @param  parameters function parameters
 * @retval 1 on success
 * @retval 0 when malloc failed
 */
int elektraDeferredCallAdd (ElektraDeferredCallList * list, const char * name, KeySet * parameters)
{
	ELEKTRA_NOT_NULL (list);
	ELEKTRA_NOT_NULL (name);
	_ElektraDeferredCall * item = elektraMalloc (sizeof *item);
	if (item == NULL)
	{
		return 0;
	}
	item->name = elektraStrDup (name);
	item->parameters = ksDup (parameters);
	item->next = NULL;

	if (list->head == NULL)
	{
		// Initialize list
		list->head = list->last = item;
	}
	else
	{
		// Make new item end of list
		list->last->next = item;
		list->last = item;
	}

	return 1;
}

/**
 * Create new deferred call list.
 *
 * The list needs to be deleted with elektraDeferredCallDeleteList().
 * Used internally by plugins.
 *
 * @return  new list
 */
ElektraDeferredCallList * elektraDeferredCallCreateList (void)
{
	ElektraDeferredCallList * list = elektraMalloc (sizeof *list);
	if (list == NULL)
	{
		return NULL;
	}
	list->head = NULL;
	list->last = NULL;
	return list;
}

/**
 * Delete deferred call list.
 *
 * Used internally by plugins.
 *
 * @param list list
 */
void elektraDeferredCallDeleteList (ElektraDeferredCallList * list)
{
	ELEKTRA_NOT_NULL (list);
	_ElektraDeferredCall * item = list->head;
	while (item != NULL)
	{
		elektraFree (item->name);
		ksDel (item->parameters);

		_ElektraDeferredCall * next = item->next;
		elektraFree (item);

		item = next;
	}

	elektraFree (list);
}

/**
 * Execute deferred calls on given plugin.
 *
 * Used internally by plugins.
 *
 * @param plugin plugin handle
 * @param list   list
 */
void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list)
{
	ELEKTRA_NOT_NULL (plugin);
	ELEKTRA_NOT_NULL (list);
	_ElektraDeferredCall * item = list->head;
	while (item != NULL)
	{
		size_t func = elektraPluginGetFunction (plugin, item->name);
		if (!func)
		{
			item = item->next;
			continue;
		}
		ElektraDeferredCallable callable = (ElektraDeferredCallable) func;
		callable (plugin, item->parameters);

		item = item->next;
	}
}

/**
 * @}
 */
