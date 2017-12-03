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
 * and plugins outside of the KDB.
 *
 * @{
 *
 */


ElektraInvokeHandle * elektraInvokeInitialize (const char * elektraPluginName)
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
	Plugin * plugin = elektraPluginOpen (elektraPluginName, modules, ksNew (0, KS_END), errorKey);
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

const void * elektraInvokeGetFunction (ElektraInvokeHandle * invokeHandle, const char * elektraPluginFunctionName)
{
	ElektraInvokeHandle * handle = invokeHandle;
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

KeySet * elektraInvokeGetPluginConfig (ElektraInvokeHandle * handle)
{
	return handle->plugin->config;
}

const char * elektraInvokeGetPluginName (ElektraInvokeHandle * handle)
{
	return handle->plugin->name;
}

void * elektraInvokeGetPluginData (ElektraInvokeHandle * handle)
{
	return handle->plugin->data;
}

KeySet * elektraInvokeGetModules (ElektraInvokeHandle * handle)
{
	return handle->modules;
}

KeySet * elektraInvokeGetExports (ElektraInvokeHandle * handle)
{
	return handle->exports;
}


int elektraInvoke2Args (ElektraInvokeHandle * invokeHandle, const char * elektraPluginFunctionName, KeySet * ks, Key * k)
{
	typedef int (*elektra2Args) (KeySet*, Key *);
	elektra2Args func = *(elektra2Args *)elektraInvokeGetFunction (invokeHandle, elektraPluginFunctionName);

	if (!func)
	{
		return -2;
	}

	return func (ks, k);
}

void elektraInvokeClose (ElektraInvokeHandle * invokeHandle)
{
	if (!invokeHandle)
	{
		return;
	}
	ElektraInvokeHandle * handle = invokeHandle;
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
