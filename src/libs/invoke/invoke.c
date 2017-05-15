#include <kdbinvoke.h>

#include <stdio.h>

ElektraInvokeHandle * elektraInvokeInitialize (const char * elektraPluginName)
{
	if (!elektraPluginName)
	{
		return NULL;
	}
	ElektraInvokeHandle * handle = elektraCalloc (sizeof (ElektraInvokeHandle));
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
