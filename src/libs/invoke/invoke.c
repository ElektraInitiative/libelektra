#include <kdbinvoke.h>

#include <kdbmodule.h>
#include <kdbprivate.h>
#include <stdio.h>

typedef struct
{
    Plugin * plugin;
    KeySet * modules;
}ElektraInvokeHandle;

void * elektraInvokeInitialize(const char *elektraPluginName)
{
    if(!elektraPluginName)
    {
        return NULL;
    }
    ElektraInvokeHandle *handle = elektraCalloc(sizeof(ElektraInvokeHandle));
    if(!handle)
    {
        return NULL;
    }
    Key *errorKey = keyNew(0, KEY_END);
    KeySet * modules = ksNew(0, KS_END);
    handle->modules = modules;
    elektraModulesInit(modules, NULL);
    Plugin * plugin = elektraPluginOpen(elektraPluginName, modules, ksNew (0, KS_END), errorKey);
    if(!plugin)
    {
        keyDel(errorKey);
        elektraModulesClose(modules, NULL);
        ksDel(modules);
        elektraFree(handle);
        return NULL;
    }
    keyDel(errorKey);
    handle->plugin = plugin;
    return (void *)handle;
}

const void * elektraInvokeGetFunction(void * invokeHandle, const char *elektraPluginFunctionName)
{
    ElektraInvokeHandle * handle = invokeHandle;
    if(!handle || !elektraPluginFunctionName)
    {
        return NULL;
    }
    Plugin * plugin = handle->plugin;
    KeySet * exports = ksNew(0, KS_END);
    Key * exportParent = keyNew("system/elektra/modules", KEY_END);
    keyAddBaseName(exportParent, plugin->name);
    plugin->kdbGet(plugin, exports, exportParent);
    keyAddBaseName(exportParent, "exports");
    keyAddBaseName(exportParent, elektraPluginFunctionName);
    const void * functionPtr = NULL;
    Key *functionKey = ksLookup(exports, exportParent, 0);
    if(!functionKey)
    {
        keyDel(exportParent);
        ksDel(exports);
        return NULL;
    }
    functionPtr = keyValue(functionKey);
    keyDel(exportParent);
    ksDel(exports);
    return functionPtr;
}

void elektraInvokeClose(void *invokeHandle)
{
    if(!invokeHandle)
    {
        return;
    }
    ElektraInvokeHandle * handle = invokeHandle;
    Key * errorKey = keyNew(0, KEY_END);
    elektraPluginClose(handle->plugin, errorKey);
    keyDel(errorKey);
    elektraModulesClose(handle->modules, NULL);
    ksDel(handle->modules);
    elektraFree(handle);
}
