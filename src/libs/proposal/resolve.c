#include <kdbmodule.h>
#include <kdbprivate.h>
#include <kdbproposal.h>

int elektraResolveFilename(Key *parentKey, ElektraResolveTempfile tmpFile)
{
    int rc = 0;
    KeySet * modules = ksNew(0, KS_END);
    elektraModulesInit(modules, 0);
    Plugin * resolver = NULL;
    resolver = elektraPluginOpen("resolver", modules, ksNew (0, KS_END), parentKey);
    KeySet * exports = ksNew(0, KS_END);
    Key * fpk = keyNew("system/elektra/modules", KEY_END);
    keyAddBaseName(fpk, resolver->name);
    resolver->kdbGet(resolver, exports, fpk);
    ElektraResolved *resolved = NULL;
    typedef ElektraResolved * (*resolveFileFunc)(elektraNamespace, const char *, ElektraResolveTempfile, Key *);
    keyAddBaseName(fpk, "exports");
    keyAddBaseName(fpk, "filename");
    resolveFileFunc resolveFunc = *((resolveFileFunc *)keyValue(ksLookup(exports, fpk, 0)));
    typedef void (*freeHandleFunc)(ElektraResolved *);
    keySetBaseName(fpk, "freeHandle");
    freeHandleFunc freeHandle = *((freeHandleFunc *)keyValue(ksLookup(exports, fpk, 0)));
    resolved = resolveFunc(keyGetNamespace(parentKey), keyString(parentKey), tmpFile, parentKey);
    if(!resolved)
    {
        rc = -1;
        goto RESOLVE_FAILED;
    }
    else
    {
        keySetString(parentKey, resolved->fullPath);
        freeHandle(resolved);
    }

RESOLVE_FAILED:
    elektraPluginClose(resolver, parentKey);
    elektraModulesClose(modules, NULL);
    ksDel(modules);
    ksDel(exports);
    keyDel(fpk);
    return rc;
}
