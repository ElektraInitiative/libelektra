#ifndef KDBINVOKE_H
#define KDBINVOKE_H

#include <kdb.h>
#include <kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ElektraInvokeHandle ElektraInvokeHandle;
typedef struct _ElektraDeferredCallList ElektraDeferredCallList;

/**
 * Declaration for functions that can be called with elektraDeferredCallsExecute().
 *
 * @param  plugin     plugin handle
 * @param  parameters function parameters
 */
typedef void (*ElektraDeferredCallable) (Plugin * plugin, ElektraKeyset * parameters);
typedef void (*ElektraDeferredCall) (Plugin * plugin, const char * functionName, ElektraKeyset * parameters);

ElektraInvokeHandle * elektraInvokeOpen (const char *, ElektraKeyset * config, ElektraKey * errorKey);
ElektraInvokeHandle * elektraInvokeInitialize (const char *);

const void * elektraInvokeGetFunction (ElektraInvokeHandle *, const char *);
ElektraKeyset * elektraInvokeGetPluginConfig (ElektraInvokeHandle *);
const char * elektraInvokeGetPluginName (ElektraInvokeHandle *);
void * elektraInvokeGetPluginData (ElektraInvokeHandle *);

ElektraKeyset * elektraInvokeGetModules (ElektraInvokeHandle *);
ElektraKeyset * elektraInvokeGetExports (ElektraInvokeHandle *);

int elektraInvoke2Args (ElektraInvokeHandle *, const char *, ElektraKeyset * ks, ElektraKey * k);

void elektraInvokeClose (ElektraInvokeHandle *, ElektraKey * errorKey);

int elektraInvokeCallDeferable (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, ElektraKeyset * parameters);
void elektraInvokeExecuteDeferredCalls (ElektraInvokeHandle * handle, ElektraDeferredCallList * list);

int elektraDeferredCallAdd (ElektraDeferredCallList * list, const char * name, ElektraKeyset * parameters);
ElektraDeferredCallList * elektraDeferredCallCreateList (void);
void elektraDeferredCallDeleteList (ElektraDeferredCallList * list);
void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list);
int elektraDeferredCall (Plugin * handle, const char * elektraPluginFunctionName, ElektraKeyset * parameters);

#ifdef __cplusplus
}
}
#endif


#endif
