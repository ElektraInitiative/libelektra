#ifndef KDBINVOKE_H
#define KDBINVOKE_H

#include <elektra/kdb.h>
#include <elektra/kdbplugin.h>

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
typedef void (*ElektraDeferredCallable) (Plugin * plugin, KeySet * parameters);
typedef void (*ElektraDeferredCall) (Plugin * plugin, const char * functionName, KeySet * parameters);

ElektraInvokeHandle * elektraInvokeOpen (const char *, KeySet * config, Key * errorKey);
ElektraInvokeHandle * elektraInvokeInitialize (const char *);

const void * elektraInvokeGetFunction (ElektraInvokeHandle *, const char *);
KeySet * elektraInvokeGetPluginConfig (ElektraInvokeHandle *);
const char * elektraInvokeGetPluginName (ElektraInvokeHandle *);
void * elektraInvokeGetPluginData (ElektraInvokeHandle *);

KeySet * elektraInvokeGetModules (ElektraInvokeHandle *);
KeySet * elektraInvokeGetExports (ElektraInvokeHandle *);

int elektraInvoke2Args (ElektraInvokeHandle *, const char *, KeySet * ks, Key * k);

void elektraInvokeClose (ElektraInvokeHandle *, Key * errorKey);

int elektraInvokeCallDeferable (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, KeySet * parameters);
void elektraInvokeExecuteDeferredCalls (ElektraInvokeHandle * handle, ElektraDeferredCallList * list);

int elektraDeferredCallAdd (ElektraDeferredCallList * list, const char * name, KeySet * parameters);
ElektraDeferredCallList * elektraDeferredCallCreateList (void);
void elektraDeferredCallDeleteList (ElektraDeferredCallList * list);
void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list);
int elektraDeferredCall (Plugin * handle, const char * elektraPluginFunctionName, KeySet * parameters);

#ifdef __cplusplus
}
}
#endif


#endif
