#ifndef KDBINVOKE_H
#define KDBINVOKE_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ElektraInvokeHandle ElektraInvokeHandle;

ElektraInvokeHandle * elektraInvokeOpen (const char *, KeySet * config);
ElektraInvokeHandle * elektraInvokeInitialize (const char *);

const void * elektraInvokeGetFunction (ElektraInvokeHandle *, const char *);
KeySet * elektraInvokeGetPluginConfig (ElektraInvokeHandle *);
const char * elektraInvokeGetPluginName (ElektraInvokeHandle *);
void * elektraInvokeGetPluginData (ElektraInvokeHandle *);

KeySet * elektraInvokeGetModules (ElektraInvokeHandle *);
KeySet * elektraInvokeGetExports (ElektraInvokeHandle *);

int elektraInvoke2Args (ElektraInvokeHandle *, const char *, KeySet * ks, Key * k);

void elektraInvokeClose (ElektraInvokeHandle *);


#ifdef __cplusplus
}
}
#endif


#endif
