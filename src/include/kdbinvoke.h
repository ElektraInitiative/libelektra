#ifndef KDBINVOKE_H
#define KDBINVOKE_H

#include <kdbmodule.h>
#include <kdbprivate.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct
{
	Plugin * plugin;
	KeySet * modules;
	KeySet * exports;
} ElektraInvokeHandle;

ElektraInvokeHandle * elektraInvokeInitialize (const char *);
const void * elektraInvokeGetFunction (ElektraInvokeHandle *, const char *);
void elektraInvokeClose (ElektraInvokeHandle *);


#ifdef __cplusplus
}
}
#endif


#endif
