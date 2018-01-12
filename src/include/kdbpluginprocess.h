#ifndef KDBPLUGINPROCESS_H
#define KDBPLUGINPROCESS_H

#include <kdb.h>
#include <kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _ElektraPluginProcess ElektraPluginProcess;

ElektraPluginProcess * elektraPluginProcessInit (Key *);
void elektraPluginProcessStart (Plugin *, ElektraPluginProcess *);

int elektraPluginProcessOpen (ElektraPluginProcess *, Key *);

int elektraPluginProcessIsParent (const ElektraPluginProcess *);
int elektraPluginProcessSend (const ElektraPluginProcess *, plugin_t, KeySet *, Key *);

int elektraPluginProcessClose (ElektraPluginProcess *);

#ifdef __cplusplus
}
}
#endif


#endif
