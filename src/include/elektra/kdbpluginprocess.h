#ifndef KDBPLUGINPROCESS_H
#define KDBPLUGINPROCESS_H

#include <elektra/kdb.h>
#include <elektra/kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * Switches to denote which plugin methods to call in the child process.
 * Basically a duplicate of the enum in kdbplugin.h, but necessary to
 * have our own version to bypass the duplicate export check and to
 * allow this library to possibly be extended to further commands in
 * the future
 */
typedef enum
{
	// clang-format off
ELEKTRA_PLUGINPROCESS_OPEN=1,		/*!< Call the plugin's open function */
ELEKTRA_PLUGINPROCESS_CLOSE=1<<1,	/*!< Call the plugin's close function */
ELEKTRA_PLUGINPROCESS_GET=1<<2,		/*!< Call the plugin's get function */
ELEKTRA_PLUGINPROCESS_SET=1<<3,		/*!< Call the plugin's set function */
ELEKTRA_PLUGINPROCESS_ERROR=1<<4,	/*!< Call the plugin's error function */
ELEKTRA_PLUGINPROCESS_COMMIT=1<<5,	/*!< Call the plugin's commit function */
ELEKTRA_PLUGINPROCESS_INIT=1<<6,	/*!< Call the plugin's init function */
ELEKTRA_PLUGINPROCESS_END=0			/*!< End of arguments */
	// clang-format on
} pluginprocess_t;

typedef struct _ElektraPluginProcess ElektraPluginProcess;

typedef struct ElektraPluginProcessCloseResult
{
	int result, cleanedUp;
} ElektraPluginProcessCloseResult;

ElektraPluginProcess * elektraPluginProcessInit (Key *);
void elektraPluginProcessStart (Plugin *, ElektraPluginProcess *);

int elektraPluginProcessOpen (ElektraPluginProcess *, Key *);

int elektraPluginProcessIsParent (const ElektraPluginProcess *);
int elektraPluginProcessSend (const ElektraPluginProcess *, pluginprocess_t, KeySet *, Key *);

ElektraPluginProcessCloseResult elektraPluginProcessClose (ElektraPluginProcess *, Key *);

void elektraPluginProcessSetData (ElektraPluginProcess *, void *);
void * elektraPluginProcessGetData (const ElektraPluginProcess *);

#ifdef __cplusplus
}
}
#endif


#endif
