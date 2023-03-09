/**
 * @file
 *
 * @brief Header for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_INTERNALNOTIFICATION_H
#define ELEKTRA_PLUGIN_INTERNALNOTIFICATION_H

#include <kdbchangetracking.h>
#include <kdbnotification.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>

int elektraInternalnotificationGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationClose (Plugin * handle, Key * errorKey);
int elektraInternalnotificationOpen (Plugin * handle, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

// Not exported by plugin; used for testing
void elektraInternalnotificationNotifyChangedKeys (Plugin * plugin, const ElektraDiff * diff);
void elektraInternalnotificationDoUpdate (Key * changedKey, ElektraNotificationCallbackContext * context);

#define INTERNALNOTIFICATION_REGISTER_NAME(TYPE_NAME) elektraInternalnotificationRegister##TYPE_NAME

#define INTERNALNOTIFICATION_EXPORT_FUNCTION(TYPE_NAME)                                                                                    \
	keyNew ("system:/elektra/modules/internalnotification/exports/register" #TYPE_NAME, KEY_FUNC,                                      \
		INTERNALNOTIFICATION_REGISTER_NAME (TYPE_NAME), KEY_END)

/**
 * Structure containing conversion context
 * @internal
 */
struct _ElektraInternalnotificationConversionContext
{
	void * variable;
	ElektraNotificationConversionErrorCallback errorCallback;
	void * errorCallbackContext;
};
typedef struct _ElektraInternalnotificationConversionContext _ElektraInternalnotificationConversionContext;


#endif
