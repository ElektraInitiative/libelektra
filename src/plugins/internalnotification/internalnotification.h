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

#include <kdbnotification.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>


int elektraInternalnotificationGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationClose (Plugin * handle, Key * errorKey);
int elektraInternalnotificationOpen (Plugin * handle, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (internalnotification);

// Not exported by plugin; used for testing
void elektraInternalnotificationUpdateRegisteredKeys (Plugin * plugin, KeySet * keySet);
void elektraInternalnotificationDoUpdate (Key * changedKey, ElektraNotificationCallbackContext * context);

#define INTERNALNOTIFICATION_REGISTER_NAME(TYPE_NAME) elektraInternalnotificationRegister##TYPE_NAME
#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME(TYPE_NAME) elektraInternalnotificationConvert##TYPE_NAME

#define INTERNALNOTIFICATION_EXPORT_FUNCTION(TYPE_NAME)                                                                                    \
	keyNew ("system/elektra/modules/internalnotification/exports/register" #TYPE_NAME, KEY_FUNC,                                       \
		INTERNALNOTIFICATION_REGISTER_NAME (TYPE_NAME), KEY_END)

#define INTERNALNOTIFICATION_REGISTER_SIGNATURE(TYPE, TYPE_NAME)                                                                           \
	int INTERNALNOTIFICATION_REGISTER_NAME (TYPE_NAME) (Plugin * handle, Key * key, TYPE * variable)

#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE(TYPE_NAME)                                                                      \
	void INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME) (Key * key, void * context)

#define INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE(CHECK_RANGE) (*end == 0 && errno == 0 && CHECK_RANGE)
#define INTERNALNOTIFICATION_CHECK_CONVERSION (*end == 0 && errno == 0)

#define INTERNALNOTIFICATION_TYPE(TYPE, VALUE_TYPE, TYPE_NAME, TO_VALUE, CHECK_CONVERSION)                                                 \
	INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE (TYPE_NAME)                                                                     \
	{                                                                                                                                  \
		TYPE * variable = (TYPE *) context;                                                                                        \
		char * end ELEKTRA_UNUSED;                                                                                                 \
		const char * string = keyValue (key);                                                                                      \
		errno = 0;                                                                                                                 \
		/* convert string to target type */                                                                                        \
		VALUE_TYPE value = TO_VALUE;                                                                                               \
		/* only update if conversion was successful */                                                                             \
		if (CHECK_CONVERSION)                                                                                                      \
		{                                                                                                                          \
			*(variable) = value;                                                                                               \
		}                                                                                                                          \
		else                                                                                                                       \
		{                                                                                                                          \
			ELEKTRA_LOG_WARNING ("conversion failed! string=%s, stopped=%c errno=%d", keyString (key), *end, errno);           \
		}                                                                                                                          \
	}                                                                                                                                  \
                                                                                                                                           \
	INTERNALNOTIFICATION_REGISTER_SIGNATURE (TYPE, TYPE_NAME)                                                                          \
	{                                                                                                                                  \
		PluginState * pluginState = elektraPluginGetData (handle);                                                                 \
		ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");                                         \
		KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (                                          \
			pluginState, key, INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME), variable);                            \
		if (registeredKey == NULL)                                                                                                 \
		{                                                                                                                          \
			return 0;                                                                                                          \
		}                                                                                                                          \
		return 1;                                                                                                                  \
	}

#endif
