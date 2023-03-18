/**
 * @file
 * @ingroup kdbnotification
 *
 * @brief Elektra-Notification structures and declarations for developing
 * notification and transport plugins. Only available in Elektra's source.
 *
 * Only used by elektra-notification library, notification plugins (e.g.
 * internalnotification) and transport plugins.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_NOTIFICATION_PLUGIN_H_
#define KDB_NOTIFICATION_PLUGIN_H_

#include "kdb.h"
#include "kdbnotification.h"
#include "kdbplugin.h"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#define ELEKTRA_NOTIFICATION_REGISTERFUNC_TYPEDEF(FUNC_TYPE_NAME, TYPE)                                                                    \
	typedef int (*FUNC_TYPE_NAME) (Plugin * handle, Key * key, TYPE * variable);

#define ELEKTRA_NOTIFICATION_TYPE_DEFINITION(TYPE, TYPE_NAME)                                                                              \
	ELEKTRA_NOTIFICATION_REGISTER_SIGNATURE (TYPE, TYPE_NAME)                                                                          \
	{                                                                                                                                  \
		if (!kdb || !key || !variable)                                                                                             \
		{                                                                                                                          \
			ELEKTRA_LOG_WARNING ("null pointer passed");                                                                       \
			return 0;                                                                                                          \
		}                                                                                                                          \
		/* get notification plugins */                                                                                             \
		Plugin * notificationPlugin = getNotificationPlugin (kdb);                                                                 \
		if (!notificationPlugin)                                                                                                   \
		{                                                                                                                          \
			return 0;                                                                                                          \
		}                                                                                                                          \
		/* get register function from plugin */                                                                                    \
		size_t func = elektraPluginGetFunction (notificationPlugin, "register" #TYPE_NAME);                                        \
		if (!func)                                                                                                                 \
		{                                                                                                                          \
			return 0;                                                                                                          \
		}                                                                                                                          \
		ELEKTRA_NOTIFICATION_REGISTERFUNC_TYPEDEF (RegisterFuncType, TYPE)                                                         \
		RegisterFuncType registerFunc = (RegisterFuncType) func;                                                                   \
		return registerFunc (notificationPlugin, key, variable);                                                                   \
	}

/**
 * Subscribe for updates via callback when a given key value is changed.
 * Exported as "registerCallback" by notification plugins.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  callback callback function
 * @param  context  user supplied context passed to callback function
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterCallback) (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback,
							  void * context);

/**
 * Subscribe for updates via callback when a given key or a key below is changed.
 * Exported as "registerCallbackSameOrBelow" by notification plugins.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  callback callback function
 * @param  context  user supplied context passed to callback function
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterCallbackSameOrBelow) (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback,
								     void * context);

/**
 * Allow setting a callback that is called when a value conversion failed.
 * Exported as "setConversionErrorCallback" notification plugins.
 *
 * @param  kdb      kdb handle
 * @param  callback callback
 * @param  context  context
 */
typedef void (*ElektraNotificationSetConversionErrorCallback) (Plugin * handle, ElektraNotificationConversionErrorCallback callback,
							       void * context);

/**
 * Context for notification callbacks.
 */
typedef struct _ElektraNotificationCallbackContext ElektraNotificationCallbackContext;

/**
 * Notify notification library of changes to a key.
 *
 * This callback is provided by the `internalnotification` plugin and
 * for use by notification transport plugins.
 *
 * @param  key      changed key
 * @param  context  additional data
 */
typedef void (*ElektraNotificationCallback) (Key * key, ElektraNotificationCallbackContext * context);

/**
 * Used by notification plugins to get values from the key database.
 *
 * @param  kdb        kdb handle
 * @param  changedKey which key was updated
 */
typedef void (*ElektraNotificationKdbUpdate) (KDB * kdb, Key * changedKey);

/**
 * Private struct with information about for ElektraNotificationCallback.
 * @internal
 *
 * Not intended to be used by plugins
 */
struct _ElektraNotificationCallbackContext
{
	ElektraNotificationKdbUpdate kdbUpdate; /*!< The pointer to the update function.*/

	Plugin * notificationPlugin; /*!< Notification plugin handle.*/
};

#ifdef __cplusplus
}
}
#endif

#endif
