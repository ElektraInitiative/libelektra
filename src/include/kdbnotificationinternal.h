/**
 * @file
 * @ingroup kdbnotification
 *
 * @brief Elektra-Notification structures and declarations for developing
 * notification and transport plugins.
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

/**
 * Subscribe for automatic updates to a given integer variable when the given
 * key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable integer variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterInt) (Plugin * handle, Key * key, int * variable);

/**
 * Subscribe for automatic updates to a given long variable when the given
 * key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable long variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterLong) (Plugin * handle, Key * key, long * variable);

/**
 * Subscribe for automatic updates to a given unsigned long variable when the given
 * key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable unsigned long variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterUnsignedLong) (Plugin * handle, Key * key, unsigned long * variable);

/**
 * Subscribe for automatic updates to a given float variable when the given
 * key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable float variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterFloat) (Plugin * handle, Key * key, float * variable);

/**
 * Subscribe for automatic updates to a given double variable when the given
 * key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable double variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterDouble) (Plugin * handle, Key * key, double * variable);

/**
 * Subscribe for updates via callback when a given key value is changed.
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
 * Context for notification callbacks.
 */
typedef struct _ElektraNotificationCallbackContext ElektraNotificationCallbackContext;

/**
 * Notify notification library of changes to a key.
 *
 * This callback is passed by `elektraNotificationOpen` to plugins
 * exporting a `openNotification` function.
 *
 * @param  key      changed key
 * @param  context  context passed by ElektraNotificationOpenNotification
 */
typedef void (*ElektraNotificationCallback) (Key * key, ElektraNotificationCallbackContext * context);

/**
 * Initialize plugin's notification capabilities.
 * Exported as "openNotification" by transport plugins.
 *
 * @param  handle     plugin handle
 * @param  parameters contains the keys "/callback" (ElektraNotificationCallback * ) and "/context" (ElektraNotificationCallbackContext *).
 */
typedef void (*ElektraNotificationOpenNotification) (Plugin * handle, KeySet * parameters);

/**
 * Teardown plugin's notification capabilities.
 *
 * Exported as "closeNotification" by transport plugins.
 *
 * @param  handle     plugin handle
 * @param  parameters unused
 */
typedef void (*ElektraNotificationCloseNotification) (Plugin * handle, KeySet * parameters);

/**
 * Private struct with information about for ElektraNotificationCallback.
 * @internal
 *
 * Not intended to be used by plugins
 */
struct _ElektraNotificationCallbackContext
{
	KDB * kdb; /*!< The pointer the KDB handle.*/

	Plugin * notificationPlugin; /*!< Notification plugin handle.*/
};

#ifdef __cplusplus
}
}
#endif

#endif
