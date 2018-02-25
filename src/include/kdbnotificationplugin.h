/**
 * @file
 * @ingroup kdbnotification
 *
 * @brief Elektra-Notification structures and declarations for developing
 * notification plugins.
 *
 * Only used by elektra-notification library and notification plugins (e.g.
 * internalnotification).
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_NOTIFICATION_PLUGIN_H_
#define KDB_NOTIFICATION_PLUGIN_H_

#include "kdb.h"
#include "kdbnotification.h"
#include "kdbplugin.h"

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
 * Subscribe for updates via callback when a given key value is changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  callback callback function
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
typedef int (*ElektraNotificationPluginRegisterCallback) (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback);

/**
 * Notify notification library of changes to a key.
 *
 * This callback is passed by `elektraNotificationOpen` to plugins
 * exporting a `openNotification` function.
 *
 * @param  key      changed key
 * @param  data     opaque data
 */
typedef void (*ElektraNotificationCallback) (Key * key, void * data);

/**
 * Initialize plugin's notification capabilities.
 *
 * Exported as "openNotification" by transport plugins.
 *
 * @param  handle   plugin handle
 * @param  callback callback function
 */
typedef void (*ElektraNotificationOpenNotification) (Plugin * handle, ElektraNotificationCallback callback, void * data);

/**
 * Teardown plugin's notification capabilities.
 *
 * Exported as "closeNotification" by transport plugins.
 *
 * @param  handle   plugin handle
 * @param  callback callback function
 */
typedef void (*ElektraNotificationCloseNotification) (Plugin * handle);

#endif
