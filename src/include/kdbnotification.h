/**
 * @file
 *
 * @brief Elektra-Notification structures and declarations for
 * application developers
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * @ingroup kdbnotification
 */

#ifndef KDB_NOTIFICATION_H_
#define KDB_NOTIFICATION_H_

#include "kdb.h"

/**
 * @defgroup kdbnotification Notification
 *
 * @brief Notification feature
 *
 * For an introduction to notifications please see the
 * <a href="doc_tutorials_notifications_md.html">Notification Tutorial</a>.
 *
 * @par Global Mounting
 *
 * elektraNotificationOpen() loads and mounts the
 * <a href="https://www.libelektra.org/plugins/internalnotification">internalnotification plugin</a>
 * globally at run-time.
 * The key database is not altered permanently.
 * elektraNotificationClose() reverts the mounting.
 *
 * The internalnotification plugin is mounted at its defined positions
 * (see
 * <a href="https://www.libelektra.org/plugins/internalnotification">its plugin docs</a>).
 *
 * - If there is no plugin mounted at a required position the internalnotification
 *   plugin is mounted at this position.
 * - In the default configuration or when mounting a plugin globally using
 *   `kdb global-mount` the
 *   <a href="https://www.libelektra.org/plugins/list">list plugin</a> is
 *   mounted at all positions.
 *   This plugin allows to mount multiple plugins at a position.
 *   If this plugin is present at a position the internalnotification plugin is
 *   added to the list plugin's configuration at run-time.
 * - If another plugin is mounted at a desired position the configuration is
 *   considered broken and mounting is aborted.
 *   The list plugin requires to be mounted at all positions in order to keep
 *   track of the current position and call plugins accordingly.
 *
 * @par Transport Plugins
 *
 * Notification transport plugins (or simply transport plugins) export
 * "openNotification" (ElektraNotificationOpenNotification()) and
 * optionally "closeNotification" (ElektraNotificationCloseNotification())
 * functions as part of their contract.
 *
 * The function "openNotification" is called during elektraNotificationOpen().
 * At this point an I/O binding is set and it is save to use it.
 * If no binding is set despite the plugin requires it, it should log a message
 * and perform no additional operations.
 * This ensures that the plugin can be used in applications that do not use I/O
 * bindings or notification features.
 *
 * ElektraNotificationOpenNotification() receives a callback and additional data.
 * When a key change notification is received (or a change is detected by other
 * means) this callback shall be called with the changed Key and the additional
 * data.
 *
 */

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * Initialize the notification system for the given KDB instance.
 *
 * Asynchronous receiving of notifications requires an I/O binding.
 * Use elektraIoSetBinding() before calling this function.
 *
 * May only be called once for a KDB instance. Subsequent calls return 0.
 *
 * @param  kdb         KDB instance
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraNotificationOpen (KDB * kdb);

/**
 * Stop the notification system for the given KDB instance.
 *
 * May only be called after elektraNotificationOpen() was called for given KDB
 * instance.
 *
 * @param  kdb         KDB instance
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraNotificationClose (KDB * kdb);

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
int elektraNotificationRegisterInt (KDB * kdb, Key * key, int * variable);

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
int elektraNotificationRegisterFloat (KDB * kdb, Key * key, float * variable);

/**
 * Callback function for key changes.
 *
 * @param  key      changed key
 * @param  Context  user supplied callback context
 */
typedef void (*ElektraNotificationChangeCallback) (Key * key, void * context);

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
int elektraNotificationRegisterCallback (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback, void * context);


#ifdef __cplusplus
}
}
#endif

#endif
