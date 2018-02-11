/**
 * @file
 *
 * @brief Elektra-Notification structures and declarations for
 * application developers
 * 
 * @ingroup kdbnotification
 *
 * @defgroup kdbnotification Notification
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
 * - If no plugin is mounted at a desired position it is simply mounted.
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
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_NOTIFICATION_H_
#define KDB_NOTIFICATION_H_

#include "kdb.h"
#include "kdbio.h"

/**
 * Initialize the notification system for the given
 * KDB instance.
 *
 * May only be called once for a KDB instance. Subsequent calls return 0.
 *
 * @param  kdb         KDB instance
 * @param  ioBinding   I/O binding
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraNotificationOpen (KDB * kdb, ElektraIoInterface * ioBinding);

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
 * Callback function for key changes.
 *
 * @param  key changed key
 */
typedef void (*ElektraNotificationChangeCallback) (Key * key);

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
int elektraNotificationRegisterCallback (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback);

#endif
