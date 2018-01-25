/**
 * @file
 *
 * @brief Elektra-Notification structures and declarations
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
