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

#include "elektra/kdb.h"
#include "kdbtypes.h"

/**
 * @defgroup kdbnotification Notification
 *
 * @brief Notification feature
 *
 * For an introduction to notifications please see the
 * <a href="doc_tutorials_notifications_md.html">Notification Tutorial</a>.
 *
 * Examples:
 *
 * - [Basic notifications using polling](https://www.libelektra.org/examples/notificationpolling)
 * - [Using asynchronous I/O bindings](https://www.libelektra.org/examples/notificationasync)
 * - [Reload KDB when Elektra's configuration has changed](https://www.libelektra.org/examples/notificationreload)
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
 * @ingroup kdbnotification
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
 * @ingroup kdbnotification
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

#define ELEKTRA_NOTIFICATION_REGISTER_NAME(TYPE_NAME) elektraNotificationRegister##TYPE_NAME

#define ELEKTRA_NOTIFICATION_REGISTER_SIGNATURE(TYPE, TYPE_NAME)                                                                           \
	/** @copydoc elektraNotificationRegisterInt */                                                                                     \
	int ELEKTRA_NOTIFICATION_REGISTER_NAME (TYPE_NAME) (KDB * kdb, Key * key, TYPE * variable)

#define ELEKTRA_NOTIFICATION_TYPE_DECLARATION(TYPE, TYPE_NAME) ELEKTRA_NOTIFICATION_REGISTER_SIGNATURE (TYPE, TYPE_NAME);

/**
 * @ingroup kdbnotification
 * @brief Subscribe for automatic updates to a given variable when the given key value is changed.
 *
 * On kdbGet iff the key is present and its content is valid, the registered variable is updated.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  variable variable
 *
 * @retval 1 on success
 * @retval 0 on failure
 * @{
 */
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (int, Int)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (unsigned int, UnsignedInt)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (long, Long)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (unsigned long, UnsignedLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (long long, LongLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (unsigned long long, UnsignedLongLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (float, Float)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (double, Double)

ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_boolean_t, KdbBoolean)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_char_t, KdbChar)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_octet_t, KdbOctet)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_short_t, KdbShort)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_unsigned_short_t, KdbUnsignedShort)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_long_t, KdbLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_unsigned_long_t, KdbUnsignedLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_long_long_t, KdbLongLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_unsigned_long_long_t, KdbUnsignedLongLong)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_float_t, KdbFloat)
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_double_t, KdbDouble)
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
ELEKTRA_NOTIFICATION_TYPE_DECLARATION (kdb_long_double_t, KdbLongDouble)
#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE
/** @} */


/**
 * @ingroup kdbnotification
 * Callback function called when string to number conversion failed.
 *
 * @param  key      key with invalid value
 * @param  context  user supplied callback context
 */
typedef void (*ElektraNotificationConversionErrorCallback) (Key * key, void * context);

/**
 * @ingroup kdbnotification
 * Callback function for key changes.
 *
 * @param  key      changed key
 * @param  context  user supplied callback context
 */
typedef void (*ElektraNotificationChangeCallback) (Key * key, void * context);

/**
 * @ingroup kdbnotification
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

/**
 * @ingroup kdbnotification
 * Subscribe for updates via callback when a given key or a key below changed.
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  callback callback function
 * @param  context  user supplied context passed to callback function
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
int elektraNotificationRegisterCallbackSameOrBelow (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback, void * context);


#ifdef __cplusplus
}
}
#endif

#endif
