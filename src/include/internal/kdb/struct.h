#ifndef ELEKTRA_KDB_STRUCT_H
#define ELEKTRA_KDB_STRUCT_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <elektra/plugin/plugin.h>

#include <internal/plugin/functions.h>
#include <internal/changetracking.h>

#include <stdbool.h>
#include <stddef.h>

typedef struct _SendNotificationHook
{
	Plugin * plugin;
	struct _SendNotificationHook * next;

	/**
	 * Optional, may be NULL
	 */
	kdbHookSendNotificationGetPtr get;

	/**
	 * Optional, may be NULL
	 */
	kdbHookSendNotificationSetPtr set;
} SendNotificationHook;

/**
 * The access point to the key database.
 *
 * The structure which holds all information about loaded backends.
 *
 * Its internal private attributes should not be accessed directly.
 *
 * See kdb mount tool to mount new backends.
 *
 * KDB object is defined as:
 * @code
typedef struct _KDB KDB;
 * @endcode
 *
 * @see kdbOpen() and kdbClose() for external use
 * @ingroup backend
 */

struct _KDB
{
	KeySet * modules; /*!< A list of all modules loaded at the moment.*/

	KeySet * global; /*!< This keyset can be used by plugins to pass data through
			the KDB and communicate with other plugins. Plugins shall clean
			up their parts of the global keyset, which they do not need any more.*/

	KeySet * backends;

	struct
	{
		struct
		{
			Plugin * plugin;
			kdbHookGoptsGetPtr get;
		} gopts;

		struct
		{
			Plugin * plugin;
			kdbHookSpecCopyPtr copy;
			kdbHookSpecRemovePtr remove;
		} spec;

		struct _SendNotificationHook * sendNotification;
	} hooks;

	KeySet * allKeys;

	struct _ChangeTrackingContext changeTrackingContext;
};

/**
 * Holds all data for one backend.
 *
 * This struct is used for the key values in @ref _KDB.backends
 *
 * @ingroup backend
 */
typedef struct _BackendData
{
	Plugin * backend;    /*!< the backend plugin for this backend */
	KeySet * keys;	     /*!< holds the keys for this backend, assigned by backendsDivide() */
	KeySet * plugins;    /*!< Holds all the plugins of this backend.
	    The key names are all `system:/<ref>` where `<ref>` is the same as in
	    `system:/elektra/mountpoints/<mp>/plugins/<ref>` */
	KeySet * definition; /*!< Holds all the mountpoint definition of this backend.
	 This is a copy of `system:/elektra/mountpoints/<mp>/defintion` moved to `system:/` */
	size_t getSize;	     /*!< the size of @ref _BackendData.keys at the end of kdbGet()
	      More precisely this is set by backendsMerge() to the size of @ref _BackendData.keys */
	bool initialized;    /*!< whether or not the init function of this backend has been called */
} BackendData;

#endif // ELEKTRA_KDB_STRUCT_H
