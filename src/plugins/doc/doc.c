/***************************************************************************
            doc.c  - Documentation on how to write plugins
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#include <kdbplugin.h>


#define DOC_PLUGIN_NAME "doc"
#define DOC_PLUGIN_VERSION "1.0.0"


/**
 * @defgroup plugin Plugins :: Elektra framework for plugins
 *
 * @section intro Introduction
 *
 * @since Since version 0.4.9, Elektra can dynamically load different key storage
 * plugins.
 *
 * @since Since version 0.7.0 Elektra can have multiple plugins,
 * mounted at any place in the key database.
 *
 * @since Since version 0.8.0 Elektra plugins are composed out of multiple
 * plugins.
 *
 * @subsection overview Overview
 *
 * A plugin can implement anything related to configuration.
 * There are 5 possible entry points, but you need not to implement
 * all of them.
 * See the descriptions below what each of them is supposed to do.
 *
 *
 */





/**
 * Initialize the plugin.
 * This is the first method called after dynamically loading
 * this plugin.
 *
 * This method is responsible for:
 * - plugin's specific configuration gathering
 * - all plugin's internal structs initialization
 * - if unavoidable initial setup of all I/O details such as opening a file, connecting to a
 *   database, setup connection to a server, etc.
 *
 * You may also read the configuration you can get with elektraPluginGetConfig() and transform it
 * into other structures used by your plugin.
 *
 * @note The plugin must not have any global variables. If you do your plugin will
 * not be threadsafe.
 *
 * Instead you can use elektraPluginGetData() and elektraPluginSetData() to store
 * and get any information related to your plugin.
 *
 * The correct substitute for global variables will be:
 * @code
struct _GlobalData{ int global; };
typedef struct _GlobalData GlobalData;
int elektraPluginOpen(KDB *handle) {
	PasswdData *data;
	data=malloc(sizeof(PasswdData));
	data.global = 20;
	kdbhSetBackendData(handle,data);
}
 * @endcode
 *
 * Make sure to free everything in elektraPluginClose().
 *
 * @return 0 on success
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @see kdbOpen()
 * @ingroup plugin
 */
int elektraPluginOpen(Plugin *handle)
{
	/* plugin initialization logic */

	return 0; /* success */
}




/**
 * Finalize the plugin.
 * Called prior to unloading the plugin dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 *
 * Make sure to free all memory that your plugin requested at runtime.
 *
 * Specifically make sure to capDel() all capabilites and free your pluginData in
 * kdbhGetBackendData().
 *
 * After this call, libelektra.so will unload the plugin library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup plugin
 */
int elektraPluginClose(KDB *handle) {
	return 0; /* success */
}



/**
 * Retrieve information from a permanent storage to construct
 * a keyset.
 *
 * @section intro Introduction
 *
 * This function does everything related to get keys out from a
 * plugin. There is only one function for that purpose to make
 * implementation and locking much easier.
 *
 * The keyset @p returned needs to be filled with information
 * so that the application using elektra can access it.
 * See the live cycle of a comment to understand:
 * @code
elektraPluginGet(KDB *handle, KeySet *returned, Key *parentKey)
{
	// the task of elektraPluginGet is to retrieve the comment out of the permanent storage
	Key *key = keyDup (parentKey); // generate a new key to hold the information
	char *comment;
	loadfromdisc (comment);
	keySetComment (key, comment, size); // set the information
	ksAppendKey(returned, key);
}

// Now return to kdbGet
int kdbGet(KDB *handle, KeySet *keyset, Key *parentKey, options)
{
	elektraPluginGet (handle, keyset, 0);
	// postprocess the keyset and return it
}

// Now return to usercode, waiting for the comment
void usercode (Key *key)
{
	kdbGet (handle, keyset, parentKey, 0);
	key = ksCurrent (keyset, key); // lookup the key from the keyset
	keyGetComment (key); // now the usercode retrieves the comment
}

 * @endcode
 * Of course not only the comment, but all information of every key in the keyset
 * @p returned need to be fetched from permanent storage and stored in the key.
 * So this specification needs to give
 * an exhaustive list of information present in a key.
 *
 * @section conditions Conditions
 *
 * @pre The caller kdbGet() will make sure before you are called
 * that the parentKey:
 * - is a valid key (means that it is a system or user key).
 * - is below (see keyIsBelow()) your mountpoint and that your plugin is responsible for it.
 * and that the returned:
 * - is a valid keyset.
 * - has @p all keys with the flag KEY_FLAG_SYNC set.
 * - contains only valid keys direct below (see keyIsDirectBelow()) your parentKey.
 *   That also means, that the parentKey will not be in that keyset.
 * - is in a sorted order, see ksSort().
 * and that the handle:
 *  - is a valid KDB for your plugin.
 *  - that elektraPluginhGetBackendHandle() contains the same handle for lifetime kdbOpen()
 *    until elektraPluginClose() was called.
 *
 * @pre The caller kdbGet() will make sure that afterwards you were called,
 * whenever the user requested it with the options, that:
 * - hidden keys they will be thrown away.
 * - dirs or only dirs kdbGet() will remove
 *   the other.
 * - you will be called again recursively with all subdirectories.
 * - the keyset will be sorted when needed.
 * - the keys in returned having KEY_FLAG_SYNC will be sorted out.
 *
 * @invariant There are no global variables and kdbhGetBackendData()
 *  only stores information which can be regenerated any time.
 *  The handle is the same when it is the same plugin.
 *
 * @post The keyset @p returned has the @p parentKey and all keys direct
 * below (keyIsDirectBelow()) with all information from the storage.
 * Make sure to return all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbGet() will
 * drop these keys, see above.
 *
 * @section detail Details
 *
 * Now lets look at an example how the typical elektraPluginGet() might be
 * implemented. To explain we introduce some pseudo functions which do all
 * the work with the storage (which is of course 90% of the work for a real
 * plugin):
 * - find_key() gets an key out from the storage and memorize the position.
 * - next_key() will find the next key and return it (with the name).
 * - fetch_key() gets out all information of a key from storage
 *    (details see below example).
 * - stat_key() gets all meta information (everything but value and comment).
 *   It removes the key keyNeedSync() flag afterwards.
 * returns the next key out from the storage.
 * The typical loop now will be like:
 * @code
ssize_t elektraPluginGet(KDB *handle, KeySet *update, const Key *parentKey) {
	Key * current;
	KeySet *returned = ksNew(ksGetSize(update)*2, KS_END);

	find_key (parentKey);
	current = keyDup (parentKey);
	current = fetch_key(current);

	keyClearSync (current);
	ksAppendKey(returned, current);

	while ((current = next_key()) != 0)
	{
		// search if key was passed in update by caller
		Key * tmp = ksLookup (update, current, KDB_O_WITHOWNER|KDB_O_POP);
		if (tmp) current = tmp; // key was passed, so use it
		current = fetch_key(current);
		keyClearSync (current);
		ksAppendKey(returned, current);
		// TODO: delete lookup key
	}

	if (error_happened())
	{
		errno = restore_errno();
		return -1;
	}

	ksClear (update); // the rest of update keys is not in storage anymore
	ksAppend(update, returned); // append the keys
	ksDel (returned);

	return nr_keys();
}
 * @endcode
 *
 * @note - returned and update are separated, for details why see ksLookup()
 * - the bit KEY_FLAG_SYNC is always cleared, see postconditions
 *
 * So your mission is simple: Search the @c parentKey and add it and then search
 * all keys below and add them too, of course with all the values.
 *
 * @section updating Updating
 *
 * To get all keys out of the storage over and over again can be very inefficient.
 * You might know a more efficient method to know if the key needs update or not,
 * e.g. by stating it or by an external time stamp info. In that case you can make
 * use of @p returned KeySet. There are following possibilities:
 * - The key is in returned and up to date.
 *   You just need to remove the KEY_FLAG_SYNC flag.
 * - The key is not in returned.
 *   You need to fully retrieve the key out of storage, clear
 *   KEY_FLAG_SYNC using keyClearSync() and ksAppendKey() it to the @p returned keyset.
 *
 * @note You must clear the flag KEY_FLAG_SYNC at the very last point where no more
 * modification on the key will take place, because any modification on the key will
 * set the KEY_FLAG_SYNC flag again. With that keyNeedSync() will return true and
 * the caller will sort this key out.
 *
 * @section fullget only Full Get
 *
 * In some plugins it is not useful to get only a part of the configuration, because
 * getting all keys would take as long as getting some. For this situation,
 * you can declare onlyFullGet, see kdbcGetonlyFullGet().
 *
 * The only valid call for your plugin is then that @p parentKey equals the @p mountpoint.
 * For all other @p parentKey you must, add nothing and just return 0.
 *
 * @code
if (strcmp (keyName(kdbhGetMountpoint(handle)), keyName(parentKey))) return 0;
 * @endcode
 *
 * If the @p parentKey is your mountpoint you will of course fetch all keys, and not only
 * the keys direct below the @c parentKey.
 * So @p returned is valid iff:
 * - every key is below ( keyIsBelow()) the parentKey
 * - every key has a direct parent (keyIsDirectBelow()) in the keyset
 *
 * @note This statement is only valid for plugins with kdbcGetonlyFullGet() set.
 *
 * @note If any calls you use change errno, make sure to restore the old errno.
 *
 * @see kdbGet() for caller.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset where the function need to
 * append the keys got from the storage. There might be also some
 * keys inside it, see conditions. You may use them to support
 * efficient updating of keys, see @ref updating.
 * @param parentKey contains the information below which key
 * the keys should be gotten.
 *
 * @return 1 on success
 * @return 0 when nothing was to do
 * @return -1 on failure, the current key in returned shows the position.
 *         use ELEKTRA_SET_ERROR in <kdberrors> to define the error code
 *
 * @ingroup plugin
 */
int elektraPluginGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	/* get all keys below parentKey and count them with nr_keys */

	return nr_keys; /* success */
}

/**
 * Store a keyset permanently.
 *
 * This function does everything related to set and remove keys in a
 * plugin. There is only one function for that purpose to make
 * implementation and locking much easier.
 *
 * The keyset @p returned was filled in with information from the application
 * using elektra and the task of this function is to store it in a permanent
 * way so that a subsequent call of elektraPluginGet() can rebuild the keyset
 * as it was before. See the live cycle of a comment to understand:
 * @code
void usercode (Key *key)
{
	keySetComment (key, "mycomment"); // the usercode stores a comment for the key
	ksAppendKey(keyset, key); // append the key to the keyset
	kdbSet (handle, keyset, 0, 0);
}

// so now kdbSet is called
int kdbSet(KDB *handle, KeySet *keyset, Key *parentKey, options)
{
	// find appropriate plugin
	elektraPluginSet (handle, keyset, 0); // the keyset with the key will be passed to this function
}

// so now elektraPluginSet(), which is the function described here, is called
elektraPluginSet(KDB *handle, KeySet *keyset, Key *parentKey)
{
	// the task of elektraPluginSet is now to store the comment
	Key *key = ksCurrent (keyset); // get out the key where the user set the comment before
	char *comment = allocate(size);
	keyGetComment (key, comment, size);
	savetodisc (comment);
}
 * @endcode
 * Of course not only the comment, but all information of every key in the keyset
 * @p returned need to be stored permanetly. So this specification needs to give
 * an exhaustive list of information present in a key.
 *
 * @pre The keyset @p returned holds all keys which must be saved
 * permanently for this keyset. The keyset is sorted and rewinded.
 * All keys having children must be true for keyIsDir().
 *
 * @pre The @p parentKey is the key which is the ancestor for all other keys in the
 * keyset. The first key of the keyset @p returned has the same keyname.
 * The parentKey is below the mountpoint, see kdbhGetMountpoint().
 *
 * @pre The caller kdbSet will fulfill following parts:
 * - If the user does not want hidden keys they will be thrown away.
 *   All keys in @p returned need to be stored permanently.
 * - If the user does not want dirs or only dirs kdbGet() will remove
 *   the other.
 * - Sorting of the keyset. It is not important in which order the keys
 *   are appended.
 * So make sure to set all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbSet() will
 * sort them out.
 *
 * @invariant There are no global variables and kdbhGetBackendData()
 *  only stores information which can be regenerated any time.
 *  The handle is the same when it is the same plugin.
 *
 * @post The information of the keyset @p returned is stored permanently.
 *
 * Lock your permanent storage in an exclusive way, no access of a
 * concurrent elektraPluginSet_plugin() or kdbGet() is possible
 * and these methods block until the function has finished.
 * Otherwise declare kdbcGetnoLock().
 *
 * @see kdbSet() for caller.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 *
 * @return When everything works gracefully return the number of keys you set.
 * The cursor position and the keys remaining in the keyset are not important.
 *
 * @return Return 0 on success with no changed key in database
 *
 * @return Return -1 on failure.
 *
 * @note If any calls you use change errno, make sure to restore the old errno.
 *
 * @err In normal execution cases a positive value will be returned.
 * But in some cases you are not able to set keys and have to
 * return -1. If you declare kdbcGetnoError() you are done, but
 * otherwise you have to set the cause of the error.
 * (Will be added with 0.7.1)
 *
 * You also have to make sure that ksGetCursor()
 * shows to the position where the error appeared.
 *
 * @ingroup plugin
 */
int elektraPluginSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	/* set all keys below parentKey and count them with nr_keys */

	return nr_keys;
}

/**
 * All KDB methods implemented by the plugin can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the plugin, and the first method of the backend
 * implementation that will be called.
 *
 * Its purpose is to publish the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 *
 * The first paramter is the name of the plugin.
 * Then every plugin must have:
 * @c KDB_BE_OPEN,
 * @c KDB_BE_CLOSE,
 * @c KDB_BE_GET and
 * @c KDB_BE_SET
 *
 * You might also give following information by char *:
 * @c KDB_BE_VERSION,
 * @c KDB_BE_AUTHOR,
 * @c KDB_BE_LICENCE,
 * @c KDB_BE_DESCRIPTION,
 * @c ELEKTRA_PLUGIN_NEEDS and
 * @c ELEKTRA_PLUGIN_PROVIDES
 *
 * You must use static "char arrays" in a read only segment.
 * Don't allocate storage, it won't be freed.
 *
 * With capability you can get that information on
 * runtime from any plugin with kdbGetCapability().
 *
 * The last parameter must be @c KDB_BE_END.
 *
 * @return kdbBackendExport() with the above described parameters.
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup plugin
 */
Plugin *ELEKTRA_PLUGIN_EXPORT(doc)
{
	return elektraPluginExport(DOC_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraPluginOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraPluginClose,
		ELEKTRA_PLUGIN_GET,	&elektraPluginGet,
		ELEKTRA_PLUGIN_SET,	&elektraPluginSet,
		ELEKTRA_PLUGIN_END);
}

