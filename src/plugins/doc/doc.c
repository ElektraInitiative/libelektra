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

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif


#define DOC_PLUGIN_NAME "doc"
#define DOC_PLUGIN_VERSION "1.0.0"


/**
 * @defgroup plugin Plugins
 * @brief Elektra plugin framework
 *
 * @since version 0.4.9, Elektra can dynamically load different key storage
 * plugins.
 *
 * @since version 0.7.0 Elektra can have multiple backends,
 * mounted at any place in the key database.
 *
 * @since version 0.8.0 Elektra backends are composed out of multiple
 * plugins.
 *
 * @par Overview
 * There are different types of plugins for different concerns.
 * The types of plugins handled in this document:
 * - file storage plugins (also called just storage plugins here)
 * - filter plugins
 * \n
 * See http://www.libelektra.org/ftp/elektra/thesis.pdf
 * for an detailed explanation and description of other types
 * of plugins.
 * \n
 * A plugin can implement anything related to configuration.
 * There are 5 possible entry points, as described in this
 * document:
 * - elektraDocOpen()
 * - elektraDocClose()
 * - elektraDocGet()
 * - elektraDocSet()
 * - elektraDocError() (not needed by storage or filter plugins)
 * \n
 * Depending of the type of plugin you need not to implement all of
 * them.
 * \n
 * @note that the Doc within the name is just because the plugin
 *       described here is called doc (see src/plugins/doc/doc.c).
 *       Always replace Doc with the name of the plugin you
 *       are going to implement.
 *
 * See the descriptions below what each of them is supposed to do.
 *
 * @par Storage Plugins
 * A filter plugin is a plugin which already receives some keys.
 * It may process or change the keyset.
 * Or it may reject specific keysets which do not meet some
 * criteria.
 *
 * @par Filter Plugins
 * A storage plugin gets an empty keyset and constructs the
 * information out from a file.
 * \n
 * Other persistent storage then a file is not handled within
 * this document because it involves many other issues.
 * For files the resolver plugin already takes care for
 * transactions and rollback.
 *
 * @par Error and Warnings
 * In any case of trouble, use ELEKTRA_SET_ERROR and return with -1.
 * You might add warnings with ELEKTRA_ADD_WARNING if you think
 * it is appropriate.
 *
 * @note some docu in this section might be confusing or not updated,
 * please refer to http://www.libelektra.org/ftp/elektra/thesis.pdf
 * or ask at the mailinglist if something is unclear.
 *
 * @addtogroup plugin
 * @{
 */





/**
 * Initialize the plugin.
 *
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
 * @note The plugin must not have any global variables. If you do
 *       Elektra will not be threadsafe.
 *       It is not a good assumption that your plugin will be opened
 *       only once.
 *
 * Instead you can use elektraPluginGetData() and elektraPluginSetData() to store
 * and get any information related to your plugin.
 *
 * The correct substitute for global variables will be:
 * @code
struct _GlobalData{ int global; };
typedef struct _GlobalData GlobalData;
int elektraDocOpen(Plugin *handle, Key *errorKey)
{
	GlobalData *data;
	data=malloc(sizeof(GlobalData));
	data.global = 20;
	elektraPluginSetData(handle,data);
}
 * @endcode
 *
 * If your plugin has no useful way to startup without config, the
 * module loader would not be able to load the module, too.
 * To solve that problem the module loader adds the configuration key
 * /module. Even if your plugin is basically not able to startup
 * successfully, it should still provide a fallback when /module
 * is present, so that docGet() on system/elektra/modules can be
 * called successfully later on.
 *
 * @note Make sure to free everything you allocate here within elektraDocClose().
 *
 * @return 0 on success
 * @param handle contains internal information of the plugin
 * @param errorKey defines an errorKey
 * @see kdbOpen() which will call elektraDocOpen()
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @ingroup plugin
 */
int docOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 0; /* success */
}




/**
 * Finalize the plugin.
 *
 * Called prior to unloading the plugin dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 *
 * Make sure to free all memory that your plugin requested at runtime.
 *
 * After this call, libelektra.so will unload the plugin library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @param handle contains internal information of the plugin
 * @param errorKey is needed to add warnings using ELEKTRA_ADD_WARNING
 *
 * @retval 0 on success
 * @see kdbClose()
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @ingroup plugin
 */
int docClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 0; /* success */
}



/**
 * Retrieve information from a permanent storage to construct
 * a keyset.
 *
 * @section intro Introduction
 *
 * The elektraDocGet() function handle everything related
 * to receiving keys.
 *
 *
 * @subsection storage Storage Plugins
 *
 * For storage plugins the filename is written in the value of the
 * parentKey. So the first task of the plugin is to open that file.
 * Then it should parse its content and construct a keyset with all
 * information of that file.
 *
 * You need to be able to reconstruct the same file with the information
 * of the keyset. So be sure to copy all comments, whitespaces and so on
 * into some metadata of the keys. Otherwise the information is lost
 * after writing the file the next time.
 *
 * Now lets look at an example how the typical elektraDocGet() might be
 * implemented. To explain we introduce some pseudo functions which do all
 * the work with the storage (which is of course 90% of the work for a real
 * plugin):
 * - parse_key will parse a key and a value from an open file handle
 *
 * The typical loop for a storage plugin will be like:
 * @code
int elektraDocGet(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	// contract handling, see below

	FILE *fp = fopen (keyString(parentKey), "r");
	char *key;
	char *value;

	while ((n = parse_key(fp, &key, &value)) >= 1)
	{
		Key *read = keyNew(0);
		if (keySetName(read, key) == -1)
		{
			fclose (fp);
			keyDel (read);
			ELEKTRA_SET_ERROR(59, parentKey, key);
			return -1;
		}
		keySetString(read, value);

		ksAppendKey (returned, read);
		free (key);
		free (value);
	}

	if (feof(fp) == 0)
	{
		fclose (fp);
		ELEKTRA_SET_ERROR(60, parentKey, "not at the end of file");
		return -1;
	}

	fclose (fp);

	return 1; // success
}
 * @endcode
 *
 * @subsection filter Filter Plugins
 *
 * For filter plugins the actual task is rather unspecified.
 * You basically can do anything with the keyset.
 * To get roundtrip properties you might want to undo any
 * changes you did in elektraDocSet().
 *
 * The pseudo functions (which do the real work) are:
 * - do_action() which processes every key in this filter
 *
 * @code
int elektraDocGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	// contract handling

	Key *k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		do_action(k);
	}

	return 1; // success
}
 * @endcode
 *
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
 * - is in a sorted order (given implicit by semantics of KeySet)
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
 * @invariant There are no global variables and elektraPluginSetData()
 *  stores all information.
 *  The handle is to be guaranteed to be the same if it is the same plugin.
 *
 * @post The keyset @p returned has the @p parentKey and all keys direct
 * below (keyIsDirectBelow()) with all information from the storage.
 * Make sure to return all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbGet() will
 * drop these keys, see above.
 *
 *
 * @section updating Updating
 *
 * To get all keys out of the storage over and over again can be very inefficient.
 * You might know a more efficient method to know if the key needs update or not,
 * e.g. by stating it or by an external time stamp info.
 * For file storage plugins this is automatically done for you.
 * For other types (e.g. databases) you need to implement your own
 * resolver doing this.
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
 *         use ELEKTRA_SET_ERROR of kdberrors.h to define the error code
 *
 * @ingroup plugin
 */
int docGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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
 * @param handle contains internal information of the plugin
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 *
 * @return When everything works gracefully return the number of keys you set.
 * The cursor position and the keys remaining in the keyset are not important.
 *
 *
 * @note If any calls you use change errno, make sure to restore the old errno.
 *
 * @retval 1 on success
 * @retval 0 on success with no changed key in database
 * @retval -1 on failure. The cause of the error needs to beadded in parentKey
 *
 * You also have to make sure that ksGetCursor()
 * shows to the position where the error appeared.
 *
 * @ingroup plugin
 */
int docSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	ssize_t nr_keys = 0;
	/* set all keys below parentKey and count them with nr_keys */

	return nr_keys;
}

/**
 * Rollback in case of errors.
 *
 * @param handle contains internal information of the plugin
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 *
 * @retval 1 on success
 * @retval 0 on success with no action
 * @retval -1 on failure
 *
 * @ingroup plugin
 */
int docError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 0;
}

/**
 * All KDB methods implemented by the plugin can have random names, except
 * ELEKTRA_PLUGIN_EXPORT.
 * This is the single symbol that will be looked up
 * when loading the plugin, and the first method of the backend
 * implementation that will be called.
 *
 * You need to use a macro so that both dynamic and static loading
 * of the plugin works.
 *
 * The first parameter is the name of the plugin.
 * Then every plugin should have:
 * @c ELEKTRA_PLUGIN_OPEN,
 * @c ELEKTRA_PLUGIN_CLOSE,
 * @c ELEKTRA_PLUGIN_GET,
 * @c ELEKTRA_PLUGIN_SET and optionally
 * @c ELEKTRA_PLUGIN_ERROR.
 *
 * The list is terminated with
 * @c ELEKTRA_PLUGIN_END.
 *
 * You must use static "char arrays" in a read only segment.
 * Don't allocate storage, it won't be freed.
 *
 * @return Plugin
 * @see elektraPluginExport()
 * @ingroup plugin
 */
Plugin *ELEKTRA_PLUGIN_EXPORT(doc)
{
	return elektraPluginExport(DOC_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&docOpen,
		ELEKTRA_PLUGIN_CLOSE,	&docClose,
		ELEKTRA_PLUGIN_GET,	&docGet,
		ELEKTRA_PLUGIN_SET,	&docSet,
		ELEKTRA_PLUGIN_ERROR,	&docError,
		ELEKTRA_PLUGIN_END);
}

/**
 * @}
 */
