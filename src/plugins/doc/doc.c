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



/**
 * @brief Sets the error in the keys metadata.
 *
 * Include kdberrors.h to make it work
 *
 * @ingroup plugin
 *
 * @param number the error number from src/liberror/specification
 * @param key to write the error to
 * @param text additional text for the user
 */
#define ELEKTRA_SET_ERROR(number, key, text)
#undef ELEKTRA_SET_ERROR

/**
 * @brief Sets the error in the keys metadata.
 *
 * Include kdberrors.h to make it work
 *
 * @ingroup plugin
 *
 * @param number the error number from src/liberror/specification
 * @param key to write the error to
 * @param formatstring a format string as in printf
 * @param ... further arguments as in printf
 */
#define ELEKTRA_SET_ERRORF(number, key, formatstring, ...)
#undef ELEKTRA_SET_ERRORF

/**
 * @brief Adds an warning in the keys metadata.
 *
 * Include kdberrors.h to make it work
 *
 * @ingroup plugin
 *
 * @param number the warning number from src/liberror/specification
 * @param key to write the error to
 * @param formatstring a format string as in printf
 * @param ... further arguments as in printf
 */
#define ELEKTRA_ADD_WARNINGF(number, key, formatstring, ...)
#undef ELEKTRA_ADD_WARNINGF

/**
 * @brief Adds an warning in the keys metadata.
 *
 * Include kdberrors.h to make it work
 *
 * @ingroup plugin
 *
 * @param number the warning number from src/liberror/specification
 * @param key to write the error to
 * @param text additional text for the user
 */
#define ELEKTRA_ADD_WARNING(number, key, text)
#undef ELEKTRA_ADD_WARNING

/**
 * @brief Declare a plugin's function name suitable for
 * compilation variants (see doc/tutorials).
 *
 * It can be used in the same way as #ELEKTRA_PLUGIN_EXPORT.
 * @see ELEKTRA_PLUGIN_EXPORT
 *
 * @param plugin the name of the plugin
 * @param which which function it is (open, close, get, set, error)
 */
#define ELEKTRA_PLUGIN_FUNCTION(plugin, which)
#undef ELEKTRA_PLUGIN_FUNCTION


//! [plugin_include]
#include <kdbplugin.h>
#include <kdberrors.h>
//! [plugin_include]

#include <stdlib.h>

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif


//! [global data]
typedef struct { int global; } GlobalData;
//! [global data]


#define DOC_PLUGIN_NAME "doc"
#define DOC_PLUGIN_VERSION "1.0.0"


/**
 * @defgroup plugin Plugins
 * 
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
 * To get started with writing plugins, first read our
 * plugin tutorial in doc/tutorials!
 *
 * A plugin can implement any functionality related to
 * configuration.
 * There are 5 possible entry points for a plugin.
 * - elektraDocGet() will be called when configuration or the plugin's
 *   contract is retrieved from the key database
 * - elektraDocSet() will be called when configuration is written to the
 *   key database
 * - elektraDocOpen() will be called before any other method of the
 *   plugin is called
 * - elektraDocClose() will be called as last method
 * - elektraDocError() will be called when kdbSet() failed (to give the
 *   plugin a chance to recover/undo its actions)
 *
 * The names described here contain "Doc" within the method's name
 * just because the plugin
 * described here is called doc (see src/plugins/doc/doc.c).
 * Always replace Doc with the name of the plugin you
 * are going to implement or use #ELEKTRA_PLUGIN_FUNCTION
 * if you need
 * <a href="md_compilation_variants.html">compilation variants</a>.
 *
 *
 * @par Overview
 * There are different types of plugins for different concerns.
 * The types of plugins handled in this document:
 * - A storage plugin gets an empty keyset in elektraDocGet()
 *   and constructs the information out from a file.
 *   In elektraDocSet() the keyset is written to a file.
 *   \n
 *   Other persistent storage then a file is not handled within
 *   this document because it involves many other issues.
 *   For files the resolver plugin already takes care for
 *   transactions and rollback.
 * - A filter plugin is a plugin which already receives some keys.
 *   It may process or change the keyset.
 *   Or it may reject specific keysets which do not meet some
 *   criteria.
 *
 * @par Error and Warnings
 * In case of trouble, in some methods you can use
 * the macro #ELEKTRA_SET_ERROR (in other methods it is not allowed).
 * You might add warnings with the macro #ELEKTRA_ADD_WARNING.
 * Read the documentation of the individual methods to
 * decide what you should do.
 *
 * @see http://www.libelektra.org/ftp/elektra/thesis.pdf
 *   for an detailed explanation and description of other types
 *   of plugins or ask at the mailinglist if something is unclear.
 *
 * @addtogroup plugin
 * @{
 */

/**
 * @brief Initialize data for the plugin.
 *
 * This is the first method called after dynamically loading
 * this plugin. It is guaranteed, that this method will be called
 * before any other method.
 *
 * This method is responsible for:
 * - plugin's specific configuration gathering
 * - initialization of all plugin's internal structs
 * - initial setup of all I/O details such as opening a file, connecting to a
 *   database, setup connection to a server, iff this cannot be done per
 *   invocation in elektraDocGet() and elektraDocSet().
 *
 * You may also read the configuration you can get with elektraPluginGetConfig() and transform it
 * into other structures used by your plugin.
 *
 * @note The plugin must not have any global variables. If you have one
 *       Elektra will not be threadsafe.
 *       Do not assume that your plugin will be opened
 *       only once or will not be reopened at a later time.
 *
 * Instead of global variables the methods elektraPluginGetData()
 * and elektraPluginSetData() exist to store
 * and get any information related to your plugin.
 *
 * The correct substitute for global variables will be:
 *
 * @snippet doc.c global data
 *
 * and then initialize it using:
 *
 * @snippet doc.c doc open
 *
 * Make sure to free everything you allocate within elektraDocClose().
 *
 * @see elektraDocClose()
 *
 * If your plugin has no useful way to startup without config, the
 * module loader would not be able to load the module.
 * To solve that problem the module loader adds the configuration key
 * /module. Even if your plugin is basically not able to startup
 * successfully, it should still provide a fallback when /module
 * is present, so that elektraDocGet() on system/elektra/modules can be
 * called successfully later on.
 *
 * @snippet doc.c doc module
 *
 *
 * @retval -1 on error, your plugin will be removed then and the missing
 * plugin added instead.
 * Use #ELEKTRA_ADD_WARNING to indicate the problem. The system will
 * automatically add the information that the plugin was removed,
 * so you do not need the user give that information.
 *
 * @retval 0 on success
 *
 * @param handle contains internal information of the plugin
 * @param warningsKey can be used to add warnings with the macro
 *        #ELEKTRA_ADD_WARNING (Do not add errors!)
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @ingroup plugin
 */
int elektraDocOpen(Plugin *handle, Key *warningsKey);

//! [doc open]
int elektraDocOpen(Plugin *handle, Key *warningsKey ELEKTRA_UNUSED)
{
	GlobalData *data;
	KeySet *config = elektraPluginGetConfig(handle);
	Key * kg = ksLookupByName(config, "/global", 0);

	data=malloc(sizeof(GlobalData));
	data->global = 0;
	if (kg) data->global = atoi(keyString(kg));
	elektraPluginSetData(handle,data);
//! [doc open]

//! [doc module]
	if (ksLookupByName(config, "/module", 0))
	{
		return 0;
	}
	// do some setup that will fail without configuration
//! [doc module]

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
 * @param warningsKey can be used to to add warnings using
 *        #ELEKTRA_ADD_WARNING (Do not add errors!)
 *
 * @retval 0 on success (no other return value currently allowed)
 *
 * @snippet doc.c doc close
 *
 * @retval -1 on problems (only use ELEKTRA_ADD_WARNING, but never
 * set an error).
 *
 * @see kdbClose()
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @ingroup plugin
 */
int elektraDocClose(Plugin *handle, Key *warningsKey);

#ifdef DOX
#endif

//! [doc close]
int elektraDocClose(Plugin *handle, Key *warningsKey ELEKTRA_UNUSED)
{
	free (elektraPluginGetData(handle));

	return 0; /* success */
}
//! [doc close]


#ifdef DOX
#endif

/**
 * Retrieve information from a permanent storage to construct
 * a keyset.
 *
 * @section intro Introduction
 *
 * The elektraDocGet() function handle everything related
 * to receiving keys.
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
 * - is your mountpoint and that your plugin is responsible for it.
 * and that the returned:
 * - is a valid keyset.
 * - your plugin is only called when needed (e.g. only if file was modified)
 * - has @p all keys related to your plugin.
 * - contains only valid keys direct below (see keyIsBelow()) your parentKey.
 * - is in a sorted order (given implicit by semantics of KeySet)
 * and that the handle:
 *  - is valid for your plugin.
 *  - that elektraPluginGetData() contains the same handle for lifetime
 *    of your plugin until elektraPluginClose() was called.
 *
 * @pre The caller kdbGet() will make sure that afterwards you were called,
 * whenever the user requested it with the options, that:
 * - hidden keys they will be thrown away.
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
 *         use #ELEKTRA_SET_ERROR of kdberrors.h to define the error code
 *
 * @ingroup plugin
 */
int elektraDocGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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
 * implementation much easier.
 *
 * The keyset @p returned was filled in with information from the application
 * using elektra and the task of this function is to store it in a permanent
 * way so that a subsequent call of elektraPluginGet() can rebuild the keyset
 * as it was before. See the live cycle to understand:
 * @code
void usercode (Key *key)
{
	keySetString (key, "mycomment"); // the user changes the key
	ksAppendKey(keyset, key); // append the key to the keyset
	kdbSet (handle, keyset, 0, 0); // and syncs it to disc
}

// so now kdbSet is called
int kdbSet(KDB *handle, KeySet *keyset, Key *parentKey)
{
	// find appropriate plugin and then call it:
	elektraPluginSet (handle, keyset, 0);
	// the keyset with the key (and others for this plugin)
	// will be passed to this function
}

// so now elektraPluginSet(), which is the function described here,
// is called:
elektraPluginSet(Plugin *plugin, KeySet *keyset, Key *parentKey)
{
	// the task of elektraPluginSet is now to store the keys
	Key *key = ksCurrent (keyset);
	savetodisc (key);
}
 * @endcode
 * Of course all information of every key in the keyset
 * @p returned need to be stored permanently. So this specification needs to give
 * an exhaustive list of information present in a key.
 *
 * @pre The keyset @p returned holds all keys which must be saved
 * permanently for this keyset. The keyset is sorted and rewinded.
 *
 * @pre The @p parentKey is the key which is the ancestor for all other keys in the
 * keyset. The first key of the keyset @p returned has the same keyname.
 * The name of the parentKey marks the mountpoint.
 * The string of the parentKey is the filename to write to.
 *
 * Make sure to set all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbSet()
 * and plugins will sort them out.
 *
 * @invariant There are no global variables, but instead
 *  elektraPluginGetData() will be used.
 *  The handle is the same when it is the same plugin.
 *
 * @post The information of the keyset @p returned is stored permanently.
 *
 * @see kdbSet() for caller.
 *
 * @param handle contains internal information of the plugin
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 *    (name is mountpoint your plugin is mounted, string is the
 *    file to write to)
 *
 * @return When everything works gracefully return the number of keys you set.
 * The cursor position and the keys remaining in the keyset are not important.
 *
 * @retval 1 on success
 * @retval 0 on success with no changed key in database
 * @retval -1 on failure. The cause of the error needs to be added in parentKey
 *   You also have to make sure that ksGetCursor()
 *   shows to the position where the error appeared.
 *   Set an error using #ELEKTRA_SET_ERROR to inform the user what went
 *   wrong.
 *
 * @ingroup plugin
 */
int elektraDocSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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
int elektraDocError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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
		ELEKTRA_PLUGIN_OPEN,	&elektraDocOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDocClose,
		ELEKTRA_PLUGIN_GET,	&elektraDocGet,
		ELEKTRA_PLUGIN_SET,	&elektraDocSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDocError,
		ELEKTRA_PLUGIN_END);
}

/**
 * @}
 */
