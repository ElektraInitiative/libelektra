/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef DOC_H
#define DOC_H

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
 * <a href="doc_tutorials_plugins_md.html">Plugin Tutorial</a>.
 *
 * A plugin can implement any functionality related to
 * configuration.
 * There are 6 possible entry points for a plugin.
 * - elektraDocGet() will be called when configuration or the plugin's
 *   contract is retrieved from the key database
 * - elektraDocSet() will be called when configuration is written to the
 *   key database
 * - elektraDocOpen() will be called before any other method of the
 *   plugin is called
 * - elektraDocClose() will be called as last method
 * - elektraDocError() will be called when kdbSet() failed (to give the
 *   plugin a chance to recover/undo its actions)
 * - elektraPluginExport() exports all methods for the plugin.
 *
 * Additionally, make sure that you write a contract in the README.md.
 * It is used by the build system and the mounting tools.
 *
 * Plugins should not change the keyname of the key that is passed to
 * the entry points (warningsKey and parentKey in this documentation).
 * These keys might be members in keysets.
 *
 * The names described here contain "Doc" within the method's name
 * just because the plugin described in this document is called doc
 * (the doxygen source was generated from src/plugins/doc/doc.h).
 * Always replace Doc with the name of the plugin you
 * are going to implement or use #ELEKTRA_PLUGIN_FUNCTION.
 *
 *
 * @par Overview
 * There are different types of plugins for different concerns.
 * They all only have the entry points as defined above.
 * The types of plugins handled in this document:
 * - A storage plugin gets an empty keyset in elektraDocGet()
 *   and constructs the information out from a file.
 *   In elektraDocSet() the keyset is written to a file.
 *   \n
 *   Other persistent storage then a file is not handled within
 *   this document because it involves many other issues.
 *   For files the resolver plugin already takes care of
 *   transactions and rollback.
 *   So the storage plugin is the source and dump as known
 *   from pipes and filters.
 * - A filter plugin is a plugin which operates on existing keys.
 *   It may process or change the keyset.
 *   Or it may reject specific keysets which do not meet some
 *   criteria.
 *
 * Use following include to have the functions that are not
 * implemented by you available:
 *
 * @snippet doc.c plugin include
 *
 * @par Error and Warnings
 * In case of trouble, in some methods you can use
 * the macro #ELEKTRA_SET_ERROR (in other methods it is not allowed).
 * You might add warnings with the macro #ELEKTRA_ADD_WARNING.
 * You can also use their pedants that accept a format string
 * as known by printf: #ELEKTRA_SET_ERRORF and #ELEKTRA_ADD_WARNINGF.
 * Make sure to define and use a macro in the error specification
 * (/src/error/specification) so that you can easily renumber
 * your error/warning codes:
 *
 * @snippet doc.c plugin errors spec
 *
 * Use following include to have the macros for setting the error
 * and adding the warnings available:
 *
 * @snippet doc.c plugin errors include
 *
 * and then you can use:
 *
 * @snippet doc.c plugin errors usage
 *
 * Note that you also need to return -1 in the case of error.
 * See individual description of entry points to implement below.
 *
 * @par Global KeySet Handle
 *
 * This keyset allows plugins to exchange information with other plugins.
 *
 * The keyset is initialized by the KDB for all plugins, except for manually
 * created plugins with `elektraPluginOpen()`. The global keyset is
 * tied to a KDB handle, initialized on `kdbOpen()` and deleted on `kdbClose()`.
 *
 * Obtain a handle to the global keyset and work with it:
 *
 * @snippet doc.c get global keyset
 *
 * Clean up keys which you do not need any more,
 * to keep the global keyset compact:
 *
 * @snippet doc.c get global keyset cleanup
 *
 * @par Further help
 * Do not hesitate to open an issue if anything
 * is unclear.
 *
 * @addtogroup plugin
 * @{
 */


/**
 * @brief Sets the error in the keys metadata.
 *
 * Include kdberrors.h to make it work.
 * Only a single error can be written to the key.
 *
 * @snippet doc.c plugin errors include
 *
 * @ingroup plugin
 *
 * @param number the error number from src/error/specification
 * @param key to write the error to
 * @param text additional text for the user
 */
#define ELEKTRA_SET_ERROR(number, key, text)

/**
 * @brief Sets the error in the keys metadata.
 *
 * Include kdberrors.h to make it work.
 * Only a single error can be written to the key.
 *
 * @snippet doc.c plugin errors include
 *
 * @ingroup plugin
 *
 * @param number the error number from src/error/specification
 * @param key to write the error to
 * @param formatstring a format string as in printf
 * @param ... further arguments as in printf
 */
#define ELEKTRA_SET_ERRORF(number, key, formatstring, ...)

/**
 * @brief Adds a warning in the keys metadata.
 *
 * Include kdberrors.h to make it work:
 *
 * @snippet doc.c plugin errors include
 *
 * @ingroup plugin
 *
 * @param number the warning number from src/error/specification
 * @param key to write the error to
 * @param formatstring a format string as in printf
 * @param ... further arguments as in printf
 */
#define ELEKTRA_ADD_WARNINGF(number, key, formatstring, ...)

/**
 * @brief Adds a warning in the keys metadata.
 *
 * Include kdberrors.h to make it work:
 *
 * @snippet doc.c plugin errors include
 *
 * @ingroup plugin
 *
 * @param number the warning number from src/error/specification
 * @param key to write the error to
 * @param text additional text for the user
 */
#define ELEKTRA_ADD_WARNING(number, key, text)


/**
 * @brief Set error in kdbGet() when opening the file failed
 *
 * Assumes that error reason is in `errno`.
 *
 * @param parentKey key to append error to
 *
 * To use it include:
 *
 * @snippet doc.c plugin include
 * @snippet doc.c plugin errors include
 *
 * @return
 */
#define ELEKTRA_SET_ERROR_GET(parentKey)


/**
 * @brief Set error in kdbSet() when opening the file failed
 *
 * Assumes that error reason is in `errno`.
 *
 * @param parentKey key to append error to
 *
 * To use it include:
 *
 * @snippet doc.c plugin include
 * @snippet doc.c plugin errors include
 *
 * @return
 */
#define ELEKTRA_SET_ERROR_SET(parentKey)


// undef everything, is included later
#undef ELEKTRA_SET_ERROR
#undef ELEKTRA_SET_ERRORF
#undef ELEKTRA_ADD_WARNINGF
#undef ELEKTRA_ADD_WARNING
#undef ELEKTRA_SET_ERROR_GET
#undef ELEKTRA_SET_ERROR_SET

#include <elektra/plugin/plugin.h>


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
 * If your plugin has no useful way to startup without config, the
 * module loader would not be able to load the module.
 * We need, however, to still load the plugin to get the contract.
 *
 * To solve that problem the module loader adds the configuration key
 * /module. Even if your plugin is basically not able to startup
 * successfully, it should still provide a fallback when /module
 * is present, so that elektraDocGet() on system:/elektra/modules can be
 * called successfully later on.
 *
 * @snippet doc.c doc module
 *
 * Note that for plugins where the contract will be altered based
 * on configuration this specific configuration should be considered.
 * In fact the idea of /module is to get the correct contract.
 *
 *
 * @retval -1 on error, your plugin will be removed then and the missing
 * plugin added instead.
 * Use #ELEKTRA_ADD_WARNING to indicate the problem. The system will
 * automatically add the information that the plugin was removed,
 * so you do not need the user give that information.
 *
 * @retval 1 on success
 *
 * @param handle contains internal information of the plugin
 * @param warningsKey can be used to add warnings with the macro
 *        #ELEKTRA_ADD_WARNING (Do not add errors!)
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @see elektraDocClose()
 * @ingroup plugin
 */
int elektraDocOpen (Plugin * handle, Key * warningsKey);


/**
 * @brief Finalize the plugin.
 *
 * Called prior to unloading the plugin dynamic module.
 * After this function is called, it is ensured that no
 * functions from your plugin will ever be accessed again.
 *
 * Make sure to free all memory that your plugin requested at runtime.
 * Also make sure to free what you stored by elektraPluginSetData()
 * before.
 *
 * So for the Doc plugin we need to:
 *
 * @snippet doc.c doc close
 *
 * After this call, libelektra.so will unload the plugin library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @param handle contains internal information of the plugin
 * @param warningsKey can be used to to add warnings using
 *        #ELEKTRA_ADD_WARNING (Do not add errors!)
 *
 * @retval 1 on success (no other return value currently allowed)
 *
 * @retval -1 on problems (only use ELEKTRA_ADD_WARNING, but never
 * set an error).
 *
 * @see kdbClose()
 * @see elektraPluginGetData(), elektraPluginSetData() and
 *      elektraPluginGetConfig()
 * @ingroup plugin
 */
int elektraDocClose (Plugin * handle, Key * warningsKey);

/**
 * @brief Get data from storage to application.
 *
 * Retrieve information from a permanent storage to construct
 * a keyset.
 *
 * @section intro Introduction
 *
 * The elektraDocGet() function handle everything related
 * to receiving keys.
 *
 * @subsection contract Contract Handling
 *
 * The contract is a keyset that needs to be returned if the parentKey
 * is system:/elektra/modules/yourpluginname.
 *
 * Which keys and their meaning is specified in doc/CONTRACT.ini
 *
 * Here is an example for our doc plugin:
 *
 * @snippet doc.c get contract
 *
 * Some clauses of the contract, especially the description of the
 * plugin can be done more conveniently directly in a README.md
 * that is included by #ELEKTRA_README.
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
 *
 * @snippet doc.c get storage
 *
 * When opening files, make sure to use the macros #ELEKTRA_SET_ERROR_SET
 * and #ELEKTRA_SET_ERROR_GET for errors, as shown here:
 *
 * @snippet doc.c opening files
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
 * @snippet doc.c get filter
 *
 *
 * @pre The caller kdbGet() will make sure before you are called
 * that the parentKey:
 * - is a valid key (means that it is a system or user key).
 * - is your mountpoint and that your plugin is responsible for it.
 * @pre and that the returned:
 * - is a valid keyset.
 * - your plugin is only called when needed (e.g. only if file was modified)
 * - has @p all keys related to your plugin.
 * - contains only valid keys below (see keyIsBelow()) your parentKey.
 * - is in a sorted order (given implicit by KeySet)
 * @pre and that the handle:
 *  - is valid for your plugin.
 *  - that elektraPluginGetData() contains the same handle for lifetime
 *    of your plugin.
 *
 * @pre The caller kdbGet() will make sure that afterwards you were
 * called, that:
 * - other plugins below your plugin will be called again recursively.
 * - that all keys are merged to one keyset the user gets
 * - that all keys (that should not be removed) are passed to
 *   kdbSet() if writing to disc is needed.
 *
 * @invariant There are no global variables and elektraPluginSetData()
 *  stores all information.
 *  The handle is to be guaranteed to be the same if it is the same plugin.
 *
 * @post The keyset @p returned has the @p parentKey and all keys
 * below (keyIsBelow()) with all information from the storage.
 * Make sure to return all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbGet() will
 * drop these keys with additional plugins.
 *
 * @par Updating
 * To get all keys out of the storage over and over again can be very inefficient.
 * You might know a more efficient method to know if the key needs update or not,
 * e.g. by stating it or by an external time stamp info.
 * For file storage plugins this is automatically done for you by the
 * resolver.
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
 * @retval 1 on success
 * @retval 0 when nothing was to do
 * @retval -1 on failure, the current key in returned shows the position.
 *         use #ELEKTRA_SET_ERROR of kdberrors.h to define the error code.
 *         You additionally can add as many warnings as you would like
 *         to add.
 *
 * @ingroup plugin
 */
int elektraDocGet (Plugin * handle, KeySet * returned, Key * parentKey);

/**
 * @brief Set data from application to storage.
 *
 * This function does everything related to set and remove keys in a
 * plugin. There is only one function for that purpose to make
 * implementation of file based plugins much easier.
 *
 * The keyset @p returned was filled in with information from the application
 * using elektra and the task of this function is to store it in a permanent
 * way so that a subsequent call of elektraPluginGet() can rebuild the keyset
 * as it was before. See the live cycle to understand:
 *
 * @snippet doc.c set full
 *
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
 *   Set an error using #ELEKTRA_SET_ERROR to inform the user what went
 *   wrong.
 *   Additionally you can add any number of warnings with
 *   #ELEKTRA_ADD_WARNING.
 *
 * @ingroup plugin
 */
int elektraDocSet (Plugin * handle, KeySet * returned, Key * parentKey);

/**
 * @brief Make changes to storage final.
 *
 * Once the content of @p returned has been stored, the changes need
 * to be made final and visible to other users, which is done by this function. After this function
 * has been called, no further changes can be made by elektraPluginSet() functions within this invocation of kdbSet().
 *
 * The function is called by kdbSet() if the plugin implementing it fulfills the `commit` role.
 *
 * @pre The keyset @p returned holds all stored keys which must be made final for this keyset.
 * The keyset is sorted and rewinded.
 *
 * @pre The @p parentKey is the key which is the ancestor for all other keys in the
 * keyset. The first key of the keyset @p returned has the same keyname.
 * The name of the parentKey marks the mountpoint.
 *
 * @post the storage changes made by the plugins previously called by kdbSet() will be made final.
 *
 * @see kdbSet() for caller.
 *
 * @param handle contains internal information of the plugin
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the location of the relevant keys within the key database.
 *
 * @retval 1 on success
 * @retval 0 on success without any changes
 * @retval -1 on failure. The cause of the error needs to be entered into parentKey.
 * The error can be specified using #ELEKTRA_SET_ERROR. #ELEKTRA_ADD_WARNING can be used to
 * add warnings for the user.
 *
 * @ingroup plugin
 */
int elektraDocCommit (Plugin * handle, KeySet * returned, Key * parentKey);

/**
 * @brief Rollback in case of errors.
 *
 * First for all plugins elektraDocSet() will be called.
 * If any plugin had problems before the commit (done by the
 * resolver plugin), we can safely rollback our changes.
 *
 * This method is rarely used by plugins, it is mainly used for
 * resolvers (to implement rollback) or by logging plugins.
 * It is not needed for storage plugins, because they only operate
 * on temporary files created by the resolver.
 *
 * @param handle contains internal information of the plugin
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys.
 *        can be used to add warnings with the macro
 *        #ELEKTRA_ADD_WARNING, but do not add errors!
 *
 * @retval 1 on success
 * @retval 0 on success with no action
 * @retval -1 on failure (you can add warnings, but we are already
 * in an error state, so do not set the error).
 *
 * @ingroup plugin
 */
int elektraDocError (Plugin * handle, KeySet * returned, Key * parentKey);

/**
 * @brief Validate plugin configuration at mount time.
 *
 * During the mount phase the BackendBuilder calls this method,
 * if it is provided by the plugin.
 *
 * In this method the plugin configuration can be checked for validity or
 * integrity. Missing items can be added to complete the configuration.
 *
 * @param errorKey is used to propagate error messages to the caller
 * @param conf contains the plugin configuration to be validated
 *
 * @retval 0 on success: the configuration was OK and has not been changed.
 * @retval 1 on success: the configuration has been changed and now it is OK.
 * @retval -1 on failure: the configuration was not OK and could not be fixed.
 *   Set an error using #ELEKTRA_SET_ERROR to inform the user what went wrong.
 *   Additionally you can add any number of warnings with
 *   #ELEKTRA_ADD_WARNING.
 *
 * @ingroup plugin
 */
int elektraDocCheckConf (Key * errorKey, KeySet * conf);

/**
 * @}
 */

#endif
