/**
 * @file
 *
 * @brief Loading Modules for Elektra documentation.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */

/**
 * @defgroup modules Modules
 *
 * @brief Loading Modules for Elektra.
 *
 * Unfortunately there is no portable way to load modules, plugins or libraries.
 * So Elektra needed a framework which abstracts the loading of modules.
 * Depending of the operating system the build system chooses different
 * source files which actually implement the loading of a module.
 *
 * The goals are:
 * - to have a list of all loaded modules
 * - writing module loaders should be easy
 * - handle and report errors well
 * - avoid loading of modules multiple times (maybe the OS can't handle that well)
 * - hide the OS dependent handle inside a Key (handle is needed to
 *   close module afterwards)
 */

/**
 * Initialises the module loading system.
 *
 * Most operating systems will have to do nothing here.
 * Anyway you are required to add the key system:/elektra/modules
 * if it was successful.
 *
 * On error -1 is returned and if error != 0 error information
 * is added to it.
 *
 * @param modules an empty keyset
 * @param error a key to append the error information if it is not null
 * @return -1 on error
 * @return >=0 otherwise
 * @ingroup modules
 * @post the key system:/elektra/modules is added in case of success
 */
int elektraModulesInit (KeySet * modules, Key * error)
{
	return -1;
}

/**
 * Load a library with the given name.
 *
 * @return a pointer to the factory which can create the plugin.
 *
 * Make sure that you first lookup if this module was already loaded.
 * If it was, just return the pointer and you are done.
 *
 * Otherwise load the module/library given by name. You need to take care that a proper
 * name is used. The name does not have any path, pre- or postfixes.
 *
 * The next step is to fetch the symbol elektraPluginFactory.
 *
 * If everything was successful append all information to the keyset modules
 * and return the pointer. Take care that you can close the module with that
 * information. All information needs to be stored within
 * system:/elektra/modules/name
 * You might want to use an struct and store it there as binary key.
 *
 * If anything goes wrong don't append anything to modules. Instead
 * report the error to the error key and return with 0.
 *
 * @pre the name is not null, empty and has at least one character
 *      different to '/'. It is suitable to be used as keyAddBaseName
 *      without any further error checking.
 *
 * @param modules where to get existing modules from
 *        a new module will be added there
 * @param name the name for the plugin to load. Note that it does
 *        not have any prefixes or postfixes, you need to add them
 *        yourself.
 * @param error the key to add warnings or report errors
 * @return a pointer which can create a Plugin
 * @return 0 on error
 * @ingroup modules
 */
elektraPluginFactory elektraModulesLoad (KeySet * modules, const char * name, Key * error)
{
	return 0;
}

/**
 * Close all modules.
 *
 * Iterates over all modules and closes each of them.
 *
 * Finish all affairs with the modules. Delete all keys
 * where the appropriate module could be closed.
 *
 * If it is not possible to close a module, still try to
 * close all other modules, but report the error with the
 * error key.
 *
 * @param modules all modules in this keyset will be closed
 * @param error a key to append the error information if it is not null
 * @return -1 on error
 * @return >=0 otherwise
 * @ingroup modules
 * @post all error information is stored in the key 'error'
 */
int elektraModulesClose (KeySet * modules, Key * error)
{
	return -1;
}
