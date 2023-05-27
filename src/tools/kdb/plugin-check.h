/**
 * @file
 *
 * @brief Header for plugin-check command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_PLUGIN_CHECK_H
#define ELEKTRA_KDB_PLUGIN_CHECK_H

#include <kdb.h>

/**
 * Adds options specification of plugin-check command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addPluginCheckSpec (KeySet * spec);

/**
 * Executes the plugin-check command
 *
 * @param options cli options and arguments as specified in addPluginCheckSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 plugin-check command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppPluginCheck (int argc, char** argv);

#endif // ELEKTRA_KDB_PLUGIN_CHECK_H

