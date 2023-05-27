/**
 * @file
 *
 * @brief Header for list-tools command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_LIST_TOOLS_H
#define ELEKTRA_KDB_LIST_TOOLS_H

#include <kdb.h>

/**
 * Adds options specification of list-tools command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addListToolsSpec (KeySet * spec);

/**
 * Executes the list-tools command
 *
 * @param options cli options and arguments as specified in addListToolsSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 list-tools command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppListTools (int argc, char** argv);

#endif // ELEKTRA_KDB_LIST_TOOLS_H

