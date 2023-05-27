/**
 * @file
 *
 * @brief Header for find command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_FIND_H
#define ELEKTRA_KDB_FIND_H

#include <kdb.h>

/**
 * Adds options specification of find command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addFindSpec (KeySet * spec);

/**
 * Executes the find command
 *
 * @param options cli options and arguments as specified in addFindSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 find command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppFind (int argc, char** argv);

#endif // ELEKTRA_KDB_FIND_H
