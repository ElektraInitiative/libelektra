/**
 * @file
 *
 * @brief Header for cp command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_CP_H
#define ELEKTRA_KDB_CP_H

#include <kdb.h>

/**
 * Adds options specification of cp command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addCpSpec (KeySet * spec);

/**
 * Executes the cp command
 *
 * @param options cli options and arguments as specified in addCpSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 cp command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppCp (int argc, char** argv);

#endif // ELEKTRA_KDB_CP_H
