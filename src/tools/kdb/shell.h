/**
 * @file
 *
 * @brief Header for shell command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_SHELL_H
#define ELEKTRA_KDB_SHELL_H

#include <kdb.h>

/**
 * Adds options specification of shell command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addShellSpec (KeySet * spec);

/**
 * Executes the shell command
 *
 * @param options cli options and arguments as specified in addShellSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 shell command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppShell (int argc, char ** argv);

#endif // ELEKTRA_KDB_SHELL_H
