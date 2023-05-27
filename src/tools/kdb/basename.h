/**
 * @file
 *
 * @brief Header for basename command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_BASENAME_H
#define ELEKTRA_KDB_BASENAME_H

#include <kdb.h>

/**
 * Adds options specification of basename command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addBasenameSpec (KeySet * spec);

/**
 * Executes the basename command
 *
 * @param options cli options and arguments as specified in @addBasenameSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppBasename (int argc, char ** argv);

#endif // ELEKTRA_KDB_BASENAME_H
