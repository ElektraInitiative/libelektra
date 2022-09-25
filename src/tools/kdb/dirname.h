/**
 * @file
 *
 * @brief Header for dirname command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_DIRNAME_H
#define ELEKTRA_KDB_DIRNAME_H

#include <kdb.h>

/**
 * Adds options specification of dirname command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addDirnameSpec (KeySet * spec);

/**
 * Executes the dirname command
 *
 * @param options cli options and arguments as specified in @addDirnameSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execDirname (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_DIRNAME_H
