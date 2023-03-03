/**
 * @file
 *
 * @brief Header for rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RM_H
#define ELEKTRA_KDB_RM_H

#include <kdb.h>

/**
 * Adds options specification of rm command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRmSpec (KeySet * spec);

/**
 * Executes the rm command
 *
 * @param options cli options and arguments as specified in addRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 rm command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execRm (KeySet * options, Key * errorKey);

// helper functions
int getKeyNameDepth (const char * name);

#endif // ELEKTRA_KDB_RM_H
