/**
 * @file
 *
 * @brief Header for cmerge command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_CMERGE_H
#define ELEKTRA_KDB_CMERGE_H

#include <kdb.h>
#include <stdbool.h>

/**
 * Adds options specification of cmerge command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addCmergeSpec (KeySet * spec);

/**
 * Executes the cmerge command
 *
 * @param options cli options and arguments as specified in @addCmergeSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCmerge (KeySet * options, Key * errorKey);

// helper functions
void printKsNames (KeySet * ks);

#endif // ELEKTRA_KDB_CMERGE_H
