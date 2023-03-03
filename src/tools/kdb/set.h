/**
 * @file
 *
 * @brief KDB set subcommand header
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_SET_H
#define ELEKTRA_KDB_SET_H

#include <kdb.h>

/**
 * Adds options specification of set command to keySet
 *
 * @param spec the base spec where the commands spec should be added
 */
void addSetSpec (KeySet * spec);

/**
 * Runs the set command
 *
 * @param options cli options and arguments as specified in addSetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 set command ran without errors
 * @retval 1 errors occurred, keySetMeta (errorKey, "error/reason") for info
 *
 */
int execSet (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_SET_H
