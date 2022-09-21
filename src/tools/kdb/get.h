/**
 * @file
 *
 * @brief KDB get subcommand header
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_GET_H
#define ELEKTRA_KDB_GET_H

#include <kdb.h>

/**
 * Adds options specification of get command to keySet
 *
 * @param spec the base spec where the commands spec should be added
 */
void addGetSpec (KeySet * spec);

/**
 * Runs the get command
 *
 * @param options cli options and arguments as specified in addGetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 get command ran without errors
 * @retval !0 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 * @retval 11 key was not found
 *
 */
int execGet (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_GET_H
