/**
* @file
*
* @brief Header for remount command
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#ifndef ELEKTRA_KDB_REMOUNT_H
#define ELEKTRA_KDB_REMOUNT_H

#include <kdb.h>

/**
 * Adds options specification of remount command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRemountSpec (KeySet * spec);

/**
 * Executes the remount command
 *
 * @param options cli options and arguments as specified in @addRemountSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execRemount (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_REMOUNT_H
