/**
 * @file
 *
 * @brief Header for mountpoint list command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_MOUNT_LIST_H
#define ELEKTRA_KDB_MOUNT_LIST_H

#include <kdb.h>

/**
 * Adds options specification of mountpoint list command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMountpointListSpec (KeySet * spec);

/**
 * Executes the mountpoint list command
 *
 * @param options cli options and arguments as specified in @addMountSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execMountpointList (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_MOUNT_LIST_H
