/**
* @file
*
* @brief Header for mount command
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_MOUNTPOINT_H
#define ELEKTRA_KDB_MOUNTPOINT_H

#include <kdb.h>

#define MOUNTPOINTS_PATH "system:/elektra/mountpoints"

/**
 * Adds options specification of mountpoint command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMountpointSpec (KeySet * spec);

/**
 * Executes the mount command
 *
 * @param options cli options and arguments as specified in @addMountpointSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execMountpoint (KeySet * options, Key * errorKey);

// helper functions
KeySet * getMountConfig(KDB * handle, Key * );

#endif // ELEKTRA_KDB_MOUNTPOINT_H