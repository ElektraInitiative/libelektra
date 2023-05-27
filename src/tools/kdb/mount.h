/**
 * @file
 *
 * @brief Header for mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_MOUNT_H
#define ELEKTRA_KDB_MOUNT_H

#include <kdb.h>

/**
 * Adds options specification of mount command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMountSpec (KeySet * spec);

/**
 * Executes the mount command
 *
 * @param options cli options and arguments as specified in addMountSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 mount command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppMount (int argc, char** argv);

#endif // ELEKTRA_KDB_MOUNT_H

