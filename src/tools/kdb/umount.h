/**
 * @file
 *
 * @brief Header for umount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_UMOUNT_H
#define ELEKTRA_KDB_UMOUNT_H

#include <kdb.h>

/**
 * Adds options specification of umount command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addUmountSpec (KeySet * spec);

/**
 * Executes the umount command
 *
 * @param options cli options and arguments as specified in addUmountSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 umount command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppUmount (int argc, char ** argv);

#endif // ELEKTRA_KDB_UMOUNT_H
