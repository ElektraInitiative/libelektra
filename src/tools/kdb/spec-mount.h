/**
 * @file
 *
 * @brief Header for spec-mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_SPEC_MOUNT_H
#define ELEKTRA_KDB_SPEC_MOUNT_H

#include <kdb.h>

/**
 * Adds options specification of spec-mount command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addSpecMountSpec (KeySet * spec);

/**
 * Executes the spec-mount command
 *
 * @param options cli options and arguments as specified in addSpecMountSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 spec-mount command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppSpecMount (int argc, char** argv);

#endif // ELEKTRA_KDB_SPEC_MOUNT_H

