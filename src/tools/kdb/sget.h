/**
 * @file
 *
 * @brief Header for sget command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_SGET_H
#define ELEKTRA_KDB_SGET_H

#include <kdb.h>

/**
 * Adds options specification of sget command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addSgetSpec (KeySet * spec);

/**
 * Executes the sget command
 *
 * @param options cli options and arguments as specified in addSgetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 sget command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppSget (int argc, char ** argv);

#endif // ELEKTRA_KDB_SGET_H
