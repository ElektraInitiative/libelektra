/**
 * @file
 *
 * @brief Header for convert command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_CONVERT_H
#define ELEKTRA_KDB_CONVERT_H

#include <kdb.h>

/**
 * Adds options specification of convert command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addConvertSpec (KeySet * spec);

/**
 * Executes the convert command
 *
 * @param options cli options and arguments as specified in addConvertSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 convert command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppConvert (int argc, char ** argv);

#endif // ELEKTRA_KDB_CONVERT_H
