/**
 * @file
 *
 * @brief Header for test command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_TEST_H
#define ELEKTRA_KDB_TEST_H

#include <kdb.h>

/**
 * Adds options specification of test command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addTestSpec (KeySet * spec);

/**
 * Executes the test command
 *
 * @param options cli options and arguments as specified in addTestSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 test command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppTest (int argc, char** argv);

#endif // ELEKTRA_KDB_TEST_H

