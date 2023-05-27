/**
 * @file
 *
 * @brief Header for gen command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_GEN_H
#define ELEKTRA_KDB_GEN_H

#include <kdb.h>

/**
 * Adds options specification of gen command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addGenSpec (KeySet * spec);

/**
 * Executes the gen command
 *
 * @param options cli options and arguments as specified in addGenSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 gen command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppGen (int argc, char ** argv);

#endif // ELEKTRA_KDB_GEN_H
