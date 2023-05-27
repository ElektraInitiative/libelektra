/**
 * @file
 *
 * @brief Header for meta-get command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_META_GET_H
#define ELEKTRA_KDB_META_GET_H

#include <kdb.h>

/**
 * Adds options specification of meta-get command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMetaGetSpec (KeySet * spec);

/**
 * Executes the meta-get command
 *
 * @param options cli options and arguments as specified in @addMetaGetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 get command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppMetaGet (int argc, char** argv);

#endif // ELEKTRA_KDB_META_GET_H
