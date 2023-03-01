/**
 * @file
 *
 * @brief Header for meta-set command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_META_SET_H
#define ELEKTRA_KDB_META_SET_H

#include <kdb.h>

/**
 * Adds options specification of meta-set command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMetaSetSpec (KeySet * spec);

/**
 * Executes the meta-set command
 *
 * @param options cli options and arguments as specified in @addMetaSetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 set command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execMetaSet (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_META_SET_H
