/**
 * @file
 *
 * @brief Header for meta-ls command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_META_LS_H
#define ELEKTRA_KDB_META_LS_H

#include <kdb.h>

/**
 * Adds options specification of meta-ls command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMetaLsSpec (KeySet * spec);

/**
 * Executes the meta-ls command
 *
 * @param options cli options and arguments as specified in @addMetaLsSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval >0 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execMetaLs (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_META_LS_H
