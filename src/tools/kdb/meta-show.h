/**
 * @file
 *
 * @brief Header for meta-show command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_META_SHOW_H
#define ELEKTRA_KDB_META_SHOW_H

#include <kdb.h>

/**
 * Adds options specification of meta-show command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMetaShowSpec (KeySet * spec);

/**
 * Executes the meta-show command
 *
 * @param options cli options and arguments as specified in @addMetaShowSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 show command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppMetaShow (int argc, char** argv);

#endif // ELEKTRA_KDB_META_SHOW_H
