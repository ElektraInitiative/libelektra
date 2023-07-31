/**
 * @file
 *
 * @brief Header for meta-rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_META_RM_H
#define ELEKTRA_KDB_META_RM_H

#include <kdb.h>

/**
 * Adds options specification of meta-rm command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMetaRmSpec (KeySet * spec);

/**
 * Executes the meta-rm command
 *
 * @param options cli options and arguments as specified in @addMetaRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 rm command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppMetaRm (int argc, char ** argv);

#endif // ELEKTRA_KDB_META_RM_H
