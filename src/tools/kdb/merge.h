/**
 * @file
 *
 * @brief Header for merge command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_MERGE_H
#define ELEKTRA_KDB_MERGE_H

#include <kdb.h>

/**
 * Adds options specification of merge command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMergeSpec (KeySet * spec);

/**
 * Executes the merge command
 *
 * @param options cli options and arguments as specified in @addMergeSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppMerge (int argc, char ** argv);

#endif // ELEKTRA_KDB_MERGE_H
