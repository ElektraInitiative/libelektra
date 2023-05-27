/**
 * @file
 *
 * @brief Header for record-export command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RECORD_RESET_H
#define ELEKTRA_KDB_RECORD_RESET_H

#include <kdb.h>

/**
 * Adds options specification of record-reset command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRecordResetSpec (KeySet * spec);

/**
 * Executes the record-reset command
 *
 * @param options cli options and arguments as specified in addExportSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 record-reset command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppRecordReset (int argc, char ** argv);

#endif // ELEKTRA_KDB_RECORD_RESET_H
