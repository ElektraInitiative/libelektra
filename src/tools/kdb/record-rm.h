/**
 * @file
 *
 * @brief Header for record-rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RECORD_RM_H
#define ELEKTRA_KDB_RECORD_RM_H

#include <kdb.h>

/**
 * Adds options specification of record-rm command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRecordRmSpec (KeySet * spec);

/**
 * Executes the record-record-rm command
 *
 * @param options cli options and arguments as specified in addRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 record-rm command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppRecordRm (int argc, char ** argv);

#endif // ELEKTRA_KDB_RECORD_RM_H
