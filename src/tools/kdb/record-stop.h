/**
 * @file
 *
 * @brief Header for record-stop command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RECORD_STOP_H
#define ELEKTRA_KDB_RECORD_STOP_H

#include <kdb.h>

/**
 * Adds options specification of record-stop command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRecordStopSpec (KeySet * spec);

/**
 * Executes the record-record-stop command
 *
 * @param options cli options and arguments as specified in addRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 record-stop command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppRecordStop (int argc, char ** argv);

#endif // ELEKTRA_KDB_RECORD_STOP_H
