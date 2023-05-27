/**
 * @file
 *
 * @brief Header for record-start command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RECORD_START_H
#define ELEKTRA_KDB_RECORD_START_H

#include <kdb.h>

/**
 * Adds options specification of record-start command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRecordStartSpec (KeySet * spec);

/**
 * Executes the record-record-start command
 *
 * @param options cli options and arguments as specified in addRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 record-start command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execRecordStart (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_RECORD_START_H
