/**
 * @file
 *
 * @brief Header for record-undo command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_RECORD_UNDO_H
#define ELEKTRA_KDB_RECORD_UNDO_H

#include <kdb.h>

/**
 * Adds options specification of record-undo command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addRecordUndoSpec (KeySet * spec);

/**
 * Executes the record-record-undo command
 *
 * @param options cli options and arguments as specified in addRmSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 record-undo command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execRecordUndo (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_RECORD_UNDO_H
