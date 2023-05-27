/**
 * @file
 *
 * @brief Header for export command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_EXPORT_H
#define ELEKTRA_KDB_EXPORT_H

#include <kdb.h>

/**
 * Adds options specification of export command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addExportSpec (KeySet * spec);

/**
 * Executes the export command
 *
 * @param options cli options and arguments as specified in addExportSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 export command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppExport (int argc, char ** argv);

#endif // ELEKTRA_KDB_EXPORT_H
