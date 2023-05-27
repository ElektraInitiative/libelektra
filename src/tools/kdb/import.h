/**
 * @file
 *
 * @brief Header for import command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_IMPORT_H
#define ELEKTRA_KDB_IMPORT_H

#include <kdb.h>

/**
 * Adds options specification of import command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addImportSpec (KeySet * spec);

/**
 * Executes the import command
 *
 * @param options cli options and arguments as specified in addImportSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 import command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppImport (int argc, char** argv);

#endif // ELEKTRA_KDB_IMPORT_H

