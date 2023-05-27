/**
 * @file
 *
 * @brief Header for file command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_FILE_H
#define ELEKTRA_KDB_FILE_H

#include <kdb.h>

/**
 * Adds options specification of file command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addFileSpec (KeySet * spec);

/**
 * Executes the file command
 *
 * @param options cli options and arguments as specified in addFileSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 file command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppFile (int argc, char ** argv);

#endif // ELEKTRA_KDB_FILE_H
