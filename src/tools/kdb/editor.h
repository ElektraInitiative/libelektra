/**
 * @file
 *
 * @brief Header for editor command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_EDITOR_H
#define ELEKTRA_KDB_EDITOR_H

#include <kdb.h>

/**
 * Adds options specification of editor command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addEditorSpec (KeySet * spec);

/**
 * Executes the editor command
 *
 * @param options cli options and arguments as specified in addEditorSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 editor command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppEditor (int argc, char** argv);

#endif // ELEKTRA_KDB_EDITOR_H

