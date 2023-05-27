/**
 * @file
 *
 * @brief Header for namespace command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_NAMESPACE_H
#define ELEKTRA_KDB_NAMESPACE_H

#include <kdb.h>

/**
 * Adds options specification of namespace command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addNamespaceSpec (KeySet * spec);

/**
 * Executes the namespace command
 *
 * @param options cli options and arguments as specified in @addNamespaceSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ls command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppNamespace (int argc, char ** argv);

#endif // ELEKTRA_KDB_NAMESPACE_H
