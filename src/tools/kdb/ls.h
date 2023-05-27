/**
* @file
*
* @brief Header for ls command
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#ifndef ELEKTRA_KDB_LS_H
#define ELEKTRA_KDB_LS_H

#include <kdb.h>

/**
* Adds options specification of ls command to @spec
*
* @param spec the base spec where the commands spec should be added
*/
void addLsSpec (KeySet * spec);

/**
* Executes the ls command
*
* @param options cli options and arguments as specified in addLsSpec()
* @param errorKey key where errors and warnings should be saved
*
* @retval 0 ls command ran without errors
* @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
*
*/
int execCppLs (int argc, char** argv);

#endif // ELEKTRA_KDB_LS_H
