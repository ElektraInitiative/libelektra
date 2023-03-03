/**
 * @file
 *
 * @brief Header for mv command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_MV_H
#define ELEKTRA_KDB_MV_H

#include <kdb.h>

/**
 * Adds options specification of mv command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addMvSpec (KeySet * spec);

/**
 * Executes the mv command
 *
 * @param options cli options and arguments as specified in addMvSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 mv command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execMv (KeySet * options, Key * errorKey);

// helper functions
int getKeyNameDepth (const char * name);

#endif // ELEKTRA_KDB_MV_H
