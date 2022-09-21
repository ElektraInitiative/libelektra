/**
 * @file
 *
 * @brief KDB get subcommand header
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_GET_H
#define ELEKTRA_KDB_GET_H

#include <kdb.h>

/**
 * Adds options specification of get command to keySet
 *
 * @param spec the base spec where the commands spec should be added
 */
void addGetSpec (KeySet * spec);

/**
 * Runs the get command
 *
 * @param options cli options and arguments as specified in addGetSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 get command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 * @retval -1 key was not found
 *
 */
int execGet (KeySet * options, Key * errorKey);


// helper functions for the get command
const char * getCascadingName (const char * str);
Key * printTrace (KeySet * ks, Key * key, Key * found, elektraLookupFlags options);
Key * warnOnMeta (KeySet * ks, Key * key, Key * found, elektraLookupFlags options);
void printOptions (elektraLookupFlags options);
void setCallback (Key * key, Key * (*f) (KeySet * ks, Key * key, Key * found, elektraLookupFlags flags));


#endif // ELEKTRA_KDB_GET_H
