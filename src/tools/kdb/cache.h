/**
 * @file
 *
 * @brief Header for cache command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_CACHE_H
#define ELEKTRA_KDB_CACHE_H

#include <kdb.h>

/**
 * Adds options specification of cache command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void addCacheSpec (KeySet * spec);

/**
 * Executes the cache command
 *
 * @param options cli options and arguments as specified in addCacheSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 cache command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int execCppCache (int argc, char ** argv);

#endif // ELEKTRA_KDB_CACHE_H
