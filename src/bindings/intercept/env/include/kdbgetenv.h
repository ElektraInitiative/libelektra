/**
 * @file
 *
 * @brief Header for the getenv library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef KDBGETENV_H
#define KDBGETENV_H

#include <elektra/kdb.h>


#define KDB_GETENV_VERSION "0"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * @brief The KDB repository to be used to fetch configuration
 */
extern KDB * elektraRepo;

/**
 * @brief The config of the application. Contains whole /env configuration.
 */
extern KeySet * elektraConfig;

/**
 * @brief The parentKey used to access elektraRepo.
 *
 * Contains all warnings and errors that happened during repo access
 */
extern Key * elektraParentKey;

/**
 * @brief Lock the internally used mutex to access elektraRepo, elektraConfig or elektraParentKey
 *
 * @see elektraUnlockMutex()
 *
 * @see elektraRepo, elektraConfig or elektraParentKey
 */
void elektraLockMutex ();

/**
 * @brief Unlock the internally used mutex
 *
 * @see elektraLockMutex()
 */
void elektraUnlockMutex ();

/**
 * @brief Initializes Global Elektra Repo+Config
 *
 * It is safe to call it multiple times.
 * Will automatically close using elektraClose(), if it was open.
 *
 * @param [out] argc the number of args to parse, might be reduced
 * @param [out] argv the args, might be reduced
 *
 * @see elektraClose
 */
void elektraOpen (int * argc, char ** argv);

/**
 * @brief Closes and frees Repo+Config
 *
 * It is safe to call it multiple times.
 *
 * @see elektraOpen
 */
void elektraClose ();


#ifdef __cplusplus
}
}
#endif

#endif
