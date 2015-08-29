/**
 * \file
 *
 * \brief Header for the getenv library
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef KDBGETENV_H
#define KDBGETENV_H

#include <kdb.h>

#define KDB_GETENV_VERSION "0"

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

extern KDB *elektraRepo;
extern KeySet *elektraConfig;
extern Key *elektraParentKey;


/**
 * @brief Initializes Global Elektra Repo+Config
 *
 * It is safe to call it multiple times.
 *
 * @param [out] argc the number of args to parse, might be reduced
 * @param [out] argv the args, might be reduced
 * @see elektraClose
 */
void elektraOpen(int* argc, char** argv);

/**
 * @brief Closes and frees Repo+Config
 *
 * It is safe to call it multiple times.
 *
 * @see elektraOpen
 */
void elektraClose();

/**
 * @brief Uses Elektra to get from environment.
 *
 * @param name to be looked up in the environment.
 *
 * @return the value found for that key
 * @see getenv
 * @see secure_getenv
 */
char *elektraGetEnv(const char *name);
char *getenv(const char *name);
char *secure_getenv(const char *name);

#ifdef __cplusplus
}
}
#endif

#endif
