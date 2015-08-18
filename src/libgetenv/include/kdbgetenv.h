#ifndef KDBGETENV_H
#define KDBGETENV_H

#include <kdb.h>

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
 * Its safe to call it multiple times
 *
 * @param [out] argc the number of args to parse, might be reduced
 * @param [out] argv the args, might be reduced
 */
void elektraOpen(int* argc, char** argv);
void elektraClose();

#ifdef __cplusplus
}
}
#endif

#endif
