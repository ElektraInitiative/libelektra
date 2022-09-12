/**
 * @file
 *
 * @brief Gopts contract
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBGOPTS_H
#define ELEKTRA_KDBGOPTS_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraGOptsContract (ElektraKeyset * contract, int argc, const char * const * argv, const char * const * envp, const ElektraKey * parentKey,
			  ElektraKeyset * goptsConfig);
int elektraGOptsContractFromStrings (ElektraKeyset * contract, size_t argsSize, const char * args, size_t envSize, const char * env,
				     const ElektraKey * parentKey, ElektraKeyset * goptsConfig);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBGOPTS_H
