/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDBOPTS_H
#define ELEKTRA_KDBOPTS_H

#include <elektra/kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraGetOpts (KeySet * ks, int argc, const char ** argv, const char ** envp, Key * parentKey);
char * elektraGetOptsHelpMessage (Key * errorKey, const char * usage, const char * prefix);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBOPTS_H
