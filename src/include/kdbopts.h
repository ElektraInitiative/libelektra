/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDBOPTS_H
#define ELEKTRA_KDBOPTS_H

#include <kdb.h>

int elektraGetOpts (KeySet * ks, int argc, const char ** argv, const char ** envp, Key * errorKey);

#endif // ELEKTRA_KDBOPTS_H
