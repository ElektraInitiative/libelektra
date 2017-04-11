/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PRIVATE_H
#define ELEKTRA_PRIVATE_H

#include "kdb.h"

struct _Elektra
{
    KDB * kdb;
    KeySet * config;
    Key * parentKey;
};

#endif //ELEKTRA_PRIVATE_H
