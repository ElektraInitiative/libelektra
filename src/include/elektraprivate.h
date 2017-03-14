/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRAPRIVATE_H
#define ELEKTRAPRIVATE_H

#include "kdb.h"

struct _ElektraError
{
    int errorNr;
    const char * msg;
};

struct _Elektra
{
    KDB * kdb;
    KeySet * config;
    Key * parentKey;

    struct _ElektraError * error;
};

#endif //ELEKTRAPRIVATE_H
