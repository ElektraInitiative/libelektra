/**
 * @file
 *
 * @brief metadata functions
 *
 * These functions might be removed in a later version.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBMETA_H
#define KDBMETA_H

#include "kdb.h"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

const char * keyComment (const ElektraKey * key);
ssize_t keyGetCommentSize (const ElektraKey * key);
ssize_t keyGetComment (const ElektraKey * key, char * returnedDesc, size_t maxSize);
ssize_t keySetComment (ElektraKey * key, const char * newDesc);

int elektraKeyCmpOrder (const ElektraKey * a, const ElektraKey * b);

ElektraKeyset * elektraMetaArrayToKS (ElektraKey *, const char *);

void elektraMetaArrayAdd (ElektraKey *, const char *, const char *);

char * elektraMetaArrayToString (const ElektraKey *, const char *, const char *);

int elektraSortTopology (ElektraKeyset *, ElektraKey **);

#ifdef __cplusplus
}
}
#endif

#endif
