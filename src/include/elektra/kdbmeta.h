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

#include <elektra/kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

const char * keyComment (const Key * key);
ssize_t keyGetCommentSize (const Key * key);
ssize_t keyGetComment (const Key * key, char * returnedDesc, size_t maxSize);
ssize_t keySetComment (Key * key, const char * newDesc);

int elektraKeyCmpOrder (const Key * a, const Key * b);

KeySet * elektraMetaArrayToKS (Key *, const char *);

void elektraMetaArrayAdd (Key *, const char *, const char *);

char * elektraMetaArrayToString (const Key *, const char *, const char *);

int elektraSortTopology (KeySet *, Key **);

#ifdef __cplusplus
}
}
#endif

#endif
