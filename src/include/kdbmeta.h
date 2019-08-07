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

#include "elektra/kdb.h"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

const char * keyOwner (const Key * key);
ssize_t keyGetOwnerSize (const Key * key);
ssize_t keyGetOwner (const Key * key, char * returned, size_t maxSize);
ssize_t keySetOwner (Key * key, const char * owner);

const char * keyComment (const Key * key);
ssize_t keyGetCommentSize (const Key * key);
ssize_t keyGetComment (const Key * key, char * returnedDesc, size_t maxSize);
ssize_t keySetComment (Key * key, const char * newDesc);

#ifndef _WIN32
/* Conveniences Methods regarding Meta Info */
uid_t keyGetUID (const Key * key);
int keySetUID (Key * key, uid_t uid);

gid_t keyGetGID (const Key * key);
int keySetGID (Key * key, gid_t gid);

int keySetDir (Key * key);
mode_t keyGetMode (const Key * key);
int keySetMode (Key * key, mode_t mode);

time_t keyGetATime (const Key * key);
int keySetATime (Key * key, time_t atime);

time_t keyGetMTime (const Key * key);
int keySetMTime (Key * key, time_t mtime);

time_t keyGetCTime (const Key * key);
int keySetCTime (Key * key, time_t ctime);
#endif

int elektraKeyCmpOrder (const Key * a, const Key * b);

KeySet * elektraMetaArrayToKS (const Key *, const char *);

void elektraMetaArrayAdd (Key *, const char *, const char *);

char * elektraMetaArrayToString (const Key *, const char *, const char *);

int elektraSortTopology (KeySet *, Key **);

#ifdef __cplusplus
}
}
#endif

#endif
