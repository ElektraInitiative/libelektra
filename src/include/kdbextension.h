/***************************************************************************
        kdbextension.h  -  Optional Methods which sit on top of elektra
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *
 * It is debatable if these functions belong to a general purpose key
 * database. However there are here to make working with Elektra more
 * easy or just as backwards compatability.
 *
 * They might be removed in a later version.
 *
 * They are not part of the documentation.
 *                                                                         *
 ***************************************************************************/

#ifndef KDBEXTENSION_H
#define KDBEXTENSION_H

#include "kdb.h"

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

const char *keyOwner(const Key *key);
ssize_t keyGetOwnerSize(const Key *key);
ssize_t keyGetOwner(const Key *key, char *returned, size_t maxSize);
ssize_t keySetOwner(Key *key, const char *owner);

const char *keyComment(const Key *key);
ssize_t keyGetCommentSize(const Key *key);
ssize_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize);
ssize_t keySetComment(Key *key, const char *newDesc);

#ifndef _WIN32
/* Conveniences Methods regarding Meta Info */
uid_t keyGetUID(const Key *key);
int keySetUID(Key *key, uid_t uid);

gid_t keyGetGID(const Key *key);
int keySetGID(Key *key, gid_t gid);

int keySetDir(Key *key);
mode_t keyGetMode(const Key *key);
int keySetMode(Key *key, mode_t mode);

time_t keyGetATime(const Key *key);
int keySetATime(Key *key, time_t atime);

time_t keyGetMTime(const Key *key);
int keySetMTime(Key *key, time_t mtime);

time_t keyGetCTime(const Key *key);
int keySetCTime(Key *key, time_t ctime);
#endif

#ifdef __cplusplus
}
}
#endif

#endif
