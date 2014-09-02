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

/* Conveniences Methods regarding Meta Info */
uid_t keyGetUID(const Key *key);
int keySetUID(Key *key, uid_t uid);

gid_t keyGetGID(const Key *key);
int keySetGID(Key *key, gid_t gid);

int keySetDir(Key *key);
mode_t keyGetMode(const Key *key);
int keySetMode(Key *key, mode_t mode);

int keyIsDir(const Key *key);

time_t keyGetATime(const Key *key);
int keySetATime(Key *key, time_t atime);

time_t keyGetMTime(const Key *key);
int keySetMTime(Key *key, time_t mtime);

time_t keyGetCTime(const Key *key);
int keySetCTime(Key *key, time_t ctime);

const char *keyOwner(const Key *key);
ssize_t keyGetOwnerSize(const Key *key);
ssize_t keyGetOwner(const Key *key, char *returned, size_t maxSize);
ssize_t keySetOwner(Key *key, const char *owner);

const char *keyComment(const Key *key);
ssize_t keyGetCommentSize(const Key *key);
ssize_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize);
ssize_t keySetComment(Key *key, const char *newDesc);

int elektraKeyCmpOrder(const Key *a, const Key *b);

/* Name Manipulation Methods */
ssize_t keyNameGetRootNameSize(const char *keyname);
ssize_t keyNameGetBaseNameSize(const char *keyname);
ssize_t keyNameGetFullRootNameSize(const char *keyname);

ssize_t keyGetRootNameSize(const Key *key);
ssize_t keyGetRootName(const Key *key, char *returned, size_t maxSize);

ssize_t keyGetFullRootNameSize(const Key *key);
ssize_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize);

ssize_t keyGetParentName(const Key *key, char *returned, size_t maxSize);
ssize_t keyGetParentNameSize(const Key *key);


/* Conveniences Methods for Making Tests */
int keyIsBelow(const Key *key, const Key *check);
int keyIsDirectBelow(const Key *key, const Key *check);

int keyIsSystem(const Key *key);
int keyIsUser(const Key *key);

int keyNameIsSystem(const char *keyname);
int keyNameIsUser(const char *keyname);


/***************************************
 *
 * Functions which might be reintroduced
 *
 **************************************/

/*****************
 * Namespaces
 *****************/

/*
 * Elektra currently supported Key namespaces.
 *
 * @ingroup key
 * @see kdbGet(), keyGetNamespace(), keyNameGetNamespace()
 */
enum {
	KEY_NS_SYSTEM=1,       /*!< The @p system keys */
	KEY_NS_USER=2          /*!< The @p user keys */
};

int keyGetNamespace(const Key *key);
int keyNameGetNamespace(const char *keyname);


/*****************
 * Misc Functions
 *****************/

keyswitch_t keyCompare(const Key *key1, const Key *key2);
ssize_t ksGetCommonParentName(const KeySet *ks,char *returnedCommonParent,
	size_t maxSize);


/*****************
 * Allocation
 *****************/

int ksResize(KeySet *ks, size_t size);
size_t ksGetAlloc(const KeySet *ks);

#ifdef __cplusplus
}
}
#endif

#endif
