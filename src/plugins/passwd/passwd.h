/***************************************************************************
            passwd.h  -  Access the /etc/passwd file
                             -------------------
    begin                : Nov 15 2007
    copyright            : (C) 2007 by Patrick Sabin
    email                : patricksabin@gmx.at
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef PASSWD_H
#define PASSWD_H


#define _GNU_SOURCE

#include <kdbbackend.h>
#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <stdio.h>

/*TODO don't use private details*/
#include <kdbprivate.h>


#define BACKENDNAME "passwd"
#define BACKENDVERSION "0.0.1"
#define BACKENDPATH "system/users"
#define BACKENDDESCR "add description here"

int kdbbWriteLock (FILE *f);
int kdbbReadLock (FILE *f);
int kdbbUnlock (FILE *f);

struct _PasswdData {
	char *backend;
	const char *path;
	char *mountpoint; /* path of the mountpoint inclusive '/' */
	int mountpointlen; /* strlen(root) */
};

typedef struct _PasswdData PasswdData;

/**
 * the passwd backend can be configured with three config variables:
 * backendname (optional) is the name of this backend. Default: "passwd"
 * passwd_path (optional) is the path where the passwd file is stored. Default: /etc/passwd
 * mountpoint (optional) is the mountpoint. Default: take value from handle->mountpoint.
 */

int kdbOpen_passwd(KDB *handle);
int kdbClose_passwd(KDB *handle);
ssize_t kdbGet_passwd(KDB *handle, KeySet *ks, const Key *parentKey);
ssize_t kdbSet_passwd(KDB *handle, KeySet *ks, const Key *parentKey);
ELEKTRA_PLUGIN_EXPORT(passwd);

#endif
