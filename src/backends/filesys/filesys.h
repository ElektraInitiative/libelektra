/***************************************************************************
            filesys.h  -  A filesystem backend implementation for Elektra
                             -------------------
    begin                : Mon Dec 25 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the implementation of a filesystem backend for the            *
 *   Elektra. Each Key is a file in the filesystem.                        *
 *   It is as secure as filesystem security. It is as reliable             *
 *   as filesystem. It uses only standards C calls, which makes it         *
 *   usable by very low level or early boot stage software, like           *
 *   /sbin/init.                                                           *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <stdio.h>

#include <kdbbackend.h>

/* filesys is an hack and needs private information... */
#include <kdbprivate.h>

#define BACKENDNAME "filesys"
#define BACKENDVERSION "0.2.1"


/* When FORMAT_VERSION changes, FORMAT must change too. */
#define RG_KEY_FORMAT_VERSION   2
#define RG_KEY_FORMAT           "RG00" RG_KEY_FORMAT_VERSION


char *DIR_FILENAME="%%dirdata";


int kdbbWriteLock (FILE *f);
int kdbbReadLock (FILE *f);
int kdbbUnlock (FILE *f);

/* These are some helpers we'll define bellow */
int keyFileUnserialize(Key *key,FILE *input);
size_t kdbGetFullFilename(const Key *forKey,char *returned,size_t maxSize);
int keyFileSerialize(Key *key, FILE *output);
int keyFromStat(Key *key,struct stat *stat);



int kdbGetKey_filesys(KDB *handle, Key *key);
int kdbRemoveKey_filesys(KDB *handle, Key *key);
int kdbSetKey_filesys(KDB *handle, Key *key);



/*****************
 * Error codes
 *****************/

/*
 * Error codes.
 *
 * Methods return -1 or NULL on failure. To indicate what the problem
 * was @c errno is propagated to one of the values below.
 *
 * The idea is to keep compatibility to POSIX @c errno system, so each library
 * error code maps to some POSIX E* error. Some mappings really make no sense,
 * so to detect errors use the following error names, and to detect
 * system's, use the naming conventions documented in @c errno man page.
 *
 * A very robust program should check the return value and @c errno
 * after each API call.
 *
 * @ingroup kdb
 * @see kdbhGetError(), kdbhSetError()
 * @see kdbStrError()
 */
enum KDBErr {
	KDB_ERR_OK=0,			/*!< No error */

	/* Errors related to invalid/uninitialized objects */
	KDB_ERR_NULLKEY=EINVAL,		/*!< Null Key object */

	/* Errors related to bad key names or keys not found */
	KDB_ERR_NOTFOUND=ENOENT,	/*!< Key was not found */
	KDB_ERR_INVALIDKEY=ESRCH,	/*!< Key name not valid */

	/* Errors related to empty internal key properties */
	KDB_ERR_NOKEY=EFAULT,		/*!< Key has no name */
	KDB_ERR_NODATA=ENODEV,		/*!< Key has no data */
	KDB_ERR_NODESC=ENOMSG,		/*!< Key has no comment */
	KDB_ERR_NOOWNER=EDOM,		/*!< Key has no user domain set */
	KDB_ERR_NOGROUP=ECHILD,		/*!< Key has no group */
	KDB_ERR_NOTIME=ENOTTY,		/*!< Key has no access time set */
	KDB_ERR_OVERFLOW=EOVERFLOW,	/*!< Key has too large value to convert it to type */

	/* Errors related to permissions*/
	KDB_ERR_NOCRED=EACCES,		/*!< No credentials to access resource */
	KDB_ERR_NOTEMPTY=ENOTEMPTY,	/*!< Tried to delete directory key with children */

	/* Errors related to no memory, failed internal operations */
	KDB_ERR_TRUNC=ERANGE,		/*!< Buffer was too small */
	KDB_ERR_TOOLONG=ENAMETOOLONG,	/*!< String is too big for buffer */
	KDB_ERR_NOMEM=ENOMEM,		/*!< Out of memory */
	KDB_ERR_TYPEMISMATCH=EBADF,	/*!< Failed to convert key data due to data type */
	KDB_ERR_CONVERT=EILSEQ,		/*!< Could not utf8 convert data or name */

	/* Errors related to backend access or opening */
	KDB_ERR_NOSYS=ENOSYS,		/*!< Backend method not implemented */
	KDB_ERR_EBACKEND=EIO,		/*!< Error opening backend */
	KDB_ERR_BACKEND=EAGAIN,		/*!< Error in the backend in reading or writing the data */
	KDB_ERR_CONFIG=ENXIO,		/*!< Essential configuration for mountpoint missing */

	/* Errors related to backend storage */
	KDB_ERR_NODIR=ENOTDIR,		/*!< The file where key should be is not a directory key */
	KDB_ERR_NOSPACE=ENOSPC,		/*!< No space left on device */
	KDB_ERR_NOLOCK=ENOLCK,		/*!< Could not acquire lock */
	KDB_ERR_NOTSUP=ENOTSUP,		/*!< Operation not supported could not set or delete key */
	KDB_ERR_PERM=EPERM,		/*!< Could not access data because of permission problem */
	KDB_ERR_PIPE=EPIPE,		/*!< Broken Pipe */
	KDB_ERR_EROFS=EROFS,		/*!< Read only filesystem */
	KDB_ERR_EXDEV=EXDEV		/*!< Improper link */
};
