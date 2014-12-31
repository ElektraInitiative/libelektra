/***************************************************************************
          resolver.c  -  Skeleton of a plugin to be copied
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid plugin.                             *
 *   Simple fill the empty Resolver functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/

#include "resolver.h"

#include <kdbproposal.h>

#include "kdbos.h"

#include <stdlib.h>

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

/* Needs posix */
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/types.h>
#include <dirent.h>
#include <kdberrors.h>

#ifdef ELEKTRA_LOCK_MUTEX
#include <pthread.h>

// every resolver should use the same mutex
extern pthread_mutex_t elektra_resolver_mutex;
#endif

static void resolverInit (resolverHandle *p, const char *path)
{
	p->fd = -1;
	p->mtime.tv_sec = 0;
	p->mtime.tv_nsec = 0;
	p->filemode = KDB_FILE_MODE;
	p->dirmode = KDB_FILE_MODE | KDB_DIR_MODE;

	p->filename = 0;
	p->dirname= 0;
	p->tempfile = 0;

	p->path = path;
}

static resolverHandle * elektraGetResolverHandle(Plugin *handle, Key *parentKey)
{
	resolverHandles *pks = elektraPluginGetData(handle);
	switch (keyGetNamespace(parentKey))
	{
	case KEY_NS_SPEC:
		return &pks->spec;
	case KEY_NS_DIR:
		return &pks->dir;
	case KEY_NS_USER:
		return &pks->user;
	case KEY_NS_SYSTEM:
		return &pks->system;
	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		return 0;
	}

	return 0;
}


static void resolverCloseOne (resolverHandle *p)
{
	free (p->filename); p->filename = 0;
	free (p->dirname); p->dirname= 0;
	free (p->tempfile); p->tempfile = 0;
}

static void resolverClose (resolverHandles *p)
{
	resolverCloseOne(&p->spec);
	resolverCloseOne(&p->dir);
	resolverCloseOne(&p->user);
	resolverCloseOne(&p->system);
	free (p);
}

/**
 * Locks file for exclusive read/write mode.
 *
 * This function will not block until all reader
 * and writer have left the file.
 * -> conflict with other cooperative process detected,
 *    but we were later (and lost)
 *
 * @exception 27 set if locking failed, most likely a conflict
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
static int elektraLockFile (int fd ELEKTRA_UNUSED,
		Key *parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_FILE
	struct flock l;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		if (errno == EAGAIN || errno == EACCES)
		{
			ELEKTRA_SET_ERROR (30, parentKey, "conflict because other process writes to configuration indicated by file lock");
		}
		else
		{
			char buffer[ERROR_SIZE];

			ELEKTRA_SET_ERRORF (30, parentKey, "assuming conflict because of failed file lock with message: %s",
				strerror_r(errno, buffer, ERROR_SIZE));
		}
		return -1;
	}

	return ret;
#else
	return 0;
#endif
}


/**
 * Unlocks file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
static int elektraUnlockFile (int fd ELEKTRA_UNUSED,
		Key *parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_FILE
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		char buffer[ERROR_SIZE];
		ELEKTRA_ADD_WARNINGF(32, parentKey, "fcntl SETLK unlocking failed with message: %s", 
			strerror_r(errno, buffer, ERROR_SIZE));
	}

	return ret;
#else
	return 0;
#endif
}

/**
 * @brief mutex lock for multithread-safety
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraLockMutex(Key *parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_MUTEX
	int ret = pthread_mutex_trylock(&elektra_resolver_mutex);
	if (ret != 0)
	{
		if (errno == EBUSY // for trylock
			|| errno == EDEADLK) // for error checking mutex, if enabled
		{
			ELEKTRA_SET_ERROR (30, parentKey, "conflict because other thread writes to configuration indicated by mutex lock");
		}
		else
		{
			char buffer[ERROR_SIZE];
			ELEKTRA_SET_ERRORF (30, parentKey, "assuming conflict because of failed mutex lock with message: %s",
				strerror_r(errno, buffer, ERROR_SIZE));
		}
		return -1;
	}
	return 0;
#else
	return 0;
#endif
}

/**
 * @brief mutex unlock for multithread-safety
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraUnlockMutex(Key *parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_MUTEX
	int ret = pthread_mutex_unlock(&elektra_resolver_mutex);
	if (ret != 0)
	{
		char buffer[ERROR_SIZE];
		ELEKTRA_ADD_WARNINGF(32, parentKey, "mutex unlock failed with message: %s",
			strerror_r(errno, buffer, ERROR_SIZE));
		return -1;
	}
	return 0;
#else
	return 0;
#endif
}



/**
 * @brief Close a file
 *
 * @param fd the filedescriptor to close
 * @param parentKey the key to write warnings to
 */
static void elektraCloseFile(int fd, Key *parentKey)
{
	if (close (fd) == -1)
	{
		char buffer[ERROR_SIZE];
		ELEKTRA_ADD_WARNINGF(33, parentKey, "close failed with message: %s",
			strerror_r(errno, buffer, ERROR_SIZE));
	}
}

/**
 * @brief Add error text received from strerror_r
 *
 * @param errorText should have at least ERROR_SIZE bytes in reserve
 */
static void elektraAddErrnoText(char *errorText)
{
	char buffer[ERROR_SIZE];
	if (errno == E2BIG)
	{
		strcat (errorText, "could not find a / in the pathname");
	}
	else if (errno == EBADMSG)
	{
		strcat (errorText, "went up to root for creating directory");
	}
	else
	{
		strcat(errorText, strerror_r(errno, buffer, ERROR_SIZE-2));
	}
}

int ELEKTRA_PLUGIN_FUNCTION(resolver, open)
	(Plugin *handle, Key *errorKey)
{
	KeySet *resolverConfig = elektraPluginGetConfig(handle);
	if (ksLookupByName(resolverConfig, "/module", 0)) return 0;
	const char *path = keyString(ksLookupByName(resolverConfig, "/path", 0));

	if (!path)
	{
		ELEKTRA_SET_ERROR(34, errorKey, "Could not find file configuration");
		return -1;
	}

	resolverHandles *p = malloc(sizeof(resolverHandles));
	resolverInit (&p->spec, path);
	resolverInit (&p->dir, path);
	resolverInit (&p->user, path);
	resolverInit (&p->system, path);

	// system and spec files need to be world-readable, otherwise they are
	// useless
	p->system.filemode = 0644;
	p->system.dirmode = 0755;
	p->spec.filemode = 0644;
	p->spec.dirmode = 0755;

	Key *testKey = keyNew("", KEY_END);
	keySetName(testKey, "spec");
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->spec, errorKey) == -1)
	{
		resolverClose(p);
		keyDel (testKey);
		ELEKTRA_SET_ERROR(35, errorKey, "Could not resolve spec key");
		return -1;
	}
	keySetName(testKey, "dir");
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->dir, errorKey) == -1)
	{
		resolverClose(p);
		keyDel (testKey);
		ELEKTRA_SET_ERROR(35, errorKey, "Could not resolve dir key");
		return -1;
	}

	keySetName(testKey, "user");
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->user, errorKey) == -1)
	{
		resolverClose(p);
		keyDel (testKey);
		ELEKTRA_SET_ERRORF(35, errorKey, "Could not resolve user key with conf %s", ELEKTRA_VARIANT_USER);
		return -1;
	}

	keySetName(testKey, "system");
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->system, errorKey) == -1)
	{
		resolverClose(p);
		keyDel (testKey);
		ELEKTRA_SET_ERRORF(35, errorKey, "Could not resolve system key with conf %s", ELEKTRA_VARIANT_SYSTEM);
		return -1;
	}
	keyDel (testKey);

	elektraPluginSetData(handle, p);

	return 0; /* success */
}

int ELEKTRA_PLUGIN_FUNCTION(resolver, close)
	(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	resolverHandles *ps = elektraPluginGetData(handle);

	if (ps)
	{
		resolverClose(ps);
		elektraPluginSetData(handle, 0);
	}

	return 0; /* success */
}


int ELEKTRA_PLUGIN_FUNCTION(resolver, get)
	(Plugin *handle, KeySet *returned, Key *parentKey)
{
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);

	Key *root = keyNew("system/elektra/modules/"
			ELEKTRA_PLUGIN_NAME , KEY_END);

	if (keyRel(root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet *info =
#include "contract.h"
		ksAppend(returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);

	keySetString(parentKey, pk->filename);

	int errnoSave = errno;
	struct stat buf;

	/* Start file IO with stat() */
	if (stat (pk->filename, &buf) == -1)
	{
		// no file, so storage has no job
		errno = errnoSave;
		pk->mtime.tv_sec = 0; // no file, so no time
		pk->mtime.tv_nsec = 0; // no file, so no time
		return 0;
	}
	else
	{
		// successful, remember mode
		pk->filemode = buf.st_mode;
	}

	/* Check if update needed */
	if (pk->mtime.tv_sec == buf.st_mtim.tv_sec &&
	    pk->mtime.tv_nsec == buf.st_mtim.tv_nsec)
	{
		// no update, so storage has no job
		errno = errnoSave;
		return 0;
	}

	pk->mtime.tv_sec = buf.st_mtim.tv_sec;
	pk->mtime.tv_nsec = buf.st_mtim.tv_nsec;

	errno = errnoSave;
	return 1;
}


/**
 * @brief Add identity received from getuid(), geteuid(), getgid() and getegid()
 *
 * @param errorText should have at least ERROR_SIZE bytes in reserve
 */
static void elektraAddIdentity(char *errorText)
{
	char buffer[ERROR_SIZE];
	snprintf (buffer, ERROR_SIZE-2, "uid: %u, euid: %u, gid: %u, egid: %u", getuid(), geteuid(), getgid(), getegid());
	strcat (errorText, buffer);
}

/**
 * @brief Open a file and yield an error if it did not work
 *
 * @param pk->filename will be used
 * @param parentKey to yield the error too
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraOpenFile(resolverHandle *pk, Key *parentKey)
{
	pk->fd = open (pk->filename, O_RDWR | O_CREAT, pk->filemode);

	if (pk->fd == -1)
	{
		char *errorText = malloc(
				strlen(pk->filename) + ERROR_SIZE*2 + 60);
		strcpy (errorText, "Opening configuration file \"");
		strcat (errorText, pk->filename);
		strcat (errorText, "\" failed, error was: \"");
		elektraAddErrnoText(errorText);
		strcat (errorText, "\" ");
		elektraAddIdentity(errorText);
		ELEKTRA_SET_ERROR(26, parentKey, errorText);
		free (errorText);
		return -1;
	}
	return 0;
}


/**
 * @brief Create pathname recursively.
 *
 * Try unless the whole path was
 * created or it is sure that it cannot be done.
 *
 * @param pathname The path to create.
 *
 * @retval 0 on success
 * @retval -1 on error + elektra error will be set
 */
static int elektraMkdirParents(resolverHandle *pk, const char *pathname, Key *parentKey)
{
	if (mkdir(pathname, pk->dirmode) == -1)
	{
		if (errno != ENOENT)
		{
			// hopeless, give it up
			goto error;
		}

		char *p = strrchr(pathname, '/');

		/* nothing found */
		if (p == NULL)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = E2BIG;
			goto error;
		}

		/* absolute path */
		if (p == pathname)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = EBADMSG;
			goto error;
		}

		/* Cut path at last /. */
		*p = 0;

		/* Now call ourselves recursively */
		if (elektraMkdirParents(pk, pathname, parentKey) == -1)
		{
			// do not yield an error, was already done
			// before
			*p = '/';
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, pk->dirmode) == -1)
		{
			goto error;
		}
	}

	return 0;

error:
	{
		char *errorText = malloc(
				strlen(pathname) + ERROR_SIZE*2 + 60);
		strcpy (errorText, "Could not create directory \"");
		strcat (errorText, pathname);
		strcat (errorText, "\", because: \"");
		elektraAddErrnoText(errorText);
		strcat (errorText, "\" ");
		elektraAddIdentity(errorText);
		ELEKTRA_SET_ERROR(74, parentKey, errorText);
		free (errorText);
		return -1;
	}
}

/**
 * @brief Check conflict for the current open file
 *
 * Does an fstat and checks if mtime are equal as they were 
 *
 * @param pk to get mtime and fd from
 * @param parentKey to write errors&warnings to
 *
 * @retval 0 success
 * @retval -1 error
 */
static int elektraCheckConflict(resolverHandle *pk, Key *parentKey)
{
	if (pk->mtime.tv_sec == 0 &&
		pk->mtime.tv_nsec == 0)
	{
		// this can happen if the kdbGet() path found no file

		// no conflict possible, so just return successfully
		return 0;
	}

	struct stat buf;

	if (fstat(pk->fd, &buf) == -1)
	{
		char *errorText = malloc(
				strlen(pk->filename) + ERROR_SIZE*2 + 60);
		strcpy (errorText, "Could not fstat to check for conflict \"");
		strcat (errorText, pk->filename);
		strcat (errorText, "\" ");
		strcat (errorText, "because stat said: \"");
		elektraAddErrnoText(errorText);
		strcat (errorText, "\" ");
		elektraAddIdentity(errorText);
		ELEKTRA_ADD_WARNING(29, parentKey, errorText);
		free (errorText);

		ELEKTRA_SET_ERROR (30, parentKey, "assuming conflict because of failed stat (warning 29 for details)");
		return -1;
	}

	if (buf.st_mtim.tv_sec != pk->mtime.tv_sec ||
	    buf.st_mtim.tv_nsec != pk->mtime.tv_nsec)
	{
		ELEKTRA_SET_ERRORF (30, parentKey,
				"conflict, file modification time stamp %ld.%ld is different than our time stamp %ld.%ld, config file name is \"%s\", "
				"our identity is uid: %u, euid: %u, gid: %u, egid: %u",
				buf.st_mtim.tv_sec, buf.st_mtim.tv_nsec,
				pk->mtime.tv_sec, pk->mtime.tv_nsec,
				pk->filename,
				getuid(), geteuid(), getgid(), getegid());
		return -1;
	}

	return 0;
}


/**
 * @brief Does everything needed before the storage plugin will be
 * invoked.
 *
 * @param pk
 * @param parentKey
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraSetPrepare(resolverHandle *pk, Key *parentKey)
{
	pk->fd = open (pk->filename, O_RDWR | O_CREAT, pk->filemode);
	// we can silently ignore an error, because we will retry later
	if (pk->fd == -1)
	{
		elektraMkdirParents(pk, pk->dirname, parentKey);
		if (elektraOpenFile(pk, parentKey) == -1)
		{
			// no way to be successful
			return -1;
		}
	}

	if (elektraLockMutex(parentKey) != 0)
	{
		elektraCloseFile(pk->fd, parentKey);
		pk->fd = -1;
		return -1;
	}

	// now we have a file, so lock immediately
	if (elektraLockFile(pk->fd, parentKey) == -1)
	{
		elektraCloseFile(pk->fd, parentKey);
		elektraUnlockMutex(parentKey);
		pk->fd = -1;
		return -1;
	}

	if (elektraCheckConflict(pk, parentKey) == -1)
	{
		elektraUnlockFile(pk->fd, parentKey);
		elektraCloseFile(pk->fd, parentKey);
		elektraUnlockMutex(parentKey);
		pk->fd = -1;
		return -1;
	}

	return 0;
}


/* Update timestamp of old file to provoke conflicts in
 * stalling processes that might still wait with the old
 * filedescriptor */
static void elektraUpdateFileTime(resolverHandle *pk, Key *parentKey)
{

	const struct timespec times[2] = {
		pk->mtime,  // atime
		pk->mtime}; // mtime

	if (futimens(pk->fd, times) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNINGF(99, parentKey,
			"Could not update time stamp of \"%s\", because %s",
			pk->filename, buffer);
	}
}

/**
 * @brief Now commit the temporary file to be final
 *
 * @param pk
 * @param parentKey
 *
 * It will also reset pk->fd
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraSetCommit(resolverHandle *pk, Key *parentKey)
{
	int ret = 0;

	if (rename (pk->tempfile, pk->filename) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_SET_ERROR (31, parentKey, buffer);
		ret = -1;
	}

	struct stat buf;
	if (stat (pk->filename, &buf) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING (29, parentKey, buffer);
	} else {
		/* Update my timestamp */
		pk->mtime.tv_sec = buf.st_mtim.tv_sec;
		pk->mtime.tv_nsec = buf.st_mtim.tv_nsec;
	}

	elektraUpdateFileTime(pk, parentKey);
	if (buf.st_mode != pk->filemode)
	{
		// change mode to what it was before
		chmod(pk->filename, pk->filemode);
	}

	elektraUnlockFile(pk->fd, parentKey);
	elektraCloseFile(pk->fd, parentKey);
	elektraUnlockMutex(parentKey);

	DIR * dirp = opendir(pk->dirname);
	// checking dirp not needed, fsync will have EBADF
	if (fsync(dirfd(dirp)) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNINGF(88, parentKey,
			"Could not sync directory \"%s\", because %s",
			pk->dirname, buffer);
	}
	closedir(dirp);

	return ret;
}


int ELEKTRA_PLUGIN_FUNCTION(resolver, set)
	(Plugin *handle, KeySet *r ELEKTRA_UNUSED, Key *parentKey)
{
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);

	// might be useless (case of error), will not harm
	keySetString(parentKey, pk->tempfile);

	int errnoSave = errno;
	int ret = 1;

	if (pk->fd == -1)
	{
		/* no fd up to now, so we are in first phase*/
		if (elektraSetPrepare(pk, parentKey) == -1)
		{
			ret = -1;
		}

		errno = errnoSave; // maybe some temporary error happened
	}
	else
	{
		/* we have an fd, so we are in second phase*/
		if (elektraSetCommit(pk, parentKey) == -1)
		{
			ret = -1;
		}

		errno = errnoSave; // maybe some temporary error happened

		// reset for next time
		pk->fd = -1;
	}

	return ret;
}

int ELEKTRA_PLUGIN_FUNCTION(resolver, error)
	(Plugin *handle, KeySet *r ELEKTRA_UNUSED, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);

	if (pk->fd != -1)
	{
		elektraUnlockFile(pk->fd, parentKey);
		elektraCloseFile(pk->fd, parentKey);
		elektraUnlockMutex(parentKey);
	}

	if (unlink (pk->tempfile) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		int written = strlen(buffer);
		strcat(buffer, " the file: ");
		strncat(buffer, pk->tempfile, ERROR_SIZE-written-10);
		ELEKTRA_ADD_WARNING(36, parentKey, buffer);
		errno = errnoSave;
	}

	// reset for next time
	pk->fd = -1;

	return 0;
}


Plugin *ELEKTRA_PLUGIN_EXPORT(resolver)
{
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(resolver, open),
		ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(resolver, close),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(resolver, get),
		ELEKTRA_PLUGIN_SET,	&ELEKTRA_PLUGIN_FUNCTION(resolver, set),
		ELEKTRA_PLUGIN_ERROR,	&ELEKTRA_PLUGIN_FUNCTION(resolver, error),
		ELEKTRA_PLUGIN_END);
}

