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

#ifdef ELEKTRA_CONFLICT_DEBUG
//has stop signals at certain points to let you test
//concurrent access from shell scripts
#include <signal.h>
#endif

#ifdef _GNU_SOURCE
#error "Something turned _GNU_SOURCE on, this breaks strerror_r!"
#endif

static void resolverInit (resolverHandle *p, const char *path)
{
	p->fd = -1;
	p->mtime = 0;

	p->filename = 0;
	p->dirname= 0;
	p->tempfile = 0;

	p->path = path;
}

static resolverHandle * elektraGetResolverHandle(Plugin *handle, Key *parentKey)
{
	resolverHandles *pks = elektraPluginGetData(handle);
	if (!strncmp(keyName(parentKey), "user", 4)) return &pks->user;
	else return &pks->system;
}


static void resolverClose (resolverHandle *p)
{
	free (p->filename); p->filename = 0;
	free (p->dirname); p->dirname= 0;
	free (p->tempfile); p->tempfile = 0;
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
		int error_ret = strerror_r(errno, buffer, ERROR_SIZE-2);
		if (error_ret == -1)
		{
			if (errno == EINVAL)
			{
				strcat (errorText, "Got no valid errno!");
			}
			else if (errno == ERANGE)
			{
				strcat (errorText, "Not enough space for error text in buffer!");
			}
			else
			{
				strcat (errorText, "strerror_r returned wrong error value!");
			}
		}
		else
		{
			strcat (errorText, buffer);
		}
	}
}

int ELEKTRA_PLUGIN_FUNCTION(resolver, open)
	(Plugin *handle, Key *errorKey)
{

	KeySet *resolverConfig = elektraPluginGetConfig(handle);
	const char *path = keyString(ksLookupByName(resolverConfig, "/path", 0));

	if (!path)
	{
		ELEKTRA_SET_ERROR(34, errorKey, "Could not find file configuration");
		return -1;
	}

	resolverHandles *p = malloc(sizeof(resolverHandles));
	resolverInit (&p->user, path);
	resolverInit (&p->system, path);

	Key *testKey = keyNew("system", KEY_END);
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->system, errorKey) == -1)
	{
		resolverClose(&p->user);
		resolverClose(&p->system);
		free (p);
		keyDel (testKey);
		ELEKTRA_SET_ERROR(35, errorKey, "Could not resolve system key");
		return -1;
	}

	keySetName(testKey, "user");
	if (ELEKTRA_PLUGIN_FUNCTION(resolver, filename)(testKey, &p->user, errorKey) == -1)
	{
		resolverClose(&p->user);
		resolverClose(&p->system);
		free (p);
		keyDel (testKey);
		ELEKTRA_SET_ERROR(35, errorKey, "Could not resolve user key");
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
		resolverClose(&ps->user);
		resolverClose(&ps->system);

		free (ps);
		elektraPluginSetData(handle, 0);
	}

	return 0; /* success */
}


int ELEKTRA_PLUGIN_FUNCTION(resolver, get)
	(Plugin *handle, KeySet *returned, Key *parentKey)
{
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);

	// might be useless, will not harm
	keySetString(parentKey, pk->filename);

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

	int errnoSave = errno;
	struct stat buf;

	/* Start file IO with stat() */
	if (stat (pk->filename, &buf) == -1)
	{
		// no file, so storage has no job
		errno = errnoSave;
		pk->mtime = 0; // no file, so no time
		return 0;
	}

	/* Check if update needed */
	if (pk->mtime == buf.st_mtime)
	{
		// no update, so storage has no job
		errno = errnoSave;
		return 0;
	}

	pk->mtime = buf.st_mtime;

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
	pk->fd = open (pk->filename, O_RDWR | O_CREAT, KDB_FILE_MODE);

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
static int elektraMkdirParents(const char *pathname, Key *parentKey)
{
	if (mkdir(pathname, KDB_DIR_MODE | KDB_FILE_MODE) == -1)
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
		if (elektraMkdirParents(pathname, parentKey) == -1)
		{
			// do not yield an error, was already done
			// before
			*p = '/';
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, KDB_DIR_MODE | KDB_FILE_MODE) == -1)
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
	if (pk->mtime == 0)
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

	if (buf.st_mtime != pk->mtime)
	{
		char *errorText = malloc(
				strlen(pk->filename) +
				ERROR_SIZE*2+ // for snprintf+identity
				5); // for spaces after filename
		snprintf(errorText, ERROR_SIZE,
				"conflict, file time stamp %ld is different than our time stamp %ld, config file name is \"",
				buf.st_mtime, pk->mtime);
		strcat(errorText, pk->filename);
		strcat(errorText, "\" ");
		elektraAddIdentity(errorText);
		ELEKTRA_SET_ERROR (30, parentKey, errorText);
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
#ifdef ELEKTRA_CONFLICT_DEBUG
	// we are somewhere in the middle of work
	kill(getpid(), SIGSTOP);
#endif

	pk->fd = open (pk->filename, O_RDWR | O_CREAT, KDB_FILE_MODE);
	// we can silently ignore an error, because we will retry later
	if (pk->fd == -1)
	{
		elektraMkdirParents(pk->dirname, parentKey);
		if (elektraOpenFile(pk, parentKey) == -1)
		{
			// no way to be successful
			return -1;
		}
	}

	// now we have a file, so lock immediately
	if (elektraLockFile(pk->fd, parentKey) == -1)
	{
		elektraCloseFile(pk->fd, parentKey);
		return -1;
	}

#ifdef ELEKTRA_CONFLICT_DEBUG
	kill(getpid(), SIGSTOP);
#endif

	if (elektraCheckConflict(pk, parentKey) == -1)
	{
		elektraUnlockFile(pk->fd, parentKey);
		elektraCloseFile(pk->fd, parentKey);
		return -1;
	}

	return 0;
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
		/* Update timestamp */
		pk->mtime = buf.st_mtime;
	}

	elektraUnlockFile(pk->fd, parentKey);
	elektraCloseFile(pk->fd, parentKey);

	DIR * dirp = opendir(pk->dirname);
	// checking dirp not needed, fsync will have EBADF
	if (fsync(dirfd(dirp)) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING (88, parentKey, buffer);
	}
	closedir(dirp); // TODO: check for error?

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

