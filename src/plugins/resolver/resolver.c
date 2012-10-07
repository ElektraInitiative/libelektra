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
#include <libgen.h>

#ifdef _GNU_SOURCE
#error "Something turned _GNU_SOURCE on, this breaks strerror_r!"
#endif

void resolverInit (resolverHandle *p, const char *path)
{
	p->fd = -1;
	p->mtime = 0;
	p->mode = KDB_FILE_MODE;

	p->filename = 0;
	p->tempfile = 0;
	p->lockfile = 0;

	p->path = path;
}

void resolverClose (resolverHandle *p)
{
	free (p->filename); p->filename = 0;
	free (p->lockfile); p->lockfile = 0;
	free (p->tempfile); p->tempfile = 0;
}


int elektraResolverOpen(Plugin *handle, Key *errorKey)
{

	KeySet *resolverConfig = elektraPluginGetConfig(handle);
	const char *path = keyString(ksLookupByName(resolverConfig, "/path", 0));

	resolverHandles *p = malloc(sizeof(resolverHandles));
	resolverInit (&p->user, path);
	resolverInit (&p->system, path);


	if (!path)
	{
		free (p);
		ELEKTRA_ADD_WARNING(34, errorKey, "Could not find file configuration");
		return -1;
	}

	Key *testKey = keyNew("system", KEY_END);
	if (resolveFilename(testKey, &p->system) == -1)
	{
		resolverClose(&p->user);
		resolverClose(&p->system);
		free (p);
		keyDel (testKey);
		ELEKTRA_ADD_WARNING(35, errorKey, "Could not resolve system key");
		return -1;
	}

	keySetName(testKey, "user");
	if (resolveFilename(testKey, &p->user) == -1)
	{
		resolverClose(&p->user);
		resolverClose(&p->system);
		free (p);
		keyDel (testKey);
		ELEKTRA_ADD_WARNING(35, errorKey, "Could not resolve user key");
		return -1;
	}
	keyDel (testKey);

	elektraPluginSetData(handle, p);

	return 0; /* success */
}

int elektraResolverClose(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	resolverHandles *ps = elektraPluginGetData(handle);

	resolverClose(&ps->user);
	resolverClose(&ps->system);

	free (ps);

	return 0; /* success */
}

int elektraResolverGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	resolverHandles *pks = elektraPluginGetData(handle);
	resolverHandle *pk = 0;
	if (!strncmp(keyName(parentKey), "user", 4)) pk = &pks->user;
	else pk = &pks->system;


	Key *root = keyNew("system/elektra/modules/resolver", KEY_END);

	if (keyRel(root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet *info = ksNew (50, keyNew ("system/elektra/modules/resolver",
				KEY_VALUE, "resolver plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/resolver/constants", KEY_END),
			keyNew ("system/elektra/modules/resolver/constants/KDB_DB_SYSTEM",
				KEY_VALUE, KDB_DB_SYSTEM, KEY_END),
			keyNew ("system/elektra/modules/resolver/constants/KDB_DB_HOME",
				KEY_VALUE, KDB_DB_HOME, KEY_END),
			keyNew ("system/elektra/modules/resolver/constants/KDB_DB_USER",
				KEY_VALUE, KDB_DB_USER, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports", KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/open",
				KEY_FUNC, elektraResolverOpen,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/close",
				KEY_FUNC, elektraResolverClose,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/get",
				KEY_FUNC, elektraResolverGet,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/set",
				KEY_FUNC, elektraResolverSet,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/error",
				KEY_FUNC, elektraResolverError,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/checkfile",
				KEY_FUNC, elektraResolverCheckFile,
				KEY_END),
			keyNew ("system/elektra/modules/resolver/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/description",
				KEY_VALUE, "Dumps complete Elektra Semantics", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/provides",
				KEY_VALUE, "resolver", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/placements",
				KEY_VALUE, "rollback getresolver setresolver commit", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/resolver/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);

	keySetString(parentKey, pk->filename);

	int errnoSave = errno;
	struct stat buf;
	if (stat (pk->filename, &buf) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);

		char *errorText = malloc(
				strlen(pk->filename) + ERROR_SIZE + 60);
		strcpy (errorText, "Storage plugin will try to create missing file \"");
		strcat (errorText, pk->filename);
		strcat (errorText, "\", error was: \"");
		strcat (errorText, buffer);
		strcat (errorText, "\"");
		ELEKTRA_ADD_WARNING(29, parentKey, errorText);
		free (errorText);

		errno = errnoSave;
		return 0;
		/* File not there, lets assume thats ok. */
	}

	/* Check if update needed */
	if (pk->mtime == buf.st_mtime) return 0;

	pk->mtime = buf.st_mtime;

	return 1;
}

/**
 * @brief Create pathname recursively.
 *
 * Try unless the whole path was
 * created or it is sure that it cannot be done.
 *
 * @param pathname The path to create.
 * @param mode The mode to use for the directories.
 *
 * @retval 0 on success
 * @retval -1 on error, see errno if mkdir caused the error
 *         E2BIG if no / was found
 *         EBADMSG if it was not an absolute path
 */
static int elektraMkdirParents(const char *pathname)
{
	if (mkdir(pathname, KDB_DIR_MODE | KDB_FILE_MODE) < 0)
	{
		if (errno != ENOENT)
		{
			return -1;
		}

		char *p = strrchr(pathname, '/');

		/* nothing found */
		if (p == NULL)
		{
			errno = E2BIG;
			return -1;
		}

		/* absolute path */
		if (p == pathname)
		{
			errno = EBADMSG;
			return -1;
		}

		/* Cut path at last /. */
		*p = 0;

		/* Now call ourselves recursively */
		if (elektraMkdirParents(pathname) < 0)
		{
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, KDB_DIR_MODE | KDB_FILE_MODE) < 0)
		{
			return -1;
		}
	}
	return 0;
}

int elektraResolverSet(Plugin *handle, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{
	resolverHandles *pks = elektraPluginGetData(handle);
	resolverHandle *pk = 0;
	if (!strncmp(keyName(parentKey), "user", 4)) pk = &pks->user;
	else pk = &pks->system;

	int errnoSave = errno;
	int action = 0;

	if (pk->fd == -1)
	{
		/* no fd up to now, so do action 0 */
		action = 0;
	}
	else
	{
		action = 1;
	}

	if (action == 0)
	{
		int successful_stat = 1;
		struct stat buf;
		char * cname = strdup (pk->lockfile);
		char * dname = dirname (cname);

		if (stat(dname, &buf) == -1)
		{
			successful_stat = 0;

			if (errno == ENOENT)
			{
				/* Do not fail, but try to create directory
				 * afterwards.*/
				char buffer[ERROR_SIZE];
				strerror_r(errno, buffer, ERROR_SIZE);

				char *errorText = malloc(
						strlen(pk->filename) + ERROR_SIZE + 60);
				strcpy (errorText, "No configuration directory \"");
				strcat (errorText, dname);
				strcat (errorText, "\" found: \"");
				strcat (errorText, buffer);
				strcat (errorText, "\". Will try to create one.");
				ELEKTRA_ADD_WARNING(72, parentKey, errorText);
				free (errorText);

				errno = errnoSave;
				/* note: cname is not freed() here on purpose*/
			}
			else
			{
				/* There is no hope, everything else
				 * then ENOENT is fatal.*/
				char buffer[ERROR_SIZE];
				int error_ret = strerror_r(errno, buffer, ERROR_SIZE-2);

				char *errorText = malloc(
						strlen(pk->filename) + ERROR_SIZE + 60);
				strcpy (errorText, "Would not be possible to create a directory \"");
				strcat (errorText, dname);
				strcat (errorText, "\" ");
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
						strcat (errorText, "strerror_r returned wrong error value");
					}
				}
				else
				{
					strcat (errorText, "because stat said: \"");
					strcat (errorText, buffer);
					strcat (errorText, "\" ");
				}
				snprintf (buffer, ERROR_SIZE-2, "uid: %u, euid: %u, gid: %u, egid: %u", getuid(), geteuid(), getgid(), getegid());
				strcat (errorText, buffer);
				ELEKTRA_SET_ERROR(74, parentKey, errorText);
				free (errorText);

				free (cname);

				errno = errnoSave;

				return -1;
			}
		}

		if (successful_stat && !S_ISDIR(buf.st_mode))
		{
			char *errorText = malloc(
					strlen(pk->filename) + ERROR_SIZE + 60);
			strcpy (errorText, "Existing configuration directory \"");
			strcat (errorText, cname);
			strcat (errorText, "\" is not a directory.");
			ELEKTRA_SET_ERROR(73, parentKey, errorText);
			free (errorText);

			free (cname);

			return -1;
		}

		if ((!successful_stat) && (elektraMkdirParents(dname) == -1))
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE-2);

			char *errorText = malloc(
					strlen(pk->filename) + ERROR_SIZE + 60);
			strcpy (errorText, "Could not create \"");
			strcat (errorText, dname);
			strcat (errorText, "\", because: \"");
			strcat (errorText, buffer);
			strcat (errorText, "\" ");
			snprintf (buffer, ERROR_SIZE-2, "uid: %u, euid: %u, gid: %u, egid: %u", getuid(), geteuid(), getgid(), getegid());
			strcat (errorText, buffer);
			ELEKTRA_SET_ERROR(74, parentKey, errorText);
			free (errorText);

			free (cname);

			errno = errnoSave;

			return -1;
		}

		free (cname);

		// now the directory exists and we can try to open lock file

		pk->fd = open (pk->lockfile, O_RDWR | O_CREAT, pk->mode);

		if (pk->fd == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);

			char *errorText = malloc(
					strlen(pk->filename) + ERROR_SIZE + 60);
			strcpy (errorText, "Opening lockfile \"");
			strcat (errorText, pk->lockfile);
			strcat (errorText, "\" failed, error was: \"");
			strcat (errorText, buffer);
			strcat (errorText, "\"");
			ELEKTRA_SET_ERROR(26, parentKey, errorText);
			free (errorText);

			errno = errnoSave;
			return -1;
		}

		if (elektraWriteLock(pk->fd) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR(27, parentKey, buffer);
			close(pk->fd);
			errno = errnoSave;
			return -1;
		}

		if (fchmod(pk->fd, pk->mode) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (28, parentKey, buffer);
			close(pk->fd);
			errno = errnoSave;
			return -1;
		}

		keySetString(parentKey, pk->tempfile);

		if (stat(pk->filename, &buf) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);

			char *errorText = malloc(
					strlen(pk->filename) + ERROR_SIZE + 60);
			strcpy (errorText, "No configuration file \"");
			strcat (errorText, pk->filename);
			strcat (errorText, "\" found: \"");
			strcat (errorText, buffer);
			strcat (errorText, "\". Will try to create one.");
			ELEKTRA_ADD_WARNING(29, parentKey, errorText);
			free (errorText);

			errno = errnoSave;
			/* Dont fail if configuration file currently does not exist */
			return 0;
		}

		if (buf.st_mtime > pk->mtime)
		{
			ELEKTRA_SET_ERROR (30, parentKey, "file time stamp is too new");
			close(pk->fd);
			return -1;
		}

		return 1;
	}

	if (action == 1)
	{
		int ret = 0;

		if (rename (pk->tempfile, pk->filename) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (31, parentKey, buffer);
			errno = errnoSave;
			ret = -1;
		}

		struct stat buf;
		if (stat (pk->filename, &buf) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING (29, parentKey, buffer);
			errno = errnoSave;
		} else {
			/* Update timestamp */
			pk->mtime = buf.st_mtime;
		}

		if (elektraUnlock(pk->fd) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING(32, parentKey, buffer);
			errno = errnoSave;
		}


		if (close (pk->fd) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING (33, parentKey, buffer);
			errno = errnoSave;
		}

		pk->fd = -1;

		return ret;
	}

	return 0;
}

int elektraResolverError(Plugin *handle, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraPluginGetData(handle);

	if (unlink (pk->tempfile) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING(36, parentKey, buffer);
		errno = errnoSave;
	}

	if (pk->fd == -1) return 0;

	if (elektraUnlock(pk->fd) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING(32, parentKey, buffer);
		errno = errnoSave;
	}


	if (close (pk->fd) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING (33, parentKey, buffer);
		errno = errnoSave;
	}

	pk->fd = -1;

	return 0;
}

/**
 * @return 1 on success (Relative path)
 * @returns 0 on success (Absolute path)
 * @return -1 on a non-valid file
 */
int elektraResolverCheckFile (const char* filename)
{
	if (!filename) return -1;
	if (filename[0] == '0') return -1;

	size_t size = strlen(filename);
	char *buffer = malloc (size + sizeof ("system/"));
	strcpy (buffer, "system/");
	strcat (buffer, filename);

	/* Because of the outbreak bugs these tests are not enough */
	Key *check = keyNew (buffer, KEY_END);
	if (!strcmp(keyName(check), "")) goto error;
	if (!strcmp(keyName(check), "system")) goto error;
	keyDel (check);
	free (buffer);

	/* Be strict, dont allow any .., even if it would be allowed sometimes */
	if (strstr (filename, "..") != 0) return -1;


	if (filename[0] == '/') return 0;

	return 1;

error:
	keyDel (check);
	free (buffer);
	return -1;
}


Plugin *ELEKTRA_PLUGIN_EXPORT(resolver)
{
	return elektraPluginExport("resolver",
		ELEKTRA_PLUGIN_OPEN,	&elektraResolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraResolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraResolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraResolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraResolverError,
		ELEKTRA_PLUGIN_END);
}

