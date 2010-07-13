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

void resolverInit (resolverHandle *p, const char *path)
{
	p->fd = -1;
	p->mtime = 0;
	p->mode = 0664;

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

int elektraResolverClose(Plugin *handle, Key *errorKey)
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
				KEY_SIZE, sizeof (&elektraResolverOpen),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverGet, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/close",
				KEY_SIZE, sizeof (&elektraResolverClose),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverGet, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/get",
				KEY_SIZE, sizeof (&elektraResolverGet),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverGet, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/set",
				KEY_SIZE, sizeof (&elektraResolverSet),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverSet, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/error",
				KEY_SIZE, sizeof (&elektraResolverError),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverGet, KEY_END),
			keyNew ("system/elektra/modules/resolver/exports/checkfile",
				KEY_SIZE, sizeof (&elektraResolverCheckFile),
				KEY_BINARY,
				KEY_VALUE, &elektraResolverCheckFile, KEY_END),
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
				KEY_VALUE, BACKENDVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksRewind(returned);

		Key *k;
		while ((k = ksNext(returned)) != 0) keyClearSync(k);
		return ksGetSize(returned);
	}
	keyDel (root);

	keySetString(parentKey, pk->filename);

	int errnoSave = errno;
	struct stat buf;
	if (stat (pk->filename, &buf) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING (29, parentKey, buffer);
		errno = errnoSave;
		return 0;
		/* File not there, lets assume thats ok. */
	}

	/* Check if update needed */
	if (pk->mtime == buf.st_mtime) return 0;

	pk->mtime = buf.st_mtime;

	return 1;
}

int elektraResolverSet(Plugin *handle, KeySet *returned, Key *parentKey)
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
	} else  action = 1;

	if (action == 0)
	{
		struct stat buf;
		pk->fd = open (pk->lockfile, O_RDWR | O_CREAT);

		if (pk->fd == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (26, parentKey, buffer);
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
			ELEKTRA_ADD_WARNING (29, parentKey, buffer);
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

int elektraResolverError(Plugin *handle, KeySet *returned, Key *parentKey)
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
 * @returns 0 on success (Absolut path)
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
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraResolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraResolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraResolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraResolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraResolverError,
		ELEKTRA_PLUGIN_END);
}

