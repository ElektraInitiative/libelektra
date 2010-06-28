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


int elektraResolverOpen(Plugin *handle, Key *errorKey)
{

	KeySet *resolverConfig = elektraPluginGetConfig(handle);

	resolverHandle *p = malloc(sizeof(resolverHandle));
	p->fd = -1;
	p->mtime = 0;
	p->mode = 0664;

	p->filename = 0;
	p->userFilename = 0;
	p->systemFilename = 0;

	p->path = keyString(ksLookupByName(resolverConfig, "/path", 0));

	if (!p->path)
	{
		free (p);
		ELEKTRA_ADD_WARNING(34, errorKey, "Could not find file configuration");
		return -1;
	}

	Key *testKey = keyNew("system", KEY_END);
	if (resolveFilename(testKey, p) == -1)
	{
		free (p);
		keyDel (testKey);
		ELEKTRA_ADD_WARNING(35, errorKey, "Could not resolve system key");
		return -1;
	}

	keySetName(testKey, "user");
	if (resolveFilename(testKey, p) == -1)
	{
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
	resolverHandle *p = elektraPluginGetData(handle);
	free (p->userFilename);
	free (p->systemFilename);
	free (p);

	return 0; /* success */
}

int elektraResolverGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	struct stat buf;

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

	resolverHandle *pk = elektraPluginGetData(handle);
	resolveFilename(parentKey, pk);

	int errnoSave = errno;
	if (stat (pk->filename, &buf) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_SET_ERROR (29, parentKey, buffer);
		close(pk->fd);
		errno = errnoSave;
		return -1;
	}

	pk->mtime = buf.st_mtime;
	/* TODO check if update is necessary, not supported by mainloop yet */

	keySetString(parentKey, pk->filename);

	return 1;
}

int elektraResolverSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraPluginGetData(handle);
	int action;

	if (pk->fd == -1)
	{
		/* no fd up to now, so do action 0 */
		action = 0;
	} else  action = 1;

	if (action == 0)
	{
		struct stat buf;
		pk->fd = open ("lock", O_RDWR | O_CREAT);

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

		if (stat(pk->filename, &buf) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (29, parentKey, buffer);
			close(pk->fd);
			errno = errnoSave;
			return -1;
		}

		if (buf.st_mtime > pk->mtime)
		{
			ELEKTRA_SET_ERROR (30, parentKey, "file time stamp is too new");
			close(pk->fd);
			return -1;
		}

		keySetString(parentKey, "tmp");

		return 0;
	}

	if (action == 1)
	{
		if (rename ("tmp", pk->filename) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (31, parentKey, buffer);
			errno = errnoSave;
			return -1;
		}
	}

	/* Always execute the rollback code */
	elektraResolverError(handle, returned, parentKey);

	return 0;
}

int elektraResolverError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraPluginGetData(handle);

	if (unlink ("tmp") == -1)
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

