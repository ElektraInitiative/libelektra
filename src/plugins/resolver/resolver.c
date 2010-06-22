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

	elektraPluginSetHandle(handle, p);

	return 0; /* success */
}

int elektraResolverClose(Plugin *handle, Key *errorKey)
{
	resolverHandle *p = elektraPluginGetHandle(handle);
	free (p->userFilename);
	free (p->systemFilename);
	free (p);

	return 0; /* success */
}

int elektraResolverGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	struct stat buf;
	int errnoSave = errno;

	resolverHandle *pk = elektraPluginGetHandle(handle);
	resolveFilename(parentKey, pk);

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

	keySetMeta(parentKey, "path", pk->filename);

	return 1;
}

int elektraResolverSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraPluginGetHandle(handle);
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

		if (fstat(pk->fd, &buf) == -1)
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
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (30, parentKey, buffer);
			close(pk->fd);
			errno = errnoSave;
			return -1;
		}

		keySetMeta(parentKey, "path", pk->filename);

		return 0;
	}

	if (action == 1)
	{
		if (rename ("tmp", "orig") == -1)
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
	resolverHandle *pk = elektraPluginGetHandle(handle);

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

