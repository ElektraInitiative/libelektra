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
 *   Simple fill the empty _resolver functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "resolver.h"


int kdbOpen_resolver(Plugin *handle, Key *errorKey)
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

int kdbClose_resolver(Plugin *handle, Key *errorKey)
{
	resolverHandle *p = elektraPluginGetHandle(handle);
	free (p->userFilename);
	free (p->systemFilename);
	free (p);

	return 0; /* success */
}

int kdbGet_resolver(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* TODO resolve filename + get mtime */
	return 1;
}

int kdbSet_resolver(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnoSave = errno;
	resolverHandle *pk = elektraPluginGetHandle(handle);
	int action;

	if (pk->fd == -1)
	{
		/* no fd up to now, so do action 0 */
		action = 0;
	} else  action = 3;

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
		return 0;
	}

	if (action & 1)
	{
		if (rename ("tmp", "orig") == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_SET_ERROR (31, parentKey, buffer);
			errno = errnoSave;
			/* Continue with unlocking */
		}
	}

	if (action & 2)
	{
		if (elektraUnlock(pk->fd) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING(32, parentKey, buffer);
			/* Continue with closing file */
		}


		if (close (pk->fd) == -1)
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING (33, parentKey, buffer);
			pk->fd = -1;
			return -1;
		}
		pk->fd = -1;
	}
	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(resolver)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&kdbOpen_resolver,
		ELEKTRA_PLUGIN_CLOSE,	&kdbClose_resolver,
		ELEKTRA_PLUGIN_GET,	&kdbGet_resolver,
		ELEKTRA_PLUGIN_SET,	&kdbSet_resolver,
		ELEKTRA_PLUGIN_END);
}

