/***************************************************************************
                     wresolver.c  -  Skeleton of a plugin
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
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdberrors.h>

#include <string.h>

#include "wresolver.h"
#include <sys/stat.h> /* mkdir() */
#include <stdlib.h>

#if defined(_WIN32)
#include <windows.h>
#include <shlobj.h>
#include <io.h>
#endif

/**
 * @retval 1 on success (Relative path)
 * @retval 0 on success (Absolute path)
 * @retval never -1 (success guaranteed)
 */
int elektraWresolverCheckFile(const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	time_t mtime; ///< Previous timestamp of the file
	mode_t mode;  ///< The mode to set

	char *filename;///< the full path to the configuration file

	const char *path;
};

typedef struct _resolverHandles resolverHandles;

struct _resolverHandles
{
	resolverHandle user;
	resolverHandle system;
};

static resolverHandle * elektraGetResolverHandle(Plugin *handle, Key *parentKey)
{
	resolverHandles *pks = elektraPluginGetData(handle);
	if (!strncmp(keyName(parentKey), "user", 4)) return &pks->user;
	else return &pks->system;
}


static void resolverClose (resolverHandle *p)
{
	free (p->filename); p->filename = 0;
}

static void resolverInit (resolverHandle *p, const char *path)
{
	p->mtime = 0;
	p->mode = 0;

	p->filename = 0;

	p->path = path;
}

static void elektraResolveSystem(resolverHandle *p)
{
	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen(p->path) + 1;
		p->filename = malloc (filenameSize);
		strcpy (p->filename, p->path);
		return;
	}
	size_t filenameSize = sizeof(KDB_DB_SYSTEM)
		+ strlen(p->path) + sizeof("/") + 1;
	p->filename = malloc (filenameSize);
	strcpy (p->filename, KDB_DB_SYSTEM);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
	return;
}

void elektraWresolveFileName(Key *forKey, resolverHandle *p, Key *warningsKey)
{
	if (!strncmp(keyName(forKey), "system", 6))
	{
		elektraResolveSystem(p);
	}
	else if (!strncmp(keyName(forKey), "user", 4))
	{
		p->filename = malloc(PATH_MAX);

# if defined(_WIN32)
		CHAR home[MAX_PATH];
		if(SUCCEEDED(SHGetFolderPath(NULL, CSIDL_PROFILE, NULL,
						0, home)))
		{
			int len = strlen(home), i;
			for(i=0; i < len; ++i)
			{

				if(home[i] == '\\')
				{
					home[i] = '/';
				}
			}
		}
		else
		{
			ELEKTRA_ADD_WARNING(90, warningsKey, "could not get home");
		}
# else
		char * home = (char*) getenv("HOME");
		if(!home)
		{
			ELEKTRA_ADD_WARNING(90, warningsKey, "could not get home");
		}
# endif

		strcpy (p->filename, home);
		strcat (p->filename, "/");
		strncat (p->filename, p->path, PATH_MAX);
	}
}

int elektraWresolverOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
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
	elektraWresolveFileName(testKey, &p->system, errorKey);
	keySetName(testKey, "user");
	elektraWresolveFileName(testKey, &p->user, errorKey);
	keyDel (testKey);

	elektraPluginSetData(handle, p);

	return 0; /* success */
}

int elektraWresolverClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
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

	return 1; /* success */
}

int elektraWresolverGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/wresolver"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/wresolver",
			KEY_VALUE, "wresolver plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports", KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/open",
			KEY_FUNC, elektraWresolverOpen, KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/close",
			KEY_FUNC, elektraWresolverClose, KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/get",
			KEY_FUNC, elektraWresolverGet, KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/set",
			KEY_FUNC, elektraWresolverSet, KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/error",
			KEY_FUNC, elektraWresolverError, KEY_END),
		keyNew ("system/elektra/modules/wresolver/exports/checkfile",
			KEY_FUNC, elektraWresolverCheckFile,
			KEY_END),
#include ELEKTRA_README(wresolver)
		keyNew ("system/elektra/modules/wresolver/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);
	keySetString(parentKey, pk->filename);

	struct stat buf;

	if (stat (pk->filename, &buf) == -1)
	{
		// no file, so storage has no job
		pk->mtime = 0; // no file, so no time
		return 0;
	}

	/* Check if update needed */
	if (pk->mtime == buf.st_mtime)
	{
		// no update, so storage has no job
		return 0;
	}

	pk->mtime = buf.st_mtime;

	return 1; /* success */
}

int elektraWresolverSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);
	keySetString(parentKey, pk->filename);

	/* set all keys */
	if (pk->mtime== 0)
	{
		// this can happen if the kdbGet() path found no file

		// no conflict possible, so just return successfully
		return 0;
	}

	struct stat buf;

	if (stat (pk->filename, &buf) == -1)
	{
		// no file found
		return 0;
	}

	/* Check for conflict */
	if (pk->mtime != buf.st_mtime)
	{
		// conflict
		ELEKTRA_SET_ERRORF (30, parentKey,
				"conflict, file modification time stamp %ld is different than our time stamp %ld, config file name is \"%s\", ",
				buf.st_mtime,
				pk->mtime,
				pk->filename);
		return -1;
	}

	pk->mtime = buf.st_mtime;

	return 1; /* success */
}

int elektraWresolverError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(wresolver)
{
	return elektraPluginExport("wresolver",
		ELEKTRA_PLUGIN_OPEN,	&elektraWresolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraWresolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraWresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraWresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraWresolverError,
		ELEKTRA_PLUGIN_END);
}

