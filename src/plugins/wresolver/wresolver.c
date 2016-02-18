/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdbproposal.h>

#include <kdberrors.h>

#include <string.h>

#include "wresolver.h"
#include <sys/stat.h> /* mkdir() */
#include <stdlib.h>
#include <unistd.h> /* getcwd() */
#include <errno.h> /* errno in getcwd() */

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
	resolverHandle spec;
	resolverHandle dir;
	resolverHandle user;
	resolverHandle system;
};

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
		break;
	}
	// ELEKTRA_ASSERT(0 && "namespace not valid for resolving");
	return 0;
}


static void resolverClose (resolverHandle *p)
{
	elektraFree (p->filename); p->filename = 0;
}

static void resolverInit (resolverHandle *p, const char *path)
{
	p->mtime = 0;
	p->mode = 0;

	p->filename = 0;

	p->path = path;
}

static void escapePath(char *home)
{
	int len = strlen(home), i;
	for (i=0; i < len; ++i)
	{
		if (home[i] == '\\')
		{
			home[i] = '/';
		}
	}
}

static void elektraResolveSpec(resolverHandle *p, Key *errorKey)
{
	char * system = getenv("ALLUSERSPROFILE");

	if (!system)
	{
		system = "";
		ELEKTRA_ADD_WARNING(90, errorKey, "could not get ALLUSERSPROFILE for spec, using /");
	}
	else escapePath(system);


	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen(system)
			+ strlen(p->path) + 1;
		p->filename = elektraMalloc (filenameSize);
		strcpy (p->filename, system);
		strcat (p->filename, p->path);
		return;
	}
	size_t filenameSize = sizeof(KDB_DB_SPEC)
		+ strlen(system) + strlen(p->path) + sizeof("/") + 1;
	p->filename = elektraMalloc (filenameSize);
	strcpy (p->filename, system);
	strcat (p->filename, KDB_DB_SPEC);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
	return;
}

static void elektraResolveDir(resolverHandle *p, Key *warningsKey)
{
	p->filename = elektraMalloc(KDB_MAX_PATH_LENGTH);

# if defined(_WIN32)
	CHAR dir[MAX_PATH];
	DWORD dwRet = GetCurrentDirectory(MAX_PATH, dir);
	if (dwRet == 0)
	{
		ELEKTRA_ADD_WARNINGF(90, warningsKey, "GetCurrentDirectory failed: %s", GetLastError());
	}
	else if (dwRet > MAX_PATH)
	{
		ELEKTRA_ADD_WARNINGF(90, warningsKey, "GetCurrentDirectory failed, buffer size too small, needed: %ld", dwRet);
	}
	escapePath(dir);
#else
	char dir[KDB_MAX_PATH_LENGTH];
	if (getcwd(dir, KDB_MAX_PATH_LENGTH) == 0)
	{
		ELEKTRA_ADD_WARNINGF(90, warningsKey, "getcwd failed: %s", strerror(errno));
	}
#endif

	strcpy (p->filename, dir);
	strcat (p->filename, "/");
	strncat (p->filename, p->path, KDB_MAX_PATH_LENGTH-strlen(dir)-3);
	p->filename[KDB_MAX_PATH_LENGTH-1] = 0;

	return;
}

static void elektraResolveUser(resolverHandle *p, Key *warningsKey)
{
	p->filename = elektraMalloc(KDB_MAX_PATH_LENGTH);

# if defined(_WIN32)
	CHAR home[MAX_PATH];
	if (SUCCEEDED(SHGetFolderPath(NULL, CSIDL_PROFILE, NULL,
					0, home)))
	{
		escapePath(home);
	}
	else
	{
		strcpy(home, "");
		ELEKTRA_ADD_WARNING(90, warningsKey, "could not get home (CSIDL_PROFILE), using /");
	}
# else
	char * home = (char*) getenv("HOME");
	if (!home)
	{
		home = "";
		ELEKTRA_ADD_WARNING(90, warningsKey, "could not get home, using /");
	}
# endif

	strcpy (p->filename, home);
	strcat (p->filename, "/");
	strncat (p->filename, p->path, KDB_MAX_PATH_LENGTH);
}

static void elektraResolveSystem(resolverHandle *p, Key *errorKey)
{
	char * system = getenv("ALLUSERSPROFILE");

	if (!system)
	{
		system = "";
		ELEKTRA_ADD_WARNING(90, errorKey, "could not get ALLUSERSPROFILE, using /");
	}
	else escapePath(system);

	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen(system)
			+ strlen(p->path) + 1;
		p->filename = elektraMalloc (filenameSize);
		strcpy (p->filename, system);
		strcat (p->filename, p->path);
		return;
	}
	size_t filenameSize = sizeof(KDB_DB_SYSTEM)
		+ strlen(system) + strlen(p->path) + sizeof("/") + 1;
	p->filename = elektraMalloc (filenameSize);
	strcpy (p->filename, system);
	strcat (p->filename, KDB_DB_SYSTEM);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
	return;
}

int elektraWresolverOpen(Plugin *handle, Key *errorKey)
{
	KeySet *resolverConfig = elektraPluginGetConfig(handle);
	const char *path = keyString(ksLookupByName(resolverConfig, "/path", 0));

	if (!path)
	{
		ELEKTRA_SET_ERROR(34, errorKey, "Could not find file configuration");
		return -1;
	}

	resolverHandles *p = elektraMalloc(sizeof(resolverHandles));

	// switch is only present to forget no namespace and to get
	// a warning whenever a new namespace is present.
	// (also used below in close)
	// In fact its linear code executed:
	switch (KEY_NS_SPEC)
	{
	case KEY_NS_SPEC:
		resolverInit (&p->spec, path);
		elektraResolveSpec(&p->spec, errorKey);
	case KEY_NS_DIR:
		resolverInit (&p->dir, path);
		elektraResolveDir(&p->dir, errorKey);
	case KEY_NS_USER:
		resolverInit (&p->user, path);
		elektraResolveUser(&p->user, errorKey);
	case KEY_NS_SYSTEM:
		resolverInit (&p->system, path);
		elektraResolveSystem(&p->system, errorKey);
	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		break;
	}

	elektraPluginSetData(handle, p);

	return 0; /* success */
}

int elektraWresolverClose(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	resolverHandles *ps = elektraPluginGetData(handle);

	if (ps)
	{
		switch (KEY_NS_SPEC)
		{
		case KEY_NS_SPEC:
			resolverClose(&ps->spec);
		case KEY_NS_DIR:
			resolverClose(&ps->dir);
		case KEY_NS_USER:
			resolverClose(&ps->user);
		case KEY_NS_SYSTEM:
			resolverClose(&ps->system);
		case KEY_NS_PROC:
		case KEY_NS_EMPTY:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			break;
		}

		elektraFree (ps);
		elektraPluginSetData(handle, 0);
	}

	return 0; /* success */
}

int elektraWresolverGet(Plugin *handle, KeySet *returned, Key *parentKey)
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

int elektraWresolverSet(Plugin *handle, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{
	resolverHandle *pk = elektraGetResolverHandle(handle, parentKey);
	keySetString(parentKey, pk->filename);

	/* set all keys */
	if (pk->mtime == 0)
	{
		// this can happen if the kdbGet() path found no file

		// no conflict possible, so just return successfully
		return 0;
	}

	struct stat buf;

	if (stat (pk->filename, &buf) == -1)
	{
		ELEKTRA_ADD_WARNINGF (29, parentKey,
				"could not stat config file \"%s\", ",
				pk->filename);
		// no file found
		return 0;
	}

	/* Check for conflict */
	if (pk->mtime != buf.st_mtime)
	{
		// conflict
		ELEKTRA_ADD_WARNINGF (29, parentKey,
				"conflict, file modification time stamp %ld is different than our time stamp %ld, config file name is \"%s\", ",
				buf.st_mtime,
				pk->mtime,
				pk->filename);
		return 1; // stat unreliable for windows, keep it at warning
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
	// clang-format off
	return elektraPluginExport("wresolver",
		ELEKTRA_PLUGIN_OPEN,	&elektraWresolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraWresolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraWresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraWresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraWresolverError,
		ELEKTRA_PLUGIN_END);
}

