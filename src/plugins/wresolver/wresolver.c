/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif


#include <elektra/core/errors.h>
#include <internal/macros/attributes.h>
#include <internal/macros/os.h>
#include <internal/utility/assert.h>
#include <internal/utility/old_helper.h>

#include <string.h>

#include "./wresolver.h"
#include <errno.h> /* errno in getcwd() */
#include <stdlib.h>
#include <sys/stat.h> /* mkdir() */
#include <unistd.h>   /* getcwd() */

#if defined(_WIN32)
#include <io.h>
#include <shlobj.h>
#include <windows.h>
#endif

/**
 * @retval 1 on success (Relative path)
 * @retval 0 on success (Absolute path)
 * @retval never -1 (success guaranteed)
 */
int elektraWresolverCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	time_t mtime; ///< Previous timestamp of the file
	mode_t mode;  ///< The mode to set
	int state;    ///< 0 .. invalid -> kdbGet -> 1 .. ready to set -> kdbSet -> 2

	char * filename; ///< the full path to the configuration file

	const char * path;
};

typedef struct _resolverHandles resolverHandles;

struct _resolverHandles
{
	resolverHandle spec;
	resolverHandle dir;
	resolverHandle user;
	resolverHandle system;
};

static resolverHandle * elektraGetResolverHandle (Plugin * handle, Key * parentKey)
{
	resolverHandles * pks = elektraPluginGetData (handle);
	switch (keyGetNamespace (parentKey))
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
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_DEFAULT:
		break;
	}
	ELEKTRA_ASSERT (0, "namespace %d not valid for resolving", keyGetNamespace (parentKey));
	return 0;
}


static void resolverClose (resolverHandle * p)
{
	elektraFree (p->filename);
	p->filename = 0;
}

static void resolverInit (resolverHandle * p, const char * path)
{
	p->mtime = 0;
	p->mode = 0;
	p->state = 0;

	p->filename = 0;

	p->path = path;
}

static void escapePath (char * home)
{
	int len = strlen (home), i;
	for (i = 0; i < len; ++i)
	{
		if (home[i] == '\\')
		{
			home[i] = '/';
		}
	}
}

static void elektraResolveSpec (resolverHandle * p, Key * errorKey)
{
	char * system = getenv ("ALLUSERSPROFILE");

	if (!system)
	{
		system = "";
		ELEKTRA_ADD_INSTALLATION_WARNING (
			errorKey, "Could not retrieve from passwd using getpwuid_r. Could not get ALLUSERSPROFILE for spec, using /");
	}
	else
	{
		escapePath (system);
	}


	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen (system) + strlen (p->path) + 1;
		p->filename = elektraMalloc (filenameSize);
		strcpy (p->filename, system);
		strcat (p->filename, p->path);
		return;
	}
	size_t filenameSize = sizeof (KDB_DB_SPEC) + strlen (system) + strlen (p->path) + sizeof ("/") + 1;
	p->filename = elektraMalloc (filenameSize);
	strcpy (p->filename, system);
	strcat (p->filename, KDB_DB_SPEC);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
	return;
}

static void elektraResolveDir (resolverHandle * p, Key * warningsKey)
{
	p->filename = elektraMalloc (KDB_MAX_PATH_LENGTH);

#if defined(_WIN32)
	CHAR dir[MAX_PATH];
	DWORD dwRet = GetCurrentDirectory (MAX_PATH, dir);
	if (dwRet == 0)
	{
		char buf[256];
		FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError (), MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT), buf, 256,
			       NULL);
		ELEKTRA_ADD_RESOURCE_WARNINGF (warningsKey, "GetCurrentDirectory failed: %s, defaulting to /", buf);
		dir[0] = 0;
	}
	else if (dwRet > MAX_PATH)
	{
		// TODO: Solution? Compile with higher MAX_PATH or submit a bug to the bug tracker and let us do it for you
		ELEKTRA_ADD_INSTALLATION_WARNINGF (warningsKey, "GetCurrentDirectory failed, buffer size too small, needed: %ld", dwRet);
		dir[0] = 0;
	}
	escapePath (dir);
#else
	char dir[KDB_MAX_PATH_LENGTH];
	if (getcwd (dir, KDB_MAX_PATH_LENGTH) == 0)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (warningsKey, "Command 'getcwd' failed. Defaulting to /. Reason: %s", strerror (errno));
		dir[0] = 0;
	}
#endif

	strcpy (p->filename, dir);
	strcat (p->filename, "/");
	strncat (p->filename, p->path, KDB_MAX_PATH_LENGTH - strlen (dir) - 3);
	p->filename[KDB_MAX_PATH_LENGTH - 1] = 0;

	return;
}

static void elektraResolveUser (resolverHandle * p, Key * warningsKey)
{
	p->filename = elektraMalloc (KDB_MAX_PATH_LENGTH);

#if defined(_WIN32)
	CHAR home[MAX_PATH];
	if (SUCCEEDED (SHGetFolderPath (NULL, CSIDL_PROFILE, NULL, 0, home)))
	{
		escapePath (home);
	}
	else
	{
		strcpy (home, "");
		ELEKTRA_ADD_INSTALLATION_WARNING (warningsKey, "Could not get home (CSIDL_PROFILE), using /");
	}
#else
	char * home = (char *) getenv ("HOME");
	if (!home)
	{
		home = "";
		ELEKTRA_ADD_INSTALLATION_WARNING (warningsKey, "Could not get HOME environment variable, using /");
	}
#endif

	strcpy (p->filename, home);
	strcat (p->filename, "/");
	strncat (p->filename, p->path, KDB_MAX_PATH_LENGTH);
}

static void elektraResolveSystem (resolverHandle * p, Key * errorKey)
{
	char * system = getenv ("ALLUSERSPROFILE");

	if (!system)
	{
		system = "";
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not get ALLUSERSPROFILE environment variable, using /");
	}
	else
	{
		escapePath (system);
	}

	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen (system) + strlen (p->path) + 1;
		p->filename = elektraMalloc (filenameSize);
		strcpy (p->filename, system);
		strcat (p->filename, p->path);
		return;
	}
	size_t filenameSize = sizeof (KDB_DB_SYSTEM) + strlen (system) + strlen (p->path) + sizeof ("/") + 1;
	p->filename = elektraMalloc (filenameSize);
	strcpy (p->filename, system);
	strcat (p->filename, KDB_DB_SYSTEM);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
	return;
}

static int initHandles (Plugin * handle, Key * parentKey)
{
	const char * path = keyString (parentKey);

	resolverHandles * p = elektraMalloc (sizeof (resolverHandles));

	// switch is only present to forget no namespace and to get
	// a warning whenever a new namespace is present.
	// (also used below in close)
	// In fact its linear code executed:
	switch (KEY_NS_SPEC)
	{
	case KEY_NS_SPEC:
		resolverInit (&p->spec, path);
		elektraResolveSpec (&p->spec, parentKey);
	// FALLTHROUGH
	case KEY_NS_DIR:
		resolverInit (&p->dir, path);
		elektraResolveDir (&p->dir, parentKey);
	// FALLTHROUGH
	case KEY_NS_USER:
		resolverInit (&p->user, path);
		elektraResolveUser (&p->user, parentKey);
	// FALLTHROUGH
	case KEY_NS_SYSTEM:
		resolverInit (&p->system, path);
		elektraResolveSystem (&p->system, parentKey);
	// FALLTHROUGH
	case KEY_NS_PROC:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_DEFAULT:
		break;
	}

	elektraPluginSetData (handle, p);

	return 0; /* success */
}

int elektraWresolverOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	elektraPluginSetData (handle, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraWresolverClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	resolverHandles * ps = elektraPluginGetData (handle);

	if (ps)
	{
		switch (KEY_NS_SPEC)
		{
		case KEY_NS_SPEC:
			resolverClose (&ps->spec); // FALLTHROUGH
		case KEY_NS_DIR:
			resolverClose (&ps->dir); // FALLTHROUGH
		case KEY_NS_USER:
			resolverClose (&ps->user); // FALLTHROUGH
		case KEY_NS_SYSTEM:
			resolverClose (&ps->system); // FALLTHROUGH
		case KEY_NS_PROC:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
		case KEY_NS_DEFAULT:
			break;
		}

		elektraFree (ps);
		elektraPluginSetData (handle, 0);
	}

	return 0; /* success */
}

int elektraWresolverGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/wresolver"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/wresolver", KEY_VALUE, "wresolver plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports", KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/open", KEY_FUNC, elektraWresolverOpen, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/close", KEY_FUNC, elektraWresolverClose, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/get", KEY_FUNC, elektraWresolverGet, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/set", KEY_FUNC, elektraWresolverSet, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/commit", KEY_FUNC, elektraWresolverCommit, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/error", KEY_FUNC, elektraWresolverError, KEY_END),
			keyNew ("system:/elektra/modules/wresolver/exports/checkfile", KEY_FUNC, elektraWresolverCheckFile, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/wresolver/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	if (elektraPluginGetData (handle) == NULL)
	{
		if (initHandles (handle, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);
	keySetString (parentKey, pk->filename);

	pk->state = 1;

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

int elektraWresolverSet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);
	keySetString (parentKey, pk->filename);

	switch (pk->state)
	{
	case 0:
		ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, "Command 'kdbSet()' called before 'kdbGet()'");
		return -1;
	case 1:
		++pk->state;
		break;
	case 2:
		pk->state = 1;
		// nothing to do on commit
		return 1;
	}

	/* set all keys */
	if (pk->mtime == 0)
	{
		// this can happen if the kdbGet() path found no file

		// no conflict possible, so just return successfully
		return 1;
	}

	struct stat buf;

	if (stat (pk->filename, &buf) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not stat config file '%s'", pk->filename);
		// no file found, nothing to do
		return 0;
	}

	/* Check for conflict */
	if (pk->mtime != buf.st_mtime)
	{
		// conflict
		ELEKTRA_SET_CONFLICTING_STATE_ERRORF (
			parentKey,
			"Conflict, file modification time stamp %ld is different than our time stamp %ld config file name is '%s'",
			(long) buf.st_mtime, (long) pk->mtime, pk->filename);
		pk->state = 0; // invalid state, need to kdbGet again
		return -1;
	}

	pk->mtime = buf.st_mtime;

	return 1; /* success */
}

int elektraWresolverCommit (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	return elektraWresolverSet (handle, returned, parentKey);
}

int elektraWresolverError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);
	pk->state = 1;


	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("wresolver",
		ELEKTRA_PLUGIN_OPEN,	&elektraWresolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraWresolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraWresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraWresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraWresolverError,
		ELEKTRA_PLUGIN_COMMIT,	&elektraWresolverCommit,
		ELEKTRA_PLUGIN_END);
}

