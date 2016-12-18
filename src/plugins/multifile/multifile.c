/**
 * @file
 *
 * @brief Source for multifile plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "multifile.h"

#include <dirent.h>
#include <fnmatch.h>
#include <kdbconfig.h>
#include <kdbhelper.h>
#include <kdbinternal.h>
#include <kdbmodule.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define DEFAULT_RESOLVER "resolver"
#define DEFAULT_PATTERN "*"
#define DEFAULT_STORAGE "ini"

typedef enum {
	SETRESOLVER = 0,
	SETSTORAGE,
	COMMIT,
} SetPhases;

typedef enum {
	GETRESOLVER = 0,
	GETSTORAGE,
} GetPhases;

typedef enum {
	EMPTY = -2,
	ERROR = -1,
	NOUPDATE = 0,
	SUCCESS = 1,
} Codes;


typedef struct
{
	char * directory;
	char * pattern;
	SetPhases setPhase;
	GetPhases getPhase;
	KeySet * modules;
	KeySet * childBackends;
	char * resolver;
	char * storage;
	unsigned short stayAlive;
	unsigned short hasDeleted;
} MultiConfig;

typedef struct
{
	char * filename;
	char * fullPath;
	char * parentString;
	char * tmpFilename;
	Plugin * resolver;
	Codes rcResolver;
	Plugin * storage;
	Codes rcStorage;
	KeySet * ks;
} SingleConfig;

static inline Codes rvToRc (int rc)
{
	switch (rc)
	{
	case -2:
		return EMPTY;
		break;
	case -1:
		return ERROR;
		break;
	case 0:
		return NOUPDATE;
		break;
	case 1:
		return SUCCESS;
		break;
	}
	return ERROR;
}

int elektraMultifileCheckFile (const char * filename)
{
	if (!filename) return -1;
	if (filename[0] == '/') return 0;
	return 1;
}

int elektraMultifileOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

static void closeBackend (SingleConfig * s)
{
	if (!s) return;
	if (s->fullPath) elektraFree (s->fullPath);
	if (s->filename) elektraFree (s->filename);
	if (s->parentString) elektraFree (s->parentString);
	if (s->tmpFilename) elektraFree (s->tmpFilename);
	if (s->resolver) elektraPluginClose (s->resolver, NULL);
	if (s->storage) elektraPluginClose (s->storage, NULL);
	if (s->ks) ksDel (s->ks);
	elektraFree (s);
	s = NULL;
}

static void closeBackends (KeySet * b)
{
	ksRewind (b);
	Key * k;
	while ((k = ksNext (b)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		// fprintf (stderr, "closing backend %s:(%s)\n", s->parentString, s->fullPath);
		closeBackend (s);
	}
}

int elektraMultifileClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return 1;
	closeBackends (mc->childBackends);
	if (mc->directory) elektraFree (mc->directory);
	if (mc->pattern) elektraFree (mc->pattern);
	if (mc->resolver) elektraFree (mc->resolver);
	if (mc->storage) elektraFree (mc->storage);
	elektraModulesClose (mc->modules, NULL);
	ksDel (mc->modules);
	ksDel (mc->childBackends);
	elektraFree (mc);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

static MultiConfig * initialize (Plugin * handle)
{
	KeySet * config = elektraPluginGetConfig (handle);
	const char * path = keyString (ksLookupByName (config, "/path", 0));
	Key * patternKey = ksLookupByName (config, "/pattern", 0);
	Key * storageKey = ksLookupByName (config, "/storage", 0);
	Key * resolverKey = ksLookupByName (config, "/resolver", 0);
	Key * stayAliveKey = ksLookupByName (config, "/stayalive", 0);
	MultiConfig * mc = elektraCalloc (sizeof (MultiConfig));
	if (resolverKey)
		mc->resolver = elektraStrDup (keyString (resolverKey));
	else
		mc->resolver = elektraStrDup (DEFAULT_RESOLVER);
	if (storageKey)
		mc->storage = elektraStrDup (keyString (storageKey));
	else
		mc->storage = elektraStrDup (DEFAULT_STORAGE);
	if (patternKey)
		mc->pattern = elektraStrDup (keyString (patternKey));
	else
		mc->pattern = elektraStrDup (DEFAULT_PATTERN);
	if (stayAliveKey) mc->stayAlive = 1;
	mc->directory = elektraStrDup (path);
	mc->childBackends = ksNew (0, KS_END);
	mc->modules = ksNew (0, KS_END);
	elektraModulesInit (mc->modules, NULL);
	elektraPluginSetData (handle, mc);
	return mc;
}


static Codes initBackend (MultiConfig * mc, SingleConfig * s, Key * parentKey)
{
	unsigned long fullPathLen = strlen (mc->directory) + strlen (s->filename) + 2;
	char * fullPath = elektraCalloc (fullPathLen);
	snprintf (fullPath, fullPathLen, "%s/%s", mc->directory, s->filename);
	s->fullPath = fullPath;
	unsigned long childParentStringLen = strlen (keyName (parentKey)) + strlen (s->filename) + 2;
	char * childParentString = elektraCalloc (childParentStringLen);
	snprintf (childParentString, childParentStringLen, "%s/%s", keyName (parentKey), s->filename);
	s->parentString = childParentString;
	// fprintf (stderr, "Added file %s:(%s)\n\tChildParentKey: %s\n", s->fullPath, s->filename, s->parentString);
	Plugin * resolver = NULL;
	resolver = elektraPluginOpen (mc->resolver, mc->modules, ksNew (1, keyNew ("system/path", KEY_VALUE, s->fullPath, KEY_END), KS_END),
				      parentKey);
	// fprintf (stderr, "%s:(%s)\n", keyName (parentKey), keyString (parentKey));
	if (!resolver)
	{
		// fprintf (stderr, "Failed to load resolver %s for %s\n", mc->resolver, s->parentString);
		return ERROR;
	}
	else
	{
		s->resolver = resolver;
	}
	Plugin * storage = NULL;
	storage = elektraPluginOpen (mc->storage, mc->modules, ksNew (1, keyNew ("system/path", KEY_VALUE, s->fullPath, KEY_END), KS_END),
				     parentKey);
	if (!storage)
	{
		// fprintf (stderr, "Failed to load storage %s for %s\n", mc->storage, s->parentString);
		return ERROR;
	}
	else
	{
		s->storage = storage;
	}
	return SUCCESS;
}

static Codes resolverGet (SingleConfig * s, KeySet * returned, Key * parentKey)
{
	Plugin * resolver = s->resolver;
	int rc = resolver->kdbGet (resolver, returned, parentKey);
	s->rcResolver = rvToRc (rc);
	;
	if (s->fullPath) elektraFree (s->fullPath);
	s->fullPath = elektraStrDup (keyString (parentKey));
	return s->rcResolver;
}

static Codes updateFiles (MultiConfig * mc, KeySet * returned, Key * parentKey)
{
	Codes rc = NOUPDATE;
	DIR * dir = NULL;
	struct dirent * d = NULL;
	if (!(dir = opendir (mc->directory)))
	{
		return ERROR;
	}
	KeySet * found = ksNew (0, KS_END);
	Key * initialParent = keyDup (parentKey);
	while ((d = readdir (dir)) != NULL)
	{
		if (d->d_type != DT_REG) continue;
		if (fnmatch (mc->pattern, d->d_name, FNM_PATHNAME)) continue;
		// fprintf (stderr, "- %s\n", d->d_name);
		Key * lookup = keyNew ("/", KEY_CASCADING_NAME, KEY_END);
		keyAddBaseName (lookup, d->d_name);
		Key * k;
		if ((k = ksLookup (mc->childBackends, lookup, KDB_O_NONE)) != NULL)
		{
			ksAppendKey (found, k);
		}
		else
		{
			SingleConfig * s = elektraCalloc (sizeof (SingleConfig));
			s->filename = elektraStrDup (d->d_name);
			Codes r = initBackend (mc, s, parentKey);
			if (r == ERROR)
			{
				if (!mc->stayAlive)
				{
					return ERROR;
				}
				else
				{
					closeBackend (s);
				}
			}
			else
			{
				Key * childKey = keyNew (keyName (lookup), KEY_CASCADING_NAME, KEY_BINARY, KEY_SIZE,
							 sizeof (SingleConfig *), KEY_VALUE, &s, KEY_END);
				ksAppendKey (mc->childBackends, childKey);
				ksAppendKey (found, childKey);
				rc = SUCCESS;
			}
		}
		keyDel (lookup);
	}
	closedir (dir);
	ksRewind (mc->childBackends);
	ksRewind (found);
	Key * c;
	while ((c = ksNext (found)) != NULL)
	{
		if (ksLookup (mc->childBackends, c, KDB_O_POP))
		{
			SingleConfig * s = *(SingleConfig **)keyValue (c);
			keySetName (parentKey, s->parentString);
			keySetString (parentKey, s->fullPath);
			int r = resolverGet (s, returned, parentKey);
			s->rcResolver = rvToRc (r);
			if (s->rcResolver == ERROR)
			{
				if (mc->stayAlive)
				{
					cursor_t savedCursor = ksGetCursor (found);
					ksAppendKey (found, c);
					ksSetCursor (found, savedCursor);
				}
				else
				{
					rc = ERROR;
					break;
				}
			}
			if (r > 0) rc = SUCCESS;
		}
	}
	if (ksGetSize (mc->childBackends) > 0 && rc != -1)
	{
		rc = SUCCESS;
		mc->hasDeleted = 1;
	}
	closeBackends (mc->childBackends);
	ksDel (mc->childBackends);
	if (rc == ERROR)
	{
		closeBackends (found);
		ksDel (found);
	}
	else
	{
		mc->childBackends = found;
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static Codes doGetStorage (MultiConfig * mc, Key * parentKey)
{
	ksRewind (mc->childBackends);
	Key * initialParent = keyDup (parentKey);
	Codes rc = NOUPDATE;
	Key * k;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		if (s->rcResolver != SUCCESS) continue;
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		Plugin * storage = s->storage;
		KeySet * readKS = ksNew (0, KS_END);
		int r = storage->kdbGet (storage, readKS, parentKey);
		if (r > 0)
		{
			if (s->ks) ksDel (s->ks);
			s->ks = ksDup (readKS);
			;
			rc = SUCCESS;
			;
		}
		else
			rc = ERROR;
		ksDel (readKS);
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static void fillReturned (MultiConfig * mc, KeySet * returned)
{
	ksRewind (mc->childBackends);
	ksClear (returned);
	Key * k;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		ksAppend (returned, s->ks);
	}
	ksRewind (returned);
}

int elektraMultifileGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/multifile"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/multifile", KEY_VALUE, "multifile plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/multifile/exports", KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/open", KEY_FUNC, elektraMultifileOpen, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/close", KEY_FUNC, elektraMultifileClose, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/get", KEY_FUNC, elektraMultifileGet, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/set", KEY_FUNC, elektraMultifileSet, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/error", KEY_FUNC, elektraMultifileError, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/checkconf", KEY_FUNC, elektraMultifileCheckConfig, KEY_END),
			keyNew ("system/elektra/modules/multifile/exports/checkfile", KEY_FUNC, elektraMultifileCheckFile, KEY_END),

#include ELEKTRA_README (multifile)
			keyNew ("system/elektra/modules/multifile/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc)
	{
		mc = initialize (handle);
	}
	if (!mc)
	{
		return -1;
	}
	Codes rc = NOUPDATE;
	if (mc->getPhase == GETRESOLVER)
	{
		rc = updateFiles (mc, returned, parentKey);
		if (rc == SUCCESS)
		{
			mc->getPhase = GETSTORAGE;
		}
	}
	else if (mc->getPhase == GETSTORAGE)
	{
		rc = doGetStorage (mc, parentKey);
		if (rc == SUCCESS || mc->hasDeleted)
		{
			fillReturned (mc, returned);
			mc->hasDeleted = 0;
		}
		mc->getPhase = GETRESOLVER;
	}
	elektraPluginSetData (handle, mc);
	if (rc == SUCCESS)
		return 1;
	else if (rc == NOUPDATE)
		return 0;
	else
		return -1;
}

static Codes resolverSet (MultiConfig * mc, Key * parentKey)
{
	ksRewind (mc->childBackends);
	Key * initialParent = keyDup (parentKey);
	Key * k;
	Codes rc = NOUPDATE;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		if (s->rcResolver == NOUPDATE)
		{
			// fprintf (stderr, "SKIPPING %s:(%s)\n", s->parentString, s->fullPath);
			continue;
		}
		else if (s->rcResolver == EMPTY)
		{
			// fprintf (stderr, "MARK FOR DELETE: %s:(%s)\n", s->parentString, s->fullPath);
			++rc;
			continue;
		}
		else
		{
			// fprintf (stderr, "UPDATE: %s:(%s)\n", s->parentString, s->fullPath);
		}
		
		Plugin * resolver = s->resolver;
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		int r = resolver->kdbSet (resolver, s->ks, parentKey);
		s->rcResolver = rc = rvToRc (r);
		if (rc == ERROR) break;
		if (s->tmpFilename) elektraFree (s->tmpFilename);
		s->tmpFilename = elektraStrDup (keyString (parentKey));
		// fprintf (stderr, "tmp filename for %s: %s\n", s->fullPath, s->tmpFilename);
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static Codes doSetStorage (MultiConfig * mc, Key * parentKey)
{
	ksRewind (mc->childBackends);
	Key * initialParent = keyDup (parentKey);
	Codes rc = NOUPDATE;
	Key * k;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		if (s->rcResolver == EMPTY)
		{
			rc = SUCCESS;
			s->rcStorage = EMPTY;
			continue;
		}
		else if (s->rcResolver != SUCCESS)
		{
			continue;
		}
		
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->tmpFilename);
		Plugin * storage = s->storage;
		int r = storage->kdbSet (storage, s->ks, parentKey);
		s->rcStorage = rc = rvToRc (r);

		// fprintf (stderr, "%s ->kdbSet returned %d\n", mc->storage, r);

		if (rc == ERROR) break;
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static Codes doCommit (MultiConfig * mc, Key * parentKey)
{
	ksRewind (mc->childBackends);
	Key * initialParent = keyDup (parentKey);
	Codes rc = NOUPDATE;
	Key * k;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		if (s->rcStorage == EMPTY)
		{
			unlink (s->fullPath);
			continue;
		}
		else if (s->rcStorage != 1)
			continue;
		
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		Plugin * resolver = s->resolver;
		int r = resolver->kdbSet (resolver, s->ks, parentKey);
		rc = rvToRc (r);
		if (rc == ERROR) break;
		// fprintf (stderr, "%s ->kdbSet returned %d\n", mc->resolver, r);
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static int diffOrNeedSync (KeySet * ks, KeySet * checkKS)
{
	if (ksGetSize (ks) != ksGetSize (checkKS)) return 1;
	ksRewind (ks);
	ksRewind (checkKS);
	Key * key = NULL;
	Key * check = NULL;
	int done = 0;
	while (!done)
	{
		key = ksNext (ks);
		check = ksNext (checkKS);
		if (!key && !check)
			return 0;
		else if (!key || !check)
			return 1;
		if (keyCmp (key, check)) return 1;
		if (keyNeedSync (check)) return 1;
	}
	return 0; // will never happen, but gcc complains
}

static void flagUpdateBackends (MultiConfig * mc, KeySet * returned)
{
	ksRewind (mc->childBackends);
	Key * k;
	while ((k = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (k);
		Key * cutKey = keyNew (s->parentString, KEY_END);
		KeySet * cutKS = ksCut (returned, cutKey);
		if (ksGetSize (cutKS) == 0)
		{
			s->rcResolver = EMPTY;
			if (s->ks) ksDel (s->ks);
			s->ks = NULL;
		}
		else if (diffOrNeedSync (s->ks, cutKS))
		{
			s->rcResolver = SUCCESS;
			if (s->ks) ksDel (s->ks);
			s->ks = ksDup (cutKS);
		}
		else
		{
			s->rcResolver = NOUPDATE;
		}
		keyDel (cutKey);
		ksDel (cutKS);
	}
}

int elektraMultifileSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return -1;
	Codes rc = NOUPDATE;
	if (mc->setPhase == SETRESOLVER)
	{
		flagUpdateBackends (mc, returned);
		rc = resolverSet (mc, parentKey);
		// fprintf (stderr, "resolverSet returned %d\n", rc);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = SETSTORAGE;
	}
	else if (mc->setPhase == SETSTORAGE)
	{
		rc = doSetStorage (mc, parentKey);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = COMMIT;
	}
	else if (mc->setPhase == COMMIT)
	{
		doCommit (mc, parentKey);
		mc->setPhase = SETRESOLVER;
	}
	if (rc == SUCCESS)
		return 1;
	else if (rc == NOUPDATE)
		return 0;
	else
		return -1;
}

int elektraMultifileError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return 0;
	ksRewind (mc->childBackends);
	Key * key;
	Key * initialParent = keyDup (parentKey);
	while ((key = ksNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **)keyValue (key);
		Plugin * resolver = s->resolver;
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		if (resolver->kdbError)
		{
			resolver->kdbError (handle, returned, parentKey);
		}
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);


	return 1; // success
}

int elektraMultifileCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (multifile)
{
	// clang-format off
    return elektraPluginExport ("multifile",
	    ELEKTRA_PLUGIN_OPEN,	&elektraMultifileOpen,
	    ELEKTRA_PLUGIN_CLOSE,	&elektraMultifileClose,
	    ELEKTRA_PLUGIN_GET,	&elektraMultifileGet,
	    ELEKTRA_PLUGIN_SET,	&elektraMultifileSet,
	    ELEKTRA_PLUGIN_ERROR,	&elektraMultifileError,
	    ELEKTRA_PLUGIN_END);
}

