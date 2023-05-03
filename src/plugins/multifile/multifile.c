/**
 * @file
 *
 * @brief Source for multifile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./multifile.h"

#include <dirent.h>
#include <elektra/core.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/core/namespace.h>
#include <elektra/ease/meta.h>
#include <elektra/core/errors.h>
#include <elektra/plugin/plugin.h>
#include <errno.h>
#include <fnmatch.h>
#include <glob.h>
#include <internal/config.h>
#include <internal/kdbprivate.h>
#include <internal/macros/attributes.h>
#include <internal/pluginload/module.h>
#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <internal/resolver/shared.h>
#include <elektra/plugin/invoke.h>

#define DEFAULT_RESOLVER "resolver"
#define DEFAULT_PATTERN "*"
#define DEFAULT_STORAGE "storage"

typedef enum
{
	MULTI_SETRESOLVER = 0,
	MULTI_SETSTORAGE,
	MULTI_COMMIT,
} SetPhases;

typedef enum
{
	MULTI_GETRESOLVER = 0,
	MULTI_GETSTORAGE,
} GetPhases;

typedef enum
{
	EMPTY = -2,
	ERROR = -1,
	NOUPDATE = 0,
	SUCCESS = 1,
	CACHE_HIT = 2,
} Codes;


typedef struct
{
	char * directory;
	char * originalPath;
	char * pattern;
	SetPhases setPhase;
	GetPhases getPhase;
	KeySet * modules;
	KeySet * childBackends;
	KeySet * childConfig;
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
	case 2:
		return CACHE_HIT;
		break;
	}
	return ERROR;
}

static int elektraResolveFilename (Key * parentKey, ElektraResolveTempfile tmpFile)
{
	int rc = 0;
	ElektraInvokeHandle * handle = elektraInvokeOpen ("resolver", 0, 0);
	if (!handle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	ElektraResolved * resolved = NULL;
	typedef ElektraResolved * (*resolveFileFunc) (elektraNamespace, const char *, ElektraResolveTempfile, Key *);
	resolveFileFunc resolveFunc = *(resolveFileFunc *) elektraInvokeGetFunction (handle, "filename");

	if (!resolveFunc)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	typedef void (*freeHandleFunc) (ElektraResolved *);
	freeHandleFunc freeHandle = *(freeHandleFunc *) elektraInvokeGetFunction (handle, "freeHandle");

	if (!freeHandle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	resolved = resolveFunc (keyGetNamespace (parentKey), keyString (parentKey), tmpFile, parentKey);

	if (!resolved)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	else
	{
		keySetString (parentKey, resolved->fullPath);
		freeHandle (resolved);
	}

	elektraInvokeClose (handle, 0);
	return rc;

RESOLVE_FAILED:
	ELEKTRA_LOG_DEBUG ("MULTIFILE: resolve failed!");
	elektraInvokeClose (handle, 0);
	return rc;
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
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (b); ++it)
	{
		k = ksAtCursor (b, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
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
	if (mc->originalPath) elektraFree (mc->originalPath);
	elektraModulesClose (mc->modules, NULL);
	ksDel (mc->modules);
	ksDel (mc->childBackends);
	ksDel (mc->childConfig);
	elektraFree (mc);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

static MultiConfig * initialize (Plugin * handle, Key * parentKey)
{

	KeySet * config = elektraPluginGetConfig (handle);
	Key * origPath = ksLookupByName (config, "/path", 0);
	keySetString (parentKey, keyString (origPath));
	if (elektraResolveFilename (parentKey, ELEKTRA_RESOLVER_TEMPFILE_NONE) == -1)
	{
		return NULL;
	}
	Key * patternKey = ksLookupByName (config, "/pattern", 0);
	Key * storageKey = ksLookupByName (config, "/storage", 0);
	Key * resolverKey = ksLookupByName (config, "/resolver", 0);
	Key * stayAliveKey = ksLookupByName (config, "/stayalive", 0);
	MultiConfig * mc = elektraCalloc (sizeof (MultiConfig));
	mc->directory = elektraStrDup (keyString (parentKey));
	mc->originalPath = elektraStrDup (keyString (origPath));
	if (resolverKey)
	{
		mc->resolver = elektraStrDup (keyString (resolverKey));
	}
	else
	{
		mc->resolver = elektraStrDup (DEFAULT_RESOLVER);
	}
	if (storageKey)
	{
		mc->storage = elektraStrDup (keyString (storageKey));
	}
	else
	{
		mc->storage = elektraStrDup (DEFAULT_STORAGE);
	}
	if (patternKey)
	{
		mc->pattern = elektraStrDup (keyString (patternKey));
	}
	else
	{
		mc->pattern = elektraStrDup (DEFAULT_PATTERN);
	}
	if (stayAliveKey) mc->stayAlive = 1;
	Key * cutKey = keyNew ("/child", KEY_END);
	KeySet * childConfig = ksCut (config, cutKey);
	keyDel (cutKey);
	mc->childConfig = ksRenameKeys (childConfig, "system");
	ksAppend (config, childConfig);
	ksDel (childConfig);
	mc->childBackends = ksNew (0, KS_END);
	mc->modules = ksNew (0, KS_END);
	elektraModulesInit (mc->modules, NULL);
	elektraPluginSetData (handle, mc);
	return mc;
}


static Codes initBackend (Plugin * handle, MultiConfig * mc, SingleConfig * s, Key * parentKey)
{
	unsigned long fullPathLen = strlen (mc->originalPath) + strlen (s->filename) + 2;
	char * fullPath = elektraCalloc (fullPathLen);
	snprintf (fullPath, fullPathLen, "%s/%s", mc->originalPath, s->filename);
	s->fullPath = fullPath;
	unsigned long childParentStringLen = strlen (keyName (parentKey)) + strlen (s->filename) + 2;
	char * childParentString = elektraCalloc (childParentStringLen);
	snprintf (childParentString, childParentStringLen, "%s/%s", keyName (parentKey), s->filename);
	s->parentString = childParentString;
	// fprintf (stderr, "Added file %s:(%s)\n\tChildParentKey: %s\n", s->fullPath, s->filename, s->parentString);
	Plugin * resolver = NULL;
	KeySet * resolverChildConfig = ksDup (mc->childConfig);
	ksAppendKey (resolverChildConfig, keyNew ("/path", KEY_VALUE, s->fullPath, KEY_END));
	resolver = elektraPluginOpen (mc->resolver, mc->modules, resolverChildConfig, parentKey);
	// fprintf (stderr, "%s:(%s)\n", keyName (parentKey), keyString (parentKey));
	if (!resolver)
	{
		// fprintf (stderr, "Failed to load resolver %s for %s\n", mc->resolver, s->parentString);
		return ERROR;
	}
	else
	{
		s->resolver = resolver;
		resolver->global = elektraPluginGetGlobalKeySet (handle);
	}
	Plugin * storage = NULL;
	KeySet * storageChildConfig = ksDup (mc->childConfig);
	ksAppendKey (storageChildConfig, keyNew ("system:/path", KEY_VALUE, s->fullPath, KEY_END));
	storage = elektraPluginOpen (mc->storage, mc->modules, storageChildConfig, parentKey);
	if (!storage)
	{
		// fprintf (stderr, "Failed to load storage %s for %s\n", mc->storage, s->parentString);
		return ERROR;
	}
	else
	{
		s->storage = storage;
		storage->global = elektraPluginGetGlobalKeySet (handle);
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

static Codes updateFilesGlob (Plugin * handle, MultiConfig * mc, KeySet * found, Key * parentKey)
{
	Codes rc = NOUPDATE;
	glob_t results;
	int ret;

	char pattern[strlen (mc->directory) + strlen (mc->pattern) + 2];
	snprintf (pattern, sizeof (pattern), "%s/%s", mc->directory, mc->pattern);

	ret = glob (pattern, 0, NULL, &results);
	if (ret != 0)
	{
		if (ret == GLOB_NOSPACE)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		}
		else if (ret == GLOB_ABORTED)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Glob(%s) failed with a read error", pattern);
		}
		else if (ret == GLOB_NOMATCH)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Glob(%s) failed with no matches", pattern);
		}
		return ERROR;
	}
	struct stat sb;
	for (unsigned int i = 0; i < results.gl_pathc; ++i)
	{
		ret = stat (results.gl_pathv[i], &sb);
		if (S_ISREG (sb.st_mode))
		{
			Key * lookup = keyNew ("/", KEY_END);
			keyAddBaseName (lookup, (results.gl_pathv[i]) + strlen (mc->directory));
			Key * k;
			if ((k = ksLookup (mc->childBackends, lookup, KDB_O_NONE)) != NULL)
			{
				ksAppendKey (found, k);
			}
			else
			{
				SingleConfig * s = elektraCalloc (sizeof (SingleConfig));
				s->filename = elektraStrDup ((results.gl_pathv[i]) + strlen (mc->directory) + 1);
				Codes r = initBackend (handle, mc, s, parentKey);
				if (r == ERROR)
				{
					if (!mc->stayAlive)
					{
						keyDel (lookup);
						globfree (&results);
						return ERROR;
					}
					else
					{
						closeBackend (s);
					}
				}
				else
				{
					Key * childKey = keyNew (keyName (lookup), KEY_BINARY, KEY_SIZE, sizeof (SingleConfig *), KEY_VALUE,
								 &s, KEY_END);
					ksAppendKey (mc->childBackends, childKey);
					ksAppendKey (found, childKey);
					rc = SUCCESS;
				}
			}
			keyDel (lookup);
		}
	}
	globfree (&results);
	return rc;
}

static Codes updateFiles (Plugin * handle, MultiConfig * mc, KeySet * returned, Key * parentKey)
{
	Codes rc = NOUPDATE;
	KeySet * found = ksNew (0, KS_END);
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	rc = updateFilesGlob (handle, mc, found, parentKey);
	if (rc == ERROR)
	{
		ksDel (found);
		keyDel (initialParent);
		return ERROR;
	}

	Key * c;
	ssize_t cacheHits = 0;
	ssize_t numBackends = ksGetSize (found);


	for (elektraCursor it = 0; it < ksGetSize (found); ++it)
	{
		c = ksAtCursor (found, it);
		if (ksLookup (mc->childBackends, c, KDB_O_POP))
		{
			SingleConfig * s = *(SingleConfig **) keyValue (c);
			keySetName (parentKey, s->parentString);
			keySetString (parentKey, s->fullPath);
			int r = resolverGet (s, returned, parentKey);
			elektraFree (s->fullPath);
			s->fullPath = elektraStrDup (keyString (parentKey));
			s->rcResolver = rvToRc (r);
			if (s->rcResolver == ERROR)
			{
				if (mc->stayAlive)
				{
					ksAppendKey (found, c);
				}
				else
				{
					rc = ERROR;
					break;
				}
			}

			if (r == ELEKTRA_PLUGIN_STATUS_CACHE_HIT)
			{
				++cacheHits;
			}

			if (r > 0)
			{
				rc = SUCCESS;
			}
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
	if (cacheHits == numBackends)
	{
		rc = CACHE_HIT;
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static Codes doGetStorage (MultiConfig * mc, Key * parentKey)
{
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);
	Codes rc = NOUPDATE;
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
		// When we reach this stage, we will need to load
		// any successfully resolved files (as it is done in the kdb core)
		if (s->rcResolver < 0) continue;
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		Plugin * storage = s->storage;
		KeySet * readKS = ksNew (0, KS_END);
		int r = storage->kdbGet (storage, readKS, parentKey);
		if (r > 0)
		{
			if (s->ks) ksDel (s->ks);
			s->ks = ksDup (readKS);
			rc = SUCCESS;
		}
		else
		{
			rc = ERROR;
		}
		ksDel (readKS);
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);
	return rc;
}

static void fillReturned (MultiConfig * mc, KeySet * returned)
{
	ksClear (returned);
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
		ksAppend (returned, s->ks);
	}
}

int elektraMultifileGet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/multifile"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/multifile", KEY_VALUE, "multifile plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports", KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/open", KEY_FUNC, elektraMultifileOpen, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/close", KEY_FUNC, elektraMultifileClose, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/get", KEY_FUNC, elektraMultifileGet, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/set", KEY_FUNC, elektraMultifileSet, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/commit", KEY_FUNC, elektraMultifileCommit, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/error", KEY_FUNC, elektraMultifileError, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/checkconf", KEY_FUNC, elektraMultifileCheckConf, KEY_END),
			keyNew ("system:/elektra/modules/multifile/exports/checkfile", KEY_FUNC, elektraMultifileCheckFile, KEY_END),

#include ELEKTRA_README
			keyNew ("system:/elektra/modules/multifile/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc)
	{
		mc = initialize (handle, parentKey);
	}
	if (!mc)
	{
		return -1;
	}
	Codes rc = NOUPDATE;
	if (mc->getPhase == MULTI_GETRESOLVER)
	{
		rc = updateFiles (handle, mc, returned, parentKey);
		// if it is a only a partial cache hit, we still need to load everything
		// in the next phase
		if (rc >= SUCCESS)
		{
			mc->getPhase = MULTI_GETSTORAGE;
		}
	}
	else if (mc->getPhase == MULTI_GETSTORAGE)
	{
		rc = doGetStorage (mc, parentKey);
		if (rc == SUCCESS || mc->hasDeleted)
		{
			fillReturned (mc, returned);
			mc->hasDeleted = 0;
		}
		mc->getPhase = MULTI_GETRESOLVER;
	}
	elektraPluginSetData (handle, mc);
	if (rc == CACHE_HIT)
	{
		return ELEKTRA_PLUGIN_STATUS_CACHE_HIT;
	}
	else if (rc == SUCCESS)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (rc == NOUPDATE)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static Codes resolverSet (MultiConfig * mc, Key * parentKey)
{
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);
	Key * k;
	Codes rc = NOUPDATE;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
		if (s->rcResolver == NOUPDATE)
		{
			// fprintf (stderr, "SKIPPING %s:(%s)\n", s->parentString, s->fullPath);
			continue;
		}
		else if (s->rcResolver == EMPTY)
		{
			// fprintf (stderr, "MARK FOR DELETE: %s:(%s)\n", s->parentString, s->fullPath);
			// ++rc;
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
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);
	Codes rc = NOUPDATE;
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
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
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);
	Codes rc = NOUPDATE;
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
		if (s->rcStorage == EMPTY)
		{
			unlink (s->fullPath);
			continue;
		}
		else if (s->rcStorage != 1)
		{
			continue;
		}

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

	Key * key = NULL;
	Key * check = NULL;
	int ret = -1;

	for (elektraCursor it = 0; ret == -1; ++it)
	{
		key = ksAtCursor (ks, it);
		check = ksAtCursor (checkKS, it);

		if (!key && !check)
		{
			ret = 0;
		}
		else if (!key || !check)
		{
			ret = 1;
		}
		else if (keyCmp (key, check))
		{
			ret = 1;
		}
		else if (keyNeedSync (check))
		{
			ret = 1;
		}
	}
	return ret;
}

static void flagUpdateBackends (MultiConfig * mc, KeySet * returned)
{
	Key * k;

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		k = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (k);
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
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return -1;
	Codes rc = NOUPDATE;
	if (mc->setPhase == MULTI_SETRESOLVER)
	{
		flagUpdateBackends (mc, returned);
		rc = resolverSet (mc, parentKey);
		// fprintf (stderr, "resolverSet returned %d\n", rc);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = MULTI_SETSTORAGE;
	}
	else if (mc->setPhase == MULTI_SETSTORAGE)
	{
		rc = doSetStorage (mc, parentKey);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = MULTI_COMMIT;
	}
	else if (mc->setPhase == MULTI_COMMIT)
	{
		doCommit (mc, parentKey);
		mc->setPhase = MULTI_SETRESOLVER;
	}
	if (rc == SUCCESS)
	{
		return 1;
	}
	else if (rc == NOUPDATE)
	{
		return 0;
	}
	else
	{
		return -1;
	}
}

int elektraMultifileError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return 0;
	Key * key;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	for (elektraCursor it = 0; it < ksGetSize (mc->childBackends); ++it)
	{
		key = ksAtCursor (mc->childBackends, it);
		SingleConfig * s = *(SingleConfig **) keyValue (key);
		Plugin * resolver = s->resolver;
		keySetName (parentKey, s->parentString);
		keySetString (parentKey, s->fullPath);
		if (resolver->kdbError)
		{
			resolver->kdbError (resolver, returned, parentKey);
		}
	}
	keySetName (parentKey, keyName (initialParent));
	keySetString (parentKey, keyString (initialParent));
	keyDel (initialParent);


	return 1; // success
}

int elektraMultifileCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return elektraMultifileSet (handle, returned, parentKey);
}

int elektraMultifileCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("multifile",
	    ELEKTRA_PLUGIN_OPEN,	&elektraMultifileOpen,
	    ELEKTRA_PLUGIN_CLOSE,	&elektraMultifileClose,
	    ELEKTRA_PLUGIN_GET,	&elektraMultifileGet,
	    ELEKTRA_PLUGIN_SET,	&elektraMultifileSet,
	    ELEKTRA_PLUGIN_ERROR,	&elektraMultifileError,
	    ELEKTRA_PLUGIN_COMMIT,      &elektraMultifileCommit,
	    ELEKTRA_PLUGIN_END);
}

