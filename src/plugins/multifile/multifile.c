/**
 * @file
 *
 * @brief Source for multifile plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "multifile.h"

#include <kdbhelper.h>
#include <kdbmodule.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fnmatch.h>
#include <kdbconfig.h>

#define DEFAULT_RESOLVER "resolver"
#define DEFAULT_PATTERN "*"
#define DEFAULT_STORAGE "ini"

typedef enum
{
    SETRESOLVER = 0, SETSTORAGE, COMMIT,
}SetPhases;

typedef enum
{
    GETRESOLVER = 0, GETSTORAGE,
}GetPhases;

typedef struct
{
    char *directory;
    char *pattern;
    SetPhases setPhase;
    GetPhases getPhase;
    KeySet *modules;
    KeySet *childBackends;
    char *resolver;
    char *storage;
}MultiConfig;

typedef struct
{
    char *filename;
    char *fullPath;
    char *parentString;
    Plugin *resolver;
    int rcResolver;
    Plugin *storage;
    int rcStorage;
}SingleConfig;


int elektraMultifileCheckFile(const char *filename)
{
    if(!filename)
	return -1;
    if(filename[0] == '/')
	return 0;
    return 1;
}

int elektraMultifileOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

static void closeBackends(KeySet *b)
{
    ksRewind(b);
    Key *k;
    while((k = ksNext(b)) != NULL)
    {
	SingleConfig *s = *(SingleConfig **)keyValue(k);
	if(s->fullPath)
	    elektraFree(s->fullPath);
	if(s->filename)
	    elektraFree(s->filename);
	if(s->parentString)
	    elektraFree(s->parentString);
	if(s->resolver)
	    elektraPluginClose(s->resolver, NULL);
	if(s->storage)
	    elektraPluginClose(s->storage, NULL);
	elektraFree(s);
    }
}

int elektraMultifileClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	MultiConfig *mc = elektraPluginGetData(handle);
	if(!mc)
	    return 1;
	closeBackends(mc->childBackends);
	if(mc->directory)
	    elektraFree(mc->directory);
	if(mc->pattern)
	    elektraFree(mc->pattern);
	if(mc->resolver)
	    elektraFree(mc->resolver);
	if(mc->storage)
	    elektraFree(mc->storage);
	elektraModulesClose(mc->modules, NULL);
	ksDel(mc->modules);
	ksDel(mc->childBackends);
	elektraFree(mc);
	elektraPluginSetData(handle, NULL);
	return 1; // success
}

static MultiConfig *initialize(Plugin *handle, Key *parentKey)
{
    KeySet *config = elektraPluginGetConfig(handle);
    const char *path = keyString(ksLookupByName(config, "/path", 0));
    Key *patternKey = ksLookupByName(config, "/pattern", 0);
    Key *storageKey = ksLookupByName(config, "/storage", 0);
    Key *resolverKey = ksLookupByName(config, "/resolver", 0);
    MultiConfig *mc = elektraCalloc(sizeof(MultiConfig));
    if(resolverKey)
	mc->resolver = strdup(keyString(resolverKey));
    else
	mc->resolver = strdup(DEFAULT_RESOLVER);
    if(storageKey)
	mc->storage = strdup(keyString(storageKey));
    else
	mc->storage = strdup(DEFAULT_STORAGE);
    if(patternKey)
	mc->pattern = strdup(keyString(patternKey));
    else
	mc->pattern = strdup(DEFAULT_PATTERN);
    mc->directory = strdup(path);
    mc->childBackends = ksNew(0, KS_END);
    mc->modules = ksNew(0, KS_END);
    elektraModulesInit(mc->modules, NULL);
    elektraPluginSetData(handle, mc);
    return mc;
}


static void initBackend(MultiConfig *mc, SingleConfig *s, Key *parentKey)
{
    size_t fullPathLen = strlen(mc->directory) + strlen(s->filename) +2;
    char *fullPath = elektraCalloc(fullPathLen);
    snprintf(fullPath, fullPathLen, "%s/%s", mc->directory, s->filename);
    s->fullPath = fullPath;
    size_t childParentStringLen = strlen(keyName(parentKey)) + strlen(s->filename) + 2;
    char *childParentString = elektraCalloc(childParentStringLen);
    snprintf(childParentString, childParentStringLen, "%s/%s", keyName(parentKey), s->filename);
    s->parentString = childParentString;
    fprintf(stderr, "Added file %s:(%s)\n\tChildParentKey: %s\n", s->fullPath, s->filename, s->parentString);
    Plugin *resolver = NULL;
    resolver = elektraPluginOpen(mc->resolver, mc->modules, ksNew(1, keyNew("system/path", KEY_VALUE, s->fullPath, KEY_END), KS_END), parentKey);
    fprintf(stderr, "%s:(%s)\n", keyName(parentKey), keyString(parentKey));
    if(!resolver)
    {
	fprintf(stderr, "Failed to load resolver %s for %s\n", mc->resolver, s->parentString);
    }
    else
    {
	s->resolver = resolver;
    }
    Plugin *storage = NULL;
    storage = elektraPluginOpen(mc->storage, mc->modules, ksNew(1, keyNew("system/path", KEY_VALUE, s->fullPath, KEY_END), KS_END), parentKey);
    if(!storage)
    {
	fprintf(stderr, "Failed to load storage %s for %s\n", mc->storage, s->parentString);
    }
    else
    {
	s->storage = storage;
    }
}

static int resolverGet(SingleConfig *s, KeySet *returned, Key *parentKey)
{
    Plugin *resolver = s->resolver;
    int rc = resolver->kdbGet(resolver, returned, parentKey);
    fprintf(stderr, "resolver->kdbGet %s:(%s) returned %d\n", keyName(parentKey), keyString(parentKey), rc);
    s->rcResolver = rc;
    if(s->fullPath)
	elektraFree(s->fullPath);
    s->fullPath = strdup(keyString(parentKey));
    return rc;
}

static void dropFromKS(KeySet *toRemove, KeySet *returned)
{
    Key *delKey = keyNew("/", KEY_CASCADING_NAME, KEY_END);
    ksRewind(toRemove);
    Key *k;
    while((k = ksNext(toRemove)) != NULL)
    {
	SingleConfig *s = *(SingleConfig **)keyValue(k);
	keySetName(delKey, s->parentString);
	KeySet *cut = ksCut(returned, delKey);
	ksDel(cut);
    }
    keyDel(delKey);
}

static int updateFiles(MultiConfig *mc, KeySet *returned, Key *parentKey)
{
    int rc = 0;
    DIR *dir = NULL;
    struct dirent *d = NULL;
    if(!(dir = opendir(mc->directory)))
    {
	return -1;
    }
    KeySet *found = ksNew(0, KS_END);
    Key *initialParent = keyDup(parentKey);
    while((d = readdir(dir)) != NULL)
    {
	if(d->d_type != DT_REG)
		continue;
	if(fnmatch(mc->pattern, d->d_name, FNM_PATHNAME))
	    continue;
	fprintf(stderr, "- %s\n", d->d_name);
	Key *lookup = keyNew("/", KEY_CASCADING_NAME, KEY_END);
	keyAddBaseName(lookup, d->d_name);
	Key *k;
	if((k = ksLookup(mc->childBackends, lookup, KDB_O_NONE)) != NULL)
	{
	    ksAppendKey(found, k);
	}
	else
	{
	    SingleConfig *s = elektraCalloc(sizeof(SingleConfig));
	    s->filename = strdup(d->d_name);
	    initBackend(mc, s, parentKey);
	    Key *childKey = keyNew(keyName(lookup), KEY_CASCADING_NAME, KEY_BINARY, KEY_SIZE, sizeof(SingleConfig *), KEY_VALUE, &s, KEY_END);
	    ksAppendKey(mc->childBackends, childKey);
	    ksAppendKey(found, childKey);
	}
	keyDel(lookup);
    }
    closedir(dir);
    ksRewind(mc->childBackends);
    ksRewind(found);
    Key *c;
    while((c = ksNext(found)) != NULL)
    {
	if(ksLookup(mc->childBackends, c, KDB_O_POP))
	{
	    	SingleConfig *s = *(SingleConfig **)keyValue(c);
		keySetName(parentKey, s->parentString);
		keySetString(parentKey, s->fullPath);
		int r = resolverGet(s, returned, parentKey);
		if(r > 0)
	   	 	++rc;
	}
    }
    dropFromKS(mc->childBackends, returned);
    closeBackends(mc->childBackends);
    ksDel(mc->childBackends);
    mc->childBackends = found;
    keySetName(parentKey, keyName(initialParent));
    keySetString(parentKey, keyString(initialParent));
    keyDel(initialParent);
    return rc;
}

static int doGetStorage(MultiConfig *mc, KeySet *returned, Key *parentKey)
{
    ksRewind(mc->childBackends);
    Key *initialParent = keyDup(parentKey);
    int rc = 0;
    Key *k;
    while((k = ksNext(mc->childBackends)) != NULL)
    {
	if(!keyCmp(k, parentKey))
	    continue;
	SingleConfig *s = *(SingleConfig **)keyValue(k);
	if(s->rcResolver < 1)
	    continue;
	fprintf(stderr, "calling getStorage with %s:(%s)\n", keyName(parentKey), keyString(parentKey));
	keySetName(parentKey, s->parentString);
	keySetString(parentKey, s->fullPath);
	Plugin *storage = s->storage;
	KeySet *cutKS = ksCut(returned, parentKey);
	int r = storage->kdbGet(storage, cutKS, parentKey);
	ksAppend(returned, cutKS);
	ksDel(cutKS);
	fprintf(stderr, "%s returned %d\n", mc->storage, r);
	if(r > 0)
	    ++rc;
	
    }
    keySetName(parentKey, keyName(initialParent));
    keySetString(parentKey, keyString(initialParent));
    keyDel(initialParent);
    return rc;
}

int elektraMultifileGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/multifile"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/multifile", KEY_VALUE, "multifile plugin waits for your orders", KEY_END),
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
	MultiConfig *mc = elektraPluginGetData(handle);
	if(!mc)
	{
	    mc = initialize(handle, parentKey);
	}
	if(!mc)
	{
	    return -1;
	}
	int rc = 0;
	if(mc->getPhase == GETRESOLVER)
	{
	    rc = updateFiles(mc, returned, parentKey);
	    fprintf(stderr, "updateFiles returned %d\n", rc);
	    if(rc > 0)
	    {
		mc->getPhase = GETSTORAGE;
	    }
	}
	else if(mc->getPhase == GETSTORAGE)
	{
	    rc = doGetStorage(mc, returned, parentKey);
	    mc->getPhase = GETRESOLVER;
	}
	if(rc > 0)
	    return 1;
	else if(rc == 0)
	    return 0;
	else
	    return -1;
}

int elektraMultifileSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraMultifileError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

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

