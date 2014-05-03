/**
 * \file
 *
 * \brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include "augeas.h"

#include <augeas.h>
#include <libgen.h>
#include <stdio.h>

static int convertToKeys(augeas *handle, KeySet *ks, Key *template, char *rootPath, size_t prefixSize);

int elektraAugeasOpen(Plugin *handle, Key *parentKey) {
	augeas *augeasHandle;
	augeasHandle = aug_init(NULL, NULL, AUG_NO_MODL_AUTOLOAD);


	// TODO: try to read augeas init error
	if(!augeasHandle) {
		// TODO: use correct error number
		ELEKTRA_SET_ERROR(75, parentKey, "Unable to initialize augeas");
	}



	elektraPluginSetData(handle, augeasHandle);
	return 0;
}

int elektraAugeasClose(Plugin *handle, Key *parentKey ELEKTRA_UNUSED) {
	augeas *augeasHandle;

	augeasHandle = elektraPluginGetData(handle);

	if(augeasHandle) {
		aug_close(augeasHandle);
	}

	return 0;
}

static int loadLens(Plugin* handle, augeas* augeasHandle, const char* filename) {
	char *base = basename(filename);
	char* lensConfigPath;
	asprintf(&lensConfigPath, "/augeas/load/%s/lens", base);
	char* inclPath;
	asprintf(&inclPath, "/augeas/load/%s/incl", base);
	Key* lensPathKey = ksLookupByName(elektraPluginGetConfig(handle), "/lens",
			0);

	if(!lensPathKey) return -1;

	const char* lensPath = keyString(lensPathKey);
	aug_set(augeasHandle, "/augeas/load/Hosts/lens", lensPath);
	aug_set(augeasHandle, "/augeas/load/Hosts/incl", filename);
	return aug_load(augeasHandle);
}

int elektraAugeasGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *n;

	if (!strcmp (keyName(parentKey), "system/elektra/modules/augeas"))
	{
		ksAppend (returned, n = ksNew (30,
			keyNew ("system/elektra/modules/augeas",
				KEY_VALUE, "Augeas plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/augeas/exports", KEY_END),
			keyNew ("system/elektra/modules/augeas/exports/get",
				KEY_FUNC, elektraAugeasGet,
				KEY_END),
			keyNew ("system/elektra/modules/augeas/exports/set",
				KEY_FUNC, elektraAugeasSet,
				KEY_END),
			keyNew ("system/elektra/modules/augeas/exports/open",
				KEY_FUNC, elektraAugeasOpen,
				KEY_END),
			keyNew ("system/elektra/modules/augeas/exports/close",
				KEY_FUNC, elektraAugeasClose,
				KEY_END),
			keyNew ("system/elektra/modules/augeas/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/author",
				KEY_VALUE, "Felix Berlakovich <elektra@berlakovich.net>", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/description",
				KEY_VALUE, "Reads and writes configurations with libaugeas", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/augeas/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);
		return 1;
	}

	const char *filename;
	augeas *augeasHandle;
	int result;

	augeasHandle = elektraPluginGetData(handle);

	if(!augeasHandle) {
		// TODO: use correct error number
		ELEKTRA_SET_ERROR(75, parentKey, "Augeas was not initialized correctly. Unable to write");
	}

	filename = keyString(parentKey);
	result = loadLens(handle, augeasHandle, filename);

	if (result < 0) {
		// TODO: use correct error number
		ELEKTRA_SET_ERROR(75, parentKey, "Unable to initialize augeas");
	}

	ksClear(returned);
	KeySet *append = ksNew(ksGetSize(returned)*2, KS_END);

	Key *key = keyDup(parentKey);
	ksAppendKey(append, key);

	char *pathPrefix;
	asprintf(&pathPrefix, "/files%s", filename);

	// recursively convert nodes to keys
	result = convertToKeys(augeasHandle, append, parentKey, pathPrefix, strlen(pathPrefix));
	
	if(result < 0) {
		ksDel(append);
		return -1;
	}

	ksAppend(returned, append);
	ksDel(append);
	return 1;
}

int elektraAugeasSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{

	return 0;
}

void elektraAugeasSetMeta(Key *key, int order)
{
	char buffer[50];
	snprintf (buffer, 50, "%d", order);
	keySetMeta(key, "order", buffer);
}

static int convertToKeys(augeas *handle, KeySet *ks, Key *template, char *rootPath, size_t prefixSize) {

	char *matchPath;
	asprintf(&matchPath, "%s/*", rootPath);

	char **matches;
	int resultCount = aug_match(handle, matchPath, &matches);

	if(resultCount < 0) return resultCount;

	int i;
	int result = 0;
	for(i=0; i < resultCount; i++) {
		char *curr = matches[i];
		const char *value = 0;

		result = aug_get(handle, curr, &value);

		if(result < 0) break;

		Key *key = keyDup(template);
		keySetString(key, value);
		keyAddBaseName(key, curr+prefixSize);
		elektraAugeasSetMeta(key, i);

		int sret = ksAppendKey(ks, key);
		if(sret < 0) break;

		result = convertToKeys(handle, ks, template, curr, prefixSize);

		if(result < 0) break;

		free(curr);
	}

	for(; i< resultCount; i++) {
		free(matches[i]);
	}

	free(matches);

	return result;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(augeas)
{
	return elektraPluginExport ("augeas",
		ELEKTRA_PLUGIN_GET,	&elektraAugeasGet,
		ELEKTRA_PLUGIN_SET,	&elektraAugeasSet,
		ELEKTRA_PLUGIN_OPEN, &elektraAugeasOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraAugeasClose,
		ELEKTRA_PLUGIN_END);
}

