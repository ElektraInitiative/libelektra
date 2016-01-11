/**
 * @file
 *
 * @brief Source for spec plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "spec.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fnmatch.h>
#include <kdbhelper.h>
#include <kdbease.h>


static char *keyNameToMatchingString(const Key *key)
{
	fprintf(stdout, "keyname: %s\n", keyName(key));
	uint8_t arrayCount = 0;
	const char *name = strchr(keyName(key), '/');
	for(char *ptr = name; *ptr != '\0'; ++ptr)
		if(*ptr == '#')
			++arrayCount;
	char *pattern = elektraMalloc(elektraStrLen(name)+arrayCount);
	char *dst = pattern;
	for(char *src = name+1; *src != '\0'; ++src)
	{
		if(*src == '_' && *(src-1) == '/' && (*(src+1) == '/' || *(src+1) == '\0'))
		{
			*dst++ = '*';
		}
		else if(*src == '#' && *(src-1) == '/' && (*(src+1) == '/' || *(src+1) == '\0'))
		{
			*dst++ = '#';
			*dst++ = '*';
		}
		else
		{
			*dst++ = *src;
		}
	}
	*dst = '\0';
	fprintf(stdout, "keyToPattern: %s\n", pattern);
	return pattern;
}

static int matchPatternToKey(const char *pattern, const Key *key)
{
	return !fnmatch(pattern, (strchr(keyName(key), '/')+1), FNM_NOESCAPE|FNM_PATHNAME);
}

static int validateArray(Key *key)
{
	if(!strchr(keyName(key), '#'))
		return 0;
	Key *copy = keyDup(key);
	do
	{
		if(keyBaseName(key)[0] == '#')
		{
			if(elektraArrayValidateName(key) == -1)
			{
				fprintf(stderr, "%s not a valid array name\n", keyName(copy));
				keyDel(copy);
				return -1;
			}
		}
	}while(keySetBaseName(copy, 0) != -1);
	keyDel(copy);
	return 0;
}

static void *copyMeta(KeySet *returned)
{
	Key *specCutKey = keyNew("spec", KEY_END);
	KeySet *specKS = ksCut(returned, specCutKey);
	keyDel(specCutKey);
	Key *specKey;
	ksRewind(specKS);
	KeySet *possibleMatches = ksNew(ksGetSize(returned), KS_END);
	Key *cur;
	while((specKey = ksNext(specKS)) != NULL)
	{
		char *pattern = keyNameToMatchingString(specKey);
		ksRewind(returned);
		while((cur = ksNext(returned)) != NULL)
		{
			if(matchPatternToKey(pattern, cur))
			{
				if(validateArray(cur) == -1)
					continue;
				keyCopyAllMeta(cur, specKey);
			}
		}
		ksRewind(returned);
		elektraFree(pattern);
	}
	ksAppend(returned, specKS);
	ksDel(specKS);
	ksDel(possibleMatches);
}

int elektraSpecGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/spec"))
	{
		KeySet * contract = ksNew (30,
				keyNew ("system/elektra/modules/spec",
					KEY_VALUE, "spec plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/spec/exports", KEY_END),
				keyNew ("system/elektra/modules/spec/exports/get",
					KEY_FUNC, elektraSpecGet, KEY_END),
				keyNew ("system/elektra/modules/spec/exports/set",
					KEY_FUNC, elektraSpecSet, KEY_END),
#include ELEKTRA_README (spec)
				keyNew ("system/elektra/modules/spec/infos/version",
					KEY_VALUE, PLUGINVERSION, KEY_END),
				KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	copyMeta(returned);
	return 1; // success
}

int elektraSpecSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	copyMeta(returned);
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (spec)
{
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
			ELEKTRA_PLUGIN_END);
}

