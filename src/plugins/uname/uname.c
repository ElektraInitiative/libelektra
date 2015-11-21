/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "uname.h"

#include <string.h>
#include <errno.h>
#include <sys/utsname.h>

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdberrors.h>

#if DEBUG && VERBOSE
# include <stdio.h>
#endif

static void elektraAddUname(KeySet *returned, Key *parentKey)
{
	Key *dir;
	Key *key = keyDup (parentKey);
	ksAppendKey(returned, key);

	struct utsname buf;

	uname(&buf); // TODO: handle error

	dir = keyDup (parentKey);
	keyAddBaseName(dir, "sysname");
	keySetString(dir,buf.sysname);
	ksAppendKey(returned,dir);

	dir = keyDup (parentKey);
	keyAddBaseName(dir, "nodename");
	keySetString(dir,buf.nodename);
	ksAppendKey(returned,dir);

	dir = keyDup (parentKey);
	keyAddBaseName(dir, "release");
	keySetString(dir,buf.release);
	ksAppendKey(returned,dir);

	dir = keyDup (parentKey);
	keyAddBaseName(dir, "version");
	keySetString(dir,buf.version);
	ksAppendKey(returned,dir);

	dir = keyDup (parentKey);
	keyAddBaseName(dir, "machine");
	keySetString(dir,buf.machine);
	ksAppendKey(returned,dir);
}

int elektraUnameGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
#if DEBUG && VERBOSE
	printf ("get uname %s from %s\n", keyName(parentKey), keyString(parentKey));
#endif

	if (!strcmp (keyName(parentKey), "system/elektra/modules/uname"))
	{
		KeySet *moduleConfig = ksNew (50,
			keyNew ("system/elektra/modules/uname",
				KEY_VALUE, "uname plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/uname/exports", KEY_END),
			keyNew ("system/elektra/modules/uname/exports/get",
				KEY_FUNC, elektraUnameGet,
				KEY_END),
			keyNew ("system/elektra/modules/uname/exports/set",
				KEY_FUNC, elektraUnameSet,
				KEY_END),
#include "readme_uname.c"
			keyNew ("system/elektra/modules/uname/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	elektraAddUname(returned, parentKey);

	errno = errnosave;
	return 1;
}

int elektraUnameSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{
#if DEBUG && VERBOSE
	printf ("set uname %s from %s\n", keyName(parentKey), keyString(parentKey));
#endif

	KeySet *info = ksNew(0, KS_END);
	elektraAddUname(info, parentKey);

	Key *k;
	ksRewind(info);
	ksRewind(returned);
	while ((k = ksNext(returned)))
	{
		Key *c = ksNext(info);
		if (strcmp(keyName(k), keyName(c)) || strcmp(keyString(k), keyString(c)))
		{
			ELEKTRA_SET_ERRORF(84, parentKey, "the key %s (expected %s) was modified to %s (expected %s)", keyName(k), keyName(c), keyString(k), keyString(c));
			return -1;
		}
	}

	ksDel (info);
	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(uname) {
	return elektraPluginExport("uname",
		ELEKTRA_PLUGIN_GET,            &elektraUnameGet,
		ELEKTRA_PLUGIN_SET,            &elektraUnameSet,
		ELEKTRA_PLUGIN_END);
}


