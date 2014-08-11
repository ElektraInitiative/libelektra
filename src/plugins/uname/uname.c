/***************************************************************************
            uname.c  -  Access the /etc/uname file
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Markus Raab
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
 *   This is a backend that takes /etc/uname file as its backend storage.  *
 *   The kdbGet() method will parse /etc/uname and generate a              *
 *   valid key tree. The kdbSet() method will take a KeySet with valid     *
 *   filesystem keys and print an equivalent regular uname in stdout.      *
 *                                                                         *
 ***************************************************************************/

#include "uname.h"

#include <string.h>
#include <errno.h>
#include <sys/utsname.h>

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <kdberrors.h>

int elektraUnameGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	Key *key;
	Key *dir;

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
			keyNew ("system/elektra/modules/uname/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/description",
				KEY_VALUE,
"Includes uname information into the key database.\n"
"\n"
"It defines following keynames below its mountpoint:\n"
"\n"
"sysname\n"
"nodename\n"
"release\n"
"version\n"
"machine\n"
"\n"
"To mount it, use kdb mount -R noresolver none system/uname uname \n"
"\n"
"The plugin is readonly.\n"
				, KEY_END),
			keyNew ("system/elektra/modules/uname/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/recommends",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/uname/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	key = keyDup (parentKey);
	ksAppendKey(returned, key);

	struct utsname buf;

	int ret = uname(&buf);

	if (ret != 0)
	{
		// TODO: set error
		return -1;
	}

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

	errno = errnosave;
	return 1;
}

int elektraUnameSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{
#if DEBUG && VERBOSE
	printf ("set uname %s from %s\n", keyName(parentKey), keyString(parentKey));
#endif

	ELEKTRA_SET_ERROR(84, parentKey, keyName(parentKey));

	return -1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(uname) {
	return elektraPluginExport("uname",
		ELEKTRA_PLUGIN_GET,            &elektraUnameGet,
		ELEKTRA_PLUGIN_SET,            &elektraUnameSet,
		ELEKTRA_PLUGIN_END);
}


