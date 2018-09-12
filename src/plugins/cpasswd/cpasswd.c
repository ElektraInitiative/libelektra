/**
 * @file
 *
 * @brief Source for cpasswd plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * some of the code is inspired by / taken from musl
 */

#include "cpasswd.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>

#include "getline.c"

static struct passwd * strToPasswd (char * line)
{
	struct passwd * pwd = elektraMalloc (sizeof (struct passwd));
	char * s = line;
	pwd->pw_name = s++;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_passwd = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_uid = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_gid = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_gecos = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_dir = s;
	if (!(s = strchr (s, ':'))) goto fail;
	*s++ = 0;
	pwd->pw_shell = s;
	return pwd;
fail:
	elektraFree (pwd);
	return NULL;
}

static KeySet * pwentToKS (struct passwd * pwd, Key * parentKey, SortBy index)
{
	const char * keys[PASSWD_FIELDS] = {
		"uid", "gid", "name", "passwd", "gecos", "home", "shell",
	};
	void * fields[PASSWD_FIELDS] = {
		pwd->pw_uid, pwd->pw_gid, pwd->pw_name, pwd->pw_passwd, pwd->pw_gecos, pwd->pw_dir, pwd->pw_shell,
	};
	KeySet * ks = ksNew (0, KS_END);
	Key * append = keyNew (keyName (parentKey), KEY_END);
	if (index == UID)
		keyAddBaseName (append, pwd->pw_uid);
	else
		keyAddBaseName (append, pwd->pw_name);
	keySetBinary (append, 0, 0);
	ksAppendKey (ks, keyDup (append));
	for (int i = 0; i < PASSWD_FIELDS; ++i)
	{
		keyAddBaseName (append, keys[i]);
		keySetString (append, fields[i]);
		ksAppendKey (ks, keyDup (append));
		keySetString (append, 0);
		keySetBaseName (append, 0);
	}
	keyDel (append);
	return ks;
}

int elektraCpasswdGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/cpasswd"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/cpasswd", KEY_VALUE, "cpasswd plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/cpasswd/exports", KEY_END),
			       keyNew ("system/elektra/modules/cpasswd/exports/get", KEY_FUNC, elektraCpasswdGet, KEY_END),
			       keyNew ("system/elektra/modules/cpasswd/exports/set", KEY_FUNC, elektraCpasswdSet, KEY_END),
#include ELEKTRA_README (cpasswd)
			       keyNew ("system/elektra/modules/cpasswd/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	SortBy index = UID;
	const Key * fullPathMeta = keyGetMeta (parentKey, "fullPath");
	if (fullPathMeta)
	{
		size_t cmpLen = strlen (keyName (parentKey)) + strlen ("/byName") + 1;
		char * comparePath = elektraMalloc (cmpLen);
		snprintf (comparePath, cmpLen - 1, "%s/byName", keyName (parentKey));
		if (!strncmp (comparePath, keyString (fullPathMeta), strlen (comparePath)))
		{
			keyAddBaseName (parentKey, "byName");
			index = NAME;
		}
		elektraFree (comparePath);
	}
	FILE * f = fopen (keyString (parentKey), "r");
	if (!f)
	{
		ELEKTRA_SET_ERRORF (110, parentKey, "Failed to open %s for reading\n", keyString (parentKey));
		return -1;
	}
	char * line = NULL;
	size_t len = 0;
	ssize_t l = 0;
	struct passwd * pwd;
	size_t lineno = 0;
	while ((l = __getline (&line, &len, f)) != -1)
	{
		line[l - 1] = '\0';
		pwd = strToPasswd (line);
		if (!pwd)
		{
			ELEKTRA_ADD_WARNINGF (201, parentKey, "Failed to parse line %ld: '%s'... of passwd file, skipping to next line\n",
					      lineno, line);
			++lineno;
			continue;
		}
		++lineno;
		KeySet * ks = pwentToKS (pwd, parentKey, index);
		ksAppend (returned, ks);
		ksDel (ks);
		elektraFree (pwd);
	}
	elektraFree (line);
	fclose (f);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int writeKS (FILE * f, const Key * key, KeySet * ks)
{
	const char * keys[PASSWD_FIELDS] = {
		"uid", "gid", "name", "passwd", "gecos", "home", "shell",
	};
	struct passwd * pw = elektraMalloc (sizeof (struct passwd));
	void * fields[PASSWD_FIELDS] = {
		&pw->pw_uid, &pw->pw_gid, &pw->pw_name, &pw->pw_passwd, &pw->pw_gecos, &pw->pw_dir, &pw->pw_shell,
	};
	Key * searchKey = keyNew (keyName (key), KEY_END);
	int rv = 1;
	for (int i = 0; i < PASSWD_FIELDS; ++i)
	{
		keyAddBaseName (searchKey, keys[i]);
		Key * lookup = ksLookup (ks, searchKey, KDB_O_NONE);
		*(char **) (fields[i]) = (char *) (keyString (lookup));
		keySetBaseName (searchKey, 0);
	}
	rv = fprintf (f, "%s:%s:%s:%s:%s:%s:%s\n", pw->pw_name, pw->pw_passwd, pw->pw_uid, pw->pw_gid, pw->pw_gecos, pw->pw_dir,
		      pw->pw_shell);
	if (rv < 0)
		rv = -1;
	else if (rv > 0)
		rv = 1;
	keyDel (searchKey);
	elektraFree (pw);
	return rv;
}

int elektraCpasswdSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	const Key * fullPathMeta = keyGetMeta (parentKey, "fullPath");
	if (fullPathMeta)
	{
		size_t cmpLen = strlen (keyName (parentKey)) + strlen ("/byName") + 1;
		char * comparePath = elektraMalloc (cmpLen);
		snprintf (comparePath, cmpLen - 1, "%s/byName", keyName (parentKey));
		if (!strncmp (comparePath, keyString (fullPathMeta), strlen (comparePath))) keyAddBaseName (parentKey, "byName");
		elektraFree (comparePath);
	}
	Key * cur;
	ksRewind (returned);
	FILE * f = fopen (keyString (parentKey), "w");
	if (!f)
	{
		ELEKTRA_SET_ERRORF (75, parentKey, "Failed to open %s for writing\n", keyString (parentKey));
		return -1;
	}
	int rv = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyIsDirectBelow (parentKey, cur) != 1) continue;
		rv = writeKS (f, cur, returned);
		if (rv == -1) break;
	}
	fclose (f);
	return rv;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (cpasswd)
{
	// clang-format off
    return elektraPluginExport ("cpasswd",
            ELEKTRA_PLUGIN_GET,	&elektraCpasswdGet,
            ELEKTRA_PLUGIN_SET,	&elektraCpasswdSet,
            ELEKTRA_PLUGIN_END);
}
