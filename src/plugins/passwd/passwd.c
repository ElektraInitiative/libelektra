/**
 * @file
 *
 * @brief Source for passwd plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "passwd.h"

#include <kdberrors.h>
#include <kdbhelper.h>

#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define ID_MAX_CHARACTERS 11

typedef enum {
	NAME,
	UID,
} SortBy;

// simple validation of passwd entries
static int validatepwent (struct passwd * pwd)
{
	if (!pwd->pw_name) return -1;
	if (strlen (pwd->pw_name) == 0) return -1;
	if (pwd->pw_name[0] == '-') // POSIX.1-2008 3.431 User Name
		return -1;
	const char * invalidCharacters =
		"/:;<=>?@[\\]^`"; // POSIX.1-2008 3.278 Portable File Character Set - invalid characters > 45 && <= 122
	for (char * ptr = pwd->pw_name; *ptr != '\0'; ++ptr)
	{
		if ((*ptr < '-') || (*ptr > 'z') || (strchr (invalidCharacters, *ptr) != NULL)) return -1;
	}
	if (!pwd->pw_passwd) return -1;
	if (pwd->pw_uid == (uid_t)-1) return -1;
	if (pwd->pw_gid == (gid_t)-1) return -1;
	if (!pwd->pw_gecos) return -1;
	if (!pwd->pw_dir) return -1;
	if (!pwd->pw_shell) return -1;
	return 0;
}

static KeySet * pwentToKS (struct passwd * pwd, Key * parentKey, SortBy index)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * append = keyNew (keyName (parentKey), KEY_END);
	char id[ID_MAX_CHARACTERS];
	if (index == UID)
	{
		snprintf (id, sizeof (id), "%u", pwd->pw_uid);
		keyAddBaseName (append, id);
		keySetBinary (append, 0, 0);
		ksAppendKey (ks, keyDup (append));
		keyAddBaseName (append, "name");
		keySetString (append, pwd->pw_name);
	}
	else
	{
		keyAddBaseName (append, pwd->pw_name);
		keySetBinary (append, 0, 0);
		ksAppendKey (ks, keyDup (append));
		snprintf (id, sizeof (id), "%u", pwd->pw_uid);
		keyAddBaseName (append, "uid");
		keySetString (append, id);
	}
	ksAppendKey (ks, keyDup (append));
	keySetString (append, 0);
	keySetBaseName (append, "shell");
	keySetString (append, pwd->pw_shell);
	ksAppendKey (ks, keyDup (append));
	keySetString (append, 0);
	keySetBaseName (append, "home");
	keySetString (append, pwd->pw_dir);
	ksAppendKey (ks, keyDup (append));
	keySetString (append, 0);
	keySetBaseName (append, "gid");
	snprintf (id, sizeof (id), "%u", pwd->pw_gid);
	keySetString (append, id);
	ksAppendKey (ks, keyDup (append));
	keySetString (append, 0);
	keySetBaseName (append, "passwd");
	keySetString (append, pwd->pw_passwd);
	ksAppendKey (ks, keyDup (append));
	keySetString (append, 0);
	keySetBaseName (append, "gecos");
	keySetString (append, pwd->pw_gecos);
	ksAppendKey (ks, keyDup (append));
	keyDel (append);
	return ks;
}

int elektraPasswdGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/passwd"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/passwd", KEY_VALUE, "passwd plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/passwd/exports", KEY_END),
			       keyNew ("system/elektra/modules/passwd/exports/get", KEY_FUNC, elektraPasswdGet, KEY_END),
			       keyNew ("system/elektra/modules/passwd/exports/set", KEY_FUNC, elektraPasswdSet, KEY_END),
#include ELEKTRA_README (passwd)
			       keyNew ("system/elektra/modules/passwd/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	SortBy index;
	KeySet * config = elektraPluginGetConfig (handle);
	Key * sortByKey = ksLookupByName (config, "/index", 0);
	if (sortByKey)
	{
		if (!strcmp (keyString (sortByKey), "uid"))
			index = UID;
		else if (!strcmp (keyString (sortByKey), "name"))
			index = NAME;
		else
			index = UID;
	}
	else
		index = UID;
	struct passwd * pwd;
#if HAS_FGETPWENT
	FILE * pwfile = fopen (keyString (parentKey), "r");
	if (!pwfile)
	{
		ELEKTRA_SET_ERRORF (110, parentKey, "Failed to open configuration file %s\n", keyString (parentKey));
		return -1;
	}
	while ((pwd = fgetpwent (pwfile)) != NULL)
#else
	while ((pwd = getpwent ()) != NULL)
#endif
	{
		KeySet * ks = pwentToKS (pwd, parentKey, index);
		ksAppend (returned, ks);
		ksDel (ks);
	}
	endpwent ();
#if HAS_FGETPWENT
	fclose (pwfile);
#endif
	return 1; // success
}

static struct passwd * KStoPasswd (KeySet * ks, SortBy index)
{
	struct passwd * pwd = elektraMalloc (sizeof (struct passwd));
	ksRewind (ks);
	Key * parent = ksNext (ks);
	Key * lookup = keyDup (parent);
	Key * found = NULL;
	if (index == UID)
	{
		found = ksLookup (ks, parent, 0);
		if (!found)
			pwd->pw_uid = (uid_t)-1;
		else
			pwd->pw_uid = atoi (keyBaseName (found));
		keyAddBaseName (lookup, "name");
		found = ksLookup (ks, lookup, 0);
		if (!found)
			pwd->pw_name = NULL;
		else
			pwd->pw_name = (char *)keyString (found);
	}
	else
	{
		found = ksLookup (ks, parent, 0);
		if (!found)
			pwd->pw_name = NULL;
		else
			pwd->pw_name = (char *)keyBaseName (found);
		keyAddBaseName (lookup, "uid");
		found = ksLookup (ks, lookup, 0);
		if (!found)
			pwd->pw_uid = (uid_t)-1;
		else
			pwd->pw_uid = atoi (keyString (found));
	}
	keySetBaseName (lookup, "shell");
	found = ksLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_shell = NULL;
	else
		pwd->pw_shell = (char *)keyString (found);
	keySetBaseName (lookup, "gid");
	found = ksLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_gid = (gid_t)-1;
	else
		pwd->pw_gid = atoi (keyString (found));
	keySetBaseName (lookup, "home");
	found = ksLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_dir = NULL;
	else
		pwd->pw_dir = (char *)keyString (found);
	keySetBaseName (lookup, "gecos");
	found = ksLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_gecos = "";
	else
		pwd->pw_gecos = (char *)keyString (found);
	keySetBaseName (lookup, "passwd");
	found = ksLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_passwd = "";
	else
		pwd->pw_passwd = (char *)keyString (found);
	keyDel (parent);
	keyDel (lookup);
	return pwd;
}

static int writeKS (KeySet * returned, Key * parentKey, SortBy index)
{
	FILE * pwfile = fopen (keyString (parentKey), "w");
	if (!pwfile)
	{
		ELEKTRA_SET_ERRORF (75, parentKey, "Failed to open %s for writing\n", keyString (parentKey));
		return -1;
	}
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!keyIsDirectBelow (parentKey, cur)) continue;
		KeySet * cutKS = ksCut (returned, cur);
		struct passwd * pwd = KStoPasswd (cutKS, index);
		if (validatepwent (pwd) == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_PASSWD_VALIDATION, parentKey, "Invalid passwd entry %s:%s:%u:%u:%s:%s:%s\n",
					    pwd->pw_name, pwd->pw_passwd, pwd->pw_uid, pwd->pw_gid, pwd->pw_gecos, pwd->pw_dir,
					    pwd->pw_shell);
		}
		else
		{
#if HAS_PUTPWENT
			putpwent (pwd, pwfile);
#else
			fprintf (pwfile, "%s:%s:%u:%u:%s:%s:%s\n", pwd->pw_name, pwd->pw_passwd, pwd->pw_uid, pwd->pw_gid, pwd->pw_gecos,
				 pwd->pw_dir, pwd->pw_shell);
#endif
		}
		elektraFree (pwd);
		ksAppend (returned, cutKS);
		ksDel (cutKS);
	}
	fclose (pwfile);
	return 1;
}

int elektraPasswdSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	// this function is optional
	SortBy index;
	KeySet * config = elektraPluginGetConfig (handle);
	Key * sortByKey = ksLookupByName (config, "/index", 0);
	if (sortByKey)
	{
		if (!strcmp (keyString (sortByKey), "uid"))
			index = UID;
		else if (!strcmp (keyString (sortByKey), "name"))
			index = NAME;
		else
			index = UID;
	}
	else
		index = UID;

	int rc = writeKS (returned, parentKey, index);

	return rc; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (passwd)
{
	// clang-format off
    return elektraPluginExport ("passwd",
	    ELEKTRA_PLUGIN_GET,	&elektraPasswdGet,
	    ELEKTRA_PLUGIN_SET,	&elektraPasswdSet,
	    ELEKTRA_PLUGIN_END);
}

