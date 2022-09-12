/**
 * @file
 *
 * @brief Source for passwd plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
#include <sys/types.h>

#define ID_MAX_CHARACTERS 11

typedef enum
{
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
	if (pwd->pw_uid == (uid_t) -1) return -1;
	if (pwd->pw_gid == (gid_t) -1) return -1;
	if (!pwd->pw_gecos) return -1;
	if (!pwd->pw_dir) return -1;
	if (!pwd->pw_shell) return -1;
	return 0;
}

#if defined(USE_FGETPWENT_LOCAL)
// clang-format off
/* Taken from musl libc
 *
 * https://github.com/ifduyue/musl/blob/b4b1e10364c8737a632be61582e05a8d3acf5690/src/passwd/getpwent_a.c
 * https://github.com/ifduyue/musl/blob/b4b1e10364c8737a632be61582e05a8d3acf5690/src/passwd/fgetpwent.c
 */

static unsigned atou(char **s)
{
	unsigned x;
	for (x=0; (unsigned char)(**s-'0')<10U; ++*s) x=10*x+((unsigned char)**s-'0');
	return x;
}

int __getpwent_a(FILE *f, struct passwd *pw, char **line, size_t *size, struct passwd **res)
{
	ssize_t l;
	char *s;
	int rv = 0;
	for (;;) {
		if ((l=getline(line, size, f)) < 0) {
			rv = ferror(f) ? errno : 0;
			free(*line);
			*line = 0;
			pw = 0;
			break;
		}
		line[0][l-1] = 0;

		s = line[0];
		pw->pw_name = s++;
		if (!(s = strchr(s, ':'))) continue;

		*s++ = 0; pw->pw_passwd = s;
		if (!(s = strchr(s, ':'))) continue;

		*s++ = 0; pw->pw_uid = atou(&s);
		if (*s != ':') continue;

		*s++ = 0; pw->pw_gid = atou(&s);
		if (*s != ':') continue;

		*s++ = 0; pw->pw_gecos = s;
		if (!(s = strchr(s, ':'))) continue;

		*s++ = 0; pw->pw_dir = s;
		if (!(s = strchr(s, ':'))) continue;

		*s++ = 0; pw->pw_shell = s;
		break;
	}
	*res = pw;
	if (rv) errno = rv;
	return rv;
}

struct passwd *fgetpwent_l(FILE *f)
{
	static char *line;
	static struct passwd pw;
	size_t size=0;
	struct passwd *res;
	__getpwent_a(f, &pw, &line, &size, &res);
	return res;
}
// clang-format on
#endif

static ElektraKeyset * pwentToKS (struct passwd * pwd, ElektraKey * parentKey, SortBy index)
{
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * append = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_END);
	char id[ID_MAX_CHARACTERS];
	if (index == UID)
	{
		snprintf (id, sizeof (id), "%u", pwd->pw_uid);
		elektraKeyAddBaseName (append, id);
		elektraKeySetBinary (append, 0, 0);
		elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
		elektraKeyAddBaseName (append, "name");
		elektraKeySetString (append, pwd->pw_name);
	}
	else
	{
		elektraKeyAddBaseName (append, pwd->pw_name);
		elektraKeySetBinary (append, 0, 0);
		elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
		snprintf (id, sizeof (id), "%u", pwd->pw_uid);
		elektraKeyAddBaseName (append, "uid");
		elektraKeySetString (append, id);
	}
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeySetString (append, 0);
	elektraKeySetBaseName (append, "shell");
	elektraKeySetString (append, pwd->pw_shell);
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeySetString (append, 0);
	elektraKeySetBaseName (append, "home");
	elektraKeySetString (append, pwd->pw_dir);
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeySetString (append, 0);
	elektraKeySetBaseName (append, "gid");
	snprintf (id, sizeof (id), "%u", pwd->pw_gid);
	elektraKeySetString (append, id);
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeySetString (append, 0);
	elektraKeySetBaseName (append, "passwd");
	elektraKeySetString (append, pwd->pw_passwd);
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeySetString (append, 0);
	elektraKeySetBaseName (append, "gecos");
	elektraKeySetString (append, pwd->pw_gecos);
	elektraKeysetAppendKey (ks, elektraKeyDup (append, ELEKTRA_KEY_CP_ALL));
	elektraKeyDel (append);
	return ks;
}

int elektraPasswdGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/passwd"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/passwd", ELEKTRA_KEY_VALUE, "passwd plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/passwd/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/passwd/exports/get", ELEKTRA_KEY_FUNC, elektraPasswdGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/passwd/exports/set", ELEKTRA_KEY_FUNC, elektraPasswdSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/passwd/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	// get all keys
	SortBy index;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * sortByKey = elektraKeysetLookupByName (config, "/index", 0);
	if (sortByKey)
	{
		if (!strcmp (elektraKeyString (sortByKey), "uid"))
			index = UID;
		else if (!strcmp (elektraKeyString (sortByKey), "name"))
			index = NAME;
		else
			index = UID;
	}
	else
		index = UID;
	struct passwd * pwd;
	FILE * pwfile = fopen (elektraKeyString (parentKey), "r");
	if (!pwfile)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open configuration file %s. Reason: %s\n", elektraKeyString (parentKey),
					     strerror (errno));
		return -1;
	}
#if defined(USE_FGETPWENT)
	while ((pwd = fgetpwent (pwfile)) != NULL)
#elif defined(USE_FGETPWENT_LOCAL)
	while ((pwd = fgetpwent_l (pwfile)) != NULL)
#else
#error Configuration error in CMakeLists.txt. Neither fgetpwent nor getline were provided. Please open an issue at https://issue.libelektra.org.
#endif
	{
		ElektraKeyset * ks = pwentToKS (pwd, parentKey, index);
		elektraKeysetAppend (returned, ks);
		elektraKeysetDel (ks);
	}
	fclose (pwfile);
	return 1; // success
}

static struct passwd * KStoPasswd (ElektraKeyset * ks, SortBy index)
{
	struct passwd * pwd = elektraMalloc (sizeof (struct passwd));
	elektraKeysetRewind (ks);
	ElektraKey * parent = elektraKeysetNext (ks);
	ElektraKey * lookup = elektraKeyDup (parent, ELEKTRA_KEY_CP_ALL);
	ElektraKey * found = NULL;
	if (index == UID)
	{
		found = elektraKeysetLookup (ks, parent, 0);
		if (!found)
			pwd->pw_uid = (uid_t) -1;
		else
			pwd->pw_uid = atoi (elektraKeyBaseName (found));
		elektraKeyAddBaseName (lookup, "name");
		found = elektraKeysetLookup (ks, lookup, 0);
		if (!found)
			pwd->pw_name = NULL;
		else
			pwd->pw_name = (char *) elektraKeyString (found);
	}
	else
	{
		found = elektraKeysetLookup (ks, parent, 0);
		if (!found)
			pwd->pw_name = NULL;
		else
			pwd->pw_name = (char *) elektraKeyBaseName (found);
		elektraKeyAddBaseName (lookup, "uid");
		found = elektraKeysetLookup (ks, lookup, 0);
		if (!found)
			pwd->pw_uid = (uid_t) -1;
		else
			pwd->pw_uid = atoi (elektraKeyString (found));
	}
	elektraKeySetBaseName (lookup, "shell");
	found = elektraKeysetLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_shell = NULL;
	else
		pwd->pw_shell = (char *) elektraKeyString (found);
	elektraKeySetBaseName (lookup, "gid");
	found = elektraKeysetLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_gid = (gid_t) -1;
	else
		pwd->pw_gid = atoi (elektraKeyString (found));
	elektraKeySetBaseName (lookup, "home");
	found = elektraKeysetLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_dir = NULL;
	else
		pwd->pw_dir = (char *) elektraKeyString (found);
	elektraKeySetBaseName (lookup, "gecos");
	found = elektraKeysetLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_gecos = "";
	else
		pwd->pw_gecos = (char *) elektraKeyString (found);
	elektraKeySetBaseName (lookup, "passwd");
	found = elektraKeysetLookup (ks, lookup, 0);
	if (!found)
		pwd->pw_passwd = "";
	else
		pwd->pw_passwd = (char *) elektraKeyString (found);
	elektraKeyDel (lookup);
	return pwd;
}

static int writeKS (ElektraKeyset * returned, ElektraKey * parentKey, SortBy index)
{
	FILE * pwfile = fopen (elektraKeyString (parentKey), "w");
	if (!pwfile)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open %s for writing\n. Reason: %s", elektraKeyString (parentKey),
					     strerror (errno));
		return -1;
	}
	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (!elektraKeyIsDirectlyBelow (parentKey, cur)) continue;
		ElektraKeyset * cutKS = elektraKeysetCut (returned, cur);
		struct passwd * pwd = KStoPasswd (cutKS, index);
		if (validatepwent (pwd) == -1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid passwd entry %s:%s:%u:%u:%s:%s:%s\n", pwd->pw_name,
								 pwd->pw_passwd, pwd->pw_uid, pwd->pw_gid, pwd->pw_gecos, pwd->pw_dir,
								 pwd->pw_shell);
		}
		else
		{
#if defined(USE_PUTPWENT)
			putpwent (pwd, pwfile);
#else
			fprintf (pwfile, "%s:%s:%u:%u:%s:%s:%s\n", pwd->pw_name, pwd->pw_passwd, pwd->pw_uid, pwd->pw_gid, pwd->pw_gecos,
				 pwd->pw_dir, pwd->pw_shell);
#endif
		}
		elektraFree (pwd);
		elektraKeysetAppend (returned, cutKS);
		elektraKeysetDel (cutKS);
	}
	fclose (pwfile);
	return 1;
}

int elektraPasswdSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	// this function is optional
	SortBy index;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * sortByKey = elektraKeysetLookupByName (config, "/index", 0);
	if (sortByKey)
	{
		if (!strcmp (elektraKeyString (sortByKey), "uid"))
			index = UID;
		else if (!strcmp (elektraKeyString (sortByKey), "name"))
			index = NAME;
		else
			index = UID;
	}
	else
		index = UID;

	int rc = writeKS (returned, parentKey, index);

	return rc; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("passwd",
	    ELEKTRA_PLUGIN_GET,	&elektraPasswdGet,
	    ELEKTRA_PLUGIN_SET,	&elektraPasswdSet,
	    ELEKTRA_PLUGIN_END);
}

