/**
 * @file
 *
 * @brief Source for group plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * some of the code is inspired by / taken from musl
 */

#include "group.h"
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "../cpasswd/getline.c"

static struct group * strToGroup (char * line)
{
	struct group * grp = elektraCalloc (sizeof (struct group));
	char * s = line;
	grp->gr_name = s++;
	if (!(s = strchr (s, ':'))) goto cleanup;
	*s++ = 0;
	grp->gr_passwd = s;
	if (!(s = strchr (s, ':'))) goto cleanup;
	*s++ = 0;
	grp->gr_gid = s;
	if (!(s = strchr (s, ':'))) goto cleanup;
	*s++ = 0;
	grp->gr_mem = (char **) s;
	return grp;
cleanup:
	elektraFree (grp);
	return NULL;
}

static KeySet * grpentToKS (struct group * grp, Key * parentKey, SortBy index)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * append = keyNew (keyName (parentKey), KEY_END);
	if (index == GID)
		keyAddBaseName (append, grp->gr_gid);
	else
		keyAddBaseName (append, grp->gr_name);
	keySetBinary (append, 0, 0);
	ksAppendKey (ks, keyDup (append));

	const char * keys[GROUP_FIELDS] = {
		"name",
		"gid",
		"passwd",
	};
	void * fields[GROUP_FIELDS] = {
		grp->gr_name,
		grp->gr_gid,
		grp->gr_passwd,
	};
	for (int i = 0; i < GROUP_FIELDS; ++i)
	{
		keyAddBaseName (append, keys[i]);
		keySetString (append, fields[i]);
		ksAppendKey (ks, keyDup (append));
		keySetString (append, 0);
		keySetBaseName (append, 0);
	}

	Key * membersKey = keyNew (keyName (append), KEY_END);
	keySetBaseName (membersKey, "members");
	keySetBaseName (append, "members");
	keyAddBaseName (append, "#");
	char * ptr = (char *) grp->gr_mem;
	while (*ptr != '\0')
	{
		char * s = ptr;
		while (*ptr)
		{
			++(ptr);
			if (*ptr == ',') break;
		}
		if (*ptr == ',')
		{
			*ptr = '\0';
			if (elektraArrayIncName (append) == -1) break;
			Key * memKey = keyDup (append);
			keySetString (memKey, s);
			ksAppendKey (ks, memKey);
			keySetString (membersKey, keyBaseName (append));
			++(ptr);
		}
		else if (*ptr == '\0' && ptr > s)
		{
			if (elektraArrayIncName (append) == -1) break;
			Key * memKey = keyDup (append);
			keySetString (memKey, s);
			ksAppendKey (ks, memKey);
			keySetString (membersKey, keyBaseName (append));
			break;
		}
		else
			break;
	}
	ksAppendKey (ks, membersKey);
	keyDel (append);
	return ks;
}

int elektraGroupGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/group"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/group", KEY_VALUE, "group plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/group/exports", KEY_END),
			       keyNew ("system/elektra/modules/group/exports/get", KEY_FUNC, elektraGroupGet, KEY_END),
			       keyNew ("system/elektra/modules/group/exports/set", KEY_FUNC, elektraGroupSet, KEY_END),
#include ELEKTRA_README (group)
			       keyNew ("system/elektra/modules/group/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	SortBy index = GID;
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
	struct group * grp;
	size_t lineno = 0;
	while ((l = __getline (&line, &len, f)) != -1)
	{
		line[l - 1] = '\0';
		grp = strToGroup (line);
		if (!grp)
		{
			ELEKTRA_ADD_WARNINGF (201, parentKey, "Failed to parse line %ld '%s'... of group file, skipping to next line\n",
					      lineno, line);
			++lineno;
			continue;
		}
		++lineno;
		KeySet * ks = grpentToKS (grp, parentKey, index);
		ksAppend (returned, ks);
		ksDel (ks);
		elektraFree (grp);
	}
	elektraFree (line);
	fclose (f);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int writeKS (FILE * f, const Key * key, KeySet * ks)
{
	Key * searchKey = keyNew (keyName (key), KEY_END);
	struct group * grp = elektraMalloc (sizeof (struct group));
	keyAddBaseName (searchKey, "gid");
	Key * lookup = ksLookup (ks, searchKey, KDB_O_NONE);
	grp->gr_gid = (char *) keyString (lookup);
	keySetBaseName (searchKey, "name");
	lookup = ksLookup (ks, searchKey, KDB_O_NONE);
	grp->gr_name = (char *) keyString (lookup);
	keySetBaseName (searchKey, "passwd");
	lookup = ksLookup (ks, searchKey, KDB_O_NONE);
	grp->gr_passwd = (char *) keyString (lookup);
	keySetBaseName (searchKey, "members");
	int rv = 0;
	rv = fprintf (f, "%s:%s:%s:", grp->gr_name, grp->gr_passwd, grp->gr_gid);
	if (rv < 0) goto cleanuponfail;
	lookup = ksLookup (ks, searchKey, KDB_O_NONE);
	if (keyString (lookup)[0] == '#')
	{
		KeySet * cutKS = ksCut (ks, lookup);
		ksRewind (cutKS);
		Key * cur;
		char delim[2] = "";
		while ((cur = ksNext (cutKS)) != NULL)
		{
			if (!keyCmp (lookup, cur)) continue;
			rv = fprintf (f, "%s%s", delim, keyString (cur));
			if (rv < 0)
			{
				ksAppend (ks, cutKS);
				ksDel (cutKS);
				goto cleanuponfail;
			}
			strncpy (delim, ",", 2);
		}
		ksAppend (ks, cutKS);
		ksDel (cutKS);
	}
	fprintf (f, "\n");
	keyDel (searchKey);
	elektraFree (grp);
	return 1;

cleanuponfail:
	keyDel (searchKey);
	elektraFree (grp);
	return -1;
}

int elektraGroupSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{

	const Key * fullPathMeta = keyGetMeta (parentKey, "fullPath");
	if (fullPathMeta)
	{
		size_t cmpLen = strlen (keyName (parentKey)) + strlen ("/byName") + 1;
		char * comparePath = elektraMalloc (cmpLen);
		snprintf (comparePath, cmpLen - 1, "%s/byName", keyName (parentKey));
		if (!strncmp (comparePath, keyString (fullPathMeta), strlen (comparePath)))
		{
			keyAddBaseName (parentKey, "byName");
		}
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

Plugin * ELEKTRA_PLUGIN_EXPORT (group)
{
	// clang-format off
    return elektraPluginExport ("group",
            ELEKTRA_PLUGIN_GET,	&elektraGroupGet,
            ELEKTRA_PLUGIN_SET,	&elektraGroupSet,
            ELEKTRA_PLUGIN_END);
}
