/**
 * @file
 *
 * @brief Source for chosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "chosts.h"
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdberrors.h>
#include <kdbhelper.h>

#include "inet_XX.c"

#include "../cpasswd/getline.c"

static int getAddressFamily (const char * address)
{
	struct in_addr addr;
	if (__inet_aton (address, &addr)) return AF_INET;
	unsigned char buf[sizeof (struct in6_addr)];
	int s = __inet_pton (AF_INET6, address, buf);
	if (s <= 0)
	{
		if (s == 0)
			return 0;
		else
			return -1;
	}
	else
		return AF_INET6;
}

static char * parseLine (char * line, char ** endPtr)
{
	char * ptr = line;
	*endPtr = NULL;
	while (*ptr && isspace (*ptr))
		++ptr;
	if ((!*ptr)) return NULL;
	char * startPtr = ptr;
	while (*ptr && !isspace (*ptr))
		++ptr;
	if (*ptr) *ptr++ = '\0';
	*endPtr = ptr;
	return startPtr;
}

static struct hostent * strToHent (char * line)
{
	struct hostent * hent = elektraCalloc (sizeof (struct hostent));
	char * endPtr;
	char * ptr = parseLine (line, &endPtr);
	int af = getAddressFamily (ptr);
	if (af <= 0) goto fail;
	hent->h_addrtype = af;
	hent->h_length = af == AF_INET6 ? 16 : 4;
	hent->h_addr_list = elektraMalloc (2 * sizeof (char *));
	hent->h_addr_list[0] = ptr;
	hent->h_addr_list[1] = NULL;
	// skip spaces
	char * canon = parseLine (endPtr, &endPtr);
	if (!canon) goto fail;
	hent->h_name = canon;
	size_t aliasCount = 0;
	if (endPtr && (*endPtr + 1 != '\0'))
	{
		ptr = parseLine (endPtr, &endPtr);
		while (ptr)
		{
			++aliasCount;
			size_t newLen = ((aliasCount + 1) * (sizeof (char *)));
			elektraRealloc ((void **) &hent->h_aliases, newLen);
			hent->h_aliases[aliasCount - 1] = ptr;
			hent->h_aliases[aliasCount] = NULL;
			ptr = parseLine (endPtr, &endPtr);
		}
	}
	return hent;
fail:
	if (hent->h_addr_list) elektraFree (hent->h_addr_list);
	if (hent->h_aliases) elektraFree (hent->h_aliases);
	elektraFree (hent);
	return NULL;
}

static inline void appendAddressFamily (Key * key, struct hostent * hent)
{
	switch (hent->h_addrtype)
	{
	case AF_INET:
		keyAddBaseName (key, "ipv4");
		break;
	case AF_INET6:
		keyAddBaseName (key, "ipv6");
		break;
	default:
		break;
	}
}

static inline void addAliases (KeySet * ks, Key * append, struct hostent * hent)
{
	for (size_t i = 0; hent->h_aliases && hent->h_aliases[i]; ++i)
	{
		keyAddBaseName (append, hent->h_aliases[i]);
		keySetString (append, hent->h_addr_list[0]);
		ksAppendKey (ks, keyDup (append));
		keySetBaseName (append, 0);
	}
}

static KeySet * hentToKS (struct hostent * hent, KeySet * returned, Key * parentKey, SortBy index)
{
	Key * append = keyNew (keyName (parentKey), KEY_END);
	KeySet * ks = NULL;
	if (index == NAME)
	{
		keyAddBaseName (append, hent->h_name);
		Key * existingHost = ksLookup (returned, append, KDB_O_NONE);
		if (existingHost)
		{
			keyDel (append);
			append = keyNew (keyName (existingHost), KEY_END);
			appendAddressFamily (append, hent);
			keySetString (append, hent->h_addr_list[0]);
			if (!ksLookup (returned, append, KDB_O_NONE))
			{
				ksAppendKey (returned, keyDup (append));
			}
			addAliases (returned, append, hent);
		}
		else
		{
			ks = ksNew (0, KS_END);
			keySetBinary (append, 0, 0);
			ksAppendKey (ks, keyDup (append));
			appendAddressFamily (append, hent);
			keySetString (append, hent->h_addr_list[0]);
			ksAppendKey (ks, keyDup (append));
			addAliases (ks, append, hent);
		}
	}
	else
	{
		appendAddressFamily (append, hent);
		keyAddBaseName (append, hent->h_addr_list[0]);
		Key * existingHost = ksLookup (returned, append, KDB_O_NONE);
		if (existingHost)
		{
			keyDel (append);
			append = keyNew (keyName (existingHost), KEY_END);
			keySetString (append, hent->h_addr_list[0]);
			addAliases (returned, append, hent);
		}
		else
		{
			ks = ksNew (0, KS_END);
			keySetString (append, hent->h_name);
			ksAppendKey (ks, keyDup (append));
			addAliases (ks, append, hent);
		}
	}
	keyDel (append);
	return ks;
}

int elektraChostsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/chosts"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/chosts", KEY_VALUE, "chosts plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/chosts/exports", KEY_END),
			       keyNew ("system/elektra/modules/chosts/exports/get", KEY_FUNC, elektraChostsGet, KEY_END),
			       keyNew ("system/elektra/modules/chosts/exports/set", KEY_FUNC, elektraChostsSet, KEY_END),
#include ELEKTRA_README (chosts)
			       keyNew ("system/elektra/modules/chosts/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	SortBy index = NAME;
	const Key * fullPathMeta = keyGetMeta (parentKey, "fullPath");
	if (fullPathMeta)
	{
		size_t cmpLen = strlen (keyName (parentKey)) + strlen ("/byAddress") + 1;
		char * comparePath = elektraMalloc (cmpLen);
		snprintf (comparePath, cmpLen - 1, "%s/byAddress", keyName (parentKey));
		if (!strncmp (comparePath, keyString (fullPathMeta), strlen (comparePath)))
		{
			keyAddBaseName (parentKey, "byAddress");
			index = ADDRESS;
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
	ssize_t lineno = -1;
	struct hostent * hent;
	while ((l = __getline (&line, &len, f)) != -1)
	{
		++lineno;
		line[l - 1] = '\0';
		// drop comments
		char * ptr;
		if ((ptr = strchr (line, '#')) != NULL)
		{
			*ptr = '\0';
			l = ptr - line + 1;
		}
		ptr = line;
		while (*ptr && isspace (*ptr))
			++ptr;
		if (ptr == line + l - 1) // line contains only a comment or spaces, skip line
			continue;
		hent = strToHent (line);
		if (!hent)
		{
			ELEKTRA_ADD_WARNINGF (201, parentKey, "Failed to parese line %ld: '%s'... of hosts file, skipping to next record\n",
					      lineno, line);
			continue;
		}
		KeySet * ks = hentToKS (hent, returned, parentKey, index);
		if (ks)
		{
			ksAppend (returned, ks);
			ksDel (ks);
		}
		if (hent->h_addr_list) elektraFree (hent->h_addr_list);
		if (hent->h_aliases) elektraFree (hent->h_aliases);
		elektraFree (hent);
	}
	elektraFree (line);
	fclose (f);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraChostsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (chosts)
{
	// clang-format off
    return elektraPluginExport ("chosts",
            ELEKTRA_PLUGIN_GET,	&elektraChostsGet,
            ELEKTRA_PLUGIN_SET,	&elektraChostsSet,
            ELEKTRA_PLUGIN_END);
}
