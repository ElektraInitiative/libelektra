/**
 * @file
 *
 * @brief Source for macaddr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "macaddr.h"

#include <assert.h>
#include <ctype.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <regex.h>
#include <stdio.h>

#define SEPARATORSTANDARD 5
#define SEPARATOREONE 1
#define SEPARATORENONE 0

#define MAXMACINT 281474976710655

static void insertSeperator (char * mac)
{
	for (int i = 2; i <= 14; i += 3)
	{
		mac[i] = ':';
	}
	mac[17] = '\0';
}

void transformMac (Key * key)
{
	char * mac = keyString (key);

	int separatorOccurrences = 0;

	for (int i = 0; i < strlen (mac); ++i)
	{
		mac[i] = toupper (mac[i]);
		if (mac[i] == ':' || mac[i] == '-')
		{
			++separatorOccurrences;
		}
	}

	if (separatorOccurrences == SEPARATORSTANDARD)
	{
		insertSeperator (mac);
		keySetString (key, mac);
		keySetName (key, mac);
	}
	else if (separatorOccurrences == SEPARATOREONE)
	{
		char * newmac = (char *) elektraMalloc (strlen (mac) + 5);
		int j = 0;
		for (int i = 0; i <= 11; i += 2)
		{
			if (i == 6)
			{
				++i;
			}
			memcpy (newmac + j, mac + i, 2);
			j += 3;
		}
		insertSeperator (newmac);
		keySetString (key, newmac);
		elektraFree (newmac);
	}
	else if (separatorOccurrences == SEPARATORENONE)
	{
		char * newmac = (char *) elektraMalloc (strlen (mac) + 6);
		int j = 0;
		for (int i = 0; i <= 11; i += 2)
		{
			memcpy (newmac + j, mac + i, 2);
			j += 3;
		}
		insertSeperator (newmac);
		keySetString (key, newmac);
		elektraFree (newmac);
	}
}

int checkRegex (const char * mac, const char * regexString)
{
	regex_t regex;

	int reg = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (reg) return -1;

	reg = regexec (&regex, mac, 0, NULL, 0);
	regfree (&regex);

	if (reg == REG_NOMATCH)
	{
		return 1;
	}
	else
	{
		return 0;
	}
	// return reg == REG_NOMATCH ? 1 : 0;
}

int checkIntMac (const char * mac)
{
	if (strlen (mac) < 2) return 1;

	char * test = elektraMalloc (strlen (mac));
	strcpy (test, mac);

	while (*test)
	{
		if (isdigit (*test++) == 0) return 1;
	}
	long macLong = atol (mac);

	if (macLong > MAXMACINT || macLong < 0) return 1;

	return 0;
}

int validateMac (Key * key)
{
	const Key * metaKey = keyGetMeta (key, "check/macaddr");
	if (!metaKey) return 1;

	const char * mac = keyString (key);

	const char * regexColon = "^([0-9A-Fa-f]{2}:){5}([0-9A-Fa-f]{2})$";
	const char * regexHyphen = "^([0-9A-Fa-f]{2}-){5}([0-9A-Fa-f]{2})$";
	const char * regexHyphenSingle = "^([0-9A-Fa-f]{6}-)([0-9A-Fa-f]{6})$";

	const char * regexStrings[] = { regexColon, regexHyphen, regexHyphenSingle };

	int ret;
	int i = 0;

	ret = checkIntMac (mac);

	while (ret == 1 && i < 3)
	{
		ret = checkRegex (mac, regexStrings[i]);
		++i;
	}

	return ret;
}

void removeStandardizedSeparator (const char * mac, char * returned)
{
	int j = 0;
	for (int i = 0; i < strlen (mac); i += 3)
	{
		returned[j++] = mac[i];
		returned[j++] = mac[i + 1];
	}
}

void transformToInt (Key * key)
{
	const char * mac = keyString (key);

	char * macReduced = elektraMalloc (12);

	removeStandardizedSeparator (mac, macReduced);

	long intValue = strtol (macReduced, NULL, 16);

	const int n = snprintf (NULL, 0, "%lu", intValue);
	char * buffer = elektraMalloc (n + 1);
	snprintf (buffer, n + 1, "%lu", intValue);

	keySetString (key, buffer);
	elektraFree (macReduced);
	elektraFree (buffer);
}

int elektraMacaddrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/macaddr"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/macaddr", KEY_VALUE, "macaddr plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/macaddr/exports", KEY_END),
			       keyNew ("system/elektra/modules/macaddr/exports/get", KEY_FUNC, elektraMacaddrGet, KEY_END),
			       keyNew ("system/elektra/modules/macaddr/exports/set", KEY_FUNC, elektraMacaddrSet, KEY_END),

#include ELEKTRA_README

			       keyNew ("system/elektra/modules/macaddr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	ksRewind (returned);
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/macaddr");
		if (!meta) continue;
		const Key * origValue = keyGetMeta (cur, "origvalue");
		if (origValue)
		{
			ELEKTRA_SET_ERRORF (
				ELEKTRA_ERROR_STATE, parentKey,
				"Meta key 'origvalue' for key %s not expected to be set, another plugin has already set this meta key!",
				keyString (cur));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		int rc = validateMac (cur);
		if (rc)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "%s is not in a supported format.", keyString (cur));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (checkIntMac (keyString (cur)))
		{
			transformMac (cur);
			transformToInt (cur);
		}
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraMacaddrSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ksRewind (returned);
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/macaddr");
		if (!meta) continue;
		int rc = validateMac (cur);
		if (rc)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "%s is not in a supported format.", keyString (cur));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		const Key * origValue = keyGetMeta (cur, "origvalue");
		if (origValue)
		{
			keySetString (cur, keyString (origValue));
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
		keySetMeta (cur, "origvalue", keyString (cur));

		if (checkIntMac (keyString (cur)))
		{
			transformMac (cur);
		}

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("macaddr", ELEKTRA_PLUGIN_GET, &elektraMacaddrGet, ELEKTRA_PLUGIN_SET, &elektraMacaddrSet);
}
