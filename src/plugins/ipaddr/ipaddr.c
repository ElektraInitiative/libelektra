/**
 * @file
 *
 * @brief Source for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/kdb/errors.h>
#include <regex.h>
#include <stdio.h>

#include "ipaddr.h"

static int validateIPv4 (const char * addr)
{
	if (!addr) return 0;
	unsigned int a, b, c, d;
	a = b = c = d = 0;
	const char * regexString = "^([0-9]{1,3}\\.){3}([0-9]{1,3})$";
	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (ret) return -1;
	ret = regexec (&regex, addr, 1, &offsets, 0);
	regfree (&regex);
	if (!ret)
	{
		sscanf (addr, "%u.%u.%u.%u", &a, &b, &c, &d);
		if (a > 255 || b > 255 || c > 255 || d > 255)
		{
			return 0;
		}
		else
			return 1;
	}
	return 0;
}

static int validateIPv6 (const char * addr)
{
	if (!addr) return 0;
	const char * regexString =
		"(^((:(([0-9A-Fa-f]{0,4}):){1,6}(([0-9A-Fa-f]{1,4})))|(([0-9A-Fa-f]{1,4})(:([0-9A-Fa-f]{0,4})){1,7}))$)|(^((:(([0-9A-Fa-f]{"
		"0,4}):){1,4}(([0-9A-Fa-f]{1,4})))|(([0-9A-Fa-f]{1,4})(:([0-9A-Fa-f]{0,4})){1,5}))((([0-9]{1,3}\\.){3})([0-9]{1,3}))$)";
	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (ret) return -1;
	ret = regexec (&regex, addr, 1, &offsets, 0);
	regfree (&regex);
	if (ret)
		return 0;
	else
	{
		char * ptr = (char *) addr;
		int count = 0;
		while (*ptr)
		{
			if (*ptr == ':') ++count;
			++ptr;
		}
		if (count > 7)
		{
			return 0;
		}
		else if (count < 7)
		{
			if (!strstr (addr, "::")) return 0;
		}
		if (strchr (addr, '.'))
		{
			char * ipv4ptr = strrchr (addr, ':');
			++ipv4ptr;
			ret = validateIPv4 (ipv4ptr);
			if (!ret) return 0;
		}
		return 1;
	}
}

static int validateKey (Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta (key, "check/ipaddr");
	if (!meta) return 1;
	int rc = 0;

	if (!strcasecmp (keyString (meta), "ipv4"))
	{
		rc = validateIPv4 (keyString (key));
	}
	else if (!strcasecmp (keyString (meta), "ipv6"))
	{
		rc = validateIPv6 (keyString (key));
	}
	else
	{
		// By default we allow both type of addresses
		if (!(rc = validateIPv4 (keyString (key)))) rc = validateIPv6 (keyString (key));
	}

	if (!rc)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Validation of key %s with value %s failed", keyName (key),
							keyString (key));
	}
	else if (rc == -1)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		rc = 0;
	}

	return rc;
}

int elektraIpaddrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/ipaddr"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/ipaddr", KEY_VALUE, "ipaddr plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/ipaddr/exports", KEY_END),
			       keyNew ("system:/elektra/modules/ipaddr/exports/get", KEY_FUNC, elektraIpaddrGet, KEY_END),
			       keyNew ("system:/elektra/modules/ipaddr/exports/set", KEY_FUNC, elektraIpaddrSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/ipaddr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraIpaddrSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/ipaddr");
		if (!meta) continue;
		int rc = validateKey (cur, parentKey);
		if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("ipaddr",
		ELEKTRA_PLUGIN_GET,	&elektraIpaddrGet,
		ELEKTRA_PLUGIN_SET,	&elektraIpaddrSet,
		ELEKTRA_PLUGIN_END);
}

