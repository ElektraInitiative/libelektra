/**
 * @file
 *
 * @brief Source for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdberrors.h>
#include <regex.h>
#include <stdio.h>

#include "email.h"

static int validateEmail (const char * addr)
{
	if (!addr) return 0;
	const char * regexString =
		"^[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]+([a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~.-][a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-])*"
		"[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]?"
		"@([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$";
	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (ret) return -1;
	ret = regexec (&regex, addr, 1, &offsets, 0);
	regfree (&regex);
	if (!ret)
	{
		char* addrCopy = strdup(addr);
		const char * localPart = strtok(addrCopy, "@");
		if (strlen(localPart) > 64)
		{
			return 0;
		}

		const char * domainPart = strtok(NULL, "@");
		if (strlen(domainPart) > 255)
		{
			return 0;
		}

		return 1;
	}
	return 0;
}

static int validateKey (Key * key, Key * parentKey)
{
	int rc = validateEmail (keyString (key));

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

int elektraEmailGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/email"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/email", KEY_VALUE, "email plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/email/exports", KEY_END),
			       keyNew ("system:/elektra/modules/email/exports/get", KEY_FUNC, elektraEmailGet, KEY_END),
			       keyNew ("system:/elektra/modules/email/exports/set", KEY_FUNC, elektraEmailSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/email/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraEmailSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		int rc = validateKey (cur, parentKey);
		if (!rc) return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("email",
		ELEKTRA_PLUGIN_GET,	&elektraEmailGet,
		ELEKTRA_PLUGIN_SET,	&elektraEmailSet,
		ELEKTRA_PLUGIN_END);
}

