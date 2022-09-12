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

static int validateEmail (ElektraKey * key, ElektraKey * parentKey)
{
	const ElektraKey * meta = elektraKeyGetMeta (key, "check/email");
	if (!meta) return 1;

	const char * addr = elektraKeyString (key);
	if (!addr) return 0;
	const char * regexString = // regex based on information from wikipedia and RFC5321 built in regex101.com
		"^[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]+([a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~.-][a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-])*"
		"[a-zA-Z0-9!#$%&'*+\\/=?^_`{|}~-]?"
		"@([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$";
	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);
	if (ret)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Failed to compile regex for email validation on %s", elektraKeyString (key));
		return 0;
	}

	ret = regexec (&regex, addr, 1, &offsets, 0);
	regfree (&regex);
	if (!ret)
	{
		return 1;
	}

	ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Value %s of key %s was not a valid email", elektraKeyString (key), elektraKeyName (key));
	return 0;
}

int elektraEmailGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/email"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/email", ELEKTRA_KEY_VALUE, "email plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/email/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/email/exports/get", ELEKTRA_KEY_FUNC, elektraEmailGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/email/exports/set", ELEKTRA_KEY_FUNC, elektraEmailSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/email/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraEmailSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		int rc = validateEmail (cur, parentKey);
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

