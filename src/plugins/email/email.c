/**
 * @file
 *
 * @brief Source for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/kdb/errors.h>
#include <internal/utility/old_helper.h>
#include <regex.h>
#include <stdio.h>

#include "./email.h"

static int validateEmail (Key * key, Key * parentKey)
{
	const Key * meta = keyGetMeta (key, "check/email");
	if (!meta) return 1;

	const char * addr = keyString (key);
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
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Failed to compile regex for email validation on %s", keyString (key));
		return 0;
	}

	ret = regexec (&regex, addr, 1, &offsets, 0);
	regfree (&regex);
	if (!ret)
	{
		return 1;
	}

	ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Value %s of key %s was not a valid email", keyString (key), keyName (key));
	return 0;
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

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
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

