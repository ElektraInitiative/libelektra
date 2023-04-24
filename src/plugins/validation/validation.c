/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./validation.h"

static int validateKey (Key *, Key *);

int elektraValidationGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30,
			     keyNew ("system:/elektra/modules/validation", KEY_VALUE, "validation plugin waits for your orders", KEY_END),
			     keyNew ("system:/elektra/modules/validation/exports", KEY_END),
			     keyNew ("system:/elektra/modules/validation/exports/get", KEY_FUNC, elektraValidationGet, KEY_END),
			     keyNew ("system:/elektra/modules/validation/exports/set", KEY_FUNC, elektraValidationSet, KEY_END),
			     keyNew ("system:/elektra/modules/validation/exports/ksLookupRE", KEY_FUNC, ksLookupRE, KEY_END),
			     keyNew ("system:/elektra/modules/validation/exports/validateKey", KEY_FUNC, validateKey, KEY_END),
#include "./readme_validation.c"
			     keyNew ("system:/elektra/modules/validation/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);
	return 1;
}

static int validateKey (Key * key, Key * parentKey)
{
	const Key * regexMeta = keyGetMeta (key, "check/validation");

	const Key * icaseMeta = keyGetMeta (key, "check/validation/ignorecase");
	const Key * matchMeta = keyGetMeta (key, "check/validation/match");
	const Key * invertMeta = keyGetMeta (key, "check/validation/invert");
	const Key * typeMeta = keyGetMeta (key, "check/validation/type");

	int lineValidation = 0;
	int wordValidation = 0;
	int icaseValidation = 0;
	int invertValidation = 0;

	if (icaseMeta) icaseValidation = 1;
	if (invertMeta) invertValidation = 1;
	if (matchMeta)
	{
		char * matchCopy = elektraStrDup (keyString (matchMeta));
		char * ptr = matchCopy;
		while (*ptr)
		{
			*ptr = toupper (*ptr);
			++ptr;
		}
		if (!strcmp (matchCopy, "LINE")) lineValidation = 1;
		if (!strcmp (matchCopy, "WORD")) wordValidation = 1;
		if (!strcmp (matchCopy, "ANY"))
		{
			lineValidation = 0;
			wordValidation = 0;
		}
		elektraFree (matchCopy);
	}

	int cflags = REG_NOSUB | REG_EXTENDED;
	if (icaseValidation) cflags |= REG_ICASE;
	if (lineValidation) cflags |= REG_NEWLINE;
	if (typeMeta)
	{
		char * typeCopy = elektraStrDup (keyString (typeMeta));
		char * ptr = typeCopy;
		while (*ptr)
		{
			*ptr = toupper (*ptr);
			++ptr;
		}
		if (!strcmp (typeCopy, "ERE"))
			cflags |= REG_EXTENDED;
		else if (!strcmp (typeCopy, "BRE"))
			cflags &= REG_EXTENDED;
		elektraFree (typeCopy);
	}

	char * regexString = NULL;
	int freeString = 0;
	if (lineValidation || wordValidation)
	{
		regexString = elektraMalloc (keyGetValueSize (regexMeta) + 2);
		freeString = 1;
		sprintf (regexString, "^%s$", keyString (regexMeta));
	}
	else
	{
		regexString = (char *) keyString (regexMeta);
	}

	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, cflags);

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not compile regex '%s' of the key '%s'. Reason: %s",
							 keyString (regexMeta), keyName (key), buffer);
		regfree (&regex);
		if (freeString) elektraFree (regexString);
		return 0;
	}
	int match = 0;
	if (!wordValidation)
	{
		ret = regexec (&regex, keyString (key), 1, &offsets, 0);
		if (ret == 0) match = 1;
	}
	else
	{
		char * savePtr;
		char * token;
		char * string = (char *) keyString (key);
		while ((token = strtok_r (string, " \t\n", &savePtr)) != NULL)
		{
			ret = regexec (&regex, token, 1, &offsets, 0);
			if (ret == 0)
			{
				match = 1;
				break;
			}
			string = NULL;
		}
	}
	if (invertValidation) match = !match;

	if (!match)
	{
		const Key * msg = keyGetMeta (key, "check/validation/message");
		if (msg)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
								 "The key '%s' with value '%s' does not confirm to '%s'. Reason: %s",
								 keyName (key), keyString (key), regexString, keyString (msg));
			regfree (&regex);
			if (freeString) elektraFree (regexString);
			return 0;
		}
		else
		{
			char buffer[1000];
			regerror (ret, &regex, buffer, 999);
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
								 "The key '%s' with value '%s' does not confirm to '%s'. Reason: %s",
								 keyName (key), keyString (key), regexString, buffer);
			regfree (&regex);
			if (freeString) elektraFree (regexString);
			return 0;
		}
	}

	regfree (&regex);
	if (freeString) elektraFree (regexString);
	return 1;
}

int elektraValidationSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * regexMeta = keyGetMeta (cur, "check/validation");

		if (!regexMeta) continue;
		int rc = validateKey (cur, parentKey);
		if (!rc) return -1;
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("validation",
			ELEKTRA_PLUGIN_GET,	&elektraValidationGet,
			ELEKTRA_PLUGIN_SET,	&elektraValidationSet,
			ELEKTRA_PLUGIN_END);
}

