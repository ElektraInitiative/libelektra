/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "validation.h"

static int validateKey (ElektraKey *, ElektraKey *);

int elektraValidationGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * n;
	elektraKeysetAppend (returned,
		  n = elektraKeysetNew (30,
			     elektraKeyNew ("system:/elektra/modules/validation", ELEKTRA_KEY_VALUE, "validation plugin waits for your orders", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/validation/exports", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/validation/exports/get", ELEKTRA_KEY_FUNC, elektraValidationGet, ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/validation/exports/set", ELEKTRA_KEY_FUNC, elektraValidationSet, ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/validation/exports/ksLookupRE", ELEKTRA_KEY_FUNC, ksLookupRE, ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/validation/exports/validateKey", ELEKTRA_KEY_FUNC, validateKey, ELEKTRA_KEY_END),
#include "readme_validation.c"
			     elektraKeyNew ("system:/elektra/modules/validation/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
	elektraKeysetDel (n);
	return 1;
}

static int validateKey (ElektraKey * key, ElektraKey * parentKey)
{
	const ElektraKey * regexMeta = elektraKeyGetMeta (key, "check/validation");

	const ElektraKey * icaseMeta = elektraKeyGetMeta (key, "check/validation/ignorecase");
	const ElektraKey * matchMeta = elektraKeyGetMeta (key, "check/validation/match");
	const ElektraKey * invertMeta = elektraKeyGetMeta (key, "check/validation/invert");
	const ElektraKey * typeMeta = elektraKeyGetMeta (key, "check/validation/type");

	int lineValidation = 0;
	int wordValidation = 0;
	int icaseValidation = 0;
	int invertValidation = 0;

	if (icaseMeta) icaseValidation = 1;
	if (invertMeta) invertValidation = 1;
	if (matchMeta)
	{
		char * matchCopy = elektraStrDup (elektraKeyString (matchMeta));
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
		char * typeCopy = elektraStrDup (elektraKeyString (typeMeta));
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
		regexString = elektraMalloc (elektraKeyGetValueSize (regexMeta) + 2);
		freeString = 1;
		sprintf (regexString, "^%s$", elektraKeyString (regexMeta));
	}
	else
	{
		regexString = (char *) elektraKeyString (regexMeta);
	}

	regex_t regex;
	regmatch_t offsets;
	int ret = regcomp (&regex, regexString, cflags);

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not compile regex '%s' of the key '%s'. Reason: %s",
							 elektraKeyString (regexMeta), elektraKeyName (key), buffer);
		regfree (&regex);
		if (freeString) elektraFree (regexString);
		return 0;
	}
	int match = 0;
	if (!wordValidation)
	{
		ret = regexec (&regex, elektraKeyString (key), 1, &offsets, 0);
		if (ret == 0) match = 1;
	}
	else
	{
		char * savePtr;
		char * token;
		char * string = (char *) elektraKeyString (key);
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
		const ElektraKey * msg = elektraKeyGetMeta (key, "check/validation/message");
		if (msg)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
								 "The key '%s' with value '%s' does not confirm to '%s'. Reason: %s",
								 elektraKeyName (key), elektraKeyString (key), regexString, elektraKeyString (msg));
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
								 elektraKeyName (key), elektraKeyString (key), regexString, buffer);
			regfree (&regex);
			if (freeString) elektraFree (regexString);
			return 0;
		}
	}

	regfree (&regex);
	if (freeString) elektraFree (regexString);
	return 1;
}

int elektraValidationSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur = 0;

	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		const ElektraKey * regexMeta = elektraKeyGetMeta (cur, "check/validation");

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

