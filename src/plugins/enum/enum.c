/**
 * @file
 *
 * @brief Source for enum plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "enum.h"
#include <kdberrors.h>
#include <kdbmeta.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VALIDATE_KEY_SUBMATCHES 3 // first submatch is the string we want, second submatch , or EOL

int elektraEnumGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/enum"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/enum", KEY_VALUE, "enum plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/enum/exports", KEY_END),
			       keyNew ("system/elektra/modules/enum/exports/get", KEY_FUNC, elektraEnumGet, KEY_END),
			       keyNew ("system/elektra/modules/enum/exports/set", KEY_FUNC, elektraEnumSet, KEY_END),
#include ELEKTRA_README (enum)
			       keyNew ("system/elektra/modules/enum/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

static int validateWithList (Key * key)
{
	const char * validValues = keyString (keyGetMeta (key, "check/enum"));
	const char * regexString = "'([^']*)'\\s*(,|$|([)]}])?)";
	regex_t regex;
	if (regcomp (&regex, regexString, REG_EXTENDED | REG_NEWLINE))
	{
		ELEKTRA_SET_ERROR (120, key, "regcomp failed");
		return -1;
	}
	const char * ptr = validValues;
	regmatch_t match[VALIDATE_KEY_SUBMATCHES];
	char * value = NULL;
	int nomatch;
	int start;
	int end;
	while (1)
	{
		nomatch = regexec (&regex, ptr, VALIDATE_KEY_SUBMATCHES, match, 0);
		if (nomatch) break;

		start = match[1].rm_so + (ptr - validValues);
		end = match[1].rm_eo + (ptr - validValues);
		elektraRealloc ((void **)&value, (end - start) + 1);
		strncpy (value, validValues + start, end - start);
		value[(end - start)] = '\0';
		if (strcmp (keyString (key), value) == 0)
		{
			regfree (&regex);
			elektraFree (value);
			return 1;
		}
		ptr += match[0].rm_eo;
	}
	if (value) elektraFree (value);
	regfree (&regex);
	return 0;
}

static int validateWithArray (Key * key)
{
	const Key * multiEnum = keyGetMeta (key, "check/enum/multi");
	char *delim = "\0";
	if(multiEnum)
	    delim = (char *)keyString(multiEnum);
	char * localString = elektraStrDup (keyString (key));
	KeySet * validValues = elektraMetaArrayToKS (key, "check/enum");
	char * value = strtok (localString, delim);
	short isValid;
	while (value)
	{
		Key * cur;
		isValid = 0;
		while ((cur = ksNext (validValues)) != NULL)
		{
			if (!strcmp (value, keyString (cur)))
			{
				isValid = 1;
				break;
			}
		}
		if (!isValid)
		{
			break;
		}
		value = strtok (NULL, delim);
	}
	elektraFree (localString);
	ksDel (validValues);
	if (!isValid) return 0;
	return 1;
}

static int validateKey (Key * key)
{
	const Key * meta = keyGetMeta (key, "check/enum");
	if (!meta) return 1;
	if (keyString (meta)[0] != '#')
		return validateWithList (key);
	else
		return validateWithArray (key);
}

int elektraEnumSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!validateKey (cur))
		{
			ELEKTRA_SET_ERRORF (121, parentKey, "Validation of %s failed.", keyName (cur));
			return -1;
		}
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (enum)
{
	// clang-format off
	return elektraPluginExport ("enum", 
			ELEKTRA_PLUGIN_GET, 	&elektraEnumGet,
			ELEKTRA_PLUGIN_SET, 	&elektraEnumSet,
			ELEKTRA_PLUGIN_END);
}
