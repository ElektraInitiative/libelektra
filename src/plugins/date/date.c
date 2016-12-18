/**
 * @file
 *
 * @brief Source for date plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#define _XOPEN_SOURCE
#include "date.h"
#include <ctype.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


static int stricmp (const char * s1, const char * s2)
{

	if (!(*s1) && !(*s2))
	{
		return 0;
	}
	else if (!(*s1))
	{
		return (-1);
	}
	else if (!(*s2))
	{
		return 1;
	}
	else
	{
		char * p1 = (char *)s1;
		char * p2 = (char *)s2;
		while (1)
		{
			if (!(*p1) && !(*p2))
			{
				return 0;
			}
			else if (!(*p1))
			{
				return (-1);
			}
			else if (!(*p2))
			{
				return 1;
			}
			else
			{
				if (toupper (*p1) == toupper (*p2))
				{
					++p1;
					++p2;
				}
				else
				{
					return (toupper (*p1) - toupper (*p2));
				}
			}
		}
	}
}

static int isoStringValidation (const char * date, const char * isoString)
{
	int hasDate = 0;
	int hasT = 0;
	int hasTime = 0;
	int hasTZ = 0;
	if (strstr (isoString, "<date>") || strstr (isoString, "<calendardate>") || strstr (isoString, "<weekdate>") ||
	    strstr (isoString, "<ordinaldate>"))
		hasDate = 1;
	if (strchr (isoString, 'T')) hasT = 1;
	if (strstr (isoString, "<time>")) hasTime = 1;
	if (strstr (isoString, "<tz>")) hasTZ = 1;
	if (!hasT && ((hasDate && hasTime) || (hasDate && hasTZ))) return 0;
	if (hasT && (!hasDate || !hasTime)) return 0;
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	if (hasDate)
	{
		const char ** fmtStrings;
		char * localCopy = strdup (isoString);
		char * tPtr = strchr (localCopy, 'T');
		if (tPtr) *tPtr = '\0';
		if (!stricmp (localCopy, "<date>"))
			fmtStrings = iso8601dates;
		else if (!stricmp (localCopy, "<calendardate>"))
			fmtStrings = iso8601calendardate;
		else if (!stricmp (localCopy, "<weekdate>"))
			fmtStrings = iso8601weekdate;
		else if (!stricmp (localCopy, "<ordinaldate>"))
			fmtStrings = iso8601ordinaldate;
		else
		{
			elektraFree (localCopy);
			return 0;
		}
		elektraFree (localCopy);
		int fail = 1;
		char * ptr = NULL;
		for (int i = 0; fmtStrings[i] != NULL; ++i)
		{
			ptr = strptime (date, fmtStrings[i], &tm);
			if (ptr)
			{
				fail = 0;
				break;
			}
		}
		if (!fail && *ptr == '\0' && !hasT) return 1;
		if (!fail && *ptr != '\0' && hasT)
		{
			if (*ptr == 'T')
				++ptr;
			else
				return -1;
			fail = 1;
			for (int i = 0; iso8601time[i] != NULL; ++i)
			{
				char * ptr2 = strptime (ptr, iso8601time[i], &tm);
				if (ptr2)
				{
					fail = 0;
					ptr = ptr2;
					break;
				}
			}
			if (fail)
				return -1;
			else
			{
				if (*ptr != '\0' && !hasTZ)
					return -1;
				else if (*ptr == '\0' && hasTZ)
					return -1;
				else if (*ptr == '\0' && !hasTZ)
					return 1;
				else if (*ptr != '\0' && hasTZ)
				{
					for (int i = 0; iso8601tzdesignator[i] != NULL; ++i)
					{
						char * ptr2 = strptime (ptr, iso8601tzdesignator[i], &tm);
						if (ptr2)
						{
							if (*ptr2 != '\0')
								return -1;
							else
								return 1;
						}
					}
					return -1;
				}
			}
		}
	}
	if (hasTime)
	{
		char * ptr = NULL;
		for (int i = 0; iso8601time[i] != NULL; ++i)
		{
			ptr = strptime (date, iso8601time[i], &tm);
			if (ptr)
			{
				if (*ptr == '\0' && !hasTZ)
					return 1;
				else if (*ptr == '\0' && hasTZ)
					return -1;
				else if (*ptr != '\0' && !hasTZ)
					return -1;
				else if (*ptr != '\0' && hasTZ)
				{
					for (int j = 0; iso8601tzdesignator[j] != NULL; ++j)
					{
						char * ptr2 = strptime (ptr, iso8601tzdesignator[j], &tm);
						if (ptr2)
						{
							if (*ptr2 == '\0') return 1;
						}
					}
					return -1;
				}
			}
		}
		return -1;
	}
	return -1;
}

static int formatStringValidation (const char * date, const char * fmt)
{
	struct tm tm;
	char * ptr = strptime (date, fmt, &tm);
	if (!ptr)
		return -1;
	else if (ptr && !(*ptr))
		return 1;
	else
		return -1;
}

static int rfc2822StringValidation (const char * date)
{
	struct tm tm;
	for (int i = 0; rfc2822Strings[i] != NULL; ++i)
	{
		char * ptr = strptime (date, rfc2822Strings[i], &tm);
		if (ptr)
		{
			if (*ptr == '\0')
				return 1;
			else
				return -1;
		}
	}
	return -1;
}

static int validateDate (Key * key, Key * parentKey)
{
	const Key * formatStringMeta = keyGetMeta (key, "check/date/format");
	const Key * isoMeta = keyGetMeta (key, "check/date/iso8601");
	const Key * rfc2822Meta = keyGetMeta (key, "check/date/rfc2822");
	const char * date = keyString (key);
	int rc = 0;
	if (formatStringMeta)
	{
		const char * formatString = keyString (formatStringMeta);
		rc = formatStringValidation (date, formatString);
		if (rc == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match format string %s", date, formatString);
		}
	}
	else if (isoMeta)
	{
		rc = isoStringValidation (date, keyString (isoMeta));
		if (rc == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match iso specification %s", date,
					    keyString (isoMeta));
		}
		else if (rc == 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "syntax error in %s", keyString (isoMeta));
		}
	}
	else if (rfc2822Meta)
	{
		rc = rfc2822StringValidation (date);
		if (rc == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match rfc2822 specification", date);
		}
	}
	else
	{
		setlocale (LC_TIME, "");
		const char * formatString = nl_langinfo (D_T_FMT);
		rc = formatStringValidation (date, formatString);
		if (rc == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match environment format string %s", date,
					    formatString);
		}
	}

	return rc;
}


int elektraDateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/date"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/date", KEY_VALUE, "date plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/date/exports", KEY_END),
			       keyNew ("system/elektra/modules/date/exports/get", KEY_FUNC, elektraDateGet, KEY_END),
			       keyNew ("system/elektra/modules/date/exports/set", KEY_FUNC, elektraDateSet, KEY_END),
#include ELEKTRA_README (date)
			       keyNew ("system/elektra/modules/date/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	Key * cur;
	int rc = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/date");
		if (meta)
		{
			int r = validateDate (cur, parentKey);
			if (r == -1)
			{
				rc = -1;
			}
		}
	}
	return rc; // success
}

int elektraDateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur;
	int rc = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/date");
		if (meta)
		{
			int r = validateDate (cur, parentKey);
			if (r == -1)
			{
				rc = -1;
			}
		}
	}
	return rc; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (date)
{
	// clang-format off
	return elektraPluginExport ("date",
		ELEKTRA_PLUGIN_GET,	&elektraDateGet,
		ELEKTRA_PLUGIN_SET,	&elektraDateSet,
		ELEKTRA_PLUGIN_END);
}

