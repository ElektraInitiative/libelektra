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
#include <strings.h>
#include <time.h>


static int individualIsoStringValidation (const char * date, const RepStruct * formats)
{
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	for (int i = 0; formats[i].rep != END; ++i)
	{
		char * ptr = strptime (date, formats[i].basic, &tm);
		if (ptr && !*ptr) return 1;
		ptr = strptime (date, formats[i].extended, &tm);
		if (ptr && !*ptr) return 1;
	}
	return 0;
}

static ISOType isoStrToToken (const char * fmtString)
{
	if (!strcasecmp (fmtString, "<date>"))
		return DATE;
	else if (!strcasecmp (fmtString, "<calendardate>"))
		return CALENDAR;
	else if (!strcasecmp (fmtString, "<ordinaldate>"))
		return ORDINAL;
	else if (!strcasecmp (fmtString, "<weekdate>"))
		return WEEK;
	else if (!strcasecmp (fmtString, "<time>"))
		return TIME;
	else if (!strcasecmp (fmtString, "<timeofday>"))
		return TIMEOFDAY;
	else if (!strcasecmp (fmtString, "<utc>"))
		return UTC;
	else if (!strcasecmp (fmtString, "<datetimecomplete>"))
		return DTCMP;
	else if (!strcasecmp (fmtString, "<datetimeother>"))
		return DTOTH;
	else
		return NA;
}

static const RepStruct * typeToTable (ISOType type)
{
	switch (type)
	{
	case CALENDAR:
		return iso8601calendardate;
		break;
	case ORDINAL:
		return iso8601ordinaldate;
		break;
	case WEEK:
		return iso8601weekdate;
		break;
	case TIMEOFDAY:
		return iso8601timeofday;
		break;
	case UTC:
		return iso8601UTC;
		break;
	default:
		return NA;
		break;
	}
}

static int combineAndValidateISO (const char * toValidate, const RepStruct * date, const RepStruct * time)
{
	ssize_t basicLen = strlen (date->basic) + strlen (time->basic) + 2;
	ssize_t extendedLen = 0;
	if (date->extended && time->extended) extendedLen = strlen (date->extended) + strlen (time->extended) + 2;
	char * buffer = elektraCalloc (basicLen);
	snprintf (buffer, basicLen, "%sT%s", date->basic, time->basic);
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	char * ptr = strptime (toValidate, buffer, &tm);
	elektraFree (buffer);
	if (ptr && !(*ptr)) return 1;
	if (!extendedLen) return -1;
	buffer = elektraMalloc (extendedLen);
	snprintf (buffer, extendedLen, "%sT%s", date->extended, time->extended);
	ptr = strptime (toValidate, buffer, &tm);
	elektraFree (buffer);
	if (ptr && !(*ptr)) return 1;
	return -1;
}

static int combinedIsoStringValidation (const char * toValidate, ISOType type)
{
	const CRepStruct * formats;
	switch (type)
	{
	case DTCMP:
		formats = iso8601CombinedComplete;
		break;
	case DTOTH:
		formats = iso8601CombinedOther;
		break;
	default:
		return -1;
	}
	for (int i = 0; formats[i].dateRep != END; ++i)
	{
		const CRepStruct * e = &formats[i];
		const REP dateRep = e->dateRep;
		const REP timeRep = e->timeRep;
		const RepStruct * date = typeToTable (e->date);
		const RepStruct * time = typeToTable (e->time);
		if (!date || !time) continue;
		for (int j = 0; date[j].rep != END; ++j)
		{
			if (date[j].rep != dateRep) continue;
			for (int k = 0; time[k].rep != END; ++k)
			{
				if (time[k].rep != timeRep) continue;
				int rc = combineAndValidateISO (toValidate, &(date[j]), &(time[k]));
				if (rc == 1) return 1;
			}
		}
	}
	return -1;
}

static int isoStringValidation (const char * date, const char * fmt)
{
	ISOType isoToken = NA;
	if (fmt)
	{
		isoToken = isoStrToToken (fmt);
		if (isoToken == NA) return 0;
		int rc = -1;
		switch (isoToken)
		{
		case CALENDAR:
			rc = individualIsoStringValidation (date, iso8601calendardate);
			break;
		case ORDINAL:
			rc = individualIsoStringValidation (date, iso8601ordinaldate);
			break;
		case WEEK:
			rc = individualIsoStringValidation (date, iso8601weekdate);
			break;
		case TIMEOFDAY:
			rc = individualIsoStringValidation (date, iso8601timeofday);
			break;
		case UTC:
			rc = individualIsoStringValidation (date, iso8601UTC);
			break;
		case DATE:
			rc = individualIsoStringValidation (date, iso8601calendardate);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601ordinaldate);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601weekdate);
			break;
		case TIME:
			rc = individualIsoStringValidation (date, iso8601timeofday);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601UTC);
			break;
		case DTCMP:
			rc = combinedIsoStringValidation (date, DTCMP);
			break;
		case DTOTH:
			rc = combinedIsoStringValidation (date, DTOTH);
			break;
		default:
			break;
		}
		return rc;
	}
	else
	{
		int rc = -1;
		rc = combinedIsoStringValidation (date, DTCMP);
		if (rc != 1) rc = combinedIsoStringValidation (date, DTOTH);
		return rc;
	}
	return -1;
}


static int formatStringValidation (const char * date, const char * fmt)
{
	if (!fmt) return 0;
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	char * ptr = strptime (date, fmt, &tm);
	if (!ptr)
		return -1;
	else if (ptr && !(*ptr))
		return 1;
	else
		return -1;
}

static int rfc2822StringValidation (const char * date, const char * fmt)
{
	if (!fmt)
	{
		return formatStringValidation (date, fmt);
	}
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	for (int i = 0; rfc2822strings[i] != NULL; ++i)
	{
		char * ptr = strptime (date, rfc2822strings[i], &tm);
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
	const Key * standard = keyGetMeta (key, "check/date");
	const Key * formatStringMeta = keyGetMeta (key, "check/date/format");
	const char * date = keyString (key);
	int rc = 0;
	const char * stdString = keyString (standard);
	const char * formatString = formatStringMeta ? keyString (formatStringMeta) : NULL;
	if (!strcasecmp (stdString, "POSIX"))
	{
		rc = formatStringValidation (date, formatString);
		if (rc == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match format string %s", date, formatString);
		}
	}
	else if (!strcasecmp (stdString, "ISO8601"))
	{
		rc = isoStringValidation (date, formatString);
		if (rc == -1)
		{
			if (formatString)
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match iso specification %s", date,
						    formatString);
			else
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s is not a valid ISO8601 date", date);
		}
		else if (rc == 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "syntax error in ISO8601 format string '%s'", formatString);
		}
	}
	else if (!strcasecmp (stdString, "RFC2822"))
	{
		rc = rfc2822StringValidation (date, formatString);
		if (rc == -1)
		{
			if (formatString)
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match rfc2822 format string '%s'", date,
						    formatString);
			else
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_D_T_FMT, parentKey, "%s doesn't match rfc2822 specification", date);
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

