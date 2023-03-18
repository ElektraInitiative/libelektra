/**
 * @file
 *
 * @brief Source for date plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define _XOPEN_SOURCE
#include "date.h"
#include <ctype.h>
#include <elektra/kdb/errors.h>
#include <internal/utility/old_helper.h>
#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>


//
// use an ISO format string table to validate the key value
//

static int individualIsoStringValidation (const char * date, const RepStruct * formats, ISOType opts)
{
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	for (int i = 0; formats[i].rep != END; ++i)
	{
		if (formats[i].rep & (opts & REPMASK))
		{

			if (opts & BASIC)
			{
				char * ptr = strptime (date, formats[i].basic, &tm);
				if (ptr && !*ptr) return 1;
			}
			if ((opts & EXTD) && formats[i].extended)
			{
				char * ptr = strptime (date, formats[i].extended, &tm);
				if (ptr && !*ptr) return 1;
			}
		}
	}
	return -1;
}

//
// tokenize ISO format string
//

static ISOType ISOStrToToken (const char * fmtString)
{
	ISOType type = NA;
	if (!strncasecmp (fmtString, "datetime", sizeof ("datetime") - 1))
		type = DATETIME;
	else if (!strncasecmp (fmtString, "date", sizeof ("date") - 1))
		type = DATE;
	else if (!strncasecmp (fmtString, "calendardate", sizeof ("calendardate") - 1))
		type = CALENDAR;
	else if (!strncasecmp (fmtString, "ordinaldate", sizeof ("ordinaldate") - 1))
		type = ORDINAL;
	else if (!strncasecmp (fmtString, "weekdate", sizeof ("weekdate") - 1))
		type = WEEK;
	else if (!strncasecmp (fmtString, "timeofday", sizeof ("timeofday") - 1))
		type = TIMEOFDAY;
	else if (!strncasecmp (fmtString, "time", sizeof ("time") - 1))
		type = TIME;
	else if (!strncasecmp (fmtString, "utc", sizeof ("utc") - 1))
		type = UTC;

	if (type == NA) return type;

	const char * repPtr = strchr (fmtString, ' ');
	if (repPtr == NULL)
	{
		type |= ((CMPLT) | (RDCD) | (TRCT) | (BASIC) | (EXTD));
		return type;
	}
	else
	{
		++repPtr;
	}
	if (!strncasecmp (repPtr, "complete+reduced+truncated", sizeof ("complete+reduced+truncated") - 1))
		type |= ((CMPLT) | (RDCD) | (TRCT));
	else if (!strncasecmp (repPtr, "complete+reduced", sizeof ("complete+reduced") - 1))
		type |= ((CMPLT) | (RDCD));
	else if (!strncasecmp (repPtr, "reduced+truncated", sizeof ("reduced+truncated") - 1))
		type |= ((RDCD) | (TRCT));
	else if (!strncasecmp (repPtr, "complete", sizeof ("complete") - 1))
		type |= CMPLT;
	else if (!strncasecmp (repPtr, "reduced", sizeof ("reduced") - 1))
		type |= RDCD;
	else if (!strncasecmp (repPtr, "truncated", sizeof ("truncated") - 1))
		type |= TRCT;

	const char * repPtr2 = strchr (repPtr, ' ');
	if (!repPtr2 && (((type & REPMASK) & ~TYPEMASK) == 0))
	{
		type |= ((CMPLT) | (RDCD) | (TRCT));
		if (!strncasecmp (repPtr, "basic", sizeof ("basic") - 1))
			type |= BASIC;
		else if (!strncasecmp (repPtr, "extended", sizeof ("extended") - 1))
			type |= EXTD;
	}
	else if (!repPtr2)
	{
		type |= ((BASIC) | (EXTD));
	}
	else if (repPtr2)
	{
		if (!strncasecmp (repPtr2 + 1, "basic", sizeof ("basic") - 1))
			type |= BASIC;
		else if (!strncasecmp (repPtr2 + 1, "extended", sizeof ("extended") - 1))
			type |= EXTD;
	}
	repPtr = strrchr (fmtString, ' ');
	if (repPtr)
	{
		++repPtr;
		if (!strcasecmp (repPtr, "noT"))
		{
			if (((type & REPMASK) & ~TYPEMASK) == 0)
			{
				type |= ((CMPLT) | (RDCD) | (TRCT));
			}
			if ((type & ~REPMASK) == 0)
			{
				type |= ((BASIC) | (EXTD));
			}
			type |= OMITT;
		}
	}
	return type;
}

//
// return table matching ISOType
//

static const RepStruct * typeToTable (ISOType type)
{
	ISOType typeStripped = (type & TYPEMASK);
	switch (typeStripped)
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
		return NULL;
		break;
	}
}

static int countLeadingHyphen (const char * date)
{
	char * ptr = (char *) date;
	int count = 0;
	while (*ptr && ((*ptr == ' ') || (*ptr == '-')))
	{
		if (*ptr == '-') ++count;
		++ptr;
	}
	return count;
}

//
// create basic and extended date and time combination and
// try to validate the key value
//

static int combineAndValidateISO (const char * toValidate, const RepStruct * date, const RepStruct * time, ISOType opts)
{
	ssize_t basicLen = strlen (date->basic) + strlen (time->basic) + 2;
	ssize_t extendedLen = 0;
	if (date->extended && time->extended) extendedLen = strlen (date->extended) + strlen (time->extended) + 2;
	char * buffer = elektraCalloc (basicLen);
	unsigned short noT = 0;
	if (!strchr (toValidate, 'T')) noT = 1;

	// ISO 8601 5.4.2 Representations other than complete, rule b.
	// when truncation occurs in the date component of a combined date and time
	// expression, it is not necessary to replace the omitted higher order components
	// with the hypen [-];

	int toValidateHyphen = countLeadingHyphen (toValidate);
	int toDropHyphen = 0;
	if (toValidateHyphen == 0)
	{
		if (opts & CMPLT) toDropHyphen = countLeadingHyphen (date->basic);
	}
	if (opts & BASIC)
	{
		if (!noT)
			snprintf (buffer, basicLen, "%sT%s", (date->basic) + toDropHyphen, time->basic);
		else
			snprintf (buffer, basicLen, "%s%s", (date->basic) + toDropHyphen, time->basic);
		struct tm tm;
		memset (&tm, 0, sizeof (struct tm));
		char * ptr = strptime (toValidate, buffer, &tm);
		elektraFree (buffer);
		if (ptr && !(*ptr)) return 1;
	}
	if (opts & EXTD)
	{
		if (!extendedLen) return -1;
		buffer = elektraMalloc (extendedLen);
		if (toValidateHyphen == 0)
			toDropHyphen = countLeadingHyphen (date->extended);
		else
			toDropHyphen = 0;
		if (!noT)
			snprintf (buffer, extendedLen, "%sT%s", (date->extended) + toDropHyphen, time->extended);
		else
			snprintf (buffer, extendedLen, "%s%s", (date->extended) + toDropHyphen, time->extended);
		struct tm tm;
		memset (&tm, 0, sizeof (struct tm));
		char * ptr = strptime (toValidate, buffer, &tm);
		elektraFree (buffer);
		if (ptr && !(*ptr)) return 1;
	}
	return -1;
}

//
// loop through iso8601 table containing rules on valid combinations
// and pass them to combineAndValidateISO
//

static int combinedIsoStringValidation (const char * toValidate, ISOType opts)
{
	const CRepStruct * formats;
	ISOType strippedOpts = ((opts & REPMASK) & ~OMITT);
	switch (strippedOpts)
	{
	case CMPLT:
		formats = iso8601CombinedComplete;
		break;
	case TRCT:
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
		if (!(opts & dateRep))
		{
			continue;
		}
		const RepStruct * date = typeToTable (e->date);
		const RepStruct * time = typeToTable (e->time);
		if (!date || !time) continue;
		for (int j = 0; date[j].rep != END; ++j)
		{
			if (date[j].rep != dateRep) continue;
			for (int k = 0; time[k].rep != END; ++k)
			{
				if (time[k].rep != timeRep) continue;
				int rc = combineAndValidateISO (toValidate, &(date[j]), &(time[k]), opts);
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
		isoToken = ISOStrToToken (fmt);
		ISOType strippedToken = (isoToken & TYPEMASK);
		ISOType strippedOpts = (isoToken & ~TYPEMASK);
		if (strippedToken == NA) return 0;
		int rc = -1;
		switch (strippedToken)
		{
		case CALENDAR:
			rc = individualIsoStringValidation (date, iso8601calendardate, strippedOpts);
			break;
		case ORDINAL:
			rc = individualIsoStringValidation (date, iso8601ordinaldate, strippedOpts);
			break;
		case WEEK:
			rc = individualIsoStringValidation (date, iso8601weekdate, strippedOpts);
			break;
		case TIMEOFDAY:
			rc = individualIsoStringValidation (date, iso8601timeofday, strippedOpts);
			break;
		case UTC:
			rc = individualIsoStringValidation (date, iso8601UTC, strippedOpts);
			break;
		case DATE:
			rc = individualIsoStringValidation (date, iso8601calendardate, strippedOpts);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601ordinaldate, strippedOpts);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601weekdate, strippedOpts);
			break;
		case TIME:
			rc = individualIsoStringValidation (date, iso8601timeofday, strippedOpts);
			if (rc == 1) break;
			rc = individualIsoStringValidation (date, iso8601UTC, strippedOpts);
			break;
		case DATETIME:
			if (!strchr (date, 'T'))
			{
				if (!(strippedOpts & OMITT)) return -1;
			}
			rc = combinedIsoStringValidation (date, strippedOpts);
			break;
		default:
			break;
		}
		return rc;
	}
	else
	{
		int rc = combinedIsoStringValidation (date, (DATETIME | CMPLT));
		if (rc != 1) rc = combinedIsoStringValidation (date, (DATETIME | TRCT));
		return rc;
	}
	return -1;
}


//
// validate key value using POSIX (strptime) format string
//

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

//
// validate key value using supplied RFC2822 format string
// or all possible format strings derived from the specification
//

static int rfc2822StringValidation (const char * date)
{
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	for (int i = 0; rfc2822strings[i] != NULL; ++i)
	{
		char * ptr = strptime (date, rfc2822strings[i], &tm);
		if (ptr)
		{
			if (*ptr == '\0') return 1;
		}
	}
	return -1;
}

static int rfc822StringValidation (const char * date)
{
	struct tm tm;
	memset (&tm, 0, sizeof (struct tm));
	for (int i = 0; rfc822strings[i] != NULL; ++i)
	{
		char * ptr = strptime (date, rfc822strings[i], &tm);
		if (ptr)
		{
			if (*ptr == '\0') return 1;
		}
	}
	return -1;
}

static int validateKey (Key * key, Key * parentKey)
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
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Date '%s' doesn't match format string %s", date, formatString);
			rc = 0;
		}
	}
	else if (!strcasecmp (stdString, "ISO8601"))
	{
		rc = isoStringValidation (date, formatString);
		if (rc == -1)
		{
			if (formatString)
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Date '%s' doesn't match iso specification %s", date,
									 formatString);
			else
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Date '%s' is not a valid ISO8601 date", date);
			rc = 0;
		}
		else if (rc == 0)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Syntax error in ISO8601 format string '%s'", formatString);
		}
	}
	else if (!strcasecmp (stdString, "RFC2822"))
	{
		rc = rfc2822StringValidation (date);
		if (rc == -1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Date '%s' doesn't match rfc2822 specification", date);
			rc = 0;
		}
	}
	else if (!strcasecmp (stdString, "RFC822"))
	{
		rc = rfc822StringValidation (date);
		if (rc == -1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Date '%s' doesn't match format string %s", date, formatString);
			rc = 0;
		}
	}

	return rc;
}


int elektraDateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/date"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/date", KEY_VALUE, "date plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/date/exports", KEY_END),
			       keyNew ("system:/elektra/modules/date/exports/get", KEY_FUNC, elektraDateGet, KEY_END),
			       keyNew ("system:/elektra/modules/date/exports/set", KEY_FUNC, elektraDateSet, KEY_END),
			       keyNew ("system:/elektra/modules/date/exports/validateKey", KEY_FUNC, validateKey, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/date/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	Key * cur;
	int rc = 1;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/date");
		if (meta)
		{
			int r = validateKey (cur, parentKey);
			if (r == 0)
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

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * meta = keyGetMeta (cur, "check/date");
		if (meta)
		{
			int r = validateKey (cur, parentKey);
			if (r == 0)
			{
				rc = -1;
			}
		}
	}
	return rc; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("date",
		ELEKTRA_PLUGIN_GET,	&elektraDateGet,
		ELEKTRA_PLUGIN_SET,	&elektraDateSet,
		ELEKTRA_PLUGIN_END);
}

