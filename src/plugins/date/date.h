/**
 * @file
 *
 * @brief Header for date plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DATE_H
#define ELEKTRA_PLUGIN_DATE_H

#include <elektra/kdbplugin.h>


int elektraDateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDateSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;


/*
 *
 * RFC822
 *
 */

const char * rfc822strings[] = { "%a, %d %b %y %T %z", "%d %b %y %T %z", "%a, %d %b %y %H:%M %z", "%d %b %y %H:%M %z", NULL };


/*
 *
 * RFC2822 format strings derived from the specification
 *
 */

const char * rfc2822strings[] = { "%a, %d %b %Y %T %z", "%d %b %Y %T %z", "%a, %d %b %Y %H:%M %z", "%d %b %Y %H:%M %z", NULL };


/*
 *
 * ISO8601 DATA
 *
 */

typedef enum
{
	END = 0,
	COMPLETE = (1 << 4),
	REDUCED = (2 << 4),
	TRUNCATED = (4 << 4),
} REP;

typedef struct
{
	REP rep;
	const char * basic;
	const char * extended;
} RepStruct;

const RepStruct iso8601calendardate[] = {
	{ .rep = COMPLETE, .basic = "%Y%m%d", .extended = "%F" },
	{ .rep = REDUCED, .basic = "%Y-%m", .extended = NULL },
	{ .rep = REDUCED, .basic = "%Y", .extended = NULL },
	{ .rep = REDUCED, .basic = "%C", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "%y%m%d", .extended = "%y-%m-%d" },
	{ .rep = TRUNCATED, .basic = "-%y%m", .extended = "-%y-%m" },
	{ .rep = TRUNCATED, .basic = "-%y", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "--%m%d", .extended = "--%m-%d" },
	{ .rep = TRUNCATED, .basic = "--%m", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "---%d", .extended = NULL },
	{ .rep = END, .basic = NULL, .extended = NULL },
};

const RepStruct iso8601ordinaldate[] = {
	{ .rep = COMPLETE, .basic = "%Y%j", .extended = "%Y-%j" },
	{ .rep = TRUNCATED, .basic = "%y%j", .extended = "%y-%j" },
	{ .rep = TRUNCATED, .basic = "-%j", .extended = NULL },
	{ .rep = END, .basic = NULL, .extended = NULL },
};

const RepStruct iso8601weekdate[] = {
	{ .rep = COMPLETE, .basic = "%GW%V%u", .extended = "%G-W%V-%u" },
	{ .rep = REDUCED, .basic = "%GW%V", .extended = "%G-W%V" },
	{ .rep = TRUNCATED, .basic = "%gW%V%u", .extended = "%g-W%V-%u" },
	{ .rep = TRUNCATED, .basic = "%gW%V", .extended = "%g-W%V" },
	{ .rep = TRUNCATED, .basic = "-%1CW%V%u", .extended = "-%1C-W%V-%u" },
	{ .rep = TRUNCATED, .basic = "-W%V%u", .extended = "-%W%V-%u" },
	{ .rep = TRUNCATED, .basic = "-W%V", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "-W-%u", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "---%u", .extended = NULL },
	{ .rep = END, .basic = NULL, .extended = NULL },
};

const RepStruct iso8601timeofday[] = {
	{ .rep = COMPLETE, .basic = "%H%M%S", .extended = "%T" },     { .rep = COMPLETE, .basic = "%H%M%S,%Y", .extended = "%T,%Y" },
	{ .rep = REDUCED, .basic = "%H%M", .extended = "%R" },	      { .rep = REDUCED, .basic = "%H", .extended = NULL },
	{ .rep = REDUCED, .basic = "%H%M,%Y", .extended = "%R,%Y" },  { .rep = REDUCED, .basic = "%H,%Y", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "-%M%S", .extended = "-%M:%S" }, { .rep = TRUNCATED, .basic = "-%M", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "--%S", .extended = NULL },      { .rep = TRUNCATED, .basic = "%H,%Y", .extended = NULL },
	{ .rep = TRUNCATED, .basic = "-%M,%Y", .extended = NULL },    { .rep = TRUNCATED, .basic = "-%M%S,%Y", .extended = "-%M:%S,%Y" },
	{ .rep = TRUNCATED, .basic = "-%S,%Y", .extended = NULL },    { .rep = END, .basic = NULL, .extended = NULL },
};

const RepStruct iso8601UTC[] = {
	{ .rep = COMPLETE, .basic = "%H%M%S%Z", .extended = "%T%Z" },
	{ .rep = COMPLETE, .basic = "%H%M%S%z", .extended = "%T%z" },
	{ .rep = REDUCED, .basic = "%H%M%Z", .extended = "%R%Z" },
	{ .rep = REDUCED, .basic = "%H%M%z", .extended = "%R%z" },
	{ .rep = REDUCED, .basic = "%H%Z", .extended = NULL },
	{ .rep = REDUCED, .basic = "%H%z", .extended = NULL },
	{ .rep = END, .basic = NULL, .extended = NULL },
};

typedef enum
{
	NA = 0,
	CALENDAR = 1,
	ORDINAL = 2,
	WEEK = 3,
	TIMEOFDAY = 4,
	UTC = 5,
	DATE = 6,
	TIME = 7,
	DATETIME = 8,			   // Date/Time combined
	TYPEMASK = 8 | 4 | 2 | 1,	   // mask to split type from representation options
	CMPLT = (1 << 4),		   // complete
	RDCD = (2 << 4),		   // reduced
	TRCT = (4 << 4),		   // truncated
	REPMASK = 64 | 32 | 16 | TYPEMASK, // split representation options
	BASIC = (1 << 7),		   // basic representation
	EXTD = (2 << 7),		   // extended representation
	OMITT = (4 << 7),
} ISOType;


typedef struct
{
	REP dateRep;
	ISOType date;
	REP timeRep;
	ISOType time;
} CRepStruct; // Combined

// Complete representation
// either both date + time basic or both extended
// date/time separator: 'T'

const CRepStruct iso8601CombinedComplete[] = {
	{ .dateRep = COMPLETE, .date = CALENDAR, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = CALENDAR, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = COMPLETE, .date = ORDINAL, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = ORDINAL, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = COMPLETE, .date = WEEK, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = WEEK, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = COMPLETE, .date = CALENDAR, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = CALENDAR, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = COMPLETE, .date = ORDINAL, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = ORDINAL, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = COMPLETE, .date = WEEK, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = COMPLETE, .date = WEEK, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = END, .date = NA, .timeRep = END, .time = NA },
};

// representations other than complete
// date truncated, time complete or reduced
// TODO: rule c.

const CRepStruct iso8601CombinedOther[] = {
	{ .dateRep = TRUNCATED, .date = CALENDAR, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = CALENDAR, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = TRUNCATED, .date = ORDINAL, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = ORDINAL, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = TRUNCATED, .date = WEEK, .timeRep = COMPLETE, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = WEEK, .timeRep = COMPLETE, .time = UTC },
	{ .dateRep = TRUNCATED, .date = CALENDAR, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = CALENDAR, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = TRUNCATED, .date = ORDINAL, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = ORDINAL, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = TRUNCATED, .date = WEEK, .timeRep = REDUCED, .time = TIMEOFDAY },
	{ .dateRep = TRUNCATED, .date = WEEK, .timeRep = REDUCED, .time = UTC },
	{ .dateRep = END, .date = NA, .timeRep = END, .time = NA },
};


#endif
