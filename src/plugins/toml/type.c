/**
 * @file type.c
 *
 * @brief Used for validating types.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "./type.h"

#include <internal/utility/assert.h>
#include <internal/utility/old_helper.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#define SEPARATED_DIGITS(e) "(" e "(_?" e ")*)"
#define SEPARATED_DIGITS_DIFF_START(a, b) "(" a "(_?" b ")*)"
#define SEP_NUM_NZERO_START SEPARATED_DIGITS_DIFF_START ("[1-9]", "[0-9]")
#define DIG2 "[0-9]{2,2}"
#define DIG4 "[0-9]{4,4}"
#define AFTER_DOT "\\." SEPARATED_DIGITS ("[0-9]")

#define FULL_DATE "(" DIG4 "-" DIG2 "-" DIG2 ")"
#define PARTIAL_TIME "(" DIG2 ":" DIG2 ":" DIG2 "(\\.[0-9]+)?)"
#define TIME_OFFSET "(Z|([+-]" DIG2 ":" DIG2 "))"
#define TIME_SEPARATOR "[T ]"

const char * const decStr = "^[+-]?" SEP_NUM_NZERO_START "$";
const char * const floatStr = "^[+-]?(0|" SEP_NUM_NZERO_START
			      ")" // PRE-DOT DIGITS
			      "((" AFTER_DOT
			      ")|" // NEED EITHER DOT AND NUMBERS
			      "("
			      "(" AFTER_DOT ")?[eE][+-]?" SEPARATED_DIGITS ("[0-9]") "))$"; // OR DOT+NUMBERS+EXPONENT+NUMBERS
const char * const floatSpecialStr = "^[+-]?(nan|inf)$";
// const char * bareStr = "^[a-zA-Z0-9_-]+$";


const char * const offsetDateTimeStr = "^" FULL_DATE TIME_SEPARATOR PARTIAL_TIME TIME_OFFSET "$";
const char * const localDateTimeStr = "^" FULL_DATE TIME_SEPARATOR PARTIAL_TIME "$";
const char * const localDateStr = "^" FULL_DATE "$";
const char * const localTimeStr = "^" PARTIAL_TIME "$";

static bool validFullDateValues (const char * fullDate);
static bool validFullTimeValues (const char * fullTime);
static bool validPartialTimeValues (const char * partialTime);
static bool validTimeOffsetValues (const char * timeOffset);
static bool validTimeValues (int hour, int minute, int second);
static bool validDateValues (int year, int month, int day);
static bool isLeapYear (int year);

TypeChecker * createTypeChecker (void)
{
	int result ELEKTRA_UNUSED = 0;
	TypeChecker * typeChecker = (TypeChecker *) elektraCalloc (sizeof (TypeChecker));
	if (typeChecker == NULL)
	{
		return NULL;
	}
	result |= regcomp (&typeChecker->regexFloat, floatStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Float regex could not be compiled: '%s'", floatStr);
	result |= regcomp (&typeChecker->regexFloatSpecial, floatSpecialStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Special Floats regex could not be compiled: '%s'", floatSpecialStr);

	result |= regcomp (&typeChecker->regexOffsetDt, offsetDateTimeStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Offset datetime regex could not be compiled: '%s'", offsetDateTimeStr);
	result |= regcomp (&typeChecker->regexLocalDt, localDateTimeStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Local datetime regex could not be compiled: '%s'", localDateTimeStr);
	result |= regcomp (&typeChecker->regexLocalDate, localDateStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Local date regex could not be compiled: '%s'", localDateStr);
	result |= regcomp (&typeChecker->regexLocalTime, localTimeStr, REG_EXTENDED);
	ELEKTRA_ASSERT (result == 0, "Local time regex could not be compiled: '%s'", localTimeStr);


	return typeChecker;
}

void destroyTypeChecker (TypeChecker * checker)
{
	if (checker != NULL)
	{
		regfree (&checker->regexFloat);
		regfree (&checker->regexFloatSpecial);
		regfree (&checker->regexOffsetDt);
		regfree (&checker->regexLocalDt);
		regfree (&checker->regexLocalDate);
		regfree (&checker->regexLocalTime);
		elektraFree (checker);
	}
}

bool isFloat (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexFloat, str, 0, NULL, 0) == 0 || regexec (&checker->regexFloatSpecial, str, 0, NULL, 0) == 0;
}

/* bool isBareString (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexBare, str, 0, NULL, 0) == 0;
}*/

bool isDateTime (TypeChecker * checker, const char * str)
{
	return isOffsetDatetime (checker, str) || isLocalDateTime (checker, str) || isLocalDate (checker, str) ||
	       isLocalTime (checker, str);
}

bool isOffsetDatetime (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexOffsetDt, str, 0, NULL, 0) == 0 && validOffsetDateTimeValues (str);
}

bool isLocalDateTime (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexLocalDt, str, 0, NULL, 0) == 0 && validLocalDateTimeValues (str);
}

bool isLocalDate (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexLocalDate, str, 0, NULL, 0) == 0 && validLocalDateValues (str);
}

bool isLocalTime (TypeChecker * checker, const char * str)
{
	return regexec (&checker->regexLocalTime, str, 0, NULL, 0) == 0 && validLocalTimeValues (str);
}

bool validOffsetDateTimeValues (const char * str)
{
	const char * time = strpbrk (str, "T ");
	ELEKTRA_ASSERT (time != NULL, "Supplied offset datetime str was not valid, should have been checked before. Str = '%s'", str);
	return validFullDateValues (str) && validFullTimeValues (time + 1);
}

bool validLocalDateTimeValues (const char * str)
{
	const char * time = strpbrk (str, "T ");
	ELEKTRA_ASSERT (time != NULL, "Supplied local datetime str was not valid, should have been checked before. Str = '%s'", str);
	return validFullDateValues (str) && validPartialTimeValues (time + 1);
}

bool validLocalDateValues (const char * str)
{
	return validFullDateValues (str);
}

bool validLocalTimeValues (const char * str)
{
	return validPartialTimeValues (str);
}

static bool validFullDateValues (const char * fullDate)
{
	int y, m, d;
	sscanf (fullDate, "%4d-%2d-%2d", &y, &m, &d);
	return validDateValues (y, m, d);
}

static bool validFullTimeValues (const char * fullTime)
{
	const char * offset = strpbrk (fullTime, "Z+-");
	ELEKTRA_ASSERT (offset != NULL, "Supplied fulltime str was not valid, should have been checked before. Str = '%s'", fullTime);
	return validPartialTimeValues (fullTime) && validTimeOffsetValues (offset);
}

static bool validPartialTimeValues (const char * partialTime)
{
	int h, m, s;
	sscanf (partialTime, "%2d:%2d:%2d", &h, &m, &s);
	return validTimeValues (h, m, s);
}

static bool validTimeOffsetValues (const char * timeOffset)
{
	if (timeOffset[0] == 'Z')
	{
		return true;
	}
	else
	{
		int h, m;
		sscanf (timeOffset + 1, "%2d:%2d", &h, &m);
		return validTimeValues (h, m, 0);
	}
}

static bool validTimeValues (int hour, int minute, int second)
{
	return hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0 &&
	       second <= 60; // 60 to allow positive leap seconds, (also ignores negative leap seconds)
}

static bool validDateValues (int year, int month, int day)
{
	if (year > 9999 || month < 1 || month > 12)
	{
		return false;
	}
	if (day >= 1)
	{
		if (day <= 28)
		{
			return true;
		}
		else
		{
			switch (month)
			{
			case 1:
			case 3:
			case 5:
			case 7:
			case 8:
			case 10:
			case 12:
				return day <= 31;
			case 4:
			case 6:
			case 9:
			case 11:
				return day <= 30;
			case 2:
				return day <= (isLeapYear (year) ? 29 : 28);
			default:
				ELEKTRA_ASSERT (0, "Invalid month: %d, should have been checked before.", month);
			}
		}
	}
	return false;
}

static bool isLeapYear (int year)
{
	if (year % 4 == 0)
	{
		if (year % 100 == 0)
		{
			if (year % 400 == 0)
			{
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return true;
		}
	}
	return false;
}
