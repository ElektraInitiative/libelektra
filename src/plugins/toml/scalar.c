#include "scalar.h"

static bool isValidOffsetDateTime (const char * str);
static bool isValidLocalDateTime (const char * str);
static bool isValidLocalDate (const char * str);
static bool isValidLocalTime (const char * str);
static bool isValidFullDate (const char * fullDate);
static bool isValidDate (int year, int month, int day);
static bool isLeapYear (int year);
static bool isValidFullTime (const char * fullTime);
static bool isValidPartialTime (const char * partialTime);
static bool isValidTimeOffset (const char * timeOffset);
static bool isValidTime (int hour, int minute, int second);

Scalar * createScalar (ScalarType type, char * scalarString, size_t line)
{
	Scalar * scalar = malloc (sizeof (Scalar));
	scalar->type = type;
	scalar->str = scalarString;
	scalar->line = line;
	return scalar;
}

Scalar * createScalarDup (ScalarType type, const char * scalarString, size_t line)
{
	Scalar * scalar = malloc (sizeof (Scalar));
	scalar->type = type;
	if (scalarString != NULL)
	{
		scalar->str = strdup (scalarString);
	}
	else
	{
		scalar->str = NULL;
	}
	scalar->line = line;
	return scalar;
}


bool isValidBareString (const Scalar * scalar)
{
	// [a-zA-Z0-9-_]
	for (const char * c = scalar->str; c != 0; c++)
	{
		if (!((*c >= 'A' && *c <= 'Z') || (*c >= 'a' && *c <= 'z') || (*c >= '0' && *c <= '9') || *c == '_' || *c == '-'))
		{
			return false;
		}
	}
	return true;
}

bool isValidDateTime (const Scalar * scalar)
{
	switch (scalar->type)
	{
	case SCALAR_DATE_OFFSET_DATETIME:
		return isValidOffsetDateTime (scalar->str);
	case SCALAR_DATE_LOCAL_DATETIME:
		return isValidLocalDateTime (scalar->str);
	case SCALAR_DATE_LOCAL_DATE:
		return isValidLocalDate (scalar->str);
	case SCALAR_DATE_LOCAL_TIME:
		return isValidLocalTime (scalar->str);
	default:
		return false;
	}
}

bool isValidOffsetDateTime (const char * str)
{
	const char * time = strpbrk (str, "T ");
	assert (time != NULL);
	return isValidFullDate (str) && isValidFullTime (time + 1);
}

bool isValidLocalDateTime (const char * str)
{
	const char * time = strpbrk (str, "T ");
	assert (time != NULL);
	return isValidFullDate (str) && isValidPartialTime (time + 1);
}

bool isValidLocalDate (const char * str)
{
	return isValidFullDate (str);
}

bool isValidLocalTime (const char * str)
{
	return isValidPartialTime (str);
}

static bool isValidFullDate (const char * fullDate)
{
	int y, m, d;
	sscanf (fullDate, "%4d-%2d-%2d", &y, &m, &d);
	return isValidDate (y, m, d);
}

static bool isValidFullTime (const char * fullTime)
{
	const char * offset = strpbrk (fullTime, "Z+-");
	assert (offset != NULL);
	return isValidPartialTime (fullTime) && isValidTimeOffset (offset);
}

static bool isValidPartialTime (const char * partialTime)
{
	int h, m, s;
	sscanf (partialTime, "%2d:%2d:%2d", &h, &m, &s);
	return isValidTime (h, m, s);
}

static bool isValidTimeOffset (const char * timeOffset)
{
	if (timeOffset[0] == 'Z')
	{
		return true;
	}
	else
	{
		int h, m;
		sscanf (timeOffset + 1, "%2d:%2d", &h, &m);
		return isValidTime (h, m, 0);
	}
}

static bool isValidTime (int hour, int minute, int second)
{
	return hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0 &&
	       second <= 60; // 60 to allow positive leap seconds, (also ignores negative leap seconds)
}

static bool isValidDate (int year, int month, int day)
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
				assert (0);
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
