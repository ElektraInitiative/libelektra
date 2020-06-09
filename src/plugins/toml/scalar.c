#include "scalar.h"

#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// for unicode -> utf8 conversion, not sure if needs adaption to work on big endian
#define TRAIL 0x80
#define MASK(b) (b == 6 ? 0x3F : b == 5 ? 0x1F : b == 4 ? 0x0F : 0x07)
#define GET_BITS(v, s, b) (((v) >> (s)) & MASK (b))
#define LEAD(n) (n == 1 ? 0x00 : n == 2 ? 0xC0 : n == 3 ? 0xE0 : 0xF0)
#define ZERO(n) (n == 1 ? 0x7F : n == 2 ? 0xDF : n == 3 ? 0xEF : 0xF7)
#define ZERO_TRAIL 0xBF

static char * convertBinary (const char * binStr);
static char * convertBoolean (const char * str);
static char * convertBasicStr (const char * str, size_t skipCount);
static char * convertLiteralStr (const char * str, size_t skipCount);
static char * stripUnderscores (const char * num);
static const char * skipLineEndingBackslash (const char * str);
static const char * skipUntilNonWhitespace (const char * str);
static size_t unicodeCodepointToUtf8 (const char * codepoint, int len, unsigned char * utf8);

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
	Scalar * scalar = elektraCalloc (sizeof (Scalar));
	if (scalar == NULL)
	{
		return NULL;
	}
	scalar->type = type;
	scalar->str = scalarString;
	scalar->line = line;
	return scalar;
}

Scalar * createScalarDup (ScalarType type, const char * scalarString, size_t line)
{
	Scalar * scalar = elektraCalloc (sizeof (Scalar));
	if (scalar == NULL)
	{
		return NULL;
	}
	scalar->type = type;
	if (scalarString != NULL)
	{
		scalar->str = elektraStrDup (scalarString);
		if (scalar->str == NULL)
		{
			elektraFree (scalar);
			return NULL;
		}
	}
	else
	{
		scalar->str = NULL;
	}
	scalar->line = line;
	return scalar;
}

void freeScalar(Scalar * scalar) {
	if (scalar != NULL) {
		elektraFree(scalar->str);
		elektraFree(scalar);
	}
}

const char * getTypeCheckerType (const Scalar * scalar)
{
	switch (scalar->type)
	{
	case SCALAR_INTEGER_DEC:
	case SCALAR_INTEGER_HEX:
	case SCALAR_INTEGER_OCT:
	case SCALAR_INTEGER_BIN:
		return "long_long";
	case SCALAR_BOOLEAN:
		return "boolean";
	case SCALAR_FLOAT_NUM:
	case SCALAR_FLOAT_INF:
	case SCALAR_FLOAT_POS_INF:
	case SCALAR_FLOAT_NEG_INF:
	case SCALAR_FLOAT_NAN:
	case SCALAR_FLOAT_POS_NAN:
	case SCALAR_FLOAT_NEG_NAN:
		return "double";
	default:
		return "string";
	}
}

char * translateScalar (const Scalar * scalar)
{
	switch (scalar->type)
	{
	case SCALAR_INTEGER_DEC:
	case SCALAR_INTEGER_HEX:
	case SCALAR_INTEGER_OCT:
	case SCALAR_FLOAT_NUM:
		return stripUnderscores (scalar->str);
	case SCALAR_FLOAT_INF:
	case SCALAR_FLOAT_POS_INF:
	case SCALAR_FLOAT_NEG_INF:
	case SCALAR_FLOAT_NAN:
	case SCALAR_FLOAT_POS_NAN:
	case SCALAR_FLOAT_NEG_NAN:
		return elektraStrDup (scalar->str);
	case SCALAR_INTEGER_BIN:
		return convertBinary (scalar->str);
	case SCALAR_BOOLEAN:
		return convertBoolean (scalar->str);
	case SCALAR_STRING_BASIC:
		return convertBasicStr (scalar->str, 1);
	case SCALAR_STRING_ML_BASIC:
		return convertBasicStr (scalar->str, 3);
	case SCALAR_STRING_LITERAL:
		return convertLiteralStr (scalar->str, 1);
	case SCALAR_STRING_ML_LITERAL:
		return convertLiteralStr (scalar->str, 3);
	case SCALAR_STRING_COMMENT:
		return elektraStrDup (scalar->str);
	case SCALAR_DATE_OFFSET_DATETIME:
	case SCALAR_DATE_LOCAL_DATETIME:
	case SCALAR_DATE_LOCAL_DATE:
	case SCALAR_DATE_LOCAL_TIME:
		return elektraStrDup (scalar->str);
	case SCALAR_STRING_BARE:
		return elektraStrDup (scalar->str);
	default:
		ELEKTRA_ASSERT (0, "All possible scalar enums must be handeled, but got into default branch");
		return NULL;
	}
}

static char * convertBoolean (const char * str)
{
	if (elektraStrCmp (str, "true") == 0)
	{
		return elektraStrDup ("1");
	}
	else
	{
		return elektraStrDup ("0");
	}
}
static char * convertLiteralStr (const char * str, size_t skipCount)
{
	char * outStr = elektraCalloc (elektraStrLen (str));
	if (outStr == NULL)
	{
		return NULL;
	}
	const char * stop = str + elektraStrLen (str) - skipCount - 1;
	str += skipCount;

	char * ptr = outStr;
	if (str[0] == '\n')
	{
		str++;
	}
	while (str < stop)
	{
		if (*str == '\\')
		{ // only possible escape sequence in literal is line ending backslash
			str = skipLineEndingBackslash (str);
		}
		else
		{
			*(ptr++) = *(str++);
		}
	}
	return outStr;
}

static char * convertBasicStr (const char * str, size_t skipCount)
{
	size_t size = elektraStrLen (str) + 4;
	char * outStr = elektraCalloc (size);
	if (outStr == NULL)
	{
		return NULL;
	}
	size_t outPos = 0;
	const char * stop = str + elektraStrLen (str) - skipCount - 1;
	str += skipCount;

	while (str < stop)
	{
		if (outPos + 4 >= size)
		{ // 4 is maximal amount of chars possibly written per loop
			size *= 2;
			if (elektraRealloc ((void **) &outStr, size) < 0)
			{
				return NULL;
			}
		}
		if (*str == '\\')
		{
			switch (*(++str))
			{
			case 'b':
				outStr[outPos++] = 0x08;
				str++;
				break;
			case 't':
				outStr[outPos++] = 0x09;
				str++;
				break;
			case 'n':
				outStr[outPos++] = 0x0A;
				str++;
				break;
			case 'f':
				outStr[outPos++] = 0x0C;
				str++;
				break;
			case 'r':
				outStr[outPos++] = 0x0D;
				str++;
				break;
			case '"':
				outStr[outPos++] = 0x22;
				str++;
				break;
			case '\\':
				outStr[outPos++] = 0x5C;
				str++;
				break;
			case 'u':
				outPos += unicodeCodepointToUtf8 (str + 1, 4, (unsigned char *) outStr + outPos);
				str += 4 + 1;
				break;
			case 'U':
				outPos += unicodeCodepointToUtf8 (str + 1, 8, (unsigned char *) outStr + outPos);
				str += 8 + 1;
				break;
			// handling of line ending backslashes
			case ' ':
			case '\t': // WHITESPACE +  CR? + LF
			case '\n': // LF
			case '\r': // CR + LF
				str = skipLineEndingBackslash (str - 1);
				break;
			default:
				ELEKTRA_ASSERT (0, "No invalid escape codes allowed at this stage");
			}
		}
		else
		{
			if (outPos > 0 || (outPos == 0 && *str != '\n'))
			{
				outStr[outPos++] = *(str++);
			}
			else
			{
				str++;
			}
		}
	}
	return outStr;
}

static const char * skipLineEndingBackslash (const char * str)
{
	ELEKTRA_ASSERT (*str == '\\', "skipLineEndingBackslash expects str to be at a backslash");
	switch (*(++str))
	{
	case ' ':
	case '\t': // WHITESPACE* +  CR? + LF + WHITESPACE*
		str = skipUntilNonWhitespace (str + 1);
		if (*str == '\r')
		{
			str++;
		}
		ELEKTRA_ASSERT (*str == '\n', "After backslash, \\r only allowed when followed by \\n. This should have already been checked.");
		str = skipUntilNonWhitespace (str + 1);
		break;
	case '\n': // LF + WHITESPACE *
		str = skipUntilNonWhitespace (str + 1);
		break;
	case '\r': // CR + LF + WHITESPACE*
		ELEKTRA_ASSERT (*(str + 1) == '\n', "After backslash, \\r only allowed when followed by \\n. This should have already been checked.");
		str = skipUntilNonWhitespace (str + 2);
		break;
	default:
		ELEKTRA_ASSERT (0, "Invalid character after line ending backslash (0x%02X). This should already have been checked.", *str);
	}
	return str;
}

static const char * skipUntilNonWhitespace (const char * str)
{
	while (*str == ' ' || *str == '\t' || *str == '\n' || *str == '\r')
	{
		str++;
	}
	return str;
}

static size_t unicodeCodepointToUtf8 (const char * codepoint, int len, unsigned char * utf8)
{
	unsigned long cpValue;
	if (len == 4)
	{
		sscanf (codepoint, "%4lX", &cpValue);
	}
	else if (len == 8)
	{
		sscanf (codepoint, "%8lX", &cpValue);
	}
	else
	{
		ELEKTRA_ASSERT (0, "Code point len must be 4 or 8, but was %d.", len);
	}
	if (cpValue <= 0x7F)
	{
		utf8[0] = (char) cpValue;
		return 1;
	}
	else if (cpValue >= 0x80 && cpValue <= 0x7FF)
	{
		utf8[0] = (unsigned char) ZERO (2) & (LEAD (2) | GET_BITS (cpValue, 6, 5));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 0, 6));
		return 2;
	}
	else if (cpValue >= 0x800 && cpValue <= 0xFFFF)
	{
		utf8[0] = (unsigned char) ZERO (3) & (LEAD (3) | GET_BITS (cpValue, 6 + 6, 4));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 6, 6));
		utf8[2] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 0, 6));
		return 3;
	}
	else if (cpValue >= 0x10000 && cpValue <= 0x1FFFF)
	{
		utf8[0] = (unsigned char) ZERO (4) & (LEAD (4) | GET_BITS (cpValue, 6 + 6 + 6, 3));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 6 + 6, 6));
		utf8[2] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 6, 6));
		utf8[3] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (cpValue, 0, 6));
		return 4;
	}
	else
	{
		ELEKTRA_ASSERT (0, "Invalid unicode codepoints should already have been checked.");
		return 0;
	}
}

static char * convertBinary (const char * binStr)
{
	binStr += 2; // skip 0b prefix
	long long value = 0;
	long long exp = 1;
	for (int i = elektraStrLen (binStr) - 2; i >= 0; i--)
	{
		if (binStr[i] == '1')
		{
			value += exp;
		}
		if (binStr[i] != '_')
		{
			exp <<= 1;
		}
	}
	char * ret = elektraCalloc (40);
	if (ret == NULL)
	{
		return NULL;
	}
	snprintf (ret, 40, "%lli", value);
	return ret;
}

static char * stripUnderscores (const char * num)
{
	char * dup = elektraStrDup (num);
	if (dup == NULL)
	{
		return NULL;
	}
	char * ptr = dup;
	while (*num != 0)
	{
		if (*num != '_')
		{
			*ptr = *num;
			ptr++;
		}
		num++;
	}
	return dup;
}

bool isValidBareString (const char * str)
{
	// [a-zA-Z0-9-_]
	for (const char * c = str; *c != 0; c++)
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
	ELEKTRA_ASSERT (time != NULL, "Supplied offset datetime str was not valid, should have been checked before. Str = '%s'", str);
	return isValidFullDate (str) && isValidFullTime (time + 1);
}

bool isValidLocalDateTime (const char * str)
{
	const char * time = strpbrk (str, "T ");
	ELEKTRA_ASSERT (time != NULL, "Supplied local datetime str was not valid, should have been checked before. Str = '%s'", str);
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
	ELEKTRA_ASSERT (offset != NULL, "Supplied fulltime str was not valid, should have been checked before. Str = '%s'", fullTime);
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
