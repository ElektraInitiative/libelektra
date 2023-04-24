/**
 * @file scalar.c
 *
 * @brief Functions for handling scalar key values, used on reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "./scalar.h"

#include <internal/utility/assert.h>
#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./codepoint.h"
#include "./type.h"

static char * convertBinary (const char * binStr);
static char * convertBoolean (const char * str);
static char * stripUnderscores (const char * num);

static char * uNumToStr (unsigned long long num);
static char * convertDecimal (const char * str);
static char * convertHex (const char * str);
static char * convertOctal (const char * str);
static char * convertBinary (const char * str);

Scalar * createScalar (ScalarType type, char * scalarString, char * origString, size_t line)
{
	Scalar * scalar = elektraCalloc (sizeof (Scalar));
	if (scalar == NULL)
	{
		return NULL;
	}
	scalar->type = type;
	scalar->str = scalarString;
	scalar->orig = origString;
	scalar->line = line;
	scalar->leadingSpaces = 0;
	return scalar;
}

Scalar * createScalarDup (ScalarType type, const char * scalarString, const char * origString, size_t line)
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
	if (origString != NULL)
	{
		scalar->orig = elektraStrDup (origString);
		if (scalar->orig == NULL)
		{
			elektraFree (scalar);
			return NULL;
		}
	}
	else
	{
		scalar->orig = NULL;
	}
	scalar->line = line;
	return scalar;
}

void freeScalar (Scalar * scalar)
{
	if (scalar != NULL)
	{
		if (scalar->str != NULL)
		{
			elektraFree (scalar->str);
		}
		if (scalar->orig != NULL)
		{
			elektraFree (scalar->orig);
		}
		elektraFree (scalar);
	}
}

char * translateScalar (const Scalar * scalar)
{
	switch (scalar->type)
	{
	case SCALAR_INTEGER_DEC:
		return convertDecimal (scalar->str);
	case SCALAR_INTEGER_HEX:
		return convertHex (scalar->str);
	case SCALAR_INTEGER_OCT:
		return convertOctal (scalar->str);
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
		return elektraStrDup (scalar->str);
	case SCALAR_STRING_ML_BASIC:
		return elektraStrDup (scalar->str);
	case SCALAR_STRING_LITERAL:
		return elektraStrDup (scalar->str);
	case SCALAR_STRING_ML_LITERAL:
		return elektraStrDup (scalar->str);
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
		ELEKTRA_ASSERT (0, "All possible scalar enums must be handled, but got into default branch");
		return NULL;
	}
}

static char * uNumToStr (unsigned long long num)
{
	char * ret = elektraCalloc (100);
	if (ret == NULL)
	{
		return NULL;
	}
	snprintf (ret, 100, "%llu", num);
	return ret;
}

static char * convertDecimal (const char * str)
{
	return stripUnderscores (str);
}
static char * convertHex (const char * str)
{
	unsigned long long n = 0;
	char * stripped = stripUnderscores (str);
	if (sscanf (stripped, "0x%llx", &n) != 1)
	{
		elektraFree (stripped);
		ELEKTRA_ASSERT (0, "str must be convertible as long long hex");
		return NULL;
	}
	elektraFree (stripped);
	return uNumToStr (n);
}

static char * convertOctal (const char * str)
{
	unsigned long long n = 0;
	char * stripped = stripUnderscores (str);
	if (sscanf (stripped, "0o%llo", &n) != 1)
	{
		elektraFree (stripped);
		ELEKTRA_ASSERT (0, "str must be convertible as long long octal");
		return NULL;
	}
	elektraFree (stripped);
	return uNumToStr (n);
}

static char * convertBinary (const char * str)
{
	str += 2; // skip 0b prefix
	unsigned long long value = 0;
	unsigned long long exp = 1;
	for (int i = elektraStrLen (str) - 2; i >= 0; i--)
	{
		if (str[i] == '1')
		{
			value += exp;
		}
		if (str[i] != '_')
		{
			exp <<= 1;
		}
	}
	return uNumToStr (value);
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
		while (*num == '_')
		{
			num++;
		}
		*ptr = *num;
		ptr++;
		num++;
	}
	*ptr = 0;
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
		return validOffsetDateTimeValues (scalar->str);
	case SCALAR_DATE_LOCAL_DATETIME:
		return validLocalDateTimeValues (scalar->str);
	case SCALAR_DATE_LOCAL_DATE:
		return validLocalDateValues (scalar->str);
	case SCALAR_DATE_LOCAL_TIME:
		return validLocalTimeValues (scalar->str);
	default:
		return false;
	}
}
