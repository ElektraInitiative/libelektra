
#include <internal/utility/old_helper.h>
#include <limits.h>

#include "./integer.h"

static bool getValue (const char * start, const char * str, unsigned long long base, unsigned long long factor, unsigned long long * value);
static unsigned long long getDigitValue (char digit);

bool isValidIntegerAnyBase (const char * str)
{
	return isValidInteger (str, 2) || isValidInteger (str, 8) || isValidInteger (str, 10) || isValidInteger (str, 16);
}

bool isValidInteger (const char * str, unsigned long long base)
{
	if (str[0] == '0' || base == 10)
	{
		if ((base == 2 && str[1] == 'b') || (base == 8 && str[1] == 'o') || base == 10 || (base == 16 && str[1] == 'x'))
		{
			const char * start = str;
			if (base != 10)
			{
				start += 2;
			}
			else if (*str == '-' || *str == '+')
			{
				start++;
			}
			if (*start == '_' || *start == 0)
			{
				return false;
			}
			unsigned long long value = 0;
			bool success = getValue (start, start + elektraStrLen (start) - 2, base, 1, &value);
			if (!success)
			{
				return false;
			}
			else if (base == 10)
			{
				if (*str == '-')
				{
					return value <= (unsigned long long) LLONG_MIN;
				}
				else
				{
					return value <= (unsigned long long) LLONG_MAX;
				}
			}
			else
			{
				return success;
			}
		}
	}
	return false;
}

static unsigned long long getDigitValue (char digit)
{
	if (digit >= '0' && digit <= '9')
	{
		return digit - '0';
	}
	else if (digit >= 'a' && digit <= 'f')
	{
		return 10 + digit - 'a';
	}
	else if (digit >= 'A' && digit <= 'F')
	{
		return 10 + digit - 'A';
	}
	else
	{
		return (unsigned long long) -1;
	}
}

static bool getValue (const char * start, const char * str, unsigned long long base, unsigned long long factor, unsigned long long * value)
{
	if (str < start)
	{
		return true;
	}
	else if (*str == '_')
	{
		return getValue (start, str - 1, base, factor, value);
	}
	else
	{
		unsigned long long digitValue = getDigitValue (*str);
		if (digitValue >= base)
		{
			return false;
		}
		else if (digitValue > 0 && digitValue > ULLONG_MAX / factor)
		{
			return false;
		}
		unsigned long long factoredValue = factor * digitValue;
		if (factoredValue > ULLONG_MAX - *value)
		{
			return false;
		}
		else
		{
			*value += factoredValue;
			if (factor > ULLONG_MAX / base)
			{
				return str - 1 < start;
			}
			else
			{
				return getValue (start, str - 1, base, factor * base, value);
			}
		}
	}
	return false;
}
