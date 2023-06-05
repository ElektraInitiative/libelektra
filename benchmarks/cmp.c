/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./benchmarks.h"

#include <ctype.h>

int owncmp (const char * str1, const char * str2)
{
	while (*str1 && (*str1 == *str2))
	{
		str1++;
		str2++;
	}
	return *(const unsigned char *) str1 - *(const unsigned char *) str2;
}

// warning: these are not correct implementations, but just to get an
// impression about performance

int slacmp (const char * str1, const char * str2)
{
	while (*str1 && (*str1 == *str2))
	{
		str1++;
		str2++;
	}

	if (*str1 == '/')
	{
		return 1;
	}
	else if (*str2 == '/')
	{
		return -1;
	}
	return *(const unsigned char *) str1 - *(const unsigned char *) str2;
	// found different char
}

int natcmp (const char * str1, const char * str2)
{
	int count_num = -1;
	while (*str1 && (*str1 == *str2))
	{
		str1++;
		str2++;
		if (*str1 == '#')
		{
			count_num = 0;
		}
		else if (count_num >= 0 && !isdigit (*str1))
		{
			++count_num;
		}
		else
		{
			count_num = -1;
		}
	}

	if (count_num > 0)
	{
		return 12;
	}

	if (*str1 == '/')
	{
		return 1;
	}
	else if (*str2 == '/')
	{
		return -1;
	}
	return *(const unsigned char *) str1 - *(const unsigned char *) str2;
}


int main (void)
{
	long long nrIterations = 100000000;
	const char str1[] = "some string to be compared with/some\\/more with a long common part, and only a bit different";
	char * str2 = elektraMalloc (sizeof (str1));
	strcpy (str2, str1);
	str2[sizeof (str1) - 5] = 'X';

	int res = 0;

	timeInit ();
	for (int i = 0; i < nrIterations; ++i)
	{
		res ^= strcmp (str1, str2);
	}
	timePrint ("strcmp");
	for (int i = 0; i < nrIterations; ++i)
	{
		res ^= memcmp (str1, str2, sizeof (str1));
	}
	timePrint ("memcmp");
	for (int i = 0; i < nrIterations; ++i)
	{
		res ^= owncmp (str1, str2);
	}
	timePrint ("owncmp");
	for (int i = 0; i < nrIterations; ++i)
	{
		res ^= slacmp (str1, str2);
	}
	timePrint ("slacmp");
	for (int i = 0; i < nrIterations; ++i)
	{
		res ^= natcmp (str1, str2);
	}
	timePrint ("natcmp");

	printf ("%d\n", res);
}
