#define __STDC_FORMAT_MACROS

#include <kdbproposal.h>
#include "kdbtypes.h"

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

/**
 * @brief validate array syntax
 *
 * @param key an element of an array
 *
 * @retval -1 if no array element/syntax error/no key
 * @retval 0 if start
 * @retval 1 if array element
 */
int elektraArrayValidateName(Key *key)
{
	if (!key)
	{
		return -1;
	}

	const char *current = keyBaseName(key);

	if (!current)
	{
		return -1;
	}

	if (!strcmp(current, "#"))
	{
		return 0;
	}

	if (*current == '#')
	{
		current++;
		int underscores = 0;
		int digits = 0;

		for (; *current == '_'; current++)
		{
			underscores++;
		}

		for (; isdigit (*current); current++)
		{
			digits++;
		}

		if (underscores != digits -1) return -1;
	}
	else
	{
		return -1;
	}

	return 1;
}

/**
 * @brief Increment the name of the key by one
 *
 * Alphabetical order will remain
 *
 * e.g. user/abc/\#9 will be changed to
 *      user/abc/\#_10
 *
 * For the start:
 *      user/abc/\#
 * will be changed to
 *      user/abc/\#0
 *
 * @param key which base name will be incremented
 *
 * @retval -1 on error (e.g. too large array, not validated array)
 * @retval 0 on success
 */
int elektraArrayIncName(Key *key)
{
	const char * baseName = keyBaseName(key);

	int arrayElement = elektraArrayValidateName(key);

	if (arrayElement == -1)
	{
		return -1;
	}

	++baseName; // jump over #
	while(*baseName == '_') // jump over all _
	{
		++baseName;
	}

	int64_t oldIndex  = 0;

	int errnosave = errno;
	errno = 0;
	if (!arrayElement)
	{
		// we have a start element
		oldIndex = -1;
	}
	else
	{
		if (sscanf(baseName, "%"PRId64, &oldIndex) != 1)
		{
			errno = errnosave;
			return -1;
		}

		if (errno != 0) // any error
		{
			errno = errnosave;
			return -1;
		}

		if (oldIndex < 0) // underflow
		{
			return -1;
		}

		if (oldIndex >= INT64_MAX) // overflow
		{
			return -1;
		}
	}

	int64_t newIndex = oldIndex+1; // we increment by one

	// maximal size calculation (C99 would also allow non maximum though...)
	size_t sizeMax_ = 9; // maximum of n-1 _
	size_t sizeNum = 10; // maximum of 10 digits in 32bit number
	size_t size = sizeMax_ + sizeNum + 1;
	char newName[size]; // #_________4000000000

	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	int64_t i = newIndex/10;
	while (i>0)
	{
		newName[index++] = '_'; // index n-1 of decimals
		i/=10;
	}
	if (snprintf (&newName[index], size, "%"PRId64, newIndex)  < 0)
	{
		return -1;
	}
	keySetBaseName(key, 0);
	keyAddName(key, newName);

	return 0;
}
