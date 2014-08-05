#include <kdbproposal.h>
#include "kdbtypes.h"

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>

/**
 * @brief Increment the name of the key by one
 *
 * Alphabetical order will remain
 *
 * e.g. user/abc/#9 will be changed to
 *      user/abc/#_10
 *
 * For the start:
 *      user/abc
 * will be changed to
 *      user/abc/#0
 *
 * @param key which base name will be incremented
 *
 * @retval -1 on error (e.g. too large array)
 * @retval 0 on success
 */
int elektraArrayIncName(Key *key)
{
	if (!key)
	{
		return -1;
	}

	const char * baseName = keyBaseName(key);
	if (!baseName)
	{
		return -1;
	}
	else if (*baseName != '#')
	{
		// finished: just start a new array
		keyAddBaseName(key, "#0");
		return 0;
	}

	++baseName; // jump over #
	while(*baseName == '_') // jump over all _
	{
		++baseName;
	}

	long int oldIndex  = 0;

	char *endptr;
	int errnosave = errno;
	errno = 0;
	oldIndex = strtol(baseName, &endptr, 10);

	if (errno != 0) // any error
	{
		errno = errnosave;
		return -1;
	}

	if (oldIndex < 0) // underflow
	{
		return -1;
	}

	if (oldIndex == LONG_MAX) // overflow
	{
		return -1;
	}

	if (endptr == baseName)
	{
		return -1;
	}

	long int newIndex = oldIndex+1; // we increment by one

	// maximal size calculation (C99 would also allow non maximum though...)
	size_t sizeMax_ = 9; // maximum of n-1 _
	size_t sizeNum = 10; // maximum of 10 digits in 32bit number
	size_t size = sizeMax_ + sizeNum + 1;
	char newName[size]; // #_________4000000000

	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	long int i = newIndex/10;
	while (i>0)
	{
		newName[index++] = '_'; // index n-1 of decimals
		i/=10;
	}
	if (snprintf (&newName[index], size, "%ld", newIndex)  < 0)
	{
		return -1;
	}
	keySetBaseName(key, newName);

	return 0;
}
