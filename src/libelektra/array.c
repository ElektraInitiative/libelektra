/**
 * @file
 *
 * @brief Array methods.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#define __STDC_FORMAT_MACROS

#include <kdbprivate.h>

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

/**
 * @internal
 * @brief validate array syntax
 *
 * @param key an element of an array
 *
 * @retval -1 if no array element/syntax error/no key
 * @retval 0 if start
 * @retval 1 if array element
 */
int elektraArrayValidateName(const Key *key)
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
		if (underscores + digits > ELEKTRA_MAX_ARRAY_SIZE-2)
		{
			return -1;
		}
	}
	else
	{
		return -1;
	}

	return 1;
}

int elektraReadArrayNumber(const char *baseName, kdb_long_long_t *oldIndex)
{

	int errnosave = errno;
	errno = 0;
	if (sscanf(baseName, ELEKTRA_LONG_LONG_F, oldIndex) != 1)
	{
		errno = errnosave;
		return -1;
	}

	if (errno != 0) // any error
	{
		errno = errnosave;
		return -1;
	}

	if (*oldIndex < 0) // underflow
	{
		return -1;
	}

	/*
	overflow not possible, cannot be larger than largest number
	if (*oldIndex >= INT64_MAX) // overflow
	{
		return -1;
	}
	*/
	return 0;
}

/**
 * @internal
 * @brief Writes a elektra array name
 *
 * @param newName the buffer to write to (size must be
 *       #ELEKTRA_MAX_ARRAY_SIZE or more)
 * @param newIndex the index of the array to write
 *
 * @retval 0 on success
 * @retval -1 on error
 */
int elektraWriteArrayNumber(char *newName, kdb_long_long_t newIndex)
{
	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	kdb_long_long_t i = newIndex/10;

	while (i>0)
	{
		newName[index++] = '_'; // index n-1 of decimals
		i/=10;
	}
	if (snprintf (&newName[index], ELEKTRA_MAX_ARRAY_SIZE,
				ELEKTRA_LONG_LONG_F, newIndex)  < 0)
	{
		return -1;
	}

	return 0;
}

