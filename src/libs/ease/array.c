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
	while (*baseName == '_') // jump over all _
	{
		++baseName;
	}

	kdb_long_long_t oldIndex  = 0;
	if (!arrayElement)
	{
		// we have a start element
		oldIndex = -1;
	}
	else
	{
		if (elektraReadArrayNumber(baseName, &oldIndex) == -1)
		{
			return -1;
		}
	}

	kdb_long_long_t newIndex = oldIndex+1; // we increment by one

	char newName[ELEKTRA_MAX_ARRAY_SIZE];

	elektraWriteArrayNumber(newName, newIndex);
	keySetBaseName(key, newName);

	return 0;
}

/**
 * @internal
 *
 * Returns true (1) for all keys that are part of the array
 * identified by the supplied array parent. Only the array
 * eleements themself, but no subkeys of them will be filtered
 *
 * @pre The supplied argument has to be of type (const Key *)
 * and is the parent of the array to be extracted. For example
 * if the keys of the array comment/# are to be extracted, a key
 * with the name "comment" has to be supplied
 *
 * @param key the key to be checked against the array
 * @param argument the array parent
 * @return 1 if the key is part of the array identified by the
 * array parent, 0 otherwise
 *
 */
static int arrayFilter(const Key *key, void *argument)
{
	const Key *arrayParent = (const Key *) argument;

	return keyIsDirectBelow(arrayParent, key) && elektraArrayValidateName(key);
}


/**
 * Return all the array keys below the given arrayparent
 * The arrayparent itself is not returned.
 * For example, if user/config/# is an array,
 * user/config is the array parent.
 * Only the direct array keys will be returned. This means
 * that for example user/config/#1/key will not be included,
 * but only user/config/#1.
 *
 * A new keyset will be allocated for the resulting keys.
 * This means that the caller must ksDel the resulting keyset.
 *
 * @param arrayParent the parent of the array to be returned
 * @param keys the keyset containing the array keys.
 *
 * @return a keyset containing the arraykeys (if any)
 * @retval NULL on NULL pointers
 */
KeySet *elektraArrayGet(const Key *arrayParent, KeySet *keys)
{
	if (!arrayParent) return 0;

	if (!keys) return 0;

	KeySet *arrayKeys = ksNew(ksGetSize(keys), KS_END);
	elektraKsFilter(arrayKeys, keys, &arrayFilter, (void *)arrayParent);
	return arrayKeys;
}

/**
 *
 * Return the next key in the given array.
 * The function will automatically allocate memory
 * for a new key and name it accordingly.
 *
 * @pre The supplied keyset must contain only valid array keys.
 *
 * The caller has to keyDel the resulting key.
 *
 * @param arraykeys the array where the new key will belong to
 *
 * @return the new array key on success
 * @retval NULL if the passed array is empty
 * @retval NULL on NULL pointers or if an error occurs
 */
Key *elektraArrayGetNextKey(KeySet *arrayKeys)
{
	if (!arrayKeys) return 0;

	Key *last = ksPop(arrayKeys);

	if (!last) return 0;

	ksAppendKey(arrayKeys, last);
	Key *newKey = keyDup(last);
	int ret = elektraArrayIncName(newKey);

	if (ret == -1)
	{
		keyDel(newKey);
		return 0;
	}

	return newKey;
}
