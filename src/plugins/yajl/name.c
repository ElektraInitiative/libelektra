/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "name.h"

#include <string.h>

#include "iterator.h"
#include "yajl.h"

// TODO defined privately in keyhelpers.c, API break possible..
char * keyNameGetOneLevel (const char * name, size_t * size);

/**
 * @brief Count number of levels in name of key
 *
 * @param cur the key to count levels
 *
 * @return number of levels in key name
 */
ssize_t elektraKeyCountLevel (const Key * cur)
{
	if (!cur)
	{
		return -1;
	}

	ssize_t curLevels = 0;
	keyNameReverseIterator curIt = elektraKeyNameGetReverseIterator (cur);
	while (elektraKeyNameReverseNext (&curIt))
	{
		++curLevels;
	}
	return curLevels;
}

/**
 * @brief Count how many levels are equal between cur and cmp
 * (starting from begin)
 *
 * @param cmp1 one key to compare
 * @param cmp2 the other key to compare
 *
 * @retval 0 on null pointers or nothing equal
 * @retval -1 when too many equal levels
 */
ssize_t elektraKeyCountEqualLevel (const Key * cmp1, const Key * cmp2)
{
	if (!cmp1)
	{
		return 0;
	}
	if (!cmp2)
	{
		return 0;
	}

	const char * pcmp1 = keyName (cmp1);
	const char * pcmp2 = keyName (cmp2);
	size_t size1 = 0;
	size_t size2 = 0;
	ssize_t counter = 0;

	while (*(pcmp1 = keyNameGetOneLevel (pcmp1 + size1, &size1)) && *(pcmp2 = keyNameGetOneLevel (pcmp2 + size2, &size2)) &&
	       size1 == size2 && !strncmp (pcmp1, pcmp2, size1))
	{
		++counter;
	}

	if (counter < 0)
	{
		counter = -1;
	}

	return counter;
}
