/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./name.h"

#include <string.h>

#include "./iterator.h"
#include "./yajl.h"

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

	const char * pcmp1 = keyUnescapedName (cmp1);
	const char * pcmp2 = keyUnescapedName (cmp2);

	size_t size1 = keyGetUnescapedNameSize (cmp1);
	size_t size2 = keyGetUnescapedNameSize (cmp2);

	size_t counter = 0;
	const char * cur1 = pcmp1;
	const char * cur2 = pcmp2;
	while (strcmp (cur1, cur2) == 0)
	{
		const char * next1 = strchr (cur1, '\0');
		const char * next2 = strchr (cur2, '\0');

		++counter;

		cur1 = next1 + 1;
		cur2 = next2 + 1;

		if (pcmp1 + size1 <= cur1 || pcmp2 + size2 <= cur2)
		{
			break;
		}
	}

	return counter;
}
