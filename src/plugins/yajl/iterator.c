/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "iterator.h"

keyNameReverseIterator elektraKeyNameGetReverseIterator (const Key * k)
{
	keyNameReverseIterator it;
	it.rend = keyName (k);
	it.rbegin = it.rend + keyGetNameSize (k);
	it.current = it.rbegin;
	it.size = 0;

	if (keyGetUnescapedNameSize (k) == 3)
	{
		it.current -= 2;
	}

	return it;
}


/**
 * @brief Go to the previous element
 *
 * @param it the iterator to iterate
 *
 * @return
 */
int elektraKeyNameReverseNext (keyNameReverseIterator * it)
{
	if (it->current == it->rend) // we are at the end (move that to hasNext?)
	{
		return 0;
	}

	const char * real = it->current - 1; // start at one position left

	while (real >= it->rend)
	{
		--real;
		while (real >= it->rend && *real != '/')
		{
			--real;
		}

		size_t backslashes = 0;
		while (real - backslashes > it->rend && *(real - backslashes - 1) == '\\')
		{
			++backslashes;
		}

		if (backslashes % 2 == 0)
		{
			++real;
			break;
		}
	}

	// update iterator and return it
	it->size = it->current - 1 - real;
	it->current = real;
	return 1;
}

/**
 * @brief Gets the first key which is not below the given one
 *
 * e.g.
 * user:/sw/x
 * user:/sw/x/y
 * user:/sw/x/y/z1
 *
 * @retval last element if no other found.
 * @retval 0 if there is no other element afterwards
 *
 * @param ks keyset to use
 * @param pos the Position where the search should start
 *
 * @return key after starting position which is not below (to any latter
 * one)
 */
Key * elektraNextNotBelow (KeySet * ks, elektraCursor pos)
{
	const Key * previous = ksAtCursor (ks, pos);

	if (!previous) return 0;

	const Key * current = previous;
	do
	{
		previous = current; // remember last key
		current = ksAtCursor (ks, ++pos);
	} while (current && keyIsBelow (previous, current));

	return ksAtCursor (ks, pos - 1); // return candidate
}
