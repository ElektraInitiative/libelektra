/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "iterator.h"

keyNameReverseIterator elektraKeyNameGetReverseIterator (const ElektraKey * k)
{
	keyNameReverseIterator it;
	it.rend = elektraKeyName (k);
	it.rbegin = it.rend + elektraKeyGetNameSize (k);
	it.current = it.rbegin;
	it.size = 0;

	if (elektraKeyGetUnescapedNameSize (k) == 3)
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
		while (real >= it->rend && *real != KDB_PATH_SEPARATOR)
		{
			--real;
		}

		size_t backslashes = 0;
		while (real - backslashes > it->rend && *(real - backslashes - 1) == KDB_PATH_ESCAPE)
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
 * @brief Forwards to key which is not below the next one
 *
 * Forwards at least forward one element.
 * ksCurrent() will point at the same key as the key which is returned.
 *
 * e.g.
 * user:/sw/x
 * user:/sw/x/y
 * user:/sw/x/y/z1
 *
 * @retval last element if no other found.
 * @retval 0 if there is no other element afterwards (keyset will be
 * rewinded then)
 *
 * @param ks keyset to use
 *
 * @return key after starting position which is not below (to any latter
 * one)
 */
ElektraKey * elektraNextNotBelow (ElektraKeyset * ks)
{
	const ElektraKey * previous = elektraKeysetNext (ks);

	if (!previous)
	{
		elektraKeysetRewind (ks);
		return 0;
	}

	// uninitialized variables are ok, because do{}while guarantees initialisation
	elektraCursor pos;		// always one before current
	const ElektraKey * current = previous; // current is same as ksCurrent()
	do
	{
		pos = elektraKeysetGetCursor (ks); // remember candidate
		previous = current;	// and remember last key
		current = elektraKeysetNext (ks);	// look forward to next key
	} while (current && elektraKeyIsBelow (previous, current));

	// jump to and return candidate, because next is known to be not
	// below candidate
	elektraKeysetSetCursor (ks, pos);
	return elektraKeysetCurrent (ks);
}
