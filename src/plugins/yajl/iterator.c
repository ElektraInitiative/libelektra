/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "iterator.h"

keyNameReverseIterator elektraKeyNameGetReverseIterator (const Key * k)
{
	keyNameReverseIterator it;
	it.rend = keyName (k);
	it.rbegin = it.rend + keyGetNameSize (k);
	it.current = it.rbegin;
	it.size = 0;
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
	int endReached = 0;

	// skip all repeating '/' in the "beginning" of string
	while (*real == KDB_PATH_SEPARATOR)
	{
		--real;
	}

	if (*real == KDB_PATH_ESCAPE)
	{
		++real; // we skipped to much
	}

	const char * currentEnd = real; // now we know where the string will end

	// now see where this basename begins
	// also handles escaped chars with '\'
	while (real != it->rend && !endReached)
	{
		--real;
		if (real != it->rend && *real == KDB_PATH_SEPARATOR)
		{
			// only decrement if we have not send the end
			--real;
			if (*real != KDB_PATH_ESCAPE)
			{
				endReached = 1;
				real += 2; // fix for lookahead
			}
		}
	}

	// update iterator and return it
	it->size = currentEnd - real + 1;
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
 * user/sw/x
 * user/sw/x/y
 * user/sw/x/y/z1
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
Key * elektraNextNotBelow (KeySet * ks)
{
	const Key * previous = ksNext (ks);

	if (!previous)
	{
		ksRewind (ks);
		return 0;
	}

	// unitialized variables are ok, because do{}while guarantees initialisation
	cursor_t pos;			// always one before current
	const Key * current = previous; // current is same as ksCurrent()
	do
	{
		pos = ksGetCursor (ks); // remember candidate
		previous = current;     // and remember last key
		current = ksNext (ks);  // look forward to next key
	} while (current && keyIsBelow (previous, current));

	// jump to and return candidate, because next is known to be not
	// below candidate
	ksSetCursor (ks, pos);
	return ksCurrent (ks);
}
