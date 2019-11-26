/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <string.h>

#include <kdbprivate.h>

/**
 * @brief Pop key at given cursor position
 *
 * @param ks the keyset to pop key from
 * @param c where to pop
 *
 * The internal cursor will be rewinded using ksRewind(). You can use
 * ksGetCursor() and ksSetCursor() jump back to the previous position.
 * e.g. to pop at current position within ksNext() loop:
 * @code
 * cursor_t c = ksGetCursor(ks);
 * keyDel (elektraKsPopAtCursor(ks, c));
 * ksSetCursor(ks, c);
 * ksPrev(ks); // to have correct key after next ksNext()
 * @endcode
 *
 * @warning do not use, will be superseded by external iterator API
 *
 * @return the popped key
 * @retval 0 if ks is 0
 */
Key * elektraKsPopAtCursor (KeySet * ks, elektraCursor pos)
{
	if (!ks) return 0;
	if (pos < 0) return 0;
	if (pos > SSIZE_MAX) return 0;

	size_t c = pos;
	if (c >= ks->size) return 0;

	if (c != ks->size - 1)
	{
		Key ** found = ks->array + c;
		Key * k = *found;
		/* Move the array over the place where key was found
		 *
		 * e.g. c = 2
		 *   size = 6
		 *
		 * 0  1  2  3  4  5  6
		 * |--|--|c |--|--|--|size
		 * move to (c/pos is overwritten):
		 * |--|--|--|--|--|
		 *
		 * */
		memmove (found, found + 1, (ks->size - c - 1) * sizeof (Key *));
		*(ks->array + ks->size - 1) = k; // prepare last element to pop
	}
	else
	{
		// if c is on last position it is just a ksPop..
		// so do nothing..
	}

	ksRewind (ks);

	return ksPop (ks);
}
