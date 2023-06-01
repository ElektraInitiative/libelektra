/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <string.h>

#include <internal/kdbprivate.h>
#include <internal/macros/os.h>

/**
 * @brief Pop key at given cursor position
 *
 * @param ks the keyset to pop key from
 * @param c where to pop
 *
 * @return the popped key
 * @retval 0 if ks is 0
 */
Key * elektraKsPopAtCursor (KeySet * ks, elektraCursor pos)
{
	if (!ks) return 0;
	if (pos < 0) return 0;
	if (pos > SSIZE_MAX) return 0;

	keySetDetachData (ks);

	size_t c = pos;
	if (c >= ks->data->size) return 0;

	if (c != ks->data->size - 1)
	{
		Key ** found = ks->data->array + c;
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
		memmove (found, found + 1, (ks->data->size - c - 1) * sizeof (Key *));
		*(ks->data->array + ks->data->size - 1) = k; // prepare last element to pop
	}
	else
	{
		// if c is on last position it is just a ksPop..
		// so do nothing..
	}

	/* TODO: Remove use of deprecated internal iterator! */
	ksRewind (ks);

	return ksPop (ks);
}
