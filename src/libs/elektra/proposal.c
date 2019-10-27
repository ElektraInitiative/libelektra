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
 * @copydoc keyLock
 */
int elektraKeyLock (Key * key, /*option_t*/ enum elektraLockOptions what)
{
	int ret = 0;

	if (!key) return -1;

	if (test_bit (what, KEY_LOCK_NAME))
	{
		if (!test_bit (key->flags, KEY_FLAG_RO_NAME))
		{
			set_bit (key->flags, KEY_FLAG_RO_NAME);
			set_bit (ret, KEY_LOCK_NAME);
		}
	}

	if (test_bit (what, KEY_LOCK_VALUE))
	{
		if (!test_bit (key->flags, KEY_FLAG_RO_VALUE))
		{
			set_bit (key->flags, KEY_FLAG_RO_VALUE);
			set_bit (ret, KEY_LOCK_VALUE);
		}
	}

	if (test_bit (what, KEY_LOCK_META))
	{
		if (!test_bit (key->flags, KEY_FLAG_RO_META))
		{
			set_bit (key->flags, KEY_FLAG_RO_META);
			set_bit (ret, KEY_LOCK_META);
		}
	}

	return ret;
}


/**
 * @copydoc ksPopAtCursor
 */
Key * elektraKsPopAtCursor (KeySet * ks, cursor_t pos)
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
