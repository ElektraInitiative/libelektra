/**
 * @file
 *
 * @brief Functional helper.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define __STDC_FORMAT_MACROS

#include <elektra/old_kdb.h>

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif


/**
 * @brief return only those keys from the given
 * keyset that pass the supplied filter function
 * with the supplied argument
 *
 * @param result the keyset that should contain the filtered keys
 * @param input the keyset whose keys should be filtered
 * @param filter a function pointer to a function that will be used to
 * filter the keyset. A key will be taken if the function returns a value
 * greater than 0.
 * @param argument an argument that will be passed to the filter function
 * each time it is called
 * @return the number of filtered keys if the filter function always
 * returned a positive value, -1 otherwise
 * @retval NULL on NULL pointer
 */
int elektraKsFilter (KeySet * result, KeySet * input, int (*filter) (const Key * k, void * argument), void * argument)
{
	if (!result) return -1;

	if (!input) return -1;

	if (!filter) return -1;

	int ret = 0;

	for (elektraCursor it = 0; it < ksGetSize (input); ++it)
	{
		Key * current = ksAtCursor (input, it);
		int rc = filter (current, argument);
		if (rc <= -1)
			return -1;
		else if (rc > 0)
		{
			++ret;
			ksAppendKey (result, current);
		}
	}
	return ret;
}

/**
 * Builds an array of pointers to the keys in the supplied keyset.
 * The keys are not copied, calling keyDel may remove them from
 * the keyset.
 *
 * The size of the buffer can be easily allocated via ksGetSize. Example:
 * @code
 * KeySet *ks = somekeyset;
 * Key **keyArray = calloc (ksGetSize(ks), sizeof (Key *));
 * elektraKsToMemArray (ks, keyArray);
 * ... work with the array ...
 * elektraFree (keyArray);
 * @endcode
 *
 * @param ks the keyset object to work with
 * @param buffer the buffer to put the result into
 * @return the number of elements in the array if successful
 * @return a negative number on null pointers or if an error occurred
 */
int elektraKsToMemArray (KeySet * ks, Key ** buffer)
{
	if (!ks) return -1;
	if (!buffer) return -1;

	/* clear the received buffer */
	memset (buffer, 0, ksGetSize (ks) * sizeof (Key *));

	size_t idx = 0;


	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * key = ksAtCursor (ks, it);
		buffer[idx] = key;
		++idx;
	}

	return idx;
}
