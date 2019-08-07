/**
 * @file
 *
 * @brief Functional helper.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define __STDC_FORMAT_MACROS

#include <elektra/kdb.h>

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
	Key * current;

	cursor_t cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		int rc = filter (current, argument);
		if (rc <= -1)
			return -1;
		else if (rc > 0)
		{
			++ret;
			ksAppendKey (result, current);
		}
	}
	ksSetCursor (input, cursor);
	return ret;
}
