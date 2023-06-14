/**
 * @file
 *
 * @brief Helpers for key manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <stdarg.h>
#include <stdlib.h>
#include <string.h>


#include <elektra/core/key.h>
#include <elektra/type/types.h>

#include <internal/config.h>
#include <internal/core/key.h>

/**
 * @internal
 *
 * @pre key->meta must be NULL or a valid keyset
 *
 * clears key (all data members are set to zero)
 * Initializes an empty metadata keyset if null or clears it.
 */
void keyInit (Key * key)
{
	memset (key, 0, sizeof (Key));
}
