/**
 * @file
 *
 * @brief Helpers for key manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


#include <elektra/core/key.h>
#include <elektra/type/types.h>
#include <internal/kdbprivate.h>

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
