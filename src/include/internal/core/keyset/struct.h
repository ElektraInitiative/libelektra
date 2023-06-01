/**
 * @file
 *
 * @brief Internal structure of keyset
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEYSET_STRUCT_INTERNAL_H
#define ELEKTRA_CORE_KEYSET_STRUCT_INTERNAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/**
 * The private KeySet structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs. Use the @ref keyset "KeySet access methods" instead.
 * Only a backend writer needs to have access to the private attributes of the
 * KeySet object which is defined as:
 * @code
typedef struct _KeySet KeySet;
 * @endcode
 *
 * @ingroup backend
 */
struct _KeySet
{
	/**
	 * Copy-on-write data
	 */
	struct _KeySetData * data;

	struct _Key * cursor; /**< Internal cursor */
	size_t current;	      /**< Current position of cursor */

	uint16_t refs; /**< Reference counter */

	/**
	 * Is this structure stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * KeySet need sync.
	 * If keys were popped from the Keyset this flag will be set,
	 * so that the backend will sync the keys to database.
	 */
	bool needsSync : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 14;
};

#endif // ELEKTRA_CORE_KEYSET_STRUCT_INTERNAL_H
