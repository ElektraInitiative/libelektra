/**
 * @file
 *
 * @brief Internal structure of key
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEY_STRUCT_INTERNAL_H
#define ELEKTRA_CORE_KEY_STRUCT_INTERNAL_H

#include <elektra/core/types.h>

#include <stdbool.h>
#include <stdint.h>

/**
 * The private Key struct.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs. Use the @ref key "Key access methods" instead.
 * Only a backend writer needs to have access to the private attributes of the
 * Key object which is defined as:
 * @code
typedef struct _Key Key;
 * @endcode
 *
 * @ingroup backend
 */
struct _Key
{
	/**
	 * Copy-on-write structure for the key data
	 */
	struct _KeyData * keyData;

	/**
	 * Copy-on-write structure for the key name
	 */
	struct _KeyName * keyName;

	/**
	 * All the key's meta information.
	 */
	KeySet * meta;

	/**
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Is this structure stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Key need sync.
	 * If name, value or metadata are changed this flag will be set,
	 * so that the backend will sync the key to database.
	 */
	bool needsSync : 1;

	/**
	 * Read only flag for name.
	 * Key name is read only and not allowed to be changed.
	 * All attempts to change the name will lead to an error.
	 * Needed for metakeys and keys that are in a data structure that depends on name ordering.
	 */
	bool hasReadOnlyName : 1;

	/**
	 * Read only flag for value.
	 * Key value is read only and not allowed  to be changed.
	 * All attempts to change the value will lead to an error.
	 * Needed for metakeys
	 */
	bool hasReadOnlyValue : 1;

	/**
	 * Read only flag for meta.
	 * Key meta is read only and not allowed to be changed.
	 * All attempts to change the value will lead to an error.
	 * Needed for metakeys.
	 */
	bool hasReadOnlyMeta : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 11;
};

#endif // ELEKTRA_CORE_KEY_STRUCT_INTERNAL_H
