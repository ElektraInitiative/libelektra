/**
 * @file
 *
 * @brief Internal API for key data
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEY_DATA_INTERNAL_H
#define ELEKTRA_CORE_KEY_DATA_INTERNAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/**
 * The private copy-on-write key data structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs.
 *
 */
struct _KeyData
{
	/**
	 * The value, which is a NULL terminated string or binary.
	 * @see keyString(), keyBinary(),
	 * @see keyGetString(), keyGetBinary(),
	 * @see keySetString(), keySetBinary()
	 */
	union
	{
		char * c;
		void * v;
	} data;

	/**
	 * Size of the value, in bytes, including ending NULL.
	 * @see keyGetValueSize()
	 */
	size_t dataSize;

	/**
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Is this structure and its data stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 15;
};

struct _KeyData * keyDataNew (void);
uint16_t keyDataRefInc (struct _KeyData * keydata);
uint16_t keyDataRefDec (struct _KeyData * keydata);
uint16_t keyDataRefDecAndDel (struct _KeyData * keydata);
void keyDataDel (struct _KeyData * keydata);

static inline bool isKeyDataInMmap (const struct _KeyData * keydata)
{
	return keydata->isInMmap;
}

static inline void setKeyDataIsInMmap (struct _KeyData * keydata, bool isInMmap)
{
	keydata->isInMmap = isInMmap;
}

#endif // ELEKTRA_CORE_KEY_DATA_INTERNAL_H
