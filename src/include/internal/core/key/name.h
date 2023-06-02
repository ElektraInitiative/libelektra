/**
 * @file
 *
 * @brief Internal API for keyname
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEY_NAME_INTERNAL_H
#define ELEKTRA_CORE_KEY_NAME_INTERNAL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/**
 * The private copy-on-write keyname structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs.
 *
 */
struct _KeyName
{
	/**
	 * The canonical (escaped) name of the key.
	 * @see keyGetName(), keySetName()
	 */
	char * key;

	/**
	 * Size of the name, in bytes, including ending NULL.
	 * @see keyGetName(), keyGetNameSize(), keySetName()
	 */
	size_t keySize;

	/**
	 * The unescaped name of the key.
	 * Note: This is NOT a standard null-terminated string.
	 * @see keyGetName(), keySetName()
	 */
	char * ukey;

	/**
	 * Size of the unescaped key name in bytes, including all NULL.
	 * @see keyBaseName(), keyUnescapedName()
	 */
	size_t keyUSize;

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

// private methods for COW keys
struct _KeyName * keyNameNew (void);
struct _KeyName * keyNameCopy (struct _KeyName * source);
uint16_t keyNameRefInc (struct _KeyName * keyname);
uint16_t keyNameRefDec (struct _KeyName * keyname);
uint16_t keyNameRefDecAndDel (struct _KeyName * keyname);
void keyNameDel (struct _KeyName * keyname);

static inline bool isKeyNameInMmap (const struct _KeyName * keyname)
{
	return keyname->isInMmap;
}

static inline void setKeyNameIsInMmap (struct _KeyName * keyname, bool isInMmap)
{
	keyname->isInMmap = isInMmap;
}

bool elektraKeyNameValidate (const char * name, bool isComplete);
void elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t * canonicalSizePtr, size_t offset, size_t * usizePtr);
void elektraKeyNameUnescape (const char * name, char * unescapedName);
size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart);

#endif // ELEKTRA_CORE_KEY_NAME_INTERNAL_H
