/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <ctype.h>
#include <string.h>

#include <kdbassert.h>
#include <kdbinternal.h>
#include <kdblogger.h>

/**
 * @defgroup proposal Proposals for Elektra
 * @brief Might be added to, changed or removed from future Elektra releases.
 */

/**
 * @defgroup api API Proposals for Elektra
 * @brief for kdb.h.
 * @ingroup proposal
 *
 * @warning Do not use these methods if you do not want to depend on
 * exactly the Elektra version your binary was built for.
 *
 * These methods are a technical preview of what might be added in
 * future Elektra releases. It is a requirement that methods are first
 * added here, before they are added to the public API.
 *
 * Usually, names in proposal stage should be prefixed with elektra to
 * clearly mark that the signature is likely to be changed and not yet
 * ABI compatible.
 *
 * @{
 */


/**
 * @brief Set a formatted string
 *
 * @param key the key to set the string value
 * @param format NULL-terminated text format string
 * @param ... more arguments
 *
 * @return the size of the string as set (with including 0)
 */
ssize_t keySetStringF (Key * key, const char * format, ...)
{
	va_list arg_list;

	keySetMeta (key, "binary", 0);

	va_start (arg_list, format);
	char * p = elektraVFormat (format, arg_list);
	va_end (arg_list);

	if (!p)
	{
		return -1;
	}

	if (key->data.c && !test_bit (key->flags, KEY_FLAG_MMAP_DATA))
	{
		elektraFree (key->data.c);
	}

	key->data.c = p;
	key->dataSize = elektraStrLen (key->data.c);
	set_bit (key->flags, KEY_FLAG_SYNC);

	return key->dataSize;
}


/**
 * @brief Takes the first key and cuts off this common part
 * for all other keys, instead name will be prepended
 *
 * @return a new allocated keyset with keys in user namespace.
 *
 * The first key is removed in the resulting keyset.
 */
KeySet * ksRenameKeys (KeySet * config, const Key * name)
{
	return elektraRenameKeys (config, keyName (name));
}

/**
 * @brief Permanently locks a part of the key
 *
 * This can be:
 * - KEY_FLAG_LOCK_NAME to lock the name
 * - KEY_FLAG_LOCK_VALUE to lock the value
 * - KEY_FLAG_LOCK_META to lock the metadata
 *
 * To unlock the key, duplicate it.
 *
 * It is also possible to lock when the key is created with
 * keyNew().
 *
 * Some data structures need to lock the key (most likely
 * its name), so that the ordering does not get confused.
 *
 * @param key which name should be locked
 *
 * @see keyNew(), keyDup(), ksAppendKey()
 * @retval >0 the bits that were successfully locked
 * @retval 0 if everything was locked before
 * @retval -1 if it could not be locked (nullpointer)
 */
int keyLock (Key * key, option_t what)
{
	return elektraKeyLock (key, what);
}


/**
 * @brief Return metadata as keyset
 *
 * @param key the key object to work with
 *
 * @return a duplication of the keyset representing the metadata
 */
KeySet * elektraKeyGetMetaKeySet (const Key * key)
{
	if (!key) return 0;
	if (!key->meta) return 0;

	return ksDup (key->meta);
}


/**
 * @brief Pop key at given cursor position
 *
 * @param ks the keyset to pop key from
 * @param c where to pop
 *
 * The internal cursor will be rewinded using ksRewind(). You can use
 * ksGetCursor() and ksSetCursor() jump back to the previous position.
 * e.g. to pop at current position within ksNext() loop:
 * @code
 * cursor_t c = ksGetCursor(ks);
 * keyDel (ksPopAtCursor(ks, c));
 * ksSetCursor(ks, c);
 * ksPrev(ks); // to have correct key after next ksNext()
 * @endcode
 *
 * @warning do not use, will be superseded by external iterator API
 *
 * @return the popped key
 * @retval 0 if ks is 0
 */
Key * ksPopAtCursor (KeySet * ks, cursor_t pos)
{
	return elektraKsPopAtCursor (ks, pos);
}
