/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <string.h>

#include <kdbprivate.h>

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

	if (key->data.c)
	{
		elektraFree (key->data.c);
	}

	key->data.c = p;
	key->dataSize = elektraStrLen (key->data.c);
	set_bit (key->flags, KEY_FLAG_SYNC);

	return key->dataSize;
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

	cursor_t cursor = ksGetCursor (ks);
	ksRewind (ks);
	size_t idx = 0;

	Key * key;
	while ((key = ksNext (ks)) != 0)
	{
		buffer[idx] = key;
		++idx;
	}
	ksSetCursor (ks, cursor);

	return idx;
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
 * - KEY_FLAG_LOCK_META to lock the meta data
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
 * @brief Return meta data as keyset
 *
 * @param key the key object to work with
 *
 * @return a duplication of the keyset representing the meta data
 */
KeySet * elektraKeyGetMetaKeySet (const Key * key)
{
	if (!key) return 0;
	if (!key->meta) return 0;

	return ksDup (key->meta);
}


/**
 * Returns the previous Key in a KeySet.
 *
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksPrev() is called the cursor is decremented and the new current Key
 * is returned.
 *
 * You'll get a NULL pointer if the key before begin of the KeySet was reached.
 *
 * Don't delete the key, use ksPop() if you want to delete it.
 *
 * @return the new current Key
 * @see ksRewind(), ksCurrent()
 *
 */
Key * ksPrev (KeySet * ks)
{
	return elektraKsPrev (ks);
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


/**
 * keyRel replacement
 */

// keyRel2 helper, turns key into a cascading key ( removes namespace)
Key * keyAsCascading (const Key * key)
{
	if (keyName (key)[0] == '/')
	{
		return keyDup (key);
	}
	else
	{
		const char * name = keyName (key);
		const char * ptr = strchr (name, '/');
		if (!ptr)
		{
			return keyNew ("/", KEY_CASCADING_NAME, KEY_END);
		}
		else
		{
			ssize_t length = keyGetNameSize (key);
			if ((ptr - name) == (length - 1))
			{
				return keyNew ("/", KEY_CASCADING_NAME, KEY_END);
			}
			else
			{
				return keyNew (ptr, KEY_CASCADING_NAME, KEY_END);
			}
		}
	}
}

// keyRel2 helper, returns how many levels check is below key, or 0 if check isn't below
int keyGetLevelsBelow (const Key * key, const Key * check)
{
	if (!keyIsBelow (key, check)) return 0;
	if (keyGetNamespace (key) != keyGetNamespace (check)) return 0;
	Key * toCheck = keyDup (check);
	int levels = 0;
	while (strcmp (keyName (key), keyName (toCheck)))
	{
		keySetBaseName (toCheck, 0);
		if (keyName (toCheck)[0] == '\0') keySetName (toCheck, "/");
		++levels;
	}
	keyDel (toCheck);
	return levels;
}

/**
 * @brief Replacement proposal for keyRel
 * @return depending on relation type
 * @retval -1 usage error
 * @retval 0 test failed
 * @retval >1 true for binary tests, number of levels below for other relation tests
 *
 * @param key the key object to work with
 * @param check the second key object to check the relation with
 * @param which what kind of relationship test should be done
 */


int keyRel2 (const Key * key, const Key * check, KeyRelType which)
{
	if (!key || !check) return -1;
	if (!key->key || !check->key) return -1;

	Key * cKey = keyAsCascading (key);
	Key * cCheck = keyAsCascading (check);
	Key * cKeyParent = keyDup (cKey);
	keySetBaseName (cKeyParent, 0);
	if (keyName (cKeyParent)[0] == '\0') keySetName (cKeyParent, "/");
	int isBelow = 0;
	int isSilblingNephew = 0;
	isBelow = keyGetLevelsBelow (cKey, cCheck);
	if (!isBelow) isSilblingNephew = keyGetLevelsBelow (cKeyParent, cCheck);
	elektraNamespace keyNamespace = keyGetNamespace (key);
	elektraNamespace checkNamespace = keyGetNamespace (check);
	int retVal = 0;
	int bits = 0;
	for (KeyRelType type = 1; type != 0; type <<= 1)
	{
		if (type & which) ++bits;
	}
	if (bits != 1) return -1;
	switch (which)
	{
	case ELEKTRA_REL_BELOW_SAME_NS:
		if (isBelow && (keyNamespace == checkNamespace)) retVal = isBelow;
		break;
	case ELEKTRA_REL_BELOW_IGNORE_NS:
		if (isBelow) retVal = isBelow;
		break;
	case ELEKTRA_REL_BELOW_CASCADING_NS:
		if (isBelow && ((checkNamespace == KEY_NS_CASCADING) || (keyNamespace == KEY_NS_CASCADING))) retVal = isBelow;
		break;
	case ELEKTRA_REL_DIRECT_BELOW_SAME_NS:
		if ((isBelow == 1) && (keyNamespace == checkNamespace)) retVal = 1;
		break;
	case ELEKTRA_REL_DIRECT_BELOW_IGNORE_NS:
		if (isBelow == 1) retVal = 1;
		break;
	case ELEKTRA_REL_DIRECT_BELOW_CASCADING_NS:
		if ((isBelow == 1) && ((checkNamespace == KEY_NS_CASCADING) || (keyNamespace == KEY_NS_CASCADING))) retVal = 1;
		break;
	case ELEKTRA_REL_SILBLING_SAME_NS:
		if ((isSilblingNephew == 1) && (keyNamespace == checkNamespace)) retVal = 1;
		break;
	case ELEKTRA_REL_SILBLING_IGNORE_NS:
		if (isSilblingNephew == 1) retVal = 1;
		break;
	case ELEKTRA_REL_SILBLING_CASCADING_NS:
		if ((isSilblingNephew == 1) && ((checkNamespace == KEY_NS_CASCADING) || (keyNamespace == KEY_NS_CASCADING))) retVal = 1;
		break;
	case ELEKTRA_REL_NEPHEW_SAME_NS:
		if ((isSilblingNephew > 1) && (keyNamespace == checkNamespace)) retVal = isSilblingNephew - 1;
		break;
	case ELEKTRA_REL_NEPHEW_IGNORE_NS:
		if (isSilblingNephew > 1) retVal = isSilblingNephew - 1;
		break;
	case ELEKTRA_REL_NEPHEW_CASCADING_NS:
		if ((isSilblingNephew > 1) && ((checkNamespace == KEY_NS_CASCADING) || (keyNamespace == KEY_NS_CASCADING)))
			retVal = isSilblingNephew - 1;
		break;
	default:
		retVal = -1;
		break;
	}
	keyDel (cKey);
	keyDel (cCheck);
	keyDel (cKeyParent);
	return retVal;
}


/**
 * @}
 */
