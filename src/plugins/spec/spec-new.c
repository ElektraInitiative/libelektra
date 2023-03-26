#include "spec-new.h"
#include "kdbglobbing.h"

#include <kdbhelper.h>

/**
 * Check whether the {@link otherKey} matches the {@link specKey}.
 *
 * @param specKey specification key
 * @param otherKey the other key to match the specification key with
 * @return 0 - if the keys match
 * 	  -1 - if the keys do not match
 */
static bool specMatches (Key * specKey, Key * otherKey)
{
	// ignore namespaces for globbing
	Key * globKey = keyNew (strchr (keyName (otherKey), '/'), KEY_END);
	bool matches = elektraKeyGlob (globKey, strchr (keyName (specKey), '/')) == 0;
	keyDel (globKey);
	return matches;
}

/**
 * In case of an error, warning, information it handles it by appended the meta key
 * to the parent key.
 *
 * @param parentKey the parent key to append the meta key too
 * @param msg the value of the meta key type
 */
static void handle (Key * parentKey, const char * type, const char * msg)
{
	keySetMeta (parentKey, type, msg);
}

/**
 * Appends a key to the KeySet with the default value specified in the meta key (meta:/default) in the
 * {@link specKey}.
 *
 * The default key is added to the `default:/` namespace.
 *
 * @param ks the key store to append the new default key to
 * @param parentKey of the key to appended
 * @param specKey specification key with meta data of the new default key
 */
static void addDefaultKeyIfNotExists (KeySet * ks, Key * parentKey, Key * specKey)
{
	const Key * defaultMetaKey = keyGetMeta (specKey, "meta:/default");
	const char * defaultValue = keyString (defaultMetaKey);

	Key * newDefaultKey = keyNew (keyName(parentKey), keyName (specKey), KEY_VALUE, defaultValue, KEY_END);
	keyCopyAllMeta (newDefaultKey, specKey);

	ksAppendKey (ks, newDefaultKey);
}

static bool isRequired (Key * specKey)
{
	if (keyGetMeta (specKey, "meta:/require") == 0)
	{
		return false;
	}
	return true;
}

static bool hasDefault (Key * specKey)
{
	if (keyGetMeta (specKey, "meta:/default") == 0)
	{
		return false;
	}
	return true;
}

/**
 * Extracts all the specification keys from the KeySet. After applying this method the {@link ks}
 * contains no specification namespace.
 *
 * It returns the extracted specification keys.
 *
 * @param ks the KeySet to extract specification keys from
 *
 * @return the extracted specification keys
 */
static KeySet * extractSpecKeys (KeySet * ks)
{
	KeySet * specKeys = ksNew (0, KS_END);
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		if (keyGetNamespace (current) == KEY_NS_SPEC)
		{
			ksAppendKey (specKeys, current);
		}
	}

	Key * specKey = keyNew ("spec:/", KEY_END);
	ksDel (ksCut (ks, specKey));
	keyDel (specKey);

	return specKeys;
}

/**
 * Copies all meta keys from the {@link specKey} to the provided {@link key}.
 *
 * @param key
 * @param specKey
 *
 * @return 0 - in case the copying was successful
 * 	  -1 - if the copying was unsuccessful
 */
static int copyMeta (Key * key, Key * specKey)
{
	KeySet * metaKeys = ksDup (keyMeta (specKey));

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); it++)
	{
		Key * current = ksAtCursor (metaKeys, it);

		if (!keyCopyMeta (key, specKey, keyName (current)))
		{
			return -1;
		}
	}

	ksDel (metaKeys);

	return 0;
}

/**
 * Copy the meta data by searching through the {@link ks} KeySet.
 *
 * In case no key was found for {@link specKey} and it has meta key default (meta:/default) it will
 * be created.
 *
 * In case the key is missing and has no meta key default the method returns -1 and it fails by
 * adding error.
 *
 * @param parentKey the parent key (primarily used to handle in case of error, warning, information)
 * @param specKey the specification key containing the meta data to be copied
 * @param ks the KeySet to search for the key
 * @return 0 - if the meta data was copied successfully
 * 	  -1 - if the metadata could not be copied (error is also added there)
 * 	     - if the key was not found but has meta:/require and no meta:/default in {@link specKey}
 */
static int copyMetaData (Key * parentKey, Key * specKey, KeySet * ks)
{
	int found = -1;

	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);

		if (specMatches(specKey, current))
		{
			if (found == -1)
			{
				found = 0;
			}

			if (copyMeta (current, specKey) != 0)
			{
				handle (parentKey, ERROR_KEY, "Could not copy metadata");
				return -1;
			}
		}
	}

	// key was not found
	if (found == -1)
	{
		// TODO concat string for error and info message

		if (isRequired(specKey) && hasDefault (specKey))
		{
			addDefaultKeyIfNotExists (ks, parentKey, specKey);
			return 0;
		}
		else if (isRequired (specKey))
		{
			handle (parentKey, ERROR_KEY, "Key for specification %s does not exist");
			return -1;
		}
		else
		{
			handle (parentKey, INFO_KEY, "Key for specification %s does not exist");
			return 0;
		}
	}

	return 0;
}

int elektraSpecCopy (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey, ELEKTRA_UNUSED bool isKdbGet)
{
	KeySet * specKeys = extractSpecKeys (returned);

 	for (elektraCursor it = 0; it < ksGetSize (specKeys); it++)
	{
		Key * current = ksAtCursor (specKeys, it);
		if (copyMetaData (parentKey, current, returned) != 0)
		{
			return ELEKTRA_PLUGIN_ERROR;
		}
	}

	ksAppend (returned, specKeys);

	ksDel (specKeys);

	return 1;
}
