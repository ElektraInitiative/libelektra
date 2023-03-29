#include "spec-new.h"
#include "kdberrors.h"
#include "kdbglobbing.h"

#include <kdbhelper.h>
#include <printf.h>
#include <stdlib.h>

#ifdef __MINGW32__
static bool specMatches (Key * specKey, Key * otherKey)
{
	/**
	 * Known limitation: For MINGW builds fnmatch.h does not exist. Therefore, globbing can't be used.
	 * This means that there is no support for # and _ in key names.
	 * This function was copied from 68e9dff, doesn't use globbing and therefore doesn't require the globbing library which is not
	 * compatible with Windows:
	 */
	const char * spec = keyUnescapedName (specKey);
	size_t specNsLen = strlen (spec) + 1;
	spec += specNsLen; // skip namespace
	const char * other = keyUnescapedName (otherKey);
	size_t otherNsLen = strlen (other) + 1;
	other += otherNsLen; // skip namespace
	size_t const specSize = keyGetUnescapedNameSize (specKey) - specNsLen;
	size_t const otherSize = keyGetUnescapedNameSize (otherKey) - otherNsLen;

	return specSize == otherSize && memcmp (spec, other, specSize) == 0;
}
#else
/**
 * Check whether the {@link otherKey} matches the {@link specKey}.
 *
 * @param specKey specification key
 * @param otherKey the other key to match the specification key with
 * @return 1 - if the keys match
 * 	   0 - if the keys do not match
 */
static bool specMatches (Key * specKey, Key * otherKey)
{
	// ignore namespaces for globbing
	Key * globKey = keyNew (strchr (keyName (otherKey), '/'), KEY_END);
	bool matches = elektraKeyGlob (globKey, strchr (keyName (specKey), '/')) == 0;
	keyDel (globKey);
	return matches;
}
#endif

static void replaceCharacter (const char * str, char * newStr, const char searchFor, const char c)
{
	for (size_t i = 0; i < elektraStrLen (str); i++)
	{
		if (str [i] == searchFor)
		{
			newStr [i] = c;
		}
		else
		{
			newStr [i] = str [i];
		}
	}
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
	const Key * defaultMetaKey = keyGetMeta (specKey, "default");
	const char * defaultValue = keyString (defaultMetaKey);

	const char * parentKeyName = strchr (keyName (parentKey), '/');
	const char * specKeyName = keyBaseName (specKey);

	Key * newDefaultKey = keyNew (elektraFormat ("%s:/%s/%s", "default", parentKeyName, specKeyName), KEY_VALUE, defaultValue, KEY_END);
	keyCopyAllMeta (newDefaultKey, specKey);

	ksAppendKey (ks, newDefaultKey);
}

static bool isRequired (Key * specKey)
{
	const Key * key = keyGetMeta (specKey, "require");

	if (key == 0)
	{
		return false;
	}

	const char * keyValue = keyString (key);

	return elektraStrCmp (keyValue, "true") == 0;
}

static bool hasDefault (Key * specKey)
{
	return keyGetMeta (specKey, "default") != 0;
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
 * Checks if the {@link specKey} is a array specification key.
 *
 * @param specKey the specification key to check if it is an array specification
 * @return true  - if it is an array specification
 * 	   false - if it is no array specification
 */
static bool isArraySpecification (Key * specKey)
{
	const char * keyWithoutNamespace = strchr (keyName (specKey), '/');
	for (size_t i = 0; i < elektraStrLen (keyWithoutNamespace); i++)
	{
		if (keyWithoutNamespace [i] == '#')
		{
			return true;
		}
	}

	return false;
}

/**
 * Checks if this specification key has a wildcard character in the key name.
 *
 * @param specKey the specification to check for if it has a wildcard character
 * @return true - if the specification key has a wildcard character in the key name
 * 	   false - if the specification does not contain a wildcard character
 */
static bool isWildcardSpecification (Key * specKey)
{
	const char * keyWithoutNamespace = strchr (keyName (specKey), '/');
	for (size_t i = 0; i < elektraStrLen (keyWithoutNamespace); i++)
	{
		if (keyWithoutNamespace [i] == '_')
		{
			return true;
		}
	}

	return false;
}

/**
 * Checks if the specification has a collision. A collision is when two specification keys exist, one as wildcard specification,
 * the other as array specification and it is not clear in this case what the correct specification is.
 *
 * Example:
 * 	spec:/server/_/name => meta:/description = value1
 * 	spec:/server/#/name => meta:/description = value2
 *
 * @param specKeys specification keys to check for collision
 * @return 0 - if no collision was found
 * 	   pointer to the key which caused collision
 */
static Key * specCollision (KeySet * specKeys)
{
	for (elektraCursor it = 0; it < ksGetSize (specKeys); it++)
	{
		Key * current = ksAtCursor (specKeys, it);

		if (isWildcardSpecification (current))
		{
			const char * wildcardSpec = strchr (keyName (current), '/');

			char * arraySpecKey = elektraMalloc (elektraStrLen (wildcardSpec));
			replaceCharacter (wildcardSpec, arraySpecKey, '_', '#');

			Key * foundKey = ksLookupByName(specKeys, elektraFormat ("spec:/%s", arraySpecKey), 0);
			elektraFree (arraySpecKey);
			if (foundKey != 0)
			{
				return foundKey;
			}
		}
		else if (isArraySpecification (current))
		{
			const char * arraySpec = strchr (keyName (current), '/');

			char * wildcardSpecKey = elektraMalloc (elektraStrLen (arraySpec));
			replaceCharacter (arraySpec, wildcardSpecKey, '#', '_');

			Key * foundKey = ksLookupByName (specKeys, elektraFormat ("spec:/%s", wildcardSpecKey), 0);
			elektraFree (wildcardSpecKey);
			if (foundKey != 0)
			{
				return foundKey;
			}
		}
	}
	return 0;
}

/**
 * Copies all meta keys from the {@link specKey} to the provided {@link key}.
 *
 * @param key the key to copy the meta data too
 * @param specKey the specification key to copy meta data from
 *
 * @return 0 - in case the copying was successful
 * 	  -1 - if the copying was unsuccessful
 */
static int copyMeta (Key * key, Key * specKey)
{
	KeySet * metaKeys = keyMeta (specKey);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); it++)
	{
		Key * current = ksAtCursor (metaKeys, it);

		if (!keyCopyMeta (key, specKey, keyName (current)))
		{
			return -1;
		}
	}

	return 0;
}

/**
 * Copy the meta data for a given key {@link specKey} by searching through the {@link ks} KeySet.
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
 * @param isKdbGet
 * @return 0 - if the meta data was copied successfully
 * 	  -1 - if the metadata could not be copied (error is also added there)
 * 	       if the key was not found but has meta:/require and no meta:/default in {@link specKey}
 */
static int copyMetaData (Key * parentKey, Key * specKey, KeySet * ks, bool isKdbGet)
{
	int found = -1;

	if (isArraySpecification(specKey))
	{
		Key * key = ksLookupByName (ks, strchr (keyName (specKey), '/'), 0);
		const Key * arrayMetaKey = keyGetMeta (key, "array");

		if (arrayMetaKey == 0 || keyGetNamespace (key) == KEY_NS_SPEC)
		{
			// no array size found, skip
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Could not find array size for key %s",
							       keyName (specKey));
			return -1;
		}
	}

	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);

		if (specMatches (specKey, current))
		{
			found = 0;
			if (copyMeta (current, specKey) != 0)
			{
				if (isKdbGet)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Could not copy metadata from spec key %s",
										 keyName (specKey));
				}
				else
				{
					ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey, "Could not copy metadata from spec key %s",
									       keyName (specKey));
				}

				return -1;
			}
		}
	}

	// key was not found
	if (found == -1)
	{
		if (hasDefault (specKey))
		{
			addDefaultKeyIfNotExists (ks, parentKey, specKey);
			return 0;
		}
		else
		{
			const char * msg = elektraFormat ("Key for specification %s does not exist", keyName (specKey));
			keySetMeta (parentKey, elektraFormat ("%s/%s", INFO_KEY, "description"), msg);
			return 0;
		}
	}

	return 0;
}

int elektraSpecCopy (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey, ELEKTRA_UNUSED bool isKdbGet)
{
	KeySet * specKeys = extractSpecKeys (returned);

	Key * collisionKey = specCollision (specKeys);
	if (collisionKey != 0)
	{
		if (isKdbGet)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Specification %s has a collision. It seems that there exists an array and wildcard specification for the same key.", keyName (collisionKey));
		}
		else
		{
			ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey, "Specification %s has a collision. It seems that there exists an array and wildcard specification for the same key.", keyName (collisionKey));
		}

		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

 	for (elektraCursor it = 0; it < ksGetSize (specKeys); it++)
	{
		Key * current = ksAtCursor (specKeys, it);

		// if required and no default => cascade lookup if exists in other namespaces
		if ((isRequired (current) && !hasDefault (current)) ||
		    (isRequired (current) && isWildcardSpecification (current)))
		{
			Key * cascadingKey = keyNew (strchr (keyName (current), '/'), KEY_END);
			if (ksLookup (returned, cascadingKey, 0) == 0)
			{
				if (isKdbGet)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Key for specification %s does not exist", keyName (current));
				}
				else
				{
					ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey, "Key for specification %s does not exist", keyName (current));
				}
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		if (copyMetaData (parentKey, current, returned, isKdbGet) != 0)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksAppend (returned, specKeys);

	ksDel (specKeys);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
