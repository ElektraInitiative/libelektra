#include "spec-new.h"
#include "../../../tests/cframework/tests.h"
#include "kdberrors.h"
#include "kdbglobbing.h"

#include <kdbhelper.h>
#include <stdio.h>
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

/**
 * Replace the {@link searchFor} with {@link c} in the {@link str}.
 * The {@link newStr} contains the string with the replaced character.
 *
 * @param str the old string
 * @param newStr the newly created string with the already replaced character
 * @param searchFor the character to replace
 * @param c the character to use instead
 */
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
static void addDefaultKey (KeySet * ks, Key * parentKey, Key * specKey)
{
	const Key * defaultMetaKey = keyGetMeta (specKey, "default");
	const char * defaultValue = keyString (defaultMetaKey);

	const char * parentKeyName = strchr (keyName (parentKey), '/');
	const char * specKeyName = keyBaseName (specKey);

	Key * newDefaultKey = keyNew (elektraFormat ("default:/%s/%s", parentKeyName, specKeyName), KEY_VALUE, defaultValue, KEY_END);
	keyCopyAllMeta (newDefaultKey, specKey);

	ksAppendKey (ks, newDefaultKey);
}

/**
 * Check if the specification key has a meta key `required`.
 *
 * @param specKey the specification key to check for the required meta key
 * @return true - if the specification key contains a meta key required
 * 	   false - if the specification key does not contain a meta key required
 */
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

/**
 * Check if the specification key has a meta key `default`.
 *
 * @param specKey the specification key to check for the default meta key
 * @return true - if the specification key contains a meta key default
 * 	   false - if the specification key does not contain the meta key default
 */
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
 * Checks if this specification key contains an `_` in the array specification.
 *
 * @param specKey the specification key to check for underline if it is array specification
 * @return true - if the array specification contains an `_`
 * 	   false - if the array specification does not contain an `_`
 */
static bool containsUnderlineInArraySpec (Key * specKey)
{
	const char * keyWithoutNamespace = strchr (keyName (specKey), '/');
	size_t len = elektraStrLen (keyWithoutNamespace);

	for (size_t i = 0; i < len; i++)
	{
		if (keyWithoutNamespace [i] == '#' && (i != len && keyWithoutNamespace [i + 1] == '_'))
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
 * Creates the name of the array element.
 *
 * Example:
 * 	arrayNumber = 2
 * 	size = 2
 *
 * 	arrayElement = #2
 *
 * 	arrayNumber = 10
 * 	size = 4
 *
 * 	arrayElement = #_10
 *
 * 	arrayNumber = 100
 * 	size = 6
 *
 * 	arrayElement = #__100
 *
 * @param arrayElement the element to store the name in
 * @param arrayNumber the array element number
 * @param size the size of array
 */
static void createArrayElementName (char * arrayElement, int arrayNumber, int size)
{
	arrayElement [0] = '#';
	for (int j = 1; j < (arrayNumber % 10); j++)
	{
		arrayElement [j] = '_';
	}
	sprintf (&arrayElement [size], "%d", arrayNumber);
}

/**
 * Creates the corresponding array element keys and copies all the meta data keys from {@link specKey}.
 *
 * @param specKey the specification key to copy the meta data from
 * @param ks the KeySet to append the newly created array element keys too
 * @param arraySize number of array elements to create under the key
 * @param pos the position of the array element (`#`) to start instantiating at
 */
static void instantiateArraySpecificationAndCopyMeta (Key * specKey, KeySet * ks, int arraySize, int pos)
{
	KeySet * instantiatedArraySpecs = ksNew (arraySize, KS_END);
	for (int i = 0; i < arraySize; i++)
	{
		char * keyNameWithoutNamespace = strchr (keyName (specKey), '/');

		char * strUntilArrayElement = elektraMalloc (pos);
		memcpy (strUntilArrayElement, &keyNameWithoutNamespace [0], pos - 1);
		strUntilArrayElement [pos] = '\0';

		size_t keyNameSize = elektraStrLen (keyNameWithoutNamespace);
		char * strAfterArrayElement = elektraMalloc (keyNameSize + 1);
		memcpy (strAfterArrayElement, &keyNameWithoutNamespace [pos], keyNameSize);
		strAfterArrayElement [keyNameSize + 1] = '\0';

		char * arrayElementName = elektraMalloc (1 + (i % 10) + i);
		createArrayElementName (arrayElementName, i, 1 + (i % 10) + i);

		Key * key = keyNew (elektraFormat ("default:/%s/%s/%s", strUntilArrayElement, arrayElementName,
							   strAfterArrayElement), KEY_END);
		keyCopyAllMeta (key, specKey);

		ksAppendKey (instantiatedArraySpecs, key);

		elektraFree (strUntilArrayElement);
		elektraFree (strAfterArrayElement);
		elektraFree (arrayElementName);
	}

	ksAppend (ks, instantiatedArraySpecs);
}

/**
 * Validate the array size of a specification key.
 *
 * @param key the key to fetch the array size from
 * @param specKey the specification key to validate the array size from
 * @return true - if the array size is valid
 * 	   false - if the array size is not valid
 */
static bool validateArraySize (Key * key, Key * specKey)
{
	const Key * arrayMetaKey = keyGetMeta (key, "array");
	const Key * arrayMinSizeKey = keyGetMeta (specKey, "array/min");
	const Key * arrayMaxSizeKey = keyGetMeta (specKey, "array/max");

	const char * arraySize = keyString (arrayMetaKey);
	const char * minSize = keyString (arrayMinSizeKey);
	const char * maxSize = keyString (arrayMaxSizeKey);

	return (arrayMinSizeKey == 0 || elektraStrCmp (minSize, arraySize) < 0) &&
	       (arrayMaxSizeKey == 0 || elektraStrCmp (maxSize, arraySize) > 0);
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

static Key * getMatchingKeyFromKeySet (KeySet * ks, char * name)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		if (elektraStrCmp (strchr (keyName (current), '/'), name) == 0)
		{
			return current;
		}
	}
	return 0;
}

/**
 * Get the key which contains the array size as meta key.
 *
 * @param specKeys the specification keys
 * @param specKey the specification key to use for lookup
 * @return 0 - if the key was not found
 * 	   key - the key which contains the array size
 */
static Key * getArraySizeOfArrayParent (KeySet * specKeys, Key * specKey)
{
	const char * specKeyName = keyName (specKey);
	char * copiedKeyName = elektraMalloc (elektraStrLen (specKeyName) + 1);
	strcpy (copiedKeyName, (char *) specKeyName);

	char * arrayParent = strtok (copiedKeyName, "#");
	arrayParent [strlen (arrayParent) - 1] = '\0';

	Key * key = getMatchingKeyFromKeySet (specKeys, strchr (arrayParent, '/'));

	elektraFree (copiedKeyName);

	return key;
}

/**
 * Get the number of array characters in an array specification key name.
 *
 * @param specKey the specification key to use
 * @return number of array characters in the array specification key name
 */
static int getNumberOfArrayCharactersInSpecName (Key * specKey)
{
	char * withoutNamespace = strchr (keyName (specKey), '/');

	int count = 0;

	for (size_t i = 0; i < elektraStrLen (withoutNamespace); i++)
	{
		if (withoutNamespace [i] == '#')
		{
			count++;
		}
	}

	return count;
}

/**
 * The positions of where the `#` occurs in the specification key.
 *
 * @param keyNameWithoutNamespace the key name without the namespace
 * @param arrayPositions the array to hold all the positions of the `#` in the specification key
 * @param arraySize number of `#` in the whole specification key
 */
static void setArrayPositions (const char * keyNameWithoutNamespace, int * arrayPositions)
{
	int arrPos = 0;
	for (size_t i = 0; i < elektraStrLen (keyNameWithoutNamespace); i++)
	{
		if (keyNameWithoutNamespace [i] == '#')
		{
			arrayPositions [arrPos++] = (int) i;
		}
	}
}

/**
 * Validate the array size. If it does not find any array size a warning is appended to the parent key.
 *
 * @param ks the key set to look for the key
 * @param specKeys the specification keys
 * @param parentKey the parent key to be used for appending warning, error, information
 * @param specKey the array specification key
 * @return true - if the array size is valid
 *         false - if the array size was not found
 *               - if the array size is not valid
 */
static bool isValidArraySize (KeySet * ks, KeySet * specKeys, Key * parentKey, Key * specKey)
{
	Key * key = ksLookupByName (ks, strchr (keyName (specKey), '/'), 0);
	Key * keyToFetchArraySizeFrom = key == NULL ? getArraySizeOfArrayParent (specKeys, specKey) : key;
	const Key * arrayMetaKey = key == NULL ? keyGetMeta(keyToFetchArraySizeFrom, "array") : keyGetMeta (key, "array");

	if (arrayMetaKey == 0)
	{
		// no array size found, skip
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Could not find array size for key %s",
							   keyName (specKey));
		return false;
	}

	if (!validateArraySize (keyToFetchArraySizeFrom, specKey))
	{
		return false;
	}

	return true;
}

/**
 * Check if the array is empty.
 *
 * @param ks the KeySet with all keys
 * @param arrayPosition the position of the array character
 * @return true - if the array is empty
 * 	   false - if the array is not empty
 */
static bool isArrayEmpty (KeySet * ks, int arrayPosition)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		char * withoutNamespace = strchr (keyName (current), '/');

		size_t len = elektraStrLen (withoutNamespace);
		if (withoutNamespace [arrayPosition] == '#' && ((int) len != arrayPosition + 1 && (withoutNamespace [arrayPosition + 1] == '_' || withoutNamespace [arrayPosition + 1] != '/')))
		{
			return false;
		}
	}

	return true;
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
 * @param specKeys the specification keys in this {@link ks}
 * @param ks the KeySet to search for the key
 * @param isKdbGet
 * @return 0 - if the meta data was copied successfully
 * 	  -1 - if the metadata could not be copied (error is also added there)
 * 	       if the key was not found but has meta:/require and no meta:/default in {@link specKey}
 * 	       if the array specification is not valid
 */
static int copyMetaData (Key * parentKey, Key * specKey, KeySet * specKeys, KeySet * ks, bool isKdbGet)
{
	int found = -1;

	bool isArraySpec = isArraySpecification (specKey);
	if (isArraySpec && !isValidArraySize (ks, specKeys, parentKey, specKey))
	{
		return -1;
	}

	// in case array spec does not look like #_10, #__100
	// this will instantiate array keys and add to default:/ if they contain a default value
	if (isArraySpec && !containsUnderlineInArraySpec (specKey) && hasDefault (specKey))
	{
		int num = getNumberOfArrayCharactersInSpecName (specKey);

		int * arrayPositions = elektraMalloc (num);
		setArrayPositions (strchr (keyName (specKey), '/'), arrayPositions);

		char * keyNameWithoutNamespace = strchr (keyName (specKey), '/');

		for (int i = 0; i < num; i++)
		{
			char * untilArrayElementAtPositionI = elektraMalloc (arrayPositions[i]);
			memcpy (untilArrayElementAtPositionI, &keyNameWithoutNamespace[0], arrayPositions[i]);
			untilArrayElementAtPositionI [arrayPositions [i] - 1] = '\0';

			Key * arraySizeKeyToInstantiate = getMatchingKeyFromKeySet (specKeys, untilArrayElementAtPositionI);
			const char * arraySizeToInstantiate = keyString (keyGetMeta (arraySizeKeyToInstantiate, "array"));

			if (!isArrayEmpty (ks, arrayPositions [i]))
			{
				continue;
			}

			char * end;
			int size = strtol (arraySizeToInstantiate, &end, 10);
			instantiateArraySpecificationAndCopyMeta (specKey, ks, size, arrayPositions[i]);

			elektraFree (untilArrayElementAtPositionI);
		}

		elektraFree (arrayPositions);

		return 0;
	}

	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);

		if (specMatches (specKey, current))
		{
			found = 0;
			if (keyCopyAllMeta (current, specKey) < 0)
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
			addDefaultKey (ks, parentKey, specKey);
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

int elektraSpecCopy (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet)
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

		if (copyMetaData (parentKey, current, specKeys, returned, isKdbGet) != 0)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksAppend (returned, specKeys);

	ksDel (specKeys);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecRemove (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, ELEKTRA_UNUSED Key * parentKey)
{
	Key * specName = keyNew ("spec:/", KEY_END);

	for (elektraCursor i = 0; i < ksGetSize (returned); i++)
	{
		Key * current = ksAtCursor (returned, i);

		if (keyGetNamespace (current) == KEY_NS_SPEC)
		{
			continue;
		}

		// Find out if there is a spec:/ key for the current key
		keySetName (specName, "spec:/");
		keyAddName (specName, strchr (keyName (current), '/'));
		Key * specKey = ksLookup (returned, specName, 0);

		if (specKey != NULL)
		{
			KeySet * specMeta = keyMeta (specKey);
			KeySet * meta = keyMeta (current);

			for (elektraCursor it = 0; it < ksGetSize (specMeta); it++)
			{
				Key * m = ksAtCursor (specMeta, it);
				if (ksLookup (meta, m, 0) == m)
				{
					keySetMeta (current, keyName (m), NULL);
				}
			}
		}
	}

	keyDel (specName);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecGet (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/spec"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/spec", KEY_VALUE, "spec plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports", KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/get", KEY_FUNC, elektraSpecGet, KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/hook/spec/copy", KEY_FUNC, elektraSpecCopy, KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/hook/spec/remove", KEY_FUNC, elektraSpecRemove, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/spec/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_END);
}
