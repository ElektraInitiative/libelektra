#include "spec.h"
#include "kdberrors.h"
#include "kdbglobbing.h"

#include <ctype.h>
#include <kdbhelper.h>
#include <math.h>
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
 * @retval 1 - if the keys match
 * @retval 0 - if the keys do not match
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
 * @param[out] newStr the newly created string with the already replaced character
 * 	       It has to provide enough allocated memory to store the
 * 	       same size as in {@link str}.
 * @param searchFor the character to replace
 * @param c the character to use instead
 */
static void replaceCharacter (const char * str, char * newStr, const char searchFor, const char c)
{
	for (size_t i = 0; i < elektraStrLen (str); i++)
	{
		if (str[i] == searchFor)
		{
			newStr[i] = c;
		}
		else
		{
			newStr[i] = str[i];
		}
	}
}

/**
 * Appends a key to the KeySet with the default value specified in the meta key (meta:/default) in the
 * {@link specKey}.
 *
 * The default key is added to the `default:/` namespace.
 *
 * @param ks the KeySet to append the new default key to
 * @param specKey specification key with meta data of the new default key
 */
static void addDefaultKey (KeySet * ks, Key * specKey)
{
	const Key * defaultMetaKey = keyGetMeta (specKey, "default");
	const char * defaultValue = keyString (defaultMetaKey);

	const char * specKeyName = strchr (keyName (specKey), '/');

	char * formattedKeyName = elektraFormat ("default:/%s", specKeyName);
	Key * newDefaultKey = keyNew (formattedKeyName, KEY_VALUE, defaultValue, KEY_END);
	keyCopyAllMeta (newDefaultKey, specKey);

	ksAppendKey (ks, newDefaultKey);

	elektraFree (formattedKeyName);
}

/**
 * Check if the specification key has a meta key `require` with the value `true`.
 *
 * @param specKey the specification key to check for the require meta key
 * @retval true - if the specification key contains a meta key require
 * @retval false - if the specification key does not contain a meta key require
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
 * @retval true - if the specification key contains a meta key default
 * @retval false - if the specification key does not contain the meta key default
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
 * @retval the extracted specification keys
 */
static KeySet * extractSpecKeys (KeySet * ks)
{
	Key * specKey = keyNew ("spec:/", KEY_END);
	KeySet * ksRet = ksCut (ks, specKey);
	keyDel (specKey);
	return ksRet;
}

/**
 * Checks if the {@link specKey} is an array specification key.
 *
 * @param specKey the specification key to check if it is an array specification
 * @retval true  - if it is an array specification
 * @retval false - if it is no array specification
 */
static bool isArraySpecification (Key * specKey)
{
	return strchr (keyName (specKey), '#') != NULL;
}

/**
 * Checks if this specification key contains an `_` in the array specification.
 *
 * @param specKey the specification key to check for underline if it is array specification
 * @retval true - if the array specification contains an `_`
 * @retval false - if the array specification does not contain an `_`
 */
static bool containsUnderlineInArraySpec (Key * specKey)
{
	return strchr (keyName (specKey), '_') != NULL;
}

/**
 * Checks if this specification key has a wildcard character in the key name.
 *
 * @param specKey the specification to check for if it has a wildcard character
 * @retval true - if the specification key has a wildcard character in the key name
 * @retval false - if the specification does not contain a wildcard character
 */
static bool isWildcardSpecification (Key * specKey)
{
	const char * keyWithoutNamespace = strchr (keyName (specKey), '/');
	for (size_t i = 0; i < elektraStrLen (keyWithoutNamespace); i++)
	{
		if (keyWithoutNamespace[i] == '_')
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
 * @param arrayNumber the array element number
 * @return the created array element allocated with {@code elektraCalloc} e.g. #0, #_10, #__100
 * 	   Make sure to free the memory with {@code elektraFree}.
 */
static char * createArrayElementName (int arrayNumber)
{
	// number of digits in arrayNumber
	int nDigits = arrayNumber == 0 ? 1 : floor (log10 (abs (arrayNumber))) + 1;

	// allocate enough space for #, the digits and \0
	char * name = elektraCalloc ((nDigits + 2) * sizeof (char));

	name[0] = '#';

	for (int j = 1; j < nDigits; j++)
	{
		name[j] = '_';
	}

	sprintf (&name[nDigits], "%d", arrayNumber);

	return name;
}

/**
 * Creating a formatted array key name in the default namespace.
 *
 * @param keyNameWithoutNamespace the key name without the namespace
 * @param arrayNumber the array element number e.g. #1 => array number is 1
 * @param pos the position of the # character
 * @return the formatted array key name in the default namespace
 */
static char * createFormattedArrayKeyNameInDefaultNamespace (char * keyNameWithoutNamespace, int arrayNumber, int pos)
{
	char * strUntilArrayElement = elektraMalloc (pos + 1);
	memcpy (strUntilArrayElement, &keyNameWithoutNamespace[0], pos - 1);
	strUntilArrayElement[pos] = '\0';

	size_t wholeKeyNameSize = elektraStrLen (keyNameWithoutNamespace);

	size_t keyNameSize = elektraStrLen (&keyNameWithoutNamespace[pos + 1]);
	char * strAfterArrayElement = elektraMalloc (keyNameSize);
	memcpy (strAfterArrayElement, &keyNameWithoutNamespace[pos + 1], wholeKeyNameSize);
	strAfterArrayElement[keyNameSize] = '\0';

	char * arrayElementName = createArrayElementName (arrayNumber);

	char * formattedKeyName = elektraFormat ("default:/%s/%s/%s", strUntilArrayElement, arrayElementName, strAfterArrayElement);

	elektraFree (strUntilArrayElement);
	elektraFree (strAfterArrayElement);
	elektraFree (arrayElementName);

	return formattedKeyName;
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

		char * formattedKeyName = createFormattedArrayKeyNameInDefaultNamespace (keyNameWithoutNamespace, i, pos);
		Key * key = keyNew (formattedKeyName, KEY_END);
		keyCopyAllMeta (key, specKey);

		ksAppendKey (instantiatedArraySpecs, key);

		elektraFree (formattedKeyName);
	}

	ksAppend (ks, instantiatedArraySpecs);
}

/**
 * Validate the array size of a specification key.
 *
 * @param key the key to fetch the array size from
 * @param specKey the specification key to validate the array size from
 * @retval true - if the array size is valid
 * @retval false - if the array size is not valid
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
 * @return pointer to the key which caused collision
 * @retval 0 - if no collision was found
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

			char * formattedLookupKeyName = elektraFormat ("spec:/%s", arraySpecKey);
			Key * foundKey = ksLookupByName (specKeys, formattedLookupKeyName, 0);
			elektraFree (arraySpecKey);
			elektraFree (formattedLookupKeyName);
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

			char * formattedLookupKeyName = elektraFormat ("spec:/%s", wildcardSpecKey);
			Key * foundKey = ksLookupByName (specKeys, formattedLookupKeyName, 0);
			elektraFree (wildcardSpecKey);
			elektraFree (formattedLookupKeyName);
			if (foundKey != 0)
			{
				return foundKey;
			}
		}
	}
	return 0;
}

/**
 * Check if the passed key name is located in the {@link ks}. If yes, return the key.
 *
 * @param ks the KeySet to search for the key name
 * @param name the key name to search for
 * @return a pointer to the key in the KeySet which matches the key name of {@link name}
 * @retval key - pointer to the matching key
 * @retval 0 - if no key was found with the given name {@link name}
 */
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
 * @return the key which contains the array size
 * @retval 0 - if the key was not found
 */
static Key * getArraySizeOfArrayParent (KeySet * specKeys, Key * specKey)
{
	char * copiedKeyName = elektraStrDup (keyName (specKey));

	char * rest = NULL;
	char * arrayParent = strtok_r (copiedKeyName, "#", &rest);

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
		if (withoutNamespace[i] == '#')
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
 * @param[out] arrayPositions the array to hold all the positions of the `#` in the specification key
 * @param arraySize number of `#` in the whole specification key
 */
static void setArrayPositions (const char * keyNameWithoutNamespace, int * arrayPositions)
{
	int arrPos = 0;
	for (int i = 0; i < (int) elektraStrLen (keyNameWithoutNamespace); i++)
	{
		if (keyNameWithoutNamespace[i] == '#')
		{
			arrayPositions[arrPos++] = i;
		}
	}
}

/**
 * Validate the array size. If it does not find any array size a warning is appended to the parent key.
 *
 * @param ks the key set to look for the key
 * @param specKeys the specification keys
 * @param specKey the array specification key
 * @retval true - if the array size is valid
 * @retval false - if the array size was not found
 * @retval false - if the array size is not valid
 */
static bool isValidArraySize (KeySet * ks, KeySet * specKeys, Key * specKey)
{
	Key * key = ksLookupByName (ks, strchr (keyName (specKey), '/'), 0);
	Key * keyToFetchArraySizeFrom = key == NULL ? getArraySizeOfArrayParent (specKeys, specKey) : key;
	const Key * arrayMetaKey = keyGetMeta (keyToFetchArraySizeFrom, "array");

	if (arrayMetaKey == 0)
	{
		// no array size found, skip
		return true;
	}

	return validateArraySize (keyToFetchArraySizeFrom, specKey);
}

/**
 * Check if the array is empty.
 *
 * @param ks the KeySet with all keys
 * @param arrayPosition the position of the array character
 * @retval true - if the array is empty
 * @retval false - if the array is not empty
 */
static bool isArrayEmpty (KeySet * ks, int arrayPosition)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		char * withoutNamespace = strchr (keyName (current), '/');

		int len = elektraStrLen (withoutNamespace);
		if (arrayPosition >= len - 1)
		{
			continue;
		}

		if (withoutNamespace[arrayPosition] == '#' &&
		    (arrayPosition < (len - 1) &&
		     (withoutNamespace[arrayPosition + 1] == '_' || withoutNamespace[arrayPosition + 1] != '/' ||
		      isdigit (withoutNamespace[arrayPosition + 1]))))
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
 * adding an error (or warning in the case of `kdbGet`).
 *
 * @param parentKey the parent key (primarily used to handle in case of error, warning, information)
 * @param specKey the specification key containing the meta data to be copied
 * @param specKeys the specification keys in this {@link ks}
 * @param ks the KeySet to search for the key
 * @param isKdbGet
 * @retval 0 - if the meta data was copied successfully
 * @retval -1 - if the metadata could not be copied (error is also added there)
 * @retval -1 - if the key was not found but has meta:/require and no meta:/default in {@link specKey}
 * @retval -1 - if the array specification is not valid
 */
static int copyMetaData (Key * parentKey, Key * specKey, KeySet * specKeys, KeySet * ks, bool isKdbGet)
{
	int found = -1;

	bool isArraySpec = isArraySpecification (specKey);
	if (isArraySpec && !isValidArraySize (ks, specKeys, parentKey))
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
			untilArrayElementAtPositionI[arrayPositions[i] - 1] = '\0';

			Key * arraySizeKeyToInstantiate = getMatchingKeyFromKeySet (specKeys, untilArrayElementAtPositionI);
			const char * arraySizeToInstantiate = keyString (keyGetMeta (arraySizeKeyToInstantiate, "array"));

			if (arraySizeKeyToInstantiate == NULL)
			{
				addDefaultKey (ks, specKey);
				continue;
			}

			if (!isArrayEmpty (ks, arrayPositions[i]))
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
			addDefaultKey (ks, specKey);
			return 0;
		}
		else
		{
			char * msg = elektraFormat ("Key for specification %s does not exist", keyName (specKey));
			char * formattedInfoMessage = elektraFormat ("%s/%s", INFO_KEY, "description");
			keySetMeta (parentKey, formattedInfoMessage, msg);
			elektraFree (msg);
			elektraFree (formattedInfoMessage);
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
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey,
								 "Specification %s has a collision. It seems that there exists an array "
								 "and wildcard specification for the same key.",
								 keyName (collisionKey));
		}
		else
		{
			ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
							       "Specification %s has a collision. It seems that there exists an array and "
							       "wildcard specification for the same key.",
							       keyName (collisionKey));
		}

		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (elektraCursor it = 0; it < ksGetSize (specKeys); it++)
	{
		Key * current = ksAtCursor (specKeys, it);

		// if required and no default => cascade lookup if exists in other namespaces
		if ((isRequired (current) && !hasDefault (current)) || (isRequired (current) && isWildcardSpecification (current)))
		{
			Key * cascadingKey = keyNew (strchr (keyName (current), '/'), KEY_END);
			if (ksLookup (returned, cascadingKey, 0) == 0)
			{
				if (isKdbGet)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Key for specification %s does not exist",
										 keyName (current));
				}
				else
				{
					ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey, "Key for specification %s does not exist",
									       keyName (current));
				}

				keyDel (cascadingKey);

				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			keyDel (cascadingKey);
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
