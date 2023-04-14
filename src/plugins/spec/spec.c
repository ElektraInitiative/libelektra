#include "spec.h"
#include "arrayspec.h"
#include "kdberrors.h"
#include "matching.h"

#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>

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
 * @param isArraySpec boolean value indicating whether this is an array specification key
 */
static void addDefaultKey (KeySet * ks, Key * specKey, bool isArraySpec)
{
	const Key * defaultMetaKey = keyGetMeta (specKey, "default");
	const char * defaultValue = keyString (defaultMetaKey);

	if (elektraStrCmp (defaultValue, "(null)") == 0)
	{
		return;
	}

	const char * specKeyName = strchr (keyName (specKey), '/');

	char * formattedKeyName = elektraFormat (isArraySpec && specKeyName[elektraStrLen (specKeyName) - 2] == '#' ?
							 (specKeyName[0] == '/' ? "default:%s0" : "default:/%s0") :
							 (specKeyName[0] == '/' ? "default:%s" : "default:/%s"),
						 specKeyName);

	if (containsArraySpecElementWithNoDigitOrUnderlineAfterwards (formattedKeyName))
	{
		elektraFree (formattedKeyName);
		return;
	}

	Key * newDefaultKey = keyNew (formattedKeyName, KEY_VALUE, defaultValue, KEY_END);
	keyCopyAllMeta (newDefaultKey, specKey);

	ksAppendKey (ks, newDefaultKey);

	elektraFree (formattedKeyName);

	keyDel (newDefaultKey);
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
 * @retval NULL - if the {@link ks} is NULL
 */
static KeySet * extractSpecKeys (KeySet * ks)
{
	if (ks == NULL)
	{
		return NULL;
	}

	Key * specKey = keyNew ("spec:/", KEY_END);
	KeySet * ksRet = ksCut (ks, specKey);
	keyDel (specKey);

	return ksRet;
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

		if (num == 0)
		{
			return 0;
		}

		int * arrayPositions = elektraMalloc (num * sizeof (int));
		if (arrayPositions == NULL)
		{
			return 0;
		}

		setArrayPositions (strchr (keyName (specKey), '/'), arrayPositions, num);

		char * keyNameWithoutNamespace = strchr (keyName (specKey), '/');

		if (keyNameWithoutNamespace == NULL)
		{
			elektraFree (arrayPositions);
			return 0;
		}

		for (int i = 0; i < num; i++)
		{
			char * untilArrayElementAtPositionI = elektraCalloc (arrayPositions[i] + 1);
			if (untilArrayElementAtPositionI == NULL)
			{
				return 0;
			}
			memcpy (untilArrayElementAtPositionI, &keyNameWithoutNamespace[0], arrayPositions[i]);

			Key * arraySizeKeyToInstantiate = getMatchingKeyFromKeySet (specKeys, untilArrayElementAtPositionI);
			if (arraySizeKeyToInstantiate == NULL)
			{
				arraySizeKeyToInstantiate = getMatchingKeyFromKeySet (ks, untilArrayElementAtPositionI);
			}
			const char * arraySizeToInstantiate = keyString (keyGetMeta (arraySizeKeyToInstantiate, "array"));

			if (arraySizeKeyToInstantiate == NULL)
			{
				// no array size, does any instantiated array element at this already exist
				if (copyAllMetaDataForMatchingArrayKeyName (ks, parentKey, specKey, isKdbGet) == -1)
				{
					addDefaultKey (ks, specKey, true);
				}

				elektraFree (untilArrayElementAtPositionI);

				continue;
			}

			int actualArraySize = getActualArraySize (ks, specKey, arrayPositions[i]);

			char * end;
			char * rest;

			char * afterPossibleArrayElement = elektraStrDup (arraySizeToInstantiate);

			int size = strchr (arraySizeToInstantiate, '#') == NULL ?
					   strtol (arraySizeToInstantiate, &end, 10) :
					   strtol (strtok_r (afterPossibleArrayElement, "#", &rest), &end, 10) + 1;

			elektraFree (afterPossibleArrayElement);
			elektraFree (untilArrayElementAtPositionI);

			if (actualArraySize == size)
			{
				continue;
			}

			instantiateArraySpecificationAndCopyMeta (specKey, ks, size, arrayPositions[i]);
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
			addDefaultKey (ks, specKey, false);
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

		elektraFree (specKeys);

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

				ksDel (specKeys);

				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			keyDel (cascadingKey);
		}

		if (copyMetaData (parentKey, current, specKeys, returned, isKdbGet) != 0)
		{
			ksDel (specKeys);

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
