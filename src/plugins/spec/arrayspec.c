#include "./arrayspec.h"
#include "./matching.h"

#include <elektra/core/errors.h>
#include <elektra/utility/format.h>

#include <ctype.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <internal/utility/string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Checks if the {@link specKey} is an array specification key.
 *
 * @param specKey the specification key to check if it is an array specification
 * @retval true  - if it is an array specification
 * @retval false - if it is no array specification
 */
bool isArraySpecification (Key * specKey)
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
bool containsUnderlineInArraySpec (Key * specKey)
{
	return strchr (keyName (specKey), '_') != NULL;
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
char * createArrayElementName (int arrayNumber)
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
char * createFormattedArrayKeyNameInDefaultNamespace (char * keyNameWithoutNamespace, int arrayNumber, int pos)
{
	if (keyNameWithoutNamespace == NULL)
	{
		return NULL;
	}

	if (pos > (int) elektraStrLen (keyNameWithoutNamespace) - 1)
	{
		return NULL;
	}

	char * strUntilArrayElement = elektraCalloc (pos + 1);
	if (strUntilArrayElement == NULL)
	{
		return NULL;
	}
	memcpy (strUntilArrayElement, &keyNameWithoutNamespace[0], pos);

	size_t keyNameSize = elektraStrLen (&keyNameWithoutNamespace[pos + 1]);
	char * strAfterArrayElement = elektraCalloc (keyNameSize);
	if (strAfterArrayElement == NULL)
	{
		elektraFree (strUntilArrayElement);
		return NULL;
	}
	memcpy (strAfterArrayElement, &keyNameWithoutNamespace[pos + 1], keyNameSize - 1);

	char * arrayElementName = createArrayElementName (arrayNumber);

	char * formattedKeyName = elektraFormat ("default:%s/%s%s", strUntilArrayElement, arrayElementName, strAfterArrayElement);

	elektraFree (strUntilArrayElement);
	elektraFree (strAfterArrayElement);
	elektraFree (arrayElementName);

	return formattedKeyName;
}

/**
 * Check if the array specification element contains an array element '#' and afterwards no digit or '_'.
 *
 * Sample:
 * 	spec:/sw/example/menu/#0/current/menu/#/command => true
 *	spec:/sw/example/menu/#0/current/menu => false
 *
 * @param keyNameWithNamespace the key name with namespace to look for
 * @retval true - if the array specification element contain an array element '#' with a digit or '_' afterwards
 * @retval false - if the array specification element  does not contain any digit or '_' after the array element '#
 */
bool containsArraySpecElementWithNoDigitOrUnderlineAfterwards (const char * keyNameWithNamespace)
{
	for (int i = 0; i < (int) elektraStrLen (keyNameWithNamespace); i++)
	{
		if (keyNameWithNamespace[i] == '#' && (keyNameWithNamespace[i + 1] == '/' || keyNameWithNamespace[i + 1] == '\0'))
		{
			return true;
		}
	}
	return false;
}

/**
 * Creates the corresponding array element keys and copies all the meta data keys from {@link specKey}.
 *
 * @param specKey the specification key to copy the meta data from
 * @param ks the KeySet to append the newly created array element keys too
 * @param arraySize number of array elements to create under the key
 * @param pos the position of the array element (`#`) to start instantiating at
 */
void instantiateArraySpecificationAndCopyMeta (Key * specKey, KeySet * ks, int arraySize, int pos)
{
	KeySet * instantiatedArraySpecs = ksNew (arraySize + 1, KS_END);
	for (int i = 0; i < arraySize; i++)
	{
		char * keyNameWithoutNamespace = strchr (keyName (specKey), '/');

		char * formattedKeyName = createFormattedArrayKeyNameInDefaultNamespace (keyNameWithoutNamespace, i, pos);
		if (containsArraySpecElementWithNoDigitOrUnderlineAfterwards (formattedKeyName))
		{
			ksDel (instantiatedArraySpecs);
			elektraFree (formattedKeyName);
			return;
		}

		Key * key = ksLookupByName (ks, strchr (formattedKeyName, '/'), 0);
		if (key == 0)
		{
			key = keyNew (formattedKeyName, KEY_END);
		}

		// check if key has a value
		const Key * defaultValue = keyGetMeta (specKey, "meta:/default");
		if (elektraStrCmp (keyString (key), "") == 0 || elektraStrCmp (keyString (key), "(null)") == 0)
		{
			const char * defaultValueString = keyString (defaultValue);
			if (elektraStrCmp (defaultValueString, "(null)") != 0)
			{
				keySetString (key, keyString (defaultValue));
			}
		}

		keyCopyAllMeta (key, specKey);
		ksAppendKey (instantiatedArraySpecs, key);

		elektraFree (formattedKeyName);

		keyDel (key);
	}

	ksAppend (ks, instantiatedArraySpecs);

	ksDel (instantiatedArraySpecs);
}

/**
 * The positions of where the `#` occurs in the specification key.
 *
 * @param keyNameWithoutNamespace the key name without the namespace
 * @param[out] arrayPositions the array to hold all the positions of the `#` in the specification key
 * @param arraySize number of `#` in the whole specification key
 */
void setArrayPositions (const char * keyNameWithoutNamespace, int * arrayPositions, int arraySize)
{
	if (keyNameWithoutNamespace == NULL || arrayPositions == NULL)
	{
		return;
	}

	int arrPos = 0;
	for (int i = 0; i < (int) elektraStrLen (keyNameWithoutNamespace) - 1; i++)
	{
		if (keyNameWithoutNamespace[i] == '#')
		{
			if (arrPos < arraySize)
			{
				arrayPositions[arrPos++] = i;
			}
		}
	}
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
Key * getMatchingKeyFromKeySet (KeySet * ks, char * name)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);

		size_t nameSize = elektraStrLen (name);
		char * nameDup;

		bool memoryAllocated = false;

		if (name[nameSize - 2] == '/')
		{
			nameDup = elektraStrDup (name);
			nameDup[nameSize - 2] = '\0';
			memoryAllocated = true;
		}
		else
		{
			nameDup = name;
		}

		if (elektraStrCmp (strchr (keyName (current), '/'), nameDup) == 0)
		{
			if (memoryAllocated)
			{
				elektraFree (nameDup);
			}

			return current;
		}

		if (memoryAllocated)
		{
			elektraFree (nameDup);
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
Key * getArraySizeOfArrayParent (KeySet * specKeys, Key * specKey)
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
int getNumberOfArrayCharactersInSpecName (Key * specKey)
{
	char * withoutNamespace = strchr (keyName (specKey), '/');

	if (withoutNamespace == NULL)
	{
		return 0;
	}

	int count = 0;

	for (size_t i = 0; i < elektraStrLen (withoutNamespace) - 1; i++)
	{
		if (withoutNamespace[i] == '#')
		{
			count++;
		}
	}

	return count;
}

/**
 * Validate the array size of a specification key.
 *
 * @param key the key to fetch the array size from
 * @param specKey the specification key to validate the array size from
 * @retval true - if the array size is valid
 * @retval false - if the array size is not valid
 */
bool validateArraySize (Key * key, Key * specKey)
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
 * Validate the array size. If it does not find any array size a warning is appended to the parent key.
 *
 * @param ks the key set to look for the key
 * @param specKeys the specification keys
 * @param specKey the array specification key
 * @retval true - if the array size is valid
 * @retval false - if the array size was not found
 * @retval false - if the array size is not valid
 */
bool isValidArraySize (KeySet * ks, KeySet * specKeys, Key * specKey)
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
 * Get the actual array size.
 *
 * @param ks the KeySet with all keys
 * @param specKey the array specification key
 * @param arrayPosition the position of the array character
 * @retval true - if the array is empty
 * @retval false - if the array is not empty
 */
int getActualArraySize (KeySet * ks, Key * specKey, int arrayPosition)
{
	int count = 0;
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		char * withoutNamespace = strchr (keyName (current), '/');

		int len = elektraStrLen (withoutNamespace);
		if (arrayPosition >= len - 1)
		{
			continue;
		}

		if (!specMatches (specKey, current))
		{
			continue;
		}

		if (withoutNamespace[arrayPosition] == '#' &&
		    (arrayPosition < (len - 1) &&
		     (withoutNamespace[arrayPosition + 1] == '_' || withoutNamespace[arrayPosition + 1] != '/' ||
		      isdigit (withoutNamespace[arrayPosition + 1]))))
		{
			count++;
		}
	}

	return count;
}

/**
 * Copy all meta keys from {@link specKey} to all matching keys in {@link ks}.
 *
 * @param ks the KeySet to search for matching key names for {@link specKey}
 * @param parentKey the parent key to be used in case there is an error / warning
 * @param specKey the specification key to check in {@link ks} for matching elements
 * @retval 0 - if copying of all meta keys was successful
 * @retval  - if no meta key needed to be copied
 * @retval -1 - if copying of meta keys was not successful
 */
int copyAllMetaDataForMatchingArrayKeyName (KeySet * ks, Key * parentKey, Key * specKey, bool isKdbGet)
{
	int copied = -1;
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * key = ksAtCursor (ks, it);
		if (keyGetNamespace (key) != KEY_NS_SPEC && specMatches (specKey, key))
		{
			if (keyCopyAllMeta (key, specKey) < 0)
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
			else
			{
				copied = 0;
			}
		}
	}

	return copied;
}
