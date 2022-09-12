
#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdberrors.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef LIBGITFOUND
#include <git2.h>
#endif

#define INT_BUF_SIZE 11 // Avoid math.h. int has at most 10 digits, +1 for \0

/**
 * @brief Get a statistical value from an information key
 * @param informationKey contains the statistics in its meta information
 * @param metaName which statistic to get
 * @retval the statistical value
 * @retval -1 on error
 */
static int getStatisticalValue (ElektraKey * informationKey, char * metaName)
{
	const ElektraKey * metaKey = elektraKeyGetMeta (informationKey, metaName);
	if (metaKey == NULL)
	{
		return 0;
	}
	char * test = elektraMalloc (elektraKeyGetValueSize (metaKey));
	if (elektraKeyGetString (metaKey, test, elektraKeyGetValueSize (metaKey)) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not get statistical value.");
		return -1;
	}
	int asInt = atoi (test);
	elektraFree (test);
	return asInt;
}

/**
 * @brief Set a statistical value in an information key.
 * @param informationKey contains the statistics in its meta information
 * @param metaName which statistic to set
 * @param value which value to set it to, must be a number
 * @retval 0 on success
 * @retval -1 on error
 *
 * This enforces that a number is set.
 */
static int setStatisticalValue (ElektraKey * informationKey, char * metaName, int value)
{
	char stringy[INT_BUF_SIZE];
	int printsize = snprintf (stringy, INT_BUF_SIZE, "%d", value);
	if (printsize == INT_BUF_SIZE || printsize < 0)
	{
		if (printsize < 0)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (
				informationKey,
				"Encoding error when setting %s: Could not convert %d into the string that should be used as meta value "
				"for the information key.",
				metaName, value);
		}
		else
		{
			ELEKTRA_SET_INTERNAL_ERRORF (informationKey,
						     "Statistical value %d was too large for its buffer. This happened with meta name %s.",
						     value, metaName);
		}
		return -1;
	}
	ssize_t size = elektraKeySetMeta (informationKey, metaName, stringy);
	if (size <= 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not set statistical value.");
		return -1;
	}
	return 0;
}

/**
 * @brief Increase a statistical value in an information key by one
 * @param informationKey contains the statistics in its meta information
 * @param metaName which statistic to increase
 * @retval new value
 */
static int increaseStatisticalValue (ElektraKey * informationKey, char * metaName)
{
	int value = getStatisticalValue (informationKey, metaName);
	value++;
	setStatisticalValue (informationKey, metaName, value);
	return value;
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlap conflicts where only base exists
 */
static int getNonOverlapOnlyBaseConflicts (ElektraKey * informationKey)
{
	return getStatisticalValue (informationKey, "nonOverlapOnlyBaseCounter");
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlap conflicts where all keys existed
 */
static int getNonOverlapAllExistConflicts (ElektraKey * informationKey)
{
	int nonOverlapAllExistCounter = getStatisticalValue (informationKey, "nonOverlapAllExistCounter");
	if (nonOverlapAllExistCounter % 3 != 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter input must not be null.");
	}
	return nonOverlapAllExistCounter / 3;
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlap conflicts where the key in the base set was empty
 */
static int getNonOverlapBaseEmptyConflicts (ElektraKey * informationKey)
{
	int nonOverlapBaseEmptyCounter = getStatisticalValue (informationKey, "nonOverlapBaseEmptyCounter");
	if (nonOverlapBaseEmptyCounter % 2 != 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter input must not be null.");
	}
	return nonOverlapBaseEmptyCounter / 2;
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of overlaps where all 3 keys were different
 */
static int getOverlap3different (ElektraKey * informationKey)
{
	int overlap3different = getStatisticalValue (informationKey, "overlap3different");
	if (overlap3different % 3 != 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter input must not be null.");
	}
	return overlap3different / 3;
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of overlaps where one key was empty, thus the other two keys had different values
 */
static int getOverlap1empty (ElektraKey * informationKey)
{
	int overlap1empty = getStatisticalValue (informationKey, "overlap1empty");
	if (overlap1empty % 2 != 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter input must not be null.");
	}
	return overlap1empty / 2;
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of overlaps that happened
 */
static int getTotalOverlaps (ElektraKey * informationKey)
{
	return getOverlap1empty (informationKey) + getOverlap3different (informationKey);
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlaps that happened
 */
static int getTotalNonOverlaps (ElektraKey * informationKey)
{
	return getNonOverlapBaseEmptyConflicts (informationKey) + getNonOverlapAllExistConflicts (informationKey) +
	       getNonOverlapOnlyBaseConflicts (informationKey);
}

/**
 * This function returns the number of conflicts that is store in the key
 *
 * @param informationKey contains the statistics in its meta information
 * @returns the number of conflicts stored in the key
 */
int getConflicts (ElektraKey * informationKey)
{
	return getTotalNonOverlaps (informationKey) + getTotalOverlaps (informationKey);
}

/**
 * @brief Removes one string from the other
 * @param sub this will be removed from string
 * @param string sub is removed from this char *
 * @returns the resulting string
 */
static char * strremove (char * string, const char * sub)
{
	size_t length = strlen (sub);
	if (length > 0)
	{
		char * p = string;
		while ((p = strstr (p, sub)) != NULL)
		{
			memmove (p, p + length, strlen (p + length) + 1);
		}
	}
	return string;
}

/**
 *  @brief This is the counterpart to the removeRoot function
 *
 *  Also turns the name for the /root key into the original one again
 *
 *  @param result all keys with extended name will be appended here
 *  @param input keys are from here
 *  @param string will be prepended to all the key names
 *  @param informationKey errors will be set here
 *  @retval -1 on error
 *  @retval  0 on success
 */
static int prependStringToAllKeyNames (ElektraKeyset * result, ElektraKeyset * input, const char * string, ElektraKey * informationKey)
{
	if (input == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter input must not be null.");
		return -1;
	}
	if (result == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter result must not be null.");
		return -1;
	}
	if (string == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter string must not be null.");
		return -1;
	}
	ElektraKey * key;
	elektraKeysetRewind (input);
	while ((key = elektraKeysetNext (input)) != NULL)
	{
		bool isRoot = strcmp (elektraKeyName (key), "/root") == 0;
		size_t size = strlen (string);
		if (isRoot)
		{
			size += 1;
		}
		else
		{
			size += elektraKeyGetNameSize (key);
		}
		char * newName = elektraMalloc (size);
		strcpy (newName, string);
		if (!isRoot)
		{
			strcat (newName, elektraKeyName (key));
		}
		ElektraKey * duplicateKey = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL); // keySetName returns -1 if key was inserted to a keyset before
		int status = elektraKeySetName (duplicateKey, newName);
		elektraFree (newName);
		if (status < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not set key name.");
		}
		status = elektraKeysetAppendKey (result, duplicateKey);
		if (status < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
		}
	}
	return 0;
}

/**
 * @brief Remove the root from each key of a set
 * @param original the key set from which root will be rmeoved
 * @param root remove this from all the keys
 * @param informationKey will contain information if an error occurs
 * @returns a new key set without the root
 *
 * Example: If root is user:/example and the KeySet contains a key with the name user:/example/something then
 * the returned KeySet will contain the key /something.
 */
static ElektraKeyset * removeRoot (ElektraKeyset * original, ElektraKey * root, ElektraKey * informationKey)
{
	elektraKeysetRewind (original);
	ElektraKeyset * result = elektraKeysetNew (0, ELEKTRA_KS_END);
	const char * rootKeyNameString = elektraKeyName (root);
	ElektraKey * currentKey;
	while ((currentKey = elektraKeysetNext (original)) != NULL)
	{
		char * currentKeyNameString = elektraMalloc (elektraKeyGetNameSize (currentKey));
		if (elektraKeyGetName (currentKey, currentKeyNameString, elektraKeyGetNameSize (currentKey)) < 0)
		{
			ELEKTRA_ASSERT (false, "ERROR: This should not happen");
			elektraFree (currentKeyNameString);
			elektraKeysetDel (result);
			return NULL;
		};
		if (elektraKeyIsBelow (root, currentKey) || elektraKeyCmp (currentKey, root) == 0)
		{
			ElektraKey * duplicateKey = elektraKeyDup (currentKey, ELEKTRA_KEY_CP_ALL);
			int retVal;
			if (elektraKeyIsBelow (root, currentKey))
			{
				currentKeyNameString = strremove (currentKeyNameString, rootKeyNameString);
				retVal = elektraKeySetName (duplicateKey, currentKeyNameString);
			}
			else
			{
				// If the root itself is in the keyset then create a special name for it as it would be empty otherwise
				retVal = elektraKeySetName (duplicateKey, "/root");
			}
			if (retVal < 0)
			{
				elektraFree (currentKeyNameString);
				elektraKeysetDel (result);
				elektraKeyDel (duplicateKey);
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Setting new key name was not possible.");
				return NULL;
			}
			elektraKeysetAppendKey (result, duplicateKey);
			elektraFree (currentKeyNameString);
		}
		else
		{
			elektraFree (currentKeyNameString);
			elektraKeysetDel (result);
			ELEKTRA_SET_INTERNAL_ERROR (
				informationKey,
				"Setting new key name was not possible. The current key is not below or equal to the root key.");
			return NULL;
		}
	}
	return result;
}

/**
 * @brief Compares two keys
 * @retval true if two keys are equal
 * @retval false otherwise
 */
static bool keysAreEqual (ElektraKey * a, ElektraKey * b)
{
	// Two nothings are not keys and thus false too
	if (a == NULL || b == NULL)
	{
		return false;
	}
	if (elektraKeyGetValueSize (a) != elektraKeyGetValueSize (b))
	{
		return false;
	}
	if (0 != memcmp (elektraKeyValue (a), elektraKeyValue (b), elektraKeyGetValueSize (a)))
	{
		return false;
	}
	return true;
}

/**
 * @brief Helper function for checkSingleSet for when the key (name is relevant) is only in two of the three key sets
 * @retval -1 on error
 * @retval 0 on success
 */
static int twoOfThreeExistHelper (ElektraKey * checkedKey, ElektraKey * keyInFirst, ElektraKey * keyInSecond, ElektraKeyset * result, bool checkedIsDominant,
				  int baseIndicator, ElektraKey * informationKey)
{
	ElektraKey * existingKey;
	bool thisConflict = false;
	/** This if or the else if happen when our and their set have a key that
	 *  base does not have. This is a conflict case.
	 *  This place is hit twice, thus overlap1empty gets double the amount of errors.
	 */
	if (keyInFirst != NULL)
	{
		existingKey = keyInFirst;
	}
	else if (keyInSecond != NULL)
	{
		existingKey = keyInSecond;
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
		return -1;
	}
	if (thisConflict)
	{
		increaseStatisticalValue (informationKey, "nonOverlapBaseEmptyCounter");
	}
	if (!keysAreEqual (checkedKey, existingKey))
	{
		// overlap  with single empty
		// This spot is hit twice for a single overlap conflict. Thus calculate half later on.
		increaseStatisticalValue (informationKey, "overlap1empty");
		if (checkedIsDominant)
		{
			if (elektraKeysetAppendKey (result, checkedKey) < 0)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
				return -1;
			}
		}
	}
	else
	{
		// uses the NULL properties of keysAreEqual
		if (keysAreEqual (checkedKey, keyInFirst) && baseIndicator == 2)
		{
			thisConflict = true;
		}
		if (keysAreEqual (checkedKey, keyInSecond) && baseIndicator == 1)
		{
			thisConflict = true;
		}
		if (thisConflict)
		{
			// base is empty and other and their have the same (non-empty) value
			// this is a conflict
			increaseStatisticalValue (informationKey, "nonOverlapBaseEmptyCounter");
			if (checkedIsDominant)
			{
				if (elektraKeysetAppendKey (result, checkedKey) < 0)
				{
					ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
					return -1;
				}
			}
		}
	}
	return 0;
}

/**
 * @brief Helper function for allExistHelper checking if two of the three keys are equal and acting accordingly (side effects!)
 * @retval true if exactly two of the three keys have the same value
 * @retval false otherwise
 */
static bool twoOfThoseKeysAreEqual (ElektraKey * checkedKey, ElektraKey * keyInFirst, ElektraKey * keyInSecond, ElektraKeyset * result, bool checkedIsDominant,
				    int baseIndicator, ElektraKey * informationKey)
{
	/**
	 * One example for the next 3 ifs
	 *
	 * Cell contents are the values of the keys (/#0, /#1, ...)
	 * in the different key sets
	 *     base     our      their
	 * /#0 one      previous previous
	 * /#1 two      one      one
	 * /#2 three    two      two
	 * /#3 four     three    three
	 * /#4 five     four     four
	 * /#5          five     five
	 * In the area from top down to line /#4 (inclusive) each cell triggers
	 * the nonOverlapAllExistCounter. However, we must not count one conflict
	 * multiple times, thus divide by 3 as there are three key sets (=columns).
	 */
	if (keysAreEqual (keyInFirst, keyInSecond))
	{
		if (baseIndicator == 0)
		{
			/** This is a non-overlap conflict
			 *  Base is currently checked and has value A, their and our have a different value B
			 */
			increaseStatisticalValue (informationKey, "nonOverlapAllExistCounter");
			if (checkedIsDominant)
			{
				// If base is also dominant then append it's key
				if (elektraKeysetAppendKey (result, checkedKey) < 0)
				{
					ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
				}
			}
		}
		return true;
	}
	else if (keysAreEqual (checkedKey, keyInFirst))
	{
		if (baseIndicator == 0)
		{
			if (elektraKeysetAppendKey (result, keyInSecond) < 0)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
			}
		}
		else
		{
			if (baseIndicator == 2)
			{
				/** This is a non-overlap conflict
				 *  Base is currently secondCompare and has value A, their and our have a different
				 *  value B
				 */
				increaseStatisticalValue (informationKey, "nonOverlapAllExistCounter");
				if (checkedIsDominant)
				{
					// If base is also dominant then append it's key
					if (elektraKeysetAppendKey (result, checkedKey) < 0)
					{
						ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
					}
				}
			}
		}
		return true;
	}
	else if (keysAreEqual (checkedKey, keyInSecond))
	{

		if (baseIndicator == 0)
		{
			if (elektraKeysetAppendKey (result, keyInFirst) < 0)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
			}
		}
		else
		{
			if (baseIndicator == 1)
			{
				/** This is a non-overlap conflict
				 *  Base is currently firstCompare and has value A, their and our have a different
				 *  value B
				 */
				increaseStatisticalValue (informationKey, "nonOverlapAllExistCounter");
				if (checkedIsDominant)
				{
					// If base is also dominant then append it's key
					if (elektraKeysetAppendKey (result, checkedKey) < 0)
					{
						ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
					}
				}
			}
		}
		return true;
	}
	else
	{
		return false;
	}
}

/**
 * @brief Helper function for checkSingleSet for when a key exists in all key sets.
 * @retval -1 on error
 * @retval 0 on success
 */
static int allExistHelper (ElektraKey * checkedKey, ElektraKey * keyInFirst, ElektraKey * keyInSecond, ElektraKeyset * result, bool checkedIsDominant,
			   int baseIndicator, ElektraKey * informationKey)
{
	if (keysAreEqual (checkedKey, keyInFirst) && keysAreEqual (checkedKey, keyInSecond))
	{
		/**
		 * append any of the three keys
		 * will be appended multiple times, but that doesn't matter for the result
		 */
		if (elektraKeysetAppendKey (result, checkedKey) < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
			return -1;
		}
	}
	else
	{
		if (!twoOfThoseKeysAreEqual (checkedKey, keyInFirst, keyInSecond, result, checkedIsDominant, baseIndicator, informationKey))
		{
			/**
			 * Overlap conflict case
			 *
			 * The same overlap conflict gets detected three times, once for each of the three invocations of
			 * checkSingleSet. However, only one of those three times is required. Thus use a getter function
			 * that calculates a third.
			 */
			increaseStatisticalValue (informationKey, "overlap3different");
			if (checkedIsDominant)
			{
				if (elektraKeysetAppendKey (result, checkedKey) < 0)
				{
					ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
					return -1;
				}
			}
		}
	}
	return 0;
}


/**
 * and the element is not already in the result key set.
 * It should be called 3 times, each time with a different of our key set, their key set and base key set as checkedSet parameter.
 * Which of the remaining two key sets is firstCompared or secondCompared is irrelevant.
 *
 * @param checkedIsDominant parameter is for the merge strategy. If a conflict occurs and checkedIsDominant is true then the current element
 * of checkedSet is inserted. Consequently, it has to be set to true for exactly one of the three calls of this function.
 *
 * @param baseIndicator indicates which of the three key sets is the base key set. 0 is checkedSet, 1 firstcompared, 2 secondCompared
 * @param informationKey will contain information if an error ocurred
 *
 * @retval -1 on error
 * @retval 0 on success
 *
 */
static int checkSingleSet (ElektraKeyset * checkedSet, ElektraKeyset * firstCompared, ElektraKeyset * secondCompared, ElektraKeyset * result, bool checkedIsDominant,
			   int baseIndicator, ElektraKey * informationKey)
{
	elektraKeysetRewind (checkedSet);
	elektraKeysetRewind (firstCompared);
	elektraKeysetRewind (secondCompared);
	ElektraKey * checkedKey;
	while ((checkedKey = elektraKeysetNext (checkedSet)) != NULL)
	{
		/**
		 * Check if a key with the same name exists
		 * Nothing about values is said yet
		 */
		ElektraKey * keyInFirst = elektraKeysetLookup (firstCompared, checkedKey, 0);
		ElektraKey * keyInSecond = elektraKeysetLookup (secondCompared, checkedKey, 0);
		if (keyInFirst != NULL && keyInSecond != NULL)
		{
			allExistHelper (checkedKey, keyInFirst, keyInSecond, result, checkedIsDominant, baseIndicator, informationKey);
		}
		else if (keyInFirst == NULL && keyInSecond == NULL)
		{
			if (baseIndicator == 0)
			{
				/**
				 * Non-overlap conflict https://www.gnu.org/software/diffutils/manual/html_node/diff3-Merging.html
				 *
				 * Here keys from base could be appended. But doing so is not useful.
				 */
				increaseStatisticalValue (informationKey, "nonOverlapOnlyBaseCounter");
			}
			else
			{
				if (elektraKeysetAppendKey (result, checkedKey) < 0)
				{
					ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not append key.");
				}
			}
		}
		else
		{
			twoOfThreeExistHelper (checkedKey, keyInFirst, keyInSecond, result, checkedIsDominant, baseIndicator,
					       informationKey);
		}
	}
	return 0;
}

#ifdef LIBGITFOUND

/**
 * Iterates over all keys that belong to an key array started by arrayStart and puts their values into a newly allocated string
 * Each value is separated by a newline.
 * Does not modify arrayStart.
 *
 * @param ks the key set of which the array is part
 * @param arrayStart key that is the start of the array /#0
 * @param informationKey the key for error codes
 * @returns pointer to the string, NULL on error
 * */
static char * getValuesAsArray (ElektraKeyset * ks, const ElektraKey * arrayStart, ElektraKey * informationKey)
{
	ELEKTRA_LOG_DEBUG ("Converting key set to char array");
	if (ks == NULL || arrayStart == NULL || informationKey == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameters must not be null");
		return NULL;
	}
	int bufferSize = 64;
	int stringSize = 2; // 1 for final \n and \0
	/**
	 * this calloc sets the first character to \0 so that the first invocation of
	 * strncat has something to append to
	 */
	char * buffer = elektraCalloc (bufferSize);
	/** ksLookup does not get a copy of the key but the real key.
	 *  The elektraArrayIncName would then change the name of the real key.
	 *  We don't want that as we only increase the name to loop over all keys.
	 */
	ElektraKey * iterator = elektraKeyDup (arrayStart, ELEKTRA_KEY_CP_NAME);
	if (iterator == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not duplicate key to iterate.");
		elektraFree (buffer);
		return NULL;
	}
	ElektraKey * lookup;
	int counter = 0;
	while ((lookup = elektraKeysetLookup (ks, iterator, ELEKTRA_KDB_O_POP)) != 0)
	{
		counter++;
		int tmpSize = elektraKeyGetValueSize (lookup);
		stringSize += tmpSize; // keyGetValueSize includes size for \0 which we use for \n instead
		if (stringSize > bufferSize)
		{
			/** A single key could require more memory than multiplying once gives us
			 * For example if the first key name is 255
			 * The = in >= is important so that there is space for the null terminator of \n in strcat later on
			 */
			while (stringSize >= bufferSize)
			{
				bufferSize *= 2;
			}
			if (elektraRealloc ((void **) &buffer, bufferSize) < 0)
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (informationKey);
				elektraFree (buffer);
				elektraKeyDel (iterator);
				return NULL;
			}
		}
		strncat (buffer, elektraKeyString (lookup), tmpSize);
		// LibGit2 later operates on the newlines. This also ensures
		// that there is a null terminator at the end of all the
		// concatenated lines
		strcat (buffer, "\n");
		if (counter >= 2)
		{
			int retval = elektraKeyDel (lookup);
			if (retval != 0)
			{
				if (retval < 0)
				{
					ELEKTRA_SET_INTERNAL_ERROR (informationKey,
								    "Could not delete key from key set because null pointer.");
				}
				else
				{
					ELEKTRA_SET_INTERNAL_ERRORF (
						informationKey,
						"Could not delete key with name %s from key set. There are %d references left.",
						elektraKeyName (lookup), retval);
				}
				elektraFree (buffer);
				elektraKeyDel (iterator);
				return NULL;
			}
		}
		if (elektraArrayIncName (iterator) < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Increasing array key failed.");
			elektraFree (buffer);
			elektraKeyDel (iterator);
			return NULL;
		}
	}
	elektraKeyDel (iterator);
	return buffer;
}

/**
 * Turns an array from getValuesAsArray into a key set again.
 * Frees the array and creates a new key set.
 *
 * @param array array to be converted, has no \0 terminator at end
 * @param length length of array, does not include size for \0 as this is not there
 * @param informationKey for errors
 * @returns the KeySet
 */
static ElektraKeyset * ksFromArray (const char * array, int length, ElektraKey * informationKey)
{
	if (array == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Parameter must not be null.");
		return NULL;
	}
	ElektraKeyset * result = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (result == NULL)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (informationKey);
		return NULL;
	}
	ElektraKey * iterator = elektraKeyNew ("/#0", ELEKTRA_KEY_END);
	if (iterator == NULL)
	{
		elektraKeysetDel (result);
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (informationKey);
		return NULL;
	}
	char * buffer = elektraCalloc (length + 1); // + 1 for terminating null character
	memcpy (buffer, array, length);
	char * saveptr;
	char * token = strtok_r (buffer, "\n", &saveptr);
	do
	{
		elektraKeysetAppendKey (result, elektraKeyNew (elektraKeyName (iterator), ELEKTRA_KEY_VALUE, token, ELEKTRA_KEY_END));
		if (elektraArrayIncName (iterator) < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Increasing array key failed.");
			elektraFree (result);
			elektraKeyDel (iterator);
			return NULL;
		}
	} while ((token = strtok_r (NULL, "\n", &saveptr)) != NULL);
	elektraFree (buffer);
	elektraKeyDel (iterator);
	return result;
}


/**
 * Counts how many conflict markers ======= are in the string "text"
 * @param text The text in which conflict markers occur
 * @returns the number of ocnflict markers in the text
 */
static int numberOfConflictMarkers (const char * text)
{
	int result = 0;
	const char * tmp = text;
	// conflict markers in LibGit2 are exactly 7 equal signs
	while ((tmp = strstr (tmp, "=======")))
	{
		result++;
		tmp++;
	}
	return result;
}
/**
 * Removes all the arrays from our, their, base and result and puts the result of the merge into resultSet
 * @param ourSet our
 * @param theirSet their
 * @param baseSet base
 * @param resultSet result
 * @retval 0 on success
 * @retval -1 on error
 */
static int handleArrays (ElektraKeyset * ourSet, ElektraKeyset * theirSet, ElektraKeyset * baseSet, ElektraKeyset * resultSet, ElektraKey * informationKey, int strategy)
{
	ELEKTRA_LOG ("cmerge now handles arrays");
	ElektraKey * checkedKey;
	ElektraKeyset * toAppend = NULL;
	while ((checkedKey = elektraKeysetNext (baseSet)) != NULL)
	{
		if (elektraArrayValidateName (checkedKey) >= 0)
		{
			ElektraKey * keyInOur = elektraKeysetLookup (ourSet, checkedKey, 0);
			ElektraKey * keyInTheir = elektraKeysetLookup (theirSet, checkedKey, 0);
			char * baseArray = getValuesAsArray (baseSet, checkedKey, informationKey);
			if (baseArray == NULL)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not convert `base` KeySet into char[] for LibGit.");
				elektraKeyDel (checkedKey);
				elektraKeysetDel (toAppend);
				return -1;
			}
			char * ourArray = getValuesAsArray (ourSet, keyInOur, informationKey);
			if (ourArray == NULL)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not convert `our` KeySet into char[] for LibGit.");
				elektraFree (baseArray);
				elektraKeyDel (checkedKey);
				elektraKeysetDel (toAppend);
				return -1;
			}
			char * theirArray = getValuesAsArray (theirSet, keyInTheir, informationKey);
			if (theirArray == NULL)
			{
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not convert `their` KeySet into char[] for LibGit.");
				elektraFree (ourArray);
				elektraFree (baseArray);
				ELEKTRA_ASSERT (elektraKeyGetRef (checkedKey) > 0, "WTF 3");
				elektraKeyDel (checkedKey);
				elektraKeysetDel (toAppend);
				return -1;
			}
			git_merge_file_result out = { 0 }; // out.ptr will not receive a terminating null character
			const git_merge_file_input libgit_base = { .ptr = baseArray, .size = strlen (baseArray) };
			const git_merge_file_input libgit_our = { .ptr = ourArray, .size = strlen (ourArray) };
			const git_merge_file_input libgit_their = { .ptr = theirArray, .size = strlen (theirArray) };
			// This is the usual way but it throws a compiler error
			// git_merge_file_options options = GIT_MERGE_FILE_OPTIONS_INIT;
			git_merge_file_options options = { 0 };
			switch (strategy)
			{
			case MERGE_STRATEGY_OUR:
				options.favor = GIT_MERGE_FILE_FAVOR_OURS;
				break;
			case MERGE_STRATEGY_THEIR:
				options.favor = GIT_MERGE_FILE_FAVOR_THEIRS;
				break;
			}
			int ret = git_merge_file (&out, &libgit_base, &libgit_our, &libgit_their, &options);
			if (ret == 0)
			{
				if (out.automergeable)
				{
					toAppend = ksFromArray (out.ptr, out.len, informationKey);
					ELEKTRA_LOG ("libgit successfully handled an array");
				}
				else
				{
					if (!MERGE_STRATEGY_ABORT)
					{
						ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Expected merge strategy abort.");
					}
					int currentNumberOfConflicts = numberOfConflictMarkers (out.ptr);
					int previousNumberOfConflicts = getStatisticalValue (informationKey, "libgitConflicts");
					int newNumberOfConflicts = previousNumberOfConflicts + currentNumberOfConflicts;
					setStatisticalValue (informationKey, "libgitConflicts", newNumberOfConflicts);
					char msg[300];
					snprintf (msg, 300, "libgit could not automerge an array. It contains %d conflict markers.",
						  currentNumberOfConflicts);
					ELEKTRA_LOG ("%s", msg);
				}
			}
			else
			{
				ELEKTRA_LOG ("libgit returned error code %d", ret);
			}
			git_merge_file_result_free (&out);
			elektraFree (ourArray);
			elektraFree (theirArray);
			elektraFree (baseArray);
			elektraKeyDel (keyInOur);
			elektraKeyDel (keyInTheir);
			elektraKeyDel (checkedKey);
		}
	}
	if (toAppend != NULL)
	{
		elektraKeysetAppend (resultSet, toAppend);
		elektraKeysetDel (toAppend);
	}
	return 0;
}

#endif // LIBGITFOUND


/**
 *
 * This function can incorporate changes from two modified versions (our and their) into a common preceding version (base) of a key set.
 * This lets you merge the sets of changes represented by the two newer key sets. This is called a three-way merge between key sets.
 *
 * @brief Join three key sets together
 * @param our our key set
 * @param ourRoot key that has the root of our as name
 * @param their their key set
 * @param theirRoot key that has the root of their as name
 * @param base base key set
 * @param baseRoot key that has the root of base as name
 * @param resultRoot the name of this key determines where the resulting key set will be stored
 * @param strategy specify which merge strategy to choose in case of a conflict
 * @param informationKey stores errors as well as statistics
 * @returns the merged key set and NULL on error
 */
ElektraKeyset * elektraMerge (ElektraKeyset * our, ElektraKey * ourRoot, ElektraKeyset * their, ElektraKey * theirRoot, ElektraKeyset * base, ElektraKey * baseRoot, ElektraKey * resultRoot,
		       int strategy, ElektraKey * informationKey)
{
	ELEKTRA_LOG ("cmerge starts with strategy %d (see kdbmerge.h)", strategy);

	ElektraKeyset * ourCropped = removeRoot (our, ourRoot, informationKey);
	if (ourCropped == NULL)
	{
		return NULL;
	}
	ElektraKeyset * theirCropped = removeRoot (their, theirRoot, informationKey);
	if (theirCropped == NULL)
	{
		elektraKeysetDel (ourCropped);
		return NULL;
	}
	ElektraKeyset * baseCropped = removeRoot (base, baseRoot, informationKey);
	if (baseCropped == NULL)
	{
		elektraKeysetDel (ourCropped);
		elektraKeysetDel (theirCropped);
		return NULL;
	}
	ElektraKeyset * result = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetRewind (ourCropped);
	elektraKeysetRewind (theirCropped);
	elektraKeysetRewind (baseCropped);
	bool ourDominant = false;
	bool theirDominant = false;
	switch (strategy)
	{
	case MERGE_STRATEGY_OUR:
		ourDominant = true;
		break;
	case MERGE_STRATEGY_THEIR:
		theirDominant = true;
		break;
	}

#ifdef LIBGITFOUND
	git_libgit2_init ();
	ELEKTRA_LOG ("cmerge can use libgit2 to handle arrays");
	if (handleArrays (ourCropped, theirCropped, baseCropped, result, informationKey, strategy) > 0)
	{
		elektraKeysetDel (result);
		return NULL;
	}
#else
	ELEKTRA_LOG ("cmerge can NOT use libgit2 to handle arrays");
#endif

	elektraKeysetRewind (ourCropped);
	elektraKeysetRewind (theirCropped);
	elektraKeysetRewind (baseCropped);
	checkSingleSet (baseCropped, ourCropped, theirCropped, result, false, 0, informationKey); // base is never dominant
	checkSingleSet (theirCropped, baseCropped, ourCropped, result, theirDominant, 1, informationKey);
	checkSingleSet (ourCropped, theirCropped, baseCropped, result, ourDominant, 2, informationKey);
	elektraKeysetRewind (ourCropped);

	if (elektraKeysetDel (ourCropped) != 0 || elektraKeysetDel (theirCropped) != 0 || elektraKeysetDel (baseCropped) != 0)
	{
		elektraKeysetDel (result);
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not delete a key set.");
		return NULL;
	}
	if (getConflicts (informationKey) > 0)
	{
		if (strategy == MERGE_STRATEGY_ABORT)
		{
			elektraKeysetDel (result);
			ELEKTRA_SET_INTERNAL_ERRORF (informationKey, "Abort strategy was set and %d conflicts occured.",
						     getConflicts (informationKey));
			return NULL;
		}
	}

	ElektraKeyset * resultWithRoot = elektraKeysetNew (0, ELEKTRA_KS_END);
	prependStringToAllKeyNames (resultWithRoot, result, elektraKeyName (resultRoot), informationKey);
	elektraKeysetDel (result);
	return resultWithRoot;
}
