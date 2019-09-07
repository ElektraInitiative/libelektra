
#include "kdbmerge.h"
#include "elektra/kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INT_BUF_SIZE 11 // Avoid math.h. int has at most 10 digits, +1 for \0

/**
 * @brief Get a statistical value from an information key
 * @param informationKey contains the statistics in its meta information
 * @param metaName which statistic to get
 * @param retval the statistical value
 * @param retval -1 on error
 */
static int getStatisticalValue (Key * informationKey, char * metaName)
{
	const Key * metaKey = keyGetMeta (informationKey, metaName);
	if (metaKey == NULL)
	{
		return 0;
	}
	char * test = elektraMalloc (keyGetValueSize (metaKey));
	if (keyGetString (metaKey, test, keyGetValueSize (metaKey)) < 0)
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
 *
 * This enforces that a number is set.
 */
static void setStatisticalValue (Key * informationKey, char * metaName, int value)
{
	char stringy[INT_BUF_SIZE];
	int printsize = snprintf (stringy, INT_BUF_SIZE, "%d", value);
	if (printsize < INT_BUF_SIZE)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Statistical value was too large for its buffer.");
	}
	ssize_t size = keySetMeta (informationKey, metaName, stringy);
	if (size <= 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not set statistical value.");
	}
}

/**
 * @brief Increase a statistical value in an information key by one
 * @param informationKey contains the statistics in its meta information
 * @param metaName which statistic to increase
 * @retval new value
 */
static int increaseStatisticalValue (Key * informationKey, char * metaName)
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
static int getNonOverlapOnlyBaseConflicts (Key * informationKey)
{
	return getStatisticalValue (informationKey, "nonOverlapOnlyBaseCounter");
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlap conflicts where all keys existed
 */
static int getNonOverlapAllExistConflicts (Key * informationKey)
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
static int getNonOverlapBaseEmptyConflicts (Key * informationKey)
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
static int getOverlap3different (Key * informationKey)
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
static int getOverlap1empty (Key * informationKey)
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
static int getTotalOverlaps (Key * informationKey)
{
	return getOverlap1empty (informationKey) + getOverlap3different (informationKey);
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of non-overlaps that happened
 */
static int getTotalNonOverlaps (Key * informationKey)
{
	return getNonOverlapBaseEmptyConflicts (informationKey) + getNonOverlapAllExistConflicts (informationKey) +
	       getNonOverlapOnlyBaseConflicts (informationKey);
}

/**
 * @param informationKey contains the statistics in its meta information
 * @retval the number of overlaps and non-overlaps that happened
 */
static int getTotalConflicts (Key * informationKey)
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
 *  @param input keys are from here
 *  @param result all keys with extended name will be appended here
 *  @param informationKey errors will be set here
 *  @retval -1 on error
 *  @retval  0 on success
 */
static int prependStringToAllKeyNames (KeySet * result, KeySet * input, const char * string, Key * informationKey)
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
	Key * key;
	ksRewind (input);
	while ((key = ksNext (input)) != NULL)
	{
		char * newName = elektraMalloc (keyGetNameSize (key) + strlen (string));
		strcpy (newName, string);
		strcat (newName, keyName (key));
		Key * duplicateKey = keyDup (key); // keySetName returns -1 if key was inserted to a keyset before
		int status = keySetName (duplicateKey, newName);
		elektraFree (newName);
		if (status < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not set key name.");
		}
		status = ksAppendKey (result, duplicateKey);
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
 * Example: If root is user/example and the KeySet contains a key with the name user/example/something then
 * the returned KeySet will contain the key /something.
 */
static KeySet * removeRoot (KeySet * original, Key * root, Key * informationKey)
{
	ksRewind (original);
	KeySet * result = ksNew (0, KS_END);
	const char * rootKeyNameString = keyName (root);
	Key * currentKey;
	while ((currentKey = ksNext (original)) != NULL)
	{
		char * currentKeyNameString = elektraMalloc (keyGetNameSize (currentKey));
		if (keyGetName (currentKey, currentKeyNameString, keyGetNameSize (currentKey)) < 0)
		{
			ELEKTRA_ASSERT (false, "ERROR: This should not happen");
			elektraFree (currentKeyNameString);
			ksDel (result);
			return NULL;
		};
		if (keyIsBelow (root, currentKey) || keyCmp (currentKey, root) == 0)
		{
			Key * keyCopy = keyDup (currentKey);
			int retVal;
			if (keyIsBelow (root, currentKey))
			{
				currentKeyNameString = strremove (currentKeyNameString, rootKeyNameString);
				retVal = keySetName (keyCopy, currentKeyNameString);
			}
			else
			{
				// If the root itself is in the keyset then create a special name for it as it would be empty otherwise
				retVal = keySetName (keyCopy, "/root");
			}
			if (retVal < 0)
			{
				elektraFree (currentKeyNameString);
				ksDel (result);
				ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Setting new key name was not possible.");
			}
			ksAppendKey (result, keyCopy);
			elektraFree (currentKeyNameString);
		}
		else
		{
			elektraFree (currentKeyNameString);
			ksDel (result);
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Setting new key name was not possible.");
		}
	}
	return result;
}

/**
 * @brief Compares two keys
 * @retval true if two keys are equal
 * @retval false otherwise
 */
static bool keysAreEqual (Key * a, Key * b)
{
	// Two nothings are not keys and thus false too
	if (a == NULL || b == NULL)
	{
		return false;
	}
	if (keyGetValueSize (a) != keyGetValueSize (b))
	{
		return false;
	}
	if (0 != memcmp (keyValue (a), keyValue (b), keyGetValueSize (a)))
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
static int twoOfThreeExistHelper (Key * checkedKey, Key * keyInFirst, Key * keyInSecond, KeySet * result, bool checkedIsDominant,
				  int baseIndicator, Key * informationKey)
{
	Key * existingKey;
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
		if (checkedIsDominant) // TODO This also happens when there is no conflict
		{
			if (ksAppendKey (result, checkedKey) < 0)
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
				if (ksAppendKey (result, checkedKey) < 0)
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
static bool twoOfThoseKeysAreEqual (Key * checkedKey, Key * keyInFirst, Key * keyInSecond, KeySet * result, bool checkedIsDominant,
				    int baseIndicator, Key * informationKey)
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
				if (ksAppendKey (result, checkedKey) < 0)
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
			if (ksAppendKey (result, keyInSecond) < 0)
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
					if (ksAppendKey (result, checkedKey) < 0)
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
			if (ksAppendKey (result, keyInFirst) < 0)
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
					if (ksAppendKey (result, checkedKey) < 0)
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
static int allExistHelper (Key * checkedKey, Key * keyInFirst, Key * keyInSecond, KeySet * result, bool checkedIsDominant,
			   int baseIndicator, Key * informationKey)
{
	if (keysAreEqual (checkedKey, keyInFirst) && keysAreEqual (checkedKey, keyInSecond))
	{
		/**
		 * append any of the three keys
		 * will be appended multiple times, but that doesn't matter for the result
		 */
		if (ksAppendKey (result, checkedKey) < 0)
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
				if (ksAppendKey (result, checkedKey) < 0)
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
static int checkSingleSet (KeySet * checkedSet, KeySet * firstCompared, KeySet * secondCompared, KeySet * result, bool checkedIsDominant,
			   int baseIndicator, Key * informationKey)
{
	ksRewind (checkedSet);
	ksRewind (firstCompared);
	ksRewind (secondCompared);
	Key * checkedKey;
	while ((checkedKey = ksNext (checkedSet)) != NULL)
	{
		/**
		 * Check if a key with the same name exists
		 * Nothing about values is said yet
		 */
		Key * keyInFirst = ksLookup (firstCompared, checkedKey, 0);
		Key * keyInSecond = ksLookup (secondCompared, checkedKey, 0);
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
				if (ksAppendKey (result, checkedKey) < 0)
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
KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultRoot,
		       int strategy, Key * informationKey)
{
	ELEKTRA_LOG ("cmerge starts with strategy %d", strategy);
	KeySet * result = ksNew (0, KS_END);
	KeySet * ourCropped = removeRoot (our, ourRoot, informationKey);
	KeySet * theirCropped = removeRoot (their, theirRoot, informationKey);
	KeySet * baseCropped = removeRoot (base, baseRoot, informationKey);
	ksRewind (ourCropped);
	ksRewind (theirCropped);
	ksRewind (baseCropped);
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

	checkSingleSet (baseCropped, ourCropped, theirCropped, result, false, 0, informationKey); // base is never dominant
	checkSingleSet (theirCropped, baseCropped, ourCropped, result, theirDominant, 1, informationKey);
	checkSingleSet (ourCropped, theirCropped, baseCropped, result, ourDominant, 2, informationKey);
	if (ksDel (ourCropped) != 0 || ksDel (theirCropped) != 0 || ksDel (baseCropped) != 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Could not delete a key set.");
		return NULL;
	}
	if (getTotalConflicts (informationKey) > 0)
	{
		if (strategy == MERGE_STRATEGY_ABORT)
		{
			ksDel (result);
			ELEKTRA_SET_INTERNAL_ERROR (informationKey, "Abort strategy was set and at least one conflict occured.");
			return NULL;
		}
	}
	KeySet * resultWithRoot = ksNew (0, KS_END);
	prependStringToAllKeyNames (resultWithRoot, result, keyName (resultRoot), informationKey);
	ksDel (result);
	return resultWithRoot;
}
