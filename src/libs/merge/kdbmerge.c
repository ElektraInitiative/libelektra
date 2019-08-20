
#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

int nonOverlapOnlyBaseCounter = 0;  // Conflict where only base is different (it exists) and our and their are empty.
int nonOverlapBaseEmptyCounter = 0; // Conflict where only base is different (it is empty) and our=their (not empty).
int nonOverlapAllExistCounter = 0;  // All three values exist, only base is different
int overlap3different = 0;	  // Only overlap conflicts where all three keys exist and have different values
int overlap1empty = 0;		    // counts overlaps where one set is empty. Is the double of the real amount in the end.

static int getNonOverlapAllExistConflicts (void)
{
	if (nonOverlapAllExistCounter % 3 != 0)
	{
		fprintf (stderr, "This should be a multiple of 3 at the end of each checkSingleSet\n");
		return -1;
	}
	return nonOverlapAllExistCounter / 3;
}

static int getBaseEmptyConflicts (void)
{
	if (nonOverlapBaseEmptyCounter % 2 != 0)
	{
		fprintf (stderr, "This should be a multiple of 2 at the end of each checkSingleSet\n");
		return -1;
	}
	return nonOverlapBaseEmptyCounter / 2;
}

static int getOverlap3different (void)
{
	if (overlap3different % 3 != 0)
	{
		fprintf (stderr, "This should be a multiple of 3 at the end of each checkSingleSet\n");
		return -1;
	}
	return overlap3different / 3;
}

static int getOverlap1empty (void)
{
	if (overlap1empty % 2 != 0)
	{
		fprintf (stderr, "This should be a multiple of 2 at the end of each checkSingleSet\n");
		return -1;
	}
	return overlap1empty / 2;
}

static int getTotalOverlaps (void)
{
	return getOverlap1empty () + getOverlap3different ();
}

static int getTotalNonOverlaps (void)
{
	return getBaseEmptyConflicts () + getNonOverlapAllExistConflicts () + nonOverlapOnlyBaseCounter;
}

static int getTotalConflicts (void)
{
	return getTotalNonOverlaps () + getTotalOverlaps ();
}

/**
 * For debugging only
 * Prints the names of all keys in a key set
 */
static void printKs (KeySet * ks)
{
	Key * cur = 0;
	fprintf (stdout, "DEBUG: Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		fprintf (stdout, "DEBUG: --%s\n", keyName (cur));
	}
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
 *  @retval -1 on error
 *  @retval  0 on success
 */
static int prependStringToAllKeyNames (KeySet * result, KeySet * input, const char * string)
{
	if (input == NULL)
	{
		fprintf (stderr, "input must not be null\n");
		return -1;
	}
	if (result == NULL)
	{
		fprintf (stderr, "result must not be null!\n");
		return -1;
	}
	if (string == NULL)
	{
		fprintf (stderr, "string must not be null!\n");
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
			fprintf (stderr, "Could not set new key name!\n");
			return -1;
		}
		status = ksAppendKey (result, duplicateKey);
		if (status < 0)
		{
			fprintf (stderr, "Could not append key!\n");
			return -1;
		}
	}
	return 0;
}

/**
 * @brief Remove the root from each key of a set
 * @param original the key set from which root will be rmeoved
 * @param root remove this from all the keys
 * @returns a new key set without the root
 *
 * Example: If root is user/example and the KeySet contains a key with the name user/example/something then
 * the returned KeySet will contain the key /something.
 */
static KeySet * removeRoot (KeySet * original, Key * root)
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
				fprintf (stderr, "ERROR in %s: Setting new name was not possible! keySetName returned %d\n", __func__,
					 retVal);
				elektraFree (currentKeyNameString);
				ksDel (result);
				return NULL;
			}
			ksAppendKey (result, keyCopy);
			elektraFree (currentKeyNameString);
		}
		else
		{
			fprintf (stderr,
				 "ERROR in %s: Removing root %s from beginning of key %s is not possible as the current key is not below "
				 "the root. Have you passed correct parameters to elektraMerge?\n",
				 __func__, rootKeyNameString, currentKeyNameString);
			elektraFree (currentKeyNameString);
			ksDel (result);
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
 * checkSingleSet iterates over the elements of the checkedSet key set and puts them into result if the 3-way merge rules are fulfilled
 * and the element is not already in the result key set.
 * It should be called 3 times, each time with a different of our key set, their key set and base key set as checkedSet parameter.
 * Which of the remaining two key sets is firstCompared or secondCompared is irrelevant.
 *
 * @param checkedIsDominant parameter is for the merge strategy. If a conflict occurs and checkedIsDominant is true then the current element
 * of checkedSet is inserted. Consequently, it has to be set to true for exactly one of the three calls of this function.
 *
 * @param baseIndicator indicates which of the three key sets is the base key set. 0 is checkedSet, 1 firstcompared, 2 secondCompared
 *
 * @retval -1 on error
 * @retval 0 on success
 *
 */
static int checkSingleSet (KeySet * checkedSet, KeySet * firstCompared, KeySet * secondCompared, KeySet * result, bool checkedIsDominant,
			   int baseIndicator)
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
			if (keysAreEqual (checkedKey, keyInFirst) && keysAreEqual (checkedKey, keyInSecond))
			{
				/**
				 * append any of the three keys
				 * will be appended multiple times, but that doesn't matter for the result
				 */
				if (ksAppendKey (result, checkedKey) < 0)
				{
					return -1;
				}
			}
			else
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
						nonOverlapAllExistCounter++;
						if (checkedIsDominant)
						{
							// If base is also dominant then append it's key
							if (ksAppendKey (result, checkedKey) < 0)
							{
								return -1;
							}
						}
					}
				}
				else if (keysAreEqual (checkedKey, keyInFirst))
				{
					if (baseIndicator == 0)
					{
						if (ksAppendKey (result, keyInSecond) < 0)
						{
							return -1;
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
							nonOverlapAllExistCounter++;
							if (checkedIsDominant)
							{
								// If base is also dominant then append it's key
								if (ksAppendKey (result, checkedKey) < 0)
								{
									return -1;
								}
							}
						}
					}
				}
				else if (keysAreEqual (checkedKey, keyInSecond))
				{

					if (baseIndicator == 0)
					{
						if (ksAppendKey (result, keyInFirst) < 0)
						{
							return -1;
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
							nonOverlapAllExistCounter++;
							if (checkedIsDominant)
							{
								// If base is also dominant then append it's key
								if (ksAppendKey (result, checkedKey) < 0)
								{
									return -1;
								}
							}
						}
					}
				}
				else
				{
					/**
					 * Overlap conflict case
					 *
					 * The same overlap conflict gets detected three times, once for each of the three invocations of
					 * checkSingleSet. However, only one of those three times is required. Thus use a getter function
					 * that calculates a third.
					 */
					overlap3different++;
					if (checkedIsDominant)
					{
						if (ksAppendKey (result, checkedKey) < 0)
						{
							return -1;
						}
					}
				}
			}
		}
		else if (keyInFirst == NULL && keyInSecond == NULL)
		{
			if (baseIndicator == 0)
			{
				/**
				 * Non-overlap conflict https://www.gnu.org/software/diffutils/manual/html_node/diff3-Merging.html
				 */
				nonOverlapOnlyBaseCounter++;
				if (checkedIsDominant) // currently iterating over base and base strategy is set
				{
					if (ksAppendKey (result, checkedKey) < 0)
					{
						return -1;
					}
				}
			}
			else
			{
				if (ksAppendKey (result, checkedKey) < 0)
				{
					return -1;
				}
			}
		}
		else
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
				fprintf (stderr, "Something went wrong\n");
				return -1;
			}
			if (thisConflict)
			{
				nonOverlapBaseEmptyCounter++;
			}
			if (!keysAreEqual (checkedKey, existingKey))
			{
				// overlap  with single empty
				// This spot is hit twice for a single overlap conflict. Thus calculate half later on.
				overlap1empty++;
				if (checkedIsDominant) // TODO This also happens when there is no conflict
				{
					if (ksAppendKey (result, checkedKey) < 0)
					{
						fprintf (stderr, "Could not append key\n");
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
					nonOverlapBaseEmptyCounter++;
					if (checkedIsDominant)
					{
						if (ksAppendKey (result, checkedKey) < 0)
						{
							return -1;
						}
					}
				}
			}
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
 * @returns the merged key set and NULL on error
 */
KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultRoot,
		       int strategy)
{
	ELEKTRA_LOG ("cmerge starts");
	fprintf (stdout, "cmerge starts with strategy %d\n", strategy);
	nonOverlapOnlyBaseCounter = 0;
	nonOverlapBaseEmptyCounter = 0;
	nonOverlapAllExistCounter = 0;
	overlap3different = 0;
	overlap1empty = 0;
	KeySet * result = ksNew (0, KS_END);
	KeySet * ourCropped = removeRoot (our, ourRoot);
	KeySet * theirCropped = removeRoot (their, theirRoot);
	KeySet * baseCropped = removeRoot (base, baseRoot);
	ksRewind (ourCropped);
	ksRewind (theirCropped);
	ksRewind (baseCropped);
	bool baseDominant = false;
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
	case MERGE_STRATEGY_BASE:
		baseDominant = true;
		break;
	}

	checkSingleSet (baseCropped, ourCropped, theirCropped, result, baseDominant, 0);
	checkSingleSet (theirCropped, baseCropped, ourCropped, result, theirDominant, 1);
	checkSingleSet (ourCropped, theirCropped, baseCropped, result, ourDominant, 2);
	if (ksDel (ourCropped) != 0 || ksDel (theirCropped) != 0 || ksDel (baseCropped) != 0)
	{
		fprintf (stderr, "Could not delete keysets\n");
	}
	if (getTotalConflicts () > 0)
	{
		fprintf (stdout, "Conflict statistic:\n  %2d overlaps\n  %2d non-overlaps\n   --------------\n  %2d total\n",
			 getTotalOverlaps (), getTotalNonOverlaps (), getTotalConflicts ());
		if (strategy == MERGE_STRATEGY_ABORT)
		{
			ksDel (result);
			return NULL;
		}
	}
	KeySet * resultWithRoot = ksNew (0, KS_END);
	prependStringToAllKeyNames (resultWithRoot, result, keyName (resultRoot));
	ksDel (result);
	return resultWithRoot;
}
