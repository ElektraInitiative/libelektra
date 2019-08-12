
#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

int nonOverlapConflictCounter = 0;
int overlap3different = 0; // Only overlap conflicts where all three keys exist and have different values
// always access with getter
int overlap1empty = 0; // counts overlaps where one set is empty. Is the double of the real amount in the end.

int getOverlap3different (void)
{
	if (overlap3different % 3 != 0)
	{
		fprintf (stderr, "This should be a multiple of 3 at the end of each checkSingleSet\n");
		return -1;
	}
	return overlap3different / 3;
}

int getOverlap1empty (void)
{
	if (overlap1empty % 2 != 0)
	{
		fprintf (stderr, "This should be a multiple of 2 at the end of each checkSingleSet\n");
		return -1;
	}
	return overlap1empty / 2;
}

int getTotalOverlaps (void)
{
	return getOverlap1empty () + getOverlap3different ();
}
int getTotalConflicts (void)
{
	return nonOverlapConflictCounter + getTotalOverlaps ();
}

void printKs (KeySet * ks)
{
	Key * cur = 0;
	fprintf (stdout, "DEBUG: Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		fprintf (stdout, "DEBUG: --%s\n", keyName (cur));
	}
}

char * strremove (char * string, const char * sub)
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
 * This is in contrast to the removeRoots function
 *  Returns -1 on error, 0 on success
 */
int prependStringToAllKeyNames (KeySet * result, KeySet * input, const char * string)
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
		char * newName = elektraMalloc (strlen (keyName (key)) + strlen (string) + 1);
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

static KeySet * removeRoots (KeySet * original, Key * root)
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
			return NULL;
		};
		if (!keyIsBelow (root, currentKey))
		{
			fprintf (stderr,
				 "ERROR in %s: Removing root %s from beginning of key %s is not possible as the current key is not below "
				 "the "
				 "root. Have you passed correct parameters to kdbMerge?\n",
				 __func__, rootKeyNameString, currentKeyNameString);
			return NULL;
		}
		currentKeyNameString = strremove (currentKeyNameString, rootKeyNameString);
		Key * keyCopy = keyDup (currentKey);
		int retVal = keySetName (keyCopy, currentKeyNameString);
		if (retVal < 0)
		{
			fprintf (stderr, "ERROR in %s: Setting new name was not possible! keySetName returned %d\n", __func__, retVal);
			return NULL;
		}
		elektraFree (currentKeyNameString);
		ksAppendKey (result, keyCopy);
	}
	return result;
}

static bool keysAreEqual (Key * a, Key * b)
{
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
 * The checkedIsDominant parameter is for the merge strategy. If a conflict occurs and checkedIsDominant is true then the current element
 * of checkedSet is inserted. Consequently, it has to be set to true for exactly one of the three calls of this function.
 *
 * checkedIsBase must be true when the checkedSet parameter is the base set of the three way merge.
 *
 * This returns -1 on error and 0 if successful.
 *
 */
int checkSingleSet (KeySet * checkedSet, KeySet * firstCompared, KeySet * secondCompared, KeySet * result, bool checkedIsDominant,
		    bool checkedIsBase)
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
			printf ("all exist\n");
			if (keysAreEqual (checkedKey, keyInFirst) && keysAreEqual (checkedKey, keyInSecond))
			{
				printf ("A\n");
				// append any of the three keys
				if (ksAppendKey (result, checkedKey) < 0)
				{
					return -1;
				}
			}
			else if (keysAreEqual (checkedKey, keyInFirst))
			{
				printf ("b\n");
				if (ksAppendKey (result, keyInSecond) < 0)
				{
					return -1;
				}
			}
			else if (keysAreEqual (checkedKey, keyInSecond))
			{
				printf ("c\n");
				if (ksAppendKey (result, keyInFirst) < 0)
				{
					return -1;
				}
			}
			else
			{
				/**
				 * Conflict case
				 *
				 * The same non-overlap conflict gets detected three times, once for each of the three invocations of
				 * checkSingleSet. However, only one of those three times is required. Thus use a getter function that
				 * calculates a third.
				 */
				overlap3different++;
				if (checkedIsDominant)
				{
					printf ("appending key\n");
					if (ksAppendKey (result, checkedKey) < 0)
					{
						return -1;
					}
				}
			}
		}
		else if (keyInFirst == NULL && keyInSecond == NULL)
		{
			printf ("both other keys are null\n");
			if (checkedIsBase)
			{
				/**
				 * Non-overlap conflict https://www.gnu.org/software/diffutils/manual/html_node/diff3-Merging.html
				 */
				nonOverlapConflictCounter++;
				if (checkedIsDominant) // currently iterating over base and base strategy is set
				{
					printf ("checked is dominant\n");
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
			printf ("wop\n");
			Key * existingKey;
			if (keyInFirst != NULL)
			{
				printf ("first\n");
				existingKey = keyInFirst;
			}
			else if (keyInSecond != NULL)
			{
				printf ("second\n");
				existingKey = keyInSecond;
			}
			else
			{
				return -1;
			}
			if (keysAreEqual (checkedKey, existingKey))
			{
				printf ("equal key thingy\n");
			}
			else
			{
				// overlap  with single empty
				// This spot is hit twice for a single overlap conflict. Thus calculate half later on.
				overlap1empty++;
			}
			if (checkedIsDominant)
			{
				printf ("checked Is dominant\n");
				if (ksAppendKey (result, checkedKey) < 0)
				{
					fprintf (stderr, "Could not append key\n");
					return -1;
				}
			}
		}
	}
	return 0;
}


/**
 * Returns merged key set
 */
KeySet * kdbMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultRoot,
		   int strategy)
{
	ELEKTRA_LOG ("cmerge starts");
	fprintf (stdout, "cmerge starts with strategy %d\n", strategy);
	nonOverlapConflictCounter = 0;
	overlap3different = 0;
	overlap1empty = 0;
	KeySet * result = ksNew (0, KS_END);
	KeySet * ourCropped = removeRoots (our, ourRoot);
	KeySet * theirCropped = removeRoots (their, theirRoot);
	KeySet * baseCropped = removeRoots (base, baseRoot);
	ksRewind (ourCropped);
	ksRewind (theirCropped);
	ksRewind (baseCropped);
	bool baseDominant = false;
	bool ourDominant = false;
	bool theirDominant = false;
	switch (strategy)
	{
	case 3:
		printf ("our dominant\n");
		ourDominant = true;
		break;
	case 4:
		printf ("their dominant\n");
		theirDominant = true;
		break;
	case 5:
		printf ("base dominant\n");
		baseDominant = true;
		break;
	}

	checkSingleSet (baseCropped, ourCropped, theirCropped, result, baseDominant, true);
	checkSingleSet (theirCropped, baseCropped, ourCropped, result, theirDominant, false);
	checkSingleSet (ourCropped, theirCropped, baseCropped, result, ourDominant, false);
	if (ksDel (ourCropped) != 0 || ksDel (theirCropped) != 0 || ksDel(baseCropped) != 0) {
		fprintf(stderr, "Could not delete keysets\n");
	}
	if (getTotalConflicts () > 0)
	{
		fprintf (stdout, "Conflict statistic:\n  %2d overlaps\n  %2d non-overlaps\n   --------------\n  %2d total\n",
			 getTotalOverlaps (), nonOverlapConflictCounter, getTotalConflicts ());
		if (strategy == MERGE_STRATEGY_ABORT)
		{
			return NULL;
		}
	}
	KeySet * resultWithRoot = ksNew (0, KS_END);
	prependStringToAllKeyNames (resultWithRoot, result, keyName (resultRoot));
	ksDel (result);
	return resultWithRoot;
}
