
#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// static bool metaEqual (Key * a, Key * b, bool semanticallySuffices);
static bool keysAreSyntacticallyEqual (Key * a, Key * b, bool);
int conflictCounter = 0;

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

/**
 * Performs normalization for basic semantic equivalence checks
 * 1. Removes all whitespaces
 * 2. Removes trailing zeros from numbers
 *
 * This modifies the contents of the parameter to_normalize and does not create a copy itself.
 * Normalizes up to size in memory.
 * Returns the number of elements in the normalized version.
 */
static size_t normalize (void * to_normalize, size_t * size)
{
	// Each character is checked alone
	char * as_int = to_normalize;
	int removed_count = 0;
	size_t i = 0;
	while (i < *size)
	{
		/**
		 * 0123456789ab         012345
		 * ___abc_abc__ becomes abcabc
		 */
		if (isblank (as_int[i]))
		{
			if (i < *size - 1)
			{
				memmove (&as_int[i], &as_int[i + 1], *size - i);
			}
			removed_count += 1;
		}
		else
		{
			// Only here! memmove => no increase of i
			i++;
		}
	}
	size_t reducedSize = *size - removed_count;
	*size = reducedSize;
	return reducedSize;
}

///**
// * Checks for each meta information of meta key a if it is in meta key b as well and has the same value
// */
// static bool metaEqualHelper (Key * a, Key * b, bool semanticallySuffices)
//{
//	keyRewindMeta (a);
//	keyRewindMeta (b);
//	const Key * currentMeta;
//	while ((currentMeta = keyNextMeta (a)) != 0)
//	{
//		const char * currentName = keyName (currentMeta);
//		if (currentName == 0 || strncmp (currentName, "", 2) == 0)
//		{
//			return false;
//		}
//		const Key * metaInB = keyGetMeta (b, currentName);
//		if (metaInB == 0)
//		{
//			return false;
//		}
//		//		if (!keysAreSyntacticallyEqual (a, b, true))
//		//		// key dup is only to discard const qualifier
//		if (!keysAreSyntacticallyEqual (keyDup (currentMeta), keyDup (metaInB), true))
//		{
//			// comments may be different
//			if (!semanticallySuffices && strcmp (currentName, "comment") != 0)
//			{
//				return false;
//			}
//		}
//	}
//	return true;
//}
///**
// * Set semantically true if semantic differences like comments are irrelevant
// */
// static bool metaEqual (Key * a, Key * b, bool semanticallySuffices)
//{
//	// If a set A (meta keys of key a) is a subset of B and B is a subset of A then A == B
//	return metaEqualHelper (a, b, semanticallySuffices) && metaEqualHelper (b, a, semanticallySuffices);
//}

/**
 * When checking the equality of a regular key, the meta information is relevant as well.
 * As then this function is not called from a successor of the function metaEqual, set callFromMeta false.
 * If checking the equality of a meta key, there is no additional meta information stored.
 * set callFromMeta true in this case (i.e. calling from metaEqual)
 */
static bool keysAreSyntacticallyEqual (Key * a, Key * b, bool callFromMeta)
{
	if (callFromMeta)
	{
		// only here for a compiler warning;
	}
	if (keyGetValueSize (a) != keyGetValueSize (b))
	{
		return false;
	}
	if (0 != memcmp (keyValue (a), keyValue (b), keyGetValueSize (a)))
	{
		return false;
	}
	// Forget about metadata for the moment
	//	if ((!callFromMeta) && !metaEqual (a, b, false))
	//	{
	//		return false;
	//	}
	return true;
}

static bool keysAreSemanticallyEqual (Key * a, Key * b)
{
	/**
	 * Opposed to string values binary values can have '\0' inside the value
	 * This is an important property for the semantic equivalence of values
	 * containing whitespace such as 'aa'=='a  a'
	 */
	ssize_t a_size_original = keyGetValueSize (a); // ssize_t is not ANSI C99?
	ssize_t b_size_original = keyGetValueSize (b);
	if (a_size_original < 0 || b_size_original < 0)
	{
		fprintf (stderr, "ERROR in %s", __func__);
		return false;
	}
	size_t a_size = (size_t) a_size_original;
	size_t b_size = (size_t) b_size_original;
	void * a_value = elektraMalloc (a_size); // Could contain string but also binary data
	void * b_value = elektraMalloc (b_size);
	if (a_value == 0 || b_value == 0)
	{
		fprintf (stderr, "ERROR in %s", __func__);
		return false;
	}
	memcpy (a_value, keyValue (a), a_size);
	memcpy (b_value, keyValue (b), b_size);
	size_t new_a_size = normalize (a_value, &a_size);
	size_t new_b_size = normalize (b_value, &b_size);
	if (new_a_size != new_b_size) return false;
	bool result = false; // Don't return immediately because of free
	if (memcmp (a_value, b_value, new_a_size) == 0)
	{
		result = true;
	}
	else
	{
		result = false;
	}
	elektraFree (a_value);
	elektraFree (b_value);
	return result;
}

static bool keysAreEqual (Key * a, Key * b)
{
	/**
	 * In our case syntactic equivalence implies semantic equivalence but not vice versa.
	 *
	 * (...) the || operator guarantees left-to-right evaluation (...) If the first operand
	 * compares unequal to 0, the second operand is not evaluated (C99 standard)
	 *
	 * As a result, a costly semantic analysis can be omitted.
	 */
	return (keysAreSyntacticallyEqual (a, b, false) || keysAreSemanticallyEqual (a, b));
}


/**
 * checkSingleSet iterates over the elements of the checkedSet key set and puts them into result if the 3-way merge rules are fulfilled
 * and the element is not already in the result key set.
 * It should be called 3 times, each time with a different of our key set, their key set and base key set as checkedSet parameter.
 * Which of the remaining two key sets is firstCompared or secondCompared is irrelevant.
 * The checkedIsDominant parameter is for the merge strategy. If a conflict occurs and checkedIsDominant is true then the current element
 * of checkedSet is inserted. Consequently, it has to be set to true for exactly one of the three calls of this function.
 *
 * This returns -1 on error and 0 if successful.
 *
 */
int checkSingleSet (KeySet * checkedSet, KeySet * firstCompared, KeySet * secondCompared, KeySet * result, bool checkedIsDominant)
{
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
				// append any of the three keys
				if (ksAppendKey (result, checkedKey) < 0)
				{
					return -1;
				}
			}
			else if (keysAreEqual (checkedKey, keyInFirst))
			{
				if (ksAppendKey (result, keyInSecond) < 0)
				{
					return -1;
				}
			}
			else if (keysAreEqual (checkedKey, keyInSecond))
			{
				if (ksAppendKey (result, keyInFirst) < 0)
				{
					return -1;
				}
			}
			else
			{
				// Conflict case
				if (checkedIsDominant)
				{
					if (ksAppendKey (result, checkedKey) < 0)
					{
						return -1;
					}
				}
			}
		}
		else if (keyInFirst == NULL && keyInSecond == NULL)
		{
			if (ksAppendKey (result, checkedKey) < 0)
			{
				return -1;
			}
		}
		else
		{
			Key * existingKey;
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
				return -1;
			}
			if (ksAppendKey (result, existingKey) < 0)
			{
				return -1;
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
	conflictCounter = 0;
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
		ourDominant = true;
		break;
	case 4:
		theirDominant = true;
		break;
	case 5:
		baseDominant = true;
		break;
	}

	checkSingleSet (baseCropped, ourCropped, theirCropped, result, baseDominant);
	checkSingleSet (theirCropped, baseCropped, ourCropped, result, theirDominant);
	checkSingleSet (ourCropped, theirCropped, baseCropped, result, ourDominant);
	ELEKTRA_LOG ("Resulting keyset of cmerge has size %ld. There were %d conflicts.\n", ksGetSize (result), conflictCounter);
	ksDel (ourCropped);
	ksDel (theirCropped);
	ksDel (baseCropped);
	if (conflictCounter > 0)
	{
		fprintf (stdout, "There are %d conflicts\n", conflictCounter);
		if (strategy == MERGE_STRATEGY_ABORT)
		{
			return NULL;
		}
	}
	KeySet * resultWithRoot = ksNew (0, KS_END);
	size_t rootNameSize = keyGetNameSize (resultRoot);
	char * string = elektraMalloc (rootNameSize);
	keyGetName (resultRoot, string, rootNameSize);
	prependStringToAllKeyNames (resultWithRoot, result, keyName (resultRoot));
	ksDel (result);
	return resultWithRoot;
}
