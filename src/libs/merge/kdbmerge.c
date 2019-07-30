
#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static bool metaEqual (Key * a, Key * b, bool semanticallySuffices);
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
 * Places extension before the name of toAppend and puts the result into ks
 */
static int prependAndAppend (KeySet * ks, Key * toAppend, const char * extension)
{
	if (toAppend == NULL)
	{
		fprintf (stderr, "Key that should be appended must not be null!\n");
	}
	if (ks == NULL)
	{
		fprintf (stderr, "Key set where key should be appended must not be null!\n");
	}
	char * newName = elektraMalloc (strlen (keyName (toAppend)) + strlen (extension) + 1);
	strcpy (newName, extension);
	strcat (newName, keyName (toAppend));
	int status = keySetName (toAppend, newName);
	elektraFree (newName);
	if (status < 0)
	{
		return -1;
	}
	status = ksAppendKey (ks, toAppend);
	if (status < 0)
	{
		return -1;
	}
	return 1;
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

/**
 * Checks for each meta information of meta key a if it is in meta key b as well and has the same value
 */
static bool metaEqualHelper (Key * a, Key * b, bool semanticallySuffices)
{
	keyRewindMeta (a);
	keyRewindMeta (b);
	const Key * currentMeta;
	while ((currentMeta = keyNextMeta (a)) != 0)
	{
		const char * currentName = keyName (currentMeta);
		if (currentName == 0 || strncmp (currentName, "", 2) == 0)
		{
			return false;
		}
		const Key * metaInB = keyGetMeta (b, currentName);
		if (metaInB == 0)
		{
			return false;
		}
//		if (!keysAreSyntacticallyEqual (a, b, true))
//		// key dup is only to discard const qualifier
		if (!keysAreSyntacticallyEqual (keyDup(currentMeta), keyDup(metaInB), true))
		{
			// comments may be different
			if (!semanticallySuffices && strcmp (currentName, "comment") != 0)
			{
				return false;
			}
		}
	}
	return true;
}
/**
 * Set semantically true if semantic differences like comments are irrelevant
 */
static bool metaEqual (Key * a, Key * b, bool semanticallySuffices)
{
	// If a set A (meta keys of key a) is a subset of B and B is a subset of A then A == B
	return metaEqualHelper (a, b, semanticallySuffices) && metaEqualHelper (b, a, semanticallySuffices);
}

/**
 * When checking the equality of a regular key, the meta information is relevant as well.
 * As then this function is not called from a successor of the function metaEqual, set callFromMeta false.
 * If checking the equality of a meta key, there is no additional meta information stored.
 * set callFromMeta true in this case (i.e. calling from metaEqual)
 */
static bool keysAreSyntacticallyEqual (Key * a, Key * b, bool callFromMeta)
{
	if (keyGetValueSize (a) != keyGetValueSize (b))
	{
		return false;
	}
	if (0 != memcmp (keyValue (a), keyValue (b), keyGetValueSize (a)))
	{
		return false;
	}
	if ((!callFromMeta) && !metaEqual (a, b, false))
	{
		return false;
	}
	return true;
}

static bool keysAreSemanticallyEqual (Key * a, Key * b)
{
	/**
	 * Opposed to string values binary values can have '\0' inside the value
	 * This is an important property for the semantic equivalence of values
	 * containing whitespace such as 'aa'=='a  a'
	 */
	ssize_t a_size_original = keyGetValueSize (a); // ssize_t is not ANSI C99?!
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

static void conflictHandler (KeySet * result, Key * resultRoot, int strategy, Key * our, Key * their)
{
	/**
	 * our or their is null when a key was changed in one set and deleted in the other
	 * our    their   base     result
	 * key1=2         key1=1
	 *
	 */
	if (result == NULL || resultRoot == NULL)
	{
		fprintf (stderr, "Arguments in conflict handler must not be null!\n");
	}
	conflictCounter++;
	switch (strategy)
	{
	case MERGE_STRATEGY_OUR:
		keyDup (our);
		keyName (resultRoot);
		// See top comment for null check
		if (our != NULL && prependAndAppend (result, keyDup (our), keyName (resultRoot)) < 0)
		{
			fprintf (stderr, "Could not add key to key set\n");
			return;
		}
		break;
	case MERGE_STRATEGY_THEIR:
		if (their != NULL && prependAndAppend (result, keyDup (their), keyName (resultRoot)) < 0)
		{
			fprintf (stderr, "Could not add key to key set\n");
			return;
		}
		break;
	}
}
/**
 * New keys can be added in the our and their keyset.
 * Those keys should also be in the result keyset.
 * An exception is if a key with the same name but different value is added to the our and their keysets.
 * This exception is a conflict case.
 *
 * This covers the "e" cases in the diagram.
 * It adds every key from checkKeySet to result if it's not in compareKeySet with a different value.
 *
 * Returns false on error
 */
#define CURRENTLY_OUR 1
#define CURRENTLY_THEIR 2
static bool addMissingKeys (KeySet * checkKeySet, KeySet * compareKeySet, KeySet * base, KeySet * result, Key * resultRoot, int strategy,
			    int whichIsChecked)
{
	ksRewind (checkKeySet);
	Key * checkKey;
	while ((checkKey = ksNext (checkKeySet)) != NULL)
	{
		/** all the cases are only relevant when the base is null and something is happening in ours or theirs
		 * that is the "e" cases
		 */
		Key * keyInBase = ksLookup (base, checkKey, 0);
		Key * keyInCompare = ksLookup (compareKeySet, checkKey, 0);
		if (keyInBase == NULL)
		{
			if (keyInCompare == NULL)
			{
				if (prependAndAppend (result, keyDup (checkKey), keyName (resultRoot)) < 0)
				{
					fprintf (stderr, "ERROR in %s: Could not add key to keyset\n", __func__);
					return false;
				}
			}
			else
			{
				/**
				 * Add keys that are in our and their keyset with same value only once
				 */
				if (keysAreEqual (checkKey, keyInCompare))
				{
					Key * ourInResult = ksLookup (result, checkKey, 0);
					if (ourInResult == NULL)
					{
						if (prependAndAppend (result, keyDup (checkKey), keyName (resultRoot)) < 0)
						{
							fprintf (stderr, "ERROR in %s: Could not add key to keyset\n", __func__);
							return false;
						}
					}
				}
				else
				{
					/**
					 * This is not an error but only a conflict.
					 * This happens e.g. when base is empty and our and their are different.
					 */
					if (whichIsChecked == CURRENTLY_OUR)
					{
						conflictHandler (result, resultRoot, strategy, checkKey, keyInCompare);
					}
					else if (whichIsChecked == CURRENTLY_THEIR)
					{
						conflictHandler (result, resultRoot, strategy, keyInCompare, checkKey);
					}
				}
			}
		}
		else
		{
			/**
			 * This happens when a key was changed in one set and deleted in the other
			 * our    their   base     result
			 * key1=2         key1=1
			 */
			if (!keysAreEqual (checkKey, keyInBase) && keyInCompare == NULL)
			{
				if (whichIsChecked == CURRENTLY_OUR)
				{
					conflictHandler (result, resultRoot, strategy, checkKey, keyInCompare);
				}
				else if (whichIsChecked == CURRENTLY_THEIR)
				{
					conflictHandler (result, resultRoot, strategy, keyInCompare, checkKey);
				}
			}
		}
	}
	return true;
}

/**
 * If there is a conflict returns the number of conflicts as negative number, else return the size of the resulting key set
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
	Key * base_key;
	while ((base_key = ksNext (baseCropped)) != NULL)
	{
		/**
		 * Check if a key with the same name exists
		 * Nothing about values is said yet
		 */
		Key * baseInOur = ksLookup (ourCropped, base_key, 0);
		Key * baseInTheir = ksLookup (theirCropped, base_key, 0);
		if (baseInOur != NULL && baseInTheir != NULL)
		{
			// Value of Base is irrelevant for this
			if (keysAreEqual (baseInOur, baseInTheir))
			{
				// Any of the two is ok to append
				if (prependAndAppend (result, keyDup (baseInOur), keyName (resultRoot)) < 0)
				{
					return NULL;
				}
			}
			else if (keysAreEqual (base_key, baseInOur))
			{
				if (prependAndAppend (result, keyDup (baseInTheir), keyName (resultRoot)) < 0)
				{
					return NULL;
				}
			}
			else if (keysAreEqual (base_key, baseInTheir))
			{
				if (prependAndAppend (result, keyDup (baseInOur), keyName (resultRoot)) < 0)
				{
					return NULL;
				}
			}
			else
			{
				conflictHandler (result, resultRoot, strategy, baseInOur, baseInTheir);
			}
		}
	}
	if (!(addMissingKeys (ourCropped, theirCropped, baseCropped, result, resultRoot, strategy, CURRENTLY_OUR) &&
	      addMissingKeys (theirCropped, ourCropped, baseCropped, result, resultRoot, strategy, CURRENTLY_THEIR)))
	{
		fprintf (stderr, "An error happened adding missing keys\n");
		return NULL;
	}
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
	return result;
}
