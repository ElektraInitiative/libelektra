
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

void printKs (KeySet * ks)
{
	Key * cur = 0;
	printf ("Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		printf ("--%s\n", keyName (cur));
	}
}

char * strremove (char * str, const char * sub)
{
	size_t len = strlen (sub);
	if (len > 0)
	{
		char * p = str;
		while ((p = strstr (p, sub)) != NULL)
		{
			memmove (p, p + len, strlen (p + len) + 1);
		}
	}
	return str;
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
 * Normalizes up to size in memory
 */
static void normalize (void * to_normalize, size_t * size)
{
	printf ("In normalize\n");
	printf("Normalizing >%s< with size %ld\n", (char *) to_normalize, *size);
	// Each character is checked alone
	char * as_int = to_normalize;
	int removed_count = 0;
	size_t i = 0;
	while (i < *size)
	{
		printf ("%ld: Character in normalize is as number %d which represents >%c<\n", i, as_int[i], as_int[i]);
		/**
		 * 0123456789ab         012345
		 * ___abc_abc__ becomes abcabc
		 */
		if (isblank (as_int[i]))
		{
			if (i < *size - 1) {
				memmove (&as_int[i], &as_int[i+1], *size - i);
			}
			removed_count += 1;
		} else {
			// memmove => don't increase!
			i++;
		}
	}
	size_t reducedSize = *size - removed_count;
	*size = reducedSize;
	printf("Normalized to >%s< with %ld\n", (char *) to_normalize, reducedSize);
}

static bool valueIsEqual (Key * a, Key * b)
{
	if (keyGetValueSize (a) != keyGetValueSize (b))
	{
		return false;
	}
	return 0 == memcmp (keyValue (a), keyValue (b), keyGetValueSize (a));
}

static bool metadataIsEqual (Key * a, Key * b)
{
	keyValue (a); // Only here to omit compiler warnings
	keyValue (b); // Only here to omit compiler warnings
	return true;
}

static bool keysAreSyntacticallyEqual (Key * a, Key * b)
{
	bool test = valueIsEqual (a, b) && metadataIsEqual (a, b);
	return test;
}

static bool keysAreSemanticallyEqual (Key * a, Key * b)
{
	printf ("In semantic check\n");
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
	printf ("value a is %s and b is %s\n", (char *) a_value, (char *) b_value);
	normalize (a_value, &a_size);
	normalize (b_value, &b_size);
	printf ("After normalization value a is %s and b is %s\n", (char *) a_value, (char *) b_value);

	return false;
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
	return (keysAreSyntacticallyEqual (a, b) || keysAreSemanticallyEqual (a, b));
	//	return keysAreSyntacticallyEqual (a, b);
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
 * Returns false on conflict
 */
static bool addMissingKeys (KeySet * checkKeySet, KeySet * compareKeySet, KeySet * base, KeySet * result, Key * resultRoot)
{
	ksRewind (checkKeySet);
	Key * checkKey;
	while ((checkKey = ksNext (checkKeySet)) != NULL)
	{
		/** all the cases are only relevant when the base is null and something is happening in ours or theirs
		 * that is the "e" cases
		 */
		if (ksLookup (base, checkKey, 0) == NULL)
		{
			Key * foundKey = ksLookup (compareKeySet, checkKey, 0);
			if (foundKey == NULL)
			{
				if (prependAndAppend (result, keyDup (checkKey), keyName (resultRoot)) < 0)
				{
					return false;
				}
			}
			else
			{
				/**
				 * Add keys that are in our and their keyset with same value only once
				 */
				if (keysAreEqual (checkKey, foundKey))
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
					fprintf (stderr, "ERROR in %s: Conflict due to two different values\n", __func__);
					return false;
				}
			}
		}
	}
	return true;
}

KeySet * kdbMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultRoot)
{
	ELEKTRA_LOG ("cmerge starts");
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
		 * Nothing about values or metadata is said yet
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
				return NULL;
			}
		}
	}
	if (!(addMissingKeys (ourCropped, theirCropped, baseCropped, result, resultRoot) &&
	      addMissingKeys (theirCropped, ourCropped, baseCropped, result, resultRoot)))
	{
		return NULL;
	}
	ELEKTRA_LOG ("Resulting keyset of cmerge has size %ld", ksGetSize (result));
	return result;
}
