/**
 * @file
 *
 * @brief Provides functions to hash Elektra data structures.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbease.h>
#include <kdbtypes.h>

#include <string.h>
#include <stdio.h>

#include "sha-256.h"

static void hash_to_string(char string[65], const uint8_t hash[32]);

/**
 * Calculate a specification token for the KeySet of an application.
 *
 * The KeySet of an application is identified as all keys below the applications root key.
 *
 * @pre The parentKey must have the correct namespace. E.g. If only keys from the spec:/ should be considered for the token calculation,
 * pass a key with KEY_NS_SPEC.
 *
 * @param hash_string A string. After successful execution this will contain the hash as hex-string.
 * @param ks The KeySet for the application.
 * @param parentKey The Key below which all the relevant keys are. Keys that are not below @p parentKey are ignored. The key's namespace is important (see preconditions)
 * @retval false If an error occurred.
 * @retval true If the computation was successful.
 */
kdb_boolean_t calculateSpecificationToken (char * hash_string, KeySet * ks, Key * parentKey) {
	// Initialize sha_256 for streaming
	uint8_t hash[SIZE_OF_SHA_256_HASH];
	struct Sha_256 sha_256;
	sha_256_init(&sha_256, hash);

	//TODO: use parentKey for error reporting!


	// Duplicate ks, then cut out parentKey and all keys below. These are the ones we take into account for token calculation.
	KeySet * dupKs = ksDup (ks);
	KeySet * cutKs = ksCut (dupKs, parentKey);

	/**
	 * Loop through all keys relevant for token calculation.
 	*/
	Key * currentKey;
	ksRewind(cutKs);
	while ((currentKey = ksNext (cutKs)) != NULL) {
		Key * cascadingKey = keyDup (currentKey, KEY_CP_NAME);
		/**
		 * Key namespace is ignored for token calculation (via setting it to cascading).
		 * Reason: Different callers of this function pass the keys for token calculation using different namespaces,
		 * but the specification they pass is actually the same.
		 * (e.g. tools/kdb/gen.cpp passes keys in cascading namespace while src/libs/highlevel/elektra.c passes keys in spec namespace).
		 */
		keySetNamespace (cascadingKey, KEY_NS_CASCADING);
		// Feed key name into sha_256_write()
		sha_256_write (&sha_256, keyName(cascadingKey), keyGetNameSize(cascadingKey));
		// Note: The value of the key itself is not relevant / part of specification. Only the key's name + its metadata!

		KeySet * currentMetaKeys = keyMeta(currentKey);
		Key * currentMetaKey;
		ksRewind(currentMetaKeys);
		// Feed name + values from meta keys into sha_256_write().
		while ((currentMetaKey = ksNext (currentMetaKeys)) != NULL) {
			sha_256_write (&sha_256, keyName(currentMetaKey), keyGetNameSize (currentMetaKey));
			sha_256_write (&sha_256, keyString(currentMetaKey), keyGetValueSize(currentMetaKey));
		}

		keyDel(cascadingKey);
	}

	sha_256_close(&sha_256);
    	hash_to_string(hash_string, hash);

	ksDel(dupKs);
	ksDel(cutKs);

	return true;
}

/**
 * Convert hash array to hex string
 * Copied from https://github.com/amosnier/sha-2/blob/f0d7baf076207b943649e68946049059f018c10b/test.c
 * @param string A string array of length 65 (64 characters + \0)
 * @param hash The input hash array
 */
static void hash_to_string(char string[65], const uint8_t hash[32])
{
	size_t i;
	for (i = 0; i < 32; i++) {
		string += sprintf(string, "%02x", hash[i]);
	}
}
