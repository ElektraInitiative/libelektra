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
 * Note that the keys' values are ignored.
 *
 * @param hash A pointer to a uint8_t. This will contain the hash.
 * @param inputKs The KeySet for the application.
 * @param parentKey The Key below which all the relevant keys are. Keys that are not below @p parentKey are ignored.
 * @retval false If an error occurred.
 * @retval true If the computation was successful.
 */
kdb_boolean_t calculateSpecificationToken (char * hash_string, KeySet * specKs, KeySet * fullKs) {
	// Initialize sha_256 for streaming
	uint8_t hash[SIZE_OF_SHA_256_HASH];
	struct Sha_256 sha_256;
	sha_256_init(&sha_256, hash);

	/**
	 * Loop through all keys from the specificationKs:
	 * 1. Remove namespace from key name, if any.
	 * 2. For each key, perform a cascading lookup within the fullKs.
	 * 3. With the result from 2. : Feed full key name and meta keys + values into sha_256_write().
 	*/
	Key * currentKey;
	ksRewind(specKs);
	while ((currentKey = ksNext (specKs)) != NULL) {
		if (keyGetNamespace (currentKey) != KEY_NS_SPEC
		    && keyGetNamespace (currentKey) != KEY_NS_CASCADING) {
			// TODO: add error message
			return false;
		}

		Key * currentKeyCascading = keyDup (currentKey, KEY_CP_ALL);
		keySetNamespace (currentKeyCascading, KEY_NS_CASCADING);
		ksRewind(fullKs);
		Key * keyFromCascadingLookup = ksLookup (fullKs, currentKeyCascading, 0);

		fprintf(stdout, "sha_256_write %s \n", keyName(currentKeyCascading));
		sha_256_write (&sha_256, keyName(currentKeyCascading), keyGetNameSize(currentKeyCascading));
		KeySet * currentMetaKeys = keyMeta(keyFromCascadingLookup);
		ksRewind(currentMetaKeys);
		Key * currentMetaKey;
		while ((currentMetaKey = ksNext (currentMetaKeys)) != NULL) {
			Key * currentMetaKeyCascading = keyDup(currentMetaKey, KEY_CP_ALL);
			keySetNamespace (currentMetaKeyCascading, KEY_NS_CASCADING);
			fprintf(stdout, "sha_256_write %s \n", keyName(currentMetaKeyCascading));
			sha_256_write (&sha_256, keyName(currentMetaKeyCascading), keyGetNameSize (currentMetaKey));
			fprintf(stdout, "sha_256_write %s \n", keyString(currentMetaKey));
			sha_256_write (&sha_256, keyString(currentMetaKey), keyGetValueSize(currentMetaKey));
			keyDel(currentMetaKeyCascading);
		}

		keyDel(currentKeyCascading);
	}

	sha_256_close(&sha_256);
    	hash_to_string(hash_string, hash);

	fprintf(stdout, "sha256 hash is %s \n", hash_string);

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
