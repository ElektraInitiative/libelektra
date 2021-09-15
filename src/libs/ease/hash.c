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
 * Calculate a sha256 hash of a KeySet. The keys' values are ignored.
 * @param hash A pointer to a uint8_t. This will contain the hash.
 * @param inputKs The KeySet to hash.
 * @param parentKey The Key below which all the relevant keys are. Keys that are not below @p parentKey are ignored.
 * @retval false If an error occurred.
 * @retval true If the computation was successful
 */
kdb_boolean_t computeSha256OfKeySetWithoutValues (char hash_string[65], KeySet * inputKs, Key * parentKey) {
	ksRewind (inputKs);
	KeySet * ksCopy = ksDup(inputKs);
	if(ksCopy == NULL) {
		return false;
	}
	KeySet * onlyBelowParentKey = ksCut(ksCopy, parentKey);
	if(onlyBelowParentKey == NULL) {
		return false;
	}

	uint8_t hash[SIZE_OF_SHA_256_HASH];

	// Initialize sha_256 for streaming
	struct Sha_256 sha_256;
	sha_256_init(&sha_256, hash);

	// Loop through all keys, feed full key name and meta keys + values into sha_256_write().
	Key * currentKey;
	ksRewind(onlyBelowParentKey);
	while ((currentKey = ksNext (onlyBelowParentKey)) != NULL) {
		sha_256_write (&sha_256, keyName(currentKey), strlen(keyName(currentKey)));
		KeySet * currentMetaKeys = keyMeta(currentKey);
		Key * currentMetaKey;
		while ((currentMetaKey = ksNext (currentMetaKeys))) {
			sha_256_write (&sha_256, keyName(currentMetaKey), strlen(keyName(currentMetaKey)));
			sha_256_write (&sha_256, keyString(currentMetaKey), strlen(keyString(currentMetaKey)));
		}
	}

	hash_to_string(hash_string, hash);

	sha_256_close(&sha_256);
	ksDel(ksCopy);
	ksDel(onlyBelowParentKey);

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
