package org.libelektra.keySetExt

import org.libelektra.Key
import org.libelektra.KeySet

/**
 * @param keyName name of the key to lookup in the KeySet
 * @return the key, if found
 * @throws NoSuchElementException if the key is not found
 */
fun KeySet.lookupOrThrow(keyName: String): Key = lookup(keyName).orElseThrow()

/**
 * @param keyName name of the key to lookup in the KeySet
 * @return the key, if found or null otherwise
 */
fun KeySet.lookupOrNull(keyName: String): Key? = lookup(keyName).orElse(null)

/**
 * @param keyName name of the key to lookup in the KeySet
 * @return the key, if found or null otherwise
 */
operator fun KeySet.get(keyName: String): Key? = lookupOrNull(keyName)
