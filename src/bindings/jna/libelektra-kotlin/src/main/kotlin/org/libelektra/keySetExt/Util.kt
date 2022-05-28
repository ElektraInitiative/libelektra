package org.libelektra.keySetExt

import org.libelektra.Key
import org.libelektra.KeySet

/**
 * Constructs a KeySet with the given keys
 *
 * @param keys will be added to the new KeySet
 * @return KeySet containing all given keys
 */
fun keySetOf(vararg keys: Key): KeySet {
    return KeySet.create(*keys)
}
