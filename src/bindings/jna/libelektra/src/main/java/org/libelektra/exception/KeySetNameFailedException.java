package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates {@link Key#setName(String)} or {@link Key#setBaseName(String)} failed because if the key name is
 * invalid, the key was inserted in a key set before or the key name is
 * read-only
 */
public class KeySetNameFailedException extends KeyException {

	private static final long serialVersionUID = 1L;

}