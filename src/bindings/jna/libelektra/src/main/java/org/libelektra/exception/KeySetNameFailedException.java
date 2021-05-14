package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates {@link Key#setName(String)} or {@link Key#setBaseName(String)} failed because the key name is
 * invalid, the key was inserted in a key set before, the key name is
 * read-only For there where allocation problems in the native library
 */
public class KeySetNameFailedException extends KeyException
{
	private static final long serialVersionUID = 1L;
}