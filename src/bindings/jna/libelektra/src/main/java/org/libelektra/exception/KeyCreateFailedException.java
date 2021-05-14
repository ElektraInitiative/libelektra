package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates {@link Key#create(String, Key...)} or
 * {@link Key#create(String, Object, Key...)} failed because the key name is
 * invalid or there where allocation problems in the native library.
 */
public class KeyCreateFailedException extends KeyException
{

	private static final long serialVersionUID = 1L;
}