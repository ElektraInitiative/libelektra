package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates a {@link Key key's} underlying native key is of type binary<br>
 * <br>
 * Binary key values are currently not supported by this binding.
 */
public class KeyBinaryTypeNotSupportedException extends KeyException
{

	private static final long serialVersionUID = 1L;
}