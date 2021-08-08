package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates that an already released {@link Key} has been accessed
 */
public class KeyReleasedException extends IllegalStateException
{
	private static final long serialVersionUID = 1L;
}