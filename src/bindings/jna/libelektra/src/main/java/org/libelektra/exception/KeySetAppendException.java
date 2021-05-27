package org.libelektra.exception;

import org.libelektra.Key;
import org.libelektra.KeySet;

/**
 * Indicates {@link KeySet#append(KeySet)} or {@link KeySet#append(Key)} failed,
 * which also might have freed the passed key or a key of the passed key set
 */
public class KeySetAppendException extends KeyException
{
	private static final long serialVersionUID = 1L;
}