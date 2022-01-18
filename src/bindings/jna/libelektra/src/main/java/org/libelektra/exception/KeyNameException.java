package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates {@link Key#setName(String)}, {@link Key#setBaseName(String)} or {@link
 * Key#addBaseName(String)} failed because the key name is invalid, the key was inserted in a key
 * set before, the key name is read-only For there where allocation problems in the native library
 */
public class KeyNameException extends KeyException {
  private static final long serialVersionUID = 1L;
}
