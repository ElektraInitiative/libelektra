package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates a generic exception while calling one of {@link Key}'s methods<br>
 * <br>
 * This exception is related to unrecoverable C API specific errors (primarily allocation problems).
 * All other exceptional states are represented by more specific exceptions. For more detailed
 * information see exceptions in this package as well as a method's individual documentation.
 */
public class KeyException extends RuntimeException {
  private static final long serialVersionUID = 1L;
}
