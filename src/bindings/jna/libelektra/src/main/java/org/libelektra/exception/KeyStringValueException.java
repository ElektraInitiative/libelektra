package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates a {@link Key key's} underlying native key value is not of type string, and therefore is
 * not compatible with the invoked functionality raising this exception
 */
public class KeyStringValueException extends IllegalStateException {
  private static final long serialVersionUID = 1L;
}
