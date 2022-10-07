package org.libelektra.exception;

import org.libelektra.ErrorCode;
import org.libelektra.Key;

public class OutOfMemoryException extends ResourceException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.OUT_OF_MEMORY.getNumber();

  public OutOfMemoryException(Key k) {
    super(k);
  }
}
