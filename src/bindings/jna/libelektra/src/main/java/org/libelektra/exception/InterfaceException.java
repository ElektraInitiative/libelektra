package org.libelektra.exception;

import org.libelektra.ErrorCode;
import org.libelektra.Key;

public class InterfaceException extends LogicalException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.INTERFACE.getNumber();

  public InterfaceException(Key k) {
    super(k);
  }
}
