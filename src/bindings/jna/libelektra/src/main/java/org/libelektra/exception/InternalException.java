package org.libelektra.exception;

import org.libelektra.Key;

public class InternalException extends LogicalException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C01310";

  public InternalException(Key k) {
    super(k);
  }
}
