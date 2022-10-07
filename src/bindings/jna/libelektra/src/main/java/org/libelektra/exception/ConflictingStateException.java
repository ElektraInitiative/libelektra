package org.libelektra.exception;

import org.libelektra.ErrorCode;
import org.libelektra.KDBException;
import org.libelektra.Key;

public class ConflictingStateException extends KDBException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.CONFLICTING_STATE.getNumber();

  public ConflictingStateException(Key k) {
    super(k);
  }
}
