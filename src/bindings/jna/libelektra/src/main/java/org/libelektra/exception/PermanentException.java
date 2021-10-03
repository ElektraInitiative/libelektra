package org.libelektra.exception;

import org.libelektra.KDBException;
import org.libelektra.Key;

public abstract class PermanentException extends KDBException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C01000";

  protected PermanentException(Key k) {
    super(k);
  }
}
