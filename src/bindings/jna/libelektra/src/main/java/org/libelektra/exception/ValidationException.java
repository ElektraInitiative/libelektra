package org.libelektra.exception;

import org.libelektra.KDBException;
import org.libelektra.Key;

public abstract class ValidationException extends KDBException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C03000";

  protected ValidationException(Key k) {
    super(k);
  }
}
