package org.libelektra.exception;

import org.libelektra.Key;

public class SemanticValidationException extends ValidationException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C03200";

  public SemanticValidationException(Key k) {
    super(k);
  }
}
