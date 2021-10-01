package org.libelektra.exception;

import org.libelektra.Key;

public class ResourceException extends PermanentException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C01100";

  public ResourceException(Key k) {
    super(k);
  }
}
