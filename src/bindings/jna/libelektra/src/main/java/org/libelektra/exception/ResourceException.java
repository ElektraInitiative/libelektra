package org.libelektra.exception;

import org.libelektra.ErrorCode;
import org.libelektra.Key;

public class ResourceException extends PermanentException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.RESOURCE.getNumber();

  public ResourceException(Key k) {
    super(k);
  }
}
