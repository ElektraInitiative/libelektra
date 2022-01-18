package org.libelektra.exception;

import org.libelektra.Key;

public class PluginMisbehaviorException extends LogicalException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = "C01330";

  public PluginMisbehaviorException(Key k) {
    super(k);
  }
}
