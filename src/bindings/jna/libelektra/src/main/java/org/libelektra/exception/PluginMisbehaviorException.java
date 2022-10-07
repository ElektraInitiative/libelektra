package org.libelektra.exception;

import org.libelektra.ErrorCode;
import org.libelektra.Key;

public class PluginMisbehaviorException extends LogicalException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.PLUGIN_MISBEHAVIOR.getNumber();

  public PluginMisbehaviorException(Key k) {
    super(k);
  }
}
