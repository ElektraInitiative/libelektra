package org.libelektra.exception;

import org.libelektra.KDB;

/** Indicates that an already closed {@link KDB} session has been accessed */
public class KDBClosedException extends IllegalStateException {
  private static final long serialVersionUID = 1L;
}
