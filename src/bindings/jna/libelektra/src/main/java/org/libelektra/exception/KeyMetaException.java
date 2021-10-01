package org.libelektra.exception;

import org.libelektra.Key;

/**
 * Indicates {@link Key#copyMeta(Key, String)}, {@link Key#copyAllMeta(Key)}, {@link
 * Key#setMeta(String, String)} or {@link Key#removeMeta(String)} failed because the key name is
 * invalid, the meta information is read-only or there where allocation problems in the native
 * library
 */
public class KeyMetaException extends KeyException {
  private static final long serialVersionUID = 1L;
}
