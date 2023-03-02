package org.libelektra;

import com.sun.jna.Pointer;
import java.util.Iterator;
import java.util.NoSuchElementException;

/** An {@link Iterator} for a {@link ReadableKey}'s name parts (separated by /) */
class KeyNameIterator implements Iterator<String> {

  private final Pointer keyPointer;
  private int position = 0;
  private int size = 0;

  /**
   * @param key Key which name is used in iterator
   * @throws IllegalStateException if {@code key} has already been released
   */
  KeyNameIterator(ReadableKey key) {
    keyPointer = Elektra.INSTANCE.keyUnescapedName(key.getPointer());
    size = Elektra.INSTANCE.keyGetUnescapedNameSize(key.getPointer());
  }

  /** @return True, if another value is available, false otherwise */
  @Override
  public boolean hasNext() {
    return position < size;
  }

  /**
   * Gets the next value of iteration
   *
   * @return Next key name part in iteration
   */
  @Override
  public String next() {
    if (position == size) {
      throw new NoSuchElementException("End of key names reached");
    }

    final String ret = keyPointer.getString(position);
    position += ret.length() + 1;
    return ret;
  }

  /**
   * @throws UnsupportedOperationException because removal of key name parts is not supported by
   *     this iterator
   */
  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }
}
