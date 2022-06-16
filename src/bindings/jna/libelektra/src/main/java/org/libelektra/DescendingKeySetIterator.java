package org.libelektra;

import static org.libelektra.ValidationUtil.checkPointer;

import com.sun.jna.Pointer;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Function;
import javax.annotation.Nullable;

/** An descending {@link Iterator} for a {@link KeySet} returning {@link Key}s */
class DescendingKeySetIterator<T extends ReadableKey> implements Iterator<T> {

  private final KeySet keySet;
  private int end;
  private int position;
  private Function<Pointer, T> factory;

  @Nullable private T current = null;

  /**
   * @param keySet {@link KeySet} backing this iterator
   * @param factory Factory for creating elements of type {@code T} from {@link Pointer}
   */
  DescendingKeySetIterator(KeySet keySet, Function<Pointer, T> factory) {
    this(keySet, factory, 0, keySet.size());
  }

  /**
   * @param keySet {@link KeySet} backing this iterator
   * @param factory Factory for creating elements of type {@code T} from {@link Pointer}
   * @param lowerBound Lower index bound for {@code keySet}, where the iteration ends (inclusive)
   * @param upperBound Upper index bound for {@code keySet}, where the iteration starts (exclusive)
   */
  DescendingKeySetIterator(
      KeySet keySet, Function<Pointer, T> factory, int lowerBound, int upperBound) {
    this.keySet = keySet;
    this.factory = factory;
    end = lowerBound;
    position = upperBound - 1;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this backing {@link KeySet} has already been released
   */
  @Override
  public boolean hasNext() {
    return position >= end;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this backing {@link KeySet} has already been released
   * @throws NoSuchElementException {@inheritDoc}
   */
  @Override
  public T next() {
    current =
        checkPointer(
            Elektra.INSTANCE.ksAtCursor(keySet.getPointer(), position),
            factory,
            NoSuchElementException::new);
    position--;
    return current;
  }

  /**
   * Removes the {@code Key} element from the {@link KeySet} backing this iterator.
   *
   * @throws IllegalStateException {@inheritDoc} or if this backing {@link KeySet} or the {@code
   *     Key} last returned by {@link #next()} has already been released
   */
  @Override
  public void remove() {
    if (current == null) {
      throw new IllegalStateException();
    }
    keySet.remove(current);
    current = null;
    position++;
  }
}
