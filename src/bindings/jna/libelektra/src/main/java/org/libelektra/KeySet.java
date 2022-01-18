package org.libelektra;

import static java.util.Objects.requireNonNull;
import static org.libelektra.Elektra.KDB_O_NONE;
import static org.libelektra.Elektra.KS_END;
import static org.libelektra.ValidationUtil.argNotNull;
import static org.libelektra.ValidationUtil.argNotNullOrBlank;
import static org.libelektra.ValidationUtil.checkPointer;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.SortedSet;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KeySetException;

/** Java representation of a native Elektra key set, a container for keys */
public class KeySet extends AbstractSet<Key> implements SortedSet<Key> {

  @Nullable private Pointer pointer;

  @Nullable private Cleaner.Cleanable cleanable;

  /**
   * Constructs a new {@link KeySet} containing the specified {@link Key keys}<br>
   * <br>
   * Example: KeySet keySet = KeySet.create(Key.create("A"), Key.create("B"));
   *
   * @param keys List of initial keys for the key set
   * @return New key set containing the specified initial keys
   * @throws KeySetException on allocation problems
   * @see #release()
   */
  @Nonnull
  public static KeySet create(Key... keys) {
    return create(keys.length, keys);
  }

  /**
   * Constructs a new {@link KeySet} containing the specified {@link Key keys}<br>
   * <br>
   * Example: KeySet keySet = KeySet.create(10, Key.create("A"), Key.create("B"));
   *
   * @param allocationHint Hint indicating the expected size of the key set
   * @param keys List of initial keys for the key set
   * @return New key set containing the specified initial keys
   * @throws KeySetException on allocation problems
   * @see #release()
   */
  @Nonnull
  public static KeySet create(int allocationHint, Key... keys) {
    Object[] args =
        Stream.concat(Arrays.stream(keys).map(Key::getPointer), Stream.of(KS_END)).toArray();
    return checkPointer(
        Elektra.INSTANCE.ksNew(allocationHint >= args.length ? allocationHint : args.length, args),
        KeySet::new,
        KeySetException::new);
  }

  /**
   * Constructs an empty {@link KeySet} with a default allocation hint of 16
   *
   * @return Newly allocated key set
   * @throws KeySetException on allocation problems
   * @see #release()
   */
  @Nonnull
  public static KeySet create() {
    return create(16);
  }

  /**
   * Constructor associating a new {@link KeySet} instance with a native pointer in long format
   *
   * @param nativePointer Native pointer to key set in long format
   */
  protected KeySet(long nativePointer) {
    this(nativePointer, false);
  }

  /**
   * Constructor associating a new {@link KeySet} instance with a native pointer in long format<br>
   * <br>
   * Suppressing clean-up has been introduced for usage of this binding as JNI plug-in and should
   * normally not be used in any other case.
   *
   * @param nativePointer Native pointer to key set in long format
   * @param suppressCleanUp True to suppress native reference clean-up as soon as this {@link
   *     KeySet} instance becomes phantom reachable, false otherwise
   * @see #release()
   */
  protected KeySet(long nativePointer, boolean suppressCleanUp) {
    pointer = new Pointer(nativePointer);
    cleanable =
        (suppressCleanUp ? null : ReferenceCleaner.registerKeySetCleanUp(this)); // see #3825
  }

  /**
   * Constructor associating a new {@link KeySet} instance with a JNA pointer
   *
   * @param pointer JNA {@link Pointer} to key set
   * @throws IllegalArgumentException if {@code pointer} is {@code null}
   * @see #release()
   */
  protected KeySet(Pointer pointer) {
    this.pointer = argNotNull(pointer, "Pointer 'pointer'");
    cleanable = ReferenceCleaner.registerKeySetCleanUp(this);
  }

  /**
   * Clean-up method to release key set reference by trying to free the native reference<br>
   * <br>
   * Call this method if you have obtained a {@link KeySet} via any of its public methods or {@link
   * KDB#get(Key)} and you do not longer need it. If you do not manually release such {@link KeySet
   * key sets}, they will get cleaned up by garbage collection as soon as they get phantom
   * reachable. Therefore its encouraged to release {@link KeySet key set instances} as soon as you
   * do not use them anymore.
   */
  public void release() {
    if (cleanable != null) {
      cleanable.clean();
      cleanable = null;
    }
    pointer = null;
  }

  /**
   * Duplicates the key set
   *
   * @return New {@link KeySet} containing the same key references as this {@link KeySet} does
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @see #release()
   */
  @Nonnull
  public KeySet dup() {
    return new KeySet(Elektra.INSTANCE.ksDup(getPointer()));
  }

  /**
   * Copies key references from {@code source} to <b>this</b> {@link KeySet}
   *
   * @param source Key set that is used as source
   * @return This {@link KeySet}, enabling a fluent interface
   * @throws IllegalStateException if this {@link KeySet} or the specified {@code source} has
   *     already been released
   * @throws IllegalArgumentException if {@code source} is {@code null}
   */
  public KeySet copy(KeySet source) {
    argNotNull(source, "KeySet 'source'");
    Elektra.INSTANCE.ksCopy(getPointer(), source.getPointer());
    return this;
  }

  /**
   * Append key to key set
   *
   * @param key {@link Key} to append
   * @return This {@link KeySet}, enabling a fluent interface
   * @throws IllegalStateException if this {@link KeySet} or the specified {@code key} has already
   *     been released
   * @throws IllegalArgumentException if {@code key} is {@code null}
   * @throws KeySetException if appending the {@code key} failed because of allocation problems
   * @see #add(Key)
   */
  @Nonnull
  public KeySet append(Key key) {
    argNotNull(key, "Key 'key'");
    if (Elektra.INSTANCE.ksAppendKey(getPointer(), key.getPointer()) <= 0) {
      throw new KeySetException();
    }
    return this;
  }

  /**
   * Appends keys from key set
   *
   * @param source Source {@link KeySet} to append all of its {@link Key keys}
   * @return This {@link KeySet}, enabling a fluent interface
   * @throws IllegalStateException if this {@link KeySet} or the specified {@code source} has
   *     already been released
   * @throws IllegalArgumentException if {@code source} is {@code null}
   * @throws KeySetException if appending the {@code source} failed because of allocation problems
   */
  @Nonnull
  public KeySet append(KeySet source) {
    argNotNull(source, "KeySet 'keySet'");
    if (Elektra.INSTANCE.ksAppend(getPointer(), source.getPointer()) < 0) {
      throw new KeySetException();
    }
    return this;
  }

  /**
   * Creates new key set with help of a cut point
   *
   * @param cutpoint Key that is used as cutting point
   * @return New {@link KeySet} containing all keys until the cutting point
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IllegalArgumentException if {@code cutpoint} is {@code null}
   * @throws KeySetException if {@code cutpoint} is missing a key name or on allocation problems
   * @see #release()
   */
  @Nonnull
  public KeySet cut(Key cutpoint) {
    argNotNull(cutpoint, "Key 'cutpoint'");
    return checkPointer(
        Elektra.INSTANCE.ksCut(getPointer(), cutpoint.getPointer()),
        KeySet::new,
        KeySetException::new);
  }

  /**
   * Removes the specified key from key set
   *
   * @param key Key to remove
   * @return Removed {@link Key} from the key set, matching the specified {@code key}'s name. May or
   *     may not reference the same native key resource. {@link Optional#empty()} if the specified
   *     {@code key} was not found.
   * @throws IllegalStateException if {@link KeySet} or {@code key} has already been released
   * @throws IllegalArgumentException if {@code key} is {@code null}
   * @see #remove(Object)
   */
  @Nonnull
  public Optional<Key> remove(ReadableKey key) {
    argNotNull(key, "Key 'key'");
    return Key.create(Elektra.INSTANCE.ksLookup(getPointer(), key.getPointer(), Elektra.KDB_O_POP));
  }

  /**
   * Removes the key with the specified name from key set
   *
   * @param find Name of the key to remove
   * @return Removed {@link Key} from the key set, matching the specified {@code key}'s name. {@link
   *     Optional#empty()} if the no key matching the specified name was not found.
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IllegalArgumentException if {@code find} is {@link String#isBlank() blank}
   */
  @Nonnull
  public Optional<Key> remove(String find) {
    argNotNullOrBlank(find, "String 'find'");
    return Key.create(Elektra.INSTANCE.ksLookupByName(getPointer(), find, Elektra.KDB_O_POP));
  }

  /**
   * Returns key from key set and also removes it from the set
   *
   * @param cursor Cursor position of the key to remove; starting from 0
   * @return Key found at given cursor position
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IndexOutOfBoundsException if position is out of bounds
   * @see Key#release()
   */
  @Nonnull
  public Key remove(int cursor) {
    return checkPointer(
        Elektra.INSTANCE.elektraKsPopAtCursor(getPointer(), cursor),
        Key::new,
        IndexOutOfBoundsException::new);
  }

  /**
   * Gets the key at the given cursor position
   *
   * @param cursor Cursor position used to fetch key; starting from 0
   * @return Key found at specified cursor position
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IndexOutOfBoundsException if position is out of bounds
   * @see Key#release()
   */
  @Nonnull
  public Key at(int cursor) {
    return checkPointer(
        Elektra.INSTANCE.ksAtCursor(getPointer(), cursor),
        Key::new,
        IndexOutOfBoundsException::new);
  }

  /**
   * @param key Key to look for
   * @return Index of the {@code key} in this {@link KeySet}
   * @throws IllegalStateException if {@link KeySet} or {@code key} has already been released
   * @throws IllegalArgumentException if {@code key} was not found in this {@link KeySet}
   */
  public int indexOf(Key key) {
    int index = searchForIndexOf(key);
    if (index < 0) {
      throw new IllegalArgumentException();
    }
    return index;
  }

  /**
   * Binary search in the native key set resulting in the index of the key, if it is already
   * contained in the key set or the index where the {@code key} would be inserted.
   *
   * @param key Key to look for
   * @return Index of the {@code key} in this {@link KeySet} (&gt;=0) or if {@code key} was not
   *     found in this {@link KeySet}, the index of the position where the {@code key} would be
   *     inserted (&lt;0). To get the correct insertion index calculate {@code result * -1 - 1}
   * @throws IllegalStateException if {@link KeySet} or {@code key} has already been released
   */
  private int searchForIndexOf(ReadableKey key) {
    return Elektra.INSTANCE.ksSearch(this.getPointer(), key.getPointer());
  }

  /**
   * Search for a key in the key set
   *
   * @param find Key used in search
   * @return Key if search successful, {@link Optional#empty()} otherwise
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IllegalArgumentException if {@code key} is {@code null}
   * @see Key#release()
   * @see #contains(Object)
   */
  @Nonnull
  public Optional<Key> lookup(Key find) {
    argNotNull(find, "Key 'find'");
    return Key.create(Elektra.INSTANCE.ksLookup(getPointer(), find.getPointer(), KDB_O_NONE));
  }

  /**
   * Search for a key in the key set
   *
   * @param find Key name used in search
   * @return Key if search successful, {@link Optional#empty()} otherwise
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws IllegalArgumentException if {@code find} is {@link String#isBlank() blank}
   * @see Key#release()
   */
  @Nonnull
  public Optional<Key> lookup(String find) {
    argNotNullOrBlank(find, "String 'find'");
    return Key.create(Elektra.INSTANCE.ksLookupByName(getPointer(), find, KDB_O_NONE));
  }

  /**
   * @return JNA pointer to the native pointer for this key set
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Nonnull
  protected Pointer getPointer() {
    if (pointer == null) {
      throw new IllegalStateException();
    }
    return pointer;
  }

  /**
   * Iterates though all keys in this key set and appends their representation to the output. Uses
   * the toString() function of the Key objects.
   *
   * @return Represents this {@link KeySet} as string
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    String sep = "";
    for (Key k : this) {
      sb.append(sep);
      sb.append(k);
      sep = "\n";
    }
    return sb.toString();
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public int size() {
    return Elektra.INSTANCE.ksGetSize(getPointer());
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public boolean isEmpty() {
    return super.isEmpty();
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released or the passed
   *     {@link Object o} is a {@link Key} that has already been released
   * @throws NullPointerException if the specified element {@code o} is {@code null}
   * @see #lookup(Key)
   */
  @Override
  public boolean contains(@Nullable Object o) {
    requireNonNull(o);
    if (o instanceof ReadableKey) {
      // no need to release found key, because it points to the key contained in the
      // key set
      return searchForIndexOf((ReadableKey) o) >= 0;
    }
    return false;
  }

  /**
   * {@inheritDoc}
   *
   * @return New {@link KeySetIterator} backed by this {@link KeySet}
   */
  @Override
  public Iterator<Key> iterator() {
    return new KeySetIterator<>(this, Key::new);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public Object[] toArray() {
    return super.toArray();
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public <T> T[] toArray(T[] a) {
    return super.toArray(a);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} or the specified {@code key} has already
   *     been released
   * @throws NullPointerException if {@code key} is {@code null}
   * @throws KeySetException if inserting the {@code key} failed because of allocation problems
   * @see #append(Key)
   */
  @Override
  public boolean add(Key e) {
    if (contains(e)) {
      return false;
    }
    append(e);
    return true;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @see #remove(ReadableKey)
   */
  @Override
  public boolean remove(Object o) {
    requireNonNull(o);
    if (o instanceof ReadableKey) {
      return Elektra.INSTANCE.ksLookup(
              getPointer(), ((ReadableKey) o).getPointer(), Elektra.KDB_O_POP)
          != null;
    }
    return false;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if the specified collection is {@code null} or contains {@code
   *     null} elements
   */
  @Override
  public boolean containsAll(Collection<?> c) {
    return super.containsAll(c);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} or any {@link Key} contained in the
   *     specified collection {@code c} has already been released
   * @throws NullPointerException if the specified collection is {@code null} or contains {@code
   *     null} elements
   * @throws KeySetException if inserting the {@code key} failed because of allocation problems
   */
  @Override
  public boolean addAll(Collection<? extends Key> c) {
    return super.addAll(c);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if the specified collection is {@code null} or contains {@code
   *     null} elements
   */
  @Override
  public boolean retainAll(Collection<?> c) {
    return super.retainAll(c);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if the specified collection is {@code null} or contains {@code
   *     null} elements
   */
  @Override
  public boolean removeAll(Collection<?> c) {
    return super.removeAll(c);
  }

  /**
   * Removes all elements form this {@link KeySet}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public void clear() {
    Elektra.INSTANCE.ksClear(getPointer());
  }

  /**
   * {@inheritDoc}
   *
   * @implSpec Returns {@code null} because natural ordering of keys is used ({@link ReadableKey}
   *     implements {@link Comparable})
   * @throws IllegalStateException if this {@link KeySet} has already been released
   */
  @Override
  public Comparator<? super Key> comparator() {
    return null;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if {@code fromElement} or {@code toElement} is {@code null}
   * @throws IllegalArgumentException {@inheritDoc}
   */
  @Override
  public SortedSet<Key> subSet(Key fromElement, Key toElement) {
    requireNonNull(fromElement);
    requireNonNull(toElement);

    var fromElementIndex = indexOf(fromElement);
    var toElementIndex = indexOf(toElement);

    if (fromElementIndex == toElementIndex) {
      return Collections.emptySortedSet();
    }

    if (fromElementIndex > toElementIndex) {
      throw new IllegalArgumentException();
    }

    return new KeySetView(this, fromElement, toElement);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if {@code toElement} is {@code null}
   * @throws IllegalArgumentException {@inheritDoc}
   */
  @Override
  public SortedSet<Key> headSet(Key toElement) {
    if (!contains(toElement)) {
      throw new IllegalArgumentException();
    }

    return new KeySetView(this, null, toElement);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NullPointerException if {@code fromElement} is {@code null}
   * @throws IllegalArgumentException {@inheritDoc}
   */
  @Override
  public SortedSet<Key> tailSet(Key fromElement) {
    if (!contains(fromElement)) {
      throw new IllegalArgumentException();
    }

    return new KeySetView(this, fromElement, null);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NoSuchElementException {@inheritDoc}
   * @see Key#release()
   */
  @Nonnull
  public Key first() {
    return checkPointer(
        Elektra.INSTANCE.ksHead(getPointer()), Key::new, NoSuchElementException::new);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalStateException if this {@link KeySet} has already been released
   * @throws NoSuchElementException {@inheritDoc}
   * @see Key#release()
   */
  @Nonnull
  public Key last() {
    return checkPointer(
        Elektra.INSTANCE.ksTail(getPointer()), Key::new, NoSuchElementException::new);
  }

  /**
   * View backed by a {@link KeySet}. Changes in the backing {@link KeySet} are reflected in this
   * view and vice versa. The range of this view is defined by an inclusive start element and an
   * inclusive end element.
   */
  private static class KeySetView extends AbstractSet<Key> implements SortedSet<Key> {

    private final KeySet keySet;

    /**
     * Inclusive start element for this view on the backing {@link KeySet keySet}. {@link
     * Optional#empty()} if this view does not have a lower bound.
     */
    private final Optional<Key> oFromElement;

    /**
     * Exclusive end element for this view on the backing {@link KeySet keySet}. {@link
     * Optional#empty()} if this view does not have an upper bound.
     */
    private final Optional<Key> oToElement;

    private KeySetView(KeySet keySet, @Nullable Key fromElement, @Nullable Key toElement) {
      this.keySet = keySet;
      this.oFromElement = Optional.ofNullable(fromElement);
      this.oToElement = Optional.ofNullable(toElement);
    }

    /**
     * @return Lower (inclusive) index bound for this view
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or the
     *     {@link Key} signifying this view's lower bound has already been released
     */
    private int getLowerBound() {
      return oFromElement.map(keySet::searchForIndexOf).map(i -> i < 0 ? i * -1 - 1 : i).orElse(0);
    }

    /** @return True, if this view has a lower bound, false otherwise */
    private boolean hasLowerBound() {
      return oFromElement.isPresent();
    }

    /**
     * @return Upper (exclusive) index bound for this view
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or the
     *     {@link Key} signifying this view's upper bound has already been released
     */
    private int getUpperBound() {
      return oToElement
          .map(keySet::searchForIndexOf)
          .map(i -> i < 0 ? i * -1 : i)
          .orElseGet(keySet::size);
    }

    /** @return True, if this view has an upper bound, false otherwise */
    private boolean hasUpperBound() {
      return oToElement.isPresent();
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released
     */
    @Override
    public int size() {
      return getUpperBound() - getLowerBound();
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released
     */
    @Override
    public boolean isEmpty() {
      return super.isEmpty();
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released or the
     *     passed {@link Object o} is a {@link Key} that has already been released
     * @throws NullPointerException if the specified element {@code o} is {@code null}
     */
    @Override
    public boolean contains(@Nullable Object o) {
      requireNonNull(o);
      if (o instanceof ReadableKey) {
        int foundIndex = keySet.searchForIndexOf((ReadableKey) o);
        if (foundIndex >= 0 && foundIndex >= getLowerBound() && foundIndex < getUpperBound()) {
          return true;
        }
      }
      return false;
    }

    /**
     * {@inheritDoc}
     *
     * @return New {@link KeySetIterator} backed by the {@link KeySet} backing this {@link
     *     KeySetView}
     * @throws IllegalStateException if any {@link Key} signifying this view's lower or upper bound
     *     has already been released
     */
    @Override
    public Iterator<Key> iterator() {
      return new KeySetIterator<>(keySet, Key::new, getLowerBound(), getUpperBound());
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released
     */
    @Override
    public Object[] toArray() {
      return super.toArray();
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released
     */
    @Override
    public <T> T[] toArray(T[] a) {
      return super.toArray(a);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView}, the
     *     specified {@code key} or any {@link Key} signifying this view's lower or upper bound has
     *     already been released
     * @throws NullPointerException if {@code key} is {@code null}
     * @throws IllegalArgumentException if {@code key} is violating the lower or upper bound of this
     *     {@link KeySetView}
     * @throws KeySetException if inserting the {@code key} failed because of allocation problems
     */
    @Override
    public boolean add(Key e) {
      requireNonNull(e);
      int insertPosition = keySet.searchForIndexOf(e);
      // check whether key to add is already contained in backing key set and within
      // range of this view
      if (insertPosition >= 0
          && (!hasLowerBound() || insertPosition >= getLowerBound())
          && (!hasUpperBound() || insertPosition < getUpperBound())) {
        return false;
      }

      // check whether key to add is within range of this view
      if (insertPosition < 0) {
        insertPosition = insertPosition * -1 - 1;
      }
      if ((hasLowerBound() && insertPosition < getLowerBound())
          || (hasUpperBound()
              && (e.equals(oToElement.get()) || insertPosition > getUpperBound()))) {
        throw new IllegalArgumentException();
      }

      keySet.append(e);
      return true;
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} has
     *     already been released
     */
    @Override
    public boolean remove(Object o) {
      requireNonNull(o);
      if (o instanceof ReadableKey) {
        ReadableKey key = (ReadableKey) o;
        int keyIndex = keySet.searchForIndexOf(key);
        if (keyIndex >= 0
            && (!hasLowerBound() || keyIndex >= getLowerBound())
            && (!hasUpperBound() || keyIndex < getUpperBound())) {
          return Elektra.INSTANCE.ksLookup(
                  keySet.getPointer(), ((ReadableKey) o).getPointer(), Elektra.KDB_O_POP)
              != null;
        }
      }
      return false;
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released or the
     *     passed {@link Object o} is a {@link Key} that has already been released
     * @throws NullPointerException if the specified collection is {@code null} or contains {@code
     *     null} elements
     */
    @Override
    public boolean containsAll(Collection<?> c) {
      return super.containsAll(c);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released
     * @throws IllegalArgumentException if {@code key} is violating the lower or upper bound of this
     *     {@link KeySetView}
     * @throws NullPointerException if the specified collection is {@code null} or contains {@code
     *     null} elements
     * @throws KeySetException if inserting the {@code key} failed because of allocation problems
     */
    @Override
    public boolean addAll(Collection<? extends Key> c) {
      return super.addAll(c);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released or the
     *     passed {@link Object o} is a {@link Key} that has already been released
     * @throws NullPointerException if the specified collection is {@code null} or contains {@code
     *     null} elements
     */
    @Override
    public boolean retainAll(Collection<?> c) {
      return super.retainAll(c);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if the {@link KeySet} backing this {@link KeySetView} or any
     *     {@link Key} signifying this view's lower or upper bound has already been released or the
     *     passed {@link Object o} is a {@link Key} that has already been released
     * @throws NullPointerException if the specified collection is {@code null} or contains {@code
     *     null} elements
     */
    @Override
    public boolean removeAll(Collection<?> c) {
      return super.removeAll(c);
    }

    /**
     * Removes all elements form this {@link KeySet}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     */
    @Override
    public void clear() {
      super.clear();
    }

    /**
     * {@inheritDoc}
     *
     * @implSpec Returns {@code null} because natural ordering of keys is used ({@link ReadableKey}
     *     implements {@link Comparable})
     * @throws IllegalStateException if this {@link KeySet} has already been released
     */
    @Override
    public Comparator<? super Key> comparator() {
      return null;
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     * @throws NullPointerException if {@code fromElement} or {@code toElement} is {@code null}
     * @throws IllegalArgumentException {@inheritDoc}
     */
    @Override
    public SortedSet<Key> subSet(Key fromElement, Key toElement) {
      requireNonNull(fromElement);
      requireNonNull(toElement);

      var fromElementIndex = keySet.indexOf(fromElement);
      var toElementIndex = keySet.indexOf(toElement);

      if (fromElementIndex == toElementIndex) {
        return Collections.emptySortedSet();
      }

      if (fromElementIndex > toElementIndex
          || (hasLowerBound() && fromElementIndex <= getLowerBound())
          || (hasUpperBound() && toElementIndex > getUpperBound())) {
        throw new IllegalArgumentException();
      }

      return new KeySetView(this.keySet, fromElement, toElement);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     * @throws NullPointerException if {@code toElement} is {@code null}
     * @throws IllegalArgumentException {@inheritDoc}
     */
    @Override
    public SortedSet<Key> headSet(Key toElement) {
      var toElementIndex = keySet.indexOf(toElement);
      if (toElementIndex < 0 || (hasUpperBound() && toElementIndex > getUpperBound())) {
        throw new IllegalArgumentException();
      }

      return new KeySetView(this.keySet, oFromElement.orElse(null), toElement);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     * @throws NullPointerException if {@code fromElement} is {@code null}
     * @throws IllegalArgumentException {@inheritDoc}
     */
    @Override
    public SortedSet<Key> tailSet(Key fromElement) {
      var fromElementIndex = keySet.indexOf(fromElement);
      if (fromElementIndex < 0 || (hasLowerBound() && fromElementIndex < getLowerBound())) {
        throw new IllegalArgumentException();
      }

      return new KeySetView(this.keySet, fromElement, oToElement.orElse(null));
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     * @throws NoSuchElementException {@inheritDoc}
     * @see Key#release()
     */
    @Override
    public Key first() {
      return checkPointer(
          Elektra.INSTANCE.ksAtCursor(keySet.getPointer(), getLowerBound()),
          Key::new,
          NoSuchElementException::new);
    }

    /**
     * {@inheritDoc}
     *
     * @throws IllegalStateException if this {@link KeySet} has already been released
     * @throws NoSuchElementException {@inheritDoc}
     * @see Key#release()
     */
    @Override
    public Key last() {
      return checkPointer(
          Elektra.INSTANCE.ksAtCursor(keySet.getPointer(), getUpperBound() - 1),
          Key::new,
          NoSuchElementException::new);
    }
  }
}
