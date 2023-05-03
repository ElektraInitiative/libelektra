package org.libelektra;

import static org.libelektra.ValidationUtil.argNotNull;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.Iterator;
import java.util.Objects;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KeyException;
import org.libelektra.exception.KeyStringValueException;

/**
 * Read only key representing a native Elektra key providing read access to its name and value
 *
 * @apiNote This abstraction is used to represent meta keys being read only by definition and cannot
 *     contain binary data
 */
public class ReadableKey implements Comparable<ReadableKey> {

  protected static final String BOOLEAN_TRUE = "1";
  protected static final String BOOLEAN_FALSE = "0";

  /** Flag for use with {@link Key#copy(Key, int)} and {@link #dup(int)} for copying the key name */
  public static final int KEY_CP_NAME = 1 << 0;

  /**
   * Flag for use with {@link Key#copy(Key, int)} and {@link #dup(int)} for copying the key value,
   * if it is a string
   *
   * @apiNote Do not use together with {@link #KEY_CP_VALUE}
   */
  public static final int KEY_CP_STRING = 1 << 1;

  /**
   * Flag for use with {@link Key#copy(Key, int)} and {@link #dup(int)} for copying the key value
   *
   * @apiNote Do not use together with {@link #KEY_CP_STRING}
   */
  public static final int KEY_CP_VALUE = 1 << 2;

  /**
   * Flag for use with {@link Key#copy(Key, int)} and {@link #dup(int)} for copying the key metadata
   */
  public static final int KEY_CP_META = 1 << 3;

  /**
   * Flag for use with {@link Key#copy(Key, int)} and {@link #dup(int)} for copying the key name,
   * value and metadata
   */
  public static final int KEY_CP_ALL = KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META;

  @Nullable private Pointer pointer;

  @Nullable private Cleaner.Cleanable cleanable;

  /**
   * Constructs a new {@link ReadableKey} instance associated with a JNA pointer
   *
   * @param pointer Optional JNA {@link Pointer} to key
   * @return New {@link ReadableKey} instance if {@code pointer} is non-null, {@link
   *     Optional#empty()} otherwise
   */
  @Nonnull
  protected static Optional<ReadableKey> createReadOnly(@Nullable Pointer pointer) {
    return Optional.ofNullable(pointer).map(ReadableKey::new);
  }

  /**
   * Constructor associating a new {@link ReadableKey} instance with a JNA pointer
   *
   * @param pointer JNA {@link Pointer} to key
   */
  protected ReadableKey(Pointer pointer) {
    this(pointer, false);
  }

  /**
   * Constructor associating a new {@link ReadableKey} instance with a JNA pointer<br>
   * <br>
   * Suppressing clean-up has been introduced for usage of this binding as JNI plug-in and should
   * normally not be used in any other case.
   *
   * @param pointer JNA {@link Pointer} to key
   * @param suppressCleanUp True to suppress native reference clean-up as soon as this {@link Key}
   *     instance becomes phantom reachable, false otherwise
   */
  protected ReadableKey(Pointer pointer, boolean suppressCleanUp) {
    argNotNull(pointer, "Pointer 'pointer'");
    this.pointer = pointer;
    cleanable = (suppressCleanUp ? null : ReferenceCleaner.registerKeyCleanUp(this)); // see #3825
  }

  /**
   * Clean-up method to release key reference by first decrementing its reference counter and then
   * trying to free the native reference<br>
   * <br>
   * {@link ReadableKey keys}, will get cleaned up by garbage collection as soon as they get phantom
   * reachable.
   */
  protected void release() {
    if (cleanable != null) {
      cleanable.clean();
      cleanable = null;
    }
    pointer = null;
  }

  /** @return Key name in string format as returned by {@link #getName()} */
  @Override
  public String toString() {
    return getName();
  }

  /**
   * @return New {@link KeyNameIterator} backed by this {@link ReadableKey}
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public Iterator<String> keyNameIterator() {
    return new KeyNameIterator(this);
  }

  /**
   * @see <a href="https://www.libelektra.org/decisions/boolean">Definition of Bool</a>
   * @return {@link #getString()} interpreted as boolean value
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public boolean getBoolean() {
    return BOOLEAN_TRUE.equals(getString());
  }

  /**
   * @return {@link #getString()} parsed as {@code byte}
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable {@code
   *     byte}
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public byte getByte() {
    return Byte.parseByte(getString());
  }

  /**
   * @return {@link #getString()} parsed as {@code short}
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable {@code
   *     short}
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public short getShort() {
    return Short.parseShort(getString());
  }

  /**
   * @return {@link #getString()} parsed as integer
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable integer
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public int getInt() {
    return Integer.parseInt(getString());
  }

  /**
   * @return {@link #getString()} parsed as {@code long}
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable {@code
   *     long}
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public long getLong() {
    return Long.parseLong(getString());
  }

  /**
   * @return {@link #getString()} parsed as {@code float}
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable {@code
   *     float}
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public float getFloat() {
    return Float.parseFloat(getString());
  }

  /**
   * @return {@link #getString()} parsed as {@code double}
   * @throws NumberFormatException if the {@link #getString()} does not return a parsable {@code
   *     double}
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public double getDouble() {
    return Double.parseDouble(getString());
  }

  /**
   * @return This key's value as string
   * @throws KeyStringValueException if the underlying native key is not of type string
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  @Nonnull
  public String getString() {
    if (!isString()) {
      throw new KeyStringValueException();
    }
    return Elektra.INSTANCE.keyString(getPointer());
  }

  /**
   * Duplicates this {@link ReadableKey} as {@link Key}
   *
   * @return New {@link Key} object containing the same information as this key
   * @throws KeyException if copying failed
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   * @see #dup(int)
   */
  @Nonnull
  public Key dup() {
    return dup(KEY_CP_ALL);
  }

  /**
   * Duplicates this {@link ReadableKey} as {@link Key}
   *
   * @param flags Flags indicating which parts of the key to copy<br>
   *     Example:<br>
   *     {@link #KEY_CP_NAME} | {@link #KEY_CP_VALUE}
   * @return New {@link Key} object containing the same information as this key
   * @throws KeyException if copying failed
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   * @see #dup()
   * @see #KEY_CP_ALL
   * @see #KEY_CP_META
   * @see #KEY_CP_NAME
   * @see #KEY_CP_STRING
   * @see #KEY_CP_VALUE
   */
  @Nonnull
  public Key dup(int flags) {
    Key duped = Key.create();
    Pointer result = Elektra.INSTANCE.keyCopy(duped.getPointer(), getPointer(), flags);
    if (result == null) {
      throw new KeyException();
    }
    return duped;
  }

  /**
   * Compares this key with the {@code other} key by comparing the key name with string comparison
   *
   * @param other Other key to compare this key to
   * @return
   *     <ul>
   *       <li>0 if key name is equal
   *       <li>-1 if this key name has lower alphabetical order than the {@code other} key
   *       <li>1 if this key has higher alphabetical order
   *     </ul>
   *
   * @throws IllegalStateException if this or the {@code other} {@link ReadableKey} has already been
   *     released
   * @throws IllegalArgumentException if {@code other} is {@code null}
   */
  @Override
  public int compareTo(ReadableKey other) {
    argNotNull(other, "Key 'other'");
    return Integer.signum(Elektra.INSTANCE.keyCmp(getPointer(), other.getPointer()));
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(getName());
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    ReadableKey other = (ReadableKey) obj;
    return this.compareTo(other) == 0;
  }

  /**
   * Checks whether this key is sub-key of the {@code other} key
   *
   * @param other Key that is used in check as parent key
   * @return Boolean if this key is (non-direct) sub-key of other-key
   * @throws IllegalStateException if this or the {@code other} {@link ReadableKey} has already been
   *     released
   * @throws IllegalArgumentException if {@code other} is {@code null}
   */
  public boolean isBelow(ReadableKey other) {
    argNotNull(other, "Key 'other'");
    return Elektra.INSTANCE.keyIsBelow(other.getPointer(), getPointer()) == 1;
  }

  /**
   * Checks whether this key is the same as the {@code other} key or a sub-key of the {@code other}
   * key
   *
   * @param other Key that is used in check as parent key
   * @return Boolean if this key is other key or (non-direct) sub-key of other-key
   * @throws IllegalStateException if this or the {@code other} {@link ReadableKey} has already been
   *     released
   * @throws IllegalArgumentException if {@code other} is {@code null}
   */
  public boolean isBelowOrSame(ReadableKey other) {
    argNotNull(other, "Key 'other'");
    return Elektra.INSTANCE.keyIsBelowOrSame(other.getPointer(), getPointer()) == 1;
  }

  /**
   * Checks whether this key is direct sub-key of the {@code other} key
   *
   * @param other Key that is used in check as parent key
   * @return Boolean if this key is direct sub-key of other key ("child")
   * @throws IllegalStateException if this or the {@code other} {@link ReadableKey} has already been
   *     released
   * @throws IllegalArgumentException if {@code other} is {@code null}
   */
  public boolean isDirectlyBelow(ReadableKey other) {
    argNotNull(other, "Key 'other'");
    return Elektra.INSTANCE.keyIsDirectlyBelow(other.getPointer(), getPointer()) == 1;
  }

  /**
   * @return True if the underlying native key's value is of type binary, false otherwise
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public boolean isBinary() {
    return Elektra.INSTANCE.keyIsBinary(getPointer()) == 1;
  }

  /**
   * @return True if the underlying native key's value is a valid string, false otherwise
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public boolean isString() {
    return Elektra.INSTANCE.keyIsString(getPointer()) == 1;
  }

  /**
   * @return Key name (key part of "key-value" pair)
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  @Nonnull
  public String getName() {
    return Elektra.INSTANCE.keyName(getPointer());
  }

  /**
   * @return Length of key name
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public int getNameSize() {
    return Elektra.INSTANCE.keyGetNameSize(getPointer());
  }

  /**
   * @return Key's base name as String
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  @Nonnull
  public String getBaseName() {
    return Elektra.INSTANCE.keyBaseName(getPointer());
  }

  /**
   * @return Length of key's base name
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public int getBaseNameSize() {
    return Elektra.INSTANCE.keyGetBaseNameSize(getPointer());
  }

  /**
   * @return Length / size of key value in bytes
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public int getValueSize() {
    return Elektra.INSTANCE.keyGetValueSize(getPointer());
  }

  /**
   * @return True, if the key has no value, false otherwise
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  public boolean isNull() {
    // An empty string has size 1, because the zero-terminator is included in the size
    return getValueSize() == 0;
  }

  /**
   * @return JNA pointer to the native pointer for this key
   * @throws IllegalStateException if this {@link ReadableKey} has already been released
   */
  @Nonnull
  protected Pointer getPointer() {
    if (pointer == null) {
      throw new IllegalStateException("Native resource pointer is null.");
    }
    return pointer;
  }
}
