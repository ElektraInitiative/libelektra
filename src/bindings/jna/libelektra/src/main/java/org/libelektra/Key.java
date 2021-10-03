package org.libelektra;

import static org.libelektra.Key.CreateArgumentTag.KEY_END;
import static org.libelektra.Key.CreateArgumentTag.KEY_META;
import static org.libelektra.Key.CreateArgumentTag.KEY_VALUE;
import static org.libelektra.ValidationUtil.argNotNull;
import static org.libelektra.ValidationUtil.argNotNullOrBlank;
import static org.libelektra.ValidationUtil.checkPointer;

import com.sun.jna.Pointer;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KeyBinaryValueException;
import org.libelektra.exception.KeyException;
import org.libelektra.exception.KeyMetaException;
import org.libelektra.exception.KeyNameException;
import org.libelektra.exception.PluginMisbehaviorException;

/** Key represents a native Elektra key providing access to its name, value and meta information */
public final class Key extends ReadableKey implements Iterable<ReadableKey> {

  private static final String WARNINGS = "warnings";

  /** Argument tags for use with {@link #create(String, Object...)} */
  public enum CreateArgumentTag {

    /** Used as a parameter terminator */
    KEY_END(0),

    /** Flag for the key name */
    KEY_NAME(1),

    /** Flag for the key data */
    KEY_VALUE(1 << 1),

    /** Flag for the key comment */
    KEY_COMMENT(1 << 3),

    /** Flag if the key is binary */
    KEY_BINARY(1 << 4),

    /** Flag for maximum size to limit value */
    KEY_SIZE(1 << 11),

    /** Flag for metadata */
    KEY_META(1 << 15);

    public final Integer value;

    private CreateArgumentTag(int value) {
      this.value = Integer.valueOf(value);
    }
  }

  /**
   * Constructs a new {@link Key} instance associated with a JNA pointer
   *
   * @param pointer Optional JNA {@link Pointer} to key
   * @return New {@link Key} instance if {@code pointer} is non-null, {@link Optional#empty()}
   *     otherwise
   * @see #release()
   */
  @Nonnull
  protected static Optional<Key> create(@Nullable Pointer pointer) {
    return Optional.ofNullable(pointer).map(Key::new);
  }

  /**
   * Constructs a temporary nameless {@link Key} which cannot be saved to the key data base but used
   * for transferring warnings and error information.
   *
   * @return New nameless key
   * @throws KeyException on allocation problems
   * @see #release()
   */
  @Nonnull
  public static Key create() {
    return checkPointer(
        Elektra.INSTANCE.keyNew(Elektra.CASCADING_ROOT_KEY_NAME), Key::new, KeyException::new);
  }

  /**
   * Constructs a new {@link Key} with the specified content and arguments<br>
   *
   * @param name Key name; first part of key-value pair
   * @param args Arguments used for key value<br>
   *     Example:<br>
   *     {@link CreateArgumentTag#KEY_VALUE}, "custom key value", {@link CreateArgumentTag#KEY_END}
   * @return New key
   * @throws KeyException if the key name is invalid or there have been allocation problems
   * @see CreateArgumentTag
   * @see #release()
   */
  @Nonnull
  protected static Key create(String name, Object... args) {
    return checkPointer(
        Elektra.INSTANCE.keyNew(
            name,
            Arrays.stream(args)
                .map(o -> (o instanceof CreateArgumentTag) ? ((CreateArgumentTag) o).value : o)
                .toArray()),
        Key::new,
        KeyException::new);
  }

  /**
   * Constructs a new {@link Key} with the specified content and arguments<br>
   *
   * @param name Name of the key (first part of key-value pair)
   * @param value Optional Value of key. will be determine from the object by calling {@link
   *     Object#toString()}. To set a binary value, please see {@link #setBinary(byte[])}.
   * @param meta Metadata that should be added to this key, null keys will be filtered away
   * @return New key
   * @throws KeyException if the key name is invalid or there have been allocation problems
   * @see #release()
   */
  @Nonnull
  public static Key create(String name, @Nullable Object value, Key... meta) {
    int size = 0;
    for (Key m : meta) {
      if (m != null) {
        size++;
      }
    }
    // 3 -> KEY_VALUE, value, KEY_END, 4 -> one more for KEY_META
    size += size > 0 ? 4 : 3;
    Object[] args = new Object[size];
    int cur = 0;
    args[cur++] = KEY_VALUE;
    args[cur++] = value != null ? value.toString() : null;
    if (size > 3) {
      args[cur++] = KEY_META;
      for (Key m : meta) {
        args[cur++] = m;
      }
    }
    args[cur] = KEY_END;
    return create(name, args);
  }

  /**
   * Basic constructor of key class
   *
   * @param name Key name; first part of key-value pair
   * @param meta Metadata that should be added to this key. Will filter null values.
   * @return New key object
   * @throws KeyException if the key name is invalid or there have been allocation problems
   * @see #release()
   */
  @Nonnull
  public static Key create(String name, Key... meta) {
    return create(name, null, meta);
  }

  /**
   * Constructor associating a new {@link Key} instance with a native pointer in long format<br>
   * <br>
   * Suppressing clean-up has been introduced for usage of this binding as JNI plug-in and should
   * normally not be used in any other case.
   *
   * @param nativePointer Native pointer to key in long format
   * @param suppressCleanUp True to suppress native reference clean-up as soon as this {@link Key}
   *     instance becomes phantom reachable, false otherwise
   * @see #release()
   * @implNote Increased the native key's reference counter, even if {@code suppressCleanUp} is
   *     {@code true}
   */
  protected Key(long nativePointer, boolean suppressCleanUp) {
    super(new Pointer(nativePointer), suppressCleanUp);
  }

  /**
   * Constructor associating a new {@link Key} instance with a JNA pointer
   *
   * @param pointer JNA {@link Pointer} to key
   * @see #release()
   */
  protected Key(Pointer pointer) {
    super(pointer);
  }

  /**
   * Clean-up method to release key reference by first decrementing its reference counter and then
   * trying to free the native reference<br>
   * <br>
   * Call this method if you do not longer need a {@link Key} and obtained it via any of its public
   * methods or the public methods of {@link KeySet}. If you do not manually release such {@link Key
   * keys}, they will get cleaned up by garbage collection as soon as they get phantom reachable.
   * Therefore its encouraged to release {@link Key key instances} as soon as you do not use them
   * anymore.
   */
  @Override
  public void release() {
    super.release();
  }

  /**
   * @return This key's value as string
   * @throws KeyBinaryValueException if the underlying native key is not of type binary
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public byte[] getBinary() {
    if (!isBinary()) {
      throw new KeyBinaryValueException();
    }

    int binaryValueSize = getValueSize();
    byte[] returnValue = new byte[binaryValueSize];
    if (Elektra.INSTANCE.keyGetBinary(getPointer(), returnValue, binaryValueSize)
        != binaryValueSize) {
      throw new AssertionError(
          "'keyGetBinary' return value is not equal to expected binary value size");
    }
    return returnValue;
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setBoolean(boolean value) {
    return setString(Boolean.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setByte(byte value) {
    return setString(Byte.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setShort(short value) {
    return setString(Short.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setInt(int value) {
    return setString(Integer.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setLong(long value) {
    return setString(Long.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setFloat(float value) {
    return setString(Float.toString(value));
  }

  /**
   * Sets the key's value by converting {@code value} to string
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   */
  @Nonnull
  public Key setDouble(double value) {
    return setString(Double.toString(value));
  }

  /**
   * Sets the key's value
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code value} is {@code null}
   * @throws KeyException if the key's value is read-only or there have been allocation problems
   */
  @Nonnull
  public Key setString(String value) {
    argNotNull(value, "String 'value'");
    checkReturnValue(Elektra.INSTANCE.keySetString(getPointer(), value));
    return this;
  }

  /**
   * Sets the key's binary value
   *
   * @param value Value to set
   * @return This {@link Key}, enabling a fluent interface
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code value} is {@code null}
   * @throws KeyException if the key's value is read-only or there have been allocation problems
   */
  @Nonnull
  public Key setBinary(byte[] value) {
    argNotNull(value, "byte[] 'value'");
    checkReturnValue(Elektra.INSTANCE.keySetBinary(getPointer(), value, value.length));
    return this;
  }

  private void checkReturnValue(int returnValue) {
    if (returnValue < 0) {
      throw new KeyException();
    }
  }

  /**
   * Sets proper error meta for key
   *
   * @param text Reason for the error (format pattern)
   * @param args Custom arguments to the format pattern {@code text}
   * @return This {@link Key}, enabling a fluent interface
   */
  @Nonnull
  public Key setError(String text, Object... args) {
    StackTraceElement[] e = Thread.currentThread().getStackTrace();
    setMeta("error", "number description module file line function reason");
    setMeta("error/number", PluginMisbehaviorException.ERROR_NUMBER);
    setMeta("error/description", "jni/java error");
    setMeta("error/module", e[1].getClassName() + " " + e[1].getMethodName());
    setMeta("error/file", e[1].getFileName());
    setMeta("error/line", Integer.toString(e[1].getLineNumber()));
    setMeta("error/mountpoint", getName());
    setMeta("error/configfile", getString());
    setMeta("error/reason", String.format(text, args));
    return this;
  }

  /**
   * Adds warning meta for key
   *
   * @param text Reason for the error (format pattern)
   * @param args Custom arguments to the format pattern {@code text}
   * @return This {@link Key}, enabling a fluent interface
   */
  @Nonnull
  public Key addWarning(String text, Object... args) {
    StackTraceElement[] e = Thread.currentThread().getStackTrace();
    Optional<String> oMetaKeyValue = getMeta(WARNINGS).map(ReadableKey::getString);
    StringBuilder builder = new StringBuilder(WARNINGS + "/#");
    if (oMetaKeyValue.isEmpty()) {
      builder.append("00");
      setMeta(Key.WARNINGS, "00");
    } else {
      builder.append(oMetaKeyValue.get());
      builder.setCharAt(11, (char) (builder.charAt(11) + 1));
      if (builder.charAt(11) > '9') {
        builder.setCharAt(11, '0');
        builder.setCharAt(10, (char) (builder.charAt(10) + 1));
        if (builder.charAt(10) > '9') {
          builder.setCharAt(10, '0');
        }
      }
      setMeta(Key.WARNINGS, builder.substring(10));
    }
    setMeta(builder + "", "number description module file line function reason");
    setMeta(builder + "/number", PluginMisbehaviorException.ERROR_NUMBER);
    setMeta(builder + "/description", "jni/java warning");
    setMeta(builder + "/module", e[1].getClassName() + " " + e[1].getMethodName());
    setMeta(builder + "/file", e[1].getFileName());
    setMeta(builder + "/line", Integer.toString(e[1].getLineNumber()));
    setMeta(builder + "/mountpoint", getName());
    setMeta(builder + "/configfile", getString());
    setMeta(builder + "/reason", String.format(text, args));
    return this;
  }

  /**
   * Copies the information from the {@code source} key into <b>this</b> key.
   *
   * @param source Source Key object containing the information to copy
   * @param flags Flags indicating which parts of the key to copy<br>
   *     Example:<br>
   *     {@link #KEY_CP_NAME} | {@link #KEY_CP_VALUE}
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyException if copying failed
   * @throws IllegalStateException if this or the {@code source} {@link Key} has already been
   *     released
   * @throws IllegalArgumentException if {@code source} is {@code null}
   * @see #dup()
   * @see #dup(int)
   * @see #KEY_CP_ALL
   * @see #KEY_CP_META
   * @see #KEY_CP_NAME
   * @see #KEY_CP_STRING
   * @see #KEY_CP_VALUE
   */
  @Nonnull
  public Key copy(Key source, int flags) {
    argNotNull(source, "Key 'source'");
    if (Elektra.INSTANCE.keyCopy(getPointer(), source.getPointer(), flags) == null) {
      throw new KeyException();
    }
    return this;
  }

  /**
   * Rewinds the internal iterator for meta information of this key
   *
   * @throws IllegalStateException if this {@link Key} has already been released
   * @return This {@link Key}, enabling a fluent interface
   * @see #nextMeta()
   * @see #currentMeta()
   */
  @Nonnull
  public Key rewindMeta() {
    Elektra.INSTANCE.keyRewindMeta(getPointer());
    return this;
  }

  /**
   * Gets the next element of this key's internal meta information iterator
   *
   * @return New {@link Key} object containing the requested meta information or {@link
   *     Optional#empty()}, if no next meta key is available
   * @throws IllegalStateException if this {@link Key} has already been released
   * @see #rewindMeta()
   * @see #currentMeta()
   */
  @Nonnull
  public Optional<ReadableKey> nextMeta() {
    return createReadOnly(Elektra.INSTANCE.keyNextMeta(getPointer()));
  }

  /**
   * Gets the current element of this key's internal meta information iterator
   *
   * @return new {@link Key} object containing the current meta information
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws NoSuchElementException if no current meta key is available or internal iterator has
   *     been reset
   * @see #rewindMeta()
   * @see #nextMeta()
   */
  @Nonnull
  public ReadableKey currentMeta() {
    return checkPointer(
        Elektra.INSTANCE.keyCurrentMeta(getPointer()),
        ReadableKey::new,
        NoSuchElementException::new);
  }

  /**
   * Copies some meta information from a {@code source} key to this key
   *
   * @param source Key used as source
   * @param metaName Key name of the meta information to be copied
   * @return True, if meta was successfully copied, false if source does not contain the specified
   *     meta information and nothing had to be done
   * @throws KeyMetaException if this key's meta information is read-only of copying failed
   * @throws IllegalStateException if this or the {@code source} {@link Key} has already been
   *     released
   * @throws IllegalArgumentException if {@code source} is {@code null} or {@code metaName} is
   *     {@link String#isBlank() blank}
   * @see #copyAllMeta(Key)
   */
  public boolean copyMeta(Key source, String metaName) {
    argNotNull(source, "Key 'source'");
    argNotNullOrBlank(metaName, "String 'metaName'");
    int result = Elektra.INSTANCE.keyCopyMeta(getPointer(), source.getPointer(), metaName);
    if (result < 0) {
      throw new KeyMetaException();
    }
    return (result > 0);
  }

  /**
   * Copies all meta information from a {@code source} key to this key
   *
   * @param source Key used as source
   * @return True, if meta was successfully copied, false if {@code source} does not contain any
   *     meta and nothing had to be done
   * @throws KeyMetaException if copying failed
   * @throws IllegalStateException if this or the {@code source} {@link Key} has already been
   *     released
   * @throws IllegalArgumentException if {@code source} is {@code null}
   * @see #copyMeta(Key, String)
   */
  public boolean copyAllMeta(Key source) {
    argNotNull(source, "Key 'source'");
    int result = Elektra.INSTANCE.keyCopyAllMeta(getPointer(), source.getPointer());
    if (result < 0) {
      throw new KeyMetaException();
    }
    return (result > 0);
  }

  /**
   * Getter for meta information
   *
   * @param metaName Key name of meta information to be fetched
   * @return New {@link ReadableKey} object containing the requested meta information or {@link
   *     Optional#empty()}, if {@code metaName} was not found
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code metaName} is {@link String#isBlank() blank}
   */
  @Nonnull
  public Optional<ReadableKey> getMeta(String metaName) {
    argNotNullOrBlank(metaName, "String 'metaName'");
    return createReadOnly(Elektra.INSTANCE.keyGetMeta(getPointer(), metaName));
  }

  /**
   * Sets meta information
   *
   * @param metaName Key name of meta information to be set
   * @param newMetaString Meta value to be set
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyMetaException if {@code metaName} is invalid
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code metaName} is {@link String#isBlank() blank} or
   *     {@code newMetaString} is {@code null}
   */
  @Nonnull
  public Key setMeta(String metaName, String newMetaString) {
    argNotNullOrBlank(metaName, "String 'metaName'");
    argNotNull(newMetaString, "String 'newMetaString'");
    if (Elektra.INSTANCE.keySetMeta(getPointer(), metaName, newMetaString) < 0) {
      throw new KeyMetaException();
    }
    return this;
  }

  /**
   * Removes meta information
   *
   * @param metaName Key name of meta information to be removed
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyMetaException if {@code metaName} is invalid
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code metaName} is {@link String#isBlank() blank}
   */
  @Nonnull
  public Key removeMeta(String metaName) {
    argNotNullOrBlank(metaName, "String 'metaName'");
    if (Elektra.INSTANCE.keySetMeta(getPointer(), metaName, null) != 0) {
      throw new KeyMetaException();
    }
    return this;
  }

  /**
   * Sets the key's name
   *
   * @param name New key name to use
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyNameException if {@code name} is invalid, the key was inserted in a key set before
   *     or the key name is read-only
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code baseName} is {@link String#isBlank() blank}
   */
  @Nonnull
  public Key setName(String name) {
    argNotNullOrBlank(name, "String 'name'");
    if (Elektra.INSTANCE.keySetName(getPointer(), name) == -1) {
      throw new KeyNameException();
    }
    return this;
  }

  /**
   * Sets the key's base name; will replace current base name with new base name
   *
   * @param baseName New key base name to use
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyNameException if {@code baseName} is invalid, the key was inserted in a key set
   *     before or the key name is read-only
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code baseName} is {@code null}
   */
  @Nonnull
  public Key setBaseName(String baseName) throws KeyNameException {
    argNotNull(baseName, "String 'baseName'");
    if (Elektra.INSTANCE.keySetBaseName(getPointer(), baseName) == -1) {
      throw new KeyNameException();
    }
    return this;
  }

  /**
   * Adds key base name; will add given base name to current key so that new key is sub key of
   * current key
   *
   * @param baseName New key base name to add
   * @return This {@link Key}, enabling a fluent interface
   * @throws KeyNameException if {@code baseName} is invalid, the key was inserted in a key set
   *     before or the key name is read-only
   * @throws IllegalStateException if this {@link Key} has already been released
   * @throws IllegalArgumentException if {@code baseName} is {@link String#isBlank() blank}
   */
  @Nonnull
  public Key addBaseName(String baseName) {
    argNotNullOrBlank(baseName, "String 'baseName'");
    if (Elektra.INSTANCE.keyAddBaseName(getPointer(), baseName) == -1) {
      throw new KeyNameException();
    }
    return this;
  }

  /** @return {@link KeySetIterator} for the {@link ReadableKey meta data} of this {@link Key} */
  @Override
  public Iterator<ReadableKey> iterator() {
    return Optional.ofNullable(Elektra.INSTANCE.keyMeta(getPointer()))
        .map(KeySet::new)
        .map(ks -> (Iterator<ReadableKey>) new KeySetIterator<>(ks, ReadableKey::new))
        .orElse(Collections.<ReadableKey>emptyIterator());
  }
}
