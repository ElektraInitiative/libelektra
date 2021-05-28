package org.libelektra;

import static org.libelektra.Key.KeyNewArgumentTag.KEY_END;
import static org.libelektra.Key.KeyNewArgumentTag.KEY_META;
import static org.libelektra.Key.KeyNewArgumentTag.KEY_VALUE;
import static org.libelektra.ValidationUtil.argNotNull;
import static org.libelektra.ValidationUtil.argNotNullOrBlank;
import static org.libelektra.ValidationUtil.checkKeyPointer;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.libelektra.exception.KeyBinaryTypeNotSupportedException;
import org.libelektra.exception.KeyException;
import org.libelektra.exception.KeyMetaException;
import org.libelektra.exception.KeyNameException;
import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.PluginMisbehaviorException;

/**
 * Key represents an native Elektra key providing acces to its name, value and
 * meta information
 */
public class Key implements Iterable<String>
{

	private static final String WARNINGS = "warnings";

	/**
	 * Argument tags for use with {@link #create(String, Object...)}
	 */
	public enum KeyNewArgumentTag {

		/**
		 * Used as a parameter terminator
		 */
		KEY_END (0),

		/**
		 * Flag for the key name
		 */
		KEY_NAME (1),

		/**
		 * Flag for the key data
		 */
		KEY_VALUE (1 << 1),

		/**
		 * Flag for the key comment
		 */
		KEY_COMMENT (1 << 3),

		/**
		 * Flag if the key is binary
		 */
		KEY_BINARY (1 << 4),

		/**
		 * Flag for maximum size to limit value
		 */
		KEY_SIZE (1 << 11),

		/**
		 * Flag for metadata
		 */
		KEY_META (1 << 15);

		public final Integer value;

		private KeyNewArgumentTag (int value)
		{
			this.value = Integer.valueOf (value);
		}
	}

	/**
	 * Flag for use with {@link #copy(Key, int)} for copying the key name
	 */
	public static final int KEY_CP_NAME = 1 << 0;

	/**
	 * Flag for use with {@link #copy(Key, int)} for copying the key value, if it is
	 * a string
	 *
	 * @apiNote Do not use together with {@link #KEY_CP_VALUE}
	 */
	public static final int KEY_CP_STRING = 1 << 1;

	/**
	 * Flag for use with {@link #copy(Key, int)} for copying the key value
	 *
	 * @apiNote Do not use together with {@link #KEY_CP_STRING}
	 */
	public static final int KEY_CP_VALUE = 1 << 2;

	/**
	 * Flag for use with {@link #copy(Key, int)} for copying the key metadata
	 */
	public static final int KEY_CP_META = 1 << 3;

	/**
	 * Flag for use with {@link #copy(Key, int)} for copying the key name, value and
	 * metadata
	 */
	public static final int KEY_CP_ALL = KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META;

	/**
	 * Use this as name for a new {@link Key} if a local key is required (e.g.
	 * usable for error feedback used in some API methods)
	 */
	public static final String KEY_LOCAL_NAME = Elektra.KEY_LOCAL_NAME;

	@Nullable private Pointer pointer;

	@Nullable private Cleaner.Cleanable cleanable;

	/**
	 * Constructs a new {@link Key} instance associated with a JNA pointer
	 *
	 * @param pointer Optional JNA {@link Pointer} to key
	 * @return New {@link Key} instance if {@code pointer} is non-null,
	 *         {@link Optional#empty()} otherwise
	 * @see #release()
	 */
	@Nonnull protected static Optional<Key> create (@Nullable Pointer pointer)
	{
		return Optional.ofNullable (pointer).map (Key::new);
	}

	/**
	 * Constructs a new {@link Key} with the specified content and arguments<br>
	 *
	 * @param name Key name; first part of key-value pair
	 * @param args Arguments used for key value<br>
	 *             Example:<br>
	 *             {@link KeyNewArgumentTag#KEY_VALUE}, "custom key value",
	 *             {@link KeyNewArgumentTag#KEY_END}
	 * @return New key
	 * @throws KeyNameException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	@Nonnull protected static Key create (String name, Object... args)
	{
		return create (Elektra.INSTANCE.keyNew (
				       name, Arrays.stream (args)
						     .map (o -> (o instanceof KeyNewArgumentTag) ? ((KeyNewArgumentTag) o).value : o)
						     .toArray ()))
			.orElseThrow (KeyNameException::new);
	}

	/**
	 * Constructs a new {@link Key} with the specified content and arguments<br>
	 *
	 * @param name  Key name; first part of key-value pair
	 * @param value Key value; will be determine from the object by calling
	 *              {@link Object#toString()}, null is supported too
	 * @param meta  Metadata that should be added to this key, null keys will be
	 *              filtered away
	 * @return New key
	 * @throws KeyNameException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	@Nonnull public static Key create (String name, @Nullable Object value, Key... meta)
	{
		int size = 0;
		for (Key m : meta)
		{
			if (m != null)
			{
				size++;
			}
		}
		// 3 -> KEY_VALUE, value, KEY_END, 4 -> one more for KEY_META
		size += size > 0 ? 4 : 3;
		Object[] args = new Object[size];
		int cur = 0;
		args[cur++] = KEY_VALUE;
		args[cur++] = value != null ? value.toString () : null;
		if (size > 3)
		{
			args[cur++] = KEY_META;
			for (Key m : meta)
			{
				args[cur++] = m;
			}
		}
		args[cur] = KEY_END;
		return create (name, args);
	}

	/**
	 * Basic constructor of key class
	 *
	 * @param name Key name; first part of key-value pair
	 * @param meta Metadata that should be added to this key. Will filter null
	 *             values.
	 * @return New key object
	 * @throws KeyNameException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	@Nonnull public static Key create (String name, Key... meta)
	{
		return create (name, null, meta);
	}

	/**
	 * Constructor associating a new {@link Key} instance with a native pointer in
	 * long format
	 *
	 * @param nativePointer Native pointer to key in long format
	 * @see #release()
	 * @implNote Increased the native key's reference counter
	 */
	protected Key (long nativePointer)
	{
		this(nativePointer, false);
	}

	/**
	 * Constructor associating a new {@link Key} instance with a native pointer in
	 * long format<br>
	 * <br>
	 * Suppressing clean-up has been introduced for usage of this binding as JNI
	 * plug-in and should normally not be used in any other case.
	 *
	 * @param nativePointer   Native pointer to key in long format
	 * @param suppressCleanUp True to suppress native reference clean-up as soon as
	 *                        this {@link Key} instance becomes phantom reachable,
	 *                        false otherwise
	 * @see #release()
	 * @implNote Increased the native key's reference counter, even if
	 *           {@code suppressCleanUp} is {@code true}
	 */
	protected Key (long nativePointer, boolean suppressCleanUp)
	{
		pointer = new Pointer (nativePointer);
		ReferenceCleaner.keyWrapperCreated (this);
		cleanable = (suppressCleanUp ? null : ReferenceCleaner.registerKeyCleanUp (this)); // see #3825
	}

	/**
	 * Constructor associating a new {@link Key} instance with a JNA pointer
	 *
	 * @param pointer JNA {@link Pointer} to key
	 * @see #release()
	 */
	protected Key (Pointer pointer)
	{
		argNotNull (pointer, "Pointer 'pointer'");
		this.pointer = pointer;
		ReferenceCleaner.keyWrapperCreated (this);
		cleanable = ReferenceCleaner.registerKeyCleanUp (this);
	}

	/**
	 * Clean-up method to release key reference by first decrementing its reference
	 * counter and then trying to free the native reference<br>
	 * <br>
	 * Call this method if you do not longer need a {@link Key} and obtained it via
	 * any of its public methods or the public methods of {@link KeySet}. If you do
	 * not manually release such {@link Key keys}, they will get cleaned up by
	 * garbage collection as soon as they get phantom reachable. Therefore its
	 * encouraged to release {@link Key key instances} as soon as you do not use
	 * them anymore.
	 */
	public void release ()
	{
		if (cleanable != null)
		{
			cleanable.clean ();
			cleanable = null;
		}
		pointer = null;
	}

	/**
	 * @return Key name in string format as returned by {@link #getName()}
	 */
	@Override public String toString ()
	{
		return getName ();
	}

	/**
	 * @return New {@link KeyNameIterator} backed by this {@link Key}
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	@Override public Iterator<String> iterator ()
	{
		return new KeyNameIterator (this);
	}

	/**
	 * @return {@link #getString()} interpreted as boolean value
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public boolean getBoolean ()
	{
		return Boolean.parseBoolean (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as {@code byte}
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable {@code byte}
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public byte getByte ()
	{
		return Byte.parseByte (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as {@code short}
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable
	 *                                            {@code short}
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public short getShort ()
	{
		return Short.parseShort (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as integer
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable integer
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public int getInteger ()
	{
		return Integer.parseInt (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as {@code long}
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable {@code long}
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public long getLong ()
	{
		return Long.parseLong (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as {@code float}
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable
	 *                                            {@code float}
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public float getFloat ()
	{
		return Float.parseFloat (getString ());
	}

	/**
	 * @return {@link #getString()} parsed as {@code double}
	 * @throws NumberFormatException              if the {@link #getString()} does
	 *                                            not return a parsable
	 *                                            {@code double}
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	public double getDouble ()
	{
		return Double.parseDouble (getString ());
	}

	/**
	 * @return This key's value as string
	 * @throws KeyBinaryTypeNotSupportedException if the underlying native key is of
	 *                                            type binary
	 * @throws KeyReleasedException               if this {@link Key} has already
	 *                                            been released
	 */
	@Nonnull public String getString () throws KeyBinaryTypeNotSupportedException
	{
		if (isBinary ())
		{
			throw new KeyBinaryTypeNotSupportedException ();
		}
		return Elektra.INSTANCE.keyString (getPointer ());
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setBoolean (boolean value)
	{
		setString (Boolean.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setByte (byte value)
	{
		setString (Byte.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setShort (short value)
	{
		setString (Short.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setInteger (int value)
	{
		setString (Integer.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setLong (long value)
	{
		setString (Long.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setFloat (float value)
	{
		setString (Float.toString (value));
	}

	/**
	 * Sets the key's value by converting {@code value} to string
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setDouble (double value)
	{
		setString (Double.toString (value));
	}

	/**
	 * Sets the key's value
	 *
	 * @param value Value to set
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public void setString (String value)
	{
		Elektra.INSTANCE.keySetString (getPointer (), value);
	}

	/**
	 * Sets proper error meta for key
	 *
	 * @param text Reason for the error
	 * @param args Custom arguments
	 */
	public void setError (String text, Object... args)
	{
		StackTraceElement[] e = Thread.currentThread ().getStackTrace ();
		setMeta ("error", "number description module file line function reason");
		setMeta ("error/number", PluginMisbehaviorException.ERROR_NUMBER);
		setMeta ("error/description", "jni/java error");
		setMeta ("error/module", e[1].getClassName () + " " + e[1].getMethodName ());
		setMeta ("error/file", e[1].getFileName ());
		setMeta ("error/line", Integer.toString (e[1].getLineNumber ()));
		setMeta ("error/mountpoint", getName ());
		setMeta ("error/configfile", getString ());
		setMeta ("error/reason", String.format (text, args));
	}

	/**
	 * Adds warning meta for key
	 *
	 * @param text Reason for the warning
	 * @param args Custom arguments
	 */
	public void addWarning (String text, Object... args)
	{
		StackTraceElement[] e = Thread.currentThread ().getStackTrace ();
		Optional<String> oMetaKeyValue = getMeta (WARNINGS).map (Key::getString);
		StringBuilder builder = new StringBuilder (WARNINGS + "/#");
		if (oMetaKeyValue.isEmpty ())
		{
			builder.append ("00");
			setMeta (Key.WARNINGS, "00");
		}
		else
		{
			builder.append (oMetaKeyValue.get ());
			builder.setCharAt (11, (char) (builder.charAt (11) + 1));
			if (builder.charAt (11) > '9')
			{
				builder.setCharAt (11, '0');
				builder.setCharAt (10, (char) (builder.charAt (10) + 1));
				if (builder.charAt (10) > '9')
				{
					builder.setCharAt (10, '0');
				}
			}
			setMeta (Key.WARNINGS, builder.substring (10));
		}
		setMeta (builder + "", "number description module file line function reason");
		setMeta (builder + "/number", PluginMisbehaviorException.ERROR_NUMBER);
		setMeta (builder + "/description", "jni/java warning");
		setMeta (builder + "/module", e[1].getClassName () + " " + e[1].getMethodName ());
		setMeta (builder + "/file", e[1].getFileName ());
		setMeta (builder + "/line", Integer.toString (e[1].getLineNumber ()));
		setMeta (builder + "/mountpoint", getName ());
		setMeta (builder + "/configfile", getString ());
		setMeta (builder + "/reason", String.format (text, args));
	}

	/**
	 * Duplicates the key
	 *
	 * @return New Key object containing the same information as this key
	 * @throws KeyException         if copying failed
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #dup(int)
	 * @see #copy(Key, int)
	 * @see #release()
	 */
	@Nonnull public Key dup ()
	{
		return dup (KEY_CP_ALL);
	}

	/**
	 * Duplicates the key
	 *
	 * @param flags Flags indicating which parts of the key to copy<br>
	 *              Example:<br>
	 *              {@link #KEY_CP_NAME} | {@link #KEY_CP_VALUE}
	 * @return New Key object containing the same information as this key
	 * @throws KeyException         if copying failed
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #dup()
	 * @see #copy(Key, int)
	 * @see #release()
	 */
	@Nonnull public Key dup (int flags)
	{
		Pointer result = Elektra.INSTANCE.keyDup (getPointer (), flags);
		if (result == null)
		{
			throw new KeyException ();
		}
		return new Key (result);
	}

	/**
	 * Copies the information from the {@code source} key into this key.
	 *
	 * @param source Source Key object containing the information to copy
	 * @param flags  Flags indicating which parts of the key to copy<br>
	 *               Example:<br>
	 *               {@link #KEY_CP_NAME} | {@link #KEY_CP_VALUE}
	 * @throws KeyException             if copying failed
	 * @throws KeyReleasedException     if this or the {@code source} {@link Key}
	 *                                  has already been released
	 * @throws IllegalArgumentException if {@code source} is {@code null}
	 * @see #dup()
	 * @see #dup(int)
	 */
	public void copy (Key source, int flags)
	{
		argNotNull (source, "Key 'source'");
		if (Elektra.INSTANCE.keyCopy (getPointer (), source.getPointer (), flags) == null)
		{
			throw new KeyException ();
		}
	}

	/**
	 * Increments the reference counter for the underlying native key
	 *
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	protected void incRef ()
	{
		Elektra.INSTANCE.keyIncRef (getPointer ());
	}

	/**
	 * Decrements the reference counter for the underlying native key
	 *
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	protected void decRef ()
	{
		Elektra.INSTANCE.keyDecRef (getPointer ());
	}

	/**
	 * Gets the reference counter for the underlying native key
	 *
	 * @return Current reference counter value
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	protected int getRef ()
	{
		return Elektra.INSTANCE.keyGetRef (getPointer ());
	}

	/**
	 * Rewinds the internal iterator for meta information of this key
	 *
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #nextMeta()
	 * @see #currentMeta()
	 */
	public void rewindMeta ()
	{
		Elektra.INSTANCE.keyRewindMeta (getPointer ());
	}

	/**
	 * Gets the next element of this key's internal meta information iterator
	 *
	 * @return new Key object containing the next meta information
	 * @throws KeyReleasedException   if this {@link Key} has already been released
	 * @throws NoSuchElementException if no next meta key is available
	 * @see #rewindMeta()
	 * @see #currentMeta()
	 * @see #release()
	 */
	@Nonnull public Key nextMeta ()
	{
		return checkKeyPointer (Elektra.INSTANCE.keyNextMeta (getPointer ()), NoSuchElementException::new);
	}

	/**
	 * Gets the current element of this key's internal meta information iterator
	 *
	 * @return new {@link Key} object containing the current meta information
	 * @throws KeyReleasedException   if this {@link Key} has already been released
	 * @throws NoSuchElementException if no next meta key is available or internal
	 *                                iterator has been reset
	 * @see #rewindMeta()
	 * @see #nextMeta()
	 * @see #release()
	 */
	@Nonnull public Key currentMeta ()
	{
		return checkKeyPointer (Elektra.INSTANCE.keyCurrentMeta (getPointer ()), NoSuchElementException::new);
	}

	/**
	 * Copies some meta information from a {@code source} key to this key
	 *
	 * @param source   Key used as source
	 * @param metaName Key name of the meta information to be copied
	 * @return True, if meta was successfully copied, false if source does not
	 *         contain the specified meta information and nothing had to be done
	 * @throws KeyMetaException         if this key's meta information is read-only
	 *                                  of copying failed
	 * @throws KeyReleasedException     if this or the {@code source} {@link Key}
	 *                                  has already been released
	 * @throws IllegalArgumentException if {@code source} is {@code null} or
	 *                                  {@code metaName} is {@link String#isBlank()
	 *                                  blank}
	 * @see #copyAllMeta(Key)
	 */
	public boolean copyMeta (Key source, String metaName)
	{
		argNotNull (source, "Key 'source'");
		argNotNullOrBlank (metaName, "String 'metaName'");
		int result = Elektra.INSTANCE.keyCopyMeta (getPointer (), source.getPointer (), metaName);
		if (result < 0)
		{
			throw new KeyMetaException ();
		}
		return (result > 0);
	}

	/**
	 * Copies all meta information from a {@code source} key to this key
	 *
	 * @param source Key used as source
	 * @return True, if meta was successfully copied, false if {@code source} does
	 *         not contain any meta and nothing had to be done
	 * @throws KeyMetaException         if copying failed
	 * @throws KeyReleasedException     if this or the {@code source} {@link Key}
	 *                                  has already been released
	 * @throws IllegalArgumentException if {@code source} is {@code null}
	 * @see #copyMeta(Key, String)
	 */
	public boolean copyAllMeta (Key source)
	{
		argNotNull (source, "Key 'source'");
		int result = Elektra.INSTANCE.keyCopyAllMeta (getPointer (), source.getPointer ());
		if (result < 0)
		{
			throw new KeyMetaException ();
		}
		return (result > 0);
	}

	/**
	 * Getter for meta information
	 *
	 * @param metaName Key name of meta information to be fetched
	 * @return New {@link Key} object containing the requested meta information or
	 *         {@link Optional#empty()}, if {@code metaName} was not found
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code metaName} is
	 *                                  {@link String#isBlank() blank}
	 * @see #release()
	 */
	@Nonnull public Optional<Key> getMeta (String metaName)
	{
		argNotNullOrBlank (metaName, "String 'metaName'");
		return create (Elektra.INSTANCE.keyGetMeta (getPointer (), metaName));
	}

	/**
	 * Sets meta information
	 *
	 * @param metaName      Key name of meta information to be set
	 * @param newMetaString Meta value to be set
	 * @throws KeyMetaException         if {@code metaName} is invalid
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code metaName} is
	 *                                  {@link String#isBlank() blank} or
	 *                                  {@code newMetaString} is {@code null}
	 */
	public void setMeta (String metaName, String newMetaString)
	{
		argNotNullOrBlank (metaName, "String 'metaName'");
		argNotNull (newMetaString, "String 'newMetaString'");
		if (Elektra.INSTANCE.keySetMeta (getPointer (), metaName, newMetaString) < 0)
		{
			throw new KeyMetaException ();
		}
	}

	/**
	 * Removes meta information
	 *
	 * @param metaName Key name of meta information to be removed
	 * @throws KeyMetaException         if {@code metaName} is invalid
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code metaName} is
	 *                                  {@link String#isBlank() blank}
	 */
	public void removeMeta (String metaName)
	{
		argNotNullOrBlank (metaName, "String 'metaName'");
		if (Elektra.INSTANCE.keySetMeta (getPointer (), metaName, null) != 0)
		{
			throw new KeyMetaException ();
		}
	}

	/**
	 * Compares this key with the {@code other} keys by comparing the key name with
	 * string comparison
	 *
	 * @param other Other key to compare this key to
	 * @return
	 *         <ul>
	 *         <li>0 if key name is equal</li>
	 *         <li>-1 if this key name has lower alphabetical order than the
	 *         {@code other} key</li>
	 *         <li>1 if this key has higher alphabetical order</li>
	 *         </ul>
	 * @throws KeyReleasedException     if this or the {@code other} {@link Key} has
	 *                                  already been released
	 * @throws IllegalArgumentException if {@code other} is {@code null}
	 */
	public int cmp (Key other)
	{
		argNotNull (other, "Key 'other'");
		return Integer.signum (Elektra.INSTANCE.keyCmp (getPointer (), other.getPointer ()));
	}

	/**
	 * Checks whether synchronization is necessary
	 *
	 * @return True, if this key needs sync, false if no change was done
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public boolean needsSync ()
	{
		return Elektra.INSTANCE.keyNeedSync (getPointer ()) > 0;
	}

	/**
	 * Checks whether this key is sub-key of the {@code other} key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is (non-direct) sub-key of other-key
	 * @throws KeyReleasedException     if this or the {@code other} {@link Key} has
	 *                                  already been released
	 * @throws IllegalArgumentException if {@code other} is {@code null}
	 */
	public boolean isBelow (Key other)
	{
		argNotNull (other, "Key 'other'");
		return Elektra.INSTANCE.keyIsBelow (other.getPointer (), getPointer ()) == 1;
	}

	/**
	 * Checks whether this key is the same as the {@code other} key or a sub-key of
	 * the {@code other} key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is other key or (non-direct) sub-key of other-key
	 * @throws KeyReleasedException     if this or the {@code other} {@link Key} has
	 *                                  already been released
	 * @throws IllegalArgumentException if {@code other} is {@code null}
	 */
	public boolean isBelowOrSame (Key other)
	{
		argNotNull (other, "Key 'other'");
		return Elektra.INSTANCE.keyIsBelowOrSame (other.getPointer (), getPointer ()) == 1;
	}

	/**
	 * Checks whether this key is direct sub-key of the {@code other} key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is direct sub-key of other key ("child")
	 * @throws KeyReleasedException     if this or the {@code other} {@link Key} has
	 *                                  already been released
	 * @throws IllegalArgumentException if {@code other} is {@code null}
	 */
	public boolean isDirectBelow (Key other)
	{
		argNotNull (other, "Key 'other'");
		return Elektra.INSTANCE.keyIsDirectlyBelow (other.getPointer (), getPointer ()) == 1;
	}

	/**
	 * @return True if the underlying native key's value is of type binary, false
	 *         otherwise
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public boolean isBinary ()
	{
		return Elektra.INSTANCE.keyIsBinary (getPointer ()) == 1;
	}

	/**
	 * @return True if the underlying native key's value is a valid string, false
	 *         otherwise
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public boolean isString ()
	{
		return Elektra.INSTANCE.keyIsString (getPointer ()) == 1;
	}

	/**
	 * @return Key name (key part of "key-value" pair)
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	@Nonnull public String getName ()
	{
		return Elektra.INSTANCE.keyName (getPointer ());
	}

	/**
	 * @return Length of key name
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int getNameSize ()
	{
		return Elektra.INSTANCE.keyGetNameSize (getPointer ());
	}

	/**
	 * Sets the key's name
	 *
	 * @param name New key name to use
	 * @throws KeyNameException         if {@code name} is invalid, the key was
	 *                                  inserted in a key set before or the key name
	 *                                  is read-only
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code baseName} is
	 *                                  {@link String#isBlank() blank}
	 */
	public void setName (String name)
	{
		argNotNullOrBlank (name, "String 'name'");
		if (Elektra.INSTANCE.keySetName (getPointer (), name) == -1)
		{
			throw new KeyNameException ();
		}
	}

	/**
	 * @return Key's base name as String
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	@Nonnull public String getBaseName ()
	{
		return Elektra.INSTANCE.keyBaseName (getPointer ());
	}

	/**
	 * @return Length of key's base name
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int getBaseNameSize ()
	{
		return Elektra.INSTANCE.keyGetBaseNameSize (getPointer ());
	}

	/**
	 * Sets the key's base name; will replace current base name with new base name
	 *
	 * @param baseName New key base name to use
	 * @throws KeyNameException         if {@code baseName} is invalid, the key was
	 *                                  inserted in a key set before or the key name
	 *                                  is read-only
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code baseName} is {@code null}
	 */
	public void setBaseName (String baseName) throws KeyNameException
	{
		argNotNull (baseName, "String 'baseName'");
		if (Elektra.INSTANCE.keySetBaseName (getPointer (), baseName) == -1)
		{
			throw new KeyNameException ();
		}
	}

	/**
	 * Adds key base name; will add given base name to current key so that new key
	 * is sub key of current key
	 *
	 * @param baseName New key base name to add
	 * @throws KeyNameException         if {@code baseName} is invalid, the key was
	 *                                  inserted in a key set before or the key name
	 *                                  is read-only
	 * @throws KeyReleasedException     if this {@link Key} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code baseName} is
	 *                                  {@link String#isBlank() blank}
	 */
	public void addBaseName (String baseName)
	{
		argNotNullOrBlank (baseName, "String 'baseName'");
		if (Elektra.INSTANCE.keyAddBaseName (getPointer (), baseName) == -1)
		{
			throw new KeyNameException ();
		}
	}

	/**
	 * @return Length/Size of key value
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int getValueSize ()
	{
		return Elektra.INSTANCE.keyGetValueSize (getPointer ());
	}

	/**
	 * @return JNA pointer to the native pointer for this key
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	@Nonnull protected Pointer getPointer ()
	{
		if (pointer == null)
		{
			throw new KeyReleasedException ();
		}
		return pointer;
	}
}
