package org.libelektra;

import static org.libelektra.Elektra.KeyNewArgumentFlags.KEY_END;
import static org.libelektra.Elektra.KeyNewArgumentFlags.KEY_META;
import static org.libelektra.Elektra.KeyNewArgumentFlags.KEY_VALUE;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.Arrays;
import java.util.Optional;
import javax.annotation.Nullable;
import org.libelektra.Elektra.KeyNewArgumentFlags;
import org.libelektra.exception.KeyBinaryTypeNotSupportedException;
import org.libelektra.exception.KeyCreateFailedException;
import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.KeySetNameFailedException;
import org.libelektra.exception.PluginMisbehaviorException;

/**
 * Key is an essential class that encapsulates key name , value and metainfo.
 */
public class Key implements Iterable<String>
{

	private static final String WARNINGS = "warnings";

	// constants
	public static final int KEY_CP_NAME = 1 << 0;
	public static final int KEY_CP_STRING = 1 << 1;
	public static final int KEY_CP_VALUE = 1 << 2;
	public static final int KEY_CP_META = 1 << 3;
	public static final int KEY_CP_ALL = KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META;

	/**
	 * Use this as name for a new {@link Key} if a local key is required (e.g.
	 * usable for error feedback used in some API methods)
	 */
	public static final String KEY_LOCAL_NAME = Elektra.KEY_LOCAL_NAME;

	@Nullable private Pointer pointer;

	@Nullable private Cleaner.Cleanable cleanable;

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
	protected Key (@Nullable Pointer pointer)
	{
		this.pointer = pointer;
		ReferenceCleaner.keyWrapperCreated (this);
		cleanable = ReferenceCleaner.registerKeyCleanUp (this);
	}

	/**
	 * Constructor associating a new {@link Key} instance with a JNA pointer
	 *
	 * @param pointer Optional JNA {@link Pointer} to key
	 * @return New {@link Key} instance if {@code pointer} is non-null,
	 *         {@link Optional#empty()} otherwise
	 * @see #release()
	 */
	protected static Optional<Key> create (@Nullable Pointer pointer)
	{
		return Optional.ofNullable (pointer).map (Key::new);
	}

	/**
	 * Constructs a new {@link Key} with the specified content and arguments<br>
	 *
	 * @param name Key name; first part of key-value pair
	 * @param args Arguments used for key value. Example:<br>
	 *             KeyNewArgumentFlags.KEY_VALUE, "custom key value",
	 *             KeyNewArgumentFlags.KEY_END
	 * @return New key
	 * @throws KeyCreateFailedException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	protected static Key create (String name, Object... args)
	{
		return create (Elektra.INSTANCE.keyNew (
				       name,
				       Arrays.stream (args)
					       .map (o -> (o instanceof KeyNewArgumentFlags) ? ((KeyNewArgumentFlags) o).getValue () : o)
					       .toArray ()))
			.orElseThrow (KeyCreateFailedException::new);
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
	 * @throws KeyCreateFailedException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	public static Key create (String name, @Nullable Object value, Key... meta)
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
	 * @throws KeyCreateFailedException if the key name is invalid
	 * @see #KEY_LOCAL_NAME
	 * @see #release()
	 */
	public static Key create (String name, Key... meta)
	{
		return create (name, null, meta);
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
	@Override public java.util.Iterator<String> iterator ()
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
	public String getString () throws KeyBinaryTypeNotSupportedException
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
	 * Helper function to set proper error meta for key
	 *
	 * @param text Reason for the error
	 * @param args Custom arguments
	 */
	public void setError (String text, Object... args)
	{
		StackTraceElement[] e = Thread.currentThread ().getStackTrace ();
		setMeta ("error", "number description module file line function reason");
		setMeta ("error/number", PluginMisbehaviorException.errorNumber ());
		setMeta ("error/description", "jni/java error");
		setMeta ("error/module", e[1].getClassName () + " " + e[1].getMethodName ());
		setMeta ("error/file", e[1].getFileName ());
		setMeta ("error/line", Integer.toString (e[1].getLineNumber ()));
		setMeta ("error/mountpoint", getName ());
		setMeta ("error/configfile", getString ());
		setMeta ("error/reason", String.format (text, args));
	}

	/**
	 * Helper function to add warning meta for key
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
		setMeta (builder + "/number", PluginMisbehaviorException.errorNumber ());
		setMeta (builder + "/description", "jni/java warning");
		setMeta (builder + "/module", e[1].getClassName () + " " + e[1].getMethodName ());
		setMeta (builder + "/file", e[1].getFileName ());
		setMeta (builder + "/line", Integer.toString (e[1].getLineNumber ()));
		setMeta (builder + "/mountpoint", getName ());
		setMeta (builder + "/configfile", getString ());
		setMeta (builder + "/reason", String.format (text, args));
	}

	/*
	 * Wrapped methods
	 */

	/**
	 * Duplicates the key
	 *
	 * @return New Key object containing the same information as this key
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #release()
	 */
	public Key dup ()
	{
		return dup (KEY_CP_ALL);
	}

	/**
	 * Duplicates the key
	 *
	 * @param flags what parts of the key to copy (a combination of KEY_CP_* flags)
	 * @return New Key object containing the same information as this key
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #release()
	 */
	public Key dup (int flags)
	{
		return new Key (Elektra.INSTANCE.keyDup (getPointer (), flags));
	}

	/**
	 * Copies the information from the {@code source} key into this key. Does
	 * nothing if {@code source} is {@code null}.
	 *
	 * @param source Source Key object containing the information to copy
	 * @param flags  what parts of the key to copy (a combination of KEY_CP_* flags)
	 * @throws KeyReleasedException if this or the {@code source} {@link Key} has
	 *                              already been released
	 */
	public void copy (Key source, int flags)
	{
		if (source != null)
		{
			Elektra.INSTANCE.keyCopy (getPointer (), source.getPointer (), flags);
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
	 * Tries to rewind the meta information for this key
	 *
	 * @return 0 in case of no errors; 1 if key is not found; 2 if metakey is not
	 *         found
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int rewindMeta ()
	{
		return Elektra.INSTANCE.keyRewindMeta (getPointer ());
	}

	/**
	 * Gets the next meta information for this key
	 *
	 * @return new Key object containing the next meta information
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #release()
	 */
	public Key nextMeta ()
	{
		return new Key (Elektra.INSTANCE.keyNextMeta (getPointer ()));
	}

	/**
	 * Gets the current meta information for this key
	 *
	 * @return new Key object containing the current meta information
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #release()
	 */
	public Key currentMeta ()
	{
		return new Key (Elektra.INSTANCE.keyNextMeta (getPointer ()));
	}

	/**
	 * Helper function to copy some meta information from a source Key to this key.
	 *
	 * @param source   Key object that is used as source
	 * @param metaName Key name of the meta to be copied
	 * @return 1 if meta was successfully copied, 0 if source doesn't contain the
	 *         required meta and nothing had to be done, -1 in case of an error or
	 *         if the source parameter was null
	 * @throws KeyReleasedException if this or the {@code source} {@link Key} has
	 *                              already been released
	 */
	public int copyMeta (Key source, String metaName)
	{
		if (source == null)
		{
			return -1;
		}
		return Elektra.INSTANCE.keyCopyMeta (getPointer (), source.getPointer (), metaName);
	}

	/**
	 * Helper function to copy all meta information from a source key to this key
	 *
	 * @param source Key object that is used as source
	 * @return 1 if meta was successfully copied, 0 if source doesn't contain any
	 *         meta and nothing had to be done, -1 in case of an error or if the
	 *         source parameter was null
	 * @throws KeyReleasedException if this or the {@code source} {@link Key} has
	 *                              already been released
	 */
	public int copyAllMeta (Key source)
	{
		if (source == null)
		{
			return -1;
		}
		return Elektra.INSTANCE.keyCopyAllMeta (getPointer (), source.getPointer ());
	}

	/**
	 * Getter for meta information
	 *
	 * @param metaName Key name of meta information to be fetched
	 * @return New Key object containing the requested meta information or
	 *         {@link Optional#empty()}, if {@code metaName} was not found
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 * @see #release()
	 */
	public Optional<Key> getMeta (String metaName)
	{
		return create (Elektra.INSTANCE.keyGetMeta (getPointer (), metaName));
	}

	/**
	 * Setter for meta information
	 *
	 * @param metaName      Key name of meta information to be set
	 * @param newMetaString Meta value to be set
	 * @return -1 in case of an error, 0 if no meta with given name is available for
	 *         the key and value &gt; 0 representing the size of newMetaString if
	 *         update successful
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int setMeta (String metaName, String newMetaString)
	{
		return Elektra.INSTANCE.keySetMeta (getPointer (), metaName, newMetaString);
	}

	/**
	 * Helper function to compare two keys. Compares the key name with normal String
	 * comparison.
	 *
	 * @param other Other Key object that is used in comparison
	 * @return 0 if key name is equal; -1 if this key name has lower alphabetical
	 *         order than the other key; 1 if this key has higher alphabetical order
	 * @throws KeyReleasedException if this or the {@code other} {@link Key} has
	 *                              already been released
	 */
	public int cmp (Key other)
	{
		if (other == null)
		{
			throw new IllegalArgumentException ("other should be a key, not null");
		}
		return Integer.signum (Elektra.INSTANCE.keyCmp (getPointer (), other.getPointer ()));
	}

	/**
	 * Helper function to check if synchronization is necessary
	 *
	 * @return 1 if needs sync, 0 if no change done and -1 in case of a null pointer
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public int needsSync ()
	{
		return Elektra.INSTANCE.keyNeedSync (getPointer ());
	}

	/**
	 * Helper function to check if key is sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is (non-direct) sub-key of other-key
	 * @throws KeyReleasedException if this or the {@code other} {@link Key} has
	 *                              already been released
	 */
	public boolean isBelow (Key other)
	{
		if (other == null)
		{
			throw new IllegalArgumentException ("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyIsBelow (other.getPointer (), getPointer ()) == 1;
	}

	/**
	 * Helper function to check if key is other key or sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is other key or (non-direct) sub-key of other-key
	 * @throws KeyReleasedException if this or the {@code other} {@link Key} has
	 *                              already been released
	 */
	public boolean isBelowOrSame (Key other)
	{
		if (other == null)
		{
			throw new IllegalArgumentException ("other should be a key, not null");
		}
		return Elektra.INSTANCE.keyIsBelowOrSame (other.getPointer (), getPointer ()) == 1;
	}

	/**
	 * Helper function to check if key is direct sub-key of other key
	 *
	 * @param other Key that is used in check as parent key
	 * @return Boolean if this key is direct sub-key of other key ("child")
	 * @throws KeyReleasedException if this or the {@code other} {@link Key} has
	 *                              already been released
	 */
	public boolean isDirectBelow (Key other)
	{
		if (other == null)
		{
			throw new IllegalArgumentException ("other should be a key, not null");
		}
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
	public String getName ()
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
	 * @throws KeySetNameFailedException if the key name is invalid, the key was
	 *                                   inserted in a key set before or the key
	 *                                   name is read-only
	 * @throws KeyReleasedException      if this {@link Key} has already been
	 *                                   released
	 */
	public void setName (String name)
	{
		if (Elektra.INSTANCE.keySetName (getPointer (), name) == -1)
		{
			throw new KeySetNameFailedException ();
		}
	}

	/**
	 * @return Key's base name as String
	 * @throws KeyReleasedException if this {@link Key} has already been released
	 */
	public String getBaseName ()
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
	 * @throws KeySetNameFailedException if the key name is invalid, the key was
	 *                                   inserted in a key set before or the key
	 *                                   name is read-only
	 * @throws KeyReleasedException      if this {@link Key} has already been
	 *                                   released
	 */
	public void setBaseName (String baseName) throws KeySetNameFailedException
	{
		if (Elektra.INSTANCE.keySetBaseName (getPointer (), baseName) == -1)
		{
			throw new KeySetNameFailedException ();
		}
	}

	/**
	 * Adds key base name; will add given base name to current key so that new key
	 * is sub key of current key
	 *
	 * @param baseName New key base name to add
	 * @throws KeySetNameFailedException if the key name is invalid, the key was
	 *                                   inserted in a key set before or the key
	 *                                   name is read-only
	 * @throws KeyReleasedException      if this {@link Key} has already been
	 *                                   released
	 */
	public void addBaseName (String baseName) throws KeySetNameFailedException
	{
		if (Elektra.INSTANCE.keyAddBaseName (getPointer (), baseName) == -1)
		{
			throw new KeySetNameFailedException ();
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
	protected Pointer getPointer ()
	{
		if (pointer == null)
		{
			throw new KeyReleasedException ();
		}
		return pointer;
	}
}
