package org.libelektra;

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
import org.libelektra.exception.KeySetAppendException;
import org.libelektra.exception.KeySetReleasedException;

/**
 * Java representation of a native Elektra key set, a container for keys
 */
public class KeySet implements Iterable<Key>
{

	// constants
	public static final int KDB_O_NONE = 0;
	public static final int KDB_O_DEL = 1;
	public static final int KDB_O_POP = 1 << 1;
	public static final Pointer KS_END = null;

	@Nullable private Pointer pointer;

	@Nullable private Cleaner.Cleanable cleanable;

	/**
	 * Constructs a new {@link KeySet} with the specified content<br>
	 * <br>
	 * Example: KeySet keySet = KeySet.create(10, Key.create("A"), Key.create("B"));
	 *
	 * @param alloc Hint indicating the expected size of the key set
	 * @param args  List of initial arguments for the key set. Example:<br>
	 *              new Key(...), new Key(...), existing_key_reference,
	 *              KeySet.KS_END
	 * @return New key set with the specified initial data
	 * @see #release()
	 */
	@Nonnull public static KeySet create (int alloc, Object... args)
	{
		int i = 0;
		for (i = 0; i < args.length; ++i)
		{
			if (args[i] instanceof Key)
			{
				Key k = (Key) args[i];
				args[i] = k.getPointer ();
			}
		}
		if (args.length > 0 && args[i - 1] != KeySet.KS_END)
		{
			Object[] sanitized = Arrays.copyOf (args, args.length + 1);
			sanitized[i] = KeySet.KS_END;
			return new KeySet (Elektra.INSTANCE.ksNew (alloc > sanitized.length ? alloc : sanitized.length, sanitized));
		}
		return new KeySet (Elektra.INSTANCE.ksNew (alloc, args));
	}

	/**
	 * Constructs an empty {@link KeySet} with a default allocation hint of 16
	 *
	 * @return Newly allocated key set
	 * @see #release()
	 */
	@Nonnull public static KeySet create ()
	{
		return create (16);
	}

	/**
	 * Constructor associating a new {@link KeySet} instance with a native pointer
	 * in long format
	 *
	 * @param nativePointer Native pointer to key set in long format
	 */
	protected KeySet (long nativePointer)
	{
		this(nativePointer, false);
	}

	/**
	 * Constructor associating a new {@link KeySet} instance with a native pointer
	 * in long format<br>
	 * <br>
	 * Suppressing clean-up has been introduced for usage of this binding as JNI
	 * plug-in and should normally not be used in any other case.
	 *
	 * @param nativePointer   Native pointer to key set in long format
	 * @param suppressCleanUp True to suppress native reference clean-up as soon as
	 *                        this {@link KeySet} instance becomes phantom
	 *                        reachable, false otherwise
	 * @see #release()
	 */
	protected KeySet (long nativePointer, boolean suppressCleanUp)
	{
		pointer = new Pointer (nativePointer);
		cleanable = (suppressCleanUp ? null : ReferenceCleaner.registerKeySetCleanUp (this)); // see #3825
	}

	/**
	 * Constructor associating a new {@link KeySet} instance with a JNA pointer
	 *
	 * @param pointer JNA {@link Pointer} to key set
	 * @throws IllegalArgumentException if {@code pointer} is {@code null}
	 * @see #release()
	 */
	protected KeySet (Pointer pointer)
	{
		this.pointer = argNotNull (pointer, "Pointer 'pointer'");
		cleanable = ReferenceCleaner.registerKeySetCleanUp (this);
	}

	/**
	 * Clean-up method to release key set reference by trying to free the native
	 * reference<br>
	 * <br>
	 * Call this method if you have obtained a {@link KeySet} via any of its public
	 * methods or {@link KDB#get(Key)} and you do not longer need it. If you do not
	 * manually release such {@link KeySet key sets}, they will get cleaned up by
	 * garbage collection as soon as they get phantom reachable. Therefore its
	 * encouraged to release {@link KeySet key set instances} as soon as you do not
	 * use them anymore.
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
	 * @return New {@link KeySetIterator} backed by this {@link KeySet}
	 */
	@Override public Iterator<Key> iterator ()
	{
		return new KeySetIterator (this);
	}

	/**
	 * Iterates though all keys in this key set and appends their representation to
	 * the output. Uses the toString() function of the Key objects.
	 *
	 * @return Represents this {@link KeySet} as string
	 */
	@Override public String toString ()
	{
		StringBuilder sb = new StringBuilder ();
		String sep = "";
		for (Key k : this)
		{
			sb.append (sep);
			sb.append (k);
			sep = "\n";
		}
		return sb.toString ();
	}

	/**
	 * Duplicates the key set
	 *
	 * @return New {@link KeySet} containing the same key references as this
	 *         {@link KeySet} does
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 * @see #release()
	 */
	@Nonnull public KeySet dup ()
	{
		return new KeySet (Elektra.INSTANCE.ksDup (getPointer ()));
	}

	/**
	 * Copies key references from {@code source} to {@code this} {@link KeySet}
	 *
	 * @param source Key set that is used as source
	 * @throws KeySetReleasedException  if this {@link KeySet} or the specified
	 *                                  {@code source} has already been released
	 * @throws IllegalArgumentException if {@code source} is {@code null}
	 */
	public void copy (KeySet source)
	{
		argNotNull (source, "KeySet 'source'");
		Elektra.INSTANCE.ksCopy (getPointer (), source.getPointer ());
	}

	/**
	 * Indicates whether synchronization is necessary
	 *
	 * @return True if sync is necessary, false otherwise
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 */
	public boolean needsSync ()
	{
		return Elektra.INSTANCE.ksNeedSync (getPointer ()) > 0;
	}

	/**
	 * Indicates the key set size
	 *
	 * @return Number of keys contained by this key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 */
	public int size ()
	{
		return Elektra.INSTANCE.ksGetSize (getPointer ());
	}

	/**
	 * Append key to key set
	 *
	 * @param key {@link Key} to append
	 * @return This {@link KeySet}, enabling a fluent interface
	 * @throws IllegalArgumentException if {@code key} is {@code null}
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 */
	@Nonnull public KeySet append (Key key)
	{
		argNotNull (key, "Key 'key'");
		if (Elektra.INSTANCE.ksAppendKey (getPointer (), key.getPointer ()) <= 0)
		{
			throw new KeySetAppendException ();
		}
		return this;
	}

	/**
	 * Appends keys from key set
	 *
	 * @param source Source {@link KeySet} to append all of its {@link Key keys}
	 * @return This {@link KeySet}, enabling a fluent interface
	 * @throws KeySetReleasedException  if this {@link KeySet} or the specified
	 *                                  {@code source} has already been released
	 * @throws IllegalArgumentException if {@code source} is {@code null}
	 * @throws KeySetAppendException    if appending the {@code source} failed
	 */
	@Nonnull public KeySet append (KeySet source)
	{
		argNotNull (source, "KeySet 'keySet'");
		if (Elektra.INSTANCE.ksAppend (getPointer (), source.getPointer ()) < 0)
		{
			throw new KeySetAppendException ();
		}
		return this;
	}

	/**
	 * Creates new key set with help of a cut point
	 *
	 * @param cutpoint Key that is used as cutting point
	 * @return New KeySet containing all keys until the cutting point, this if null
	 *         was provided
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code cutpoint} is {@code null}
	 * @see #release()
	 */
	@Nonnull public KeySet cut (Key cutpoint)
	{
		argNotNull (cutpoint, "Key 'cutpoint'");
		return new KeySet (Elektra.INSTANCE.ksCut (getPointer (), cutpoint.getPointer ()));
	}

	/**
	 * Returns key from key set and also removes it from the set
	 *
	 * @param cursor Cursor position of the key to remove; starting from 0
	 * @return Key found at given cursor position
	 * @throws KeySetReleasedException   if this {@link KeySet} has already been
	 *                                   released
	 * @throws IndexOutOfBoundsException if position is out of bounds
	 * @see Key#release()
	 */
	@Nonnull public Key remove (int cursor)
	{
		return checkKeyPointer (Elektra.INSTANCE.elektraKsPopAtCursor (getPointer (), cursor), IndexOutOfBoundsException::new);
	}

	/**
	 * Gets the key set head key
	 *
	 * @return First element of the key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 * @throws NoSuchElementException  if key set is empty
	 * @see Key#release()
	 */
	@Nonnull public Key head ()
	{
		return checkKeyPointer (Elektra.INSTANCE.ksHead (getPointer ()), NoSuchElementException::new);
	}

	/**
	 * Gets the key set tail key
	 *
	 * @return Last element of the key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 * @throws NoSuchElementException  if key set is empty
	 * @see Key#release()
	 */
	@Nonnull public Key tail ()
	{
		return checkKeyPointer (Elektra.INSTANCE.ksTail (getPointer ()), NoSuchElementException::new);
	}

	/**
	 * Gets the key at the given cursor position
	 *
	 * @param cursor Cursor position used to fetch key; starting from 0
	 * @return Key found at specified cursor position
	 * @throws KeySetReleasedException   if this {@link KeySet} has already been
	 *                                   released
	 * @throws IndexOutOfBoundsException if position is out of bounds
	 * @see Key#release()
	 */
	@Nonnull public Key at (int cursor)
	{
		return checkKeyPointer (Elektra.INSTANCE.ksAtCursor (getPointer (), cursor), IndexOutOfBoundsException::new);
	}

	/**
	 * Search for a key in the key set
	 *
	 * @param find Key used in search
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code key} is {@code null}
	 * @see Key#release()
	 */
	@Nonnull public Optional<Key> lookup (Key find)
	{
		return lookup (find, 0);
	}

	/**
	 * Search for a key in the key set
	 *
	 * @param find    Key used in search
	 * @param options Custom search options; concatenation of flags
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code find} is {@code null}
	 * @see Key#release()
	 */
	@Nonnull public Optional<Key> lookup (Key find, int options)
	{
		argNotNull (find, "Key 'find'");
		return Key.create (Elektra.INSTANCE.ksLookup (getPointer (), find.getPointer (), options));
	}

	/**
	 * Search for a key in the key set
	 *
	 * @param find Key name used in search
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code find} is {@link String#isBlank()
	 *                                  blank}
	 * @see Key#release()
	 */
	@Nonnull public Optional<Key> lookup (String find)
	{
		return lookup (find, 0);
	}

	/**
	 * Search for a key in the key set
	 *
	 * @param find    Key name used in search
	 * @param options Custom search options; concatenation of flags
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException  if this {@link KeySet} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code find} is {@link String#isBlank()
	 *                                  blank}
	 * @see Key#release()
	 */
	@Nonnull public Optional<Key> lookup (String find, int options)
	{
		argNotNullOrBlank (find, "String 'find'");
		return Key.create (Elektra.INSTANCE.ksLookupByName (getPointer (), find, options));
	}

	/**
	 * @return JNA pointer to the native pointer for this key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been
	 *                                 released
	 */
	@Nonnull protected Pointer getPointer ()
	{
		if (pointer == null)
		{
			throw new KeySetReleasedException ();
		}
		return pointer;
	}
}
