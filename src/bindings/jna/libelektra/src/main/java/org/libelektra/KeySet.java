package org.libelektra;

import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Optional;
import javax.annotation.Nullable;
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
	 * Constructor associating a new {@link KeySet} instance with a native
	 * pointer in long format
	 *
	 * @param nativePointer Native pointer to key set in long format
	 */
	protected KeySet (long nativePointer)
	{
		this(nativePointer, false);
	}

	/**
	 * Constructor associating a new {@link KeySet} instance with a native
	 * pointer in long format<br>
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
		if (pointer == null)
		{
			throw new IllegalArgumentException ("Passed pointer must not be null");
		}
		this.pointer = pointer;
		cleanable = ReferenceCleaner.registerKeySetCleanUp (this);
	}

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
	public static KeySet create (int alloc, Object... args)
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
	public static KeySet create ()
	{
		return create (16);
	}

	/**
	 * Basic constructor for key set
	 *
	 * @param alloc Length of key set (key count) to be allocated
	 * @param args  List of initial keys for the key set.
	 * @return New key set with the given initial data
	 * @see #release()
	 */
	public static KeySet create (int alloc, Key... args)
	{
		if (args == null)
		{
			return create (alloc);
		}
		final Object[] keys = Arrays.copyOf (args, args.length + 1, Object[].class);
		keys[args.length] = KS_END;
		return create (alloc, keys);
	}

	/**
	 * Clean-up method to release key set reference by trying to free the native
	 * reference<br>
	 * <br>
	 * Call this method if you have obtained a {@link KeySet} via any of its public
	 * methods and you do not longer need it. If you do not manually release such
	 * {@link KeySet key sets}, they will get cleaned up by garbage collection as
	 * soon as they get phantom reachable. Therefore its encouraged to release
	 * {@link KeySet key set instances} as soon as you do not use them anymore.
	 */
	public void release ()
	{
		if (cleanable != null)
		{
			cleanable.clean ();
			cleanable = null;
			pointer = null;
		}
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

	/*
	 * Wrapped Methods
	 */

	/**
	 * Duplicates the key set
	 *
	 * @return New KeySet containing the same key references as this object does@see
	 *         #release()
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see #release()
	 */
	public KeySet dup ()
	{
		return new KeySet (Elektra.INSTANCE.ksDup (getPointer ()));
	}

	/**
	 * Copies key references from {@code source} to {@code this} {@link KeySet}
	 *
	 * @param source Key set that is used as source
	 * @throws KeySetReleasedException if this {@link KeySet} or the specified {@code source} has already been released
	 */
	public void copy (KeySet source)
	{
		if (source == null)
		{
			throw new IllegalArgumentException ("Passed source key set must not be null");
		}
		Elektra.INSTANCE.ksCopy (getPointer (), source.getPointer ());
	}

	/**
	 * Helper function to check if synchronization is necessary
	 *
	 * @return 1 if sync is necessary, 0 if no sync is necessary, -1 in case of an
	 *         error (null key)
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 */
	public int needsSync ()
	{
		return Elektra.INSTANCE.ksNeedSync (getPointer ());
	}

	/**
	 * Helper function that returns key set size
	 *
	 * @return Size of key set (number of possible keys)
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 */
	public int length ()
	{
		return Elektra.INSTANCE.ksGetSize (getPointer ());
	}

	/**
	 * Helper function to append key to key set. Does nothing if null is provided.
	 *
	 * @param key Key to append
	 * @return Index of key in key set; starting from 1, -1 if null was provided
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 */
	public int append (Key key)
	{
		if (key == null)
		{
			return -1;
		}
		return Elektra.INSTANCE.ksAppendKey (getPointer (), key.getPointer ());
	}

	/**
	 * Helper function that appends keys of key set
	 *
	 * @param keySet Key set to append
	 * @return Highest new index of key in key set; starting from 1, -1 if null was
	 *         provided
	 * @throws KeySetReleasedException if this {@link KeySet} or the specified {@code keySet} has already been released
	 */
	public int append (KeySet keySet)
	{
		if (keySet == null)
		{
			return -1;
		}

		int result = -1;
		Iterator<Key> iter = keySet.iterator ();
		while (iter.hasNext ())
		{
			result = Elektra.INSTANCE.ksAppendKey (getPointer (), iter.next ().getPointer ());
		}
		return result;
	}

	/**
	 * Helper function that creates new key set with help of a cut point
	 *
	 * @param cutpoint Key that is used as cutting point
	 * @return New KeySet containing all keys until the cutting point, this if null
	 *         was provided
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see #release()
	 */
	public KeySet cut (Key cutpoint)
	{
		if (cutpoint == null)
		{
			return this;
		}
		return new KeySet (Elektra.INSTANCE.ksCut (getPointer (), cutpoint.getPointer ()));
	}

	/**
	 * Helper function that returns key from key set and also removes it from the
	 * set
	 *
	 * @param cursor Cursor position of the key to remove; starting from 0
	 * @return First Key in the set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Key remove (int cursor)
	{
		return new Key (Elektra.INSTANCE.elektraKsPopAtCursor (getPointer (), cursor));
	}

	/**
	 * Helper function that gets the key set head
	 *
	 * @return First element of the key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Key head ()
	{
		return new Key (Elektra.INSTANCE.ksHead (getPointer ()));
	}

	/**
	 * Helper function that gets the key set tail
	 *
	 * @return Last element of the key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Key tail ()
	{
		return new Key (Elektra.INSTANCE.ksTail (getPointer ()));
	}

	/**
	 * Helper function that gets the Key at the given cursor position
	 *
	 * @param cursor Cursor position used to fetch key; starting from 0
	 * @return Key at given cursor position
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Key at (int cursor)
	{
		return new Key (Elektra.INSTANCE.ksAtCursor (getPointer (), cursor));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find Key used in search
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Optional<Key> lookup (Key find)
	{
		return lookup (find, 0);
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find    Key used in search
	 * @param options Custom search options; concatenation of flags
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Optional<Key> lookup (Key find, int options)
	{
		if (find == null)
		{
			return null;
		}
		return Key.create (Elektra.INSTANCE.ksLookup (getPointer (), find.getPointer (), options));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find Key name used in search
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Optional<Key> lookup (String find)
	{
		return lookup (find, 0);
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find    Key name used in search
	 * @param options Custom search options; concatenation of flags
	 * @return Key if search successful, {@link Optional#empty()} otherwise
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 * @see Key#release()
	 */
	public Optional<Key> lookup (String find, int options)
	{
		if (find == null)
		{
			return null;
		}
		return Key.create (Elektra.INSTANCE.ksLookupByName (getPointer (), find, options));
	}

	/**
	 * @return JNA pointer to the native pointer for this key set
	 * @throws KeySetReleasedException if this {@link KeySet} has already been released
	 */
	protected Pointer getPointer ()
	{
		if (pointer == null)
		{
			throw new KeySetReleasedException ();
		}
		return pointer;
	}
}
