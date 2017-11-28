package org.libelektra;

import java.util.Arrays;
import java.util.Iterator;

import com.sun.jna.Pointer;

/**
 * A keyset holds together a set of keys.
 */
public class KeySet implements java.lang.Iterable<Key> {

	// constants
	public static final int KDB_O_NONE = 0;
	public static final int KDB_O_DEL = 1;
	public static final int KDB_O_POP = 1 << 1;
	public static final int KDB_O_NODIR = 1 << 2;
	public static final int KDB_O_DIRONLY = 1 << 3;
	public static final int KDB_O_NOREMOVE = 1 << 6;
	public static final int KDB_O_REMOVEONLY = 1 << 7;
	public static final int KDB_O_INACTIVE = 1 << 8;
	public static final int KDB_O_SYNC = 1 << 9;
	public static final int KDB_O_SORT = 1 << 10;
	public static final int KDB_O_NORECURSIVE = 1 << 11;
	public static final int KDB_O_NOCASE = 1 << 12;
	public static final int KDB_O_WITHOWNER = 1 << 13;
	public static final int KDB_O_NOALL = 1 << 14;
	public static final Pointer KS_END = null;

	private Pointer ks;

	/**
	 * Helper constructor for duplication by pointer in long format
	 *
	 * @param p
	 *            Pointer to another KeySet in long format
	 */
	protected KeySet(final long p) {
		ks = new Pointer(p);
	}

	/**
	 * Helper constructor for duplication by pointer
	 *
	 * @param p
	 *            Pointer to another KeySet
	 */
	protected KeySet(final Pointer p) {
		ks = p;
	}

	/**
	 * Basic constructor for key set
	 *
	 * @param alloc
	 *            Length of key set (key count) to be allocated
	 * @param args
	 *            List of initial arguments for the key set. Example:<br>
	 *            new Key(...), new Key(...), existing_key_reference, KeySet.KS_END
	 * @return New key set with the given initial data
	 */
	protected static KeySet create(final int alloc, final Object... args) {
		int i = 0;
		for (i = 0; i < args.length; ++i) {
			if (args[i] instanceof Key) {
				final Key k = (Key) args[i];
				args[i] = k.get();
			}
		}
		if (args.length > 0 && args[i - 1] != KeySet.KS_END) {
			final Object[] sanitized = Arrays.copyOf(args, args.length + 1);
			sanitized[i] = KeySet.KS_END;
			return new KeySet(Elektra.INSTANCE.ksNew(alloc < sanitized.length ? alloc + 1 : sanitized.length, sanitized));
		}
		return new KeySet(Elektra.INSTANCE.ksNew(alloc, args));
	}

	/**
	 * Basic constructor for key set
	 *
	 * @param alloc
	 *            Length of key set (key count) to be allocated
	 * @param args
	 *            List of initial keys for the key set.
	 * @return New key set with the given initial data
	 */
	public static KeySet create(final int alloc, final Key... args) {
		if (args == null)
			return create(alloc);
		final Object[] keys = Arrays.copyOf(args, args.length + 1, Object[].class);
		keys[args.length] = KS_END;
		return create(alloc, keys);
	}

	/**
	 * Clean-up method to release keyset reference
	 */
	public void release() {
		// Otherwise this reference would most likely be lost, resulting in a potential leak
		if (ks != null) {
			Elektra.INSTANCE.ksDel(ks);
		}
		ks = null;
	}

	/**
	 * Clean-up method to inform underlying c-library about the release of the keyset reference
	 */
	@Override
	protected void finalize() throws Throwable {
		release();
	}

	/**
	 * Iterable interface function
	 *
	 * @return Custom KeySetIterator
	 */
	@Override
	public java.util.Iterator<Key> iterator() {
		return new KeySetIterator(this);
	}

	/**
	 * Basic java function that represents object as String.<br>
	 * Iterates though all keys in this key set and appends their representation to the output. Uses the toString() function of the Key
	 * objects.
	 *
	 * @return List of key-value pairs contained in this key set
	 */
	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		String sep = "";
		for (final Key k : this) {
			sb.append(sep);
			sb.append(k);
			sep = "\n";
		}
		return sb.toString();
	}

	/*
	 * Wrapped Methods
	 */

	/**
	 * Duplicates the key set
	 *
	 * @return New KeySet containing the same key references as this object does
	 */
	public KeySet dup() {
		return new KeySet(Elektra.INSTANCE.ksDup(get()));
	}

	/**
	 * Copies key references from other key set
	 *
	 * @param other
	 *            Key set that is used as source
	 * @return 1 in case of success, 0 if source was NULL and dest (this) was cleared successfully, -1 in case of an error (null pointer)
	 */
	public int copy(final KeySet other) {
		if (other == null)
			return -1;
		return Elektra.INSTANCE.ksCopy(get(), other.get());
	}

	/**
	 * Helper function to check if synchronization is necessary
	 *
	 * @return 1 if sync is necessary, 0 if no sync is necessary, -1 in case of an error (null key)
	 */
	public int needsSync() {
		return Elektra.INSTANCE.ksNeedSync(get());
	}

	/**
	 * Helper function that returns key set size
	 *
	 * @return Size of key set (number of possible keys)
	 */
	public int length() {
		return Elektra.INSTANCE.ksGetSize(get());
	}

	/**
	 * Helper function to append key to key set. Does nothing if null is provided.
	 *
	 * @param k
	 *            Key to append
	 * @return Index of key in key set; starting from 1, -1 if null was provided
	 */
	public int append(final Key k) {
		if (k == null) {
			return -1;
		}
		return Elektra.INSTANCE.ksAppendKey(get(), k.get());
	}

	/**
	 * Helper function that appends keys of key set
	 *
	 * @param ks
	 *            Key set to append
	 * @return Highest new index of key in key set; starting from 1, -1 if null was provided
	 */
	public int append(final KeySet ks) {
		if (ks == null) {
			return -1;
		}

		int result = -1;
		final Iterator<Key> iter = ks.iterator();
		while (iter.hasNext()) {
			result = Elektra.INSTANCE.ksAppendKey(get(), iter.next().get());
		}
		return result;
	}

	/**
	 * Helper function that creates new key set with help of a cut point
	 *
	 * @param cutpoint
	 *            Key that is used as cutting point
	 * @return New KeySet containing all keys until the cutting point, this if null was provided
	 */
	public KeySet cut(final Key cutpoint) {
		if (cutpoint == null)
			return this;
		return new KeySet(Elektra.INSTANCE.ksCut(get(), cutpoint.get()));
	}

	/**
	 * Helper function that returns key from key set and also removes it from the set
	 *
	 * @return First Key in the set
	 */
	public Key pop() {
		return new Key(Elektra.INSTANCE.ksPop(get()));
	}

	/**
	 * Helper function that returns current key from the key set
	 *
	 * @return Current Key in iteration
	 */
	public Key current() {
		return new Key(Elektra.INSTANCE.ksCurrent(get()));
	}

	/**
	 * Helper function that returns the next key in the key set
	 *
	 * @return Next Key in key set
	 */
	public Key next() {
		return new Key(Elektra.INSTANCE.ksNext(get()));
	}

	/**
	 * Helper function that rewinds the current key set
	 *
	 * @return
	 */
	public int rewind() {
		return Elektra.INSTANCE.ksRewind(get());
	}

	/**
	 * Helper function that gets the key set head
	 *
	 * @return First element of the key set
	 */
	public Key head() {
		return new Key(Elektra.INSTANCE.ksHead(get()));
	}

	/**
	 * Helper function that gets the key set tail
	 *
	 * @return Last element of the key set
	 */
	public Key tail() {
		return new Key(Elektra.INSTANCE.ksTail(get()));
	}

	/**
	 * Helper function that gets the current cursor of the key set
	 *
	 * @return Cursor position as integer; initially -1, incremented by next()
	 */
	public int getCursor() {
		return Elektra.INSTANCE.ksGetCursor(get());
	}

	/**
	 * Helper function that sets the current cursor of the key set
	 *
	 * @param cursor
	 *            Cursor position as integer
	 * @return 1 in case of success
	 */
	public int setCursor(final int cursor) {
		return Elektra.INSTANCE.ksSetCursor(get(), cursor);
	}

	/**
	 * Helper function that gets the Key at the given cursor position
	 *
	 * @param cursor
	 *            Cursor position used to fetch key; starting from 0
	 * @return Key at given cursor position
	 */
	public Key at(final int cursor) {
		return new Key(Elektra.INSTANCE.ksAtCursor(get(), cursor));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find
	 *            Key used in search
	 * @param options
	 *            Custom search options; concatenation of flags
	 * @return Key if search successful, null otherwise
	 */
	public Key lookup(final Key find, final int options) {
		if (find == null) {
			return null;
		}
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), options));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find
	 *            Key used in search
	 * @return Key if search successful, null otherwise
	 */
	public Key lookup(final Key find) {
		if (find == null) {
			return null;
		}
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), 0));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find
	 *            Key name used in search
	 * @param options
	 *            Custom search options; concatenation of flags
	 * @return Key if search successful, null otherwise
	 */
	public Key lookup(final String find, final int options) {
		return new Key(Elektra.INSTANCE.ksLookupByName(ks, find, options));
	}

	/**
	 * Helper function to search for a key in the key set
	 *
	 * @param find
	 *            Key name used in search
	 * @return Key if search successful, null otherwise
	 */
	public Key lookup(final String find) {
		return new Key(Elektra.INSTANCE.ksLookupByName(ks, find, 0));
	}

	/**
	 * Native pointer used by JNA
	 *
	 * @return Native pointer object used for this key set
	 */
	protected Pointer get() {
		return ks;
	}
}
