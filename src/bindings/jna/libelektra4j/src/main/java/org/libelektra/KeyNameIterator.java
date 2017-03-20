package org.libelektra;

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.sun.jna.Pointer;

/**
 * An {@link Iterator} over a {@link Key}'s name parts, separated by /.
 */
public class KeyNameIterator implements java.util.Iterator<String> {

	private int pos = 0;
	private int size = 0;
	private final Pointer con;

	/**
	 * Basic constructor for key name iterator
	 *
	 * @param key
	 *            Key which name is used in iterator
	 */
	KeyNameIterator(final Key key) {
		con = Elektra.INSTANCE.keyUnescapedName(key.get());
		size = Elektra.INSTANCE.keyGetUnescapedNameSize(key.get());
	}

	/**
	 * Checks if another value is available
	 *
	 * @return Boolean if another value is available
	 */
	@Override
	public boolean hasNext() {
		return pos < size;
	}

	/**
	 * Gets the next value of iteration
	 *
	 * @return Next key name part in iteration
	 */
	@Override
	public String next() {
		if (pos == size) {
			throw new NoSuchElementException("End of key names reached");
		}

		final String ret = con.getString(pos);
		pos += ret.length() + 1;
		return ret;
	}

	/**
	 * NOT SUPPORTED
	 *
	 * @throws UnsupportedOperationException
	 */
	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
