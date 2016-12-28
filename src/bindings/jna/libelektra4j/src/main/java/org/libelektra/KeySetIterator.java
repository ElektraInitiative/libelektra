package org.libelektra;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An {@link Iterator} over a {@link KeySet} resulting in {@link Key}s.
 */
public class KeySetIterator implements java.util.Iterator<Key> {

	private int pos = 0;
	private final KeySet con;

	/**
	 * Basic constructor for key set iterator
	 *
	 * @param container
	 *            KeySet which is used in iterator
	 */
	KeySetIterator(final KeySet container) {
		con = container;
	}

	/**
	 * Checks if another value is available
	 *
	 * @return Boolean if another value is available
	 */
	@Override
	public boolean hasNext() {
		return pos != con.length();
	}

	/**
	 * Gets the next value of iteration
	 *
	 * @return Next Key in iteration
	 */
	@Override
	public Key next() {
		if (pos == con.length()) {
			throw new NoSuchElementException("End of KeySet reached");
		}

		final Key ret = con.at(pos);
		++pos;
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
