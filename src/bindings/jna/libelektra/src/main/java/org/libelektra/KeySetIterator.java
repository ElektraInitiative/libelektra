package org.libelektra;

import java.util.Iterator;
import java.util.NoSuchElementException;
import org.libelektra.exception.KeySetReleasedException;

/**
 * An {@link Iterator} for a {@link KeySet} returning {@link Key}s
 */
public class KeySetIterator implements Iterator<Key>
{

	private final KeySet keySet;
	private int position = 0;
	private Key current;

	/**
	 * @param keySet {@link KeySet} backing this iterator
	 */
	KeySetIterator (final KeySet keySet)
	{
		this.keySet = keySet;
	}

	/**
	 * @return True, if another value is available, false otherwise
	 * @throws KeySetReleasedException if this backing {@link KeySet} has already
	 *                                 been released
	 */
	@Override public boolean hasNext ()
	{
		return position != keySet.size ();
	}

	/**
	 * Gets the next value
	 *
	 * @return Next key in iteration
	 * @throws KeySetReleasedException if this backing {@link KeySet} has already
	 *                                 been released
	 * @throws NoSuchElementException  if end of key set is reached
	 * @apiNote {@link Key Keys} returned by this method normally should not be
	 *          {@link Key#release() released} manually!
	 */
	@Override public Key next ()
	{
		current = keySet.at (position);
		++position;
		return current;
	}

	/**
	 * Removes the element of the {@link KeySet} backing the iterator
	 *
	 * @throws KeySetReleasedException if this backing {@link KeySet} has already
	 *                                 been released
	 */
	@Override public void remove ()
	{
		keySet.lookup (current, KeySet.KDB_O_POP).ifPresent (Key::release);
		--position;
	}
}
