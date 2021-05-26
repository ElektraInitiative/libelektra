package org.libelektra;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An {@link Iterator} over a {@link KeySet} resulting in {@link Key}s.
 */
public class KeySetIterator implements Iterator<Key>
{

	private int position = 0;
	private final KeySet container;
	private Key current;

	/**
	 * Basic constructor for key set iterator
	 *
	 * @param container KeySet which is used in iterator
	 */
	KeySetIterator (final KeySet container)
	{
		this.container = container;
	}

	/**
	 * Checks if another value is available
	 *
	 * @return Boolean if another value is available
	 */
	@Override public boolean hasNext ()
	{
		return position != container.length ();
	}

	/**
	 * Gets the next value of iteration
	 *
	 * @return Next Key in iteration
	 * @apiNote {@link Key Keys} returned by this method normally should not be {@link Key#release() released} manually!
	 */
	@Override public Key next ()
	{
		if (position == container.length ())
		{
			throw new NoSuchElementException ("End of key set reached");
		}

		current = container.at (position);
		++position;
		return current;
	}

	/**
	 * Removes the element of the {@link KeySet} backing the iterator
	 */
	@Override public void remove ()
	{
		container.lookup (current, KeySet.KDB_O_POP).ifPresent (Key::release);
		--position;
	}
}
