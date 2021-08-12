package org.libelektra;

import static org.libelektra.ValidationUtil.checkKeyPointer;

import com.sun.jna.Pointer;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Function;
import org.libelektra.exception.KeySetReleasedException;

/**
 * An {@link Iterator} for a {@link KeySet} returning {@link Key}s
 */
public class KeySetIterator<T extends ReadOnlyKey> implements Iterator<T>
{

	private final KeySet keySet;
	private int position = 0;
	private T current;
	private Function<Pointer, T> factory;

	/**
	 * @param keySet  {@link KeySet} backing this iterator
	 * @param factory Factory for creating elements of type {@code T} from
	 *                {@link Pointer}
	 */
	KeySetIterator (KeySet keySet, Function<Pointer, T> factory)
	{
		this.keySet = keySet;
		this.factory = factory;
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
	@Override public T next ()
	{
		current = checkKeyPointer (Elektra.INSTANCE.ksAtCursor (keySet.getPointer (), position), factory,
					   NoSuchElementException::new);
		++position;
		return current;
	}

	/**
	 * Removes the {@code Key} element of the {@link KeySet} backing the iterator
	 * afterwards releasing it. Therefore any reference held to the removed
	 * {@code Key} element will get unusable.
	 *
	 * @throws KeySetReleasedException if this backing {@link KeySet} has already
	 *                                 been released
	 */
	@Override public void remove ()
	{
		keySet.remove (current);
		current.release ();
		--position;
	}
}
