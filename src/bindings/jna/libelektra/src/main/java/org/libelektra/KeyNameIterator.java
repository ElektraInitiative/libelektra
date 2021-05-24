package org.libelektra;

import com.sun.jna.Pointer;
import java.util.Iterator;
import java.util.NoSuchElementException;
import org.libelektra.exception.KeyReleasedException;

/**
 * An {@link Iterator} over a {@link Key}'s name parts, separated by /.
 */
public class KeyNameIterator implements Iterator<String>
{

	private int pos = 0;
	private int size = 0;
	private final Pointer con;

	/**
	 * Basic constructor for key name iterator
	 *
	 * @param key Key which name is used in iterator
	 * @throws KeyReleasedException if {@code key} has already been released
	 */
	KeyNameIterator (final Key key)
	{
		con = Elektra.INSTANCE.keyUnescapedName (key.getPointer ());
		size = Elektra.INSTANCE.keyGetUnescapedNameSize (key.getPointer ());
	}

	/**
	 * Checks if another value is available
	 *
	 * @return Boolean if another value is available
	 */
	@Override public boolean hasNext ()
	{
		return pos < size;
	}

	/**
	 * Gets the next value of iteration
	 *
	 * @return Next key name part in iteration
	 */
	@Override public String next ()
	{
		if (pos == size)
		{
			throw new NoSuchElementException ("End of key names reached");
		}

		final String ret = con.getString (pos);
		pos += ret.length () + 1;
		return ret;
	}

	/**
	 * NOT SUPPORTED
	 *
	 * @throws UnsupportedOperationException TODO #3754 detailed exception description
	 */
	@Override public void remove ()
	{
		throw new UnsupportedOperationException ();
	}
}
