package elektra;

public class KeySetIterator implements java.util.Iterator<Key> {
	private int pos= 0;
	private KeySet con;

	/**
	 * Basic constructor for key set iterator
	 * @param container KeySet which is used in iterator
	 */
	KeySetIterator(KeySet container) {
		con = container;
	}

	/**
	 * Checks if another value is available
	 * @return Boolean if another value is available
	 */
	public boolean hasNext() {
		return pos != con.length();
	}

	/**
	 * Gets the next value of iteration
	 * @return Next Key in iteration
	 */
	public Key next() {
		Key ret = con.at(pos);
		++pos;
		return ret;
	}

	/**
	 * NOT SUPPORTED
	 * @throws UnsupportedOperationException
	 */
	public void remove() {
	    throw new UnsupportedOperationException();
	}
}
