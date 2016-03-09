package elektra;

public class KeySetIterator implements java.util.Iterator<Key> {
	private int pos= 0;
	private KeySet con;

	KeySetIterator(KeySet container) {
		con = container;
	}

	public boolean hasNext() {
		return pos != con.length();
	}

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
