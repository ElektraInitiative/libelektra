package elektra;

import com.sun.jna.Pointer;

public class KeyNameIterator implements java.util.Iterator<String> {
	
	private int pos=0;
	private int size=0;
	private Pointer con;

	/**
	 * Basic constructor for key name iterator
	 * @param key Key which name is used in iterator
	 */
	KeyNameIterator(Key key) {
		con = Elektra.INSTANCE.keyUnescapedName(key.get());
		size = Elektra.INSTANCE.keyGetUnescapedNameSize(key.get());
	}

	/**
	 * Checks if another value is available
	 * @return Boolean if another value is available
	 */
	public boolean hasNext() {
		return pos < size;
	}

	/**
	 * Gets the next value of iteration
	 * @return Next key name part in iteration
	 */
	public String next() {
		String ret = con.getString(pos);
		pos += ret.length()+1;
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
