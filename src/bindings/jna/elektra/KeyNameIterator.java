package elektra;

import com.sun.jna.Pointer;

public class KeyNameIterator implements java.util.Iterator<String> {
	private int pos=0;
	private int size=0;
	private Pointer con;

	KeyNameIterator(Key key) {
		con = Elektra.INSTANCE.keyUnescapedName(key.get());
		size = Elektra.INSTANCE.keyGetUnescapedNameSize(key.get());
	}

	public boolean hasNext() {
		return pos < size;
	}

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
