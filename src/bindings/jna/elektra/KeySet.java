package elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class KeySet implements java.lang.Iterable<Key> {
	// constants
	public static final int KDB_O_NONE=0;
	public static final int KDB_O_DEL=1;
	public static final int KDB_O_POP=1<<1;
	public static final int KDB_O_NODIR=1<<2;
	public static final int KDB_O_DIRONLY=1<<3;
	public static final int KDB_O_NOREMOVE=1<<6;
	public static final int KDB_O_REMOVEONLY=1<<7;
	public static final int KDB_O_INACTIVE=1<<8;
	public static final int KDB_O_SYNC=1<<9;
	public static final int KDB_O_SORT=1<<10;
	public static final int KDB_O_NORECURSIVE=1<<11;
	public static final int KDB_O_NOCASE=1<<12;
	public static final int KDB_O_WITHOWNER=1<<13;
	public static final int KDB_O_NOALL=1<<14;
	public static final Pointer KS_END= null;

	// basics, construction and destruction
	public static KeySet create(int alloc, Object... args) {
		for (int i=0; i<args.length-1; ++i) {
			if (args[i] instanceof Key) {
				Key k = (Key) args[i];
				args[i] = k.get();
			}
		}
		return new KeySet(Elektra.INSTANCE.ksNew(alloc, args));
	}

	public KeySet(long p) {
		ks = new Pointer(p);
	}

	public KeySet(Pointer p) {
		ks = p;
	}

	public void release() {
		ks = null;
	}

	protected void finalize() throws Throwable {
		Elektra.INSTANCE.ksDel(ks);
	}

	// java's specials
	public java.util.Iterator<Key> iterator() {
		return new KeySetIterator(this);
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		String sep = "";
		for (Key k:this) {
			sb.append(sep);
			sb.append(k);
			sep = "\n";
		}
		return sb.toString();
	}

	// wrapped methods
	public KeySet dup() {
		return new KeySet(Elektra.INSTANCE.ksDup(get()));
	}

	public int copy(KeySet other) {
		return Elektra.INSTANCE.ksCopy(get(), other.get());
	}

	public int needsSync() {
		return Elektra.INSTANCE.ksNeedSync(get());
	}

	public int length() {
		return Elektra.INSTANCE.ksGetSize(get());
	}

	public int append(Key k) {
		return Elektra.INSTANCE.ksAppendKey(get(), k.get());
	}

	public int append(KeySet ks) {
		return Elektra.INSTANCE.ksAppendKey(get(), ks.get());
		int result = -1;
		Iterator<Key> iter = ks.iterator();
		while(iter.hasNext()) {
			result = Elektra.INSTANCE.ksAppendKey(get(), iter.next().get());
		}
		return result;
	}

	public KeySet cut(Key cutpoint) {
		return new KeySet(Elektra.INSTANCE.ksCut(get(), cutpoint.get()));
	}

	public Key pop() {
		return new Key(Elektra.INSTANCE.ksPop(get()));
	}

	public Key current() {
		return new Key(Elektra.INSTANCE.ksCurrent(get()));
	}

	public Key next() {
		return new Key(Elektra.INSTANCE.ksNext(get()));
	}

	public int rewind() {
		return Elektra.INSTANCE.ksRewind(get());
	}

	public Key head() {
		return new Key(Elektra.INSTANCE.ksHead(get()));
	}

	public Key tail() {
		return new Key(Elektra.INSTANCE.ksTail(get()));
	}

	public int getCursor() {
		return Elektra.INSTANCE.ksGetCursor(get());
	}

	public int setCursor(int cursor) {
		return Elektra.INSTANCE.ksSetCursor(get(), cursor);
	}

	public Key at(int cursor) {
		return new Key(Elektra.INSTANCE.ksAtCursor(get(), cursor));
	}

	public Key lookup(Key find, int options) {
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), options));
	}

	public Key lookup(Key find) {
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), 0));
	}

	public Key lookup(String find, int options) {
		return new Key(Elektra.INSTANCE.ksLookupByName(ks, find, options));
	}

	public Key lookup(String find) {
		return new Key(Elektra.INSTANCE.ksLookupByName(ks, find, 0));
	}

	// native pointer
	public Pointer get() {
		return ks;
	}

	private Pointer ks;
}
