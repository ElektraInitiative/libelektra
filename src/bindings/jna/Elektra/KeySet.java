package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class KeySet {
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

	public static KeySet create(int alloc, Object... args) {
		return new KeySet(Elektra.INSTANCE.ksNew(alloc, args));
	}

	public KeySet(long p) {
		ks = new Pointer(p);
	}

	public KeySet(Pointer p) {
		ks = p;
	}

	public Key lookup(Key find, int options) {
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), options));
	}

	public Key lookup(Key find) {
		return new Key(Elektra.INSTANCE.ksLookup(ks, find.get(), 0));
	}

	public Pointer get() {
		return ks;
	}

	private Pointer ks;
}
