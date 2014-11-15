package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class KeySet {
	public static KeySet create(int alloc, Object... args) {
		return new KeySet(Elektra.INSTANCE.ksNew(alloc, args));
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
