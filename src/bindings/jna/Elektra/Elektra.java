package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public interface Elektra extends Library {
	Elektra INSTANCE = (Elektra)
		Native.loadLibrary(("elektra"), Elektra.class);
	Pointer keyNew(String name, Object... args);
	String keyName(Pointer k);
	String keyString(Pointer k);

	Pointer ksNew(int alloc, Object... args);
	Pointer ksLookup(Pointer ks, Pointer key, int options);

	Pointer kdbOpen(Pointer p);
	Pointer kdbGet(Pointer kdb, Pointer ks, Pointer key);
}
