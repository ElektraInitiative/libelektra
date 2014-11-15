package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class Key {
	public static Key create(String name, Object... args) {
		return new Key(Elektra.INSTANCE.keyNew(name, args));
	}

	public Key(Pointer p) {
		key = p;
	}

	public String name() {
		return Elektra.INSTANCE.keyName(key);
	}

	public String string() {
		return Elektra.INSTANCE.keyString(key);
	}

	public Pointer get() {
		return key;
	}

	private Pointer key;
}
