package Elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class Key {
	public static final int KEY_END=0;
	public static final int KEY_NAME=1;
	public static final int KEY_VALUE=1<<1;
	public static final int KEY_OWNER=1<<2;
	public static final int KEY_COMMENT=1<<3;
	public static final int KEY_BINARY=1<<4;
	public static final int KEY_UID=1<<5;
	public static final int KEY_GID=1<<6;
	public static final int KEY_MODE=1<<7;
	public static final int KEY_ATIME=1<<8;
	public static final int KEY_MTIME=1<<9;
	public static final int KEY_CTIME=1<<10;
	public static final int KEY_SIZE=1<<11;
	public static final int KEY_FUNC=1<<12;
	public static final int KEY_DIR=1<<14;
	public static final int KEY_META=1<<15;
	public static final int KEY_NULL=1<<16;

	public static Key create(String name, Object... args) {
		return new Key(Elektra.INSTANCE.keyNew(name, args));
	}

	public void incRef() {
		Elektra.INSTANCE.keyIncRef(key);
	}

	public void decRef() {
		Elektra.INSTANCE.keyDecRef(key);
	}

	public Key(long p) {
		key = new Pointer(p);
		incRef();
	}

	public Key(Pointer p) {
		key = p;
		incRef();
	}

	public void release() {
		key = null;
	}

	protected void finalize() throws Throwable {
		decRef();
		Elektra.INSTANCE.keyDel(key);
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
