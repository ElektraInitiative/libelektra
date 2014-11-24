package elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class KDB implements AutoCloseable {
	// exceptions
	public class KDBException extends java.io.IOException {
		KDBException(Key k) {
			super(new Throwable("failure in I/O to KDB"));
			errorKey = k;
		}

		Key errorKey;
	}


	// basics, construction and destruction
	public static KDB open(Key parentKey) {
		return new KDB(Elektra.INSTANCE.kdbOpen(parentKey.get()));
	}

	public KDB(Pointer p) {
		kdb = p;
	}

	// java's specials
	@Override
	public void close() {
		Key k = Key.create("", Key.KEY_END);
		close(k);
	}

	// wrapped methods
	public void get(KeySet ks, Key parentKey) throws KDBException {
		int ret = Elektra.INSTANCE.kdbGet(kdb, ks.get(),
				parentKey.get());
		if (ret == -1) {
			throw new KDBException(parentKey);
		}
	}

	public void set(KeySet ks, Key parentKey) throws KDBException {
		int ret = Elektra.INSTANCE.kdbSet(kdb, ks.get(),
				parentKey.get());
		if (ret == -1) {
			throw new KDBException(parentKey);
		}
	}

	public void close(Key parentKey) {
		Elektra.INSTANCE.kdbClose(kdb, parentKey.get());
	}

	// native pointer
	public Pointer get() {
		return kdb;
	}

	private Pointer kdb;
}
