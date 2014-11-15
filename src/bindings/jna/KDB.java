import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class KDB {
	public static KDB open(Key parentKey) {
		return new KDB(Elektra.INSTANCE.kdbOpen(parentKey.get()));
	}

	public KDB(Pointer p) {
		kdb = p;
	}

	public void get(KeySet ks, Key parentKey) {
		Elektra.INSTANCE.kdbGet(kdb, ks.get(), parentKey.get());
	}

	public Pointer get() {
		return kdb;
	}

	private Pointer kdb;
}
