package elektra;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public interface CLibrary extends Library {

	public int getpid();

	// weitere Definitionen siehe unten

	CLibrary INSTANCE = (CLibrary) Native.loadLibrary("c", CLibrary.class);
}
