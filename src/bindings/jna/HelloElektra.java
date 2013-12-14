import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
 
/** Simple hello world example how Elektra could be wrapped to java. */
public class HelloElektra {
	public interface Elektra extends Library {
		Elektra INSTANCE = (Elektra)
			Native.loadLibrary(("elektra"), Elektra.class);
		Pointer keyNew(String name, Object... args);
		String keyName(Pointer k);
	}

	public static void main(String[] args) {
		Pointer k = Elektra.INSTANCE.keyNew("user/sw//./key");
		System.out.println(Elektra.INSTANCE.keyName(k));
	}
}
