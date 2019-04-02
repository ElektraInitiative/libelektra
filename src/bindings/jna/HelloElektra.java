import org.libelektra.*;
import org.libelektra.plugin.Echo;

/** Simple hello world example how Elektra could be wrapped to java. */
public class HelloElektra {

	public static void main(String[] args) {
		final Key key = Key.create("user/hello_world", "Hello World");
		System.out.println(key); // to get name
		System.out.println(key.getString());

		final KeySet ks = KeySet.create(10, Key.create("user/hello_world2", "Hello World2"), key, KeySet.KS_END);

		for (Key k : ks) {
			System.out.println("iter: " + k.getName() + " " + k.getString());
		}
		System.out.println(ks);

		final KeySet ks2 = ks.dup();
		ks2.copy(ks);
		System.out.println(ks2.length());
		ks2.append(ks);
		ks2.append(key);

		try (final KDB kdb = KDB.open(key)) {
			kdb.get(ks, key);
			final Key k = ks.lookup(key);
			System.out.println(k.getString());
		} catch (KDB.KDBException e) {
			System.out.println(e);
		}

		final Echo dp = new Echo();
		dp.open(ks, key);
		dp.get(ks, key);
		dp.close(key);

		final Key b = Key.create("user/boolean", "true");
		System.out.println(b.getBoolean());
		b.setBoolean(false);
		System.out.println(b.getBoolean());

		final Key n = Key.create("user/weird\\/name///\\\\/is/\\no/_\\\\problem", Key.KEY_END);
		for (String s : n) {
			System.out.println("itername: " + s);
		}
	}
}
