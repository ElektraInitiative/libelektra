import elektra.*;
import elektra.plugin.Echo;

/** Simple hello world example how Elektra could be wrapped to java. */
public class HelloElektra {

	public static void main(String[] args) {
		Key key = Key.create("user/hello_world",
			Key.KEY_VALUE, "Hello World",
			Key.KEY_END);
		System.out.println(key); // to get name
		System.out.println(key.getString());


		KeySet ks = KeySet.create(10,
			Key.create("user/hello_world2",
				Key.KEY_VALUE, "Hello World2",
				Key.KEY_END),
			key,
			KeySet.KS_END);

		for (Key k: ks) {
			System.out.println("iter: " + k.getName() + " " +
					k.getString());
		}
		System.out.println(ks);

		KeySet ks2 = ks.dup();
		ks2.copy(ks);
		System.out.println(ks2.length());
		ks2.append(ks);
		ks2.append(key);

		try (KDB kdb = KDB.open(key)) {
			kdb.get(ks, key);
			Key k = ks.lookup(key);
			System.out.println(k.getString());
		} catch (KDB.KDBException e) {
			System.out.println(e);
		}

		Echo dp = new Echo();
		dp.open(ks, key);
		dp.get(ks, key);
		dp.close(key);


		Key b = Key.create("user/boolean",
			Key.KEY_VALUE, "true",
			Key.KEY_END);
		System.out.println(b.getBoolean());
		b.setBoolean(false);
		System.out.println(b.getBoolean());

		Key n = Key.create("user/weird\\/name///\\\\/is/\\no/_\\\\problem",
			Key.KEY_END);
		for (String s: n) {
			System.out.println("itername: " + s);
		}
	}
}
