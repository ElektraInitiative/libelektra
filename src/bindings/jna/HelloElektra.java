import Elektra.*;

/** Simple hello world example how Elektra could be wrapped to java. */
public class HelloElektra {

	public static void main(String[] args) {
		Key key = Key.create("user/hello_world");
		System.out.println(key.name());
		System.out.println(key.string());

		KeySet ks = KeySet.create(0);
		KDB kdb = KDB.open(key);
		kdb.get(ks, key);
		Key k = ks.lookup(key);
		System.out.println(k.string());
	}
}
