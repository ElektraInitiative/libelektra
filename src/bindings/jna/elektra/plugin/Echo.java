package elektra.plugin;

import elektra.*;

public class Echo implements Plugin {
	public Echo() {
		System.out.println("construct plugin");
	}

	public int open(KeySet conf, Key errorKey) {
		System.out.println("open plugin");
		System.out.println(errorKey);
		System.out.println(errorKey.getString());
		System.out.println(conf);
		return 0;
	}

	public int get(KeySet ks, Key parentKey) {
		System.out.println("get plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		String name = parentKey+"/infos/provides";
		System.out.println("name: " + name);
		ks.append(Key.create(name,
				Key.KEY_VALUE, "java",
				Key.KEY_END));
		return 0;
	}

	public int set(KeySet ks, Key parentKey) {
		System.out.println("set plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		return 0;
	}

	public int error(KeySet ks, Key parentKey) {
		System.out.println("error plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		return 0;
	}

	public int close(Key parentKey) {
		System.out.println("close plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		return 0;
	}
}
