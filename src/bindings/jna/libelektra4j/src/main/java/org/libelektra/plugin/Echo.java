package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class Echo implements Plugin {

	public Echo() {
		System.out.println("construct plugin");
	}

	@Override
	public int open(final KeySet conf, final Key errorKey) {
		System.out.println("open plugin");
		System.out.println(errorKey);
		System.out.println(errorKey.getString());
		System.out.println(conf);
		return 0;
	}

	@Override
	public int get(final KeySet ks, final Key parentKey) {
		System.out.println("get plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		final String name = parentKey + "/infos/provides";
		System.out.println("name: " + name);
		ks.append(Key.create(name, Key.KEY_VALUE, "java", Key.KEY_END));
		return 0;
	}

	@Override
	public int set(final KeySet ks, final Key parentKey) {
		System.out.println("set plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		return 0;
	}

	@Override
	public int error(final KeySet ks, final Key parentKey) {
		System.out.println("error plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		System.out.println(ks);
		return 0;
	}

	@Override
	public int close(final Key parentKey) {
		System.out.println("close plugin");
		System.out.println(parentKey);
		System.out.println(parentKey.getString());
		return 0;
	}
}
