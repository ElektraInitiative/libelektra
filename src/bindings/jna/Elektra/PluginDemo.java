package Elektra;

public class PluginDemo implements Plugin {
	public void PluginDemo() {
		System.err.println("construct plugin");
	}

	static public void print() {
		System.out.println("open plugin");
		try {
			int pid = CLibrary.INSTANCE.getpid();
			System.out.println("My Process id is " + pid);
			Key key = Key.create("user/hello_world",
				Key.KEY_VALUE, "Hello World",
				Key.KEY_END);
		} catch (Exception e) {
			System.out.println("got exception: " + e.toString());
		}

		System.out.println("after open plugin");
	}

	public int open(Key errorKey) {
		/*
		System.out.println(key.name());
		System.out.println(key.string());
		key.print();
		*/
		print();
		return 0;
	}

	public int get(KeySet ks, Key parentKey) {
		System.out.println("get plugin");
		return 0;
	}

	public int set(KeySet ks, Key parentKey) {
		System.out.println("set plugin");
		return 0;
	}

	public int error(KeySet ks, Key parentKey) {
		System.out.println("error plugin");
		return 0;
	}

	public int close(Key parentKey) {
		System.out.println("close plugin");
		return 0;
	}
}
