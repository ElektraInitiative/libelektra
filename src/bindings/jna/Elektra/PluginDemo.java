package Elektra;

public class PluginDemo implements Plugin {
	public void PluginDemo() {
		System.err.println("construct plugin");
	}

	public int open(Key errorKey) {
		System.out.println("open plugin");
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
