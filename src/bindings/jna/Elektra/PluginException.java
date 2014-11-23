package Elektra;

public class PluginException implements Plugin {
	public PluginException() {
		throw new RuntimeException("constr");
	}

	public int open(KeySet conf, Key errorKey) {
		throw new RuntimeException("open");
	}

	public int get(KeySet ks, Key parentKey) {
		throw new RuntimeException("get");
	}

	public int set(KeySet ks, Key parentKey) {
		throw new RuntimeException("set");
	}

	public int error(KeySet ks, Key parentKey) {
		throw new RuntimeException("error");
	}

	public int close(Key parentKey) {
		throw new RuntimeException("close");
	}
}
