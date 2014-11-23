package Elektra;

public class PluginTemplate implements Plugin {
	public PluginTemplate() {
	}

	public int open(Key errorKey) {
		return 0;
	}

	public int get(KeySet ks, Key parentKey) {
		return 0;
	}

	public int set(KeySet ks, Key parentKey) {
		return 0;
	}

	public int error(KeySet ks, Key parentKey) {
		return 0;
	}

	public int close(Key parentKey) {
		return 0;
	}
}
