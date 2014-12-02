package elektra.plugin;

import elektra.*;

public class Template implements Plugin {
	public Template() {
	}

	public int open(KeySet conf, Key errorKey) {
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
