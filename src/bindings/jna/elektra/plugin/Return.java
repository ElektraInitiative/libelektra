package elektra.plugin;

import elektra.*;

public class Return implements Plugin {
	public Return() {
	}

	public int open(KeySet conf, Key errorKey) {
		return 0;
	}

	public int get(KeySet ks, Key parentKey) {
		return 10;
	}

	public int set(KeySet ks, Key parentKey) {
		return 20;
	}

	public int error(KeySet ks, Key parentKey) {
		return 30;
	}

	public int close(Key parentKey) {
		return 0;
	}
}
