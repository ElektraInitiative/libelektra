package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class Return implements Plugin {

    @Override
    public KeySet getConfig() {
        return KeySet.create();
    }

    @Override
	public int kdbOpen(final KeySet conf, final Key errorKey) {
		return 0;
	}

	@Override
	public int kdbGet(final KeySet ks, final Key parentKey) {
		return 10;
	}

	@Override
	public int kdbSet(final KeySet ks, final Key parentKey) {
		return 20;
	}

	@Override
	public int kdbError(final KeySet ks, final Key parentKey) {
		return 30;
	}

	@Override
	public int kdbClose(final Key parentKey) {
		return 0;
	}

    @Override
    public String getName() {
        return "Return";
    }

}
