package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class ExceptionTest implements Plugin {

	public ExceptionTest() {
		throw new RuntimeException("constr");
	}

	@Override
	public int open(final KeySet conf, final Key errorKey) {
		throw new RuntimeException("open");
	}

	@Override
	public int get(final KeySet ks, final Key parentKey) {
		throw new RuntimeException("get");
	}

	@Override
	public int set(final KeySet ks, final Key parentKey) {
		throw new RuntimeException("set");
	}

	@Override
	public int error(final KeySet ks, final Key parentKey) {
		throw new RuntimeException("error");
	}

	@Override
	public int close(final Key parentKey) {
		throw new RuntimeException("close");
	}

}
