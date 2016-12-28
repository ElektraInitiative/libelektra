package org.libelektra;

public interface Plugin {

	public int open(KeySet conf, Key errorKey);

	public int get(KeySet ks, Key parentKey);

	public int set(KeySet ks, Key parentKey);

	public int error(KeySet ks, Key parentKey);

	public int close(Key parentKey);

}
