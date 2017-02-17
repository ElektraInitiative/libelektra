package org.libelektra;

/**
 * This is a Java representation of a plugin.
 */
public interface Plugin {

	/**
	 * Calls the open function of the plugin.
	 *
	 * @param conf
	 *            a configuration keyset
	 * @param errorKey
	 *            a key
	 * @return the plugin's return value for open
	 */
	int open(KeySet conf, Key errorKey);

	/**
	 * Calls the get function of the plugin.
	 *
	 * @param ks
	 *            a keyset
	 * @param parentKey
	 *            a key
	 * @return the plugin's return value for get
	 */
	int get(KeySet ks, Key parentKey);

	/**
	 * Calls the set function of the plugin.
	 *
	 * @param ks
	 *            a keyset
	 * @param parentKey
	 *            a key
	 * @return the plugin's return value for set
	 */
	int set(KeySet ks, Key parentKey);

	/**
	 * Calls the error function of the plugin.
	 *
	 * @param ks
	 *            a keyset
	 * @param parentKey
	 *            a key
	 * @return the plugin's return value for error
	 */
	int error(KeySet ks, Key parentKey);

	/**
	 * Calls the close function of the plugin.
	 *
	 * @param parentKey
	 *            a key
	 * @return the plugin's return value for close
	 */
	int close(Key parentKey);

}
