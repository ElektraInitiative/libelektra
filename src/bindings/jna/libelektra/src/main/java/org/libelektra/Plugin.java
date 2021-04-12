package org.libelektra;

import org.libelektra.exception.KDBException;

/**
 * This is a Java representation of a plugin.
 */
public interface Plugin {

	/**
	 * Gets the config which was used to configure the plugin
	 *
	 * @return A KeySet containing the configuration of the plugin
	 */
	KeySet getConfig ();

	/**
	 * Calls the open function of the plugin.
	 *
	 * @param conf     a configuration keyset
	 * @param errorKey a key
	 * @return the plugin's return value for open
	 */
	int open (KeySet conf, Key errorKey);

	/**
	 * Calls the get function of the plugin.
	 *
	 * @param ks        a keyset
	 * @param parentKey a key
	 * @throws KDBException when Elektra could not set the keyset
	 * @return the plugin's return value for get
	 */
	int get (KeySet ks, Key parentKey) throws KDBException;

	/**
	 * Calls the set function of the plugin.
	 *
	 * @param ks        a keyset
	 * @param parentKey a key
	 * @throws KDBException when Elektra could not set the keyset
	 * @return the plugin's return value for set
	 */
	int set (KeySet ks, Key parentKey) throws KDBException;

	/**
	 * Calls the error function of the plugin.
	 *
	 * @param ks        a keyset
	 * @param parentKey a key
	 * @return the plugin's return value for error
	 */
	int error (KeySet ks, Key parentKey);

	/**
	 * Calls the close function of the plugin.
	 *
	 * @param parentKey a key
	 * @return the plugin's return value for close
	 */
	int close (Key parentKey);

	/**
	 * Returns the plugin name
	 *
	 * @return plugin name
	 */
	String getName ();
}
