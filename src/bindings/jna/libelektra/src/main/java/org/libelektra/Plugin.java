package org.libelektra;

import javax.annotation.Nonnull;

/**
 * Java representation of an Elektra plugin
 */
public interface Plugin {

	/**
	 * Return value for plugin methods: An error occurred inside the plugin function
	 */
	static final int STATUS_ERROR = -1;

	/**
	 * Return value for plugin methods: Everything went fine
	 */
	static final int STATUS_SUCCESS = 1;

	/**
	 * Return value for plugin methods: Everything went fine and the function **did not** update the given key set / configuration
	 */
	static final int STATUS_NO_UPDATE = 0;

	/**
	 * @return {@link KeySet} containing the plugin configuration
	 */
	@Nonnull KeySet getConfig ();

	/**
	 * @return Name of the plugin
	 */
	@Nonnull String getName ();

	/**
	 * Calls the plugin's open function
	 *
	 * @param config   Plugin configuration key set
	 * @param errorKey Used to store warnings and error information
	 * @return Plugin's return value for open
	 * @see #STATUS_SUCCESS
	 * @see #STATUS_ERROR
	 */
	int open (KeySet config, Key errorKey);

	/**
	 * Calls the plugin's get function
	 *
	 * @param keySet    Key set to store the retrieved keys in
	 * @param parentKey Parent key for retrieval
	 * @throws KDBException if Elektra could not set the key set
	 * @return the plugin's return value for get
	 * @see #STATUS_SUCCESS
	 * @see #STATUS_ERROR
	 */
	int get (KeySet keySet, Key parentKey) throws KDBException;

	/**
	 * Calls the set function of the plugin.
	 *
	 * @param keySet    a keyset
	 * @param parentKey a key
	 * @throws KDBException when Elektra could not set the keyset
	 * @return the plugin's return value for set
	 */
	int set (KeySet keySet, Key parentKey) throws KDBException;

	/**
	 * Calls the error function of the plugin.
	 *
	 * @param keySet    a keyset
	 * @param parentKey a key
	 * @return the plugin's return value for error
	 */
	int error (KeySet keySet, Key parentKey);

	/**
	 * Calls the close function of the plugin.
	 *
	 * @param parentKey a key
	 * @return the plugin's return value for close
	 */
	int close (Key parentKey);
}
