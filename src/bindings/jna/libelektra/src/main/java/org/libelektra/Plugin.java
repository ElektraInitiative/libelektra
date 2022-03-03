package org.libelektra;

import javax.annotation.Nonnull;

/**
 * Java representation of an Elektra plugin
 *
 * @implNote because of interface inheritance, it is required that all methods (open, get, set,
 *     error, close) are implemented, even if they are not supported. Whether or not a method is
 *     supported, must be defined via the correspoding `exports/has` key of the contract. Any method
 *     that is not supported, should simply be implemented as `throw new
 *     UnsupportedOperationException()`. If get isn't supported, you must still implement it and
 *     return the contract, when the parent key is below (or the same as) PROCESS_CONTRACT_ROOT. For
 *     other parent keys, you can safely throw UnsupportedOperationException.
 */
public interface Plugin {

  /** This is the root key of the JNI plugin wrapping a Java plugin for use by Elektra */
  static final String JNI_MODULE_CONTRACT_ROOT = "system:/elektra/modules/jni";

  /** This is the root key of the process plugin wrapping a Java plugin for use by Elektra */
  static final String PROCESS_CONTRACT_ROOT = "system:/elektra/modules/java";

  /** Return value for plugin methods: An error occurred inside the plugin function */
  static final int STATUS_ERROR = -1;

  /** Return value for plugin methods: Everything went fine */
  static final int STATUS_SUCCESS = 1;

  /**
   * Return value for plugin methods: Everything went fine and the function **did not** update the
   * given key set / configuration
   */
  static final int STATUS_NO_UPDATE = 0;

  /**
   * @return Name of the plugin
   */
  @Nonnull
  String getName();

  /**
   * Calls the plugin's open function
   *
   * @param config Plugin configuration key set
   * @param errorKey Used to store warnings and error information
   * @return Plugin's return value for open
   * @see #STATUS_SUCCESS
   * @see #STATUS_ERROR
   */
  int open(KeySet config, Key errorKey);

  /**
   * Calls the plugin's get function
   *
   * @param keySet Key set to store the retrieved keys in
   * @param parentKey Parent key for retrieval
   * @throws KDBException if Elektra could not set the key set
   * @return the plugin's return value for get
   * @see #STATUS_SUCCESS
   * @see #STATUS_ERROR
   */
  int get(KeySet keySet, Key parentKey) throws KDBException;

  /**
   * Calls the set function of the plugin.
   *
   * @param keySet a keyset
   * @param parentKey a key
   * @throws KDBException when Elektra could not set the keyset
   * @return the plugin's return value for set
   */
  int set(KeySet keySet, Key parentKey) throws KDBException;

  /**
   * Calls the error function of the plugin.
   *
   * @param keySet a keyset
   * @param parentKey a key
   * @return the plugin's return value for error
   */
  int error(KeySet keySet, Key parentKey);

  /**
   * Calls the close function of the plugin.
   *
   * @param parentKey a key
   * @return the plugin's return value for close
   */
  int close(Key parentKey);
}
