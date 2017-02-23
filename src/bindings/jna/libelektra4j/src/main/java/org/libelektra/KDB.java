package org.libelektra;

import com.sun.jna.Pointer;

/**
 * Represents session with the Key database. Close after usage, or simply use a try-with-resources statement.
 */
public class KDB implements AutoCloseable {

	/**
	 * Custom KDB exception class being used for I/O errors
	 */
	public static class KDBException extends java.io.IOException {

		private static final long serialVersionUID = 1L;

		final transient Key errorKey;

		public KDBException(final Key k) {
			super(new Throwable(String.format("failure in I/O to KDB: %s%n%s", KeyUtils.getErrors(k), KeyUtils.getWarnings(k))));
			errorKey = k;
		}

		public Key getErrorKey() {
			return errorKey;
		}
	}

	private final Pointer kdb;

	/**
	 * Helper constructor for duplication by pointer
	 *
	 * @param p
	 *            Pointer to another KDB object
	 */
	public KDB(final Pointer p) {
		kdb = p;
	}

	/**
	 * Basic constructor of KDB class<br>
	 * Opens KDB session with the given parentKey to write possible warning and error information to
	 *
	 * @param parentKey
	 *            Parent key being used for this KDB session; it is used to store warning and error information
	 * @return New KDB session object
	 */
	public static KDB open(final Key parentKey) {
		return new KDB(Elektra.INSTANCE.kdbOpen(parentKey.get()));
	}

	/**
	 * Clean-up function initiating closing of the KDB session
	 */
	@Override
	public void close() {
		final Key k = Key.create("", Key.KEY_END);
		close(k);
	}

	/*
	 * Wrapped methods
	 */

	/**
	 * Will fetch at least all keys that are sub-keys or children of sub-keys of the supplied parent key.
	 *
	 * @param ks
	 *            KeySet where the fetched keys will be stored in
	 * @param parentKey
	 *            Root key which name will be used to fetch keys below it
	 * @throws KDBException
	 *             In case of an error when loading keys
	 */
	public void get(final KeySet ks, final Key parentKey) throws KDBException {
		final int ret = Elektra.INSTANCE.kdbGet(kdb, ks.get(), parentKey.get());
		if (ret == -1) {
			throw new KDBException(parentKey);
		}
	}

	/**
	 * Will update changed keys of the given keyset in the backend. get() has to be called before this function may be executed.
	 *
	 * @param ks
	 *            KeySet which contains keys to be updated in the backend
	 * @param parentKey
	 *            Is used to add warnings and set an error, if necessary
	 * @throws KDBException
	 *             In case of an error when storing keys
	 */
	public void set(final KeySet ks, final Key parentKey) throws KDBException {
		final int ret = Elektra.INSTANCE.kdbSet(kdb, ks.get(), parentKey.get());
		if (ret == -1) {
			throw new KDBException(parentKey);
		}
	}

	/**
	 * Clean-up method that closes the KDB session
	 *
	 * @param parentKey
	 *            Key holding error and warning information
	 */
	public void close(final Key parentKey) {
		Elektra.INSTANCE.kdbClose(kdb, parentKey.get());
	}

	/**
	 * Native pointer being used by JNA
	 *
	 * @return Native pointer object
	 */
	protected Pointer get() {
		return kdb;
	}

}
